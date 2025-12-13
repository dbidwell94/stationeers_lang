using System;
using System.Collections.Generic;
using System.IO;
using System.IO.Compression;
using System.Text;

namespace Slang;

public static class GlobalCode
{
    public const string SLANG_REF = "#SLANG_REF:";
    public const string SLANG_SRC = "#SLANG_SRC:";

    // This is a Dictionary of ENCODED source code, compressed
    // so that save file data is smaller
    private static Dictionary<Guid, string> codeDict = new();

    // This Dictionary stores the source maps for the given SLANG_REF, where
    // the key is the IC10 line, and the value is a List of Slang ranges where that
    // line would have come from
    private static Dictionary<Guid, Dictionary<uint, List<Range>>> sourceMaps = new();

    public static void ClearCache()
    {
        codeDict.Clear();
    }

    public static void SetSourceMap(Guid reference, List<SourceMapEntry> sourceMapEntries)
    {
        var builtDictionary = new Dictionary<uint, List<Range>>();

        foreach (var entry in sourceMapEntries)
        {
            if (!builtDictionary.ContainsKey(entry.Ic10Line))
            {
                builtDictionary[entry.Ic10Line] = new();
            }
            builtDictionary[entry.Ic10Line].Add(entry.SlangSource);
        }

        sourceMaps[reference] = builtDictionary;
    }

    public static bool GetSlangErrorLineFromICError(
        Guid reference,
        uint icErrorLine,
        out uint slangSrc,
        out Range slangSpan
    )
    {
        slangSrc = icErrorLine;
        slangSpan = new Range { };

        if (!sourceMaps.ContainsKey(reference))
        {
            return false;
        }

        if (!sourceMaps[reference].ContainsKey(icErrorLine))
        {
            return false;
        }

        var foundRange = sourceMaps[reference][icErrorLine];

        if (foundRange is null)
        {
            return false;
        }

        slangSrc = foundRange[0].StartLine;
        slangSpan = foundRange[0];
        return true;
    }

    public static string GetSource(Guid reference)
    {
        if (!codeDict.ContainsKey(reference))
        {
            return string.Empty;
        }

        return DecodeSource(codeDict[reference]);
    }

    public static void SetSource(Guid reference, string source)
    {
        codeDict[reference] = EncodeSource(source);
    }

    public static string? GetEncoded(Guid reference)
    {
        if (!codeDict.ContainsKey(reference))
            return null;

        return codeDict[reference];
    }

    public static void SetEncoded(Guid reference, string encodedSource)
    {
        if (codeDict.ContainsKey(reference))
        {
            codeDict[reference] = encodedSource;
        }
        else
        {
            codeDict.Add(reference, encodedSource);
        }
    }

    private static string EncodeSource(string source)
    {
        if (string.IsNullOrEmpty(source))
        {
            return "";
        }

        byte[] bytes = Encoding.UTF8.GetBytes(source);

        using (var memoryStream = new MemoryStream())
        {
            using (var gzipStream = new GZipStream(memoryStream, CompressionMode.Compress))
            {
                gzipStream.Write(bytes, 0, bytes.Length);
            }
            return Convert.ToBase64String(memoryStream.ToArray());
        }
    }

    private static string DecodeSource(string source)
    {
        if (string.IsNullOrEmpty(source))
        {
            return "";
        }

        byte[] compressedBytes = Convert.FromBase64String(source);

        using (var memoryStream = new MemoryStream(compressedBytes))
        {
            using (var gzipStream = new GZipStream(memoryStream, CompressionMode.Decompress))
            {
                using (var outputStream = new MemoryStream())
                {
                    gzipStream.CopyTo(outputStream);

                    return Encoding.UTF8.GetString(outputStream.ToArray());
                }
            }
        }
    }
}
