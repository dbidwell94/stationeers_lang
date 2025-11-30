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

    public static void ClearCache()
    {
        codeDict.Clear();
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
        if (codeDict.ContainsKey(reference))
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
