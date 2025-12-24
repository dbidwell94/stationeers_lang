using System;
using System.Collections.Generic;

namespace Slang;

public static class GlobalCode
{
    /// <summary>
    /// This is the OLD way of handling saving / loading. This has been replaced with a native
    /// save / load from IC10 Editor. However; this needs to remain for compatibility with
    /// previous versions of code not compiled with 0.4.2 or later.
    /// </summary>
    public const string SLANG_SRC = "#SLANG_SRC:";

    /// <summary>
    /// This Dictionary stores the source maps for the given SLANG_REF, where
    /// the key is the IC10 line, and the value is a List of Slang ranges where that
    /// line would have come from
    /// </summary>
    private static Dictionary<Guid, Dictionary<uint, List<Range>>> sourceMaps = new();

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
}
