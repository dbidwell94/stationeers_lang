namespace Slang;

using System;
using Assets.Scripts.Objects;
using Assets.Scripts.Objects.Electrical;
using Assets.Scripts.Objects.Motherboards;
using Assets.Scripts.UI;
using HarmonyLib;

[HarmonyPatch]
public static class SlangPatches
{
    private static ProgrammableChipMotherboard? _currentlyEditingMotherboard;
    private static AsciiString? _motherboardCachedCode;
    private static Guid? _currentlyEditingGuid;

    [HarmonyPatch(
        typeof(ProgrammableChipMotherboard),
        nameof(ProgrammableChipMotherboard.InputFinished)
    )]
    [HarmonyPrefix]
    public static void pgmb_InputFinished(ref string result)
    {
        _currentlyEditingMotherboard = null;
        _motherboardCachedCode = null;
        // guard to ensure we have valid IC10 before continuing
        if (
            !SlangPlugin.IsSlangSource(ref result)
            || !Marshal.CompileFromString(result, out var compiled, out var sourceMap)
            || string.IsNullOrEmpty(compiled)
        )
        {
            return;
        }

        var thisRef = _currentlyEditingGuid ?? Guid.NewGuid();

        // Ensure we cache this compiled code for later retreival.
        GlobalCode.SetSource(thisRef, result);
        GlobalCode.SetSourceMap(thisRef, sourceMap);

        _currentlyEditingGuid = null;

        // Append REF to the bottom
        compiled += $"\n{GlobalCode.SLANG_REF}{thisRef}";
        result = compiled;
    }

    [HarmonyPatch(typeof(ProgrammableChipMotherboard), nameof(ProgrammableChipMotherboard.OnEdit))]
    [HarmonyPrefix]
    public static void isc_OnEdit(ProgrammableChipMotherboard __instance)
    {
        _currentlyEditingMotherboard = __instance;
        _motherboardCachedCode = __instance.GetSourceCode();
        var sourceCode = System.Text.Encoding.UTF8.GetString(
            System.Text.Encoding.ASCII.GetBytes(__instance.GetSourceCode())
        );

        if (string.IsNullOrEmpty(sourceCode))
        {
            return;
        }

        // Look for REF at the bottom
        var tagIndex = sourceCode.LastIndexOf(GlobalCode.SLANG_REF);

        if (tagIndex == -1)
        {
            // this is not slang managed code
            return;
        }

        if (
            !Guid.TryParse(
                sourceCode.Substring(tagIndex + GlobalCode.SLANG_REF.Length).Trim(),
                out Guid sourceRef
            )
        )
        {
            // not a valid Guid, not managed by slang
            return;
        }

        _currentlyEditingGuid = sourceRef;
        var slangSource = GlobalCode.GetSource(sourceRef);

        if (string.IsNullOrEmpty(slangSource))
        {
            // Didn't find that source ref in the global code manager.
            return;
        }

        __instance.SetSourceCode(slangSource);
    }

    private static void HandleSerialization(ref string sourceCode)
    {
        if (string.IsNullOrEmpty(sourceCode))
            return;

        // Check if the file ends with the Reference Tag
        var tagIndex = sourceCode.LastIndexOf(GlobalCode.SLANG_REF);

        if (tagIndex == -1)
            return;

        string guidString = sourceCode.Substring(tagIndex + GlobalCode.SLANG_REF.Length).Trim();

        if (!Guid.TryParse(guidString, out Guid slangRefGuid))
        {
            L.Warning($"Found SLANG_REF but failed to parse GUID: {guidString}");
            return;
        }

        var slangEncoded = GlobalCode.GetEncoded(slangRefGuid);

        if (string.IsNullOrEmpty(slangEncoded))
        {
            L.Warning(
                $"Could not find encoded source for ref {slangRefGuid}. Save will contain compiled IC10 only."
            );
            return;
        }

        // Extract the clean IC10 code (everything before the tag)
        var cleanIc10 = sourceCode.Substring(0, tagIndex).TrimEnd();

        // Append the encoded source tag to the bottom
        sourceCode = $"{cleanIc10}\n{GlobalCode.SLANG_SRC}{slangEncoded}";
    }

    [HarmonyPatch(typeof(ProgrammableChip), nameof(ProgrammableChip.SerializeSave))]
    [HarmonyPostfix]
    public static void pgc_SerializeSave(ProgrammableChip __instance, ref ThingSaveData __result)
    {
        if (__result is not ProgrammableChipSaveData chipData)
            return;

        string code = chipData.SourceCode;
        HandleSerialization(ref code);
        chipData.SourceCode = code;
    }

    [HarmonyPatch(
        typeof(ProgrammableChipMotherboard),
        nameof(ProgrammableChipMotherboard.SerializeSave)
    )]
    [HarmonyPostfix]
    public static void pgmb_SerializeSave(
        ProgrammableChipMotherboard __instance,
        ref ThingSaveData __result
    )
    {
        if (__result is not ProgrammableChipMotherboardSaveData chipData)
            return;

        string code = chipData.SourceCode;
        HandleSerialization(ref code);
        chipData.SourceCode = code;
    }

    private static void HandleDeserialization(ref string sourceCode)
    {
        // Safety check for null/empty code
        if (string.IsNullOrEmpty(sourceCode))
            return;

        // Check for the #SLANG_SRC: footer
        int tagIndex = sourceCode.LastIndexOf(GlobalCode.SLANG_SRC);

        // If the tag is missing, this is just a normal IC10 script. Do nothing.
        if (tagIndex == -1)
            return;

        // Extract the Encoded Source (Base64)
        string encodedSource = sourceCode.Substring(tagIndex + GlobalCode.SLANG_SRC.Length).Trim();

        // Extract the IC10 Code (strip off the tag and the newline before it)
        string ic10Code = sourceCode.Substring(0, tagIndex).TrimEnd();

        // Generate a new Runtime GUID for this session
        Guid runtimeGuid = Guid.NewGuid();

        // Hydrate the Cache
        GlobalCode.SetEncoded(runtimeGuid, encodedSource);

        // Rewrite the SourceCode to the "Runtime" format (REF at bottom)
        sourceCode = $"{ic10Code}\n{GlobalCode.SLANG_REF}{runtimeGuid}";
    }

    [HarmonyPatch(typeof(ProgrammableChip), nameof(ProgrammableChip.DeserializeSave))]
    [HarmonyPrefix]
    public static void pgc_DeserializeSave(ref ThingSaveData savedData)
    {
        if (savedData is not ProgrammableChipSaveData pcSaveData)
            return;

        string code = pcSaveData.SourceCode;
        HandleDeserialization(ref code);
        pcSaveData.SourceCode = code;
    }

    [HarmonyPatch(
        typeof(ProgrammableChipMotherboard),
        nameof(ProgrammableChipMotherboard.DeserializeSave)
    )]
    [HarmonyPrefix]
    public static void pgmb_DeserializeSave(ref ThingSaveData savedData)
    {
        if (savedData is not ProgrammableChipMotherboardSaveData pcSaveData)
            return;

        string code = pcSaveData.SourceCode;
        HandleDeserialization(ref code);
        pcSaveData.SourceCode = code;
    }

    [HarmonyPatch(typeof(InputSourceCode), nameof(InputSourceCode.ButtonInputCancel))]
    [HarmonyPrefix]
    public static void isc_ButtonInputCancel()
    {
        if (_currentlyEditingMotherboard is null || _motherboardCachedCode is null)
        {
            return;
        }

        _currentlyEditingMotherboard.SetSourceCode(_motherboardCachedCode);

        _currentlyEditingMotherboard = null;
        _motherboardCachedCode = null;
        _currentlyEditingGuid = null;
    }

    [HarmonyPatch(typeof(Stationpedia), nameof(Stationpedia.Regenerate))]
    [HarmonyPostfix]
    public static void Stationpedia_Regenerate()
    {
        foreach (var page in Marshal.GetSlangDocs())
        {
            Stationpedia.Register(page);
        }
    }
}
