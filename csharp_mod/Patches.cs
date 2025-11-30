namespace Slang;

using System;
using Assets.Scripts;
using Assets.Scripts.Objects;
using Assets.Scripts.Objects.Electrical;
using Assets.Scripts.Objects.Motherboards;
using Assets.Scripts.UI;
using HarmonyLib;

[HarmonyPatch]
public static class SlangPatches
{
    [HarmonyPatch(
        typeof(ProgrammableChipMotherboard),
        nameof(ProgrammableChipMotherboard.InputFinished)
    )]
    [HarmonyPrefix]
    public static void pgmb_InputFinished(ref string result)
    {
        // guard to ensure we have valid IC10 before continuing
        if (
            !SlangPlugin.IsSlangSource(ref result)
            || !Marshal.CompileFromString(result, out string compiled)
            || string.IsNullOrEmpty(compiled)
        )
        {
            return;
        }

        var thisRef = Guid.NewGuid();

        // Ensure we cache this compiled code for later retreival.
        GlobalCode.SetSource(thisRef, result);

        compiled += $"\n{GlobalCode.SLANG_REF}{thisRef}";
        result = compiled;
    }

    [HarmonyPatch(typeof(ProgrammableChipMotherboard), nameof(ProgrammableChipMotherboard.OnEdit))]
    [HarmonyPrefix]
    public static void isc_OnEdit(ProgrammableChipMotherboard __instance)
    {
        var sourceCode = System.Text.Encoding.UTF8.GetString(
            System.Text.Encoding.ASCII.GetBytes(__instance.GetSourceCode())
        );

        if (string.IsNullOrEmpty(sourceCode))
        {
            return;
        }

        var tagIndex = sourceCode.LastIndexOf(GlobalCode.SLANG_REF);

        if (tagIndex == -1)
        {
            // this is not slang managed code
            return;
        }

        if (
            !Guid.TryParse(
                sourceCode.Substring(tagIndex + GlobalCode.SLANG_REF.Length),
                out Guid sourceRef
            )
        )
        {
            // not a valid Guid, not managed by slang
            return;
        }

        var slangSource = GlobalCode.GetSource(sourceRef);

        if (string.IsNullOrEmpty(slangSource))
        {
            // Didn't find that source ref in the global code manager.
            return;
        }

        __instance.SetSourceCode(slangSource);
    }

    [HarmonyPatch(typeof(ProgrammableChip), nameof(ProgrammableChip.SerializeSave))]
    [HarmonyPostfix]
    public static void pgc_SerializeSave(ProgrammableChip __instance, ref ThingSaveData __result)
    {
        if (__result is not ProgrammableChipSaveData chipData)
            return;
        if (string.IsNullOrEmpty(chipData.SourceCode))
            return;

        var firstLine = chipData.SourceCode.Split('\n')[0].Trim();

        // Check if the file starts with the Reference Tag
        if (!firstLine.StartsWith(GlobalCode.SLANG_REF))
            return;

        string guidString = firstLine.Substring(GlobalCode.SLANG_REF.Length).Trim();

        if (!Guid.TryParse(guidString, out Guid slangRefGuid))
            return;

        var slangEncoded = GlobalCode.GetEncoded(slangRefGuid);

        if (string.IsNullOrEmpty(slangEncoded))
            return;

        // We add 1 to length to remove the '\n' character as well
        // Handle edge case where there is only one line
        int removeLength = firstLine.Length;
        if (chipData.SourceCode.Length > firstLine.Length)
            removeLength++;

        var cleanIc10 = chipData.SourceCode.Remove(0, removeLength);

        chipData.SourceCode = $"{cleanIc10}\n{GlobalCode.SLANG_SRC}{slangEncoded}";
    }

    [HarmonyPatch(typeof(ProgrammableChip), nameof(ProgrammableChip.DeserializeSave))]
    [HarmonyPrefix]
    public static void pgc_DeserializeSave(ref ThingSaveData savedData)
    {
        // 1. Ensure we are looking at a Programmable Chip
        if (savedData is not ProgrammableChipSaveData pcSaveData)
        {
            return;
        }

        // 2. Safety check for null/empty code
        if (string.IsNullOrEmpty(pcSaveData.SourceCode))
            return;

        // 3. Check for the #SLANG_SRC: footer we added during serialization
        int tagIndex = pcSaveData.SourceCode.LastIndexOf(GlobalCode.SLANG_SRC);

        // If the tag is missing, this is just a normal IC10 script. Do nothing.
        if (tagIndex == -1)
            return;

        // 4. Extract the Encoded Source (Base64)
        // The format in the file is: <IC10_CODE>\n#SLANG_SRC:<BASE64>
        string encodedSource = pcSaveData.SourceCode.Substring(
            tagIndex + GlobalCode.SLANG_SRC.Length
        );

        // 5. Extract the IC10 Code
        // We strip off the tag and the newline we added before it.
        // Using TrimEnd() helps clean up that specific newline.
        string ic10Code = pcSaveData.SourceCode.Substring(0, tagIndex).TrimEnd();

        // 6. Generate a new Runtime GUID
        // We don't need to persist the GUID from the last session; we just need a key for *this* session.
        Guid runtimeGuid = Guid.NewGuid();

        // 7. Hydrate the Cache
        GlobalCode.SetEncoded(runtimeGuid, encodedSource);

        // 8. Rewrite the SourceCode to the "Runtime" format
        // This ensures that when the user opens the editor, SlangPlugin.TryRestoreSourceCode matches the header.
        pcSaveData.SourceCode = $"{GlobalCode.SLANG_REF} {runtimeGuid}\n{ic10Code}";
    }
}
