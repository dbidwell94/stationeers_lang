using System;
using Assets.Scripts.Objects.Motherboards;
using HarmonyLib;

namespace Slang
{
    [HarmonyPatch]
    public static class SlangPatches
    {
        [HarmonyPatch(
            typeof(ProgrammableChipMotherboard),
            nameof(ProgrammableChipMotherboard.InputFinished)
        )]
        [HarmonyPrefix]
        public static void PGM_InputFinished(ref string result)
        {
            if (string.IsNullOrEmpty(result) || !SlangPlugin.IsSlangSource(ref result))
            {
                return;
            }

            L.Debug("Detected Slang source, compiling...");

            // Compile the Slang source into IC10
            string compiled = SlangPlugin.Compile(result);

            // Ensure that the string is correct
            if (string.IsNullOrEmpty(compiled))
            {
                return;
            }

            var newUuid = Guid.NewGuid().ToString();

            SlangPlugin.CopySourceToFile(result);

            // Set the result to be the compiled source so the rest of the function can continue as normal
            result = compiled;
        }
    }
}
