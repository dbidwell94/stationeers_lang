namespace Slang;

using Assets.Scripts.UI;
using HarmonyLib;

[HarmonyPatch]
public static class SlangPatches
{
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
