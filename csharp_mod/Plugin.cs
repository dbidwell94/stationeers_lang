using BepInEx;
using HarmonyLib;

namespace Slang
{
    class L
    {
        private static BepInEx.Logging.ManualLogSource? _logger;

        public static void SetLogger(BepInEx.Logging.ManualLogSource logger)
        {
            _logger = logger;
        }

        public static void Debug(string message)
        {
            _logger?.LogDebug(message);
        }

        public static void Info(string message)
        {
            _logger?.LogInfo(message);
        }

        public static void Error(string message)
        {
            _logger?.LogError(message);
        }

        public static void Warning(string message)
        {
            _logger?.LogWarning(message);
        }
    }

    [BepInPlugin(PluginGuid, PluginName, PluginVersion)]
    [BepInDependency(StationeersIC10Editor.IC10EditorPlugin.PluginGuid)]
    public class SlangPlugin : BaseUnityPlugin
    {
        public const string PluginGuid = "com.biddydev.slang";
        public const string PluginName = "Slang";
        public const string PluginVersion = "0.5.1";

        private static Harmony? _harmony;

        public void Awake()
        {
            L.SetLogger(Logger);
            _harmony = new Harmony(PluginGuid);

            // If we failed to load the compiler, bail from the rest of the patches. It won't matter,
            // as the compiler itself has failed to load.
            if (!Marshal.Init())
            {
                L.Error("Marshal failed to init");
                return;
            }

            _harmony.PatchAll();
            L.Debug("Ran Harmony patches");
        }

        public void OnDestroy()
        {
            Marshal.Destroy();
            _harmony?.UnpatchSelf();
            L.Debug("Cleaned up Harmony patches");
        }
    }
}
