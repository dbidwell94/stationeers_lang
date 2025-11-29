using System.Text.RegularExpressions;
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

    [BepInPlugin(PluginGuid, PluginName, "0.1.0")]
    [BepInDependency(StationeersIC10Editor.IC10EditorPlugin.PluginGuid)]
    public class SlangPlugin : BaseUnityPlugin
    {
        public const string PluginGuid = "com.biddydev.slang";
        public const string PluginName = "Slang";

        private Harmony? _harmony;

        private static Regex? _slangSourceCheck = null;

        private static Regex SlangSourceCheck
        {
            get
            {
                if (_slangSourceCheck is null)
                {
                    _slangSourceCheck = new Regex(@"[;{}()]|\b(let|fn|device)\b|\/\/");
                }

                return _slangSourceCheck;
            }
        }

        public static unsafe string Compile(string source)
        {
            string compiled;
            if (Marshal.CompileFromString(source, out compiled))
            {
                // TODO: handle saving the original source code
                return compiled;
            }
            else
            {
                return compiled;
            }
        }

        /// <summary>Take original slang source code and copies it to a file
        /// for use in restoring later.
        /// </summary>
        public static bool CopySourceToFile(string source)
        {
            return true;
        }

        public static bool IsSlangSource(ref string input)
        {
            return SlangSourceCheck.IsMatch(input);
        }

        private void Awake()
        {
            L.SetLogger(Logger);
            this._harmony = new Harmony(PluginGuid);
            L.Info("slang loaded");

            // If we failed to load the compiler, bail from the rest of the patches. It won't matter,
            // as the compiler itself has failed to load.
            if (!Marshal.Init())
            {
                return;
            }

            this._harmony.PatchAll();
        }

        private void OnDestroy()
        {
            if (Marshal.Destroy())
            {
                L.Info("FFI references cleaned up.");
            }
            if (this._harmony is not null)
            {
                this._harmony.UnpatchSelf();
            }
        }
    }
}
