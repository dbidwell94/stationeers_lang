using System.IO;
using System.Reflection;
using System.Text.RegularExpressions;
using BepInEx;
using HarmonyLib;
using StationeersIC10Editor;

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

            if (ExtractNativeDll(Ffi.RustLib))
            {
                var harmony = new Harmony(PluginGuid);
                harmony.PatchAll();
                CodeFormatters.RegisterFormatter("slang", () => new SlangFormatter(), true);
            }
        }

        private bool ExtractNativeDll(string fileName)
        {
            string destinationPath = Path.Combine(Path.GetDirectoryName(Info.Location), fileName);

            Assembly assembly = Assembly.GetExecutingAssembly();

            using (Stream stream = assembly.GetManifestResourceStream(fileName))
            {
                if (stream == null)
                {
                    L.Error(
                        $"{Ffi.RustLib} not found. This means it was not embedded in the mod. Please contact the mod author!"
                    );
                    return false;
                }

                try
                {
                    using (FileStream fileStream = new FileStream(destinationPath, FileMode.Create))
                    {
                        stream.CopyTo(fileStream);
                    }
                    return true;
                }
                catch (IOException e)
                {
                    L.Warning($"Could not overwrite {fileName} (it might be in use): {e.Message}");
                    return false;
                }
            }
        }
    }
}
