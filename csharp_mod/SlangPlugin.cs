using System;
using System.IO;
using System.Reflection;
using System.Runtime.InteropServices;
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
            if (string.IsNullOrEmpty(source))
                return "";

            // Add a null terminator char at the end of the source string (turns into a CStr)
            source += "\0";

            var bytes = System.Text.Encoding.UTF8.GetBytes(source);

            // don't move my memory around, C#!
            fixed (byte* pBytes = bytes)
            {
                var compiled = Ffi.compile_from_string(pBytes);
                try
                {
                    return compiled.AsString();
                }
                finally
                {
                    Ffi.free_string(compiled);
                }
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
            ExtractNativeDll("slang.dll");
            var harmony = new Harmony(PluginGuid);
            harmony.PatchAll();
        }

        private void ExtractNativeDll(string fileName)
        {
            string destinationPath = Path.Combine(Path.GetDirectoryName(Info.Location), fileName);

            Assembly assembly = Assembly.GetExecutingAssembly();

            using (Stream stream = assembly.GetManifestResourceStream(fileName))
            {
                if (stream == null)
                {
                    L.Error(
                        "slang.dll compiler not found. This means it was not embedded in the mod. Please contact the mod author!"
                    );
                    return;
                }

                try
                {
                    using (FileStream fileStream = new FileStream(destinationPath, FileMode.Create))
                    {
                        stream.CopyTo(fileStream);
                    }
                }
                catch (IOException e)
                {
                    L.Warning($"Could not overwrite {fileName} (it might be in use): {e.Message}");
                }
            }
        }
    }
}
