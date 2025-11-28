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

        const string RUST_DLL_NAME = "slang.compiler.dll";

        /// <summary>Takes raw `Slang` source code and compiles it into IC10</summary>
        [DllImport(RUST_DLL_NAME, CallingConvention = CallingConvention.Cdecl)]
        private static extern IntPtr compile_from_string(string input);

        /// <summary>Frees memory that was allocated by the FFI call to `compile_from_string`</summary>
        [DllImport(RUST_DLL_NAME, CallingConvention = CallingConvention.Cdecl)]
        private static extern void free_slang_string(IntPtr ptr);

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

        public static string Compile(string source)
        {
            if (string.IsNullOrEmpty(source))
                return "";

            IntPtr ptr = compile_from_string(source);
            try
            {
                return Marshal.PtrToStringAnsi(ptr);
            }
            finally
            {
                free_slang_string(ptr);
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
            ExtractNativeDll(RUST_DLL_NAME);
            var harmony = new Harmony("com.dbidwell94.slang");
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
                        $"{RUST_DLL_NAME} compiler not found. This means it was not embedded in the mod. Please contact the mod author!"
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
