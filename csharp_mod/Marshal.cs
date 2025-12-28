namespace Slang;

using System;
using System.Collections.Generic;
using System.IO;
using System.Reflection;
using System.Runtime.InteropServices;
using Assets.Scripts.UI;
using StationeersIC10Editor;

public struct Range
{
    public uint StartCol = 0;
    public uint EndCol = 0;
    public uint StartLine = 0;
    public uint EndLine = 0;

    public Range(uint startLine, uint startCol, uint endLine, uint endCol)
    {
        StartLine = startLine;
        StartCol = startCol;
        EndLine = endLine;
        EndCol = endCol;
    }

    public override string ToString()
    {
        return $"L{StartLine}C{StartCol} - L{EndLine}C{EndCol}";
    }
}

public struct Diagnostic
{
    public string Message;
    public int Severity;
    public Range Range;
}

public struct SourceMapEntry
{
    public Range SlangSource;
    public uint Ic10Line;

    public override string ToString()
    {
        return $"IC10: {Ic10Line} Slang: `{SlangSource}`";
    }
}

public static class Marshal
{
    private static IntPtr _libraryHandle = IntPtr.Zero;

    [DllImport("kernel32", SetLastError = true, CharSet = CharSet.Ansi)]
    private static extern IntPtr LoadLibrary([MarshalAs(UnmanagedType.LPStr)] string lpFileName);

    [DllImport("kernel32", SetLastError = true)]
    private static extern bool FreeLibrary(IntPtr hModule);

    private static bool EnsureLibLoaded()
    {
        if (_libraryHandle != IntPtr.Zero)
        {
            return true;
        }

        try
        {
            _libraryHandle = LoadLibrary(ExtractNativeLibrary(Ffi.RustLib));
            L.Debug("Rust DLL loaded successfully. Enjoy native speed compilations!");
            CodeFormatters.RegisterFormatter("Slang", typeof(SlangFormatter), true);

            return true;
        }
        catch (Exception ex)
        {
            L.Error($"Failed to init slang compiler: {ex.Message}");
            return false;
        }
    }

    public static bool Init()
    {
        return EnsureLibLoaded();
    }

    public static bool Destroy()
    {
        if (_libraryHandle == IntPtr.Zero)
        {
            return true;
        }

        try
        {
            CodeFormatters.RegisterFormatter("Slang", typeof(PlainTextFormatter), true);
            if (!FreeLibrary(_libraryHandle))
            {
                L.Warning("Unable to free Rust library");
            }
            _libraryHandle = IntPtr.Zero;
            L.Debug("Rust DLL library freed");
            return true;
        }
        catch (Exception ex)
        {
            L.Warning($"Unable to free handle to slang compiler's dll. {ex.Message}");
            return false;
        }
    }

    public static unsafe bool CompileFromString(
        string inputString,
        out string compiledString,
        out List<SourceMapEntry> sourceMapEntries
    )
    {
        if (String.IsNullOrEmpty(inputString) || !EnsureLibLoaded())
        {
            compiledString = String.Empty;
            sourceMapEntries = new();
            return false;
        }

        fixed (char* ptrString = inputString)
        {
            var input = new slice_ref_uint16_t
            {
                ptr = (ushort*)ptrString,
                len = (UIntPtr)inputString.Length,
            };

            var result = Ffi.compile_from_string(input);

            try
            {
                sourceMapEntries = result.source_map.ToList();
                compiledString = result.output_code.AsString();
                return true;
            }
            finally
            {
                Ffi.free_ffi_compilation_result(result);
            }
        }
    }

    public static unsafe List<Diagnostic> DiagnoseSource(string inputString)
    {
        if (string.IsNullOrEmpty(inputString) || !EnsureLibLoaded())
        {
            return new();
        }

        fixed (char* ptrInput = inputString)
        {
            var input = new slice_ref_uint16_t
            {
                ptr = (ushort*)ptrInput,
                len = (UIntPtr)inputString.Length,
            };

            return Ffi.diagnose_source(input).ToList();
        }
    }

    public static unsafe List<SemanticToken> TokenizeLine(string inputString)
    {
        if (string.IsNullOrEmpty(inputString) || !EnsureLibLoaded())
        {
            return new List<SemanticToken>();
        }

        fixed (char* ptrInputStr = inputString)
        {
            var strRef = new slice_ref_uint16_t
            {
                len = (UIntPtr)inputString.Length,
                ptr = (ushort*)ptrInputStr,
            };

            var tokens = Ffi.tokenize_line(strRef);
            return tokens.ToTokenList();
        }
    }

    /// <summary>
    /// Gets the currently documented items from the Slang compiler and returns new StationpediaPages with correct formatting.
    /// </summary>
    public static unsafe List<StationpediaPage> GetSlangDocs()
    {
        return Ffi.get_docs().ToList();
    }

    private static string ExtractNativeLibrary(string libName)
    {
        string destinationPath = Path.Combine(Path.GetTempPath(), libName);

        Assembly assembly = Assembly.GetExecutingAssembly();

        using (Stream resourceStream = assembly.GetManifestResourceStream(libName))
        {
            if (resourceStream == null)
            {
                L.Error(
                    $"{libName} not found. This means it was not embedded in the mod. Please contact the mod author!"
                );
                return "";
            }

            // Check if file exists and contents are identical to avoid overwriting locked files
            if (File.Exists(destinationPath))
            {
                try
                {
                    using (
                        FileStream fileStream = new FileStream(
                            destinationPath,
                            FileMode.Open,
                            FileAccess.Read,
                            FileShare.ReadWrite
                        )
                    )
                    {
                        if (resourceStream.Length == fileStream.Length)
                        {
                            if (StreamsContentsAreEqual(resourceStream, fileStream))
                            {
                                L.Debug(
                                    $"DLL {libName} already exists and matches. Skipping extraction."
                                );
                                return destinationPath;
                            }
                        }
                    }
                }
                catch (IOException ex)
                {
                    L.Warning(
                        $"Could not verify existing {libName}, attempting overwrite. {ex.Message}"
                    );
                }
            }

            resourceStream.Position = 0;

            // Attempt to overwrite if missing or different
            try
            {
                using (FileStream fileStream = new FileStream(destinationPath, FileMode.Create))
                {
                    resourceStream.CopyTo(fileStream);
                }
                return destinationPath;
            }
            catch (IOException e)
            {
                // If we fail here, the file is likely locked.
                // However, if we are here, it means the file is DIFFERENT or we couldn't read it.
                // As a fallback for live-reload, we can try returning the path anyway
                // assuming the existing locked file might still work.
                L.Warning(
                    $"Could not overwrite {libName} (it might be in use): {e.Message}. Attempting to use existing file."
                );
                return destinationPath;
            }
        }
    }

    private static bool StreamsContentsAreEqual(Stream stream1, Stream stream2)
    {
        const int bufferSize = 4096;
        byte[] buffer1 = new byte[bufferSize];
        byte[] buffer2 = new byte[bufferSize];

        while (true)
        {
            int count1 = stream1.Read(buffer1, 0, bufferSize);
            int count2 = stream2.Read(buffer2, 0, bufferSize);

            if (count1 != count2)
                return false;
            if (count1 == 0)
                return true;

            for (int i = 0; i < count1; i++)
            {
                if (buffer1[i] != buffer2[i])
                    return false;
            }
        }
    }
}
