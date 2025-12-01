namespace Slang;

using System;
using System.Collections.Generic;
using System.IO;
using System.Reflection;
using System.Runtime.InteropServices;
using StationeersIC10Editor;

public struct Range
{
    public uint StartCol;
    public uint EndCol;
    public uint StartLine;
    public uint EndLine;
}

public struct Diagnostic
{
    public string Message;
    public int Severity;
    public Range Range;
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
            FreeLibrary(_libraryHandle);
            _libraryHandle = IntPtr.Zero;
            return true;
        }
        catch (Exception ex)
        {
            L.Warning($"Unable to free handle to slang compiler's dll. {ex.Message}");
            return false;
        }
    }

    public static unsafe bool CompileFromString(string inputString, out string compiledString)
    {
        if (String.IsNullOrEmpty(inputString) || !EnsureLibLoaded())
        {
            compiledString = String.Empty;
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
                if ((ulong)result.len < 1)
                {
                    compiledString = String.Empty;
                    return false;
                }
                compiledString = result.AsString();
                return true;
            }
            finally
            {
                result.Drop();
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

    public static unsafe Line TokenizeLine(string inputString)
    {
        if (string.IsNullOrEmpty(inputString) || !EnsureLibLoaded())
        {
            return new Line(inputString);
        }

        fixed (char* ptrInputStr = inputString)
        {
            var strRef = new slice_ref_uint16_t
            {
                len = (UIntPtr)inputString.Length,
                ptr = (ushort*)ptrInputStr,
            };

            var tokens = Ffi.tokenize_line(strRef);

            return tokens.ToLine(inputString);
        }
    }

    private static string ExtractNativeLibrary(string libName)
    {
        string destinationPath = Path.Combine(Path.GetTempPath(), libName);

        Assembly assembly = Assembly.GetExecutingAssembly();

        using (Stream stream = assembly.GetManifestResourceStream(libName))
        {
            if (stream == null)
            {
                L.Error(
                    $"{libName} not found. This means it was not embedded in the mod. Please contact the mod author!"
                );
                return "";
            }

            try
            {
                using (FileStream fileStream = new FileStream(destinationPath, FileMode.Create))
                {
                    stream.CopyTo(fileStream);
                }
                return destinationPath;
            }
            catch (IOException e)
            {
                L.Warning($"Could not overwrite {libName} (it might be in use): {e.Message}");
                return "";
            }
        }
    }
}
