using System;
using StationeersIC10Editor;

namespace Slang
{
    public static class Marshal
    {
        public static unsafe Line TokenizeLine(string source)
        {
            if (String.IsNullOrEmpty(source))
            {
                return new Line();
            }

            L.Info("Input string not empty");

            fixed (char* ptrString = source)
            {
                L.Info("In `fixed` block.");
                var input = new slice_ref_uint16_t
                {
                    ptr = (ushort*)ptrString,
                    len = (UIntPtr)source.Length,
                };
                L.Info("Calling tokenize_line");
                return Ffi.tokenize_line(input).AsList();
            }
        }

        public static unsafe bool CompileFromString(string inputString, out string compiledString)
        {
            if (String.IsNullOrEmpty(inputString))
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
    }
}
