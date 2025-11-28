using System;
using System.Text;
using StationeersIC10Editor;

namespace Slang
{
    public static unsafe class SlangExtensions
    {
        // 1. Convert the Rust Byte Vector (Vec_uint8_t) to a C# String
        public static string AsString(this Vec_uint8_t vec)
        {
            if (vec.ptr == null || vec.len == UIntPtr.Zero)
            {
                return string.Empty;
            }

            // Rust strings are UTF-8. Read bytes from raw pointer.
            var toReturn = Encoding.UTF8.GetString(vec.ptr, (int)vec.len);

            return toReturn;
        }

        public static void Drop(this Vec_uint8_t vec)
        {
            Ffi.free_string(vec);
        }

        // 2. Convert Rust Token Vector to C# List
        public static Line AsList(this Vec_FfiToken_t vec)
        {
            var list = new Line();
            list.Capacity = (int)vec.len;

            var currentPtr = vec.ptr;

            // Iterate through the raw memory array
            for (int i = 0; i < (int)vec.len; i++)
            {
                // Dereference pointer to get the struct at index i
                FfiToken_t token = currentPtr[i];

                var newToken = new Token(token.text.AsString(), token.column);
                newToken.Error = token.error.AsString();

                list.Add(newToken);
            }

            Ffi.free_ffi_token_vec(vec);

            return list;
        }
    }
}
