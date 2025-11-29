using System;
using System.Text;
using StationeersIC10Editor;

namespace Slang
{
    public static unsafe class SlangExtensions
    {
        /**
         * <summary>
         * This is a helper method to convert a Rust struct for a string pointer
         * into a C# style string.
         * </summary>
         */
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

        /**
         * <summary>This will free a Rust string struct. Because this is a pointer to a struct, this memory
         * is managed by Rust, therefor it must be freed by Rust
         * </summary>
         */
        public static void Drop(this Vec_uint8_t vec)
        {
            Ffi.free_string(vec);
        }

        /**
         * <summary>This helper converts a Rust vec to a C# List. This handles freeing the
         * Rust allocation after the List is created, there is no need to Drop this memory.
         * </summary>
         */
        public static Line AsList(this Vec_FfiToken_t vec)
        {
            L.Info("Converting output into a C# List.");
            var list = new Line();
            L.Info("Created new `Line`.");
            list.Capacity = (int)vec.len;
            L.Info("Changed `Capacity` to be returned Vec's len");

            var currentPtr = vec.ptr;

            // Iterate through the raw memory array
            for (int i = 0; i < (int)vec.len; i++)
            {
                // Dereference pointer to get the struct at index i
                FfiToken_t token = currentPtr[i];

                var newToken = new Token(token.text.AsString(), token.column);

                list.Add(newToken);
            }

            Ffi.free_ffi_token_vec(vec);

            return list;
        }
    }
}
