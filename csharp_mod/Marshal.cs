using System;
using System.Text;
using StationeersIC10Editor;

namespace Slang
{
    public static class Marshal
    {
        public static unsafe Line TokenizeLine(string input)
        {
            if (String.IsNullOrEmpty(input))
            {
                return new Line();
            }

            // Make sure the string is a null terminated string
            if (input[input.Length - 1] != '\0')
            {
                input += '\0';
            }

            var strBytes = Encoding.UTF8.GetBytes(input);

            fixed (byte* ptrString = strBytes)
            {
                return Ffi.tokenize_line(ptrString).AsList();
            }
        }
    }
}
