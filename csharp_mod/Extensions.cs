namespace Slang;

using System;
using System.Collections.Generic;
using System.Text;
using Assets.Scripts.UI;
using StationeersIC10Editor;

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
    public static Line ToLine(this Vec_FfiToken_t vec, string sourceText)
    {
        var list = new Line(sourceText);

        var currentPtr = vec.ptr;

        // Iterate through the raw memory array
        for (int i = 0; i < (int)vec.len; i++)
        {
            var token = currentPtr[i];

            var color = GetColorForKind(token.token_kind);

            int colIndex = token.column - 1;
            if (colIndex < 0)
                colIndex = 0;

            var semanticToken = new SemanticToken(
                0,
                colIndex,
                token.length,
                color,
                token.token_kind,
                0,
                token.tooltip.AsString()
            );

            string errMsg = token.error.AsString();
            if (!string.IsNullOrEmpty(errMsg))
            {
                semanticToken.IsError = true;
                semanticToken.Data = errMsg;
                semanticToken.Color = ICodeFormatter.ColorError;
            }
            list.AddToken(semanticToken);
        }

        Ffi.free_ffi_token_vec(vec);

        return list;
    }

    public static unsafe List<Diagnostic> ToList(this Vec_FfiDiagnostic_t vec)
    {
        var toReturn = new List<Diagnostic>((int)vec.len);

        var currentPtr = vec.ptr;

        for (int i = 0; i < (int)vec.len; i++)
        {
            var item = currentPtr[i];

            toReturn.Add(
                new Slang.Diagnostic
                {
                    Message = item.message.AsString(),
                    Severity = item.severity,
                    Range = new Slang.Range
                    {
                        EndCol = Math.Max(item.range.end_col - 2, 0),
                        EndLine = item.range.end_line - 1,
                        StartCol = Math.Max(item.range.start_col - 2, 0),
                        StartLine = item.range.end_line - 1,
                    },
                }
            );
        }

        Ffi.free_ffi_diagnostic_vec(vec);
        return toReturn;
    }

    private static uint GetColorForKind(uint kind)
    {
        switch (kind)
        {
            case 1:
                return SlangFormatter.ColorString; // String
            case 2:
                return SlangFormatter.ColorString; // Number
            case 3:
                return SlangFormatter.ColorInstruction; // Boolean
            case 4:
                return SlangFormatter.ColorSelection; // Keyword
            case 5:
                return SlangFormatter.ColorLineNumber; // Identifier
            case 6:
                return SlangFormatter.ColorDefault; // Symbol
            default:
                return SlangFormatter.ColorDefault;
        }
    }

    public static unsafe List<StationpediaPage> ToList(this Vec_FfiDocumentedItem_t vec)
    {
        var toReturn = new List<StationpediaPage>((int)vec.len);

        var currentPtr = vec.ptr;

        for (int i = 0; i < (int)vec.len; i++)
        {
            var doc = currentPtr[i];
            var docItemName = doc.item_name.AsString();

            var formattedText = TextMeshProFormatter.FromMarkdown(doc.docs.AsString());

            var pediaPage = new StationpediaPage(
                $"slang-item-{docItemName}",
                docItemName,
                formattedText
            );

            pediaPage.Text = formattedText;
            pediaPage.Description = formattedText;
            pediaPage.ParsePage();

            toReturn.Add(pediaPage);
        }

        Ffi.free_docs_vec(vec);
        return toReturn;
    }
}
