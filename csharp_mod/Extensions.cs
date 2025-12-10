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
    public static List<SemanticToken> ToTokenList(this Vec_FfiToken_t vec)
    {
        var tokens = new List<SemanticToken>();

        var currentPtr = vec.ptr;

        // Iterate through the raw memory array
        for (int i = 0; i < (int)vec.len; i++)
        {
            var token = currentPtr[i];

            var color = GetColorForKind(token.token_kind);

            int colIndex = token.column;
            if (colIndex < 0)
                colIndex = 0;

            var semanticToken = new SemanticToken(
                0,
                colIndex,
                token.length,
                token.token_kind,
                color,
                token.tooltip.AsString()
            );

            string errMsg = token.error.AsString();
            if (!string.IsNullOrEmpty(errMsg))
            {
                semanticToken.IsError = true;
                semanticToken.Data = errMsg;
                semanticToken.Color = ICodeFormatter.ColorError;
            }
            tokens.Add(semanticToken);
        }

        Ffi.free_ffi_token_vec(vec);

        return tokens;
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
                        EndCol = Math.Max(item.range.end_col, 0),
                        EndLine = item.range.end_line,
                        StartCol = Math.Max(item.range.start_col, 0),
                        StartLine = item.range.start_line,
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
            case 1: // Strings
                return SlangFormatter.ColorString;
            case 2: // Numbers
                return SlangFormatter.ColorNumber;
            case 3: // Booleans
                return SlangFormatter.ColorBoolean;

            case 4: // (if, else, loop)
                return SlangFormatter.ColorControl;
            case 5: // (let, const, device)
                return SlangFormatter.ColorDeclaration;

            case 6: // (variables)
                return SlangFormatter.ColorIdentifier;
            case 7: // (punctuation)
                return SlangFormatter.ColorDefault;

            case 8: // Comments
                return SlangFormatter.ColorComment;

            case 10: // (syscalls)
                return SlangFormatter.ColorFunction;

            case 11: // Comparisons
            case 12: // Math
            case 13: // Logic
                return SlangFormatter.ColorOperator;

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
