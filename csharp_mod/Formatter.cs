namespace Slang;

using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using ImGuiNET;
using StationeersIC10Editor;
using StationeersIC10Editor.IC10;
using UnityEngine;

public class SlangFormatter : ICodeFormatter
{
    private CancellationTokenSource? _lspCancellationToken;
    private object _tokenLock = new();

    protected static Editor? Ic10Editor = null;
    private IC10CodeFormatter iC10CodeFormatter = new IC10CodeFormatter();
    private string ic10CompilationResult = "";
    private List<SourceMapEntry> ic10SourceMap = new();

    #region Colors
    public static readonly uint ColorControl = ColorFromHTML("#C586C0"); // Pink (if, return, loop)
    public static readonly uint ColorDeclaration = ColorFromHTML("#569CD6"); // Blue (let, device, fn)
    public static readonly uint ColorFunction = ColorFromHTML("#DCDCAA"); // Yellow (syscalls)
    public static readonly uint ColorString = ColorFromHTML("#CE9178"); // Orange
    public static new readonly uint ColorNumber = ColorFromHTML("#B5CEA8"); // Light Green
    public static readonly uint ColorBoolean = ColorFromHTML("#569CD6"); // Blue (true/false)
    public static readonly uint ColorIdentifier = ColorFromHTML("#9CDCFE"); // Light Blue (variables)
    public static new readonly uint ColorDefault = ColorFromHTML("#D4D4D4"); // White (punctuation ; { } )
    public static readonly uint ColorOperator = ColorFromHTML("#D4D4D4");
    #endregion

    private HashSet<uint> _linesWithErrors = new();
    private int _lastLineCount = -1;

    public SlangFormatter()
        : base()
    {
        OnCodeChanged += HandleCodeChanged;
        OnCaretMoved += UpdateIc10Formatter;
    }

    public static double MatchingScore(string input)
    {
        // Empty input is not valid Slang
        if (string.IsNullOrWhiteSpace(input))
            return 0d;

        // Run the compiler to get diagnostics
        var diagnostics = Marshal.DiagnoseSource(input);

        // Count the number of actual Errors (Severity 1).
        // We ignore Warnings (2), Info (3), etc.
        double errorCount = diagnostics.Count(d => d.Severity == 1);

        // Get the total line count to calculate error density
        double lineCount = input.Split('\n').Length;

        // Prevent division by zero
        if (lineCount == 0)
            return 0d;

        // Calculate score: Start at 1.0 (100%) and subtract the ratio of errors per line.
        // Example: 10 lines with 0 errors = 1.0
        // Example: 10 lines with 2 errors = 0.8
        // Example: 10 lines with 10+ errors = 0.0
        double score = 1.0d - (errorCount / lineCount);

        // Clamp the result between 0 and 1
        return Math.Max(0d, Math.Min(1d, score));
    }

    public override string Compile()
    {
        return this.Lines.RawText;
    }

    public override StyledLine ParseLine(string line)
    {
        // We create the line first
        var styledLine = new StyledLine(line);

        // We get the semantic tokens (color + data)
        var tokens = Marshal.TokenizeLine(line);

        // We call update to create the basic tokens
        styledLine.Update(tokens);

        ReattachMetadata(styledLine, tokens);

        return styledLine;
    }

    private void HandleCodeChanged()
    {
        CancellationToken token;
        string inputSrc;
        lock (_tokenLock)
        {
            _lspCancellationToken?.Cancel();
            _lspCancellationToken = new CancellationTokenSource();
            token = _lspCancellationToken.Token;
            inputSrc = this.RawText;
        }

        _ = HandleLsp(inputSrc, token);
    }

    private async Task HandleLsp(string inputSrc, CancellationToken cancellationToken)
    {
        try
        {
            if (cancellationToken.IsCancellationRequested)
                return;

            await Task.Delay(200, cancellationToken: cancellationToken);

            if (cancellationToken.IsCancellationRequested)
                return;

            // Running this potentially CPU intensive work on a background thread.
            var dict = await Task.Run(
                () =>
                {
                    return Marshal
                        .DiagnoseSource(inputSrc)
                        .GroupBy(d => d.Range.StartLine)
                        .ToDictionary(g => g.Key);
                },
                cancellationToken
            );

            ApplyDiagnostics(dict);

            // If we have valid code, update the IC10 output
            if (dict.Count > 0)
            {
                return;
            }

            var (compilationSuccess, compiled, sourceMap) = await Task.Run(
                () =>
                {
                    var successful = Marshal.CompileFromString(
                        inputSrc,
                        out var compiled,
                        out var sourceMap
                    );
                    return (successful, compiled, sourceMap);
                },
                cancellationToken
            );

            if (compilationSuccess)
            {
                ic10CompilationResult = compiled;
                ic10SourceMap = sourceMap;
                UpdateIc10Formatter();
            }
        }
        catch (OperationCanceledException) { }
        catch (Exception ex)
        {
            L.Error(ex.Message);
        }
    }

    private void UpdateIc10Formatter()
    {
        if (Ic10Editor is null)
        {
            var tab = Editor.ParentTab;
            iC10CodeFormatter = new IC10CodeFormatter();
            Ic10Editor = new Editor(Editor.KeyHandler);
            Ic10Editor.IsReadOnly = true;
            iC10CodeFormatter.Editor = Ic10Editor;
            tab.AddEditor(Ic10Editor);
        }

        var caretPos = Editor.CaretPos.Line;

        // get the slang sourceMap at the current editor line
        var lines = ic10SourceMap.FindAll(entry =>
            entry.SlangSource.StartLine == caretPos || entry.SlangSource.EndLine == caretPos
        );

        // extract the current "context" of the ic10 compilation. The current Slang source line
        // should be directly next to the compiled IC10 source line, and we should highlight the
        // IC10 code that directly represents the Slang source

        Ic10Editor.ResetCode(ic10CompilationResult);

        if (lines.Count() < 1)
        {
            return;
        }
        // get the total range of the IC10 source for the selected Slang line
        var max = lines.Max(line => line.Ic10Line);
        var min = lines.Min(line => line.Ic10Line);

        // highlight all the IC10 lines that are within the specified range
        foreach (var index in Enumerable.Range((int)min, (int)(max - min) + 1))
        {
            var lineText = Ic10Editor.Lines[index].Text;

            var newLine = new StyledLine(
                lineText,
                [
                    new SemanticToken
                    {
                        Column = 0,
                        Length = lineText.Length,
                        Line = index,
                        Background = ColorIdentifier,
                        Color = ColorFromHTML("black"),
                    },
                ]
            );

            Ic10Editor.Lines[index] = newLine;
        }
    }

    // This runs on the Main Thread
    private void ApplyDiagnostics(Dictionary<uint, IGrouping<uint, Diagnostic>> dict)
    {
        HashSet<uint> linesToRefresh;

        // If the line count has changed (lines added/deleted), indices have shifted.
        // We must refresh ALL lines to ensure any line that shifted into a new position
        // gets scrubbed of its old visual state.
        if (this.Lines.Count != _lastLineCount)
        {
            linesToRefresh = new HashSet<uint>();
            for (int i = 0; i < this.Lines.Count; i++)
            {
                linesToRefresh.Add((uint)i);
            }
        }
        else
        {
            linesToRefresh = new HashSet<uint>(dict.Keys);
            linesToRefresh.UnionWith(_linesWithErrors);
        }

        _lastLineCount = this.Lines.Count;

        foreach (var lineIndex in linesToRefresh)
        {
            // safety check for out of bounds (in case lines were deleted)
            if (lineIndex >= this.Lines.Count)
                continue;

            var line = this.Lines[(int)lineIndex];

            if (line is null)
                continue;

            // 1. Get base syntax tokens, converting to a dictionary for ease in deduping error tokens
            var allTokensDict = Marshal.TokenizeLine(line.Text).ToDictionary((k) => k.Column);

            // 2. Replace valid tokens with error tokens if present
            if (dict.ContainsKey(lineIndex))
            {
                foreach (var lineDiagnostic in dict[lineIndex])
                {
                    var column = Math.Abs((int)lineDiagnostic.Range.StartCol);

                    allTokensDict[column] = new SemanticToken(
                        line: (int)lineIndex,
                        column,
                        length: Math.Abs(
                            (int)(lineDiagnostic.Range.EndCol - lineDiagnostic.Range.StartCol)
                        ),
                        type: 0,
                        style: ICodeFormatter.ColorError,
                        data: lineDiagnostic.Message,
                        isError: true
                    );
                }
            }

            var allTokens = allTokensDict.Values.ToList();

            // 3. Update the line (this clears existing tokens and uses the list we just built)
            line.Update(allTokens);

            ReattachMetadata(line, allTokens);
        }

        _linesWithErrors = new HashSet<uint>(dict.Keys);
    }

    // Helper to map SemanticToken data (tooltips/errors) back to the tokens in the line
    private void ReattachMetadata(StyledLine line, List<SemanticToken> semanticTokens)
    {
        foreach (var semToken in semanticTokens)
        {
            // Skip tokens without data
            if (string.IsNullOrEmpty(semToken.Data))
                continue;

            // Find the corresponding Token in the line
            var token = line.GetTokenAt(semToken.Column);
            if (token != null)
            {
                // Wrap text to avoid "wide as monitor" tooltips
                var wrappedMessage = WrapText(semToken.Data, 50);
                var msgText = CreateStyledTextFromLines(
                    wrappedMessage,
                    semToken.IsError ? ICodeFormatter.ColorError : ICodeFormatter.ColorDefault
                );

                if (semToken.IsError)
                {
                    token.Error = msgText;
                }
                else
                {
                    token.Tooltip = msgText;
                }
            }
        }
    }

    // Helper to create a StyledText object from a list of strings
    private StyledText CreateStyledTextFromLines(List<string> lines, uint color)
    {
        var styledText = new StyledText();
        foreach (var lineContent in lines)
        {
            var l = new StyledLine(lineContent);
            l.Add(new Token(0, lineContent, new Style(color)));
            styledText.Add(l);
        }
        return styledText;
    }

    // Text wrapper that preserves paragraph structure but enforces width
    private List<string> WrapText(string text, int maxLineLength)
    {
        var lines = new List<string>();
        if (string.IsNullOrEmpty(text))
            return lines;

        // Normalize newlines and split by paragraph
        var paragraphs = text.Replace("\r\n", "\n").Split('\n');

        foreach (var paragraph in paragraphs)
        {
            // Preserve empty lines (paragraph breaks)
            if (string.IsNullOrWhiteSpace(paragraph))
            {
                lines.Add("");
                continue;
            }

            var words = paragraph.Split(' ');
            var currentLine = "";

            foreach (var word in words)
            {
                // If adding the next word exceeds max length...
                if (currentLine.Length + word.Length + 1 > maxLineLength)
                {
                    // Push current line if it has content
                    if (currentLine.Length > 0)
                    {
                        lines.Add(currentLine.TrimEnd());
                        currentLine = "";
                    }
                }

                if (currentLine.Length > 0)
                    currentLine += " ";
                currentLine += word;
            }

            // Flush remaining content
            if (currentLine.Length > 0)
            {
                lines.Add(currentLine.TrimEnd());
            }
        }
        return lines;
    }
}
