namespace Slang;

using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading;
using Cysharp.Threading.Tasks;
using StationeersIC10Editor;

public class SlangFormatter : ICodeFormatter
{
    private CancellationTokenSource? _lspCancellationToken;
    private object _tokenLock = new();

    // VS Code Dark Theme Palette
    public static readonly uint ColorControl = ColorFromHTML("#C586C0"); // Pink (if, return, loop)
    public static readonly uint ColorDeclaration = ColorFromHTML("#569CD6"); // Blue (let, device, fn)
    public static readonly uint ColorFunction = ColorFromHTML("#DCDCAA"); // Yellow (syscalls)
    public static readonly uint ColorString = ColorFromHTML("#CE9178"); // Orange
    public static new readonly uint ColorNumber = ColorFromHTML("#B5CEA8"); // Light Green
    public static readonly uint ColorBoolean = ColorFromHTML("#569CD6"); // Blue (true/false)
    public static readonly uint ColorIdentifier = ColorFromHTML("#9CDCFE"); // Light Blue (variables)
    public static new readonly uint ColorDefault = ColorFromHTML("#D4D4D4"); // White (punctuation ; { } )

    // Operators are often the same color as default text in VS Code Dark,
    // but having a separate definition lets you tweak it (e.g. make them slightly darker or distinct)
    public static readonly uint ColorOperator = ColorFromHTML("#D4D4D4");

    private HashSet<uint> _linesWithErrors = new();

    public SlangFormatter()
        : base()
    {
        OnCodeChanged += HandleCodeChanged;
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
        L.Debug($"Parsing line for syntax highlighting: {line}");
        return new StyledLine(line, Marshal.TokenizeLine(line));
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

        HandleLsp(inputSrc, token).Forget();
    }

    private async UniTaskVoid HandleLsp(string inputSrc, CancellationToken cancellationToken)
    {
        try
        {
            await UniTask.SwitchToThreadPool();

            if (cancellationToken.IsCancellationRequested)
                return;

            await System.Threading.Tasks.Task.Delay(200, cancellationToken: cancellationToken);

            if (cancellationToken.IsCancellationRequested)
                return;

            var dict = Marshal
                .DiagnoseSource(inputSrc)
                .GroupBy(d => d.Range.StartLine)
                .ToDictionary(g => g.Key);

            await UniTask.Yield(PlayerLoopTiming.Update, cancellationToken);

            ApplyDiagnostics(dict);
        }
        catch (OperationCanceledException) { }
        catch (Exception ex)
        {
            L.Error(ex.Message);
        }
    }

    // This runs on the Main Thread
    private void ApplyDiagnostics(Dictionary<uint, IGrouping<uint, Diagnostic>> dict)
    {
        var linesToRefresh = new HashSet<uint>(dict.Keys);
        linesToRefresh.UnionWith(_linesWithErrors);

        foreach (var lineIndex in linesToRefresh)
        {
            // safety check for out of bounds (in case lines were deleted)
            if (lineIndex >= this.Lines.Count)
                continue;

            var line = this.Lines[(int)lineIndex];

            if (line is null)
                continue;

            // 1. Get base syntax tokens
            var allTokens = Marshal.TokenizeLine(line.Text);

            // 2. Overlay error tokens if diagnostics exist for this line
            if (dict.ContainsKey(lineIndex))
            {
                foreach (var lineDiagnostic in dict[lineIndex])
                {
                    allTokens.Add(
                        new SemanticToken(
                            line: (int)lineIndex,
                            column: Math.Abs((int)lineDiagnostic.Range.StartCol),
                            length: Math.Abs(
                                (int)(lineDiagnostic.Range.EndCol - lineDiagnostic.Range.StartCol)
                            ),
                            type: 0,
                            style: ICodeFormatter.ColorError,
                            data: lineDiagnostic.Message,
                            isError: true
                        )
                    );
                }
            }

            line.Update(allTokens);
        }

        _linesWithErrors = new HashSet<uint>(dict.Keys);
    }
}
