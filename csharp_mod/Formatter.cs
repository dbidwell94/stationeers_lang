namespace Slang;

using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading;
using System.Timers;
using Cysharp.Threading.Tasks;
using StationeersIC10Editor;

public class SlangFormatter : ICodeFormatter
{
    private CancellationTokenSource? _lspCancellationToken;
    private object _tokenLock = new();

    public static readonly uint ColorInstruction = ColorFromHTML("#ffff00");
    public static readonly uint ColorString = ColorFromHTML("#ce9178");

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

    public override Line ParseLine(string line)
    {
        return Marshal.TokenizeLine(line);
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

    private void OnTimerElapsed(object sender, ElapsedEventArgs e) { }

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

            line.ClearTokens();

            Dictionary<int, SemanticToken> lineDict = Marshal
                .TokenizeLine(line.Text)
                .Tokens.ToDictionary((t) => t.Column);

            if (dict.ContainsKey(lineIndex))
            {
                foreach (var lineDiagnostic in dict[lineIndex])
                {
                    lineDict[(int)lineDiagnostic.Range.StartCol] = new SemanticToken
                    {
                        Column = Math.Abs((int)lineDiagnostic.Range.StartCol),
                        Length = Math.Abs(
                            (int)(lineDiagnostic.Range.EndCol - lineDiagnostic.Range.StartCol)
                        ),
                        Line = (int)lineIndex,
                        IsError = true,
                        Data = lineDiagnostic.Message,
                        Color = SlangFormatter.ColorError,
                    };
                }
            }

            foreach (var token in lineDict.Values)
            {
                line.AddToken(token);
            }
        }

        _linesWithErrors = new HashSet<uint>(dict.Keys);
    }
}
