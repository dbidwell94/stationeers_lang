namespace Slang;

using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using System.Timers;
using StationeersIC10Editor;

public class SlangFormatter : ICodeFormatter
{
    private CancellationTokenSource? _lspCancellationToken;
    private readonly SynchronizationContext? _mainThreadContext;
    private volatile bool IsDiagnosing = false;

    public static readonly uint ColorInstruction = ColorFromHTML("#ffff00");
    public static readonly uint ColorString = ColorFromHTML("#ce9178");

    private HashSet<uint> _linesWithErrors = new();

    public SlangFormatter()
    {
        // 1. Capture the Main Thread context.
        // This works because the Editor instantiates this class on the main thread.
        _mainThreadContext = SynchronizationContext.Current;
    }

    public override string Compile()
    {
        return this.Lines.RawText;
    }

    public override Line ParseLine(string line)
    {
        HandleCodeChanged();
        return Marshal.TokenizeLine(line);
    }

    private void HandleCodeChanged()
    {
        if (IsDiagnosing)
            return;

        _lspCancellationToken?.Cancel();
        _lspCancellationToken?.Dispose();

        _lspCancellationToken = new CancellationTokenSource();

        _ = Task.Run(() => HandleLsp(_lspCancellationToken.Token), _lspCancellationToken.Token);
    }

    private void OnTimerElapsed(object sender, ElapsedEventArgs e) { }

    private async Task HandleLsp(CancellationToken cancellationToken)
    {
        try
        {
            await Task.Delay(200, cancellationToken);

            if (cancellationToken.IsCancellationRequested)
            {
                return;
            }

            // 3. Dispatch the UI update to the Main Thread
            if (_mainThreadContext != null)
            {
                // Post ensures ApplyDiagnostics runs on the captured thread (Main Thread)
                _mainThreadContext.Post(_ => ApplyDiagnostics(), null);
            }
            else
            {
                // Fallback: If context is null (rare in Unity), try running directly
                // but warn, as this might crash if not thread-safe.
                L.Warning("SynchronizationContext was null. Attempting direct update (risky).");
                ApplyDiagnostics();
            }
        }
        finally { }
    }

    // This runs on the Main Thread
    private void ApplyDiagnostics()
    {
        List<Diagnostic> diagnosis = Marshal.DiagnoseSource(this.RawText);

        var dict = diagnosis.GroupBy(d => d.Range.StartLine).ToDictionary(g => g.Key);

        var linesToRefresh = new HashSet<uint>(dict.Keys);
        linesToRefresh.UnionWith(_linesWithErrors);

        IsDiagnosing = true;

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

        IsDiagnosing = false;
    }
}
