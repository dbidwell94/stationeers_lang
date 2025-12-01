namespace Slang;

using System.Collections.Generic;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using System.Timers;
using StationeersIC10Editor;

public class SlangFormatter : ICodeFormatter
{
    private System.Timers.Timer _timer;
    private CancellationTokenSource? _lspCancellationToken;
    private readonly SynchronizationContext? _mainThreadContext;
    private volatile bool IsDiagnosing = false;

    public static readonly uint ColorInstruction = ColorFromHTML("#ffff00");
    public static readonly uint ColorString = ColorFromHTML("#ce9178");

    private object _textLock = new();

    public SlangFormatter()
    {
        // 1. Capture the Main Thread context.
        // This works because the Editor instantiates this class on the main thread.
        _mainThreadContext = SynchronizationContext.Current;

        _timer = new System.Timers.Timer(250);
        _timer.AutoReset = false;
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

        _ = HandleLsp(_lspCancellationToken.Token, this.RawText);
    }

    private void OnTimerElapsed(object sender, ElapsedEventArgs e) { }

    private async Task HandleLsp(CancellationToken cancellationToken, string text)
    {
        try
        {
            await Task.Delay(500, cancellationToken);

            if (cancellationToken.IsCancellationRequested)
                return;

            List<Diagnostic> diagnosis = Marshal.DiagnoseSource(text);

            var dict = diagnosis
                .GroupBy(d => d.Range.StartLine)
                .ToDictionary(g => g.Key, g => g.ToList());

            // 3. Dispatch the UI update to the Main Thread
            if (_mainThreadContext != null)
            {
                // Post ensures ApplyDiagnostics runs on the captured thread (Main Thread)
                _mainThreadContext.Post(_ => ApplyDiagnostics(dict), null);
            }
            else
            {
                // Fallback: If context is null (rare in Unity), try running directly
                // but warn, as this might crash if not thread-safe.
                L.Warning("SynchronizationContext was null. Attempting direct update (risky).");
                ApplyDiagnostics(dict);
            }
        }
        finally { }
    }

    // This runs on the Main Thread
    private void ApplyDiagnostics(Dictionary<uint, List<Diagnostic>> dict)
    {
        IsDiagnosing = true;
        // Standard LSP uses 0-based indexing.
        for (int i = 0; i < this.Lines.Count; i++)
        {
            uint lineIndex = (uint)i;

            if (dict.TryGetValue(lineIndex, out var lineDiagnostics))
            {
                var line = this.Lines[i];
                if (line is null)
                {
                    continue;
                }

                var tokenMap = line.Tokens.ToDictionary((t) => t.Column);

                foreach (var diag in lineDiagnostics)
                {
                    var newToken = new SemanticToken
                    {
                        Column = (int)diag.Range.StartCol,
                        Length = (int)(diag.Range.EndCol - diag.Range.StartCol),
                        Line = i,
                        IsError = true,
                        Data = diag.Message,
                        Color = ICodeFormatter.ColorError,
                    };

                    L.Info(
                        $"Col: {newToken.Column} -- Length: {newToken.Length} -- Msg: {newToken.Data}"
                    );

                    tokenMap[newToken.Column] = newToken;
                }

                line.ClearTokens();

                foreach (var token in tokenMap.Values)
                {
                    line.AddToken(token);
                }
            }
        }
        IsDiagnosing = false;
    }
}
