namespace Slang;

using System.Timers;
using StationeersIC10Editor;

public class SlangFormatter : ICodeFormatter
{
    private Timer _timer;

    public static readonly uint ColorInstruction = ColorFromHTML("#ffff00");
    public static readonly uint ColorString = ColorFromHTML("#ce9178");

    public SlangFormatter()
    {
        _timer = new Timer(250);

        this.OnCodeChanged += HandleCodeChanged;
    }

    public override string Compile()
    {
        L.Info("ICodeFormatter attempted to compile source code.");
        return this.Lines.RawText;
    }

    public override Line ParseLine(string line)
    {
        return new Line(line);
    }

    private void HandleCodeChanged()
    {
        _timer.Stop();
        _timer.Dispose();
        _timer = new Timer(250);
        _timer.Elapsed += (_, _) => HandleLsp();
    }

    private void HandleLsp() { }
}
