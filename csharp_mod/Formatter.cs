namespace Slang;

using StationeersIC10Editor;

public class SlangFormatter : ICodeFormatter
{
    public static readonly uint ColorInstruction = ColorFromHTML("#ffff00");
    public static readonly uint ColorString = ColorFromHTML("#ce9178");

    public override Line ParseLine(string line)
    {
        return Marshal.TokenizeLine(line);
    }

    public override string Compile()
    {
        L.Info("ICodeFormatter attempted to compile source code.");
        return this.Lines.RawText;
    }
}
