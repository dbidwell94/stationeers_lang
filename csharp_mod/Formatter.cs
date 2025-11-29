using StationeersIC10Editor;

namespace Slang
{
    public class SlangFormatter : ICodeFormatter
    {
        public override Line ParseLine(string line)
        {
            return Marshal.TokenizeLine(line);
        }
    }
}
