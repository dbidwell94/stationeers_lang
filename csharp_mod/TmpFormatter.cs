using System.Text.RegularExpressions;

namespace Slang;

public static class TextMeshProFormatter
{
    private const string CODE_COLOR = "#FFD700";

    public static string FromMarkdown(string markdown)
    {
        if (string.IsNullOrEmpty(markdown))
            return "";

        // 1. Normalize Line Endings
        string text = markdown.Replace("\r\n", "\n");

        // 2. Handle Code Blocks (```)
        text = Regex.Replace(
            text,
            @"```\s*(.*?)\s*```",
            match =>
            {
                var codeContent = match.Groups[1].Value;
                return $"<color={CODE_COLOR}>{codeContent}</color>"; // Gold color for code
            },
            RegexOptions.Singleline
        );

        text = Regex.Replace(
            text,
            @"^\s*##\s+(.+)$",
            "<size=110%><color=#ffffff><b>$1</b></color></size>",
            RegexOptions.Multiline
        );

        // 3. Handle # Headers SECOND (General)
        text = Regex.Replace(
            text,
            @"^\s*#\s+(.+)$",
            "<size=120%><color=#ffffff><b>$1</b></color></size>",
            RegexOptions.Multiline
        );

        // 4. Handle Inline Code (`code`)
        text = Regex.Replace(text, @"`([^`]+)`", $"<color={CODE_COLOR}>$1</color>");

        // 5. Handle Bold (**text**)
        text = Regex.Replace(text, @"\*\*(.+?)\*\*", "<b>$1</b>");

        // 6. Handle Italics (*text*)
        text = Regex.Replace(text, @"\*(.+?)\*", "<i>$1</i>");

        // 7. Convert Newlines to TMP Line Breaks
        // Stationpedia needs <br> or explicit newlines.
        // Often just ensuring \n is preserved is enough, but <br> is safer for HTML-like parsers.
        text = text.Replace("\n", "<br>");

        return text;
    }
}
