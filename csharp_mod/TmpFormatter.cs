using System;
using System.Text.RegularExpressions;

namespace Slang;

public static class TextMeshProFormatter
{
    private const string CODE_COLOR = "#FFD700";           // Gold
    private const string LINK_COLOR = "#0099FF";           // Blue
    private const string QUOTE_COLOR = "#90EE90";          // Light Green

    public static string FromMarkdown(string markdown)
    {
        if (string.IsNullOrEmpty(markdown))
            return "";

        // Normalize Line Endings
        string text = markdown.Replace("\r\n", "\n");

        // Process code blocks FIRST (``` ... ```)
        text = Regex.Replace(
            text,
            @"```[^\n]*\n(.*?)\n```",
            match =>
            {
                var codeContent = match.Groups[1].Value;
                return $"<color={CODE_COLOR}>{codeContent}</color>";
            },
            RegexOptions.Singleline
        );

        // Process headers - check for 1-6 hashes
        text = Regex.Replace(text, @"^#{1}\s+(.+)$", "<size=120%><b>$1</b></size>", RegexOptions.Multiline);
        text = Regex.Replace(text, @"^#{2}\s+(.+)$", "<size=110%><b>$1</b></size>", RegexOptions.Multiline);
        text = Regex.Replace(text, @"^#{3}\s+(.+)$", "<size=100%><b>$1</b></size>", RegexOptions.Multiline);
        text = Regex.Replace(text, @"^#{4}\s+(.+)$", "<size=90%><b>$1</b></size>", RegexOptions.Multiline);
        text = Regex.Replace(text, @"^#{5}\s+(.+)$", "<size=80%><b>$1</b></size>", RegexOptions.Multiline);
        text = Regex.Replace(text, @"^#{6}\s+(.+)$", "<size=70%><b>$1</b></size>", RegexOptions.Multiline);

        // Process markdown links [text](url)
        text = Regex.Replace(
            text,
            @"\[([^\]]+)\]\(([^\)]+)\)",
            $"<color={LINK_COLOR}><u>$1</u></color>"
        );

        // Process inline code (`code`)
        text = Regex.Replace(text, @"`([^`]+)`", $"<color={CODE_COLOR}>$1</color>");

        // Process bold (**text**)
        text = Regex.Replace(text, @"\*\*(.+?)\*\*", "<b>$1</b>");

        // Process italics (*text*)
        text = Regex.Replace(text, @"(?<!\*)\*(?!\*)(.+?)(?<!\*)\*(?!\*)", "<i>$1</i>");

        // Process block quotes (> text)
        text = Regex.Replace(
            text,
            @"^>\s+(.+)$",
            $"<color={QUOTE_COLOR}><i>$1</i></color>",
            RegexOptions.Multiline
        );

        // Process unordered lists (- items)
        text = Regex.Replace(
            text,
            @"^-\s+(.+)$",
            "  • $1",
            RegexOptions.Multiline
        );

        return text;
    }
}
