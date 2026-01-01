use crc32fast::hash as crc32_hash;

/// This function takes an input which is meant to be hashed via the CRC32 algorithm, but it then
/// converts the generated UNSIGNED number into it's SIGNED counterpart.
pub fn crc_hash_signed(input: &str) -> i128 {
    let hash = crc32_hash(input.as_bytes());

    // in stationeers, crc32 is a SIGNED int.
    let hash_value_i32 = i32::from_le_bytes(hash.to_le_bytes());

    hash_value_i32 as i128
}

/// Removes common leading whitespace from all lines in a string (dedent).
/// This is useful for cleaning up documentation strings that have uniform indentation.
pub fn dedent(text: &str) -> String {
    let lines: Vec<&str> = text.lines().collect();

    // Find minimum indentation (excluding empty lines)
    let mut min_indent = usize::MAX;
    for line in &lines {
        if !line.trim().is_empty() {
            let indent = line.len() - line.trim_start().len();
            min_indent = min_indent.min(indent);
        }
    }

    // If no lines or all empty, return as-is
    if min_indent == usize::MAX {
        return text.to_string();
    }

    // Remove the common indentation
    lines
        .iter()
        .map(|line| {
            if line.trim().is_empty() {
                ""
            } else if line.len() >= min_indent {
                &line[min_indent..]
            } else {
                line
            }
        })
        .collect::<Vec<_>>()
        .join("\n")
}
