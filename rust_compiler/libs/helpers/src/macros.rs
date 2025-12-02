#[macro_export]
macro_rules! documented {
    // -------------------------------------------------------------------------
    // Internal Helper: Filter doc comments
    // -------------------------------------------------------------------------

    // Case 1: Doc comment. Return Some("string").
    // We match the specific structure of a doc attribute.
    (@doc_filter #[doc = $doc:expr]) => {
        Some($doc)
    };

    // Case 2: Other attributes (derives, etc.). Return None.
    // We catch any other token sequence inside the brackets.
    (@doc_filter #[$($attr:tt)*]) => {
        None
    };

    // -------------------------------------------------------------------------
    // Internal Helper: Match patterns for `match self`
    // -------------------------------------------------------------------------
    (@arm $name:ident $variant:ident) => {
        $name::$variant
    };
    (@arm $name:ident $variant:ident ( $($tuple:tt)* )) => {
        $name::$variant(..)
    };
    (@arm $name:ident $variant:ident { $($structure:tt)* }) => {
        $name::$variant{..}
    };

    // -------------------------------------------------------------------------
    // Main Macro Entry Point
    // -------------------------------------------------------------------------
    (
        $(#[$enum_attr:meta])* $vis:vis enum $name:ident {
            $(
                // Capture attributes as a sequence of token trees inside brackets
                // to avoid "local ambiguity" and handle multi-token attributes (like doc="...").
                $(#[ $($variant_attr:tt)* ])*
                $variant:ident
                $( ($($tuple:tt)*) )?
                $( {$($structure:tt)*} )?
            ),* $(,)?
        }
    ) => {
        // 1. Generate the actual Enum definition
        $(#[$enum_attr])*
        $vis enum $name {
            $(
                $(#[ $($variant_attr)* ])*
                $variant
                $( ($($tuple)*) )?
                $( {$($structure)*} )?,
            )*
        }

        // 2. Implement the Trait
        impl Documentation for $name {
            fn docs(&self) -> String {
                match self {
                    $(
                        documented!(@arm $name $variant $( ($($tuple)*) )? $( {$($structure)*} )? ) => {
                            // Create a temporary array of Option<&str> for all attributes
                            let doc_lines: &[Option<&str>] = &[
                                $(
                                    documented!(@doc_filter #[ $($variant_attr)* ])
                                ),*
                            ];

                            // Filter out the Nones (non-doc attributes), join, and return
                            doc_lines.iter()
                                .filter_map(|&d| d)
                                .collect::<Vec<_>>()
                                .join("\n")
                                .trim()
                                .to_string()
                        }
                    )*
                }
            }
        }
    };
}
