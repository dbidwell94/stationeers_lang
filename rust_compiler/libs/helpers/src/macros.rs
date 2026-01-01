#[macro_export]
macro_rules! documented {
    // -------------------------------------------------------------------------
    // Internal Helper: Filter doc comments
    // -------------------------------------------------------------------------
    (@doc_filter #[doc = $doc:expr]) => {
        Some($doc)
    };

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
    // Entry Point 1: Enum with a single Lifetime (e.g. enum Foo<'a>)
    // -------------------------------------------------------------------------
    (
        $(#[$enum_attr:meta])* $vis:vis enum $name:ident < $lt:lifetime > {
            $($body:tt)*
        }
    ) => {
        documented!(@generate
            meta: [$(#[$enum_attr])*],
            vis: [$vis],
            name: [$name],
            generics: [<$lt>],
            body: [$($body)*]
        );
    };

    // -------------------------------------------------------------------------
    // Entry Point 2: Regular Enum (No Generics)
    // -------------------------------------------------------------------------
    (
        $(#[$enum_attr:meta])* $vis:vis enum $name:ident {
            $($body:tt)*
        }
    ) => {
        documented!(@generate
            meta: [$(#[$enum_attr])*],
            vis: [$vis],
            name: [$name],
            generics: [],
            body: [$($body)*]
        );
    };

    // -------------------------------------------------------------------------
    // Code Generator (Shared Logic)
    // -------------------------------------------------------------------------
    (@generate
        meta: [$(#[$enum_attr:meta])*],
        vis: [$vis:vis],
        name: [$name:ident],
        generics: [$($generics:tt)*],
        body: [
            $(
                $(#[ $($variant_attr:tt)* ])*
                $variant:ident
                $( ($($tuple:tt)*) )?
                $( {$($structure:tt)*} )?
            ),* $(,)?
        ]
    ) => {
        // 1. Generate the Enum Definition
        $(#[$enum_attr])*
        $vis enum $name $($generics)* {
            $(
                $(#[ $($variant_attr)* ])*
                $variant
                $( ($($tuple)*) )?
                $( {$($structure)*} )?,
            )*
        }

        // 2. Implement Documentation Trait
        // We apply the captured generics (e.g., <'a>) to both the impl and the type
        impl $($generics)* Documentation for $name $($generics)* {
            fn docs(&self) -> String {
                match self {
                    $(
                        documented!(@arm $name $variant $( ($($tuple)*) )? $( {$($structure)*} )? ) => {
                            let doc_lines: &[Option<&str>] = &[
                                $(
                                    documented!(@doc_filter #[ $($variant_attr)* ])
                                ),*
                            ];

                            let combined = doc_lines.iter()
                                .filter_map(|&d| d)
                                .collect::<Vec<_>>()
                                .join("\n");

                            $crate::dedent(&combined).trim().to_string()
                        }
                    )*
                }
            }

            #[allow(dead_code)]
            fn get_all_documentation() -> Vec<(&'static str, String)> {
                vec![
                    $(
                        (
                            stringify!($variant),
                            {
                                let doc_lines: &[Option<&str>] = &[
                                    $(
                                        documented!(@doc_filter #[ $($variant_attr)* ])
                                    ),*
                                ];

                                let combined = doc_lines.iter()
                                    .filter_map(|&d| d)
                                    .collect::<Vec<_>>()
                                    .join("\n");

                                $crate::dedent(&combined).trim().to_string()
                            }
                        )
                    ),*
                ]
            }
        }
    };
}
