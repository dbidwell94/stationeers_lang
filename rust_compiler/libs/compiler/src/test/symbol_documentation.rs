#[cfg(test)]
mod test {
    use anyhow::Result;

    #[test]
    fn test_variable_doc_comment() -> Result<()> {
        let metadata = compile!(metadata "/// this is a documented variable\nlet myVar = 42;");

        let var_symbol = metadata
            .symbols
            .iter()
            .find(|s| s.name == "myVar")
            .expect("myVar symbol not found");

        assert_eq!(
            var_symbol.description.as_ref().map(|d| d.as_ref()),
            Some("this is a documented variable")
        );
        Ok(())
    }

    #[test]
    fn test_const_doc_comment() -> Result<()> {
        let metadata = compile!(metadata "/// const documentation\nconst myConst = 100;");

        let const_symbol = metadata
            .symbols
            .iter()
            .find(|s| s.name == "myConst")
            .expect("myConst symbol not found");

        assert_eq!(
            const_symbol.description.as_ref().map(|d| d.as_ref()),
            Some("const documentation")
        );
        Ok(())
    }

    #[test]
    fn test_device_doc_comment() -> Result<()> {
        let metadata = compile!(metadata "/// device documentation\ndevice myDevice = \"d0\";");

        let device_symbol = metadata
            .symbols
            .iter()
            .find(|s| s.name == "myDevice")
            .expect("myDevice symbol not found");

        assert_eq!(
            device_symbol.description.as_ref().map(|d| d.as_ref()),
            Some("device documentation")
        );
        Ok(())
    }

    #[test]
    fn test_function_doc_comment() -> Result<()> {
        let metadata = compile!(metadata "/// function documentation\nfn test() { }");

        let fn_symbol = metadata
            .symbols
            .iter()
            .find(|s| s.name == "test")
            .expect("test symbol not found");

        assert_eq!(
            fn_symbol.description.as_ref().map(|d| d.as_ref()),
            Some("function documentation")
        );
        Ok(())
    }

    #[test]
    fn test_syscall_documentation() -> Result<()> {
        let metadata = compile!(metadata "fn test() { clr(d0); }");

        let clr_symbol = metadata
            .symbols
            .iter()
            .find(|s| s.name == "clr")
            .expect("clr syscall not found");

        // clr should have its built-in documentation
        assert!(clr_symbol.description.is_some());
        assert!(!clr_symbol.description.as_ref().unwrap().is_empty());
        Ok(())
    }

    #[test]
    fn test_variable_references_have_tooltips() -> Result<()> {
        let metadata = compile!(metadata "/// documented variable\nlet myVar = 5;\nlet x = myVar + 2;\nmyVar = 10;");

        // Count how many times 'myVar' appears in symbols
        let myvar_symbols: Vec<_> = metadata
            .symbols
            .iter()
            .filter(|s| s.name == "myVar")
            .collect();

        // We should have at least 2: declaration + 1 reference (in myVar + 2)
        // The assignment `myVar = 10` is a write, not a read, so doesn't create a reference
        assert!(
            myvar_symbols.len() >= 2,
            "Expected at least 2 'myVar' symbols (declaration + reference), got {}",
            myvar_symbols.len()
        );

        // All should have the same description
        let expected_desc = "documented variable";
        for sym in &myvar_symbols {
            assert_eq!(
                sym.description.as_ref().map(|d| d.as_ref()),
                Some(expected_desc),
                "Symbol description mismatch at {:?}",
                sym.span
            );
        }
        Ok(())
    }
}
