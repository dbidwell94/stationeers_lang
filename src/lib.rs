use compiler::Compiler;
use parser::Parser;
use std::{
    ffi::{CStr, CString},
    io::BufWriter,
};
use tokenizer::Tokenizer;

/// Takes a raw pointer to a string and compiles the `slang` code into valid IC10
/// # Safety
/// This must be called with a valid string pointer from C# (or wherever is calling this function)
#[no_mangle]
pub unsafe extern "C" fn compile_from_string(
    input_ptr: *const std::os::raw::c_char,
) -> *mut std::os::raw::c_char {
    if input_ptr.is_null() {
        return std::ptr::null_mut();
    }

    let c_str = unsafe { CStr::from_ptr(input_ptr) };

    let Ok(input_str) = c_str.to_str() else {
        return std::ptr::null_mut();
    };

    let mut writer = BufWriter::new(Vec::new());
    let tokenizer = Tokenizer::from(input_str);
    let parser = Parser::new(tokenizer);

    let compiler = Compiler::new(parser, &mut writer, None);

    let Ok(()) = compiler.compile() else {
        return std::ptr::null_mut();
    };

    let Ok(buffer) = writer.into_inner() else {
        return std::ptr::null_mut();
    };

    let c_string = CString::from_vec_unchecked(buffer);

    c_string.into_raw()
}

/// Takes ownership of the string pointer and drops it, freeing the memory
/// # Safety
/// Must be called with a valid string pointer
#[no_mangle]
pub unsafe extern "C" fn free_slang_string(input_ptr: *mut std::os::raw::c_char) {
    if input_ptr.is_null() {
        return;
    }

    unsafe {
        // Takes ownership of the input string, and then drops it immediately
        let _ = CString::from_raw(input_ptr);
    }
}
