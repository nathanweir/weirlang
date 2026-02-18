#![no_main]

use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &[u8]| {
    if let Ok(s) = std::str::from_utf8(data) {
        // The property: parse() never panics on any input.
        let _ = weir_parser::parse(s);
    }
});
