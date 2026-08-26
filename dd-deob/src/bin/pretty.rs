use oxc_allocator::Allocator;
use oxc_codegen::Codegen;
use std::io::Write;

fn main() {
    let mut args = std::env::args();
    let _ = args.next();
    let Some(input) = args.next() else {
        eprintln!("usage: dd-pretty <input.js> <output.js>");
        std::process::exit(1);
    };
    let Some(output) = args.next() else {
        eprintln!("usage: dd-pretty <input.js> <output.js>");
        std::process::exit(1);
    };
    let raw = std::fs::read_to_string(&input).unwrap();
    let alloc = Allocator::with_capacity(raw.len() * 4);
    let program = dd_deob::parse::parse_js(&alloc, &raw);
    let code = Codegen::new().build(&program).code;
    let mut f = std::fs::File::create(&output).unwrap();
    std::io::BufWriter::new(&mut f).write_all(code.as_bytes()).unwrap();
}
