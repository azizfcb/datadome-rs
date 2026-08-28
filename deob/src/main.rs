use deob::deobfuscate;

fn main() {
    let mut args = std::env::args().skip(1);
    let input = args.next().expect("usage: deob <in.js> [out.js]");
    let output = args.next();
    let source = std::fs::read_to_string(&input).expect("read");

    let code = match deobfuscate(&source) {
        Ok(code) => code,
        Err(e) => {
            eprintln!("parse: {e}");
            std::process::exit(1);
        }
    };

    match output {
        Some(p) => std::fs::write(p, code).expect("write"),
        None => println!("{}", &code[..code.len().min(2000)]),
    }
}
