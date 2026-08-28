use wasm::{attest, extract, flat, ir, parse, print};

fn main() {
    let mut args = std::env::args().skip(1);
    let input = args.next().expect("usage: wasm <script.js|module.wasm> <outdir>");
    let out = args.next().unwrap_or_else(|| ".".into());
    let raw = std::fs::read(&input).expect("read");

    let modules = if raw.starts_with(&[0, 0x61, 0x73, 0x6d]) {
        vec![raw]
    } else {
        extract::modules(&String::from_utf8_lossy(&raw))
    };
    if modules.is_empty() {
        eprintln!("no webassembly module found");
        std::process::exit(1);
    }

    std::fs::create_dir_all(&out).expect("mkdir");
    for (n, bytes) in modules.iter().enumerate() {
        let Some(m) = parse::module(bytes) else {
            eprintln!("module {n}: parse failed");
            continue;
        };
        let imported = m.imported_funcs();
        eprintln!(
            "module {n}: {} bytes, {} types, {} imports, {} bodies, {} exports, {} data",
            bytes.len(),
            m.types.len(),
            m.imports.len(),
            m.bodies.len(),
            m.exports.len(),
            m.data.len()
        );
        let stem = if modules.len() == 1 { String::new() } else { format!(".{n}") };
        std::fs::write(format!("{out}/module{stem}.wasm"), bytes).expect("write");
        std::fs::write(format!("{out}/header{stem}.txt"), print::header(&m)).expect("write");
        std::fs::write(format!("{out}/strings{stem}.txt"), print::strings(&m)).expect("write");
        std::fs::write(format!("{out}/disasm{stem}.txt"), print::disasm(&m)).expect("write");

        let mut body = String::new();
        for i in 0..m.bodies.len() {
            let Some(mut f) = ir::func(&m, (imported + i) as u32) else { continue };
            flat::run(&mut f.body);
            body.push_str(&print::func(&f));
            body.push('\n');
        }
        std::fs::write(format!("{out}/decompiled{stem}.c"), body).expect("write");

        if std::env::var("DD_ATTEST").is_ok() {
            let env = attest::Env {
                user_env: std::env::var("DD_USERENV").unwrap_or_default(),
                touch: 0,
                cores: 12,
                outer_height: 945,
            };
            let seeds: Vec<u32> = [env.touch, env.touch, env.cores, 945]
                .iter()
                .map(|value| attest::cyrb53(&env.user_env, *value) as u32)
                .collect();
            let (first, second, trap) = attest::run(&m, &env, &seeds, "LF7PpE", "b7lNKI");
            eprintln!("attest LF7PpE {first:?} b7lNKI {second:?} trap {trap:?}");
        }
    }
}
