use mimalloc::MiMalloc;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::time::Instant;

#[global_allocator]
static GLOBAL: MiMalloc = MiMalloc;

fn main() {
    let mut args = std::env::args();
    let prog = args.next().unwrap_or_else(|| "dd-deob".into());
    let Some(input) = args.next() else {
        eprintln!("usage: {} <input.js> [output_prefix/]", prog);
        std::process::exit(1);
    };
    let prefix = args.next().unwrap_or_else(|| {
        let p = Path::new(&input);
        let stem = p.file_stem().and_then(|s| s.to_str()).unwrap_or("script");
        let parent = p.parent().unwrap_or_else(|| Path::new("."));
        parent.join(format!("{}-out/", stem)).to_string_lossy().into_owned()
    });

    let raw = std::fs::read_to_string(&input).unwrap_or_else(|e| {
        eprintln!("failed to read {}: {}", input, e);
        std::process::exit(1);
    });

    let timings = std::env::var_os("DEOB_TIMINGS").is_some();
    let total = Instant::now();

    let report = dd_deob::pipeline::run(&raw, timings);

    let outdir = if prefix.ends_with('/') { PathBuf::from(&prefix) } else { PathBuf::from(&prefix).with_extension("") };
    std::fs::create_dir_all(&outdir).ok();

    for (name, code) in &report.modules {
        let p = outdir.join(format!("{}.js", name));
        let mut f = std::fs::File::create(&p).unwrap_or_else(|e| {
            eprintln!("failed to write {}: {}", p.display(), e);
            std::process::exit(1);
        });
        std::io::BufWriter::new(&mut f).write_all(code.as_bytes()).unwrap();
    }

    if let Some(vm) = &report.vm {
        std::fs::write(outdir.join("vm.bytecode"), &vm.bytecode).ok();
        std::fs::write(outdir.join("vm.disasm.txt"), &vm.disasm).ok();
        eprintln!("vm: {} bytes -> vm.bytecode + vm.disasm.txt", vm.bytecode.len());
    }

    if let Some(b64) = &report.extract.wasm_b64 {
        use base64::Engine;
        let cleaned: String = b64.chars().filter(|c| !c.is_whitespace()).collect();
        if let Ok(bytes) = base64::engine::general_purpose::STANDARD.decode(cleaned.as_bytes())
            .or_else(|_| base64::engine::general_purpose::STANDARD_NO_PAD.decode(cleaned.as_bytes())) {
            let wasm_path = outdir.join("module.wasm");
            std::fs::write(&wasm_path, &bytes).ok();
            match wasmprinter::print_bytes(&bytes) {
                Ok(wat) => { std::fs::write(outdir.join("module.wat"), wat).ok(); }
                Err(e) => eprintln!("wasm disasm failed: {}", e),
            }
            eprintln!("wasm: {} bytes -> {}", bytes.len(), wasm_path.display());
        }
    }

    let report_path = outdir.join("report.json");
    if let Ok(f) = std::fs::File::create(&report_path) {
        let json = report.to_json();
        let _ = serde_json::to_writer_pretty(f, &json);
    }

    eprintln!("bundle: {}  modules: {}  total: {:.1}ms",
        report.bundle_type, report.modules.len(),
        total.elapsed().as_secs_f64() * 1000.0);
    eprintln!("wrote {}", outdir.display());
}
