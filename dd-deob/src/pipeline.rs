use oxc_allocator::Allocator;
use oxc_codegen::Codegen;
use std::time::Instant;

use crate::extract::ExtractOutput;
use crate::module_split::SplitModule;

pub struct ModuleReport {
    pub bundle_type: String,
    pub modules: Vec<(String, String)>,
    pub extract: ExtractOutput,
    pub vm: Option<crate::vm::VmDump>,
}

impl ModuleReport {
    pub fn to_json(&self) -> serde_json::Value {
        let mut modules = serde_json::Map::new();
        for (name, _) in &self.modules {
            modules.insert(name.clone(), serde_json::Value::from(format!("{}.js", name)));
        }
        serde_json::json!({
            "bundleType": self.bundle_type,
            "moduleOrder": self.modules.iter().map(|(n, _)| n.clone()).collect::<Vec<_>>(),
            "modules": modules,
            "dynamic_challenge": self.extract.dynamic_challenge,
            "wasm_b64": self.extract.wasm_b64,
            "wasm_fields": self.extract.wasm_fields,
            "vm_bytecode_size": self.vm.as_ref().map(|v| v.bytecode.len()),
        })
    }
}

pub fn run(source: &str, timings: bool) -> ModuleReport {
    let alloc = Allocator::with_capacity(source.len() * 4);
    let t = Instant::now();
    let mut program = crate::parse::parse_js(&alloc, source);
    log_phase(timings, "parse", t.elapsed());

    let t = Instant::now();
    let _ = crate::hex_cleanup::run(&mut program, &alloc);
    log_phase(timings, "hex", t.elapsed());

    let t = Instant::now();
    let _ = crate::bracket::unwrap_double_brackets(&mut program, &alloc);
    log_phase(timings, "double_bracket", t.elapsed());

    let t = Instant::now();
    let (bundle, mut split) = crate::module_split::split(&mut program, &alloc);
    log_phase(timings, "split", t.elapsed());

    let mut report = ModuleReport {
        bundle_type: bundle.as_str().to_string(),
        modules: Vec::with_capacity(split.len()),
        extract: ExtractOutput::default(),
        vm: None,
    };

    let module_alloc = Allocator::with_capacity(source.len() * 4);
    for sm in split.iter_mut() {
        process_module(sm, &module_alloc, timings);
        let code = Codegen::new().build(&sm.program).code;
        report.modules.push((sm.name.clone(), code));
    }

    for sm in &split {
        let r = crate::extract::run(&sm.program);
        if r.dynamic_challenge.is_some() && report.extract.dynamic_challenge.is_none() {
            report.extract.dynamic_challenge = r.dynamic_challenge;
        }
        if r.wasm_b64.is_some() && report.extract.wasm_b64.is_none() {
            report.extract.wasm_b64 = r.wasm_b64;
        }
        if !r.wasm_fields.is_empty() && report.extract.wasm_fields.is_empty() {
            report.extract.wasm_fields = r.wasm_fields;
        }
        if sm.name == "vm-obf" && report.vm.is_none() {
            if let Some(vm) = crate::vm::extract(&sm.program) {
                report.vm = Some(vm);
            }
        }
    }

    report
}

fn process_module<'a>(sm: &mut SplitModule<'a>, alloc: &'a Allocator, timings: bool) {
    let p = &mut sm.program;

    let t = Instant::now();
    let _ = crate::inline_settimeout::run(p, alloc);
    log_phase(timings, &format!("[{}] inline_settimeout", sm.name), t.elapsed());

    let t = Instant::now();
    for _ in 0..6 {
        let mut changed = 0;
        changed += crate::pure_calls::run(p, alloc);
        changed += crate::window_methods::run(p, alloc);
        changed += crate::simplify::fold_expressions(p, alloc);
        changed += crate::simplify::fold_if_statements(p, alloc);
        changed += crate::opaque::run(p, alloc);
        changed += crate::string_decoders::run(p, alloc);
        changed += crate::tmatrix::run(p, alloc);
        changed += crate::cff::run(p, alloc);
        changed += crate::passes::scope::inline_const(p, alloc);
        changed += crate::passes::scope::unused_vars(p, alloc);
        if changed == 0 { break; }
    }
    log_phase(timings, &format!("[{}] passes", sm.name), t.elapsed());

    let t = Instant::now();
    let _ = crate::bracket::normalize_to_dot(p, alloc);
    log_phase(timings, &format!("[{}] normalize", sm.name), t.elapsed());
}

fn log_phase(enabled: bool, name: &str, dur: std::time::Duration) {
    if enabled {
        eprintln!("  {name}: {:.1}ms", dur.as_secs_f64() * 1000.0);
    }
}
