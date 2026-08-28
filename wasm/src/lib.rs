pub mod attest;
pub mod code;
pub mod exec;
pub mod extract;
pub mod flat;
pub mod ir;
pub mod parse;
pub mod print;

pub fn attestation(source: &str, env: &attest::Env) -> Option<(u32, i64)> {
    let modules = extract::modules(source);
    let bytes = modules.first()?;
    let module = parse::module(bytes)?;
    let seeds: Vec<u32> = [env.touch, env.touch, env.cores, env.outer_height]
        .iter()
        .map(|value| attest::cyrb53(&env.user_env, *value) as u32)
        .collect();
    let mut taking: Option<String> = None;
    let mut plain: Option<String> = None;
    for entry in &module.exports {
        let parse::Kind::Func(_) = entry.kind else { continue };
        if entry.name.starts_with("__wbindgen") {
            continue;
        }
        let Some(shape) = module.func_type(entry.index) else { continue };
        if shape.params.is_empty() {
            plain = Some(entry.name.clone());
        } else {
            taking = Some(entry.name.clone());
        }
    }
    let (first, second, _) = attest::run(&module, env, &seeds, &taking?, &plain?);
    Some((first?, second?))
}
