pub mod api;
pub mod css;
pub mod dis;
pub mod dom;
pub mod exec;
pub mod host;
pub mod image;
pub mod konst;
pub mod lift;
pub mod ops;
pub mod run;
pub mod stack;
pub mod strings;
pub mod template;

pub struct Trace {
    pub steps: usize,
    pub note: Option<String>,
    pub wanted: Vec<String>,
}

#[derive(Default)]
pub struct Output {
    pub r: Option<String>,
    pub i: usize,
    pub u: Option<String>,
    pub t: i64,
    pub e: Option<String>,
}

pub fn plv3(source: &str, host: host::Host, limit: usize) -> (Output, Trace) {
    let empty = Trace { steps: 0, note: Some("no image".to_string()), wanted: Vec::new() };
    let Some(boot) = image::boot(source) else { return (Output::default(), empty) };
    let img = boot.build();
    if img.get(boot.lo).copied() != Some(boot.api.define_op as i32) {
        let note = Trace { steps: 0, note: Some("no prologue".to_string()), wanted: Vec::new() };
        return (Output::default(), note);
    }
    let code: Vec<i32> = img[boot.lo..boot.hi].to_vec();
    let mut state = exec::State {
        memory: img.iter().map(|cell| run::Value::Num(*cell as f64)).collect(),
        result: std::rc::Rc::new(std::cell::RefCell::new(std::collections::BTreeMap::new())),
        reader: exec::Reader { code, base: 0 },
        api: boot.api,
        tags: boot.consts.tags.clone(),
        locals: Vec::new(),
        source: String::new(),
        returned: None,
        halted: false,
        steps: 0,
        note: None,
        wanted: Vec::new(),
        op: 0,
        watch: 0,
        applies: 0,
        spill: std::collections::BTreeMap::new(),
        newing: false,
        in_new: false,
        clock: 0.0,
        elapsed: 0.0,
        seed: 0x9e3779b97f4a7c15,
        host,
    };
    state.clock = state.host.now;
    state.elapsed = state.host.elapsed;
    state.seed = state.host.seed;
    let cell = |index: u32| state.api.cells.get(&index).copied().unwrap_or(0) as usize;
    let base = state.api.globals_base.unwrap_or(0) as f64;
    let ip = cell(state.api.ip_slot.unwrap_or(0));
    let sp = cell(state.api.sp_slot.unwrap_or(0));
    let bp = cell(state.api.bp_slot.unwrap_or(0));
    let acc = cell(state.api.acc_slot.unwrap_or(0));
    let halt = state.api.halt.unwrap_or_else(|| cell(22) as i64) as usize;
    state.put(ip, run::Value::Num(0.0));
    state.put(sp, run::Value::Num(base));
    state.put(bp, run::Value::Num(base));
    state.put(halt, run::Value::Num(0.0));
    state.put(acc, run::Value::Undefined);

    let mut machine = exec::Handlers::new(std::collections::BTreeMap::new());
    let definer = state.api.define_op;
    while !state.halted && state.steps < limit {
        let op = state.read(1) as u8;
        state.steps += 1;
        if op == definer {
            let target = state.read(1) as u8;
            let length = state.read(3) as usize;
            let mut body = String::new();
            for _ in 0..length {
                let byte = state.read(1) as u32;
                if let Some(found) = char::from_u32(byte) {
                    body.push(found);
                }
            }
            machine.install(target, body);
            continue;
        }
        machine.run(op, &mut state);
        if state.cell(halt).number() != 0.0 {
            break;
        }
    }
    let output = Output {
        r: state
            .result
            .borrow()
            .get("r")
            .map(|value| value.text())
            .filter(|body| !body.is_empty()),
        i: state.steps,
        u: state.spill.get(&-1).map(|value| value.text()),
        t: (state.elapsed) as i64,
        e: state.note.clone(),
    };
    let trace = Trace { steps: state.steps, note: state.note.clone(), wanted: state.wanted.clone() };
    (output, trace)
}

pub fn urlsafe(body: &str) -> String {
    body.chars()
        .filter(|letter| *letter != '=')
        .map(|letter| match letter {
            '+' => '-',
            '/' => '_',
            other => other,
        })
        .collect()
}
