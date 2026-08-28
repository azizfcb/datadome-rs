use vm::{dis, exec, host, image, lift, ops, run, stack, strings, template};

use std::fmt::Write;

fn merge(
    states: &mut std::collections::BTreeMap<usize, Vec<template::Node>>,
    at: usize,
    incoming: &[template::Node],
) {
    match states.get_mut(&at) {
        None => {
            states.insert(at, incoming.to_vec());
        }
        Some(existing) => {
            if existing.len() != incoming.len() {
                return;
            }
            for (slot, value) in existing.iter_mut().zip(incoming) {
                if slot.to_string() != value.to_string() {
                    *slot = template::Node::Unknown;
                }
            }
        }
    }
}

fn main() {
    let mut args = std::env::args().skip(1);
    let input = args.next().expect("usage: vm <deobfuscated.js> [disasm.txt] [handlers.txt] [tables.txt]");
    let disasm_out = args.next();
    let handlers_out = args.next();
    let tables_out = args.next();
    let lift_out = args.next();
    let source = std::fs::read_to_string(&input).expect("read");

    let boot = image::boot(&source).expect("no VM bootstrap found");
    eprintln!(
        "image {} cells, code [{}, {}), filler seed {}",
        boot.length, boot.lo, boot.hi, boot.seed
    );
    eprintln!(
        "readers {:?}, strings slot {:?}, ip slot {:?}, sp slot {:?}, table {}, definer {}",
        boot.api.readers,
        boot.api.strings,
        boot.api.ip_slot,
        boot.api.sp_slot,
        boot.api.handler_base,
        boot.api.define_op
    );
    eprintln!("constant tags {:?}", boot.consts.tags);
    eprintln!("helper roles {:?} acc {:?} image {:?} result {:?} bp {:?}", boot.api.roles, boot.api.acc_slot, boot.api.image, boot.api.result, boot.api.bp_slot);

    let img = boot.build();
    assert_eq!(
        img[boot.lo], boot.api.define_op as i32,
        "the stream must open with the one handler the VM starts with"
    );
    let (handlers, entry) = image::definitions(&img, boot.lo, boot.api.define_op);
    eprintln!("handlers {} program starts at {}", handlers.len(), entry);
    let layouts: std::collections::BTreeMap<u8, ops::Layout> =
        handlers.iter().map(|(op, src)| (*op, ops::layout(&boot.api, src))).collect();

    let boot_halt = boot.api.halt;
    if std::env::var("DD_RUN").is_ok() {
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
            watch: std::env::var("DD_WATCH").ok().and_then(|v| v.parse().ok()).unwrap_or(0),
            applies: 0,
            spill: std::collections::BTreeMap::new(),
            newing: false,
            in_new: false,
            clock: 1787961600000.0,
            elapsed: 812.5,
            seed: 0x9e3779b97f4a7c15,
            host: host::Host {
                document: std::env::var("DD_PAGE")
                    .ok()
                    .and_then(|path| std::fs::read_to_string(path).ok())
                    .unwrap_or_default(),
                ..host::Host::default()
            },
        };
        let cell = |index: u32| state.api.cells.get(&index).copied().unwrap_or(0) as usize;
        let base = state.api.globals_base.unwrap_or(0) as f64;
        let ip = cell(state.api.ip_slot.unwrap_or(0));
        let sp = cell(state.api.sp_slot.unwrap_or(0));
        let bp = cell(state.api.bp_slot.unwrap_or(0));
        let acc = cell(state.api.acc_slot.unwrap_or(0));
        let halt = boot_halt.unwrap_or_else(|| cell(22) as i64) as usize;
        state.put(ip, run::Value::Num(0.0));
        state.put(sp, run::Value::Num(base));
        state.put(bp, run::Value::Num(base));
        state.put(halt, run::Value::Num(0.0));
        state.put(acc, run::Value::Undefined);
        eprintln!("globals base {:?} cells26 {:?} cells10 {:?}", state.api.globals_base, state.api.cells.get(&26), state.api.cells.get(&10));
        eprintln!("locals base {base} sp cell {sp} bp cell {bp} ip cell {ip} halt cell {halt}");

        let limit: usize = std::env::var("DD_STEPS").ok().and_then(|v| v.parse().ok()).unwrap_or(5_000_000);
        let mut seen: std::collections::BTreeMap<u8, usize> = std::collections::BTreeMap::new();
        let mut visited: std::collections::BTreeSet<usize> = std::collections::BTreeSet::new();
        let mut last = 0usize;
        let mut where_at = 0usize;
        let mut spots: std::collections::BTreeMap<usize, usize> = std::collections::BTreeMap::new();
        let mut ring: Vec<(usize, usize, u8)> = Vec::new();
        let mut fired = false;
        let mut machine = exec::Handlers::new(std::collections::BTreeMap::new());
        let definer = state.api.define_op;
        while !state.halted && state.steps < limit {
            let op = state.read(1) as u8;
            state.steps += 1;
            if op == definer {
                let target = state.read(1) as u8;
                let length = state.read(3) as usize;
                let mut source = String::new();
                for _ in 0..length {
                    let byte = state.read(1) as u32;
                    if let Some(found) = char::from_u32(byte) {
                        source.push(found);
                    }
                }
                machine.install(target, source);
                continue;
            }
            let window: usize = std::env::var("DD_FROM").ok().and_then(|v| v.parse().ok()).unwrap_or(100);
            if std::env::var("DD_TRACE").is_ok() && state.steps >= window && state.steps < window + 40 {
                eprintln!(
                    "step {} op {} ip {} sp {}",
                    state.steps,
                    op,
                    state.ip(),
                    state.cell(sp).number()
                );
            }
            if std::env::var("DD_RING").is_ok() {
                ring.push((state.steps, state.ip() - 1, op));
                if ring.len() > 60 { ring.remove(0); }
                if state.ip() - 1 == 128831 && !fired {
                    fired = true;
                    for (st, at, code) in &ring {
                        eprintln!("ring step {st} ip {at} op {code}");
                    }
                }
            }
            if let Ok(spot) = std::env::var("DD_AT") {
                let spot: usize = spot.parse().unwrap_or(0);
                if state.ip() == spot + 1 {
                    let top = state.cell(sp).number() as usize;
                    let view: Vec<String> = (1..=6)
                        .rev()
                        .map(|back| {
                            let item = state.cell(top - back);
                            let text = item.text();
                            format!("{}:{}", item.kind(), &text[..text.len().min(24)])
                        })
                        .collect();
                    eprintln!("at {spot} op {op} step {} sp {} :: {}", state.steps, top - base as usize, view.join(" | "));
                }
            }
            if let Ok(every) = std::env::var("DD_SP") {
                let every: usize = every.parse().unwrap_or(1000);
                if state.steps % every == 0 {
                    eprintln!("sp {} at step {} ip {} op {}", state.cell(sp).number() - base, state.steps, state.ip(), op);
                }
            }
            *seen.entry(op).or_insert(0usize) += 1;
            if visited.insert(state.ip()) {
                last = state.steps;
                where_at = state.ip();
            }
            if state.steps % 500_000 == 0 {
                let base = state.base();
                eprintln!(
                    "at {} ip {} state {} {} {}",
                    state.steps,
                    state.ip(),
                    state.cell(base + 1041).kind(),
                    state.cell(base + 19).kind(),
                    state.cell(base + 1044).kind()
                );
                eprintln!("  distinct ip {}", visited.len());
            }
            if state.steps + 60_000 > limit {
                *spots.entry(state.ip()).or_insert(0usize) += 1;
            }
            machine.run(op, &mut state);
            if std::env::var("DD_TRACE").is_ok() && (104..120).contains(&state.steps) {
                eprintln!(
                    "  after step {} op {}: sp {} m[188521] {}",
                    state.steps,
                    op,
                    state.cell(sp).number(),
                    state.cell(188521).text()
                );
            }
            if state.cell(halt).number() != 0.0 {
                break;
            }
        }
        eprintln!("ran {} steps, note {:?}, applies {}", state.steps, state.note, state.applies);
        eprintln!("host calls wanted: {:?}", state.wanted);
        let mut ranked: Vec<(u8, usize)> = seen.into_iter().collect();
        ranked.sort_by_key(|(_, count)| std::cmp::Reverse(*count));
        eprintln!("top opcodes {:?}", &ranked[..ranked.len().min(14)]);
        let mut hot: Vec<(usize, usize)> = spots.iter().map(|(a, b)| (*a, *b)).collect();
        hot.sort_by_key(|(_, count)| std::cmp::Reverse(*count));
        eprintln!("hot ip {:?}", &hot[..hot.len().min(30)]);
        let mut span: Vec<usize> = spots.keys().copied().collect();
        span.sort();
        eprintln!("loop body {} ips, {:?} .. {:?}", span.len(), span.first(), span.last());
        if std::env::var("DD_BODY").is_ok() {
            eprintln!("{span:?}");
        }
        eprintln!("last new ip {where_at} at step {last}");
        for op in [31u8, 92, 155, 67, 248, 213, 6, 217] {
            let count = ranked.iter().find(|(found, _)| *found == op).map_or(0, |(_, c)| *c);
            eprintln!("op {op} ran {count}");
        }
        for (key, value) in state.result.borrow().iter() {
            let text = value.text();
            eprintln!("result {key} = {} ({} chars)", &text[..text.len().min(200)], text.len());
        }
        return;
    }

    let code = &img[boot.lo..boot.hi];
    let (insns, error) = dis::disassemble(code, entry, &layouts, &boot.consts);
    eprintln!("instructions {} error {error:?}", insns.len());

    let trampoline = handlers.values().find_map(|src| ops::trampoline(&boot.api, src));

    let untyped: Vec<u8> =
        layouts.iter().filter(|(_, l)| l.template.is_none()).map(|(op, _)| *op).collect();
    eprintln!("opcodes without a template: {} {untyped:?}", untyped.len());

    let unknown: Vec<u8> =
        layouts.iter().filter(|(_, l)| l.delta.is_none()).map(|(op, _)| *op).collect();
    eprintln!("opcodes whose stack effect depends on the branch taken: {unknown:?}");
    for op in &unknown {
        eprintln!("  op {op} branch {:?}", layouts[op].branch);
    }

    let none = |_: &dis::Insn| None;
    let (first, _) = stack::globals(&insns, &layouts, trampoline.as_ref(), &none);
    let table: Vec<u8> = match first.first() {
        Some(stack::Val::Bytes(b)) => b.clone(),
        _ => Vec::new(),
    };
    let read = |i: &dis::Insn| {
        let (index, key) = layouts[&i.op].string?;
        strings::decode(&table, i.operands.get(index)?.int()? as usize, i.operands.get(key)?.int()?)
    };
    let (globals, stopped) = stack::globals(&insns, &layouts, trampoline.as_ref(), &read);
    let arrays = globals.iter().filter(|v| matches!(v, stack::Val::Bytes(_))).count();
    eprintln!("globals {} after {stopped} instructions, {arrays} of them byte tables", globals.len());

    if let Some(path) = handlers_out {
        let mut out = String::new();
        if let Some(path) = tables_out {
        let mut out = String::new();
        for (i, v) in globals.iter().enumerate() {
            let stack::Val::Bytes(b) = v else { continue };
            let mut sorted = b.clone();
            sorted.sort_unstable();
            let kind = if sorted.len() == 256 && sorted.iter().enumerate().all(|(i, v)| i as u8 == *v)
            {
                "permutation"
            } else {
                "bytes"
            };
            let _ = writeln!(out, "global {i}  {kind}  {} bytes\n{b:?}\n", b.len());
        }
        std::fs::write(path, out).expect("write");
    }

    let permutations = globals
        .iter()
        .filter(|v| match v {
            stack::Val::Bytes(b) => {
                let mut seen = b.clone();
                seen.sort_unstable();
                seen.len() == 256 && seen.iter().enumerate().all(|(i, v)| i as u8 == *v)
            }
            _ => false,
        })
        .count();
    eprintln!("byte permutations among the globals: {permutations}");
    for (op, src) in &handlers {
            let l = &layouts[op];
            let _ = writeln!(
                out,
                "--- op {op} {:?}{}\n{}\n{src}",
                l.steps,
                match l.jump {
                    Some((i, back)) =>
                        format!(" jump operand {i}{}", if back { " back" } else { "" }),
                    None => String::new(),
                },
                ops::render(&boot.api, src)
            );
            if let Some(t) = &l.template {
                let _ = writeln!(out, "{t}");
            }
        }
        std::fs::write(path, out).expect("write");
    }

    let mut bodies: std::collections::BTreeMap<usize, (String, usize)> = Default::default();
    for (n, i) in insns.iter().enumerate() {
        if layouts[&i.op].closure.is_none() {
            continue;
        }
        let (Some(next), Some(after)) = (insns.get(n + 1), insns.get(n + 2)) else { continue };
        let Some(end) = next.target else { continue };
        let args: Vec<String> = i.operands.iter().map(|o| o.to_string()).collect();
        bodies.insert(after.at, (args.join(", "), end));
    }
    eprintln!("functions {}", bodies.len());

    let mut leaders: std::collections::BTreeSet<usize> = Default::default();
    for (n, i) in insns.iter().enumerate() {
        if let Some(t) = i.target {
            leaders.insert(t);
            if let Some(next) = insns.get(n + 1) {
                leaders.insert(next.at);
            }
        }
    }

    let index_at: std::collections::BTreeMap<usize, usize> =
        insns.iter().enumerate().map(|(n, i)| (i.at, n)).collect();

    let mut starts: std::collections::BTreeSet<usize> = Default::default();
    if let Some(first) = insns.first() {
        starts.insert(first.at);
    }
    for at in bodies.keys() {
        starts.insert(*at);
    }
    for (n, i) in insns.iter().enumerate() {
        if let Some(t) = i.target {
            starts.insert(t);
            if let Some(next) = insns.get(n + 1) {
                starts.insert(next.at);
            }
        }
    }
    let order: Vec<usize> = starts.iter().copied().filter(|a| index_at.contains_key(a)).collect();

    let mut entry_state: std::collections::BTreeMap<usize, Vec<template::Node>> = Default::default();
    if let Some(first) = insns.first() {
        entry_state.insert(first.at, Vec::new());
    }
    for at in bodies.keys() {
        entry_state.insert(*at, Vec::new());
    }

    let mut lifted = String::new();
    let mut lifter = lift::Lifter::new(&layouts, trampoline.as_ref());
    for (b, start) in order.iter().enumerate() {
        let from = index_at[start];
        let stop = order.get(b + 1).map_or(insns.len(), |a| index_at[a]);
        if let Some((args, end)) = bodies.get(start) {
            let _ = writeln!(lifted, "\nfn @{start} .. {end}  ({args})");
        } else {
            let _ = writeln!(lifted, "\n@{start}:");
        }
        match entry_state.get(start) {
            Some(state) => lifter.load(state),
            None => lifter.reset(),
        }
        for i in &insns[from..stop] {
            let text = layouts[&i.op].string.and_then(|(index, key)| {
                let stack::Val::Bytes(blob) = globals.get(i.operands.first()?.int()? as usize)?
                else {
                    return None;
                };
                strings::decode(
                    blob,
                    i.operands.get(index)?.int()? as usize,
                    i.operands.get(key)?.int()?,
                )
            });
            lifter.step(i, text.as_deref(), &mut lifted);
            if let Some(t) = i.target {
                merge(&mut entry_state, t, &lifter.save());
            }
        }
        if let Some(next) = order.get(b + 1) {
            merge(&mut entry_state, *next, &lifter.save());
        }
    }
    if let Some(path) = lift_out {
        std::fs::write(path, lifted).expect("write");
    }

    let mut out = String::new();
    for i in &insns {
        if let Some((args, end)) = bodies.get(&i.at) {
            let _ = writeln!(out, "\nfn @{} .. {end}  ({args})", i.at);
        }
        let text = layouts[&i.op].string.and_then(|(index, key)| {
            let stack::Val::Bytes(blob) = globals.get(i.operands.first()?.int()? as usize)? else {
                return None;
            };
            strings::decode(blob, i.operands.get(index)?.int()? as usize, i.operands.get(key)?.int()?)
        });
        let args: Vec<String> = i.operands.iter().map(|o| o.to_string()).collect();
        let table = layouts[&i.op].effects.iter().find_map(|e| match e {
            ops::Effect::Global(at) => i.operands.get(*at)?.int(),
            _ => None,
        });
        let note = match (text, i.target, table) {
            (Some(t), _, _) => format!("  ; {t:?}"),
            (_, Some(t), _) => format!("  ; -> {t}"),
            (_, _, Some(g)) => format!("  ; global {g}"),
            _ => String::new(),
        };
        let _ = writeln!(out, "{:>7}  {:>3}  {:<40}{note}", i.at, i.op, args.join(", "));
    }
    match disasm_out {
        Some(path) => std::fs::write(path, out).expect("write"),
        None => print!("{out}"),
    }
}
