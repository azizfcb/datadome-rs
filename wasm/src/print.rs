use crate::ir::{Expr, Func, Place, Stmt};
use crate::parse::Module;

pub fn func(f: &Func) -> String {
    let params: Vec<String> =
        f.params.iter().enumerate().map(|(i, p)| format!("{} v{i}", p.name())).collect();
    let result = f.results.first().map_or("void", |v| v.name());
    let mut out = format!("{result} {}({}) {{\n", f.name, params.join(", "));
    for (i, l) in f.locals.iter().enumerate() {
        out.push_str(&format!("  {} v{}\n", l.name(), i + f.params.len()));
    }
    block(&f.body, 1, &mut out);
    out.push_str("}\n");
    out
}

fn block(stmts: &[Stmt], depth: usize, out: &mut String) {
    let pad = "  ".repeat(depth);
    for s in stmts {
        match s {
            Stmt::Set(p, e) => out.push_str(&format!("{pad}{} = {}\n", place(p), expr(e))),
            Stmt::Store(cell, a, off, v) => {
                out.push_str(&format!("{pad}{cell}[{}] = {}\n", address(a, *off), expr(v)))
            }
            Stmt::Effect(e) => out.push_str(&format!("{pad}{}\n", expr(e))),
            Stmt::Drop(e) => out.push_str(&format!("{pad}drop {}\n", expr(e))),
            Stmt::Block(l, inner) => {
                out.push_str(&format!("{pad}L{l}: {{\n"));
                block(inner, depth + 1, out);
                out.push_str(&format!("{pad}}}\n"));
            }
            Stmt::Loop(l, inner) => {
                out.push_str(&format!("{pad}L{l}: loop {{\n"));
                block(inner, depth + 1, out);
                out.push_str(&format!("{pad}}}\n"));
            }
            Stmt::If(c, a, b) => {
                out.push_str(&format!("{pad}if ({}) {{\n", expr(c)));
                block(a, depth + 1, out);
                if !b.is_empty() {
                    out.push_str(&format!("{pad}}} else {{\n"));
                    block(b, depth + 1, out);
                }
                out.push_str(&format!("{pad}}}\n"));
            }
            Stmt::Break(l) => out.push_str(&format!("{pad}break L{l}\n")),
            Stmt::Continue(l) => out.push_str(&format!("{pad}continue L{l}\n")),
            Stmt::BreakIf(c, l) => out.push_str(&format!("{pad}if ({}) break L{l}\n", expr(c))),
            Stmt::ContinueIf(c, l) => {
                out.push_str(&format!("{pad}if ({}) continue L{l}\n", expr(c)))
            }
            Stmt::Switch(c, arms, d) => {
                let list: Vec<String> = arms.iter().map(|a| format!("L{a}")).collect();
                out.push_str(&format!(
                    "{pad}switch ({}) [{}] else L{d}\n",
                    expr(c),
                    list.join(", ")
                ))
            }
            Stmt::Return(v) => {
                let list: Vec<String> = v.iter().map(expr).collect();
                out.push_str(&format!("{pad}return {}\n", list.join(", ")))
            }
            Stmt::Copy(d, s, n) => {
                out.push_str(&format!("{pad}copy({}, {}, {})\n", expr(d), expr(s), expr(n)))
            }
            Stmt::Fill(d, v, n) => {
                out.push_str(&format!("{pad}fill({}, {}, {})\n", expr(d), expr(v), expr(n)))
            }
            Stmt::State(n, inner) => {
                out.push_str(&format!("{pad}state {n}:\n"));
                block(inner, depth + 1, out);
            }
            Stmt::Goto(n) => out.push_str(&format!("{pad}goto {n}\n")),
            Stmt::Unreachable => out.push_str(&format!("{pad}unreachable\n")),
        }
    }
}

fn place(p: &Place) -> String {
    match p {
        Place::Local(n) => format!("v{n}"),
        Place::Global(n) => format!("g{n}"),
        Place::Temp(n) => format!("t{n}"),
    }
}

fn address(a: &Expr, offset: u32) -> String {
    match (a, offset) {
        (Expr::Const(v), o) => format!("{}", v + o as i64),
        (other, 0) => expr(other),
        (other, o) => format!("{} + {o}", expr(other)),
    }
}

fn expr(e: &Expr) -> String {
    match e {
        Expr::Const(v) => v.to_string(),
        Expr::Float(v) => format!("{v:?}"),
        Expr::Local(n) => format!("v{n}"),
        Expr::Global(n) => format!("g{n}"),
        Expr::Temp(n) => format!("t{n}"),
        Expr::Result(l, i) => format!("L{l}#{i}"),
        Expr::Load(cell, a, off) => format!("{cell}[{}]", address(a, *off)),
        Expr::Un("!", a) => format!("!{}", expr(a)),
        Expr::Un("neg", a) => format!("-{}", expr(a)),
        Expr::Un(op, a) => format!("{op}({})", expr(a)),
        Expr::Bin(op, a, b) if op.chars().next().is_some_and(char::is_alphabetic) => {
            format!("{op}({}, {})", expr(a), expr(b))
        }
        Expr::Bin(op, a, b) => format!("({} {op} {})", expr(a), expr(b)),
        Expr::Select(c, a, b) => format!("({} ? {} : {})", expr(c), expr(a), expr(b)),
        Expr::Call(name, args) => {
            let list: Vec<String> = args.iter().map(expr).collect();
            format!("{name}({})", list.join(", "))
        }
        Expr::Indirect(i, args) => {
            let list: Vec<String> = args.iter().map(expr).collect();
            format!("table[{}]({})", expr(i), list.join(", "))
        }
        Expr::Size => "memory.size".into(),
        Expr::Grow(a) => format!("memory.grow({})", expr(a)),
        Expr::Null => "null".into(),
        Expr::FuncRef(n) => format!("&{n}"),
        Expr::Unknown => "?".into(),
    }
}

pub fn disasm(module: &Module) -> String {
    let imported = module.imported_funcs();
    let mut out = String::new();
    for (i, body) in module.bodies.iter().enumerate() {
        let index = (imported + i) as u32;
        let ty = module.func_type(index);
        let params: Vec<&str> =
            ty.map_or(Vec::new(), |t| t.params.iter().map(|v| v.name()).collect());
        let results: Vec<&str> =
            ty.map_or(Vec::new(), |t| t.results.iter().map(|v| v.name()).collect());
        out.push_str(&format!(
            "func {} #{index} ({}) -> ({})\n",
            module.func_name(index),
            params.join(", "),
            results.join(", ")
        ));
        for (k, l) in body.locals.iter().enumerate() {
            out.push_str(&format!("  local v{} {}\n", k + params.len(), l.name()));
        }
        let mut r = crate::parse::Reader::new(&body.code);
        let mut depth = 1usize;
        while !r.done() {
            let at = r.at;
            let Some(op) = crate::code::decode(&mut r) else {
                out.push_str(&format!("  {at:05x} <bad>\n"));
                break;
            };
            if matches!(op.name, "end" | "else") {
                depth = depth.saturating_sub(1);
            }
            out.push_str(&format!(
                "  {at:05x} {}{}{}\n",
                "  ".repeat(depth),
                op.name,
                argument(&op.arg)
            ));
            if matches!(op.name, "block" | "loop" | "if" | "else") {
                depth += 1;
            }
        }
        out.push('\n');
    }
    out
}

fn argument(a: &crate::code::Arg) -> String {
    use crate::code::Arg;
    match a {
        Arg::None | Arg::Types => String::new(),
        Arg::Index(n) => format!(" {n}"),
        Arg::Two(n) => format!(" {n}"),
        Arg::Mem(align, offset) => format!(" align={align} offset={offset}"),
        Arg::I32(v) => format!(" {v}"),
        Arg::I64(v) => format!(" {v}"),
        Arg::F32(v) => format!(" {v}"),
        Arg::F64(v) => format!(" {v}"),
        Arg::Block(b) => format!(" {b:?}"),
        Arg::Table(list, d) => {
            let items: Vec<String> = list.iter().map(|x| x.to_string()).collect();
            format!(" [{}] {d}", items.join(" "))
        }
    }
}

pub fn header(module: &Module) -> String {
    let mut out = String::new();
    for (i, im) in module.imports.iter().enumerate() {
        out.push_str(&format!("import {i}: {}.{} {:?}\n", im.module, im.name, im.kind));
    }
    for e in &module.exports {
        out.push_str(&format!("export {} = {:?} {}\n", e.name, e.kind, e.index));
    }
    for (i, g) in module.globals.iter().enumerate() {
        out.push_str(&format!(
            "global g{i}: {}{}\n",
            g.ty.name(),
            if g.mutable { " mut" } else { "" }
        ));
    }
    for (i, m) in module.memories.iter().enumerate() {
        out.push_str(&format!("memory {i}: min {} max {:?}\n", m.0, m.1));
    }
    for (i, e) in module.elements.iter().enumerate() {
        let names: Vec<String> = e.funcs.iter().map(|f| module.func_name(*f)).collect();
        out.push_str(&format!("element {i}: {}\n", names.join(", ")));
    }
    for (i, d) in module.data.iter().enumerate() {
        out.push_str(&format!("data {i}: {} bytes at {:?}\n", d.bytes.len(), origin(d)));
    }
    out.push('\n');
    for (i, t) in module.types.iter().enumerate() {
        let params: Vec<&str> = t.params.iter().map(|v| v.name()).collect();
        let results: Vec<&str> = t.results.iter().map(|v| v.name()).collect();
        out.push_str(&format!("type {i}: ({}) -> ({})\n", params.join(", "), results.join(", ")));
    }
    out
}

fn origin(d: &crate::parse::Data) -> Option<i64> {
    let bytes = d.offset.as_ref()?;
    let mut r = crate::parse::Reader::new(bytes);
    if r.byte()? != 0x41 {
        return None;
    }
    r.sleb()
}

pub fn strings(module: &Module) -> String {
    let mut out = String::new();
    for (i, d) in module.data.iter().enumerate() {
        let base = origin(d).unwrap_or(0);
        let mut run = Vec::new();
        let mut start = 0usize;
        for (k, b) in d.bytes.iter().enumerate() {
            if b.is_ascii_graphic() || *b == b' ' {
                if run.is_empty() {
                    start = k;
                }
                run.push(*b);
                continue;
            }
            if run.len() >= 6 {
                out.push_str(&format!(
                    "{i}@{}: {}\n",
                    base + start as i64,
                    String::from_utf8_lossy(&run)
                ));
            }
            run.clear();
        }
        if run.len() >= 6 {
            out.push_str(&format!(
                "{i}@{}: {}\n",
                base + start as i64,
                String::from_utf8_lossy(&run)
            ));
        }
    }
    out
}
