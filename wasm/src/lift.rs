use crate::code::{Arg, Block, Op, decode};
use crate::parse::{Module, Reader, Type, Val};

pub struct Lift<'a> {
    module: &'a Module,
    index: u32,
    locals: Vec<String>,
    stack: Vec<String>,
    out: String,
    depth: usize,
    frames: Vec<Frame>,
    temp: usize,
}

struct Frame {
    kind: Kind,
    label: usize,
    height: usize,
    results: usize,
}

#[derive(PartialEq)]
enum Kind {
    Block,
    Loop,
    If,
}

pub fn function(module: &Module, index: u32) -> String {
    let imported = module.imported_funcs();
    let Some(body) = module.bodies.get(index as usize - imported) else {
        return String::new();
    };
    let Some(ty) = module.func_type(index) else { return String::new() };

    let mut locals = Vec::new();
    for (i, p) in ty.params.iter().enumerate() {
        locals.push(format!("a{i}: {}", p.name()));
    }
    let named: Vec<String> = locals.iter().map(|s| s.split(':').next().unwrap().to_string()).collect();
    let mut names = named;
    for (i, l) in body.locals.iter().enumerate() {
        names.push(format!("v{}", i + ty.params.len()));
        let _ = l;
    }

    let mut lift = Lift {
        module,
        index,
        locals: names,
        stack: Vec::new(),
        out: String::new(),
        depth: 1,
        frames: Vec::new(),
        temp: 0,
    };

    lift.out.push_str(&signature(module, index, ty));
    lift.out.push_str(" {\n");
    for (i, l) in body.locals.iter().enumerate() {
        lift.out.push_str(&format!("  {} v{}\n", l.name(), i + ty.params.len()));
    }
    let mut r = Reader::new(&body.code);
    while !r.done() {
        let Some(op) = decode(&mut r) else {
            lift.line("<undecodable>".into());
            break;
        };
        lift.step(&op);
    }
    lift.out.push_str("}\n");
    lift.out
}

fn signature(module: &Module, index: u32, ty: &Type) -> String {
    let params: Vec<String> =
        ty.params.iter().enumerate().map(|(i, p)| format!("{} a{i}", p.name())).collect();
    let result = match ty.results.first() {
        Some(v) => v.name(),
        None => "void",
    };
    format!("{result} {}({})", module.func_name(index), params.join(", "))
}

impl<'a> Lift<'a> {
    fn pad(&self) -> String {
        "  ".repeat(self.depth)
    }

    fn line(&mut self, text: String) {
        let pad = self.pad();
        self.out.push_str(&pad);
        self.out.push_str(&text);
        self.out.push('\n');
    }

    fn push(&mut self, text: String) {
        self.stack.push(text);
    }

    fn pop(&mut self) -> String {
        self.stack.pop().unwrap_or_else(|| "?".into())
    }

    fn popn(&mut self, n: usize) -> Vec<String> {
        let at = self.stack.len().saturating_sub(n);
        self.stack.split_off(at)
    }

    fn bind(&mut self, text: String) -> String {
        let name = format!("t{}", self.temp);
        self.temp += 1;
        self.line(format!("{} = {}", name, text));
        name
    }

    fn results(&self, b: Block) -> usize {
        match b {
            Block::Empty => 0,
            Block::Value(_) => 1,
            Block::Type(i) => self.module.types.get(i as usize).map_or(0, |t| t.results.len()),
        }
    }

    fn params(&self, b: Block) -> usize {
        match b {
            Block::Type(i) => self.module.types.get(i as usize).map_or(0, |t| t.params.len()),
            _ => 0,
        }
    }

    fn label(&self, relative: u32) -> String {
        match self.frames.iter().rev().nth(relative as usize) {
            Some(f) => match f.kind {
                Kind::Loop => format!("continue L{}", f.label),
                _ => format!("break L{}", f.label),
            },
            None => "return".into(),
        }
    }

    fn step(&mut self, op: &Op) {
        match (op.name, &op.arg) {
            ("block" | "loop" | "if", Arg::Block(b)) => {
                let results = self.results(*b);
                let taken = self.params(*b);
                let condition = if op.name == "if" { Some(self.pop()) } else { None };
                let _ = taken;
                let label = self.frames.len();
                if let Some(c) = condition {
                    self.line(format!("L{label}: if ({c}) {{"));
                } else if op.name == "loop" {
                    self.line(format!("L{label}: loop {{"));
                } else {
                    self.line(format!("L{label}: {{"));
                }
                self.frames.push(Frame {
                    kind: match op.name {
                        "loop" => Kind::Loop,
                        "if" => Kind::If,
                        _ => Kind::Block,
                    },
                    label,
                    height: self.stack.len(),
                    results,
                });
                self.depth += 1;
            }
            ("else", _) => {
                if let Some(f) = self.frames.last() {
                    let height = f.height;
                    self.stack.truncate(height);
                }
                self.depth = self.depth.saturating_sub(1);
                self.line("} else {".into());
                self.depth += 1;
            }
            ("end", _) => {
                self.depth = self.depth.saturating_sub(1);
                if let Some(f) = self.frames.pop() {
                    self.line("}".into());
                    self.stack.truncate(f.height);
                    for i in 0..f.results {
                        self.stack.push(format!("L{}#{i}", f.label));
                    }
                } else if !self.stack.is_empty() {
                    let v = self.pop();
                    self.line(format!("return {v}"));
                }
            }
            ("br", Arg::Index(n)) => {
                let target = self.label(*n);
                self.line(target);
            }
            ("br_if", Arg::Index(n)) => {
                let c = self.pop();
                let target = self.label(*n);
                self.line(format!("if ({c}) {target}"));
            }
            ("br_table", Arg::Table(targets, default)) => {
                let c = self.pop();
                let list: Vec<String> = targets.iter().map(|t| self.label(*t)).collect();
                let fallback = self.label(*default);
                self.line(format!("switch ({c}) [{}] default {fallback}", list.join(", ")));
            }
            ("return", _) => {
                let ty = self.module.func_type(self.index);
                let n = ty.map_or(0, |t| t.results.len());
                let vals = self.popn(n);
                self.line(if vals.is_empty() {
                    "return".into()
                } else {
                    format!("return {}", vals.join(", "))
                });
            }
            ("unreachable", _) => self.line("unreachable".into()),
            ("nop", _) => {}
            ("drop", _) => {
                let v = self.pop();
                self.line(format!("drop {v}"));
            }
            ("select" | "select_t", _) => {
                let c = self.pop();
                let b = self.pop();
                let a = self.pop();
                self.push(format!("({c} ? {a} : {b})"));
            }
            ("call", Arg::Index(n)) => self.call(*n, None),
            ("call_indirect", Arg::Two(t, _)) => {
                let target = self.pop();
                let n = self.module.types.get(*t as usize).map_or(0, |x| x.params.len());
                let args = self.popn(n);
                let text = format!("table[{target}]({})", args.join(", "));
                let results = self.module.types.get(*t as usize).map_or(0, |x| x.results.len());
                if results == 0 {
                    self.line(text);
                } else {
                    let bound = self.bind(text);
                    self.push(bound);
                }
            }
            ("local.get", Arg::Index(n)) => {
                let v = self.local(*n);
                self.push(v);
            }
            ("local.set", Arg::Index(n)) => {
                let v = self.pop();
                let name = self.local(*n);
                self.line(format!("{name} = {v}"));
            }
            ("local.tee", Arg::Index(n)) => {
                let v = self.pop();
                let name = self.local(*n);
                self.line(format!("{name} = {v}"));
                self.push(name);
            }
            ("global.get", Arg::Index(n)) => self.push(format!("g{n}")),
            ("global.set", Arg::Index(n)) => {
                let v = self.pop();
                self.line(format!("g{n} = {v}"));
            }
            ("memory.size", _) => self.push("memory.size".into()),
            ("memory.grow", _) => {
                let v = self.pop();
                self.push(format!("memory.grow({v})"));
            }
            ("memory.copy", _) => {
                let n = self.pop();
                let s = self.pop();
                let d = self.pop();
                self.line(format!("copy(mem+{d}, mem+{s}, {n})"));
            }
            ("memory.fill", _) => {
                let n = self.pop();
                let v = self.pop();
                let d = self.pop();
                self.line(format!("fill(mem+{d}, {v}, {n})"));
            }
            ("i32.const", Arg::I32(v)) => self.push(v.to_string()),
            ("i64.const", Arg::I64(v)) => self.push(format!("{v}L")),
            ("f32.const", Arg::F32(v)) => self.push(format!("{v}f")),
            ("f64.const", Arg::F64(v)) => self.push(v.to_string()),
            ("ref.null", _) => self.push("null".into()),
            ("ref.func", Arg::Index(n)) => self.push(format!("&{}", self.module.func_name(*n))),
            ("ref.is_null", _) => {
                let v = self.pop();
                self.push(format!("({v} == null)"));
            }
            (name, Arg::Mem(_, offset)) if name.contains(".load") => {
                let base = self.pop();
                self.push(format!("{}[{}]", cell(name), address(&base, *offset)));
            }
            (name, Arg::Mem(_, offset)) if name.contains(".store") => {
                let v = self.pop();
                let base = self.pop();
                let text = format!("{}[{}] = {v}", cell(name), address(&base, *offset));
                self.line(text);
            }
            (name, _) => self.numeric(name),
        }
    }

    fn local(&self, n: u32) -> String {
        self.locals.get(n as usize).cloned().unwrap_or_else(|| format!("v{n}"))
    }

    fn call(&mut self, n: u32, over: Option<usize>) {
        let ty = self.module.func_type(n);
        let count = over.unwrap_or_else(|| ty.map_or(0, |t| t.params.len()));
        let args = self.popn(count);
        let name = self.module.func_name(n);
        let text = format!("{name}({})", args.join(", "));
        if ty.map_or(0, |t| t.results.len()) == 0 {
            self.line(text);
        } else {
            let bound = self.bind(text);
            self.push(bound);
        }
    }

    fn numeric(&mut self, name: &str) {
        if let Some(op) = unary(name) {
            let a = self.pop();
            self.push(format!("{op}({a})"));
            return;
        }
        if let Some(op) = binary(name) {
            let b = self.pop();
            let a = self.pop();
            self.push(format!("({a} {op} {b})"));
            return;
        }
        let (_, rest) = name.split_once('.').unwrap_or(("", name));
        let a = self.pop();
        self.push(format!("{rest}({a})"));
    }
}

fn address(base: &str, offset: u32) -> String {
    if offset == 0 { base.to_string() } else { format!("{base} + {offset}") }
}

fn cell(name: &str) -> &str {
    match name {
        n if n.contains("8_s") => "i8",
        n if n.contains("8_u") => "u8",
        n if n.contains("8") => "i8",
        n if n.contains("16_s") => "i16",
        n if n.contains("16_u") => "u16",
        n if n.contains("16") => "i16",
        n if n.contains("32_s") => "i32",
        n if n.contains("32_u") => "u32",
        n if n.starts_with("i64") => "i64",
        n if n.starts_with("f32") => "f32",
        n if n.starts_with("f64") => "f64",
        _ => "i32",
    }
}

fn binary(name: &str) -> Option<&'static str> {
    let (_, op) = name.split_once('.')?;
    Some(match op {
        "add" => "+",
        "sub" => "-",
        "mul" => "*",
        "div_s" | "div" => "/",
        "div_u" => "/u",
        "rem_s" => "%",
        "rem_u" => "%u",
        "and" => "&",
        "or" => "|",
        "xor" => "^",
        "shl" => "<<",
        "shr_s" => ">>",
        "shr_u" => ">>>",
        "rotl" => "<<<",
        "rotr" => ">>>>",
        "eq" => "==",
        "ne" => "!=",
        "lt_s" | "lt" => "<",
        "lt_u" => "<u",
        "gt_s" | "gt" => ">",
        "gt_u" => ">u",
        "le_s" | "le" => "<=",
        "le_u" => "<=u",
        "ge_s" | "ge" => ">=",
        "ge_u" => ">=u",
        "min" => "min",
        "max" => "max",
        "copysign" => "copysign",
        _ => return None,
    })
}

fn unary(name: &str) -> Option<&'static str> {
    let (_, op) = name.split_once('.')?;
    Some(match op {
        "eqz" => "!",
        "clz" => "clz",
        "ctz" => "ctz",
        "popcnt" => "popcnt",
        "abs" => "abs",
        "neg" => "-",
        "ceil" => "ceil",
        "floor" => "floor",
        "trunc" => "trunc",
        "nearest" => "nearest",
        "sqrt" => "sqrt",
        _ => return None,
    })
}

pub fn types(module: &Module) -> String {
    let mut out = String::new();
    for (i, t) in module.types.iter().enumerate() {
        let params: Vec<&str> = t.params.iter().map(|v| v.name()).collect();
        let results: Vec<&str> = t.results.iter().map(|v| v.name()).collect();
        out.push_str(&format!("type {i}: ({}) -> ({})\n", params.join(", "), results.join(", ")));
    }
    out
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
        out.push_str(&format!("element {i}: {} entries\n", e.funcs.len()));
    }
    for (i, d) in module.data.iter().enumerate() {
        out.push_str(&format!("data {i}: {} bytes\n", d.bytes.len()));
    }
    out
}

pub fn strings(module: &Module) -> String {
    let mut out = String::new();
    for (i, d) in module.data.iter().enumerate() {
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
                out.push_str(&format!("{i}+{start}: {}\n", String::from_utf8_lossy(&run)));
            }
            run.clear();
        }
        if run.len() >= 6 {
            out.push_str(&format!("{i}+{start}: {}\n", String::from_utf8_lossy(&run)));
        }
    }
    out
}

pub fn value(v: Val) -> &'static str {
    v.name()
}
