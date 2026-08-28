use crate::code::{Arg, Block, decode};
use crate::parse::{Module, Reader, Val};

#[derive(Clone, Debug)]
pub enum Expr {
    Const(i64),
    Float(f64),
    Local(u32),
    Global(u32),
    Temp(usize),
    Result(usize, usize),
    Load(&'static str, Box<Expr>, u32),
    Un(&'static str, Box<Expr>),
    Bin(&'static str, Box<Expr>, Box<Expr>),
    Select(Box<Expr>, Box<Expr>, Box<Expr>),
    Call(String, Vec<Expr>),
    Indirect(Box<Expr>, Vec<Expr>),
    Size,
    Grow(Box<Expr>),
    Null,
    FuncRef(String),
    Unknown,
}

#[derive(Clone, Debug)]
pub enum Place {
    Local(u32),
    Global(u32),
    Temp(usize),
}

#[derive(Clone, Debug)]
pub enum Stmt {
    Set(Place, Expr),
    Store(&'static str, Expr, u32, Expr),
    Effect(Expr),
    Drop(Expr),
    Block(usize, Vec<Stmt>),
    Loop(usize, Vec<Stmt>),
    If(Expr, Vec<Stmt>, Vec<Stmt>),
    Break(usize),
    BreakIf(Expr, usize),
    Continue(usize),
    ContinueIf(Expr, usize),
    Switch(Expr, Vec<usize>, usize),
    State(i64, Vec<Stmt>),
    Goto(i64),
    Return(Vec<Expr>),
    Copy(Expr, Expr, Expr),
    Fill(Expr, Expr, Expr),
    Unreachable,
}

pub struct Func {
    pub name: String,
    pub params: Vec<Val>,
    pub results: Vec<Val>,
    pub locals: Vec<Val>,
    pub body: Vec<Stmt>,
}

struct Build<'a> {
    module: &'a Module,
    stack: Vec<Expr>,
    temp: usize,
    label: usize,
}

struct Frame {
    label: usize,
    kind: Kind,
    height: usize,
    results: usize,
    body: Vec<Stmt>,
    other: Option<Vec<Stmt>>,
    condition: Option<Expr>,
}

#[derive(PartialEq)]
enum Kind {
    Block,
    Loop,
    If,
}

pub fn func(module: &Module, index: u32) -> Option<Func> {
    let imported = module.imported_funcs();
    let body = module.bodies.get(index as usize - imported)?;
    let ty = module.func_type(index)?;

    let mut b = Build { module, stack: Vec::new(), temp: 0, label: 0 };
    let mut out: Vec<Stmt> = Vec::new();
    let mut frames: Vec<Frame> = Vec::new();
    let mut r = Reader::new(&body.code);

    while !r.done() {
        let Some(op) = decode(&mut r) else { break };
        let sink = |frames: &mut Vec<Frame>, out: &mut Vec<Stmt>| -> *mut Vec<Stmt> {
            match frames.last_mut() {
                Some(f) => match &mut f.other {
                    Some(o) => o as *mut _,
                    None => &mut f.body as *mut _,
                },
                None => out as *mut _,
            }
        };
        macro_rules! emit {
            ($s:expr) => {{
                let target = sink(&mut frames, &mut out);
                unsafe { (*target).push($s) };
            }};
        }

        match (op.name, &op.arg) {
            ("block" | "loop" | "if", Arg::Block(bt)) => {
                let results = b.results(*bt);
                let condition = (op.name == "if").then(|| b.pop());
                let label = b.label;
                b.label += 1;
                frames.push(Frame {
                    label,
                    kind: match op.name {
                        "loop" => Kind::Loop,
                        "if" => Kind::If,
                        _ => Kind::Block,
                    },
                    height: b.stack.len(),
                    results,
                    body: Vec::new(),
                    other: None,
                    condition,
                });
            }
            ("else", _) => {
                if let Some(f) = frames.last_mut() {
                    b.stack.truncate(f.height);
                    f.other = Some(Vec::new());
                }
            }
            ("end", _) => {
                let Some(f) = frames.pop() else {
                    if !b.stack.is_empty() {
                        let vals = b.popn(ty.results.len());
                        out.push(Stmt::Return(vals));
                    }
                    break;
                };
                b.stack.truncate(f.height);
                let stmt = match f.kind {
                    Kind::Loop => Stmt::Loop(f.label, f.body),
                    Kind::Block => Stmt::Block(f.label, f.body),
                    Kind::If => Stmt::If(
                        f.condition.unwrap_or(Expr::Unknown),
                        f.body,
                        f.other.unwrap_or_default(),
                    ),
                };
                emit!(stmt);
                for i in 0..f.results {
                    b.stack.push(Expr::Result(f.label, i));
                }
            }
            ("br", Arg::Index(n)) => match target(&frames, *n) {
                Some((label, Kind::Loop)) => emit!(Stmt::Continue(label)),
                Some((label, _)) => emit!(Stmt::Break(label)),
                None => emit!(Stmt::Return(b.popn(ty.results.len()))),
            },
            ("br_if", Arg::Index(n)) => {
                let c = b.pop();
                match target(&frames, *n) {
                    Some((label, Kind::Loop)) => emit!(Stmt::ContinueIf(c, label)),
                    Some((label, _)) => emit!(Stmt::BreakIf(c, label)),
                    None => emit!(Stmt::If(c, vec![Stmt::Return(Vec::new())], Vec::new())),
                }
            }
            ("br_table", Arg::Table(list, default)) => {
                let c = b.pop();
                let arms: Vec<usize> =
                    list.iter().map(|n| target(&frames, *n).map_or(usize::MAX, |t| t.0)).collect();
                let fallback = target(&frames, *default).map_or(usize::MAX, |t| t.0);
                emit!(Stmt::Switch(c, arms, fallback));
            }
            ("return", _) => {
                let vals = b.popn(ty.results.len());
                emit!(Stmt::Return(vals));
            }
            ("unreachable", _) => emit!(Stmt::Unreachable),
            ("nop", _) => {}
            ("drop", _) => {
                let v = b.pop();
                emit!(Stmt::Drop(v));
            }
            ("select" | "select_t", _) => {
                let c = b.pop();
                let f = b.pop();
                let t = b.pop();
                b.stack.push(Expr::Select(Box::new(c), Box::new(t), Box::new(f)));
            }
            ("call", Arg::Index(n)) => {
                let ty = b.module.func_type(*n);
                let args = b.popn(ty.map_or(0, |t| t.params.len()));
                let call = Expr::Call(b.module.func_name(*n), args);
                if ty.map_or(0, |t| t.results.len()) == 0 {
                    emit!(Stmt::Effect(call));
                } else {
                    let slot = b.temp;
                    b.temp += 1;
                    emit!(Stmt::Set(Place::Temp(slot), call));
                    b.stack.push(Expr::Temp(slot));
                }
            }
            ("call_indirect", Arg::Two(t)) => {
                let signature = b.module.types.get(*t as usize);
                let index = b.pop();
                let args = b.popn(signature.map_or(0, |x| x.params.len()));
                let call = Expr::Indirect(Box::new(index), args);
                if signature.map_or(0, |x| x.results.len()) == 0 {
                    emit!(Stmt::Effect(call));
                } else {
                    let slot = b.temp;
                    b.temp += 1;
                    emit!(Stmt::Set(Place::Temp(slot), call));
                    b.stack.push(Expr::Temp(slot));
                }
            }
            ("local.get", Arg::Index(n)) => b.stack.push(Expr::Local(*n)),
            ("local.set", Arg::Index(n)) => {
                let v = b.pop();
                emit!(Stmt::Set(Place::Local(*n), v));
            }
            ("local.tee", Arg::Index(n)) => {
                let v = b.pop();
                emit!(Stmt::Set(Place::Local(*n), v));
                b.stack.push(Expr::Local(*n));
            }
            ("global.get", Arg::Index(n)) => b.stack.push(Expr::Global(*n)),
            ("global.set", Arg::Index(n)) => {
                let v = b.pop();
                emit!(Stmt::Set(Place::Global(*n), v));
            }
            ("memory.size", _) => b.stack.push(Expr::Size),
            ("memory.grow", _) => {
                let v = b.pop();
                b.stack.push(Expr::Grow(Box::new(v)));
            }
            ("memory.copy", _) => {
                let n = b.pop();
                let s = b.pop();
                let d = b.pop();
                emit!(Stmt::Copy(d, s, n));
            }
            ("memory.fill", _) => {
                let n = b.pop();
                let v = b.pop();
                let d = b.pop();
                emit!(Stmt::Fill(d, v, n));
            }
            ("i32.const", Arg::I32(v)) => b.stack.push(Expr::Const(*v as i64)),
            ("i64.const", Arg::I64(v)) => b.stack.push(Expr::Const(*v)),
            ("f32.const", Arg::F32(v)) => b.stack.push(Expr::Float(*v as f64)),
            ("f64.const", Arg::F64(v)) => b.stack.push(Expr::Float(*v)),
            ("ref.null", _) => b.stack.push(Expr::Null),
            ("ref.func", Arg::Index(n)) => b.stack.push(Expr::FuncRef(b.module.func_name(*n))),
            (name, Arg::Mem(_, offset)) if name.contains(".load") => {
                let addr = b.pop();
                b.stack.push(Expr::Load(cell(name), Box::new(addr), *offset));
            }
            (name, Arg::Mem(_, offset)) if name.contains(".store") => {
                let v = b.pop();
                let addr = b.pop();
                emit!(Stmt::Store(cell(name), addr, *offset, v));
            }
            (name, _) => {
                if let Some(op) = unary(name) {
                    let a = b.pop();
                    b.stack.push(Expr::Un(op, Box::new(a)));
                } else if let Some(op) = binary(name) {
                    let y = b.pop();
                    let x = b.pop();
                    b.stack.push(Expr::Bin(op, Box::new(x), Box::new(y)));
                } else {
                    let a = b.pop();
                    let converted = name.split_once('.').map_or(name, |x| x.1);
                    b.stack.push(Expr::Un(intern(converted), Box::new(a)));
                }
            }
        }
    }

    Some(Func {
        name: module.func_name(index),
        params: ty.params.clone(),
        results: ty.results.clone(),
        locals: body.locals.clone(),
        body: out,
    })
}

fn target(frames: &[Frame], relative: u32) -> Option<(usize, &Kind)> {
    frames.iter().rev().nth(relative as usize).map(|f| (f.label, &f.kind))
}

impl<'a> Build<'a> {
    fn pop(&mut self) -> Expr {
        self.stack.pop().unwrap_or(Expr::Unknown)
    }

    fn popn(&mut self, n: usize) -> Vec<Expr> {
        let at = self.stack.len().saturating_sub(n);
        self.stack.split_off(at)
    }

    fn results(&self, b: Block) -> usize {
        match b {
            Block::Empty => 0,
            Block::Value => 1,
            Block::Type(i) => self.module.types.get(i as usize).map_or(0, |t| t.results.len()),
        }
    }
}

fn intern(name: &str) -> &'static str {
    Box::leak(name.to_string().into_boxed_str())
}

fn cell(name: &str) -> &'static str {
    if name.contains("8_s") {
        "i8"
    } else if name.contains("8_u") {
        "u8"
    } else if name.contains("16_s") {
        "i16"
    } else if name.contains("16_u") {
        "u16"
    } else if name.contains("32_s") {
        "i32"
    } else if name.contains("32_u") {
        "u32"
    } else if name.contains("store8") {
        "i8"
    } else if name.contains("store16") {
        "i16"
    } else if name.contains("store32") {
        "i32"
    } else if name.starts_with("i64") {
        "i64"
    } else if name.starts_with("f32") {
        "f32"
    } else if name.starts_with("f64") {
        "f64"
    } else {
        "i32"
    }
}

pub fn binary(name: &str) -> Option<&'static str> {
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
        "rotl" => "rotl",
        "rotr" => "rotr",
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

pub fn unary(name: &str) -> Option<&'static str> {
    let (_, op) = name.split_once('.')?;
    Some(match op {
        "eqz" => "!",
        "clz" => "clz",
        "ctz" => "ctz",
        "popcnt" => "popcnt",
        "abs" => "abs",
        "neg" => "neg",
        "ceil" => "ceil",
        "floor" => "floor",
        "trunc" => "trunc",
        "nearest" => "nearest",
        "sqrt" => "sqrt",
        _ => return None,
    })
}
