use crate::api::Width;
use crate::konst::{Consts, Tag};
use crate::ops::{Layout, Step};
use std::collections::BTreeMap;

#[derive(Debug)]
pub enum Value {
    Num(f64),
    Bool(bool),
    Null,
    Undefined,
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Value::Num(n) => write!(f, "{n}"),
            Value::Bool(b) => write!(f, "{b}"),
            Value::Null => f.write_str("null"),
            Value::Undefined => f.write_str("undefined"),
        }
    }
}

struct Reader<'a> {
    code: &'a [i32],
    ip: usize,
}

impl<'a> Reader<'a> {
    fn u8(&mut self) -> u32 {
        let v = self.code[self.ip] as u32 & 0xff;
        self.ip += 1;
        v
    }

    fn take(&mut self, n: usize) -> u32 {
        (0..n).fold(0, |acc, _| acc << 8 | self.u8())
    }

    fn constant(&mut self, c: &Consts) -> Option<Value> {
        let tag = self.u8();
        if tag & c.small_bit != 0 {
            return Some(Value::Num((tag & c.small_mask) as f64));
        }
        Some(match c.tags.get(&(tag as u8))? {
            Tag::Bool(b) => Value::Bool(*b),
            Tag::Null => Value::Null,
            Tag::Undefined => Value::Undefined,
            Tag::Int(n) => {
                let shift = 32 - 8 * *n as u32;
                Value::Num(((self.take(*n) << shift) as i32 >> shift) as f64)
            }
            Tag::Float => {
                let bytes: Vec<u32> = (0..8).map(|_| self.u8()).collect();
                Value::Num(float(&bytes))
            }
        })
    }
}

fn float(q: &[u32]) -> f64 {
    let sign = if q[0] >> 7 == 1 { -1.0 } else { 1.0 };
    let exp = ((q[0] & 127) << 4 | q[1] >> 4 & 15) as i32;
    if exp == 2047 {
        return if q[7] & 1 == 1 { f64::NAN } else { sign * f64::INFINITY };
    }
    let mut m = 1.0f64;
    for c in 0..=52 {
        let w = c + 12;
        let bit = q.get(w / 8).copied().unwrap_or(0) >> (7 - (w & 7)) & 1;
        m += bit as f64 / 2f64.powi(c as i32 + 1);
    }
    sign * m * 2f64.powi(exp - 1023)
}

#[derive(Debug)]
pub enum Operand {
    Int(u32),
    Const(Value),
    List(Vec<Operand>),
}

impl std::fmt::Display for Operand {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Operand::Int(v) => write!(f, "{v}"),
            Operand::Const(v) => write!(f, "{v}"),
            Operand::List(v) => {
                f.write_str("[")?;
                for (i, x) in v.iter().enumerate() {
                    write!(f, "{}{x}", if i == 0 { "" } else { ", " })?;
                }
                f.write_str("]")
            }
        }
    }
}

impl Operand {
    pub fn int(&self) -> Option<u32> {
        match self {
            Operand::Int(v) => Some(*v),
            Operand::Const(Value::Num(n)) => Some(*n as u32),
            _ => None,
        }
    }

    pub fn list(&self) -> Option<Vec<f64>> {
        let Operand::List(v) = self else { return None };
        v.iter()
            .map(|x| match x {
                Operand::Int(n) => Some(*n as f64),
                Operand::Const(Value::Num(n)) => Some(*n),
                _ => None,
            })
            .collect()
    }

    pub fn bytes(&self) -> Option<Vec<u8>> {
        let Operand::List(v) = self else { return None };
        v.iter().map(|x| x.int().map(|n| n as u8)).collect()
    }
}

impl Insn {
    pub fn numbers(&self) -> Vec<i64> {
        self.operands
            .iter()
            .map(|o| match o {
                Operand::List(v) => v.len() as i64,
                _ => o.int().unwrap_or(0) as i64,
            })
            .collect()
    }
}

pub struct Insn {
    pub at: usize,
    pub op: u8,
    pub operands: Vec<Operand>,
    pub target: Option<usize>,
}

pub fn disassemble(
    code: &[i32],
    entry: usize,
    layouts: &BTreeMap<u8, Layout>,
    consts: &Consts,
) -> (Vec<Insn>, Option<String>) {
    let mut r = Reader { code, ip: entry };
    let mut out = Vec::new();
    while r.ip < code.len() {
        let at = r.ip;
        let op = r.u8() as u8;
        let Some(layout) = layouts.get(&op) else {
            return (out, Some(format!("no handler for op {op} at {at}")));
        };
        let mut operands = Vec::new();
        if read(&mut r, &layout.steps, &mut operands, consts).is_none() {
            return (out, Some(format!("bad operand at {at} (op {op})")));
        }
        let target = layout.jump.and_then(|(i, back)| {
            let d = operands.get(i)?.int()? as usize;
            if back { r.ip.checked_sub(d) } else { Some(r.ip + d) }
        });
        out.push(Insn { at, op, operands, target });
    }
    (out, None)
}

fn read(r: &mut Reader, steps: &[Step], out: &mut Vec<Operand>, consts: &Consts) -> Option<()> {
    for step in steps {
        match step {
            Step::Read(w) => out.push(match w {
                Width::U8 => Operand::Int(r.take(1)),
                Width::U16 => Operand::Int(r.take(2)),
                Width::U24 => Operand::Int(r.take(3)),
                Width::Const => Operand::Const(r.constant(consts)?),
            }),
            Step::Repeat(at, body) => {
                let n = out.get(*at)?.int()?;
                let mut group = Vec::new();
                for _ in 0..n {
                    read(r, body, &mut group, consts)?;
                }
                out.push(Operand::List(group));
            }
        }
    }
    Some(())
}
