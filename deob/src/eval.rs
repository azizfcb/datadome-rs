use oxc_ast::ast::*;
use oxc_semantic::{Scoping, SymbolId};
use oxc_syntax::operator::{BinaryOperator as Bin, LogicalOperator as Logical, UnaryOperator as Un};
use rustc_hash::{FxHashMap as Map, FxHashSet as Set};

use crate::Const;
use crate::collect::{Expr, Node};

pub struct Ctx<'a> {
    pub scoping: &'a Scoping,
    pub fns: Map<SymbolId, Node>,
    pub char_code: Set<SymbolId>,
    pub globals: Set<SymbolId>,
    pub decoders: Map<SymbolId, Vec<Option<Const>>>,
    pub consts: Map<SymbolId, Const>,
    pub grids: Map<SymbolId, crate::opaque::Grid>,
}

#[derive(Clone, Copy, PartialEq)]
enum Path {
    Global,
    Math,
    StringCtor,
    Call(Builtin),
}

#[derive(Clone, Copy, PartialEq)]
enum Builtin {
    Number,
    ParseInt,
    ParseFloat,
    FromCharCode,
    Floor,
    Ceil,
    Round,
    Abs,
    Trunc,
    Sqrt,
    Sign,
    Min,
    Max,
    Pow,
}

impl<'a> Ctx<'a> {
    pub fn symbol(&self, r: &IdentifierReference) -> Option<SymbolId> {
        self.scoping.get_reference(r.reference_id()).symbol_id()
    }

    pub fn value(&self, e: &Expression) -> Option<Const> {
        self.value_in(e, &mut Vec::new())
    }

    pub fn value_in(&self, e: &Expression, binds: &mut Vec<(SymbolId, Const)>) -> Option<Const> {
        match e {
            Expression::NumericLiteral(n) => Some(Const::Num(n.value)),
            Expression::StringLiteral(s) => Some(Const::Str(s.value.to_string())),
            Expression::BooleanLiteral(b) => Some(Const::Bool(b.value)),
            Expression::NullLiteral(_) => Some(Const::Null),
            Expression::Identifier(i) => {
                let s = self.symbol(i)?;
                binds
                    .iter()
                    .find(|(b, _)| *b == s)
                    .map(|(_, v)| v.clone())
                    .or_else(|| self.consts.get(&s).cloned())
            }
            Expression::UnaryExpression(u) => match (u.operator, self.value_in(&u.argument, binds)?) {
                (Un::LogicalNot, v) => Some(Const::Bool(!truthy(&v))),
                (op, Const::Num(v)) => finite(un(op, v)?),
                _ => None,
            },
            Expression::BinaryExpression(b) => {
                let left = self.value_in(&b.left, binds)?;
                let right = self.value_in(&b.right, binds)?;
                binary(b.operator, left, right)
            }
            Expression::AssignmentExpression(a)
                if a.operator == oxc_syntax::operator::AssignmentOperator::Assign =>
            {
                let value = self.value_in(&a.right, binds)?;
                if let AssignmentTarget::AssignmentTargetIdentifier(id) = &a.left
                    && let Some(symbol) =
                        id.reference_id.get().and_then(|r| self.scoping.get_reference(r).symbol_id())
                {
                    binds.retain(|(s, _)| *s != symbol);
                    binds.push((symbol, value.clone()));
                }
                Some(value)
            }
            Expression::SequenceExpression(s) => {
                let mut last = None;
                for x in &s.expressions {
                    last = self.value_in(x, binds);
                }
                last
            }
            Expression::LogicalExpression(l) => {
                let left = self.value_in(&l.left, binds)?;
                let take = match l.operator {
                    Logical::And => !truthy(&left),
                    Logical::Or => truthy(&left),
                    Logical::Coalesce => return None,
                };
                if take { Some(left) } else { self.value_in(&l.right, binds) }
            }
            Expression::ConditionalExpression(c) => {
                if truthy(&self.value_in(&c.test, binds)?) {
                    self.value_in(&c.consequent, binds)
                } else {
                    self.value_in(&c.alternate, binds)
                }
            }
            Expression::CallExpression(c) => self.call(c, binds),
            Expression::ComputedMemberExpression(m) => self.grid(m, binds),
            _ => None,
        }
    }

    fn call(&self, c: &CallExpression, binds: &mut Vec<(SymbolId, Const)>) -> Option<Const> {
        if let Expression::Identifier(callee) = &c.callee {
            if let Some(symbol) = self.symbol(callee) {
                if let Some(table) = self.decoders.get(&symbol) {
                    let Const::Num(i) = self.value_in(c.arguments.first()?.as_expression()?, binds)?
                    else {
                        return None;
                    };
                    return table.get(i as usize).cloned().flatten();
                }
                if let Some(f) = self.fns.get(&symbol) {
                    return finite(f.apply(&self.nums(c, binds)?)?);
                }
                if self.char_code.contains(&symbol) {
                    return from_char_code(&self.nums(c, binds)?);
                }
            }
        }
        let Path::Call(b) = self.path(&c.callee)? else { return None };
        self.builtin(b, c, binds)
    }

    fn nums(&self, c: &CallExpression, binds: &mut Vec<(SymbolId, Const)>) -> Option<Vec<f64>> {
        let mut out = Vec::with_capacity(c.arguments.len());
        for a in &c.arguments {
            match self.value_in(a.as_expression()?, binds)? {
                Const::Num(n) => out.push(n),
                _ => return None,
            }
        }
        Some(out)
    }

    fn builtin(&self, b: Builtin, c: &CallExpression, binds: &mut Vec<(SymbolId, Const)>) -> Option<Const> {
        if b == Builtin::FromCharCode {
            return from_char_code(&self.nums(c, binds)?);
        }
        if b == Builtin::Number {
            return match self.value_in(c.arguments.first()?.as_expression()?, binds)? {
                Const::Num(n) => finite(n),
                Const::Str(s) => finite(to_number(&s)?),
                Const::Bool(v) => finite(v as u8 as f64),
                Const::Null => finite(0.0),
            };
        }
        let a = self.nums(c, binds)?;
        let x = *a.first()?;
        let v = match b {
            Builtin::ParseInt => {
                if a.len() > 1 && a[1] != 10.0 && a[1] != 0.0 {
                    return None;
                }
                parse_int(x)?
            }
            Builtin::ParseFloat => x,
            Builtin::Floor => x.floor(),
            Builtin::Ceil => x.ceil(),
            Builtin::Round => (x + 0.5).floor(),
            Builtin::Abs => x.abs(),
            Builtin::Trunc => x.trunc(),
            Builtin::Sqrt => x.sqrt(),
            Builtin::Sign => x.signum(),
            Builtin::Min => a.iter().copied().fold(f64::INFINITY, f64::min),
            Builtin::Max => a.iter().copied().fold(f64::NEG_INFINITY, f64::max),
            Builtin::Pow => x.powf(*a.get(1)?),
            Builtin::Number | Builtin::FromCharCode => unreachable!(),
        };
        finite(v)
    }

    fn grid(&self, m: &ComputedMemberExpression, binds: &mut Vec<(SymbolId, Const)>) -> Option<Const> {
        let Expression::ComputedMemberExpression(row) = &m.object else { return None };
        let Expression::Identifier(name) = &row.object else { return None };
        let grid = self.grids.get(&self.symbol(name)?)?;
        let i = self.value_in(&row.expression, binds)?;
        let j = self.value_in(&m.expression, binds)?;
        let (Const::Num(i), Const::Num(j)) = (i, j) else { return None };
        finite(grid.at(i, j)? as f64)
    }

    fn path(&self, e: &Expression) -> Option<Path> {
        match e {
            Expression::Identifier(i) => match self.symbol(i) {
                Some(s) if self.globals.contains(&s) => Some(Path::Global),
                Some(_) => None,
                None => root(&i.name),
            },
            Expression::StaticMemberExpression(m) => {
                member(self.path(&m.object)?, &m.property.name)
            }
            Expression::ComputedMemberExpression(m) => {
                let Const::Str(key) = self.value(&m.expression)? else { return None };
                member(self.path(&m.object)?, &key)
            }
            _ => None,
        }
    }
}

fn root(name: &str) -> Option<Path> {
    Some(match name {
        "window" | "globalThis" | "self" => Path::Global,
        "Math" => Path::Math,
        "String" => Path::StringCtor,
        "Number" => Path::Call(Builtin::Number),
        "parseInt" => Path::Call(Builtin::ParseInt),
        "parseFloat" => Path::Call(Builtin::ParseFloat),
        _ => return None,
    })
}

fn member(base: Path, key: &str) -> Option<Path> {
    Some(match base {
        Path::Global => root(key)?,
        Path::StringCtor if key == "fromCharCode" => Path::Call(Builtin::FromCharCode),
        Path::Math => Path::Call(match key {
            "floor" => Builtin::Floor,
            "ceil" => Builtin::Ceil,
            "round" => Builtin::Round,
            "abs" => Builtin::Abs,
            "trunc" => Builtin::Trunc,
            "sqrt" => Builtin::Sqrt,
            "sign" => Builtin::Sign,
            "min" => Builtin::Min,
            "max" => Builtin::Max,
            "pow" => Builtin::Pow,
            _ => return None,
        }),
        _ => return None,
    })
}

fn from_char_code(args: &[f64]) -> Option<Const> {
    args.iter()
        .map(|n| char::from_u32(to_i32(*n) as u32 & 0xffff))
        .collect::<Option<String>>()
        .map(Const::Str)
}

fn parse_int(x: f64) -> Option<f64> {
    let s = format!("{x}");
    if s.contains(['e', 'E']) || !x.is_finite() {
        return None;
    }
    let digits = s.trim_start_matches(['+', '-']).find(|c: char| !c.is_ascii_digit());
    let end = digits.map_or(s.len(), |i| i + s.len() - s.trim_start_matches(['+', '-']).len());
    s[..end].parse().ok()
}

fn to_number(s: &str) -> Option<f64> {
    let t = s.trim();
    if t.is_empty() {
        return Some(0.0);
    }
    t.parse().ok()
}

pub fn truthy(v: &Const) -> bool {
    match v {
        Const::Num(n) => *n != 0.0 && !n.is_nan(),
        Const::Str(s) => !s.is_empty(),
        Const::Bool(b) => *b,
        Const::Null => false,
    }
}

pub fn finite(v: f64) -> Option<Const> {
    v.is_finite().then_some(Const::Num(v))
}

pub fn to_i32(x: f64) -> i32 {
    if !x.is_finite() {
        return 0;
    }
    let m = x.trunc().rem_euclid(4294967296.0);
    if m >= 2147483648.0 { (m - 4294967296.0) as i32 } else { m as i32 }
}

fn to_string(v: &Const) -> Option<String> {
    Some(match v {
        Const::Str(s) => s.clone(),
        Const::Bool(b) => b.to_string(),
        Const::Null => "null".to_string(),
        Const::Num(n) => {
            let s = format!("{n}");
            if s.contains(['e', 'E', 'i', 'N']) {
                return None;
            }
            s
        }
    })
}

fn to_num(v: &Const) -> Option<f64> {
    Some(match v {
        Const::Num(n) => *n,
        Const::Bool(b) => *b as u8 as f64,
        Const::Null => 0.0,
        Const::Str(s) => to_number(s)?,
    })
}

fn binary(op: Bin, l: Const, r: Const) -> Option<Const> {
    use Bin::*;
    match op {
        Addition => match (&l, &r) {
            (Const::Str(_), _) | (_, Const::Str(_)) => {
                Some(Const::Str(to_string(&l)? + &to_string(&r)?))
            }
            _ => finite(to_num(&l)? + to_num(&r)?),
        },
        LessThan | LessEqualThan | GreaterThan | GreaterEqualThan => {
            let ord = match (&l, &r) {
                (Const::Str(a), Const::Str(b)) => a.cmp(b),
                _ => to_num(&l)?.partial_cmp(&to_num(&r)?)?,
            };
            Some(Const::Bool(match op {
                LessThan => ord.is_lt(),
                LessEqualThan => ord.is_le(),
                GreaterThan => ord.is_gt(),
                _ => ord.is_ge(),
            }))
        }
        StrictEquality | StrictInequality => {
            let eq = match (&l, &r) {
                (Const::Num(a), Const::Num(b)) => a == b,
                (Const::Str(a), Const::Str(b)) => a == b,
                (Const::Bool(a), Const::Bool(b)) => a == b,
                (Const::Null, Const::Null) => true,
                _ => false,
            };
            Some(Const::Bool(eq == (op == StrictEquality)))
        }
        Equality | Inequality => {
            let eq = match (&l, &r) {
                (Const::Null, Const::Null) => true,
                (Const::Null, _) | (_, Const::Null) => false,
                (Const::Str(a), Const::Str(b)) => a == b,
                _ => to_num(&l)? == to_num(&r)?,
            };
            Some(Const::Bool(eq == (op == Equality)))
        }
        _ => finite(bin(op, to_num(&l)?, to_num(&r)?)?),
    }
}

fn un(op: Un, v: f64) -> Option<f64> {
    Some(match op {
        Un::UnaryNegation => -v,
        Un::UnaryPlus => v,
        Un::BitwiseNot => !to_i32(v) as f64,
        _ => return None,
    })
}

fn bin(op: Bin, l: f64, r: f64) -> Option<f64> {
    Some(match op {
        Bin::Addition => l + r,
        Bin::Subtraction => l - r,
        Bin::Multiplication => l * r,
        Bin::Division => l / r,
        Bin::Remainder => l % r,
        Bin::Exponential => l.powf(r),
        Bin::BitwiseAnd => (to_i32(l) & to_i32(r)) as f64,
        Bin::BitwiseOR => (to_i32(l) | to_i32(r)) as f64,
        Bin::BitwiseXOR => (to_i32(l) ^ to_i32(r)) as f64,
        Bin::ShiftLeft => (to_i32(l) << (to_i32(r) & 31)) as f64,
        Bin::ShiftRight => (to_i32(l) >> (to_i32(r) & 31)) as f64,
        Bin::ShiftRightZeroFill => ((to_i32(l) as u32) >> (to_i32(r) & 31)) as f64,
        _ => return None,
    })
}

impl Node {
    pub fn apply(&self, args: &[f64]) -> Option<f64> {
        let mut env: Map<String, f64> = Map::default();
        for (p, v) in self.params.iter().zip(args) {
            env.insert(p.clone(), *v);
        }
        for (name, e) in &self.locals {
            let v = eval(e, &mut env)?;
            env.insert(name.clone(), v);
        }
        eval(&self.body, &mut env)
    }
}

fn eval(e: &Expr, env: &mut Map<String, f64>) -> Option<f64> {
    Some(match e {
        Expr::Lit(v) => *v,
        Expr::Var(n) => *env.get(n)?,
        Expr::Assign(n, a) => {
            let v = eval(a, env)?;
            env.insert(n.clone(), v);
            v
        }
        Expr::Un(op, a) => un(*op, eval(a, env)?)?,
        Expr::Bin(op, a, b) => {
            let l = eval(a, env)?;
            let r = eval(b, env)?;
            bin(*op, l, r)?
        }
        Expr::Seq(list) => {
            let mut last = None;
            for x in list {
                last = Some(eval(x, env)?);
            }
            last?
        }
    })
}
