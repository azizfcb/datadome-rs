use rustc_hash::FxHashMap as Map;

use crate::ir::{Expr, Place, Stmt};

const BUDGET: usize = 400000;
const STATES: usize = 4096;

pub fn run(body: &mut Vec<Stmt>) {
    let mut env = Map::default();
    resolve(body, &mut env);
}

fn resolve(stmts: &mut Vec<Stmt>, env: &mut Map<u32, i64>) {
    for s in stmts.iter_mut() {
        match s {
            Stmt::Set(Place::Local(n), e) => match value(e, env) {
                Some(v) => {
                    env.insert(*n, v);
                }
                None => {
                    env.remove(n);
                }
            },
            Stmt::Loop(label, inner) => {
                match machine(inner, *label, env) {
                    Some(states) => *s = Stmt::Block(*label, states),
                    None => {
                        let mut scoped = Map::default();
                        resolve(inner, &mut scoped);
                    }
                }
                env.clear();
            }
            Stmt::Block(_, inner) => {
                let mut scoped = env.clone();
                resolve(inner, &mut scoped);
                env.clear();
            }
            Stmt::If(_, a, b) => {
                let mut scoped = env.clone();
                resolve(a, &mut scoped);
                let mut scoped = env.clone();
                resolve(b, &mut scoped);
                env.clear();
            }
            _ => env.clear(),
        }
    }
}

fn machine(body: &[Stmt], label: usize, env: &Map<u32, i64>) -> Option<Vec<Stmt>> {
    let state = register(body, env)?;
    let entry = *env.get(&state)?;

    let mut order = vec![entry];
    let mut done: Vec<i64> = Vec::new();
    let mut out = Vec::new();
    let mut at = 0;

    while at < order.len() {
        let current = order[at];
        at += 1;
        if done.contains(&current) {
            continue;
        }
        done.push(current);
        if done.len() > STATES {
            return None;
        }

        let mut seed = env.clone();
        seed.insert(state, current);
        let mut walk = Walk {
            env: seed,
            out: Vec::new(),
            steps: 0,
            state,
            successors: Vec::new(),
            assigned: Vec::new(),
        };
        let flow = walk.block(body);
        let mut inner = walk.out;

        let mut settled: Vec<(u32, i64)> = walk
            .env
            .iter()
            .filter(|(n, v)| **n != state && walk.assigned.contains(n) && env.get(n) != Some(v))
            .map(|(n, v)| (*n, *v))
            .collect();
        settled.sort();
        for (n, v) in settled {
            inner.push(Stmt::Set(Place::Local(n), Expr::Const(v)));
        }

        let mut next = walk.successors;
        match flow {
            Flow::Continue(l) if l == label => {
                if let Some(v) = walk.env.get(&state) {
                    next.push(*v);
                }
            }
            Flow::Break(l) if l == label => {}
            Flow::Fall => {}
            Flow::Break(l) => inner.push(Stmt::Break(l)),
            _ => return None,
        }

        next.sort();
        next.dedup();
        for n in &next {
            inner.push(Stmt::Goto(*n));
            order.push(*n);
        }
        out.push(Stmt::State(current, inner));
    }
    Some(out)
}

fn register(body: &[Stmt], env: &Map<u32, i64>) -> Option<u32> {
    let mut counts: Map<u32, usize> = Map::default();
    fn walk(stmts: &[Stmt], counts: &mut Map<u32, usize>, depth: usize) {
        if depth > 40 {
            return;
        }
        for s in stmts {
            match s {
                Stmt::Set(Place::Local(n), Expr::Bin("^", a, b))
                    if matches!(**a, Expr::Local(x) if x == *n)
                        && matches!(**b, Expr::Local(_) | Expr::Const(_)) =>
                {
                    *counts.entry(*n).or_default() += 1;
                }
                Stmt::Block(_, inner) | Stmt::Loop(_, inner) => walk(inner, counts, depth + 1),
                Stmt::If(_, a, b) => {
                    walk(a, counts, depth + 1);
                    walk(b, counts, depth + 1);
                }
                _ => {}
            }
        }
    }
    walk(body, &mut counts, 0);
    counts
        .into_iter()
        .filter(|(n, _)| env.contains_key(n))
        .max_by_key(|(_, c)| *c)
        .filter(|(_, c)| *c >= 3)
        .map(|(n, _)| n)
}

enum Flow {
    Fall,
    Break(usize),
    Continue(usize),
    Stop,
}

struct Walk {
    env: Map<u32, i64>,
    out: Vec<Stmt>,
    steps: usize,
    state: u32,
    successors: Vec<i64>,
    assigned: Vec<u32>,
}

impl Walk {
    fn block(&mut self, stmts: &[Stmt]) -> Flow {
        for s in stmts {
            self.steps += 1;
            if self.steps > BUDGET {
                return Flow::Stop;
            }
            match s {
                Stmt::Set(Place::Local(n), e) => {
                    self.assigned.push(*n);
                    match value(e, &self.env) {
                        Some(v) => {
                            self.env.insert(*n, v);
                        }
                        None => {
                            self.env.remove(n);
                            self.out.push(Stmt::Set(Place::Local(*n), fold(e, &self.env)));
                        }
                    }
                }
                Stmt::If(c, a, b) => match value(c, &self.env) {
                    Some(v) => match self.block(if v != 0 { a } else { b }) {
                        Flow::Fall => {}
                        other => return other,
                    },
                    None => {
                        let first = self.fork(a);
                        let second = self.fork(b);
                        self.out.push(Stmt::If(fold(c, &self.env), first, second));
                        for n in touched(a).into_iter().chain(touched(b)) {
                            self.env.remove(&n);
                            self.assigned.push(n);
                        }
                    }
                },
                Stmt::Block(l, inner) => match self.block(inner) {
                    Flow::Break(x) if x == *l => {}
                    Flow::Fall => {}
                    other => return other,
                },
                Stmt::Loop(l, inner) => loop {
                    self.steps += 1;
                    if self.steps > BUDGET {
                        return Flow::Stop;
                    }
                    match self.block(inner) {
                        Flow::Continue(x) if x == *l => continue,
                        Flow::Break(x) if x == *l => break,
                        Flow::Fall => break,
                        other => return other,
                    }
                },
                Stmt::Break(l) => return Flow::Break(*l),
                Stmt::Continue(l) => return Flow::Continue(*l),
                Stmt::BreakIf(c, l) => match value(c, &self.env) {
                    Some(v) if v != 0 => return Flow::Break(*l),
                    Some(_) => {}
                    None => self.out.push(Stmt::BreakIf(fold(c, &self.env), *l)),
                },
                Stmt::ContinueIf(c, l) => match value(c, &self.env) {
                    Some(v) if v != 0 => return Flow::Continue(*l),
                    Some(_) => {}
                    None => self.out.push(Stmt::ContinueIf(fold(c, &self.env), *l)),
                },
                Stmt::Switch(c, arms, default) => match value(c, &self.env) {
                    Some(v) => {
                        let index = usize::try_from(v).ok().filter(|i| *i < arms.len());
                        return Flow::Break(index.map_or(*default, |i| arms[i]));
                    }
                    None => return Flow::Stop,
                },
                Stmt::Return(_) | Stmt::Unreachable => {
                    self.out.push(s.clone());
                    return Flow::Stop;
                }
                other => self.out.push(fold_stmt(other, &self.env)),
            }
        }
        Flow::Fall
    }

    fn fork(&mut self, stmts: &[Stmt]) -> Vec<Stmt> {
        let mut arm = Walk {
            env: self.env.clone(),
            out: Vec::new(),
            steps: 0,
            state: self.state,
            successors: Vec::new(),
            assigned: Vec::new(),
        };
        let flow = arm.block(stmts);
        let mut out = arm.out;
        if let Flow::Continue(_) = flow
            && let Some(v) = arm.env.get(&arm.state)
        {
            self.successors.push(*v);
            out.push(Stmt::Goto(*v));
        }
        self.successors.extend(arm.successors);
        out
    }
}

fn touched(stmts: &[Stmt]) -> Vec<u32> {
    let mut out = Vec::new();
    fn walk(stmts: &[Stmt], out: &mut Vec<u32>) {
        for s in stmts {
            match s {
                Stmt::Set(Place::Local(n), _) => out.push(*n),
                Stmt::Block(_, inner) | Stmt::Loop(_, inner) => walk(inner, out),
                Stmt::If(_, a, b) => {
                    walk(a, out);
                    walk(b, out);
                }
                _ => {}
            }
        }
    }
    walk(stmts, &mut out);
    out
}

pub fn value(e: &Expr, env: &Map<u32, i64>) -> Option<i64> {
    Some(match e {
        Expr::Const(v) => *v,
        Expr::Local(n) => *env.get(n)?,
        Expr::Un(op, a) => {
            let x = value(a, env)?;
            match *op {
                "!" => (x == 0) as i64,
                "neg" => -x,
                "clz" => (x as u32).leading_zeros() as i64,
                "ctz" => (x as u32).trailing_zeros() as i64,
                "popcnt" => (x as u32).count_ones() as i64,
                _ => return None,
            }
        }
        Expr::Bin(op, a, b) => {
            let (x, y) = (value(a, env)?, value(b, env)?);
            let (u, v) = (x as i32, y as i32);
            match *op {
                "+" => u.wrapping_add(v) as i64,
                "-" => u.wrapping_sub(v) as i64,
                "*" => u.wrapping_mul(v) as i64,
                "&" => (u & v) as i64,
                "|" => (u | v) as i64,
                "^" => (u ^ v) as i64,
                "<<" => u.wrapping_shl(v as u32 & 31) as i64,
                ">>" => u.wrapping_shr(v as u32 & 31) as i64,
                ">>>" => (u as u32).wrapping_shr(v as u32 & 31) as i64,
                "rotl" => (u as u32).rotate_left(v as u32 & 31) as i32 as i64,
                "rotr" => (u as u32).rotate_right(v as u32 & 31) as i32 as i64,
                "==" => (u == v) as i64,
                "!=" => (u != v) as i64,
                "<" => (u < v) as i64,
                ">" => (u > v) as i64,
                "<=" => (u <= v) as i64,
                ">=" => (u >= v) as i64,
                "<u" => ((u as u32) < v as u32) as i64,
                ">u" => ((u as u32) > v as u32) as i64,
                "<=u" => ((u as u32) <= v as u32) as i64,
                ">=u" => ((u as u32) >= v as u32) as i64,
                "/" if v != 0 => u.wrapping_div(v) as i64,
                "/u" if v != 0 => ((u as u32) / v as u32) as i64,
                "%" if v != 0 => u.wrapping_rem(v) as i64,
                "%u" if v != 0 => ((u as u32) % v as u32) as i64,
                _ => return None,
            }
        }
        Expr::Select(c, a, b) => {
            if value(c, env)? != 0 { value(a, env)? } else { value(b, env)? }
        }
        _ => return None,
    })
}

fn fold(e: &Expr, env: &Map<u32, i64>) -> Expr {
    if let Some(v) = value(e, env) {
        return Expr::Const(v);
    }
    match e {
        Expr::Un(op, a) => Expr::Un(op, Box::new(fold(a, env))),
        Expr::Bin(op, a, b) => Expr::Bin(op, Box::new(fold(a, env)), Box::new(fold(b, env))),
        Expr::Select(c, a, b) => Expr::Select(
            Box::new(fold(c, env)),
            Box::new(fold(a, env)),
            Box::new(fold(b, env)),
        ),
        Expr::Load(cell, a, off) => Expr::Load(cell, Box::new(fold(a, env)), *off),
        Expr::Call(name, args) => {
            Expr::Call(name.clone(), args.iter().map(|a| fold(a, env)).collect())
        }
        Expr::Indirect(i, args) => {
            Expr::Indirect(Box::new(fold(i, env)), args.iter().map(|a| fold(a, env)).collect())
        }
        other => other.clone(),
    }
}

fn fold_stmt(s: &Stmt, env: &Map<u32, i64>) -> Stmt {
    match s {
        Stmt::Set(p, e) => Stmt::Set(p.clone(), fold(e, env)),
        Stmt::Store(cell, a, off, v) => Stmt::Store(cell, fold(a, env), *off, fold(v, env)),
        Stmt::Effect(e) => Stmt::Effect(fold(e, env)),
        Stmt::Drop(e) => Stmt::Drop(fold(e, env)),
        Stmt::Copy(a, b, c) => Stmt::Copy(fold(a, env), fold(b, env), fold(c, env)),
        Stmt::Fill(a, b, c) => Stmt::Fill(fold(a, env), fold(b, env), fold(c, env)),
        other => other.clone(),
    }
}
