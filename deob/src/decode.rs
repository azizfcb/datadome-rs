use oxc_ast::ast::*;
use oxc_ast_visit::{Visit, walk};
use oxc_semantic::{ScopeFlags, SymbolId};
use oxc_span::GetSpan;
use oxc_syntax::operator::BinaryOperator as Bin;
use rustc_hash::FxHashMap as Map;

use crate::Const;
use crate::eval::Ctx;

const STANDARD: &str = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/=";

pub fn run(ctx: &Ctx, program: &Program) -> Map<SymbolId, Vec<Option<Const>>> {
    let mut tables = Tables { ctx, out: Map::default() };
    tables.visit_program(program);

    let mut find = Find { ctx, tables: &tables.out, out: Map::default() };
    find.visit_program(program);
    find.out
}

struct Tables<'a, 'c> {
    ctx: &'a Ctx<'c>,
    out: Map<SymbolId, Vec<Option<Const>>>,
}

impl<'a, 'c, 'b> Visit<'b> for Tables<'a, 'c> {
    fn visit_variable_declarator(&mut self, d: &VariableDeclarator<'b>) {
        walk::walk_variable_declarator(self, d);
        let (BindingPattern::BindingIdentifier(id), Some(Expression::ArrayExpression(arr))) =
            (&d.id, &d.init)
        else {
            return;
        };
        let entries: Vec<Option<Const>> = arr
            .elements
            .iter()
            .map(|el| self.ctx.value(el.as_expression()?))
            .collect();
        if entries.iter().filter(|e| e.is_some()).count() >= 4 {
            self.out.insert(id.symbol_id(), entries);
        }
    }
}

struct Find<'a, 'c> {
    ctx: &'a Ctx<'c>,
    tables: &'a Map<SymbolId, Vec<Option<Const>>>,
    out: Map<SymbolId, Vec<Option<Const>>>,
}

impl<'a, 'c, 'b> Visit<'b> for Find<'a, 'c> {
    fn visit_function(&mut self, f: &Function<'b>, flags: ScopeFlags) {
        walk::walk_function(self, f, flags);
        if let Some(id) = &f.id {
            self.decoder(id.symbol_id(), &f.params, f.body.as_deref());
        }
    }

    fn visit_variable_declarator(&mut self, d: &VariableDeclarator<'b>) {
        walk::walk_variable_declarator(self, d);
        if let (BindingPattern::BindingIdentifier(id), Some(Expression::FunctionExpression(f))) =
            (&d.id, &d.init)
        {
            self.decoder(id.symbol_id(), &f.params, f.body.as_deref());
        }
    }
}

impl<'a, 'c> Find<'a, 'c> {
    fn decoder(&mut self, id: SymbolId, params: &FormalParameters, body: Option<&FunctionBody>) {
        let Some(body) = body else { return };
        let Some(BindingPattern::BindingIdentifier(p)) = params.items.first().map(|p| &p.pattern)
        else {
            return;
        };

        let mut scan =
            Scan { ctx: self.ctx, param: &p.name, table: None, alphabet: None, atob: false };
        for s in &body.statements {
            scan.visit_statement(s);
        }
        let Some((table, shift)) = scan.table else { return };
        let Some(entries) = self.tables.get(&table) else { return };
        let alphabet = match (scan.alphabet, scan.atob) {
            (Some((_, a)), _) => a,
            (None, true) => STANDARD.to_string(),
            _ => return,
        };

        let mut decoded = vec![None; entries.len().saturating_sub(shift.max(0) as usize)];
        for (i, slot) in decoded.iter_mut().enumerate() {
            let Some(e) = entries.get((i as isize + shift as isize) as usize) else { continue };
            *slot = match e {
                Some(Const::Str(s)) => b64(s, &alphabet).map(Const::Str),
                other => other.clone(),
            };
        }
        self.out.insert(id, decoded);
    }
}

struct Scan<'a, 'c> {
    ctx: &'a Ctx<'c>,
    param: &'a str,
    table: Option<(SymbolId, i32)>,
    alphabet: Option<(u32, String)>,
    atob: bool,
}

impl<'a, 'c, 'b> Visit<'b> for Scan<'a, 'c> {
    fn visit_expression(&mut self, e: &Expression<'b>) {
        walk::walk_expression(self, e);
        if let Some(Const::Str(s)) = self.ctx.value(e) {
            let width = e.span().size();
            if (64..=65).contains(&s.chars().count())
                && self.alphabet.as_ref().is_none_or(|(w, _)| width > *w)
            {
                self.alphabet = Some((width, s));
            }
        }
    }

    fn visit_computed_member_expression(&mut self, m: &ComputedMemberExpression<'b>) {
        walk::walk_computed_member_expression(self, m);
        let Expression::Identifier(obj) = &m.object else { return };
        let Some(shift) = self.index(&m.expression) else { return };
        if let Some(t) = self.ctx.symbol(obj) {
            self.table = Some((t, shift));
        }
    }

    fn visit_call_expression(&mut self, c: &CallExpression<'b>) {
        walk::walk_call_expression(self, c);
        self.atob |= matches!(&c.callee, Expression::Identifier(i) if i.name == "atob");
    }
}

impl<'a, 'c> Scan<'a, 'c> {
    fn index(&self, e: &Expression) -> Option<i32> {
        match e {
            Expression::Identifier(i) if i.name == self.param => Some(0),
            Expression::BinaryExpression(b)
                if matches!(b.operator, Bin::Addition | Bin::Subtraction) =>
            {
                let base = self.index(&b.left)?;
                let Some(Const::Num(k)) = self.ctx.value(&b.right) else { return None };
                let k = k as i32;
                Some(if b.operator == Bin::Addition { base + k } else { base - k })
            }
            _ => None,
        }
    }
}

fn b64(input: &str, alphabet: &str) -> Option<String> {
    let map: Vec<char> = alphabet.chars().collect();
    let (mut bits, mut have) = (0u32, 0u32);
    let mut out = String::new();
    for ch in input.chars() {
        if !STANDARD.contains(ch) {
            continue;
        }
        let v = map.iter().position(|c| *c == ch)?;
        if v >= 64 {
            continue;
        }
        bits = bits << 6 | v as u32;
        have += 6;
        if have >= 8 {
            have -= 8;
            out.push(((bits >> have) & 0xff) as u8 as char);
        }
    }
    Some(out)
}
