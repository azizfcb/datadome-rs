use oxc_allocator::{Allocator, TakeIn, Vec as ArenaVec};
use oxc_ast::ast::*;
use oxc_ast::builder::AstBuilder;
use oxc_ast_visit::{VisitMut, walk_mut};
use rustc_hash::{FxHashMap as Map, FxHashSet as Set};

pub fn run<'a>(alloc: &'a Allocator, program: &mut Program<'a>) {
    Flatten { ast: AstBuilder::new(alloc) }.visit_program(program);
}

struct Flatten<'a> {
    ast: AstBuilder<'a>,
}

impl<'a> VisitMut<'a> for Flatten<'a> {
    fn visit_statement(&mut self, s: &mut Statement<'a>) {
        walk_mut::walk_statement(self, s);
        let Statement::ForStatement(f) = s else { return };
        let Some(body) = self.trace(f) else { return };
        *s = Statement::new_block_statement(f.span, body, &self.ast);
    }
}

impl<'a> Flatten<'a> {
    fn trace(&self, f: &mut ForStatement<'a>) -> Option<ArenaVec<'a, Statement<'a>>> {
        if f.update.is_some() || !f.test.as_ref().is_none_or(truthy) {
            return None;
        }
        let state = {
            let Statement::BlockStatement(block) = &f.body else { return None };
            let Statement::SwitchStatement(switch) = block.body.iter().find(|s| is_switch(s))? else {
                return None;
            };
            let Expression::Identifier(state) = &switch.discriminant else { return None };
            state.name.clone()
        };
        let start = start(f.init.as_ref()?, &state)? as u64;

        let Statement::BlockStatement(block) = &mut f.body else { return None };
        let at = block.body.iter().position(is_switch)?;
        if !block.body[at + 1..]
            .iter()
            .all(|s| matches!(s, Statement::BreakStatement(b) if b.label.is_none()))
        {
            return None;
        }
        let (prologue, rest) = block.body.split_at_mut(at);
        let Statement::SwitchStatement(switch) = &mut rest[0] else { return None };

        let mut blocks: Map<u64, usize> = Map::default();
        let mut pending = Vec::new();
        for (i, case) in switch.cases.iter().enumerate() {
            let Some(Expression::NumericLiteral(label)) = &case.test else { return None };
            pending.push(label.value as u64);
            if case.consequent.is_empty() {
                continue;
            }
            for label in pending.drain(..) {
                blocks.entry(label).or_insert(i);
            }
        }
        for label in pending {
            blocks.entry(label).or_insert(switch.cases.len());
        }

        let mut order = Vec::new();
        let mut seen = Set::default();
        let mut at = start;
        loop {
            let Some(&i) = blocks.get(&at) else { return None };
            if !seen.insert(at) {
                return None;
            }
            if i == switch.cases.len() {
                break;
            }
            let next = successor(&switch.cases[i].consequent, &state)?;
            order.push((i, next.is_some()));
            match next {
                Some(n) => at = n,
                None => break,
            }
        }

        let mut out = ArenaVec::new_in(&self.ast);
        out.extend(carried(&mut f.init, &state, &self.ast));
        for s in prologue {
            out.push(s.take_in(&self.ast));
        }
        for (i, chained) in order {
            let body = &mut switch.cases[i].consequent;
            let written = out.len();
            let (at, exits) = terminator(body);
            let keep = if exits { at } else { (at + 1).min(body.len()) };
            for s in body.iter_mut().take(keep) {
                out.push(s.take_in(&self.ast));
            }
            if chained {
                for i in written..out.len() {
                    strip(&mut out[i], &state, &self.ast);
                }
            }
        }
        Some(out)
    }
}

fn carried<'a>(
    init: &mut Option<ForStatementInit<'a>>,
    state: &str,
    ast: &AstBuilder<'a>,
) -> Option<Statement<'a>> {
    let Some(ForStatementInit::VariableDeclaration(d)) = init else { return None };
    d.declarations
        .retain(|v| !matches!(&v.id, BindingPattern::BindingIdentifier(b) if b.name == state));
    if d.declarations.is_empty() {
        return None;
    }
    let (kind, span) = (d.kind, d.span);
    let declarations = d.declarations.take_in(ast);
    Some(Statement::new_variable_declaration(span, kind, declarations, false, ast))
}

fn is_switch(s: &Statement) -> bool {
    matches!(s, Statement::SwitchStatement(_))
}

fn start(init: &ForStatementInit, state: &str) -> Option<f64> {
    match init {
        ForStatementInit::VariableDeclaration(d) => {
            d.declarations.iter().find_map(|d| match (&d.id, &d.init) {
                (BindingPattern::BindingIdentifier(b), Some(Expression::NumericLiteral(n)))
                    if b.name == state =>
                {
                    Some(n.value)
                }
                _ => None,
            })
        }
        _ => assigned(init.as_expression()?, state),
    }
}

fn terminator(body: &ArenaVec<Statement>) -> (usize, bool) {
    for (i, s) in body.iter().enumerate() {
        match s {
            Statement::BreakStatement(b) if b.label.is_none() => return (i, true),
            Statement::ContinueStatement(c) if c.label.is_none() => return (i, true),
            Statement::ReturnStatement(_) | Statement::ThrowStatement(_) => return (i, false),
            _ => {}
        }
    }
    (body.len(), true)
}

fn successor(body: &ArenaVec<Statement>, state: &str) -> Option<Option<u64>> {
    let (at, exits) = terminator(body);
    if !exits {
        return Some(None);
    }
    match body.get(at) {
        Some(Statement::BreakStatement(_)) | None => Some(None),
        Some(Statement::ContinueStatement(_)) => {
            Some(Some(target(&body[..at], state)? as u64))
        }
        _ => None,
    }
}

fn target(stmts: &[Statement], state: &str) -> Option<f64> {
    for s in stmts.iter().rev() {
        let found = match s {
            Statement::ExpressionStatement(e) => assigned(&e.expression, state),
            Statement::BlockStatement(b) => target(&b.body, state),
            Statement::VariableDeclaration(d) => d.declarations.iter().rev().find_map(|v| {
                let (BindingPattern::BindingIdentifier(id), Some(Expression::NumericLiteral(n))) =
                    (&v.id, &v.init)
                else {
                    return None;
                };
                (id.name == state).then_some(n.value)
            }),
            _ => None,
        };
        if found.is_some() {
            return found;
        }
    }
    None
}

fn assigned(e: &Expression, state: &str) -> Option<f64> {
    match e {
        Expression::SequenceExpression(s) => {
            s.expressions.iter().rev().find_map(|e| assigned(e, state))
        }
        Expression::AssignmentExpression(a) => {
            let AssignmentTarget::AssignmentTargetIdentifier(t) = &a.left else { return None };
            let Expression::NumericLiteral(n) = &a.right else { return None };
            (t.name == state).then_some(n.value)
        }
        _ => None,
    }
}

fn strip<'a>(s: &mut Statement<'a>, state: &str, ast: &AstBuilder<'a>) {
    match s {
        Statement::BlockStatement(b) => {
            for inner in b.body.iter_mut() {
                strip(inner, state, ast);
            }
            b.body.retain(|inner| !matches!(inner, Statement::EmptyStatement(_)));
        }
        Statement::VariableDeclaration(d) => {
            d.declarations.retain(|v| {
                !matches!((&v.id, &v.init),
                    (BindingPattern::BindingIdentifier(id), Some(Expression::NumericLiteral(_)))
                        if id.name == state)
            });
            if d.declarations.is_empty() {
                *s = Statement::new_empty_statement(d.span, ast);
            }
        }
        Statement::ExpressionStatement(e) => {
            let span = e.span;
            match &mut e.expression {
                Expression::AssignmentExpression(a)
                    if matches!(&a.left, AssignmentTarget::AssignmentTargetIdentifier(t) if t.name == state)
                        && matches!(&a.right, Expression::NumericLiteral(_)) =>
                {
                    *s = Statement::new_empty_statement(span, ast);
                }
                Expression::SequenceExpression(seq) => {
                    seq.expressions.retain(|x| {
                        !matches!(x, Expression::AssignmentExpression(a)
                            if matches!(&a.left, AssignmentTarget::AssignmentTargetIdentifier(t) if t.name == state)
                                && matches!(&a.right, Expression::NumericLiteral(_)))
                    });
                    match seq.expressions.len() {
                        0 => *s = Statement::new_empty_statement(span, ast),
                        1 => {
                            let only = seq.expressions.pop().unwrap();
                            *s = Statement::new_expression_statement(span, only, ast);
                        }
                        _ => {}
                    }
                }
                _ => {}
            }
        }
        _ => {}
    }
}

fn truthy(e: &Expression) -> bool {
    matches!(e, Expression::BooleanLiteral(b) if b.value)
}
