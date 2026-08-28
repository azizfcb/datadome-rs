use oxc_allocator::{Allocator, TakeIn, Vec as ArenaVec};
use oxc_ast::ast::*;
use oxc_ast::builder::AstBuilder;
use oxc_ast_visit::{VisitMut, walk_mut};
use oxc_syntax::operator::UnaryOperator;
use oxc_span::{GetSpan, SPAN};

pub fn run<'a>(alloc: &'a Allocator, program: &mut Program<'a>) {
    Simplify { ast: AstBuilder::new(alloc) }.visit_program(program);
}

struct Simplify<'a> {
    ast: AstBuilder<'a>,
}

impl<'a> VisitMut<'a> for Simplify<'a> {
    fn visit_expression(&mut self, e: &mut Expression<'a>) {
        walk_mut::walk_expression(self, e);
        match e {
            Expression::ConditionalExpression(c) => self.condition(&mut c.test),
            Expression::LogicalExpression(l) => self.condition(&mut l.left),
            Expression::UnaryExpression(u) if u.operator == UnaryOperator::LogicalNot => {
                self.condition(&mut u.argument)
            }
            _ => {}
        }
        if let Expression::ConditionalExpression(c) = e
            && let Expression::SequenceExpression(q) = &mut c.test
            && let Some(decided) = q.expressions.last().and_then(truthy)
        {
            let mut parts = ArenaVec::new_in(&self.ast);
            let last = q.expressions.len() - 1;
            for x in q.expressions.iter_mut().take(last) {
                parts.push(x.take_in(&self.ast));
            }
            let branch = if decided { &mut c.consequent } else { &mut c.alternate };
            parts.push(branch.take_in(&self.ast));
            let span = c.span;
            *e = Expression::new_sequence_expression(span, parts, &self.ast);
            return;
        }
        let taken = match e {
            Expression::ConditionalExpression(c) => match truthy(&c.test) {
                Some(true) => &mut c.consequent,
                Some(false) => &mut c.alternate,
                None => return,
            },
            Expression::LogicalExpression(l) => match (l.operator, truthy(&l.left)) {
                (LogicalOperator::And, Some(true)) | (LogicalOperator::Or, Some(false)) => {
                    &mut l.right
                }
                (LogicalOperator::And, Some(false)) | (LogicalOperator::Or, Some(true)) => {
                    &mut l.left
                }
                _ => return,
            },
            _ => return,
        };
        *e = taken.take_in(&self.ast);
    }

    fn visit_statements(&mut self, stmts: &mut ArenaVec<'a, Statement<'a>>) {
        walk_mut::walk_statements(self, stmts);
        stmts.retain(|s| !dead(s));
    }

    fn visit_statement(&mut self, s: &mut Statement<'a>) {
        walk_mut::walk_statement(self, s);
        match s {
            Statement::IfStatement(i) => {
                self.condition(&mut i.test);
                if i.alternate.as_ref().is_some_and(dead) {
                    i.alternate = None;
                }
                let taken = match truthy(&i.test) {
                    Some(true) => Some(&mut i.consequent),
                    Some(false) => i.alternate.as_mut(),
                    None => {
                        if dead(&i.consequent) && i.alternate.is_some() {
                            let mut alternate = i.alternate.take().unwrap();
                            i.consequent = alternate.take_in(&self.ast);
                            i.test = negate(&mut i.test, &self.ast);
                        }
                        return;
                    }
                };
                *s = match taken {
                    Some(t) => t.take_in(&self.ast),
                    None => Statement::new_empty_statement(SPAN, &self.ast),
                };
            }
            Statement::SwitchStatement(sw) => {
                let Some(chosen) = select(sw) else { return };
                let mut body = ArenaVec::new_in(&self.ast);
                for case in sw.cases.iter_mut().skip(chosen) {
                    let mut stop = false;
                    for inner in case.consequent.iter_mut() {
                        match inner {
                            Statement::BreakStatement(b) if b.label.is_none() => {
                                stop = true;
                                break;
                            }
                            Statement::ReturnStatement(_) | Statement::ThrowStatement(_) => {
                                body.push(inner.take_in(&self.ast));
                                stop = true;
                                break;
                            }
                            _ => body.push(inner.take_in(&self.ast)),
                        }
                    }
                    if stop {
                        break;
                    }
                }
                *s = Statement::new_block_statement(sw.span, body, &self.ast);
            }
            Statement::ForStatement(f)
                if f.update.is_none()
                    && f.test.as_ref().is_some_and(|t| truthy(t) == Some(true))
                    && matches!(&f.init, None | Some(ForStatementInit::VariableDeclaration(_))) =>
            {
                let Statement::BlockStatement(b) = &f.body else { return };
                if repeats(&b.body) {
                    return;
                }
                let mut body = ArenaVec::new_in(&self.ast);
                if let Some(ForStatementInit::VariableDeclaration(d)) = &mut f.init {
                    let (kind, span) = (d.kind, d.span);
                    let declarations = d.declarations.take_in(&self.ast);
                    body.push(Statement::new_variable_declaration(
                        span,
                        kind,
                        declarations,
                        false,
                        &self.ast,
                    ));
                }
                let Statement::BlockStatement(b) = &mut f.body else { return };
                for inner in b.body.iter_mut() {
                    if matches!(inner, Statement::BreakStatement(x) if x.label.is_none()) {
                        break;
                    }
                    body.push(inner.take_in(&self.ast));
                }
                *s = Statement::new_block_statement(f.span, body, &self.ast);
            }
            Statement::WhileStatement(w) => self.condition(&mut w.test),
            Statement::DoWhileStatement(w) => self.condition(&mut w.test),
            Statement::ForStatement(f) => {
                if let Some(test) = f.test.as_mut() {
                    self.condition(test);
                }
            }
            _ => {}
        }
    }
}

impl<'a> Simplify<'a> {
    fn condition(&self, e: &mut Expression<'a>) {
        loop {
            let taken = match e {
                Expression::LogicalExpression(l) => match (l.operator, truthy(&l.right)) {
                    (LogicalOperator::Or, Some(false)) | (LogicalOperator::And, Some(true)) => {
                        &mut l.left
                    }
                    _ => return,
                },
                Expression::ConditionalExpression(c)
                    if truthy(&c.consequent) == Some(true)
                        && truthy(&c.alternate) == Some(false) =>
                {
                    &mut c.test
                }
                Expression::UnaryExpression(u) if u.operator == UnaryOperator::LogicalNot => {
                    self.condition(&mut u.argument);
                    return;
                }
                _ => return,
            };
            *e = taken.take_in(&self.ast);
        }
    }
}

fn select(sw: &SwitchStatement) -> Option<usize> {
    let value = literal(&sw.discriminant)?;
    let matched = sw.cases.iter().position(|c| {
        c.test.as_ref().and_then(literal).is_some_and(|t| same(&t, &value))
    });
    matched.or_else(|| sw.cases.iter().position(|c| c.test.is_none()))
}

enum Lit {
    Num(f64),
    Str(String),
    Bool(bool),
    Null,
}

fn literal(e: &Expression) -> Option<Lit> {
    Some(match e {
        Expression::NumericLiteral(n) => Lit::Num(n.value),
        Expression::StringLiteral(s) => Lit::Str(s.value.to_string()),
        Expression::BooleanLiteral(b) => Lit::Bool(b.value),
        Expression::NullLiteral(_) => Lit::Null,
        _ => return None,
    })
}

fn same(a: &Lit, b: &Lit) -> bool {
    match (a, b) {
        (Lit::Num(x), Lit::Num(y)) => x == y,
        (Lit::Str(x), Lit::Str(y)) => x == y,
        (Lit::Bool(x), Lit::Bool(y)) => x == y,
        (Lit::Null, Lit::Null) => true,
        _ => false,
    }
}

fn repeats(stmts: &[Statement]) -> bool {
    stmts.iter().any(|s| match s {
        Statement::ContinueStatement(c) => c.label.is_none(),
        Statement::BlockStatement(b) => repeats(&b.body),
        Statement::IfStatement(i) => {
            repeats(std::slice::from_ref(&i.consequent))
                || i.alternate.as_ref().is_some_and(|a| repeats(std::slice::from_ref(a)))
        }
        Statement::SwitchStatement(sw) => sw.cases.iter().any(|c| repeats(&c.consequent)),
        Statement::TryStatement(t) => {
            repeats(&t.block.body)
                || t.handler.as_ref().is_some_and(|h| repeats(&h.body.body))
                || t.finalizer.as_ref().is_some_and(|f| repeats(&f.body))
        }
        _ => false,
    })
}

fn negate<'a>(e: &mut Expression<'a>, ast: &AstBuilder<'a>) -> Expression<'a> {
    if let Expression::UnaryExpression(u) = e
        && u.operator == UnaryOperator::LogicalNot
    {
        return u.argument.take_in(ast);
    }
    let span = e.span();
    Expression::new_unary_expression(span, UnaryOperator::LogicalNot, e.take_in(ast), ast)
}

fn dead(s: &Statement) -> bool {
    match s {
        Statement::EmptyStatement(_) => true,
        Statement::ExpressionStatement(e) => pure(&e.expression),
        Statement::BlockStatement(b) => b.body.is_empty(),
        _ => false,
    }
}

fn pure(e: &Expression) -> bool {
    match e {
        Expression::NumericLiteral(_)
        | Expression::StringLiteral(_)
        | Expression::BooleanLiteral(_)
        | Expression::NullLiteral(_)
        | Expression::Identifier(_) => true,
        Expression::SequenceExpression(s) => s.expressions.iter().all(pure),
        _ => false,
    }
}

fn truthy(e: &Expression) -> Option<bool> {
    match e {
        Expression::BooleanLiteral(b) => Some(b.value),
        Expression::NumericLiteral(n) => Some(n.value != 0.0),
        Expression::StringLiteral(s) => Some(!s.value.is_empty()),
        Expression::NullLiteral(_) => Some(false),
        _ => None,
    }
}
