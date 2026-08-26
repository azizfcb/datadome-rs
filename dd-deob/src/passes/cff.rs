use oxc_allocator::{Allocator, Vec as AVec};
use oxc_ast::AstBuilder;
use oxc_ast::ast::*;
use oxc_ast_visit::VisitMut;
use oxc_span::SPAN;
use oxc_syntax::operator::AssignmentOperator;
use rustc_hash::FxHashMap as HashMap;

pub fn run<'a>(program: &mut Program<'a>, alloc: &'a Allocator) -> usize {
    let mut total = 0usize;
    let mut v = Unflatten { alloc, count: 0 };
    v.visit_program(program);
    total += v.count;
    total
}

struct Unflatten<'a> {
    alloc: &'a Allocator,
    count: usize,
}

impl<'a> VisitMut<'a> for Unflatten<'a> {
    fn visit_statements(&mut self, stmts: &mut AVec<'a, Statement<'a>>) {
        for s in stmts.iter_mut() {
            oxc_ast_visit::walk_mut::walk_statement(self, s);
        }

        let mut out: Vec<Statement<'a>> = Vec::with_capacity(stmts.len());
        let drained: Vec<Statement<'a>> = stmts.drain(..).collect();
        let mut idx = 0;
        while idx < drained.len() {
            let stmt = &drained[idx];
            if let Statement::ForStatement(fs) = stmt {
                if let Some((unflat, used_init)) = try_unflatten_for(fs, &out, self.alloc) {
                    if used_init {
                        out.pop();
                    }
                    out.extend(unflat);
                    self.count += 1;
                    idx += 1;
                    continue;
                }
            }
            out.push(drained[idx].clone_in(self.alloc));
            idx += 1;
        }
        for s in out { stmts.push(s); }
    }

    fn visit_statement(&mut self, stmt: &mut Statement<'a>) {
        oxc_ast_visit::walk_mut::walk_statement(self, stmt);
        let take_for: Option<&ForStatement<'a>> = if let Statement::ForStatement(fs) = stmt {
            Some(unsafe { &*(&**fs as *const ForStatement<'a>) })
        } else { None };
        if let Some(fs) = take_for {
            if let Some((unflat, _)) = try_unflatten_for(fs, &[], self.alloc) {
                let ast = AstBuilder::new(self.alloc);
                let mut block_body = AVec::with_capacity_in(unflat.len(), self.alloc);
                for s in unflat { block_body.push(s); }
                *stmt = Statement::BlockStatement(ast.alloc_block_statement(SPAN, block_body));
                self.count += 1;
            }
        }
    }
}

trait CloneIn<'a> {
    fn clone_in(&self, alloc: &'a Allocator) -> Self;
}

impl<'a> CloneIn<'a> for Statement<'a> {
    fn clone_in(&self, _alloc: &'a Allocator) -> Statement<'a> {
        let raw_ptr = self as *const Statement<'a>;
        unsafe { std::ptr::read(raw_ptr) }
    }
}

fn try_unflatten_for<'a>(
    fs: &ForStatement<'a>,
    above: &[Statement<'a>],
    alloc: &'a Allocator,
) -> Option<(Vec<Statement<'a>>, bool)> {
    if !is_loop_test_truthy(fs.test.as_ref()) { return None; }

    let (switch_stmt, before_in_block) = locate_switch(&fs.body)?;

    let Expression::Identifier(disc_id) = &switch_stmt.discriminant else { return None };
    let state_var = disc_id.name.as_str().to_string();

    let (initial, used_init_above) = get_initial_state(fs, &state_var, above)?;

    let cases = &switch_stmt.cases;
    let mut index_by_value: HashMap<i64, usize> = HashMap::default();
    for (i, c) in cases.iter().enumerate() {
        if let Some(t) = &c.test {
            if let Expression::NumericLiteral(n) = t {
                index_by_value.entry(n.value as i64).or_insert(i);
            }
        }
    }

    let mut visited: rustc_hash::FxHashSet<i64> = Default::default();
    let mut executed: Vec<Statement<'a>> = Vec::new();
    let mut state = initial;
    loop {
        if !index_by_value.contains_key(&state) || visited.contains(&state) { break; }
        visited.insert(state);
        let start_idx = index_by_value[&state];
        let body = coalesce_body(cases, start_idx);
        let (clean, last_assign) = clean_body(body, &state_var, alloc);
        let body_refs_state = clean.iter().any(|s| stmt_refs_ident(s, &state_var));
        executed.extend(clean);
        match last_assign {
            Some(s) => state = s,
            None => {
                if body_refs_state { return None; }
                break;
            }
        }
    }

    if executed.is_empty() { return None; }

    let mut output: Vec<Statement<'a>> = Vec::new();

    if let Some(init) = &fs.init {
        if let ForStatementInit::VariableDeclaration(vd) = init {
            let kind = vd.kind;
            let mut keep: Vec<VariableDeclarator<'a>> = Vec::new();
            for d in &vd.declarations {
                if let BindingPattern::BindingIdentifier(id) = &d.id {
                    if id.name.as_str() == state_var { continue; }
                }
                keep.push(unsafe { std::ptr::read(d as *const VariableDeclarator<'a>) });
            }
            if !keep.is_empty() {
                let ast = AstBuilder::new(alloc);
                let mut decls = AVec::with_capacity_in(keep.len(), alloc);
                for d in keep {
                    if let Some(init) = &d.init {
                        if let Expression::AssignmentExpression(asn) = init {
                            if let AssignmentTarget::AssignmentTargetIdentifier(lhs) = &asn.left {
                                if lhs.name.as_str() == state_var {
                                    let new_init = unsafe { std::ptr::read(&asn.right as *const Expression<'a>) };
                                    let id_pat = unsafe { std::ptr::read(&d.id as *const BindingPattern<'a>) };
                                    decls.push(ast.variable_declarator(SPAN, kind, id_pat, Option::<oxc_allocator::Box<TSTypeAnnotation>>::None, Some(new_init), false));
                                    continue;
                                }
                            }
                        }
                    }
                    decls.push(d);
                }
                let new_vd = ast.alloc_variable_declaration(SPAN, kind, decls, false);
                output.push(Statement::VariableDeclaration(new_vd));
            }
        }
    }

    for s in before_in_block {
        output.push(unsafe { std::ptr::read(s as *const Statement<'a>) });
    }
    for s in executed { output.push(s); }

    Some((output, used_init_above))
}

fn is_loop_test_truthy(test: Option<&Expression>) -> bool {
    match test {
        None => true,
        Some(Expression::BooleanLiteral(b)) => b.value,
        Some(Expression::NumericLiteral(n)) => n.value != 0.0,
        Some(_) => true,
    }
}

fn locate_switch<'a, 'b>(body: &'b Statement<'a>) -> Option<(&'b SwitchStatement<'a>, Vec<&'b Statement<'a>>)> {
    match body {
        Statement::SwitchStatement(s) => Some((s, Vec::new())),
        Statement::BlockStatement(b) => {
            let mut before: Vec<&'b Statement<'a>> = Vec::new();
            for s in &b.body {
                if let Statement::SwitchStatement(sw) = s {
                    return Some((sw, before));
                }
                before.push(s);
            }
            None
        }
        _ => None,
    }
}

fn coalesce_body<'a, 'b>(cases: &'b AVec<'a, SwitchCase<'a>>, start: usize) -> &'b AVec<'a, Statement<'a>> {
    let mut j = start;
    while j < cases.len() && cases[j].consequent.is_empty() { j += 1; }
    if j < cases.len() { &cases[j].consequent } else { &cases[start].consequent }
}

fn clean_body<'a>(stmts: &AVec<'a, Statement<'a>>, state_var: &str, alloc: &'a Allocator) -> (Vec<Statement<'a>>, Option<i64>) {
    let mut out: Vec<Statement<'a>> = Vec::new();
    let mut last_assign: Option<i64> = None;
    let ast = AstBuilder::new(alloc);

    for s in stmts {
        if matches!(s, Statement::BreakStatement(_) | Statement::ContinueStatement(_)) { continue; }
        if matches!(s, Statement::ReturnStatement(_) | Statement::ThrowStatement(_)) {
            out.push(unsafe { std::ptr::read(s as *const Statement<'a>) });
            continue;
        }
        if let Statement::ExpressionStatement(es) = s {
            let exprs = explode_sequence(&es.expression);
            let mut kept: Vec<Expression<'a>> = Vec::new();
            for e in exprs {
                if let Expression::AssignmentExpression(asn) = &e {
                    if asn.operator == AssignmentOperator::Assign {
                        if let AssignmentTarget::AssignmentTargetIdentifier(id) = &asn.left {
                            if id.name.as_str() == state_var {
                                if let Expression::NumericLiteral(n) = &asn.right {
                                    last_assign = Some(n.value as i64);
                                    continue;
                                }
                            }
                        }
                    }
                }
                kept.push(e);
            }
            for e in kept {
                let st = ast.statement_expression(SPAN, e);
                out.push(st);
            }
            continue;
        }
        out.push(unsafe { std::ptr::read(s as *const Statement<'a>) });
    }
    (out, last_assign)
}

fn stmt_refs_ident(stmt: &Statement, name: &str) -> bool {
    match stmt {
        Statement::ExpressionStatement(es) => expr_refs_ident(&es.expression, name),
        Statement::IfStatement(s) => expr_refs_ident(&s.test, name)
            || stmt_refs_ident(&s.consequent, name)
            || s.alternate.as_ref().map_or(false, |a| stmt_refs_ident(a, name)),
        Statement::BlockStatement(b) => b.body.iter().any(|s| stmt_refs_ident(s, name)),
        Statement::ReturnStatement(r) => r.argument.as_ref().map_or(false, |e| expr_refs_ident(e, name)),
        Statement::ThrowStatement(t) => expr_refs_ident(&t.argument, name),
        _ => false,
    }
}

fn expr_refs_ident(expr: &Expression, name: &str) -> bool {
    match expr {
        Expression::Identifier(id) => id.name.as_str() == name,
        Expression::AssignmentExpression(a) => {
            (matches!(&a.left, AssignmentTarget::AssignmentTargetIdentifier(id) if id.name.as_str() == name))
                || expr_refs_ident(&a.right, name)
        }
        Expression::BinaryExpression(b) => expr_refs_ident(&b.left, name) || expr_refs_ident(&b.right, name),
        Expression::LogicalExpression(l) => expr_refs_ident(&l.left, name) || expr_refs_ident(&l.right, name),
        Expression::ConditionalExpression(c) => expr_refs_ident(&c.test, name) || expr_refs_ident(&c.consequent, name) || expr_refs_ident(&c.alternate, name),
        Expression::UnaryExpression(u) => expr_refs_ident(&u.argument, name),
        Expression::UpdateExpression(u) => match &u.argument {
            SimpleAssignmentTarget::AssignmentTargetIdentifier(id) => id.name.as_str() == name,
            _ => false,
        },
        Expression::SequenceExpression(s) => s.expressions.iter().any(|e| expr_refs_ident(e, name)),
        Expression::CallExpression(c) => expr_refs_ident(&c.callee, name)
            || c.arguments.iter().any(|a| match a {
                Argument::SpreadElement(s) => expr_refs_ident(&s.argument, name),
                other => expr_refs_ident(other.to_expression(), name),
            }),
        Expression::ParenthesizedExpression(p) => expr_refs_ident(&p.expression, name),
        Expression::ComputedMemberExpression(m) => expr_refs_ident(&m.object, name) || expr_refs_ident(&m.expression, name),
        Expression::StaticMemberExpression(m) => expr_refs_ident(&m.object, name),
        _ => false,
    }
}

fn explode_sequence<'a>(expr: &Expression<'a>) -> Vec<Expression<'a>> {
    if let Expression::SequenceExpression(seq) = expr {
        let mut out: Vec<Expression<'a>> = Vec::new();
        for e in &seq.expressions {
            out.extend(explode_sequence(e));
        }
        return out;
    }
    vec![unsafe { std::ptr::read(expr as *const Expression<'a>) }]
}

fn get_initial_state<'a>(fs: &ForStatement<'a>, state_var: &str, above: &[Statement<'a>]) -> Option<(i64, bool)> {
    if let Some(init) = &fs.init {
        match init {
            ForStatementInit::VariableDeclaration(vd) => {
                for d in &vd.declarations {
                    if let BindingPattern::BindingIdentifier(id) = &d.id {
                        if id.name.as_str() == state_var {
                            if let Some(Expression::NumericLiteral(n)) = &d.init {
                                return Some((n.value as i64, false));
                            }
                        }
                    }
                    if let Some(Expression::AssignmentExpression(asn)) = &d.init {
                        if let AssignmentTarget::AssignmentTargetIdentifier(lhs) = &asn.left {
                            if lhs.name.as_str() == state_var {
                                if let Expression::NumericLiteral(n) = &asn.right {
                                    return Some((n.value as i64, false));
                                }
                            }
                        }
                    }
                }
                return look_above(above, state_var);
            }
            ForStatementInit::AssignmentExpression(asn) => {
                if let AssignmentTarget::AssignmentTargetIdentifier(id) = &asn.left {
                    if id.name.as_str() == state_var {
                        if let Expression::NumericLiteral(n) = &asn.right {
                            return Some((n.value as i64, false));
                        }
                    }
                }
                return None;
            }
            _ => {
                if let ForStatementInit::SequenceExpression(seq) = init {
                    for e in &seq.expressions {
                        if let Expression::AssignmentExpression(asn) = e {
                            if let AssignmentTarget::AssignmentTargetIdentifier(id) = &asn.left {
                                if id.name.as_str() == state_var {
                                    if let Expression::NumericLiteral(n) = &asn.right {
                                        return Some((n.value as i64, false));
                                    }
                                }
                            }
                        }
                    }
                }
                return None;
            }
        }
    }
    look_above(above, state_var)
}

fn look_above<'a>(above: &[Statement<'a>], state_var: &str) -> Option<(i64, bool)> {
    let n = above.len();
    let lo = if n >= 3 { n - 3 } else { 0 };
    for i in (lo..n).rev() {
        match &above[i] {
            Statement::ExpressionStatement(es) => {
                if let Expression::AssignmentExpression(asn) = &es.expression {
                    if let AssignmentTarget::AssignmentTargetIdentifier(id) = &asn.left {
                        if id.name.as_str() == state_var {
                            if let Expression::NumericLiteral(n) = &asn.right {
                                return Some((n.value as i64, true));
                            }
                        }
                    }
                }
            }
            Statement::VariableDeclaration(vd) => {
                for d in &vd.declarations {
                    if let BindingPattern::BindingIdentifier(id) = &d.id {
                        if id.name.as_str() == state_var {
                            if let Some(Expression::NumericLiteral(n)) = &d.init {
                                return Some((n.value as i64, false));
                            }
                            if let Some(Expression::AssignmentExpression(asn)) = &d.init {
                                if let AssignmentTarget::AssignmentTargetIdentifier(lhs) = &asn.left {
                                    if lhs.name.as_str() == state_var {
                                        if let Expression::NumericLiteral(n) = &asn.right {
                                            return Some((n.value as i64, false));
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
            _ => {}
        }
    }
    None
}
