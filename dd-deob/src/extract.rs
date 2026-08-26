use oxc_ast::ast::*;
use oxc_codegen::Codegen;

#[derive(Default, Clone)]
pub struct ExtractOutput {
    pub dynamic_challenge: Option<String>,
    pub wasm_b64: Option<String>,
    pub wasm_fields: Vec<String>,
}

pub fn run(program: &Program) -> ExtractOutput {
    let mut out = ExtractOutput::default();
    out.dynamic_challenge = find_dynamic_challenge(program);
    out.wasm_b64 = find_wasm_b64(program);
    out.wasm_fields = find_wasm_fields(program);
    out
}

fn find_dynamic_challenge(program: &Program) -> Option<String> {
    let mut hit: Option<String> = None;
    walk_calls(program, &mut |call: &CallExpression| {
        if hit.is_some() { return; }
        if call.arguments.len() != 2 { return; }
        let Argument::StringLiteral(name) = &call.arguments[0] else { return };
        if name.value.as_str() != "dynamicChallenge" { return; }
        let Argument::SpreadElement(_) = &call.arguments[1] else {
            let expr = call.arguments[1].to_expression();
            hit = { let mut cg = Codegen::default(); cg.print_expression(expr); Some(cg.into_source_text()) };
            return;
        };
    });
    if hit.is_some() { return hit; }

    let mut shape_hit: Option<String> = None;
    walk_calls(program, &mut |call: &CallExpression| {
        if shape_hit.is_some() { return; }
        if call.arguments.len() != 2 { return; }
        if matches!(&call.arguments[1], Argument::SpreadElement(_)) { return; }
        let expr = call.arguments[1].to_expression();
        if !looks_like_dynamic_challenge(expr) { return; }
        shape_hit = { let mut cg = Codegen::default(); cg.print_expression(expr); Some(cg.into_source_text()) };
    });
    shape_hit
}

fn looks_like_dynamic_challenge(e: &Expression) -> bool {
    let Expression::BinaryExpression(_) = e else { return false };
    let mut indices = rustc_hash::FxHashSet::default();
    let mut bin_count = 0usize;
    let mut has_large_xor = false;
    walk_expr(e, &mut |inner: &Expression| {
        match inner {
            Expression::ComputedMemberExpression(m) => {
                if let (Expression::Identifier(_), Expression::NumericLiteral(n)) = (&m.object, &m.expression) {
                    indices.insert(n.value as i64);
                }
            }
            Expression::BinaryExpression(b) => {
                bin_count += 1;
                if matches!(b.operator, oxc_syntax::operator::BinaryOperator::BitwiseXOR) {
                    if let Expression::NumericLiteral(n) = &b.right {
                        if n.value > 1_000_000.0 { has_large_xor = true; }
                    }
                }
            }
            _ => {}
        }
    });
    indices.len() >= 3 && has_large_xor && bin_count >= 30
}

fn find_wasm_b64(program: &Program) -> Option<String> {
    let mut hit: Option<String> = None;
    let mut total = 0usize;
    let mut max_len = 0usize;
    let mut visit_str = |s: &StringLiteral| {
        total += 1;
        let v = s.value.as_str();
        if v.len() > max_len { max_len = v.len(); }
        if hit.is_some() { return; }
        if v.len() >= 200 && v.starts_with("AGFzbQ") {
            hit = Some(v.to_string());
        }
    };
    walk_strings(program, &mut visit_str);
    if std::env::var_os("DD_DUMP_WASM").is_some() {
        eprintln!("find_wasm_b64: scanned {} strings, max_len={}, hit={}", total, max_len, hit.is_some());
    }
    hit
}

fn find_wasm_fields(program: &Program) -> Vec<String> {
    let out: Vec<String> = Vec::new();
    walk_calls(program, &mut |call: &CallExpression| {
        if !out.is_empty() { return; }
        if !is_wasm_b_callee(&call.callee) { return; }
        if call.arguments.is_empty() { return; }
        let Argument::Identifier(_) = &call.arguments[0] else { return };
    });
    out
}

fn is_wasm_b_callee(e: &Expression) -> bool {
    let Expression::StaticMemberExpression(m) = e else { return false };
    if m.property.name.as_str() != "wasm_b" { return false; }
    let Expression::StaticMemberExpression(inner) = &m.object else { return false };
    inner.property.name.as_str() == "exports"
}

fn walk_calls<'a, F: FnMut(&CallExpression<'a>)>(program: &'a Program<'a>, f: &mut F) {
    for s in &program.body { walk_calls_in_stmt(s, f); }
}

fn walk_calls_in_stmt<'a, F: FnMut(&CallExpression<'a>)>(stmt: &'a Statement<'a>, f: &mut F) {
    match stmt {
        Statement::ExpressionStatement(es) => walk_calls_in_expr(&es.expression, f),
        Statement::VariableDeclaration(vd) => {
            for d in &vd.declarations {
                if let Some(init) = &d.init { walk_calls_in_expr(init, f); }
            }
        }
        Statement::IfStatement(s) => {
            walk_calls_in_expr(&s.test, f);
            walk_calls_in_stmt(&s.consequent, f);
            if let Some(a) = &s.alternate { walk_calls_in_stmt(a, f); }
        }
        Statement::BlockStatement(b) => for s in &b.body { walk_calls_in_stmt(s, f); }
        Statement::ReturnStatement(r) => if let Some(a) = &r.argument { walk_calls_in_expr(a, f); }
        Statement::ForStatement(fs) => walk_calls_in_stmt(&fs.body, f),
        Statement::WhileStatement(w) => walk_calls_in_stmt(&w.body, f),
        Statement::DoWhileStatement(w) => walk_calls_in_stmt(&w.body, f),
        Statement::SwitchStatement(s) => {
            for c in &s.cases { for st in &c.consequent { walk_calls_in_stmt(st, f); } }
        }
        Statement::TryStatement(t) => {
            for st in &t.block.body { walk_calls_in_stmt(st, f); }
            if let Some(h) = &t.handler { for st in &h.body.body { walk_calls_in_stmt(st, f); } }
            if let Some(fi) = &t.finalizer { for st in &fi.body { walk_calls_in_stmt(st, f); } }
        }
        Statement::FunctionDeclaration(fd) => {
            if let Some(b) = &fd.body { for s in &b.statements { walk_calls_in_stmt(s, f); } }
        }
        Statement::LabeledStatement(s) => walk_calls_in_stmt(&s.body, f),
        Statement::ThrowStatement(t) => walk_calls_in_expr(&t.argument, f),
        _ => {}
    }
}

fn walk_calls_in_expr<'a, F: FnMut(&CallExpression<'a>)>(e: &'a Expression<'a>, f: &mut F) {
    if let Expression::CallExpression(c) = e { f(c); }
    walk_expr(e, &mut |inner: &Expression<'a>| {
        if let Expression::CallExpression(c) = inner { f(c); }
    });
}

fn walk_expr<'a, F: FnMut(&Expression<'a>)>(e: &'a Expression<'a>, f: &mut F) {
    f(e);
    match e {
        Expression::BinaryExpression(b) => { walk_expr(&b.left, f); walk_expr(&b.right, f); }
        Expression::LogicalExpression(l) => { walk_expr(&l.left, f); walk_expr(&l.right, f); }
        Expression::UnaryExpression(u) => walk_expr(&u.argument, f),
        Expression::ConditionalExpression(c) => { walk_expr(&c.test, f); walk_expr(&c.consequent, f); walk_expr(&c.alternate, f); }
        Expression::SequenceExpression(s) => for e in &s.expressions { walk_expr(e, f); }
        Expression::CallExpression(c) => {
            walk_expr(&c.callee, f);
            for a in &c.arguments {
                if !matches!(a, Argument::SpreadElement(_)) { walk_expr(a.to_expression(), f); }
            }
        }
        Expression::NewExpression(n) => {
            walk_expr(&n.callee, f);
            for a in &n.arguments {
                if !matches!(a, Argument::SpreadElement(_)) { walk_expr(a.to_expression(), f); }
            }
        }
        Expression::ArrayExpression(arr) => {
            for el in &arr.elements {
                if !matches!(el, ArrayExpressionElement::SpreadElement(_) | ArrayExpressionElement::Elision(_)) {
                    walk_expr(el.to_expression(), f);
                }
            }
        }
        Expression::ParenthesizedExpression(p) => walk_expr(&p.expression, f),
        Expression::StaticMemberExpression(m) => walk_expr(&m.object, f),
        Expression::ComputedMemberExpression(m) => { walk_expr(&m.object, f); walk_expr(&m.expression, f); }
        Expression::AssignmentExpression(a) => walk_expr(&a.right, f),
        _ => {}
    }
}

fn walk_strings<'a, F: FnMut(&StringLiteral<'a>)>(program: &'a Program<'a>, f: &mut F) {
    for s in &program.body { walk_strings_in_stmt(s, f); }
}

fn walk_strings_in_stmt<'a, F: FnMut(&StringLiteral<'a>)>(stmt: &'a Statement<'a>, f: &mut F) {
    match stmt {
        Statement::ExpressionStatement(es) => walk_strings_in_expr(&es.expression, f),
        Statement::VariableDeclaration(vd) => {
            for d in &vd.declarations {
                if let Some(init) = &d.init { walk_strings_in_expr(init, f); }
            }
        }
        Statement::ReturnStatement(r) => if let Some(a) = &r.argument { walk_strings_in_expr(a, f); }
        Statement::IfStatement(s) => { walk_strings_in_expr(&s.test, f); walk_strings_in_stmt(&s.consequent, f); if let Some(a) = &s.alternate { walk_strings_in_stmt(a, f); } }
        Statement::BlockStatement(b) => for s in &b.body { walk_strings_in_stmt(s, f); }
        Statement::ForStatement(fs) => walk_strings_in_stmt(&fs.body, f),
        Statement::WhileStatement(w) => walk_strings_in_stmt(&w.body, f),
        Statement::DoWhileStatement(w) => walk_strings_in_stmt(&w.body, f),
        Statement::SwitchStatement(s) => for c in &s.cases { for st in &c.consequent { walk_strings_in_stmt(st, f); } }
        Statement::TryStatement(t) => {
            for st in &t.block.body { walk_strings_in_stmt(st, f); }
            if let Some(h) = &t.handler { for st in &h.body.body { walk_strings_in_stmt(st, f); } }
            if let Some(fi) = &t.finalizer { for st in &fi.body { walk_strings_in_stmt(st, f); } }
        }
        Statement::FunctionDeclaration(fd) => {
            if let Some(b) = &fd.body { for s in &b.statements { walk_strings_in_stmt(s, f); } }
        }
        Statement::LabeledStatement(s) => walk_strings_in_stmt(&s.body, f),
        Statement::ThrowStatement(t) => walk_strings_in_expr(&t.argument, f),
        _ => {}
    }
}

fn walk_strings_in_expr<'a, F: FnMut(&StringLiteral<'a>)>(e: &'a Expression<'a>, f: &mut F) {
    if let Expression::StringLiteral(s) = e { f(s); }
    match e {
        Expression::BinaryExpression(b) => { walk_strings_in_expr(&b.left, f); walk_strings_in_expr(&b.right, f); }
        Expression::LogicalExpression(l) => { walk_strings_in_expr(&l.left, f); walk_strings_in_expr(&l.right, f); }
        Expression::UnaryExpression(u) => walk_strings_in_expr(&u.argument, f),
        Expression::ConditionalExpression(c) => { walk_strings_in_expr(&c.test, f); walk_strings_in_expr(&c.consequent, f); walk_strings_in_expr(&c.alternate, f); }
        Expression::SequenceExpression(s) => for e in &s.expressions { walk_strings_in_expr(e, f); }
        Expression::CallExpression(c) => {
            walk_strings_in_expr(&c.callee, f);
            for a in &c.arguments {
                if !matches!(a, Argument::SpreadElement(_)) { walk_strings_in_expr(a.to_expression(), f); }
            }
        }
        Expression::NewExpression(n) => {
            walk_strings_in_expr(&n.callee, f);
            for a in &n.arguments {
                if !matches!(a, Argument::SpreadElement(_)) { walk_strings_in_expr(a.to_expression(), f); }
            }
        }
        Expression::ArrayExpression(arr) => {
            for el in &arr.elements {
                if !matches!(el, ArrayExpressionElement::SpreadElement(_) | ArrayExpressionElement::Elision(_)) {
                    walk_strings_in_expr(el.to_expression(), f);
                }
            }
        }
        Expression::ParenthesizedExpression(p) => walk_strings_in_expr(&p.expression, f),
        Expression::StaticMemberExpression(m) => walk_strings_in_expr(&m.object, f),
        Expression::ComputedMemberExpression(m) => { walk_strings_in_expr(&m.object, f); walk_strings_in_expr(&m.expression, f); }
        Expression::AssignmentExpression(a) => walk_strings_in_expr(&a.right, f),
        Expression::FunctionExpression(fe) => {
            if let Some(b) = &fe.body {
                for s in &b.statements { walk_strings_in_stmt(s, f); }
            }
        }
        Expression::ArrowFunctionExpression(af) => {
            for s in &af.body.statements { walk_strings_in_stmt(s, f); }
        }
        Expression::ObjectExpression(obj) => {
            for p in &obj.properties {
                if let ObjectPropertyKind::ObjectProperty(prop) = p {
                    walk_strings_in_expr(&prop.value, f);
                }
            }
        }
        _ => {}
    }
}
