use oxc_allocator::Allocator;
use oxc_ast::AstBuilder;
use oxc_ast::ast::*;
use oxc_ast_visit::VisitMut;
use oxc_semantic::{ReferenceFlags, SemanticBuilder};
use oxc_span::SPAN;
use oxc_syntax::node::NodeId;
use oxc_syntax::number::NumberBase;
use oxc_syntax::operator::{BinaryOperator, UnaryOperator};
use rustc_hash::{FxHashMap as HashMap, FxHashSet as HashSet};

use crate::core::{
    JsValue, PureFn, atob_default, call_padded, classify_pure_fn,
    eval_expr, js_to_string, to_num,
};

// ============================================================================
// pure_calls — pure-MBA helpers + String.fromCharCode/atob aliases
// ============================================================================

#[derive(Clone, Debug)]
enum PureKind {
    Mba(PureFn),
    FromCharCode,
    Atob,
}

pub fn pure_calls<'a>(program: &mut Program<'a>, alloc: &'a Allocator) -> usize {
    let mut by_name: HashMap<String, PureKind> = HashMap::default();
    for stmt in &program.body {
        match stmt {
            Statement::FunctionDeclaration(fd) => {
                if let Some(name) = fd.id.as_ref().map(|i| i.name.as_str().to_string()) {
                    if let Some(pf) = classify_pure_fn(fd) {
                        by_name.insert(name, PureKind::Mba(pf));
                    }
                }
            }
            Statement::VariableDeclaration(vd) => {
                for d in &vd.declarations {
                    let BindingPattern::BindingIdentifier(id) = &d.id else { continue };
                    let Some(init) = &d.init else { continue };
                    if let Some(kind) = classify_pure_alias(init) {
                        by_name.insert(id.name.as_str().to_string(), kind);
                    }
                }
            }
            _ => {}
        }
    }
    if by_name.is_empty() { return 0; }
    if std::env::var_os("DD_DUMP_PURE").is_some() {
        let mut keys: Vec<&String> = by_name.keys().collect();
        keys.sort();
        eprintln!("pure_calls table ({}): {:?}", keys.len(), keys);
    }

    let plan: HashMap<NodeId, PureKind> = {
        let ret = SemanticBuilder::new().build(program);
        if !ret.errors.is_empty() { return 0; }
        let scoping = ret.semantic.scoping();
        let root_scope = scoping.root_scope_id();
        let mut plan: HashMap<NodeId, PureKind> = HashMap::default();
        for symbol_id in 0..scoping.symbols_len() {
            let sym = oxc_syntax::symbol::SymbolId::from_usize(symbol_id);
            if scoping.symbol_scope_id(sym) != root_scope { continue; }
            let name = scoping.symbol_name(sym);
            let Some(kind) = by_name.get(name) else { continue };
            for r in scoping.get_resolved_references(sym) {
                if !r.flags().contains(ReferenceFlags::Read) { continue; }
                plan.insert(r.node_id(), kind.clone());
            }
        }
        plan
    };
    if plan.is_empty() { return 0; }

    let mut v = PureCallsFolder { alloc, plan, count: 0, cache: HashMap::default() };
    v.visit_program(program);
    v.count
}

fn classify_pure_alias(init: &Expression) -> Option<PureKind> {
    match init {
        Expression::StaticMemberExpression(m) => {
            let Expression::Identifier(obj) = &m.object else { return None };
            if obj.name.as_str() == "String" && m.property.name.as_str() == "fromCharCode" {
                return Some(PureKind::FromCharCode);
            }
            None
        }
        Expression::ComputedMemberExpression(m) => {
            let Expression::Identifier(obj) = &m.object else { return None };
            let Expression::StringLiteral(s) = &m.expression else { return None };
            if obj.name.as_str() == "String" && s.value.as_str() == "fromCharCode" {
                return Some(PureKind::FromCharCode);
            }
            None
        }
        Expression::Identifier(id) => match id.name.as_str() {
            "atob" => Some(PureKind::Atob),
            _ => None,
        },
        _ => None,
    }
}

struct PureCallsFolder<'a> {
    alloc: &'a Allocator,
    plan: HashMap<NodeId, PureKind>,
    count: usize,
    cache: HashMap<(NodeId, String), JsValue>,
}

impl<'a> PureCallsFolder<'a> {
    fn key_for(args: &[JsValue]) -> String {
        let mut k = String::with_capacity(32);
        for (i, a) in args.iter().enumerate() {
            if i > 0 { k.push(','); }
            match a {
                JsValue::Num(n) => { k.push('n'); k.push_str(&n.to_bits().to_string()); }
                JsValue::Str(s) => { k.push('s'); k.push_str(s); }
                JsValue::Bool(b) => { k.push(if *b { 't' } else { 'f' }); }
                _ => k.push('?'),
            }
        }
        k
    }

    fn try_call(&mut self, nid: NodeId, kind: &PureKind, args: &[JsValue]) -> Option<JsValue> {
        let key = (nid, Self::key_for(args));
        if let Some(v) = self.cache.get(&key) { return Some(v.clone()); }
        let v = match kind {
            PureKind::Mba(f) => call_padded(f, args)?,
            PureKind::FromCharCode => {
                let mut s = String::with_capacity(args.len());
                for a in args {
                    let n = to_num(a);
                    if !n.is_finite() { return None; }
                    let c = (n as i32 as u32) & 0xFFFF;
                    s.push(char::from_u32(c).unwrap_or('\u{FFFD}'));
                }
                JsValue::Str(s)
            }
            PureKind::Atob => {
                let JsValue::Str(s) = &args[0] else { return None };
                JsValue::Str(atob_default(s)?)
            }
        };
        self.cache.insert(key, v.clone());
        Some(v)
    }
}

impl<'a> VisitMut<'a> for PureCallsFolder<'a> {
    fn visit_expression(&mut self, expr: &mut Expression<'a>) {
        oxc_ast_visit::walk_mut::walk_expression(self, expr);
        if let Expression::CallExpression(call) = expr {
            let Expression::Identifier(id) = &call.callee else { return };
            let nid = id.node_id.get();
            let Some(kind) = self.plan.get(&nid).cloned() else { return };
            let env: HashMap<&str, JsValue> = HashMap::default();
            let mut args: Vec<JsValue> = Vec::with_capacity(call.arguments.len());
            for a in &call.arguments {
                let arg_expr = match a {
                    Argument::SpreadElement(_) => return,
                    _ => a.to_expression(),
                };
                let Some(v) = eval_expr(arg_expr, &env) else { return };
                args.push(v);
            }
            if let Some(v) = self.try_call(nid, &kind, &args) {
                if let Some(new_expr) = v.to_expr(self.alloc) {
                    *expr = new_expr;
                    self.count += 1;
                }
            }
            return;
        }
        if let Expression::BinaryExpression(bin) = expr {
            if bin.operator == BinaryOperator::Addition {
                let ast = AstBuilder::new(self.alloc);
                if let (Expression::StringLiteral(a), Expression::StringLiteral(b)) = (&bin.left, &bin.right) {
                    let merged = format!("{}{}", a.value.as_str(), b.value.as_str());
                    *expr = ast.expression_string_literal(SPAN, ast.str(&merged), None);
                    self.count += 1;
                }
            }
        }
    }
}

// ============================================================================
// window_methods — fold o.Math.* / o.parseInt / o.Number / o.String.fromCharCode
// ============================================================================

pub fn window_methods<'a>(program: &mut Program<'a>, alloc: &'a Allocator) -> usize {
    let mut alias_names: HashSet<String> = HashSet::default();
    alias_names.insert("window".into());
    alias_names.insert("self".into());
    alias_names.insert("globalThis".into());
    for stmt in &program.body {
        let Statement::VariableDeclaration(vd) = stmt else { continue };
        for d in &vd.declarations {
            let BindingPattern::BindingIdentifier(id) = &d.id else { continue };
            let Some(init) = &d.init else { continue };
            if is_window_expr(init) {
                alias_names.insert(id.name.as_str().to_string());
            }
        }
    }

    let alias_refs: HashSet<NodeId> = {
        let ret = SemanticBuilder::new().build(program);
        if !ret.errors.is_empty() { return 0; }
        let scoping = ret.semantic.scoping();
        let root_scope = scoping.root_scope_id();
        let mut refs: HashSet<NodeId> = HashSet::default();
        for symbol_id in 0..scoping.symbols_len() {
            let sym = oxc_syntax::symbol::SymbolId::from_usize(symbol_id);
            if scoping.symbol_scope_id(sym) != root_scope { continue; }
            let name = scoping.symbol_name(sym);
            if !alias_names.contains(name) { continue; }
            for r in scoping.get_resolved_references(sym) {
                if r.flags().contains(ReferenceFlags::Read) {
                    refs.insert(r.node_id());
                }
            }
        }
        // Unresolved global identifiers (window/self/globalThis when not declared)
        // also need to be foldable; mark them by name fallback in the visitor.
        refs
    };

    let mut v = WindowMethodsFolder { alloc, alias_refs, alias_names, count: 0 };
    v.visit_program(program);
    v.count
}

fn is_window_expr(e: &Expression) -> bool {
    match e {
        Expression::Identifier(id) => matches!(id.name.as_str(), "window" | "self" | "globalThis"),
        Expression::ThisExpression(_) => true,
        Expression::CallExpression(call) => {
            if let Expression::FunctionExpression(fe) = &call.callee {
                if let Some(b) = &fe.body {
                    if b.statements.len() == 1 {
                        if let Statement::ReturnStatement(rs) = &b.statements[0] {
                            return matches!(rs.argument.as_ref(), Some(Expression::ThisExpression(_)));
                        }
                    }
                }
            }
            if let Expression::NewExpression(ne) = &call.callee {
                if matches!(&ne.callee, Expression::Identifier(c) if c.name.as_str() == "Function") {
                    return true;
                }
            }
            false
        }
        _ => false,
    }
}

struct WindowMethodsFolder<'a> {
    alloc: &'a Allocator,
    alias_refs: HashSet<NodeId>,
    alias_names: HashSet<String>,
    count: usize,
}

fn dotted_path<'a, 'b>(callee: &'b Expression<'a>) -> Option<(&'b IdentifierReference<'a>, Vec<String>)> {
    match callee {
        Expression::Identifier(id) => Some((id, vec![id.name.as_str().to_string()])),
        Expression::StaticMemberExpression(m) => {
            let (root, mut left) = dotted_path(&m.object)?;
            left.push(m.property.name.as_str().to_string());
            Some((root, left))
        }
        Expression::ComputedMemberExpression(m) => {
            let Expression::StringLiteral(s) = &m.expression else { return None };
            let (root, mut left) = dotted_path(&m.object)?;
            left.push(s.value.as_str().to_string());
            Some((root, left))
        }
        _ => None,
    }
}

fn invoke_builtin(method: &[String], args: &[JsValue]) -> Option<JsValue> {
    match method.iter().map(|s| s.as_str()).collect::<Vec<&str>>().as_slice() {
        ["Math", m] => math_call(m, args),
        ["String", "fromCharCode"] => {
            let mut s = String::with_capacity(args.len());
            for a in args {
                let n = to_num(a);
                if !n.is_finite() { return None; }
                let c = (n as i32 as u32) & 0xFFFF;
                if let Some(ch) = char::from_u32(c) { s.push(ch); } else { return None; }
            }
            Some(JsValue::Str(s))
        }
        ["Number"] => {
            if args.is_empty() { return Some(JsValue::Num(0.0)); }
            Some(JsValue::Num(to_num(&args[0])))
        }
        ["parseInt"] => {
            if args.is_empty() { return Some(JsValue::Num(f64::NAN)); }
            let s = match &args[0] {
                JsValue::Str(s) => s.clone(),
                JsValue::Num(n) => js_to_string(&JsValue::Num(*n)),
                _ => return None,
            };
            let radix = if args.len() >= 2 { to_num(&args[1]) as i32 } else { 10 };
            let s = s.trim_start();
            let (sign, rest) = if let Some(r) = s.strip_prefix('-') { (-1.0, r) }
                else if let Some(r) = s.strip_prefix('+') { (1.0, r) }
                else { (1.0, s) };
            let mut end = 0;
            for (i, c) in rest.char_indices() {
                if c.is_digit(radix.max(2) as u32) { end = i + c.len_utf8(); } else { break; }
            }
            if end == 0 { return Some(JsValue::Num(f64::NAN)); }
            let r = radix.max(2) as u32;
            i64::from_str_radix(&rest[..end], r).ok().map(|n| JsValue::Num(sign * n as f64))
        }
        ["parseFloat"] => {
            if args.is_empty() { return Some(JsValue::Num(f64::NAN)); }
            let s = match &args[0] {
                JsValue::Str(s) => s.clone(),
                JsValue::Num(n) => js_to_string(&JsValue::Num(*n)),
                _ => return None,
            };
            Some(JsValue::Num(s.trim().parse::<f64>().unwrap_or(f64::NAN)))
        }
        ["isNaN"] => {
            if args.is_empty() { return Some(JsValue::Bool(true)); }
            Some(JsValue::Bool(to_num(&args[0]).is_nan()))
        }
        ["isFinite"] => {
            if args.is_empty() { return Some(JsValue::Bool(false)); }
            Some(JsValue::Bool(to_num(&args[0]).is_finite()))
        }
        ["String"] => {
            if args.is_empty() { return Some(JsValue::Str(String::new())); }
            Some(JsValue::Str(js_to_string(&args[0])))
        }
        _ => None,
    }
}

fn math_call(name: &str, args: &[JsValue]) -> Option<JsValue> {
    let f = if !args.is_empty() { to_num(&args[0]) } else { f64::NAN };
    let s = if args.len() >= 2 { to_num(&args[1]) } else { f64::NAN };
    let r = match name {
        "abs" => f.abs(),
        "ceil" => f.ceil(),
        "floor" => f.floor(),
        "round" => if (f - f.floor()) >= 0.5 { f.ceil() } else { f.floor() },
        "trunc" => f.trunc(),
        "sign" => f.signum(),
        "sqrt" => f.sqrt(),
        "cbrt" => f.cbrt(),
        "log" => f.ln(),
        "log2" => f.log2(),
        "log10" => f.log10(),
        "exp" => f.exp(),
        "sin" => f.sin(),
        "cos" => f.cos(),
        "tan" => f.tan(),
        "asin" => f.asin(),
        "acos" => f.acos(),
        "atan" => f.atan(),
        "sinh" => f.sinh(),
        "cosh" => f.cosh(),
        "tanh" => f.tanh(),
        "fround" => f as f32 as f64,
        "clz32" => (f as u32).leading_zeros() as f64,
        "imul" => ((f as i32).wrapping_mul(s as i32)) as f64,
        "min" => args.iter().fold(f64::INFINITY, |a, v| a.min(to_num(v))),
        "max" => args.iter().fold(f64::NEG_INFINITY, |a, v| a.max(to_num(v))),
        "pow" => f.powf(s),
        "atan2" => f.atan2(s),
        "hypot" => f.hypot(s),
        _ => return None,
    };
    Some(JsValue::Num(r))
}

impl<'a> VisitMut<'a> for WindowMethodsFolder<'a> {
    fn visit_expression(&mut self, expr: &mut Expression<'a>) {
        oxc_ast_visit::walk_mut::walk_expression(self, expr);
        let Expression::CallExpression(call) = expr else { return };
        let Some((root_id, path)) = dotted_path(&call.callee) else { return };
        if path.is_empty() { return; }
        let root_name = path[0].as_str();
        let is_global_alias = matches!(root_name, "window" | "self" | "globalThis");
        let resolved = self.alias_refs.contains(&root_id.node_id.get());
        if !resolved && !is_global_alias { return; }
        if !resolved && !self.alias_names.contains(root_name) { return; }
        let method: Vec<String> = path[1..].iter().cloned().collect();
        if method.is_empty() { return; }
        let env: HashMap<&str, JsValue> = HashMap::default();
        let mut args: Vec<JsValue> = Vec::with_capacity(call.arguments.len());
        for a in &call.arguments {
            let arg_expr = match a {
                Argument::SpreadElement(_) => return,
                _ => a.to_expression(),
            };
            let Some(v) = eval_expr(arg_expr, &env) else { return };
            args.push(v);
        }
        let Some(v) = invoke_builtin(&method, &args) else { return };
        if let Some(new_expr) = v.to_expr(self.alloc) {
            *expr = new_expr;
            self.count += 1;
        }
    }
}

// ============================================================================
// string_decoders — atob decoder + custom-alphabet base64 decoder
// ============================================================================

#[derive(Clone, Debug)]
enum DecoderKind {
    Atob { table: String },
    CustomB64 { table: String, alphabet: String },
}

pub fn string_decoders<'a>(program: &mut Program<'a>, alloc: &'a Allocator) -> usize {
    let tables = collect_tables(program);
    if tables.is_empty() { return 0; }
    let decoders = collect_decoders(program, &tables);
    if decoders.is_empty() { return 0; }
    if std::env::var_os("DD_DUMP_DEC").is_some() {
        for (name, k) in &decoders {
            match k {
                DecoderKind::Atob { table } => eprintln!("decoder {} = atob({}[]) ({} entries)",
                    name, table, tables.get(table).map(|v| v.len()).unwrap_or(0)),
                DecoderKind::CustomB64 { table, alphabet } => eprintln!(
                    "decoder {} = custom_b64({}[], alphabet={:?}) ({} entries)",
                    name, table, alphabet, tables.get(table).map(|v| v.len()).unwrap_or(0)),
            }
        }
    }

    let plan: HashMap<NodeId, DecoderKind> = {
        let ret = SemanticBuilder::new().build(program);
        if !ret.errors.is_empty() { return 0; }
        let scoping = ret.semantic.scoping();
        let root_scope = scoping.root_scope_id();
        let mut plan: HashMap<NodeId, DecoderKind> = HashMap::default();
        for symbol_id in 0..scoping.symbols_len() {
            let sym = oxc_syntax::symbol::SymbolId::from_usize(symbol_id);
            if scoping.symbol_scope_id(sym) != root_scope { continue; }
            let name = scoping.symbol_name(sym);
            let Some(kind) = decoders.get(name) else { continue };
            for r in scoping.get_resolved_references(sym) {
                if !r.flags().contains(ReferenceFlags::Read) { continue; }
                plan.insert(r.node_id(), kind.clone());
            }
        }
        plan
    };
    if plan.is_empty() { return 0; }

    let mut v = DecoderFolder { alloc, tables, plan, count: 0 };
    v.visit_program(program);
    v.count
}

fn collect_tables(program: &Program) -> HashMap<String, Vec<Option<JsValue>>> {
    let mut out: HashMap<String, Vec<Option<JsValue>>> = HashMap::default();
    let env: HashMap<&str, JsValue> = HashMap::default();
    for stmt in &program.body {
        let Statement::VariableDeclaration(vd) = stmt else { continue };
        for d in &vd.declarations {
            let BindingPattern::BindingIdentifier(id) = &d.id else { continue };
            let Some(Expression::ArrayExpression(arr)) = &d.init else { continue };
            if arr.elements.len() < 8 { continue; }
            let mut entries: Vec<Option<JsValue>> = Vec::with_capacity(arr.elements.len());
            let mut string_count = 0usize;
            for el in &arr.elements {
                match el {
                    ArrayExpressionElement::Elision(_) | ArrayExpressionElement::SpreadElement(_) => entries.push(None),
                    _ => {
                        let e = el.to_expression();
                        match eval_expr(e, &env) {
                            Some(JsValue::Str(s)) => {
                                if !s.is_empty() && is_base64_word(&s) { string_count += 1; }
                                entries.push(Some(JsValue::Str(s)));
                            }
                            Some(other) => entries.push(Some(other)),
                            None => entries.push(None),
                        }
                    }
                }
            }
            if string_count * 4 >= entries.len() {
                out.insert(id.name.as_str().to_string(), entries);
            }
        }
    }
    out
}

fn is_base64_word(s: &str) -> bool {
    if s.is_empty() { return false; }
    s.chars().all(|c| c.is_ascii_alphanumeric() || c == '+' || c == '/' || c == '=' || c == '_' || c == '-')
}

fn collect_decoders(program: &Program, tables: &HashMap<String, Vec<Option<JsValue>>>) -> HashMap<String, DecoderKind> {
    let mut out = HashMap::default();
    for stmt in &program.body {
        let Statement::FunctionDeclaration(fd) = stmt else { continue };
        let Some(name) = fd.id.as_ref().map(|i| i.name.as_str().to_string()) else { continue };
        let Some(body) = &fd.body else { continue };
        if let Some(kind) = match_atob_decoder(body, tables) { out.insert(name, kind); continue; }
        if let Some(kind) = match_custom_b64_decoder(body, tables) { out.insert(name, kind); continue; }
    }
    out
}

fn match_atob_decoder(body: &FunctionBody, tables: &HashMap<String, Vec<Option<JsValue>>>) -> Option<DecoderKind> {
    if body.statements.is_empty() { return None; }

    if body.statements.len() == 1 {
        let Statement::ReturnStatement(rs) = &body.statements[0] else { return None };
        let arg = rs.argument.as_ref()?;
        let Expression::SequenceExpression(seq) = arg else { return None };
        if seq.expressions.len() != 2 { return None; }
        let Expression::AssignmentExpression(asn) = &seq.expressions[0] else { return None };
        let AssignmentTarget::AssignmentTargetIdentifier(_) = &asn.left else { return None };
        let Expression::ComputedMemberExpression(mem) = &asn.right else { return None };
        let Expression::Identifier(table_id) = &mem.object else { return None };
        if !tables.contains_key(table_id.name.as_str()) { return None; }
        let Expression::CallExpression(call) = &seq.expressions[1] else { return None };
        let Expression::Identifier(callee) = &call.callee else { return None };
        if callee.name.as_str() != "atob" { return None; }
        return Some(DecoderKind::Atob { table: table_id.name.as_str().to_string() });
    }

    if body.statements.len() == 2 {
        let Statement::VariableDeclaration(vd) = &body.statements[0] else { return None };
        let mut local_var: Option<String> = None;
        let mut table_name: Option<String> = None;
        for d in &vd.declarations {
            let BindingPattern::BindingIdentifier(id) = &d.id else { continue };
            let Some(Expression::ComputedMemberExpression(mem)) = &d.init else { continue };
            let Expression::Identifier(t) = &mem.object else { continue };
            if !tables.contains_key(t.name.as_str()) { continue; }
            local_var = Some(id.name.as_str().to_string());
            table_name = Some(t.name.as_str().to_string());
            break;
        }
        let local_var = local_var?;
        let table_name = table_name?;
        let Statement::ReturnStatement(rs) = &body.statements[1] else { return None };
        let Expression::CallExpression(call) = rs.argument.as_ref()? else { return None };
        let Expression::Identifier(callee) = &call.callee else { return None };
        if callee.name.as_str() != "atob" { return None; }
        if call.arguments.len() != 1 { return None; }
        let Argument::Identifier(arg_id) = &call.arguments[0] else { return None };
        if arg_id.name.as_str() != local_var { return None; }
        return Some(DecoderKind::Atob { table: table_name });
    }

    None
}

fn match_custom_b64_decoder(body: &FunctionBody, tables: &HashMap<String, Vec<Option<JsValue>>>) -> Option<DecoderKind> {
    if body.statements.is_empty() { return None; }
    let mut local_table_var: Option<String> = None;
    let mut prelude_table_name: Option<String> = None;
    if body.statements.len() >= 2 {
        let Statement::VariableDeclaration(vd) = &body.statements[0] else { return None };
        for d in &vd.declarations {
            let BindingPattern::BindingIdentifier(id) = &d.id else { continue };
            let Some(Expression::ComputedMemberExpression(mem)) = &d.init else { continue };
            let Expression::Identifier(t) = &mem.object else { continue };
            if !tables.contains_key(t.name.as_str()) { continue; }
            local_table_var = Some(id.name.as_str().to_string());
            prelude_table_name = Some(t.name.as_str().to_string());
            break;
        }
        if local_table_var.is_none() { return None; }
    }
    let last_idx = body.statements.len() - 1;
    let Statement::ReturnStatement(rs) = &body.statements[last_idx] else { return None };
    let arg = rs.argument.as_ref()?;
    let Expression::ConditionalExpression(cond) = arg else { return None };
    let table_name: String;
    {
        let Expression::BinaryExpression(bin) = &cond.test else { return None };
        let table_member = match (&bin.left, &bin.right) {
            (Expression::UnaryExpression(u), _) if matches!(u.operator, UnaryOperator::Typeof) => &u.argument,
            (_, Expression::UnaryExpression(u)) if matches!(u.operator, UnaryOperator::Typeof) => &u.argument,
            _ => return None,
        };
        let mem_expr = match table_member {
            Expression::ParenthesizedExpression(p) => &p.expression,
            other => other,
        };
        if let Some(local) = &local_table_var {
            let resolved = matches!(mem_expr, Expression::Identifier(id) if id.name.as_str() == local);
            if !resolved { return None; }
            table_name = prelude_table_name.clone()?;
        } else {
            let object_expr = match mem_expr {
                Expression::AssignmentExpression(asn) => &asn.right,
                Expression::ComputedMemberExpression(_) => mem_expr,
                _ => return None,
            };
            let Expression::ComputedMemberExpression(member) = object_expr else { return None };
            let Expression::Identifier(t) = &member.object else { return None };
            if !tables.contains_key(t.name.as_str()) { return None; }
            table_name = t.name.as_str().to_string();
        }
    }
    let Expression::CallExpression(iife_call) = &cond.consequent else { return None };
    let Expression::FunctionExpression(iife_fn) = &iife_call.callee else { return None };
    let alphabet = extract_alphabet(iife_fn)?;
    Some(DecoderKind::CustomB64 { table: table_name, alphabet })
}

fn extract_alphabet(fn_expr: &Function) -> Option<String> {
    let body = fn_expr.body.as_ref()?;
    for stmt in &body.statements {
        if let Some(s) = scan_alphabet_stmt(stmt) { return Some(s); }
    }
    None
}

fn scan_alphabet_stmt(stmt: &Statement) -> Option<String> {
    match stmt {
        Statement::ExpressionStatement(es) => scan_alphabet_expr(&es.expression),
        Statement::ForStatement(fs) => fs.init.as_ref().and_then(scan_alphabet_for_init),
        Statement::VariableDeclaration(vd) => {
            for d in &vd.declarations {
                if let Some(init) = &d.init {
                    if let Some(s) = static_string(init) {
                        if s.len() >= 60 { return Some(s); }
                    }
                }
            }
            None
        }
        _ => None,
    }
}

fn scan_alphabet_expr(e: &Expression) -> Option<String> {
    match e {
        Expression::AssignmentExpression(asn) => {
            if let Some(s) = static_string(&asn.right) {
                if s.len() >= 60 { return Some(s); }
            }
            None
        }
        Expression::SequenceExpression(seq) => {
            for inner in &seq.expressions {
                if let Some(s) = scan_alphabet_expr(inner) { return Some(s); }
            }
            None
        }
        _ => None,
    }
}

fn scan_alphabet_for_init(init: &ForStatementInit) -> Option<String> {
    match init {
        ForStatementInit::SequenceExpression(seq) => {
            for ex in &seq.expressions {
                if let Some(s) = scan_alphabet_expr(ex) { return Some(s); }
            }
            None
        }
        ForStatementInit::AssignmentExpression(asn) => {
            if let Some(s) = static_string(&asn.right) {
                if s.len() >= 60 { return Some(s); }
            }
            None
        }
        _ => None,
    }
}

fn static_string(e: &Expression) -> Option<String> {
    let env: HashMap<&str, JsValue> = HashMap::default();
    match eval_expr(e, &env) {
        Some(JsValue::Str(s)) => Some(s),
        _ => None,
    }
}

struct DecoderFolder<'a> {
    alloc: &'a Allocator,
    tables: HashMap<String, Vec<Option<JsValue>>>,
    plan: HashMap<NodeId, DecoderKind>,
    count: usize,
}

impl<'a> DecoderFolder<'a> {
    fn try_decode(&self, kind: &DecoderKind, idx: usize) -> Option<JsValue> {
        let table_name = match kind {
            DecoderKind::Atob { table } => table,
            DecoderKind::CustomB64 { table, .. } => table,
        };
        let entries = self.tables.get(table_name)?;
        let entry = entries.get(idx)?.as_ref()?;
        match kind {
            DecoderKind::Atob { .. } => {
                if let JsValue::Str(s) = entry { return Some(JsValue::Str(atob_default(s)?)); }
                Some(entry.clone())
            }
            DecoderKind::CustomB64 { alphabet, .. } => {
                if let JsValue::Str(s) = entry { return Some(JsValue::Str(custom_b64_decode(s, alphabet)?)); }
                Some(entry.clone())
            }
        }
    }
}

fn custom_b64_decode(input: &str, alphabet: &str) -> Option<String> {
    let cleaned: String = input.chars()
        .filter(|c| c.is_ascii_alphanumeric() || *c == '+' || *c == '/' || *c == '=')
        .collect();
    let alpha_bytes: Vec<u32> = alphabet.chars().map(|c| c as u32).collect();
    let alpha_index = |c: char| -> i32 {
        let cu = c as u32;
        for (i, &a) in alpha_bytes.iter().enumerate() { if a == cu { return i as i32; } }
        -1
    };
    let chars: Vec<char> = cleaned.chars().collect();
    let mut out = String::with_capacity(chars.len() * 3 / 4 + 4);
    let mut e = 0;
    while e < chars.len() {
        let i_a = alpha_index(*chars.get(e)?); e += 1;
        if e >= chars.len() { break; }
        let i_b = alpha_index(*chars.get(e)?); e += 1;
        let i_c = if e < chars.len() { let v = alpha_index(chars[e]); e += 1; v } else { 64 };
        let i_d = if e < chars.len() { let v = alpha_index(chars[e]); e += 1; v } else { 64 };
        let g = (i_a << 2) | (i_b >> 4);
        let a = ((15 & i_b) << 4) | (i_c >> 2);
        let c = ((3 & i_c) << 6) | i_d;
        out.push(char::from_u32((g & 0xFF) as u32)?);
        if i_c != 64 { out.push(char::from_u32((a & 0xFF) as u32)?); }
        if i_d != 64 { out.push(char::from_u32((c & 0xFF) as u32)?); }
    }
    Some(out)
}

impl<'a> VisitMut<'a> for DecoderFolder<'a> {
    fn visit_expression(&mut self, expr: &mut Expression<'a>) {
        oxc_ast_visit::walk_mut::walk_expression(self, expr);
        let Expression::CallExpression(call) = expr else { return };
        let Expression::Identifier(id) = &call.callee else { return };
        let nid = id.node_id.get();
        let Some(kind) = self.plan.get(&nid).cloned() else { return };
        if call.arguments.is_empty() { return; }
        let env: HashMap<&str, JsValue> = HashMap::default();
        let arg_expr = match &call.arguments[0] {
            Argument::SpreadElement(_) => return,
            other => other.to_expression(),
        };
        let Some(v) = eval_expr(arg_expr, &env) else { return };
        let n = to_num(&v);
        if !n.is_finite() || n < 0.0 { return; }
        let idx = n as usize;
        if let Some(value) = self.try_decode(&kind, idx) {
            if let Some(new_expr) = value.to_expr(self.alloc) {
                *expr = new_expr;
                self.count += 1;
            }
        }
    }
}

// ============================================================================
// tmatrix — 128x512 matrix IIFE recognition + static t[y][x] -> intern row
// ============================================================================

#[derive(Debug)]
struct TMatrixSpec {
    name: String,
    index_fn: PureFn,
    index_args: Vec<IndexArg>,
    return_row: i32,
    rows: i32,
}

#[derive(Debug, Clone)]
enum IndexArg {
    Lit(JsValue),
    LoopOuter,
    LoopInner,
}

pub fn tmatrix<'a>(program: &mut Program<'a>, alloc: &'a Allocator) -> usize {
    let mut pure_table: HashMap<String, PureFn> = HashMap::default();
    for stmt in &program.body {
        if let Statement::FunctionDeclaration(fd) = stmt {
            if let Some(name) = fd.id.as_ref().map(|i| i.name.as_str().to_string()) {
                if let Some(pf) = classify_pure_fn(fd) {
                    pure_table.insert(name, pf);
                }
            }
        }
    }
    if pure_table.is_empty() { return 0; }
    let mut specs: Vec<TMatrixSpec> = Vec::new();
    for stmt in &program.body {
        let Statement::VariableDeclaration(vd) = stmt else { continue };
        for d in &vd.declarations {
            let BindingPattern::BindingIdentifier(id) = &d.id else { continue };
            let Some(init) = &d.init else { continue };
            if let Some(spec) = match_tmatrix(id.name.as_str(), init, &pure_table) {
                specs.push(spec);
            }
        }
    }
    if specs.is_empty() { return 0; }
    if std::env::var_os("DD_DUMP_TM").is_some() {
        for s in &specs {
            eprintln!("tmatrix {} idx_fn=[{} params] return_row={} rows={} args={:?}",
                s.name, s.index_fn.params.len(), s.return_row, s.rows, s.index_args);
        }
    }

    let plan: HashMap<NodeId, usize> = {
        let ret = SemanticBuilder::new().build(program);
        if !ret.errors.is_empty() { return 0; }
        let scoping = ret.semantic.scoping();
        let root_scope = scoping.root_scope_id();
        let mut plan: HashMap<NodeId, usize> = HashMap::default();
        for symbol_id in 0..scoping.symbols_len() {
            let sym = oxc_syntax::symbol::SymbolId::from_usize(symbol_id);
            if scoping.symbol_scope_id(sym) != root_scope { continue; }
            let name = scoping.symbol_name(sym);
            let Some(spec_idx) = specs.iter().position(|s| s.name == name) else { continue };
            for r in scoping.get_resolved_references(sym) {
                if !r.flags().contains(ReferenceFlags::Read) { continue; }
                plan.insert(r.node_id(), spec_idx);
            }
        }
        plan
    };
    if plan.is_empty() { return 0; }

    let mut v = TMatrixFolder { alloc, specs, plan, count: 0 };
    v.visit_program(program);
    v.count
}

fn match_tmatrix(name: &str, init: &Expression, pure_table: &HashMap<String, PureFn>) -> Option<TMatrixSpec> {
    let Expression::CallExpression(call) = init else { return None };
    if !call.arguments.is_empty() { return None; }
    let Expression::FunctionExpression(fe) = &call.callee else { return None };
    let body = fe.body.as_ref()?;
    if body.statements.len() < 3 { return None; }
    let mut iter = body.statements.iter();
    let _decl = iter.next()?;
    let init_for = iter.next()?;
    let populate_for = iter.next()?;
    let ret = iter.next()?;
    if iter.next().is_some() { return None; }

    let row_var: String;
    let rows: i32;
    {
        let Statement::ForStatement(fs) = init_for else { return None };
        let Some(Expression::BinaryExpression(bin)) = fs.test.as_ref() else { return None };
        let Expression::NumericLiteral(n) = &bin.right else { return None };
        rows = n.value as i32;
        let Some(stmt_body) = forstmt_body(&fs.body) else { return None };
        let Statement::ExpressionStatement(es) = stmt_body else { return None };
        let Expression::AssignmentExpression(asn) = &es.expression else { return None };
        let AssignmentTarget::ComputedMemberExpression(mem) = &asn.left else { return None };
        let Expression::Identifier(id) = &mem.object else { return None };
        row_var = id.name.as_str().to_string();
        let Expression::NewExpression(new_expr) = &asn.right else { return None };
        if !matches!(&new_expr.callee, Expression::Identifier(c) if c.name.as_str() == "Array") { return None };
        if new_expr.arguments.len() != 1 { return None; }
        if !matches!(&new_expr.arguments[0], Argument::NumericLiteral(_)) { return None; }
    }

    let outer_loop_var: String;
    let inner_loop_var: String;
    let index_fn_name: String;
    let index_args: Vec<IndexArg>;
    {
        let Statement::ForStatement(outer) = populate_for else { return None };
        outer_loop_var = forinit_assign_var(outer.init.as_ref()?)?;
        let Some(outer_body) = forstmt_body(&outer.body) else { return None };
        let Statement::ForStatement(inner) = outer_body else { return None };
        inner_loop_var = forinit_assign_var(inner.init.as_ref()?)?;
        let Some(inner_body) = forstmt_body(&inner.body) else { return None };
        let Statement::ExpressionStatement(es) = inner_body else { return None };
        let Expression::AssignmentExpression(asn) = &es.expression else { return None };
        let AssignmentTarget::ComputedMemberExpression(_) = &asn.left else { return None };
        let Expression::ComputedMemberExpression(rhs_mem) = &asn.right else { return None };
        if !matches!(&rhs_mem.object, Expression::Identifier(c) if c.name.as_str() == row_var) { return None };
        let Expression::CallExpression(c) = &rhs_mem.expression else { return None };
        let Expression::Identifier(callee) = &c.callee else { return None };
        index_fn_name = callee.name.as_str().to_string();
        let mut args: Vec<IndexArg> = Vec::with_capacity(c.arguments.len());
        for a in &c.arguments {
            let env: HashMap<&str, JsValue> = HashMap::default();
            match a {
                Argument::SpreadElement(_) => return None,
                _ => {
                    let e = a.to_expression();
                    if let Expression::Identifier(id) = e {
                        let n = id.name.as_str();
                        if n == inner_loop_var { args.push(IndexArg::LoopInner); continue; }
                        if n == outer_loop_var { args.push(IndexArg::LoopOuter); continue; }
                    }
                    args.push(IndexArg::Lit(eval_expr(e, &env)?));
                }
            }
        }
        index_args = args;
    }

    let return_row: i32;
    {
        let Statement::ReturnStatement(rs) = ret else { return None };
        let Expression::ComputedMemberExpression(mem) = rs.argument.as_ref()? else { return None };
        if !matches!(&mem.object, Expression::Identifier(c) if c.name.as_str() == row_var) { return None };
        let Expression::NumericLiteral(n) = &mem.expression else { return None };
        return_row = n.value as i32;
    }

    let index_fn = pure_table.get(&index_fn_name)?.clone();
    Some(TMatrixSpec { name: name.to_string(), index_fn, index_args, return_row, rows })
}

fn forinit_assign_var(init: &ForStatementInit) -> Option<String> {
    match init {
        ForStatementInit::SequenceExpression(se) => {
            for e in &se.expressions {
                if let Expression::AssignmentExpression(asn) = e {
                    if let AssignmentTarget::AssignmentTargetIdentifier(id) = &asn.left {
                        if matches!(&asn.right, Expression::NumericLiteral(_)) {
                            return Some(id.name.as_str().to_string());
                        }
                    }
                }
            }
            None
        }
        ForStatementInit::AssignmentExpression(asn) => {
            let AssignmentTarget::AssignmentTargetIdentifier(id) = &asn.left else { return None };
            if matches!(&asn.right, Expression::NumericLiteral(_)) {
                return Some(id.name.as_str().to_string());
            }
            None
        }
        _ => None,
    }
}

fn forstmt_body<'a, 'b>(body: &'b Statement<'a>) -> Option<&'b Statement<'a>> {
    match body {
        Statement::BlockStatement(b) => b.body.first(),
        other => Some(other),
    }
}

struct TMatrixFolder<'a> {
    alloc: &'a Allocator,
    specs: Vec<TMatrixSpec>,
    plan: HashMap<NodeId, usize>,
    count: usize,
}

impl<'a> TMatrixFolder<'a> {
    fn lookup(&self, spec: &TMatrixSpec, outer: f64, inner: f64) -> Option<JsValue> {
        let mut args: Vec<JsValue> = Vec::with_capacity(spec.index_args.len());
        for a in &spec.index_args {
            args.push(match a {
                IndexArg::Lit(v) => v.clone(),
                IndexArg::LoopOuter => JsValue::Num(outer),
                IndexArg::LoopInner => JsValue::Num(inner),
            });
        }
        call_padded(&spec.index_fn, &args)
    }

    fn fold(&self, spec: &TMatrixSpec, y: f64, x: f64) -> Option<i32> {
        let mid = self.lookup(spec, y, spec.return_row as f64)?;
        let mid_n = to_num(&mid);
        if !mid_n.is_finite() { return None; }
        let mid_i = mid_n as i32;
        if mid_i < 0 || mid_i >= spec.rows { return None; }
        let f = to_num(&self.lookup(spec, x, mid_i as f64)?);
        if !f.is_finite() { return None; }
        Some(f as i32)
    }
}

impl<'a> VisitMut<'a> for TMatrixFolder<'a> {
    fn visit_expression(&mut self, expr: &mut Expression<'a>) {
        oxc_ast_visit::walk_mut::walk_expression(self, expr);
        let Expression::ComputedMemberExpression(outer_mem) = expr else { return };
        let Expression::ComputedMemberExpression(inner_mem) = &outer_mem.object else { return };
        let Expression::Identifier(t_id) = &inner_mem.object else { return };
        let Some(&spec_idx) = self.plan.get(&t_id.node_id.get()) else { return };
        let spec = &self.specs[spec_idx];
        let Expression::NumericLiteral(y) = &inner_mem.expression else { return };
        let Expression::NumericLiteral(x) = &outer_mem.expression else { return };
        if let Some(r) = self.fold(spec, y.value, x.value) {
            let ast = AstBuilder::new(self.alloc);
            *expr = if r < 0 {
                let inner_lit = ast.expression_numeric_literal(SPAN, -(r as f64), None, NumberBase::Decimal);
                ast.expression_unary(SPAN, UnaryOperator::UnaryNegation, inner_lit)
            } else {
                ast.expression_numeric_literal(SPAN, r as f64, None, NumberBase::Decimal)
            };
            self.count += 1;
        }
    }
}
