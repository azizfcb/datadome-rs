use oxc_allocator::Allocator;
use oxc_ast::AstBuilder;
use oxc_ast::ast::*;
use oxc_ast_visit::VisitMut;
use oxc_codegen::Codegen;
use oxc_parser::Parser;
use oxc_span::{SPAN, SourceType};
use rustc_hash::FxHashMap as HashMap;
use sha2::{Digest, Sha256};

use crate::vm_db::KNOWN_OPCODES;
use base64::Engine as _;

pub struct VmDump {
    pub bytecode_b64: String,
    pub bytecode: Vec<u8>,
    pub disasm: String,
    pub spec: VmSpec,
}

#[derive(Debug, Clone, Default)]
pub struct VmSpec {
    pub markers: ValueMarkers,
    pub xor_key_ascii: u8,
    pub xor_key_utf8: u8,
    pub slots: SlotRoles,
}

#[derive(Debug, Clone, Default)]
pub struct ValueMarkers {
    pub r#true: Option<u8>,
    pub r#false: Option<u8>,
    pub null: Option<u8>,
    pub undefined: Option<u8>,
    pub string: Option<u8>,
    pub utf8: Option<u8>,
    pub float64: Option<u8>,
    pub int8: Option<u8>,
    pub int16: Option<u8>,
    pub int24: Option<u8>,
    pub int32: Option<u8>,
}

#[derive(Debug, Clone, Default)]
pub struct SlotRoles {
    pub stack_pointer: Option<i64>,
    pub instruction_pointer: Option<i64>,
    pub frame_base_pointer: Option<i64>,
    pub frame_base_counter: Option<i64>,
    pub last_result: Option<i64>,
    pub exit_flag: Option<i64>,
    pub current_opcode_handler: Option<i64>,
    pub current_opcode_id: Option<i64>,
    pub stack_offset: Option<i64>,
    pub vm_start: Option<i64>,
    pub dispatch_base: Option<i64>,
    /// Base slot for "special" register area, addressed as `A[base - frame_var]`.
    /// Used by LOAD_SPECIAL / STORE_SPECIAL and a few related opcodes.
    pub specials_base: Option<i64>,
}

pub fn extract(program: &Program) -> Option<VmDump> {
    let b64 = find_vm_payload(program)?;
    let cleaned: String = b64.chars().filter(|c| !c.is_whitespace()).collect();
    let bytecode = base64::engine::general_purpose::STANDARD.decode(cleaned.as_bytes())
        .or_else(|_| base64::engine::general_purpose::STANDARD_NO_PAD.decode(cleaned.as_bytes()))
        .ok()?;

    let value_decoder = analyze_value_decoder(program);
    let mut spec = VmSpec::default();
    if let Some((markers, xa, xu)) = value_decoder {
        spec.markers = markers;
        spec.xor_key_ascii = xa;
        spec.xor_key_utf8 = xu;
    }

    let typeof_helper_name = find_typeof_helper(program);
    let inner = find_inner_vm_iife(program);
    let mut handler_table: HashMap<u8, (String, &'static str, &'static str)> = HashMap::default();
    let mut header_extra = String::new();
    if let Some((slots, mut helpers, raw_handlers, value_decoder_name)) = inner {
        helpers.typeof_helper_name = typeof_helper_name;
        spec.slots = slots.clone();
        let _ = value_decoder_name;
        header_extra.push_str(&format!(
            "; slots: SP={:?} IP={:?} VMS={:?} STACK={:?} FBP={:?} FBC={:?} LR={:?} EXIT={:?} CH={:?} CI={:?} dispatch_base={:?} specials_base={:?}\n",
            slots.stack_pointer, slots.instruction_pointer, slots.vm_start, slots.stack_offset,
            slots.frame_base_pointer, slots.frame_base_counter, slots.last_result, slots.exit_flag,
            slots.current_opcode_handler, slots.current_opcode_id, slots.dispatch_base, slots.specials_base,
        ));
        if let Some(base) = slots.dispatch_base {
            let mut matched = 0usize;
            let mut total = 0usize;
            for (slot_addr, body_text) in &raw_handlers {
                let op_idx_i = *slot_addr - base;
                if !(0..=255).contains(&op_idx_i) { continue; }
                let op_idx = op_idx_i as u8;
                total += 1;
                let normalized = normalize_handler_body(body_text, &slots, &helpers);
                let h = sha_hex(&normalized);
                if std::env::var_os("VM_DUMP").is_some() {
                    eprintln!("---deob op {} ({})---\n{}", op_idx, h, normalized);
                }
                if let Some((_, name, fmt)) = KNOWN_OPCODES.iter().find(|e| e.0 == h.as_str()) {
                    handler_table.insert(op_idx, (h.clone(), name, fmt));
                    matched += 1;
                } else {
                    handler_table.insert(op_idx, (h.clone(), "", ""));
                }
            }
            header_extra.push_str(&format!("; opcodes: matched {}/{} from KNOWN_OPCODES\n", matched, total));
        } else {
            header_extra.push_str("; opcodes: dispatch_base unknown — cannot map handler slots to opcode indices\n");
        }
    } else {
        header_extra.push_str("; opcodes: VM IIFE not located — running raw byte disasm\n");
    }

    let disasm = disassemble(&bytecode, &spec, &handler_table, &header_extra);
    Some(VmDump { bytecode_b64: b64, bytecode, disasm, spec })
}

fn sha_hex(s: &str) -> String {
    let mut h = Sha256::new();
    h.update(s.as_bytes());
    let d = h.finalize();
    let mut out = String::with_capacity(16);
    for b in &d[..8] {
        out.push_str(&format!("{:02x}", b));
    }
    out
}

fn find_vm_payload(program: &Program) -> Option<String> {
    let mut hits: Vec<String> = Vec::new();
    walk_strings(program, &mut |s: &str| {
        if s.len() < 256 { return; }
        if !s.chars().all(|c| c.is_ascii_alphanumeric() || c == '+' || c == '/' || c == '=') { return; }
        if s.starts_with("AGFzbQ") { return; }
        hits.push(s.to_string());
    });
    hits.into_iter().max_by_key(|s| s.len())
}

fn walk_strings<F: FnMut(&str)>(p: &Program, f: &mut F) {
    for s in &p.body { walk_strings_stmt(s, f); }
}

fn walk_strings_stmt<F: FnMut(&str)>(s: &Statement, f: &mut F) {
    match s {
        Statement::ExpressionStatement(es) => walk_strings_expr(&es.expression, f),
        Statement::VariableDeclaration(vd) => for d in &vd.declarations {
            if let Some(init) = &d.init { walk_strings_expr(init, f); }
        },
        Statement::ReturnStatement(r) => if let Some(a) = &r.argument { walk_strings_expr(a, f); },
        Statement::BlockStatement(b) => for s in &b.body { walk_strings_stmt(s, f); },
        Statement::IfStatement(s) => { walk_strings_expr(&s.test, f); walk_strings_stmt(&s.consequent, f); if let Some(a) = &s.alternate { walk_strings_stmt(a, f); } },
        Statement::ForStatement(fs) => walk_strings_stmt(&fs.body, f),
        Statement::WhileStatement(w) => walk_strings_stmt(&w.body, f),
        Statement::DoWhileStatement(w) => walk_strings_stmt(&w.body, f),
        Statement::TryStatement(t) => {
            for s in &t.block.body { walk_strings_stmt(s, f); }
            if let Some(h) = &t.handler { for s in &h.body.body { walk_strings_stmt(s, f); } }
            if let Some(fi) = &t.finalizer { for s in &fi.body { walk_strings_stmt(s, f); } }
        },
        Statement::SwitchStatement(s) => for c in &s.cases { for st in &c.consequent { walk_strings_stmt(st, f); } },
        Statement::FunctionDeclaration(fd) => if let Some(b) = &fd.body { for s in &b.statements { walk_strings_stmt(s, f); } },
        Statement::ThrowStatement(t) => walk_strings_expr(&t.argument, f),
        Statement::LabeledStatement(s) => walk_strings_stmt(&s.body, f),
        _ => {}
    }
}

fn walk_strings_expr<F: FnMut(&str)>(e: &Expression, f: &mut F) {
    if let Expression::StringLiteral(s) = e { f(s.value.as_str()); }
    match e {
        Expression::BinaryExpression(b) => { walk_strings_expr(&b.left, f); walk_strings_expr(&b.right, f); }
        Expression::LogicalExpression(l) => { walk_strings_expr(&l.left, f); walk_strings_expr(&l.right, f); }
        Expression::UnaryExpression(u) => walk_strings_expr(&u.argument, f),
        Expression::ConditionalExpression(c) => { walk_strings_expr(&c.test, f); walk_strings_expr(&c.consequent, f); walk_strings_expr(&c.alternate, f); }
        Expression::SequenceExpression(s) => for e in &s.expressions { walk_strings_expr(e, f); },
        Expression::CallExpression(c) => { walk_strings_expr(&c.callee, f); for a in &c.arguments { if !matches!(a, Argument::SpreadElement(_)) { walk_strings_expr(a.to_expression(), f); } } }
        Expression::NewExpression(n) => { walk_strings_expr(&n.callee, f); for a in &n.arguments { if !matches!(a, Argument::SpreadElement(_)) { walk_strings_expr(a.to_expression(), f); } } }
        Expression::ArrayExpression(arr) => for el in &arr.elements {
            if !matches!(el, ArrayExpressionElement::SpreadElement(_) | ArrayExpressionElement::Elision(_)) {
                walk_strings_expr(el.to_expression(), f);
            }
        },
        Expression::ObjectExpression(obj) => for p in &obj.properties {
            if let ObjectPropertyKind::ObjectProperty(prop) = p { walk_strings_expr(&prop.value, f); }
        },
        Expression::ParenthesizedExpression(p) => walk_strings_expr(&p.expression, f),
        Expression::StaticMemberExpression(m) => walk_strings_expr(&m.object, f),
        Expression::ComputedMemberExpression(m) => { walk_strings_expr(&m.object, f); walk_strings_expr(&m.expression, f); }
        Expression::AssignmentExpression(a) => walk_strings_expr(&a.right, f),
        Expression::FunctionExpression(fe) => if let Some(b) = &fe.body { for s in &b.statements { walk_strings_stmt(s, f); } },
        Expression::ArrowFunctionExpression(af) => for s in &af.body.statements { walk_strings_stmt(s, f); },
        _ => {}
    }
}

// ----- value-decoder switch analysis (top-level function) -------------------

fn analyze_value_decoder(program: &Program) -> Option<(ValueMarkers, u8, u8)> {
    let mut found: Option<(ValueMarkers, u8, u8)> = None;
    visit_functions(program, &mut |fd: &Function| {
        if found.is_some() { return; }
        let Some(body) = &fd.body else { return };
        for stmt in &body.statements {
            let Statement::SwitchStatement(sw) = stmt else { continue };
            if let Some(t) = classify_value_decoder(&sw.cases) {
                found = Some(t);
                return;
            }
        }
    });
    found
}

fn visit_functions<'a, F: FnMut(&'a Function<'a>)>(program: &'a Program<'a>, f: &mut F) {
    for s in &program.body { visit_functions_stmt(s, f); }
}

fn visit_functions_stmt<'a, F: FnMut(&'a Function<'a>)>(s: &'a Statement<'a>, f: &mut F) {
    match s {
        Statement::FunctionDeclaration(fd) => {
            f(fd);
            if let Some(b) = &fd.body { for s in &b.statements { visit_functions_stmt(s, f); } }
        }
        Statement::ExpressionStatement(es) => visit_functions_expr(&es.expression, f),
        Statement::VariableDeclaration(vd) => for d in &vd.declarations {
            if let Some(init) = &d.init { visit_functions_expr(init, f); }
        },
        Statement::ReturnStatement(r) => if let Some(a) = &r.argument { visit_functions_expr(a, f); },
        Statement::BlockStatement(b) => for s in &b.body { visit_functions_stmt(s, f); },
        Statement::IfStatement(s) => { visit_functions_expr(&s.test, f); visit_functions_stmt(&s.consequent, f); if let Some(a) = &s.alternate { visit_functions_stmt(a, f); } },
        Statement::ForStatement(fs) => visit_functions_stmt(&fs.body, f),
        Statement::WhileStatement(w) => visit_functions_stmt(&w.body, f),
        Statement::DoWhileStatement(w) => visit_functions_stmt(&w.body, f),
        Statement::TryStatement(t) => {
            for s in &t.block.body { visit_functions_stmt(s, f); }
            if let Some(h) = &t.handler { for s in &h.body.body { visit_functions_stmt(s, f); } }
            if let Some(fi) = &t.finalizer { for s in &fi.body { visit_functions_stmt(s, f); } }
        },
        Statement::SwitchStatement(s) => for c in &s.cases { for st in &c.consequent { visit_functions_stmt(st, f); } },
        Statement::ThrowStatement(t) => visit_functions_expr(&t.argument, f),
        Statement::LabeledStatement(s) => visit_functions_stmt(&s.body, f),
        _ => {}
    }
}

fn visit_functions_expr<'a, F: FnMut(&'a Function<'a>)>(e: &'a Expression<'a>, f: &mut F) {
    match e {
        Expression::FunctionExpression(fe) => {
            f(fe);
            if let Some(b) = &fe.body { for s in &b.statements { visit_functions_stmt(s, f); } }
        }
        Expression::ArrowFunctionExpression(af) => {
            for s in &af.body.statements { visit_functions_stmt(s, f); }
        }
        Expression::CallExpression(c) => { visit_functions_expr(&c.callee, f); for a in &c.arguments { if !matches!(a, Argument::SpreadElement(_)) { visit_functions_expr(a.to_expression(), f); } } }
        Expression::BinaryExpression(b) => { visit_functions_expr(&b.left, f); visit_functions_expr(&b.right, f); }
        Expression::LogicalExpression(l) => { visit_functions_expr(&l.left, f); visit_functions_expr(&l.right, f); }
        Expression::UnaryExpression(u) => visit_functions_expr(&u.argument, f),
        Expression::ConditionalExpression(c) => { visit_functions_expr(&c.test, f); visit_functions_expr(&c.consequent, f); visit_functions_expr(&c.alternate, f); }
        Expression::SequenceExpression(s) => for e in &s.expressions { visit_functions_expr(e, f); },
        Expression::AssignmentExpression(a) => visit_functions_expr(&a.right, f),
        Expression::ParenthesizedExpression(p) => visit_functions_expr(&p.expression, f),
        Expression::ArrayExpression(arr) => for el in &arr.elements {
            if !matches!(el, ArrayExpressionElement::SpreadElement(_) | ArrayExpressionElement::Elision(_)) {
                visit_functions_expr(el.to_expression(), f);
            }
        },
        _ => {}
    }
}

fn classify_value_decoder(cases: &oxc_allocator::Vec<SwitchCase>) -> Option<(ValueMarkers, u8, u8)> {
    if cases.len() < 5 { return None; }
    let mut markers = ValueMarkers::default();
    let mut xor_key_ascii = 0u8;
    let mut xor_key_utf8 = 0u8;
    let mut classified = 0;
    for case in cases {
        let test_val = match &case.test {
            Some(Expression::NumericLiteral(n)) => n.value as i64,
            _ => continue,
        };
        let kind = classify_case(&case.consequent);
        match kind {
            CaseKind::ReturnTrue => { markers.r#true = Some(test_val as u8); classified += 1; }
            CaseKind::ReturnFalse => { markers.r#false = Some(test_val as u8); classified += 1; }
            CaseKind::ReturnNull => { markers.null = Some(test_val as u8); classified += 1; }
            CaseKind::ReturnUndefined => { markers.undefined = Some(test_val as u8); classified += 1; }
            CaseKind::StringDecode { xor_init } => { markers.string = Some(test_val as u8); xor_key_ascii = xor_init; classified += 1; }
            CaseKind::Utf8Decode { xor_init } => { markers.utf8 = Some(test_val as u8); xor_key_utf8 = xor_init; classified += 1; }
            CaseKind::Float64 => { markers.float64 = Some(test_val as u8); classified += 1; }
            CaseKind::IntN { bytes } => {
                match bytes {
                    1 => markers.int8 = Some(test_val as u8),
                    2 => markers.int16 = Some(test_val as u8),
                    3 => markers.int24 = Some(test_val as u8),
                    4 => markers.int32 = Some(test_val as u8),
                    _ => continue,
                }
                classified += 1;
            }
            CaseKind::Unknown => {}
        }
    }
    if classified >= 5 { Some((markers, xor_key_ascii, xor_key_utf8)) } else { None }
}

#[derive(Debug)]
enum CaseKind {
    ReturnTrue, ReturnFalse, ReturnNull, ReturnUndefined,
    StringDecode { xor_init: u8 },
    Utf8Decode { xor_init: u8 },
    Float64,
    IntN { bytes: u8 },
    Unknown,
}

fn classify_case(stmts: &oxc_allocator::Vec<Statement>) -> CaseKind {
    if stmts.is_empty() { return CaseKind::Unknown; }
    // Unwrap a single BlockStatement wrapper (interstitial wraps some cases as
    // `case N: { ... }` where oxc puts a BlockStatement as the whole consequent).
    if stmts.len() == 1 {
        if let Statement::BlockStatement(b) = &stmts[0] {
            return classify_case(&b.body);
        }
    }
    if stmts.len() == 1 {
        if let Statement::ReturnStatement(rs) = &stmts[0] {
            let k = classify_return_arg(rs.argument.as_ref());
            if !matches!(k, CaseKind::Unknown) { return k; }
            if let Some(arg) = &rs.argument {
                let c = classify_complex_return(arg);
                if !matches!(c, CaseKind::Unknown) { return c; }
            }
        }
    }
    let mut has_for_string = false;
    let mut returns_w = false;
    let mut xor_key: Option<u8> = None;
    // Capture pre-return `var g = NN` (outer xor seed used by the closure passed to the
    // inner decode-function call).
    let mut pre_xor: Option<u8> = None;
    for stmt in stmts {
        if let Statement::VariableDeclaration(vd) = stmt {
            for d in &vd.declarations {
                if let Some(Expression::NumericLiteral(n)) = &d.init {
                    if n.value > 0.0 && n.value < 256.0 && pre_xor.is_none() {
                        pre_xor = Some(n.value as u8);
                    }
                }
            }
        }
        if let Statement::ForStatement(fs) = stmt {
            if scan_for_loop_string(fs, &mut xor_key) { has_for_string = true; }
        }
        if let Statement::ReturnStatement(rs) = stmt {
            if let Some(Expression::Identifier(_)) = &rs.argument { returns_w = true; }
            if let Some(arg) = &rs.argument {
                let mut c = classify_complex_return(arg);
                // If we got Utf8Decode/StringDecode with no xor (xor_init == 0), patch in pre_xor.
                if let CaseKind::Utf8Decode { xor_init: 0 } = &c {
                    if let Some(k) = pre_xor { c = CaseKind::Utf8Decode { xor_init: k }; }
                }
                if let CaseKind::StringDecode { xor_init: 0 } = &c {
                    if let Some(k) = pre_xor { c = CaseKind::StringDecode { xor_init: k }; }
                }
                if !matches!(c, CaseKind::Unknown) { return c; }
            }
        }
    }
    if has_for_string && returns_w {
        return CaseKind::StringDecode { xor_init: xor_key.or(pre_xor).unwrap_or(0) };
    }
    classify_int_chain(stmts)
}

fn scan_for_loop_string(fs: &ForStatement, key_out: &mut Option<u8>) -> bool {
    let mut uses_fcc = false;
    check_stmt_for_fcc(&fs.body, &mut uses_fcc);
    if !uses_fcc { return false; }
    if let Some(init) = &fs.init {
        match init {
            ForStatementInit::SequenceExpression(seq) => {
                for e in &seq.expressions {
                    if let Expression::AssignmentExpression(asn) = e {
                        if let Expression::NumericLiteral(n) = &asn.right {
                            if n.value > 0.0 && n.value < 256.0 && key_out.is_none() {
                                *key_out = Some(n.value as u8);
                            }
                        }
                    }
                }
            }
            ForStatementInit::VariableDeclaration(vd) => {
                for d in &vd.declarations {
                    if let Some(init) = &d.init {
                        if let Expression::NumericLiteral(n) = init {
                            if n.value > 0.0 && n.value < 256.0 && key_out.is_none() {
                                *key_out = Some(n.value as u8);
                            }
                        }
                    }
                }
            }
            _ => {}
        }
    }
    true
}

fn classify_return_arg(arg: Option<&Expression>) -> CaseKind {
    match arg {
        Some(Expression::BooleanLiteral(b)) => if b.value { CaseKind::ReturnTrue } else { CaseKind::ReturnFalse },
        Some(Expression::NumericLiteral(n)) if n.value == 1.0 => CaseKind::ReturnTrue,
        Some(Expression::NumericLiteral(n)) if n.value == 0.0 => CaseKind::ReturnFalse,
        Some(Expression::NullLiteral(_)) => CaseKind::ReturnNull,
        None => CaseKind::ReturnUndefined,
        Some(Expression::Identifier(id)) if id.name.as_str() == "undefined" => CaseKind::ReturnUndefined,
        _ => CaseKind::Unknown,
    }
}

fn classify_complex_return(arg: &Expression) -> CaseKind {
    let Expression::CallExpression(call) = arg else { return CaseKind::Unknown };
    let Expression::FunctionExpression(fe) = &call.callee else { return CaseKind::Unknown };
    let Some(body) = &fe.body else { return CaseKind::Unknown };
    let stmt_count = body.statements.len();
    let has_array_from = body.statements.iter().any(|s| {
        if let Statement::VariableDeclaration(vd) = s {
            for d in &vd.declarations {
                if let Some(Expression::CallExpression(call)) = &d.init {
                    if let Expression::StaticMemberExpression(m) = &call.callee {
                        if m.property.name.as_str() == "from" { return true; }
                    }
                }
            }
        }
        false
    });
    if has_array_from && stmt_count >= 4 { return CaseKind::Float64; }
    let has_string_from_char_code = body_contains_string_from_char_code(body);
    if has_string_from_char_code {
        // Return the kind unconditionally (xor_init = 0 if not found inside the
        // inner body); classify_case patches in `pre_xor` from a preceding
        // `let X = NN;` declaration when the seed lives outside the IIFE.
        let xor = guess_xor_key(body).unwrap_or(0);
        if body_uses_utf8_decode(body) {
            return CaseKind::Utf8Decode { xor_init: xor };
        }
        return CaseKind::StringDecode { xor_init: xor };
    }
    CaseKind::Unknown
}

fn classify_int_chain(stmts: &oxc_allocator::Vec<Statement>) -> CaseKind {
    // The DD value-decoder int markers form a fall-through chain:
    //   case INT32: B |= A() << 24, s -= 8;
    //   case INT24: B |= A() << 16, s -= 8;
    //   case INT16: B |= A() << 8,  s -= 8;
    //   case INT8 : return (B |= A()) << s >> s;
    //
    // Each case has its OWN consequent (oxc puts only that case's statements there;
    // execution falls through to the next case's consequent at runtime). So we
    // classify per-case: a shift of N tells us how many bytes total this entry
    // point reads (24 → 4 bytes = int32, 16 → 3 = int24, 8 → 2 = int16). The
    // terminal case has no shift literal but a `<expr> << s >> s` return.

    if stmts.is_empty() { return CaseKind::Unknown; }

    // Terminal int8 case: single ReturnStatement matching `<expr> << s >> s`.
    if stmts.len() == 1 {
        if let Statement::ReturnStatement(rs) = &stmts[0] {
            if let Some(arg) = &rs.argument {
                if is_terminal_int_return(arg) {
                    return CaseKind::IntN { bytes: 1 };
                }
            }
        }
    }

    // Intermediate case: ExpressionStatement(Sequence(... |= A() << SHIFT, ... -= 8))
    if stmts.len() == 1 {
        if let Statement::ExpressionStatement(es) = &stmts[0] {
            if let Some(shift) = extract_int_chain_shift(&es.expression) {
                let bytes = (shift / 8) + 1;
                if (1..=4).contains(&bytes) {
                    return CaseKind::IntN { bytes };
                }
            }
        }
    }

    CaseKind::Unknown
}

fn is_terminal_int_return(e: &Expression) -> bool {
    // Forms accepted (both compile to the same byte-extracting return):
    //   (1) `(B |= A()) << s >> s`               — captcha (single BinaryExpression)
    //   (2) `B |= A(), B << s >> s`              — interstitial (SequenceExpression)
    let target = match e {
        Expression::SequenceExpression(seq) if seq.expressions.len() >= 2 => {
            // last expression should be the shift-shift pair
            seq.expressions.last().unwrap()
        }
        _ => e,
    };
    is_shift_shift_pair(target)
}

fn is_shift_shift_pair(e: &Expression) -> bool {
    let Expression::BinaryExpression(outer) = e else { return false };
    if !matches!(outer.operator, oxc_syntax::operator::BinaryOperator::ShiftRight) { return false; }
    let Expression::BinaryExpression(inner) = &outer.left else { return false };
    if !matches!(inner.operator, oxc_syntax::operator::BinaryOperator::ShiftLeft) { return false; }
    let (Expression::Identifier(r1), Expression::Identifier(r2)) = (&inner.right, &outer.right) else { return false };
    r1.name.as_str() == r2.name.as_str()
}

fn extract_int_chain_shift(e: &Expression) -> Option<u8> {
    // pattern: SequenceExpression where the first element is `B |= A() << SHIFT`
    let Expression::SequenceExpression(seq) = e else { return None };
    let first = seq.expressions.first()?;
    let Expression::AssignmentExpression(asn) = first else { return None };
    if !matches!(asn.operator, oxc_syntax::operator::AssignmentOperator::BitwiseOR) { return None; }
    let Expression::BinaryExpression(b) = &asn.right else { return None };
    if !matches!(b.operator, oxc_syntax::operator::BinaryOperator::ShiftLeft) { return None; }
    let Expression::NumericLiteral(n) = &b.right else { return None };
    let shift = n.value as i64;
    if shift <= 0 || shift > 24 { return None; }
    Some(shift as u8)
}

fn body_contains_string_from_char_code(body: &FunctionBody) -> bool {
    let mut found = false;
    for s in &body.statements { check_stmt_for_fcc(s, &mut found); }
    found
}

fn check_stmt_for_fcc(s: &Statement, found: &mut bool) {
    if *found { return; }
    match s {
        Statement::ExpressionStatement(es) => check_expr_for_fcc(&es.expression, found),
        Statement::ReturnStatement(rs) => if let Some(a) = &rs.argument { check_expr_for_fcc(a, found); }
        Statement::ForStatement(fs) => check_stmt_for_fcc(&fs.body, found),
        Statement::BlockStatement(b) => for s in &b.body { check_stmt_for_fcc(s, found); }
        Statement::VariableDeclaration(vd) => for d in &vd.declarations {
            if let Some(init) = &d.init { check_expr_for_fcc(init, found); }
        }
        Statement::IfStatement(i) => { check_expr_for_fcc(&i.test, found); check_stmt_for_fcc(&i.consequent, found); if let Some(a) = &i.alternate { check_stmt_for_fcc(a, found); } }
        _ => {}
    }
}

fn check_expr_for_fcc(e: &Expression, found: &mut bool) {
    if *found { return; }
    if let Expression::CallExpression(call) = e {
        if let Expression::StaticMemberExpression(m) = &call.callee {
            if let Expression::Identifier(obj) = &m.object {
                if obj.name.as_str() == "String" && m.property.name.as_str() == "fromCharCode" {
                    *found = true; return;
                }
            }
        }
    }
    match e {
        Expression::BinaryExpression(b) => { check_expr_for_fcc(&b.left, found); check_expr_for_fcc(&b.right, found); }
        Expression::LogicalExpression(l) => { check_expr_for_fcc(&l.left, found); check_expr_for_fcc(&l.right, found); }
        Expression::UnaryExpression(u) => check_expr_for_fcc(&u.argument, found),
        Expression::ConditionalExpression(c) => { check_expr_for_fcc(&c.test, found); check_expr_for_fcc(&c.consequent, found); check_expr_for_fcc(&c.alternate, found); }
        Expression::SequenceExpression(s) => for e in &s.expressions { check_expr_for_fcc(e, found); }
        Expression::CallExpression(c) => { check_expr_for_fcc(&c.callee, found); for a in &c.arguments { if !matches!(a, Argument::SpreadElement(_)) { check_expr_for_fcc(a.to_expression(), found); } } }
        Expression::ParenthesizedExpression(p) => check_expr_for_fcc(&p.expression, found),
        Expression::AssignmentExpression(a) => check_expr_for_fcc(&a.right, found),
        _ => {}
    }
}

fn guess_xor_key(body: &FunctionBody) -> Option<u8> {
    let mut found: Option<u8> = None;
    for s in &body.statements {
        scan_for_xor_init(s, &mut found);
        if found.is_some() { break; }
    }
    found
}

fn scan_for_xor_init(s: &Statement, out: &mut Option<u8>) {
    if out.is_some() { return; }
    match s {
        Statement::ForStatement(fs) => {
            if let Some(init) = &fs.init {
                match init {
                    ForStatementInit::SequenceExpression(seq) => {
                        for e in &seq.expressions { scan_expr_for_xor(e, out); }
                    }
                    ForStatementInit::AssignmentExpression(asn) => {
                        scan_expr_for_xor(&asn.right, out);
                    }
                    _ => {}
                }
            }
            scan_for_xor_init(&fs.body, out);
        }
        Statement::ExpressionStatement(es) => scan_expr_for_xor(&es.expression, out),
        Statement::VariableDeclaration(vd) => for d in &vd.declarations {
            if let Some(init) = &d.init {
                if let Expression::NumericLiteral(n) = init {
                    if n.value > 0.0 && n.value < 256.0 {
                        *out = Some(n.value as u8);
                        return;
                    }
                }
            }
        },
        Statement::BlockStatement(b) => for s in &b.body { scan_for_xor_init(s, out); }
        _ => {}
    }
}

fn scan_expr_for_xor(e: &Expression, out: &mut Option<u8>) {
    if out.is_some() { return; }
    if let Expression::AssignmentExpression(asn) = e {
        if let Expression::NumericLiteral(n) = &asn.right {
            if n.value > 0.0 && n.value < 256.0 {
                *out = Some(n.value as u8);
            }
        }
    }
}

fn body_uses_utf8_decode(body: &FunctionBody) -> bool {
    let mut depth = 0;
    for s in &body.statements {
        depth += count_if_branches(s);
    }
    depth >= 3
}

fn count_if_branches(s: &Statement) -> usize {
    match s {
        Statement::IfStatement(i) => 1 + count_if_branches(&i.consequent) + i.alternate.as_ref().map_or(0, |a| count_if_branches(a)),
        Statement::BlockStatement(b) => b.body.iter().map(count_if_branches).sum(),
        Statement::ForStatement(fs) => count_if_branches(&fs.body),
        _ => 0,
    }
}

// ----- inner VM IIFE: helper detection + opcode handler enumeration ---------

#[derive(Debug, Clone, Default)]
pub struct Helpers {
    // local-name → role-tag (e.g. "B" → "$RU16")
    pub by_name: HashMap<String, String>,
    // outer IIFE params: arr (always A in our corpus) and result-object (varies)
    pub result_obj_name: Option<String>,
    // top-level typeof helper (e.g. function s(A) { ... typeof A; ... }) — referenced
    // from inside the IIFE as a free identifier.
    pub typeof_helper_name: Option<String>,
}

pub fn find_typeof_helper(program: &Program) -> Option<String> {
    // Top-level FunctionDeclaration whose body returns a self-assigned function that uses
    // `typeof Symbol`/`typeof X`. Identified by literal `Symbol` reference in body.
    let mut found: Option<String> = None;
    for s in &program.body {
        if let Statement::FunctionDeclaration(fd) = s {
            let Some(name) = fd.id.as_ref() else { continue };
            let Some(body) = &fd.body else { continue };
            if body.statements.is_empty() { continue }
            let mut has_symbol = false;
            for st in &body.statements {
                contains_symbol_typeof(st, &mut has_symbol);
            }
            if has_symbol {
                found = Some(name.name.as_str().to_string());
                break;
            }
        }
    }
    found
}

fn contains_symbol_typeof(s: &Statement, out: &mut bool) {
    if *out { return; }
    match s {
        Statement::ReturnStatement(rs) => if let Some(a) = &rs.argument { contains_symbol_typeof_expr(a, out); }
        Statement::ExpressionStatement(es) => contains_symbol_typeof_expr(&es.expression, out),
        Statement::BlockStatement(b) => for s in &b.body { contains_symbol_typeof(s, out); }
        _ => {}
    }
}

fn contains_symbol_typeof_expr(e: &Expression, out: &mut bool) {
    if *out { return; }
    if let Expression::Identifier(id) = e {
        if id.name.as_str() == "Symbol" { *out = true; return; }
    }
    match e {
        Expression::AssignmentExpression(a) => contains_symbol_typeof_expr(&a.right, out),
        Expression::SequenceExpression(s) => for e in &s.expressions { contains_symbol_typeof_expr(e, out); }
        Expression::ConditionalExpression(c) => { contains_symbol_typeof_expr(&c.test, out); contains_symbol_typeof_expr(&c.consequent, out); contains_symbol_typeof_expr(&c.alternate, out); }
        Expression::LogicalExpression(l) => { contains_symbol_typeof_expr(&l.left, out); contains_symbol_typeof_expr(&l.right, out); }
        Expression::BinaryExpression(b) => { contains_symbol_typeof_expr(&b.left, out); contains_symbol_typeof_expr(&b.right, out); }
        Expression::UnaryExpression(u) => contains_symbol_typeof_expr(&u.argument, out),
        Expression::FunctionExpression(fe) => if let Some(b) = &fe.body { for s in &b.statements { contains_symbol_typeof(s, out); } }
        _ => {}
    }
}

fn find_inner_vm_iife(program: &Program) -> Option<(SlotRoles, Helpers, Vec<(i64, String)>, Option<String>)> {
    // The IIFE we want is `function (A, e) { /* helpers + init + opcode handlers */ }`
    // wrapped in a CallExpression: `(function(A, e){...})(payload, exports)` — emitted as
    // `!function(A, e) { ... }(...)` by the deob. We locate it by scanning for any
    // FunctionExpression whose body has the unique signature: contains `A[N] = function() { ... }`
    // assignments referencing parameter A's first name.
    let mut found: Option<(SlotRoles, Helpers, Vec<(i64, String)>, Option<String>)> = None;
    visit_function_exprs(program, &mut |fe: &Function| {
        if found.is_some() { return; }
        let Some(body) = &fe.body else { return };
        if fe.params.items.len() < 1 { return; }
        let arr_name = match &fe.params.items[0].pattern {
            BindingPattern::BindingIdentifier(id) => id.name.as_str(),
            _ => return,
        };
        let result_obj_name: Option<String> = if fe.params.items.len() >= 2 {
            match &fe.params.items[1].pattern {
                BindingPattern::BindingIdentifier(id) => Some(id.name.as_str().to_string()),
                _ => None,
            }
        } else { None };
        let mut handler_count = 0usize;
        for s in &body.statements {
            count_array_function_assigns(s, arr_name, &mut handler_count);
        }
        if handler_count < 20 { return; }
        let (slots, mut helpers, handlers, vd_name) = analyze_inner(body, arr_name);
        helpers.result_obj_name = result_obj_name;
        found = Some((slots, helpers, handlers, vd_name));
    });
    found
}

fn visit_function_exprs<'a, F: FnMut(&'a Function<'a>)>(program: &'a Program<'a>, f: &mut F) {
    for s in &program.body { visit_fexprs_stmt(s, f); }
}

fn visit_fexprs_stmt<'a, F: FnMut(&'a Function<'a>)>(s: &'a Statement<'a>, f: &mut F) {
    match s {
        Statement::ExpressionStatement(es) => visit_fexprs_expr(&es.expression, f),
        Statement::VariableDeclaration(vd) => for d in &vd.declarations {
            if let Some(init) = &d.init { visit_fexprs_expr(init, f); }
        },
        Statement::ReturnStatement(r) => if let Some(a) = &r.argument { visit_fexprs_expr(a, f); },
        Statement::BlockStatement(b) => for s in &b.body { visit_fexprs_stmt(s, f); },
        Statement::IfStatement(i) => { visit_fexprs_stmt(&i.consequent, f); if let Some(a) = &i.alternate { visit_fexprs_stmt(a, f); } },
        Statement::ForStatement(fs) => visit_fexprs_stmt(&fs.body, f),
        Statement::WhileStatement(w) => visit_fexprs_stmt(&w.body, f),
        Statement::DoWhileStatement(w) => visit_fexprs_stmt(&w.body, f),
        Statement::TryStatement(t) => {
            for s in &t.block.body { visit_fexprs_stmt(s, f); }
            if let Some(h) = &t.handler { for s in &h.body.body { visit_fexprs_stmt(s, f); } }
            if let Some(fi) = &t.finalizer { for s in &fi.body { visit_fexprs_stmt(s, f); } }
        },
        Statement::SwitchStatement(s) => for c in &s.cases { for st in &c.consequent { visit_fexprs_stmt(st, f); } },
        Statement::FunctionDeclaration(fd) => {
            f(fd);
            if let Some(b) = &fd.body { for s in &b.statements { visit_fexprs_stmt(s, f); } }
        }
        Statement::ThrowStatement(t) => visit_fexprs_expr(&t.argument, f),
        Statement::LabeledStatement(s) => visit_fexprs_stmt(&s.body, f),
        _ => {}
    }
}

fn visit_fexprs_expr<'a, F: FnMut(&'a Function<'a>)>(e: &'a Expression<'a>, f: &mut F) {
    match e {
        Expression::FunctionExpression(fe) => {
            f(fe);
            if let Some(b) = &fe.body { for s in &b.statements { visit_fexprs_stmt(s, f); } }
        }
        Expression::ArrowFunctionExpression(af) => {
            for s in &af.body.statements { visit_fexprs_stmt(s, f); }
        }
        Expression::CallExpression(c) => { visit_fexprs_expr(&c.callee, f); for a in &c.arguments { if !matches!(a, Argument::SpreadElement(_)) { visit_fexprs_expr(a.to_expression(), f); } } }
        Expression::NewExpression(n) => { visit_fexprs_expr(&n.callee, f); for a in &n.arguments { if !matches!(a, Argument::SpreadElement(_)) { visit_fexprs_expr(a.to_expression(), f); } } }
        Expression::BinaryExpression(b) => { visit_fexprs_expr(&b.left, f); visit_fexprs_expr(&b.right, f); }
        Expression::LogicalExpression(l) => { visit_fexprs_expr(&l.left, f); visit_fexprs_expr(&l.right, f); }
        Expression::UnaryExpression(u) => visit_fexprs_expr(&u.argument, f),
        Expression::ConditionalExpression(c) => { visit_fexprs_expr(&c.test, f); visit_fexprs_expr(&c.consequent, f); visit_fexprs_expr(&c.alternate, f); }
        Expression::SequenceExpression(s) => for e in &s.expressions { visit_fexprs_expr(e, f); },
        Expression::AssignmentExpression(a) => visit_fexprs_expr(&a.right, f),
        Expression::ParenthesizedExpression(p) => visit_fexprs_expr(&p.expression, f),
        Expression::ArrayExpression(arr) => for el in &arr.elements {
            if !matches!(el, ArrayExpressionElement::SpreadElement(_) | ArrayExpressionElement::Elision(_)) {
                visit_fexprs_expr(el.to_expression(), f);
            }
        },
        _ => {}
    }
}

fn count_array_function_assigns(s: &Statement, arr: &str, out: &mut usize) {
    match s {
        Statement::ExpressionStatement(es) => count_array_function_in_expr(&es.expression, arr, out),
        Statement::BlockStatement(b) => for s in &b.body { count_array_function_assigns(s, arr, out); }
        _ => {}
    }
}

fn count_array_function_in_expr(e: &Expression, arr: &str, out: &mut usize) {
    match e {
        Expression::SequenceExpression(seq) => for e in &seq.expressions { count_array_function_in_expr(e, arr, out); }
        Expression::AssignmentExpression(asn) => {
            if is_arr_index_target(&asn.left, arr).is_some() && matches!(&asn.right, Expression::FunctionExpression(_)) {
                *out += 1;
            }
            count_array_function_in_expr(&asn.right, arr, out);
        }
        _ => {}
    }
}

fn is_arr_index_target(target: &AssignmentTarget, arr: &str) -> Option<i64> {
    let AssignmentTarget::ComputedMemberExpression(m) = target else { return None };
    if let Expression::Identifier(id) = &m.object {
        if id.name.as_str() != arr { return None; }
        if let Expression::NumericLiteral(n) = &m.expression {
            return Some(n.value as i64);
        }
    }
    None
}

fn analyze_inner(body: &FunctionBody, arr: &str) -> (SlotRoles, Helpers, Vec<(i64, String)>, Option<String>) {
    let mut slots = SlotRoles::default();
    let mut helpers = Helpers { by_name: HashMap::default(), result_obj_name: None, typeof_helper_name: None };
    let mut handlers: Vec<(i64, String)> = Vec::new();
    // also collect: "raw" inits (A[N] = const) for FBP detection
    let mut inits: Vec<(i64, InitVal)> = Vec::new();
    let mut value_decoder_name: Option<String> = None;

    // Pass 1: identify inner FunctionDeclarations by shape, populate slots from helper bodies
    for s in &body.statements {
        if let Statement::FunctionDeclaration(fd) = s {
            let name = fd.id.as_ref().map(|i| i.name.as_str().to_string()).unwrap_or_default();
            if name.is_empty() { continue; }
            classify_helper(fd, arr, &name, &mut slots, &mut helpers, &mut value_decoder_name);
        }
    }

    // Pass 2: collect init assignments + handler assignments from sequence/expression statements
    for s in &body.statements {
        match s {
            Statement::ExpressionStatement(es) => collect_assigns(&es.expression, arr, &mut inits, &mut handlers),
            _ => {}
        }
    }

    // Compute STACK and FBP and EXIT_FLAG from inits
    // - SP and FBP are both initialized to STACK (stack_offset)
    // - EXIT_FLAG is the slot initialized to 0 (along with IP=0, FBC=0)
    // Use SP from helpers to identify STACK as RHS, then FBP as the OTHER slot with same RHS init
    if let Some(sp) = slots.stack_pointer {
        let mut sp_init: Option<i64> = None;
        for (lhs, val) in &inits {
            if *lhs == sp {
                if let InitVal::Num(v) = val { sp_init = Some(*v); }
            }
        }
        if let Some(stack) = sp_init {
            slots.stack_offset = Some(stack);
            for (lhs, val) in &inits {
                if *lhs != sp {
                    if let InitVal::Num(v) = val {
                        if *v == stack && slots.frame_base_pointer.is_none() {
                            slots.frame_base_pointer = Some(*lhs);
                        }
                    }
                }
            }
        }
    }
    // Inits to 0 that aren't IP → FBC or EXIT_FLAG.  IP was already detected from helpers.
    let ip = slots.instruction_pointer;
    let mut zero_inits: Vec<i64> = Vec::new();
    for (lhs, val) in &inits {
        if let InitVal::Num(v) = val {
            if *v == 0 && Some(*lhs) != ip {
                zero_inits.push(*lhs);
            }
        }
    }
    // Heuristic: EXIT_FLAG is the slot zero-initialized that no helper reads as a slot;
    // FBC is the one referenced as `++A[FBC]` or `A[FBC]++` somewhere in handlers.
    // Without doing a second scan, we just mark them tentatively — both will get $-tagged
    // via slots map so even mixing them up doesn't break the hash; the header just shows the wrong
    // pretty role. Use first as FBC, second as EXIT_FLAG (matches the corpus init order).
    if zero_inits.len() >= 2 {
        slots.frame_base_counter = Some(zero_inits[0]);
        slots.exit_flag = Some(zero_inits[1]);
    } else if zero_inits.len() == 1 {
        slots.frame_base_counter = Some(zero_inits[0]);
    }

    // Detect specials_base: scan handler texts for `A[N - <ident>]` where N is a fixed
    // numeric literal. The most common such N across handlers is the specials_base.
    let mut specials_count: HashMap<i64, usize> = HashMap::default();
    for (_, body_text) in &handlers {
        scan_specials_base(body_text, &mut specials_count);
    }
    slots.specials_base = specials_count.into_iter().max_by_key(|&(_, c)| c).map(|(k, _)| k);

    (slots, helpers, handlers, value_decoder_name)
}

fn scan_specials_base(text: &str, out: &mut HashMap<i64, usize>) {
    // Look for substrings of the form `A[<digits> - ` (note: ` - ` with spaces — codegen
    // emits this consistently for binary subtraction with a literal LHS).
    let bytes = text.as_bytes();
    let mut i = 0usize;
    while i + 4 < bytes.len() {
        if bytes[i] == b'A' && bytes[i + 1] == b'[' {
            let mut j = i + 2;
            while j < bytes.len() && (bytes[j] as char).is_ascii_digit() { j += 1; }
            if j > i + 2 && j + 3 < bytes.len() && &bytes[j..j + 3] == b" - " {
                if let Ok(n) = text[i + 2..j].parse::<i64>() {
                    if n > 100 {
                        *out.entry(n).or_insert(0) += 1;
                    }
                }
            }
        }
        i += 1;
    }
}

#[derive(Debug, Clone)]
enum InitVal {
    Num(i64),
    Other,
}

fn collect_assigns(e: &Expression, arr: &str, inits: &mut Vec<(i64, InitVal)>, handlers: &mut Vec<(i64, String)>) {
    match e {
        Expression::SequenceExpression(seq) => for e in &seq.expressions { collect_assigns(e, arr, inits, handlers); }
        Expression::AssignmentExpression(asn) => {
            if let Some(idx) = is_arr_index_target(&asn.left, arr) {
                match &asn.right {
                    Expression::FunctionExpression(_) => {
                        // Print the function expression body (excluding the surrounding "function () {")
                        let s = print_expression(&asn.right);
                        handlers.push((idx, s));
                    }
                    Expression::NumericLiteral(n) => inits.push((idx, InitVal::Num(n.value as i64))),
                    Expression::UnaryExpression(u) if u.operator == oxc_syntax::operator::UnaryOperator::Void => {
                        inits.push((idx, InitVal::Other));
                    }
                    _ => inits.push((idx, InitVal::Other)),
                }
            }
        }
        _ => {}
    }
}

fn print_expression(e: &Expression) -> String {
    let mut cg = Codegen::default();
    cg.print_expression(e);
    cg.into_source_text()
}

fn strip_function_wrapper(s: &str) -> String {
    // Input is something like `function () { ... }` or `function() { ... }`. Strip the
    // outer `function (...)` header and the surrounding braces.
    let s = s.trim();
    let body_start = s.find('{').map(|i| i + 1).unwrap_or(0);
    let body_end = s.rfind('}').unwrap_or(s.len());
    if body_end > body_start {
        s[body_start..body_end].trim().to_string()
    } else {
        s.to_string()
    }
}

/// Re-parse a function-body source string, run AST canonicalization passes
/// (sequence/var-decl/compound-op flattening + if/||/?: unification), and
/// return the codegen output of the canonicalized statements as a single
/// string. The same routine runs on both labeled and deob handler bodies.
pub fn canonicalize_handler_body_source(body_src: &str) -> String {
    let alloc = Allocator::default();
    let wrapped = format!("function __probe() {{ {} }}", body_src);
    let ret = Parser::new(&alloc, &wrapped, SourceType::default()).parse();
    if !ret.errors.is_empty() {
        return body_src.to_string();
    }
    let mut program = ret.program;
    let mut v = Canonicalize { alloc: &alloc };
    v.visit_program(&mut program);
    let cg = Codegen::default().build(&program);
    let s = cg.code;
    // Strip leading `function __probe() {` and trailing `}`
    let s = s.trim();
    if let Some(start) = s.find('{') {
        if let Some(end) = s.rfind('}') {
            if end > start {
                return s[start + 1..end].trim().to_string();
            }
        }
    }
    s.to_string()
}

struct Canonicalize<'a> { alloc: &'a Allocator }

fn unwrap_single_stmt_block<'a>(s: &mut Statement<'a>, alloc: &'a oxc_allocator::Allocator) {
    let should_unwrap = if let Statement::BlockStatement(b) = s { b.body.len() == 1 } else { false };
    if !should_unwrap { return; }
    let ast = oxc_ast::AstBuilder::new(alloc);
    let take = std::mem::replace(s, ast.statement_empty(SPAN));
    if let Statement::BlockStatement(b) = take {
        let mut block = b.unbox();
        *s = block.body.pop().unwrap();
    }
}

fn collect_idents(e: &Expression, out: &mut std::collections::HashSet<String>) {
    match e {
        Expression::Identifier(id) => { out.insert(id.name.as_str().to_string()); }
        Expression::BinaryExpression(b) => { collect_idents(&b.left, out); collect_idents(&b.right, out); }
        Expression::LogicalExpression(b) => { collect_idents(&b.left, out); collect_idents(&b.right, out); }
        Expression::UnaryExpression(u) => collect_idents(&u.argument, out),
        Expression::UpdateExpression(u) => {
            if let oxc_ast::ast::SimpleAssignmentTarget::AssignmentTargetIdentifier(id) = &u.argument {
                out.insert(id.name.as_str().to_string());
            }
        }
        Expression::ComputedMemberExpression(m) => { collect_idents(&m.object, out); collect_idents(&m.expression, out); }
        Expression::StaticMemberExpression(m) => collect_idents(&m.object, out),
        Expression::CallExpression(c) => { collect_idents(&c.callee, out); for a in &c.arguments { if !matches!(a, Argument::SpreadElement(_)) { collect_idents(a.to_expression(), out); } } }
        Expression::ConditionalExpression(c) => { collect_idents(&c.test, out); collect_idents(&c.consequent, out); collect_idents(&c.alternate, out); }
        Expression::AssignmentExpression(a) => collect_idents(&a.right, out),
        Expression::SequenceExpression(s) => for e in &s.expressions { collect_idents(e, out); }
        Expression::ParenthesizedExpression(p) => collect_idents(&p.expression, out),
        _ => {}
    }
}

fn single_expr_stmt(s: Statement) -> Option<Expression> {
    match s {
        Statement::ExpressionStatement(es) => Some(es.unbox().expression),
        Statement::BlockStatement(b) => {
            let block = b.unbox();
            if block.body.len() == 1 {
                let mut iter = block.body.into_iter();
                if let Some(Statement::ExpressionStatement(es)) = iter.next() {
                    return Some(es.unbox().expression);
                }
            }
            None
        }
        _ => None,
    }
}

impl<'a> VisitMut<'a> for Canonicalize<'a> {
    fn visit_statements(&mut self, stmts: &mut oxc_allocator::Vec<'a, Statement<'a>>) {
        // Recurse into children first so nested blocks get canonicalized.
        for s in stmts.iter_mut() {
            oxc_ast_visit::walk_mut::walk_statement(self, s);
        }
        let ast = AstBuilder::new(self.alloc);
        let mut i = 0;
        while i < stmts.len() {
            // Convert `if (X) Y;` (no else, single-stmt cons) → `X && Y;` and
            // `if (!X) Y;` → `X || Y;` and `if (X) Y; else Z;` → `X ? Y : Z;`.
            // Match the COMPACT form as the canonical form so both labeled (verbose)
            // and deob (compact) end up here.
            if matches!(&stmts[i], Statement::IfStatement(_)) {
                let take = std::mem::replace(&mut stmts[i], ast.statement_empty(SPAN));
                let Statement::IfStatement(boxed) = take else { unreachable!() };
                let if_stmt = boxed.unbox();
                // Determine compaction shape based on body sizes WITHOUT moving anything yet.
                let cons_compactable = matches!(&if_stmt.consequent, Statement::ExpressionStatement(_))
                    || matches!(&if_stmt.consequent, Statement::BlockStatement(b) if b.body.len() == 1 && matches!(&b.body[0], Statement::ExpressionStatement(_)));
                let alt_compactable = match &if_stmt.alternate {
                    Some(Statement::ExpressionStatement(_)) => true,
                    Some(Statement::BlockStatement(b)) => b.body.len() == 1 && matches!(&b.body[0], Statement::ExpressionStatement(_)),
                    Some(_) => false,
                    None => true, // no else is fine
                };
                let has_else = if_stmt.alternate.is_some();
                if cons_compactable && alt_compactable {
                    let test = if_stmt.test;
                    let cons_e = single_expr_stmt(if_stmt.consequent).unwrap();
                    if has_else {
                        let alt_e = single_expr_stmt(if_stmt.alternate.unwrap()).unwrap();
                        let cond = ast.expression_conditional(SPAN, test, cons_e, alt_e);
                        stmts[i] = ast.statement_expression(SPAN, cond);
                    } else {
                        let (lhs, op) = match test {
                            Expression::UnaryExpression(u) if u.operator == oxc_syntax::operator::UnaryOperator::LogicalNot => {
                                let inner = u.unbox().argument;
                                (inner, oxc_syntax::operator::LogicalOperator::Or)
                            }
                            other => (other, oxc_syntax::operator::LogicalOperator::And),
                        };
                        let logic = ast.expression_logical(SPAN, lhs, op, cons_e);
                        stmts[i] = ast.statement_expression(SPAN, logic);
                    }
                    i += 1;
                    continue;
                } else {
                    let alloc_if = oxc_allocator::Box::new_in(if_stmt, self.alloc);
                    stmts[i] = Statement::IfStatement(alloc_if);
                }
            }
            // Flatten ExpressionStatement(SequenceExpression(...)) into separate statements.
            if let Statement::ExpressionStatement(es) = &mut stmts[i] {
                if let Expression::SequenceExpression(_) = &es.expression {
                    let take = std::mem::replace(&mut stmts[i], ast.statement_empty(SPAN));
                    let Statement::ExpressionStatement(boxed) = take else { unreachable!() };
                    let stmt = boxed.unbox();
                    let Expression::SequenceExpression(seq_box) = stmt.expression else { unreachable!() };
                    let seq = seq_box.unbox();
                    stmts.remove(i);
                    let mut idx = i;
                    for e in seq.expressions {
                        let new_es = ast.statement_expression(SPAN, e);
                        stmts.insert(idx, new_es);
                        idx += 1;
                    }
                    continue;
                }
            }
            // Normalize variable-decl kind to `var` (so let/const all hash the same).
            if let Statement::VariableDeclaration(vd) = &mut stmts[i] {
                vd.kind = oxc_ast::ast::VariableDeclarationKind::Var;
            }
            // Flatten VariableDeclaration with multiple declarators into separate decls.
            if let Statement::VariableDeclaration(vd) = &mut stmts[i] {
                if vd.declarations.len() > 1 {
                    let take = std::mem::replace(&mut stmts[i], ast.statement_empty(SPAN));
                    let Statement::VariableDeclaration(boxed) = take else { unreachable!() };
                    let vd = boxed.unbox();
                    let kind = vd.kind;
                    stmts.remove(i);
                    let mut idx = i;
                    for d in vd.declarations {
                        let mut declarators = oxc_allocator::Vec::with_capacity_in(1, self.alloc);
                        declarators.push(d);
                        let new_vd = ast.alloc_variable_declaration(SPAN, kind, declarators, false);
                        stmts.insert(idx, Statement::VariableDeclaration(new_vd));
                        idx += 1;
                    }
                    continue;
                }
            }
            // Hoist for-init declarators that aren't referenced in test/update OUT of the
            // for-init into preceding var statements. Both labeled and deob produce
            // semantically-equivalent code; this rule canonicalizes to "minimal for-init".
            if let Statement::ForStatement(fs) = &mut stmts[i] {
                if let Some(ForStatementInit::VariableDeclaration(vd)) = &mut fs.init {
                    vd.kind = oxc_ast::ast::VariableDeclarationKind::Var;
                }
                if let Some(ForStatementInit::VariableDeclaration(vd)) = &fs.init {
                    if vd.declarations.len() > 1 {
                        // Determine which declarator names are referenced by test/update.
                        let mut keep_names: std::collections::HashSet<String> = std::collections::HashSet::new();
                        if let Some(test) = &fs.test { collect_idents(test, &mut keep_names); }
                        if let Some(update) = &fs.update { collect_idents(update, &mut keep_names); }
                        // Build hoist list = declarators whose name isn't referenced.
                        // Take ownership of the for stmt to mutate it.
                        let take = std::mem::replace(&mut stmts[i], ast.statement_empty(SPAN));
                        let Statement::ForStatement(fs_box) = take else { unreachable!() };
                        let mut fs_owned = fs_box.unbox();
                        let init = fs_owned.init.take().unwrap();
                        let ForStatementInit::VariableDeclaration(vd_box) = init else { unreachable!() };
                        let vd_owned = vd_box.unbox();
                        let kind = vd_owned.kind;
                        let mut hoist: Vec<VariableDeclarator> = Vec::new();
                        let mut keep: Vec<VariableDeclarator> = Vec::new();
                        for d in vd_owned.declarations {
                            let name = match &d.id {
                                BindingPattern::BindingIdentifier(id) => Some(id.name.as_str().to_string()),
                                _ => None,
                            };
                            let referenced = name.as_ref().map_or(false, |n| keep_names.contains(n));
                            if referenced || keep.is_empty() && hoist.is_empty() {
                                // Keep at least one declarator in the for-init by default; only
                                // keep referenced ones when we know which are referenced.
                                if referenced { keep.push(d); } else { hoist.push(d); }
                            } else if referenced {
                                keep.push(d);
                            } else {
                                hoist.push(d);
                            }
                        }
                        // If keep is empty (no declarator is referenced — unusual), put all back.
                        if keep.is_empty() {
                            keep = hoist;
                            hoist = Vec::new();
                        }
                        // Re-assemble for-init from `keep`.
                        let mut keep_alloc = oxc_allocator::Vec::with_capacity_in(keep.len(), self.alloc);
                        for d in keep { keep_alloc.push(d); }
                        let new_vd = ast.alloc_variable_declaration(SPAN, kind, keep_alloc, false);
                        fs_owned.init = Some(ForStatementInit::VariableDeclaration(new_vd));
                        let fs_alloc = oxc_allocator::Box::new_in(fs_owned, self.alloc);
                        stmts[i] = Statement::ForStatement(fs_alloc);
                        // Hoist the unrefer'd declarators OUT as var statements before the for.
                        let mut idx = i;
                        for d in hoist {
                            let mut declarators = oxc_allocator::Vec::with_capacity_in(1, self.alloc);
                            declarators.push(d);
                            let new_vd = ast.alloc_variable_declaration(SPAN, kind, declarators, false);
                            stmts.insert(idx, Statement::VariableDeclaration(new_vd));
                            idx += 1;
                        }
                        i = idx + 1;
                        continue;
                    }
                }
            }
            i += 1;
        }
        stmts.retain(|s| !matches!(s, Statement::EmptyStatement(_)));
    }

    fn visit_for_statement(&mut self, fs: &mut ForStatement<'a>) {
        oxc_ast_visit::walk_mut::walk_for_statement(self, fs);
        unwrap_single_stmt_block(&mut fs.body, self.alloc);
    }

    fn visit_for_in_statement(&mut self, fs: &mut ForInStatement<'a>) {
        oxc_ast_visit::walk_mut::walk_for_in_statement(self, fs);
        unwrap_single_stmt_block(&mut fs.body, self.alloc);
    }

    fn visit_for_of_statement(&mut self, fs: &mut ForOfStatement<'a>) {
        oxc_ast_visit::walk_mut::walk_for_of_statement(self, fs);
        unwrap_single_stmt_block(&mut fs.body, self.alloc);
    }

    fn visit_while_statement(&mut self, ws: &mut WhileStatement<'a>) {
        oxc_ast_visit::walk_mut::walk_while_statement(self, ws);
        unwrap_single_stmt_block(&mut ws.body, self.alloc);
    }

    fn visit_do_while_statement(&mut self, ws: &mut DoWhileStatement<'a>) {
        oxc_ast_visit::walk_mut::walk_do_while_statement(self, ws);
        unwrap_single_stmt_block(&mut ws.body, self.alloc);
    }

    fn visit_expression(&mut self, expr: &mut Expression<'a>) {
        oxc_ast_visit::walk_mut::walk_expression(self, expr);
        // Collapse the `(0, X)` indirect-call pattern → `X`. This appears in deob output as
        // `(0, A[--A[$SP]])(arg)` to suppress `this` binding; semantically identical to `X(arg)`.
        if let Expression::SequenceExpression(seq) = expr {
            if seq.expressions.len() == 2 {
                if let Expression::NumericLiteral(n) = &seq.expressions[0] {
                    if n.value == 0.0 {
                        let ast = AstBuilder::new(self.alloc);
                        let mut taken = std::mem::replace(&mut seq.expressions[1], ast.expression_null_literal(SPAN));
                        std::mem::swap(expr, &mut taken);
                        return;
                    }
                }
            }
        }
        if let Expression::AssignmentExpression(asn) = expr {
            use oxc_syntax::operator::AssignmentOperator as AO;
            use oxc_syntax::operator::BinaryOperator as BO;
            let bin_op: Option<BO> = match asn.operator {
                AO::Addition => Some(BO::Addition),
                AO::Subtraction => Some(BO::Subtraction),
                AO::Multiplication => Some(BO::Multiplication),
                AO::Division => Some(BO::Division),
                AO::Remainder => Some(BO::Remainder),
                AO::ShiftLeft => Some(BO::ShiftLeft),
                AO::ShiftRight => Some(BO::ShiftRight),
                AO::ShiftRightZeroFill => Some(BO::ShiftRightZeroFill),
                AO::BitwiseOR => Some(BO::BitwiseOR),
                AO::BitwiseXOR => Some(BO::BitwiseXOR),
                AO::BitwiseAnd => Some(BO::BitwiseAnd),
                AO::Exponential => Some(BO::Exponential),
                _ => None,
            };
            if let Some(bop) = bin_op {
                if let Some(left_expr) = clone_assignment_target_as_expr(&asn.left, self.alloc) {
                    let ast = AstBuilder::new(self.alloc);
                    let right = std::mem::replace(&mut asn.right, ast.expression_null_literal(SPAN));
                    let new_right = ast.expression_binary(SPAN, left_expr, bop, right);
                    asn.right = new_right;
                    asn.operator = AO::Assign;
                }
            }
        }
    }
}

fn clone_assignment_target_as_expr<'a>(t: &AssignmentTarget<'a>, alloc: &'a Allocator) -> Option<Expression<'a>> {
    use oxc_ast::ast::AssignmentTarget as AT;
    let ast = AstBuilder::new(alloc);
    match t {
        AT::AssignmentTargetIdentifier(id) => Some(ast.expression_identifier(SPAN, id.name.clone())),
        AT::ComputedMemberExpression(m) => {
            let obj = clone_expression(&m.object, alloc)?;
            let idx = clone_expression(&m.expression, alloc)?;
            let boxed = ast.alloc_computed_member_expression(SPAN, obj, idx, m.optional);
            Some(Expression::ComputedMemberExpression(boxed))
        }
        AT::StaticMemberExpression(m) => {
            let obj = clone_expression(&m.object, alloc)?;
            let boxed = ast.alloc_static_member_expression(SPAN, obj, m.property.clone(), m.optional);
            Some(Expression::StaticMemberExpression(boxed))
        }
        _ => None,
    }
}

fn clone_expression<'a>(e: &Expression<'a>, alloc: &'a Allocator) -> Option<Expression<'a>> {
    let ast = AstBuilder::new(alloc);
    match e {
        Expression::Identifier(id) => Some(ast.expression_identifier(SPAN, id.name.clone())),
        Expression::NumericLiteral(n) => Some(ast.expression_numeric_literal(SPAN, n.value, n.raw.clone(), n.base)),
        Expression::StringLiteral(s) => Some(ast.expression_string_literal(SPAN, s.value.clone(), s.raw.clone())),
        Expression::BooleanLiteral(b) => Some(ast.expression_boolean_literal(SPAN, b.value)),
        Expression::NullLiteral(_) => Some(ast.expression_null_literal(SPAN)),
        Expression::ComputedMemberExpression(m) => {
            let obj = clone_expression(&m.object, alloc)?;
            let idx = clone_expression(&m.expression, alloc)?;
            let boxed = ast.alloc_computed_member_expression(SPAN, obj, idx, m.optional);
            Some(Expression::ComputedMemberExpression(boxed))
        }
        Expression::StaticMemberExpression(m) => {
            let obj = clone_expression(&m.object, alloc)?;
            let boxed = ast.alloc_static_member_expression(SPAN, obj, m.property.clone(), m.optional);
            Some(Expression::StaticMemberExpression(boxed))
        }
        Expression::BinaryExpression(b) => {
            let l = clone_expression(&b.left, alloc)?;
            let r = clone_expression(&b.right, alloc)?;
            Some(ast.expression_binary(SPAN, l, b.operator, r))
        }
        Expression::UnaryExpression(u) => {
            let arg = clone_expression(&u.argument, alloc)?;
            Some(ast.expression_unary(SPAN, u.operator, arg))
        }
        _ => None,
    }
}

fn classify_helper(
    fd: &Function,
    arr: &str,
    name: &str,
    slots: &mut SlotRoles,
    helpers: &mut Helpers,
    value_decoder_name: &mut Option<String>,
) {
    let Some(body) = &fd.body else { return };
    let stmts = &body.statements;

    // readUint8: `function (){ return A[X + A[Y]++]; }` (single statement)
    if stmts.len() == 1 && fd.params.items.is_empty() {
        if let Statement::ReturnStatement(rs) = &stmts[0] {
            if let Some(arg) = &rs.argument {
                if let Some((vms, ip)) = match_read_u8_expr(arg, arr) {
                    slots.vm_start = slots.vm_start.or(Some(vms));
                    slots.instruction_pointer = slots.instruction_pointer.or(Some(ip));
                    helpers.by_name.insert(name.to_string(), "$RU8".into());
                    return;
                }
            }
        }
    }

    // readUint16 (any compaction shape): need both IP-plus-2 assignment and `<<8|` return.
    if fd.params.items.is_empty() && stmts.len() >= 2 {
        if let Some((vms, ip)) = match_read_u16(stmts, arr) {
            slots.vm_start = slots.vm_start.or(Some(vms));
            slots.instruction_pointer = slots.instruction_pointer.or(Some(ip));
            helpers.by_name.insert(name.to_string(), "$RU16".into());
            return;
        }
    }

    // register2stack(Q): A[A[SP]++] = A[STACK + Q]
    if fd.params.items.len() == 1 && stmts.len() == 1 {
        if let Statement::ExpressionStatement(es) = &stmts[0] {
            if let Some((sp, stack)) = match_register2stack(&es.expression, arr) {
                slots.stack_pointer = slots.stack_pointer.or(Some(sp));
                slots.stack_offset = slots.stack_offset.or(Some(stack));
                helpers.by_name.insert(name.to_string(), "$R2S".into());
                return;
            }
            if let Some((sp, stack)) = match_stack2register(&es.expression, arr) {
                slots.stack_pointer = slots.stack_pointer.or(Some(sp));
                slots.stack_offset = slots.stack_offset.or(Some(stack));
                helpers.by_name.insert(name.to_string(), "$S2R".into());
                return;
            }
        }
    }

    // storeToLastResult: 1 statement: A[LR] = A[--A[SP]]
    if fd.params.items.is_empty() && stmts.len() == 1 {
        if let Statement::ExpressionStatement(es) = &stmts[0] {
            if let Some((lr, sp)) = match_store_lr(&es.expression, arr) {
                slots.last_result = slots.last_result.or(Some(lr));
                slots.stack_pointer = slots.stack_pointer.or(Some(sp));
                helpers.by_name.insert(name.to_string(), "$SLR".into());
                return;
            }
        }
    }

    // GET: var Q = A[--A[SP]]; [var B = A[A[SP] - 1];] A[A[SP] - 1] = B[Q];
    // Accept both fully-expanded (3 stmts) and multi-decl-compacted (2 stmts) forms.
    if fd.params.items.is_empty() && (stmts.len() == 2 || stmts.len() == 3) {
        if let Some(sp) = match_get_helper(stmts, arr) {
            slots.stack_pointer = slots.stack_pointer.or(Some(sp));
            helpers.by_name.insert(name.to_string(), "$GET".into());
            return;
        }
    }

    // SET: 2 statements: var Q = A[--A[SP]]; A[--A[SP]][Q] = A[A[SP] - 1];
    if fd.params.items.is_empty() && stmts.len() == 2 {
        if let Some(sp) = match_set_helper(stmts, arr) {
            slots.stack_pointer = slots.stack_pointer.or(Some(sp));
            helpers.by_name.insert(name.to_string(), "$SET".into());
            return;
        }
    }

    // getVal: 1 statement: return <ident>(function(){ return A[VMS + A[IP]++]; });
    if fd.params.items.is_empty() && stmts.len() == 1 {
        if let Statement::ReturnStatement(rs) = &stmts[0] {
            if let Some(arg) = &rs.argument {
                if let Some(vd) = match_get_val(arg, arr) {
                    helpers.by_name.insert(name.to_string(), "$RVAL".into());
                    if value_decoder_name.is_none() { *value_decoder_name = Some(vd); }
                    return;
                }
            }
        }
    }

    // fetch: 5 statements: var Q = A[IP]; var B = A[VMS+Q]; A[IP] = Q+1; var E = A[BASE+B]; A[CH] = E, A[CI] = B;
    if fd.params.items.is_empty() && stmts.len() >= 4 {
        if let Some((base, ch, ci)) = match_fetch(stmts, arr) {
            slots.dispatch_base = slots.dispatch_base.or(Some(base));
            slots.current_opcode_handler = slots.current_opcode_handler.or(Some(ch));
            slots.current_opcode_id = slots.current_opcode_id.or(Some(ci));
            helpers.by_name.insert(name.to_string(), "$FETCH".into());
            return;
        }
    }
}

fn match_read_u8_expr(e: &Expression, arr: &str) -> Option<(i64, i64)> {
    // A[VMS + A[IP]++]
    let Expression::ComputedMemberExpression(m) = e else { return None };
    if !is_ident(&m.object, arr) { return None; }
    let Expression::BinaryExpression(b) = &m.expression else { return None };
    if !matches!(b.operator, oxc_syntax::operator::BinaryOperator::Addition) { return None; }
    let vms = num_lit(&b.left)?;
    // right: A[IP]++
    let Expression::UpdateExpression(u) = &b.right else { return None };
    if u.operator != oxc_syntax::operator::UpdateOperator::Increment { return None; }
    if u.prefix { return None; }
    let oxc_ast::ast::SimpleAssignmentTarget::ComputedMemberExpression(mm) = &u.argument else { return None };
    if !is_ident(&mm.object, arr) { return None; }
    let ip = num_lit(&mm.expression)?;
    Some((vms, ip))
}

fn match_read_u16(stmts: &oxc_allocator::Vec<Statement>, arr: &str) -> Option<(i64, i64)> {
    // 1st var Q = A[IP]; some var = A[VMS + Q]; some var = A[VMS + Q + 1];
    // assignment A[IP] = Q + 2; return ... << 8 | ...
    let mut ip: Option<i64> = None;
    let mut vms: Option<i64> = None;
    let mut has_ip_assign_plus2 = false;
    let mut has_shift_return = false;
    for s in stmts.iter() {
        if let Statement::VariableDeclaration(vd) = s {
            for d in &vd.declarations {
                let Some(init) = &d.init else { continue };
                let Expression::ComputedMemberExpression(m) = init else { continue };
                if !is_ident(&m.object, arr) { continue; }
                if let Some(v) = num_lit(&m.expression) {
                    if ip.is_none() { ip = Some(v); }
                    continue;
                }
                if let Expression::BinaryExpression(b) = &m.expression {
                    if matches!(b.operator, oxc_syntax::operator::BinaryOperator::Addition) {
                        if let Some(v) = num_lit(&b.left) { vms = Some(v); }
                    }
                }
            }
        }
        // detect A[IP] = Q + 2 OR A[IP] += 2 in a sequence/assignment
        if let Statement::ExpressionStatement(es) = s {
            check_ip_plus2(&es.expression, arr, &mut has_ip_assign_plus2);
        }
        // detect return [...,] X << 8 | Y. The deob compacts the IP-plus-2 + return into a
        // single SequenceExpression argument, so scan the return arg for both checks.
        if let Statement::ReturnStatement(rs) = s {
            if let Some(arg) = &rs.argument {
                if expr_is_shift_or(arg) { has_shift_return = true; }
                if let Expression::SequenceExpression(seq) = arg {
                    for e in &seq.expressions {
                        check_ip_plus2(e, arr, &mut has_ip_assign_plus2);
                        if expr_is_shift_or(e) { has_shift_return = true; }
                    }
                }
            }
        }
    }
    if !has_ip_assign_plus2 || !has_shift_return { return None; }
    if let (Some(i), Some(v)) = (ip, vms) { Some((v, i)) } else { None }
}

fn check_ip_plus2(e: &Expression, arr: &str, out: &mut bool) {
    match e {
        Expression::SequenceExpression(seq) => for e in &seq.expressions { check_ip_plus2(e, arr, out); },
        Expression::AssignmentExpression(asn) => {
            if let AssignmentTarget::ComputedMemberExpression(t) = &asn.left {
                if is_ident(&t.object, arr) && num_lit(&t.expression).is_some() {
                    if asn.operator == oxc_syntax::operator::AssignmentOperator::Assign {
                        // RHS must be `<ident> + 2`
                        if let Expression::BinaryExpression(b) = &asn.right {
                            if matches!(b.operator, oxc_syntax::operator::BinaryOperator::Addition) {
                                if let Some(n) = num_lit(&b.right) {
                                    if n == 2 { *out = true; }
                                }
                            }
                        }
                    } else if asn.operator == oxc_syntax::operator::AssignmentOperator::Addition {
                        if let Some(n) = num_lit(&asn.right) {
                            if n == 2 { *out = true; }
                        }
                    }
                }
            }
        }
        _ => {}
    }
}

fn expr_is_shift_or(e: &Expression) -> bool {
    if let Expression::BinaryExpression(b) = e {
        if matches!(b.operator, oxc_syntax::operator::BinaryOperator::BitwiseOR) {
            // either side is `X << 8`
            if let Expression::BinaryExpression(l) = &b.left {
                if matches!(l.operator, oxc_syntax::operator::BinaryOperator::ShiftLeft) {
                    if let Some(n) = num_lit(&l.right) { return n == 8; }
                }
            }
            if let Expression::BinaryExpression(r) = &b.right {
                if matches!(r.operator, oxc_syntax::operator::BinaryOperator::ShiftLeft) {
                    if let Some(n) = num_lit(&r.right) { return n == 8; }
                }
            }
        }
    }
    false
}

fn match_register2stack(e: &Expression, arr: &str) -> Option<(i64, i64)> {
    // A[A[SP]++] = A[STACK + Q]
    let Expression::AssignmentExpression(asn) = e else { return None };
    if asn.operator != oxc_syntax::operator::AssignmentOperator::Assign { return None; }
    let AssignmentTarget::ComputedMemberExpression(target) = &asn.left else { return None };
    if !is_ident(&target.object, arr) { return None; }
    let sp = match_sp_postinc(&target.expression, arr)?;
    let Expression::ComputedMemberExpression(rhs) = &asn.right else { return None };
    if !is_ident(&rhs.object, arr) { return None; }
    let Expression::BinaryExpression(b) = &rhs.expression else { return None };
    if !matches!(b.operator, oxc_syntax::operator::BinaryOperator::Addition) { return None; }
    let stack = num_lit(&b.left)?;
    Some((sp, stack))
}

fn match_stack2register(e: &Expression, arr: &str) -> Option<(i64, i64)> {
    // A[STACK + Q] = A[A[SP] - 1]
    let Expression::AssignmentExpression(asn) = e else { return None };
    if asn.operator != oxc_syntax::operator::AssignmentOperator::Assign { return None; }
    let AssignmentTarget::ComputedMemberExpression(target) = &asn.left else { return None };
    if !is_ident(&target.object, arr) { return None; }
    let Expression::BinaryExpression(b) = &target.expression else { return None };
    if !matches!(b.operator, oxc_syntax::operator::BinaryOperator::Addition) { return None; }
    let stack = num_lit(&b.left)?;
    let Expression::ComputedMemberExpression(rhs) = &asn.right else { return None };
    if !is_ident(&rhs.object, arr) { return None; }
    let Expression::BinaryExpression(b2) = &rhs.expression else { return None };
    if !matches!(b2.operator, oxc_syntax::operator::BinaryOperator::Subtraction) { return None; }
    let sp = arr_index_of_arr(&b2.left, arr)?;
    Some((sp, stack))
}

fn match_sp_postinc(e: &Expression, arr: &str) -> Option<i64> {
    // A[SP]++
    let Expression::UpdateExpression(u) = e else { return None };
    if u.operator != oxc_syntax::operator::UpdateOperator::Increment { return None; }
    if u.prefix { return None; }
    let oxc_ast::ast::SimpleAssignmentTarget::ComputedMemberExpression(m) = &u.argument else { return None };
    if !is_ident(&m.object, arr) { return None; }
    num_lit(&m.expression)
}

fn match_sp_predec(e: &Expression, arr: &str) -> Option<i64> {
    // --A[SP]
    let Expression::UpdateExpression(u) = e else { return None };
    if u.operator != oxc_syntax::operator::UpdateOperator::Decrement { return None; }
    if !u.prefix { return None; }
    let oxc_ast::ast::SimpleAssignmentTarget::ComputedMemberExpression(m) = &u.argument else { return None };
    if !is_ident(&m.object, arr) { return None; }
    num_lit(&m.expression)
}

fn arr_index_of_arr(e: &Expression, arr: &str) -> Option<i64> {
    // A[N] where N is numeric literal
    let Expression::ComputedMemberExpression(m) = e else { return None };
    if !is_ident(&m.object, arr) { return None; }
    num_lit(&m.expression)
}

fn match_store_lr(e: &Expression, arr: &str) -> Option<(i64, i64)> {
    // A[LR] = A[--A[SP]]
    let Expression::AssignmentExpression(asn) = e else { return None };
    if asn.operator != oxc_syntax::operator::AssignmentOperator::Assign { return None; }
    let AssignmentTarget::ComputedMemberExpression(target) = &asn.left else { return None };
    if !is_ident(&target.object, arr) { return None; }
    let lr = num_lit(&target.expression)?;
    let Expression::ComputedMemberExpression(rhs) = &asn.right else { return None };
    if !is_ident(&rhs.object, arr) { return None; }
    let sp = match_sp_predec(&rhs.expression, arr)?;
    Some((lr, sp))
}

fn match_get_helper(stmts: &oxc_allocator::Vec<Statement>, arr: &str) -> Option<i64> {
    if stmts.is_empty() { return None; }
    let mut sp: Option<i64> = None;
    if let Statement::VariableDeclaration(vd) = &stmts[0] {
        for d in &vd.declarations {
            if let Some(Expression::ComputedMemberExpression(m)) = &d.init {
                if is_ident(&m.object, arr) {
                    if let Some(s) = match_sp_predec(&m.expression, arr) { sp = Some(s); }
                }
            }
        }
    }
    sp
}

fn match_set_helper(stmts: &oxc_allocator::Vec<Statement>, arr: &str) -> Option<i64> {
    // var Q = A[--A[SP]]; A[--A[SP]][Q] = A[A[SP] - 1];
    if stmts.len() < 2 { return None; }
    let mut sp: Option<i64> = None;
    if let Statement::VariableDeclaration(vd) = &stmts[0] {
        for d in &vd.declarations {
            if let Some(Expression::ComputedMemberExpression(m)) = &d.init {
                if is_ident(&m.object, arr) {
                    if let Some(s) = match_sp_predec(&m.expression, arr) { sp = Some(s); }
                }
            }
        }
    }
    if sp.is_none() { return None; }
    // verify second statement is A[--A[SP]][Q] = ...
    if let Statement::ExpressionStatement(es) = &stmts[1] {
        if let Expression::AssignmentExpression(asn) = &es.expression {
            if let AssignmentTarget::ComputedMemberExpression(outer) = &asn.left {
                if let Expression::ComputedMemberExpression(inner) = &outer.object {
                    if is_ident(&inner.object, arr) {
                        if match_sp_predec(&inner.expression, arr).is_some() {
                            return sp;
                        }
                    }
                }
            }
        }
    }
    None
}

fn match_get_val(arg: &Expression, arr: &str) -> Option<String> {
    // call to <Identifier>(function() { return A[VMS + A[IP]++]; }) or with an arrow function.
    let Expression::CallExpression(call) = arg else { return None };
    let Expression::Identifier(id) = &call.callee else { return None };
    let name = id.name.as_str().to_string();
    if call.arguments.len() != 1 { return None; }
    match &call.arguments[0] {
        Argument::FunctionExpression(fe) => {
            let Some(body) = &fe.body else { return None };
            if body.statements.len() != 1 { return None; }
            if let Statement::ReturnStatement(rs) = &body.statements[0] {
                if let Some(a) = &rs.argument {
                    if match_read_u8_expr(a, arr).is_some() { return Some(name); }
                }
            }
            None
        }
        Argument::ArrowFunctionExpression(af) => {
            // Expression-body arrow: af.expression is true, the body holds a single
            // ExpressionStatement that codegen would emit as `return X;`.
            if af.expression {
                if let Some(Statement::ExpressionStatement(es)) = af.body.statements.first() {
                    if match_read_u8_expr(&es.expression, arr).is_some() { return Some(name); }
                }
            } else if af.body.statements.len() == 1 {
                if let Statement::ReturnStatement(rs) = &af.body.statements[0] {
                    if let Some(a) = &rs.argument {
                        if match_read_u8_expr(a, arr).is_some() { return Some(name); }
                    }
                }
            }
            None
        }
        _ => None,
    }
}

fn match_fetch(stmts: &oxc_allocator::Vec<Statement>, arr: &str) -> Option<(i64, i64, i64)> {
    // statements include: var Q = A[IP]; var B = A[VMS + Q]; A[IP] = Q + 1; var E = A[BASE + B]; A[CH] = E, A[CI] = B;
    let mut base: Option<i64> = None;
    let mut ch_ci: Option<(i64, i64)> = None;
    for s in stmts {
        match s {
            Statement::VariableDeclaration(vd) => for d in &vd.declarations {
                let Some(init) = &d.init else { continue };
                if let Expression::ComputedMemberExpression(m) = init {
                    if is_ident(&m.object, arr) {
                        if let Expression::BinaryExpression(b) = &m.expression {
                            if matches!(b.operator, oxc_syntax::operator::BinaryOperator::Addition) {
                                if let Some(v) = num_lit(&b.left) {
                                    // distinguish VMS (already known by IP detector) vs BASE
                                    // BASE is whichever doesn't equal vms; but we don't have vms here.
                                    // Take the LAST seen; the fetch helper has BASE in the second var decl.
                                    base = Some(v);
                                }
                            }
                        }
                    }
                }
            },
            Statement::ExpressionStatement(es) => {
                if let Expression::SequenceExpression(seq) = &es.expression {
                    if seq.expressions.len() == 2 {
                        let mut got = (None, None);
                        for (i, e) in seq.expressions.iter().enumerate() {
                            if let Expression::AssignmentExpression(asn) = e {
                                if let AssignmentTarget::ComputedMemberExpression(t) = &asn.left {
                                    if is_ident(&t.object, arr) {
                                        if let Some(slot) = num_lit(&t.expression) {
                                            if i == 0 { got.0 = Some(slot); } else { got.1 = Some(slot); }
                                        }
                                    }
                                }
                            }
                        }
                        if let (Some(a), Some(b)) = got { ch_ci = Some((a, b)); }
                    }
                }
            }
            _ => {}
        }
    }
    if let (Some(b), Some((ch, ci))) = (base, ch_ci) {
        Some((b, ch, ci))
    } else { None }
}

fn is_ident(e: &Expression, name: &str) -> bool {
    match e { Expression::Identifier(id) => id.name.as_str() == name, _ => false }
}

fn num_lit(e: &Expression) -> Option<i64> {
    if let Expression::NumericLiteral(n) = e { Some(n.value as i64) } else { None }
}

// ----- normalization --------------------------------------------------------

pub fn normalize_handler_body(body_text: &str, slots: &SlotRoles, helpers: &Helpers) -> String {
    // body_text from print_expression looks like `function() { ... }`. Strip wrapper to get
    // just the body source, then canonicalize.
    let inner = strip_function_wrapper(body_text);
    let canon = canonicalize_handler_body_source(&inner);
    let mut s = canon;

    // Substitute slot integer-literals with role tags. Order: longest values first
    // so we don't accidentally substitute a substring of a larger number — actually
    // we use word boundaries via digit boundaries (the regex below uses `\d+` matching).
    let mut slot_subs: Vec<(i64, &str)> = Vec::new();
    if let Some(v) = slots.stack_pointer { slot_subs.push((v, "$SP")); }
    if let Some(v) = slots.instruction_pointer { slot_subs.push((v, "$IP")); }
    if let Some(v) = slots.frame_base_pointer { slot_subs.push((v, "$FBP")); }
    if let Some(v) = slots.frame_base_counter { slot_subs.push((v, "$FBC")); }
    if let Some(v) = slots.last_result { slot_subs.push((v, "$LR")); }
    if let Some(v) = slots.exit_flag { slot_subs.push((v, "$EXIT")); }
    if let Some(v) = slots.current_opcode_handler { slot_subs.push((v, "$CH")); }
    if let Some(v) = slots.current_opcode_id { slot_subs.push((v, "$CI")); }
    if let Some(v) = slots.stack_offset { slot_subs.push((v, "$STACK")); }
    if let Some(v) = slots.vm_start { slot_subs.push((v, "$VMS")); }
    if let Some(v) = slots.specials_base { slot_subs.push((v, "$SPEC")); }
    // Sort by string length desc so that 128038 doesn't get trampled by 1280 etc.
    slot_subs.sort_by_key(|(v, _)| -(v.to_string().len() as i64));

    s = replace_int_lits(&s, &slot_subs);

    // Collect locals FIRST so they shadow helper/result-obj names of the same letter.
    let locals = collect_local_names(&s);
    let mut local_subs: Vec<(String, String)> = Vec::new();
    for (i, n) in locals.iter().enumerate() {
        local_subs.push((n.clone(), format!("$L{}", i)));
    }
    local_subs.sort_by_key(|(k, _)| -(k.len() as i64));
    s = replace_idents(&s, &local_subs);

    // Substitute helper outer-scope references and the result-object outer ref AFTER local
    // renaming (since locals shadow these single-letter names within the handler).
    let mut helper_subs: Vec<(String, String)> = helpers.by_name.iter().map(|(k, v)| (k.clone(), v.clone())).collect();
    if let Some(ro) = &helpers.result_obj_name {
        helper_subs.push((ro.clone(), "$RES".into()));
    }
    if let Some(t) = &helpers.typeof_helper_name {
        helper_subs.push((t.clone(), "$TYPEOF".into()));
    }
    helper_subs.sort_by_key(|(k, _)| -(k.len() as i64));
    s = replace_idents(&s, &helper_subs);

    // Strip line + block comments; collapse whitespace
    s = strip_comments(&s);
    s = collapse_whitespace(&s);

    s
}

fn replace_int_lits(s: &str, subs: &[(i64, &str)]) -> String {
    let bytes = s.as_bytes();
    let mut out = String::with_capacity(s.len());
    let mut i = 0usize;
    while i < bytes.len() {
        let c = bytes[i] as char;
        if c.is_ascii_digit() {
            // Don't replace if previous char is identifier-ish (i.e., this digit is part of an ident)
            let prev_ident = i > 0 && is_ident_char(bytes[i - 1] as char);
            let mut j = i;
            while j < bytes.len() && (bytes[j] as char).is_ascii_digit() { j += 1; }
            if !prev_ident {
                let num_str = &s[i..j];
                let mut replaced = false;
                if let Ok(n) = num_str.parse::<i64>() {
                    for (val, tag) in subs {
                        if *val == n {
                            out.push_str(tag);
                            replaced = true;
                            break;
                        }
                    }
                }
                if !replaced { out.push_str(num_str); }
            } else {
                out.push_str(&s[i..j]);
            }
            i = j;
        } else {
            out.push(c);
            i += 1;
        }
    }
    out
}

fn is_ident_char(c: char) -> bool { c.is_ascii_alphanumeric() || c == '_' || c == '$' }

fn replace_idents(s: &str, subs: &[(String, String)]) -> String {
    if subs.is_empty() { return s.to_string(); }
    let bytes = s.as_bytes();
    let mut out = String::with_capacity(s.len());
    let mut i = 0usize;
    while i < bytes.len() {
        let c = bytes[i] as char;
        // string literals: copy through verbatim
        if c == '"' || c == '\'' || c == '`' {
            let quote = c;
            out.push(c);
            i += 1;
            while i < bytes.len() {
                let cc = bytes[i] as char;
                out.push(cc);
                i += 1;
                if cc == '\\' && i < bytes.len() { out.push(bytes[i] as char); i += 1; continue; }
                if cc == quote { break; }
            }
            continue;
        }
        if c.is_ascii_alphabetic() || c == '_' || c == '$' {
            let prev_ident = i > 0 && is_ident_char(bytes[i - 1] as char);
            let mut j = i;
            while j < bytes.len() && is_ident_char(bytes[j] as char) { j += 1; }
            let ident = &s[i..j];
            if !prev_ident {
                let mut replaced = false;
                for (k, v) in subs {
                    if ident == k {
                        out.push_str(v);
                        replaced = true;
                        break;
                    }
                }
                if !replaced { out.push_str(ident); }
            } else {
                out.push_str(ident);
            }
            i = j;
        } else {
            out.push(c);
            i += 1;
        }
    }
    out
}

pub fn collect_local_names(s: &str) -> Vec<String> {
    let mut out: Vec<String> = Vec::new();
    let mut seen: std::collections::HashSet<String> = std::collections::HashSet::new();
    let bytes = s.as_bytes();
    let mut i = 0usize;
    while i + 5 <= bytes.len() {
        // Find `var `, `let `, or `const ` followed by an identifier, at a word boundary.
        let kw_len = if &bytes[i..i + 3] == b"var" || &bytes[i..i + 3] == b"let" {
            Some(3usize)
        } else if i + 5 <= bytes.len() && &bytes[i..i + 5] == b"const" {
            Some(5usize)
        } else {
            None
        };
        if let Some(klen) = kw_len {
            let prev_ok = i == 0 || !is_ident_char(bytes[i - 1] as char);
            let after = bytes[i + klen] as char;
            if prev_ok && (after == ' ' || after == '\t' || after == '\n') {
                let mut k = i + klen;
                while k < bytes.len() && (bytes[k] as char).is_whitespace() { k += 1; }
                // collect comma-separated identifiers in this declaration
                loop {
                    let start = k;
                    while k < bytes.len() && is_ident_char(bytes[k] as char) { k += 1; }
                    if k > start {
                        let name = s[start..k].to_string();
                        if !name.starts_with('$') && name != "A" && !seen.contains(&name) {
                            seen.insert(name.clone());
                            out.push(name);
                        }
                    }
                    // advance past initializer to next `,` at depth 0 or `;` / `)` end of decl
                    let mut depth_paren = 0i32;
                    let mut depth_brack = 0i32;
                    let mut depth_brace = 0i32;
                    while k < bytes.len() {
                        let ch = bytes[k] as char;
                        match ch {
                            '"' | '\'' | '`' => {
                                let q = ch;
                                k += 1;
                                while k < bytes.len() {
                                    let cc = bytes[k] as char;
                                    k += 1;
                                    if cc == '\\' && k < bytes.len() { k += 1; continue; }
                                    if cc == q { break; }
                                }
                                continue;
                            }
                            '(' => depth_paren += 1,
                            ')' => { if depth_paren == 0 { break; } depth_paren -= 1; }
                            '[' => depth_brack += 1,
                            ']' => { if depth_brack == 0 { break; } depth_brack -= 1; }
                            '{' => depth_brace += 1,
                            '}' => { if depth_brace == 0 { break; } depth_brace -= 1; }
                            ',' if depth_paren == 0 && depth_brack == 0 && depth_brace == 0 => break,
                            ';' if depth_paren == 0 && depth_brack == 0 && depth_brace == 0 => break,
                            '\n' if depth_paren == 0 && depth_brack == 0 && depth_brace == 0 => {
                                // ASI possibility — but our codegen always emits ; so this is safe
                                break;
                            }
                            _ => {}
                        }
                        k += 1;
                    }
                    if k < bytes.len() && bytes[k] as char == ',' {
                        k += 1;
                        while k < bytes.len() && (bytes[k] as char).is_whitespace() { k += 1; }
                        continue;
                    }
                    break;
                }
                i = k;
                continue;
            }
        }
        i += 1;
    }
    out
}

fn strip_comments(s: &str) -> String {
    // Codegen typically doesn't emit comments, but be safe.
    let bytes = s.as_bytes();
    let mut out = String::with_capacity(s.len());
    let mut i = 0usize;
    while i < bytes.len() {
        let c = bytes[i] as char;
        if c == '"' || c == '\'' || c == '`' {
            let q = c;
            out.push(c);
            i += 1;
            while i < bytes.len() {
                let cc = bytes[i] as char;
                out.push(cc);
                i += 1;
                if cc == '\\' && i < bytes.len() { out.push(bytes[i] as char); i += 1; continue; }
                if cc == q { break; }
            }
            continue;
        }
        if c == '/' && i + 1 < bytes.len() {
            if bytes[i + 1] == b'/' {
                i += 2;
                while i < bytes.len() && bytes[i] != b'\n' { i += 1; }
                continue;
            }
            if bytes[i + 1] == b'*' {
                i += 2;
                while i + 1 < bytes.len() && !(bytes[i] == b'*' && bytes[i + 1] == b'/') { i += 1; }
                i = (i + 2).min(bytes.len());
                continue;
            }
        }
        out.push(c);
        i += 1;
    }
    out
}

fn collapse_whitespace(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    let mut last_ws = false;
    let bytes = s.as_bytes();
    let mut i = 0usize;
    while i < bytes.len() {
        let c = bytes[i] as char;
        // pass through string literals untouched
        if c == '"' || c == '\'' || c == '`' {
            let q = c;
            if last_ws { out.push(' '); last_ws = false; }
            out.push(c);
            i += 1;
            while i < bytes.len() {
                let cc = bytes[i] as char;
                out.push(cc);
                i += 1;
                if cc == '\\' && i < bytes.len() { out.push(bytes[i] as char); i += 1; continue; }
                if cc == q { break; }
            }
            continue;
        }
        if c.is_whitespace() {
            last_ws = true;
            i += 1;
            continue;
        }
        if last_ws && !out.is_empty() {
            out.push(' ');
            last_ws = false;
        }
        out.push(c);
        i += 1;
    }
    out.trim().to_string()
}

// ----- bytecode disassembly -------------------------------------------------

struct Reader<'a> { bytes: &'a [u8], pos: usize }

impl<'a> Reader<'a> {
    fn read(&mut self) -> Option<u8> { let b = *self.bytes.get(self.pos)?; self.pos += 1; Some(b) }
    fn read_u16_be(&mut self) -> Option<u16> { Some(((self.read()? as u16) << 8) | (self.read()? as u16)) }
    fn read_u24_be(&mut self) -> Option<u32> {
        Some(((self.read()? as u32) << 16) | ((self.read()? as u32) << 8) | (self.read()? as u32))
    }
    fn read_i32_be(&mut self) -> Option<i32> {
        let mut a: i64 = 0;
        for _ in 0..4 { a = (a << 8) | (self.read()? as i64); }
        Some(a as i32)
    }
}

fn read_value(r: &mut Reader, spec: &VmSpec) -> Option<String> {
    let m = r.read()?;
    if m & 0x80 != 0 { return Some(format!("{}", m & 0x7F)); }
    if Some(m) == spec.markers.r#true { return Some("true".into()); }
    if Some(m) == spec.markers.r#false { return Some("false".into()); }
    if Some(m) == spec.markers.null { return Some("null".into()); }
    if Some(m) == spec.markers.undefined { return Some("undefined".into()); }
    if Some(m) == spec.markers.string {
        let mut out = String::new();
        let mut k = spec.xor_key_ascii;
        loop {
            let b = r.read()?;
            let dec = b ^ k;
            k = k.wrapping_add(1);
            if dec == 0 { break; }
            out.push(dec as char);
        }
        return Some(format!("{:?}", out));
    }
    if Some(m) == spec.markers.utf8 {
        let mut bytes: Vec<u8> = Vec::new();
        let mut k = spec.xor_key_utf8;
        loop {
            let b = r.read()?;
            let dec = b ^ k;
            k = k.wrapping_add(1);
            if dec == 0 { break; }
            bytes.push(dec);
        }
        let s = String::from_utf8_lossy(&bytes).into_owned();
        return Some(format!("{:?}", s));
    }
    if Some(m) == spec.markers.float64 {
        let mut buf = [0u8; 8];
        for i in 0..8 { buf[i] = r.read()?; }
        return Some(format!("{}", f64::from_be_bytes(buf)));
    }
    if Some(m) == spec.markers.int32 {
        Some(format!("{}", r.read_i32_be()?))
    } else if Some(m) == spec.markers.int24 {
        let mut a: i64 = 0;
        for _ in 0..3 { a = (a << 8) | (r.read()? as i64); }
        Some(format!("{}", ((a << 8) as i32) >> 8))
    } else if Some(m) == spec.markers.int16 {
        let mut a: i64 = 0;
        for _ in 0..2 { a = (a << 8) | (r.read()? as i64); }
        Some(format!("{}", ((a << 16) as i32) >> 16))
    } else if Some(m) == spec.markers.int8 {
        let a = r.read()? as i64;
        Some(format!("{}", ((a << 24) as i32) >> 24))
    } else {
        Some(format!("?0x{:02x}", m))
    }
}

pub fn disassemble(
    bytes: &[u8],
    spec: &VmSpec,
    handler_table: &HashMap<u8, (String, &'static str, &'static str)>,
    header_extra: &str,
) -> String {
    let mut out = String::with_capacity(bytes.len() * 12);
    out.push_str(&format!("; vm bytecode: {} bytes\n", bytes.len()));
    out.push_str(header_extra);
    out.push_str(&format!("; markers: true={:?} false={:?} null={:?} undef={:?} str={:?}/k{} utf8={:?}/k{} f64={:?} i8={:?} i16={:?} i24={:?} i32={:?}\n",
        spec.markers.r#true, spec.markers.r#false, spec.markers.null, spec.markers.undefined,
        spec.markers.string, spec.xor_key_ascii, spec.markers.utf8, spec.xor_key_utf8,
        spec.markers.float64, spec.markers.int8, spec.markers.int16, spec.markers.int24, spec.markers.int32));
    out.push('\n');

    let mut r = Reader { bytes, pos: 0 };
    let mut count = 0usize;

    // Pre-scan jump targets for label generation
    let mut jump_targets: std::collections::BTreeSet<usize> = std::collections::BTreeSet::new();
    if !handler_table.is_empty() {
        let mut rr = Reader { bytes, pos: 0 };
        while rr.pos < bytes.len() {
            let addr = rr.pos;
            let Some(op) = rr.read() else { break; };
            let (_, _, fmt) = match handler_table.get(&op) { Some(t) => t.clone(), None => (String::new(), "", "") };
            if fmt.is_empty() { continue; }
            let pre_pos = rr.pos;
            for part in fmt.split(',') {
                match part {
                    "u8" => { rr.read(); }
                    "u16" => { rr.read_u16_be(); }
                    "u24" => { rr.read_u24_be(); }
                    "val" => { let _ = read_value(&mut rr, spec); }
                    "u8*" | "val*" | "jmp" => {}
                    _ => {}
                }
            }
            let _ = pre_pos;
            let name = handler_table.get(&op).map(|t| t.1).unwrap_or("");
            let fwd16 = matches!(name, "JMP_FWD" | "JZ" | "JZ_KEEP" | "JNZ_KEEP" | "JZ_FWD_KEEP" | "JNZ_FWD_KEEP" | "JZ_DROP");
            let fwd24 = matches!(name, "JMP_FWD_LONG" | "JZ_FWD_KEEP_LONG" | "JNZ_FWD_KEEP_LONG" | "JZ_DROP_LONG");
            let back16 = name == "JMP_BACK";
            let back24 = name == "JMP_BACK_LONG";
            if fwd16 && rr.pos >= 2 + addr {
                let off = ((bytes[addr + 1] as i32) << 8) | (bytes[addr + 2] as i32);
                let target = (rr.pos as i32 + off) as usize;
                jump_targets.insert(target);
            }
            if back16 && rr.pos >= 2 + addr {
                let off = ((bytes[addr + 1] as i32) << 8) | (bytes[addr + 2] as i32);
                let target = (rr.pos as i32 - off) as usize;
                jump_targets.insert(target);
            }
            if fwd24 && rr.pos >= 3 + addr {
                let off = ((bytes[addr + 1] as i32) << 16) | ((bytes[addr + 2] as i32) << 8) | (bytes[addr + 3] as i32);
                let target = (rr.pos as i32 + off) as usize;
                jump_targets.insert(target);
            }
            if back24 && rr.pos >= 3 + addr {
                let off = ((bytes[addr + 1] as i32) << 16) | ((bytes[addr + 2] as i32) << 8) | (bytes[addr + 3] as i32);
                let target = (rr.pos as i32 - off) as usize;
                jump_targets.insert(target);
            }
        }
    }

    while r.pos < bytes.len() {
        let addr = r.pos;
        if jump_targets.contains(&addr) {
            out.push_str(&format!("L_{:08x}:\n", addr));
        }
        let Some(op) = r.read() else { break; };
        let entry = handler_table.get(&op);
        if let Some((hash, name, fmt)) = entry {
            if !name.is_empty() {
                let args = read_args(&mut r, spec, fmt, addr);
                out.push_str(&format!("{:08x}: {}", addr, name));
                if !args.is_empty() { out.push(' '); out.push_str(&args); }
                out.push('\n');
            } else {
                out.push_str(&format!("{:08x}: UNK_{} 0x{:02x}\n", addr, hash, op));
            }
        } else {
            out.push_str(&format!("{:08x}: OP 0x{:02x}\n", addr, op));
        }
        count += 1;
        if count > 200_000 { out.push_str("# truncated\n"); break; }
    }
    out
}

fn read_args(r: &mut Reader, spec: &VmSpec, fmt: &str, _addr: usize) -> String {
    if fmt.is_empty() { return String::new(); }
    let mut parts: Vec<String> = Vec::new();
    let split: Vec<&str> = fmt.split(',').collect();
    let mut last_u8_count: Option<u32> = None;
    for (i, kind) in split.iter().enumerate() {
        match *kind {
            "u8" => {
                let v = match r.read() { Some(v) => v, None => { parts.push("?".into()); break; } };
                last_u8_count = Some(v as u32);
                parts.push(format!("{}", v));
            }
            "u16" => {
                let v = match r.read_u16_be() { Some(v) => v, None => { parts.push("?".into()); break; } };
                parts.push(format!("{}", v));
            }
            "u24" => {
                let v = match r.read_u24_be() { Some(v) => v, None => { parts.push("?".into()); break; } };
                parts.push(format!("{}", v));
            }
            "val" => {
                let v = match read_value(r, spec) { Some(v) => v, None => { parts.push("?".into()); break; } };
                parts.push(v);
            }
            "u8*" => {
                let n = last_u8_count.unwrap_or(0);
                let mut arr: Vec<String> = Vec::new();
                for _ in 0..n {
                    let v = match r.read() { Some(v) => v, None => { arr.push("?".into()); break; } };
                    arr.push(format!("{}", v));
                }
                parts.push(format!("[{}]", arr.join(",")));
            }
            "val*" => {
                let n = last_u8_count.unwrap_or(0);
                let mut arr: Vec<String> = Vec::new();
                for _ in 0..n {
                    let v = match read_value(r, spec) { Some(v) => v, None => { arr.push("?".into()); break; } };
                    arr.push(v);
                }
                parts.push(format!("[{}]", arr.join(",")));
            }
            "jmp" => {
                let v = match r.read_u16_be() { Some(v) => v, None => { parts.push("?".into()); break; } };
                let target = (r.pos as i32 + v as i32) as usize;
                parts.push(format!("L_{:08x}", target));
            }
            _ => parts.push(format!("?{}", kind)),
        }
        let _ = i;
    }
    parts.join(", ")
}

// ----- helpers used by build_vm_db binary -----------------------------------

#[derive(Debug, Clone, Default)]
pub struct LabeledNormalizer;

impl LabeledNormalizer {
    /// Normalize a labeled-vm handler body that uses *named* slot constants and helper functions
    /// (e.g. `A[stack_pointer]`, `readUint8()`). Maps named identifiers to role tags then runs the
    /// same positional local-var renaming and whitespace collapsing as the deob-side normalizer.
    pub fn normalize(body_text: &str) -> String {
        let mut s = canonicalize_handler_body_source(body_text);

        // The corpus uses bare numeric literals for a few build-specific constants
        // that the deob-side normalizer tags by role. Substitute them by integer value here.
        let lit_subs: Vec<(i64, &str)> = vec![
            (4591, "$SPEC"),
        ];
        s = replace_int_lits(&s, &lit_subs);

        let helper_subs: Vec<(&str, &str)> = vec![
            ("readUint8", "$RU8"), ("readUint16", "$RU16"), ("readUint24", "$RU24"),
            ("getVal", "$RVAL"), ("register2stack", "$R2S"), ("stack2register", "$S2R"),
            ("storeToLastResult", "$SLR"), ("GET", "$GET"), ("SET", "$SET"), ("fetch", "$FETCH"),
        ];
        let slot_subs: Vec<(&str, &str)> = vec![
            ("stack_pointer", "$SP"), ("instruction_pointer", "$IP"),
            ("frame_base_pointer", "$FBP"), ("frame_base_counter", "$FBC"),
            ("last_result", "$LR"), ("exit_flag", "$EXIT"),
            ("current_opcode_handler", "$CH"), ("current_opcode_id", "$CI"),
            ("stack_offset", "$STACK"), ("vm_start", "$VMS"),
        ];
        let mut slot_only: Vec<(String, String)> = slot_subs.iter().map(|(k, v)| (k.to_string(), v.to_string())).collect();
        slot_only.sort_by_key(|(k, _)| -(k.len() as i64));
        s = replace_idents(&s, &slot_only);

        let locals = collect_local_names(&s);
        let mut local_subs: Vec<(String, String)> = Vec::new();
        for (i, n) in locals.iter().enumerate() {
            local_subs.push((n.clone(), format!("$L{}", i)));
        }
        local_subs.sort_by_key(|(k, _)| -(k.len() as i64));
        s = replace_idents(&s, &local_subs);

        let mut helper_only: Vec<(String, String)> = helper_subs.iter().map(|(k, v)| (k.to_string(), v.to_string())).collect();
        helper_only.push(("Q".to_string(), "$RES".to_string()));
        helper_only.push(("E".to_string(), "$TYPEOF".to_string()));
        helper_only.sort_by_key(|(k, _)| -(k.len() as i64));
        s = replace_idents(&s, &helper_only);

        s = strip_comments(&s);
        s = collapse_whitespace(&s);
        s
    }
}
