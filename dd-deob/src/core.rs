use oxc_allocator::Allocator;
use oxc_ast::AstBuilder;
use oxc_ast::ast::*;
use oxc_parser::{ParseOptions, Parser};
use oxc_span::{SourceType, SPAN};
use oxc_syntax::number::NumberBase;
use oxc_syntax::operator::{BinaryOperator, LogicalOperator, UnaryOperator};
use rustc_hash::FxHashMap as HashMap;

pub fn parse_js<'a>(allocator: &'a Allocator, source: &'a str) -> Program<'a> {
    let ret = Parser::new(allocator, source, SourceType::cjs())
        .with_options(ParseOptions {
            allow_return_outside_function: true,
            preserve_parens: false,
            ..Default::default()
        })
        .parse();
    if !ret.errors.is_empty() {
        eprintln!("parse: {} errors", ret.errors.len());
        for e in ret.errors.iter().take(5) { eprintln!("  {}", e); }
    }
    assert!(!ret.panicked, "parser panicked");
    ret.program
}

#[derive(Clone, Debug)]
pub enum JsValue {
    Num(f64),
    Str(String),
    Bool(bool),
    Null,
    Undefined,
    Array(Vec<JsValue>),
    Object,
}

impl JsValue {
    pub fn is_nullish(&self) -> bool {
        matches!(self, JsValue::Null | JsValue::Undefined)
    }

    pub fn to_expr<'a>(&self, alloc: &'a Allocator) -> Option<Expression<'a>> {
        let ast = AstBuilder::new(alloc);
        match self {
            JsValue::Num(n) => {
                if n.is_nan() {
                    let z1 = ast.expression_numeric_literal(SPAN, 0.0, None, NumberBase::Decimal);
                    let z2 = ast.expression_numeric_literal(SPAN, 0.0, None, NumberBase::Decimal);
                    return Some(ast.expression_binary(SPAN, z1, BinaryOperator::Division, z2));
                }
                if n.is_infinite() {
                    let one = ast.expression_numeric_literal(SPAN, 1.0, None, NumberBase::Decimal);
                    let zero = ast.expression_numeric_literal(SPAN, 0.0, None, NumberBase::Decimal);
                    let inf = ast.expression_binary(SPAN, one, BinaryOperator::Division, zero);
                    return Some(if *n > 0.0 { inf } else { ast.expression_unary(SPAN, UnaryOperator::UnaryNegation, inf) });
                }
                if *n < 0.0 || (*n == 0.0 && n.is_sign_negative()) {
                    let inner = ast.expression_numeric_literal(SPAN, -n, None, NumberBase::Decimal);
                    Some(ast.expression_unary(SPAN, UnaryOperator::UnaryNegation, inner))
                } else {
                    Some(ast.expression_numeric_literal(SPAN, *n, None, NumberBase::Decimal))
                }
            }
            JsValue::Str(s) => Some(ast.expression_string_literal(SPAN, ast.str(s), None)),
            JsValue::Bool(b) => Some(ast.expression_boolean_literal(SPAN, *b)),
            JsValue::Null => Some(ast.expression_null_literal(SPAN)),
            JsValue::Undefined => Some(ast.void_0(SPAN)),
            _ => None,
        }
    }
}

impl PartialEq for JsValue {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (JsValue::Num(a), JsValue::Num(b)) => (a.is_nan() && b.is_nan()) || a == b,
            (JsValue::Str(a), JsValue::Str(b)) => a == b,
            (JsValue::Bool(a), JsValue::Bool(b)) => a == b,
            (JsValue::Null, JsValue::Null) => true,
            (JsValue::Undefined, JsValue::Undefined) => true,
            _ => false,
        }
    }
}

pub fn to_num(v: &JsValue) -> f64 {
    match v {
        JsValue::Num(n) => *n,
        JsValue::Bool(b) => if *b { 1.0 } else { 0.0 },
        JsValue::Null => 0.0,
        JsValue::Undefined => f64::NAN,
        JsValue::Str(s) => {
            let t = s.trim();
            if t.is_empty() { 0.0 } else { t.parse::<f64>().unwrap_or(f64::NAN) }
        }
        JsValue::Array(arr) => match arr.len() {
            0 => 0.0,
            1 => to_num(&arr[0]),
            _ => f64::NAN,
        },
        JsValue::Object => f64::NAN,
    }
}

pub fn to_bool(v: &JsValue) -> bool {
    match v {
        JsValue::Null | JsValue::Undefined => false,
        JsValue::Bool(b) => *b,
        JsValue::Num(n) => *n != 0.0 && !n.is_nan(),
        JsValue::Str(s) => !s.is_empty(),
        JsValue::Array(_) | JsValue::Object => true,
    }
}

pub fn to_prim(v: &JsValue) -> Option<JsValue> {
    match v {
        JsValue::Null | JsValue::Undefined | JsValue::Str(_) | JsValue::Num(_) | JsValue::Bool(_) => Some(v.clone()),
        JsValue::Array(arr) => {
            let parts: Vec<String> = arr.iter().map(|e| match e {
                JsValue::Null | JsValue::Undefined => String::new(),
                JsValue::Str(s) => s.clone(),
                JsValue::Num(n) => js_num_to_string(*n),
                JsValue::Bool(b) => b.to_string(),
                JsValue::Array(_) => "[array]".to_string(),
                JsValue::Object => "[object Object]".to_string(),
            }).collect();
            Some(JsValue::Str(parts.join(",")))
        }
        JsValue::Object => None,
    }
}

pub fn js_to_string(v: &JsValue) -> String {
    match v {
        JsValue::Str(s) => s.clone(),
        JsValue::Num(n) => js_num_to_string(*n),
        JsValue::Bool(b) => b.to_string(),
        JsValue::Null => "null".into(),
        JsValue::Undefined => "undefined".into(),
        JsValue::Array(arr) => arr.iter().map(|e| js_to_string(e)).collect::<Vec<_>>().join(","),
        JsValue::Object => "[object Object]".into(),
    }
}

fn js_num_to_string(n: f64) -> String {
    if n == 0.0 && n.is_sign_negative() { "0".into() }
    else if n.is_infinite() { if n > 0.0 { "Infinity".into() } else { "-Infinity".into() } }
    else if n.is_nan() { "NaN".into() }
    else if n.fract() == 0.0 && n.abs() < 1e21 { format!("{}", n as i64) }
    else { format!("{}", n) }
}

pub fn js_strict_eq(l: &JsValue, r: &JsValue) -> bool {
    match (l, r) {
        (JsValue::Num(a), JsValue::Num(b)) => a == b,
        (JsValue::Str(a), JsValue::Str(b)) => a == b,
        (JsValue::Bool(a), JsValue::Bool(b)) => a == b,
        (JsValue::Null, JsValue::Null) | (JsValue::Undefined, JsValue::Undefined) => true,
        _ => false,
    }
}

pub fn js_abstract_eq(l: &JsValue, r: &JsValue) -> bool {
    match (l, r) {
        (JsValue::Null, JsValue::Null)
        | (JsValue::Null, JsValue::Undefined)
        | (JsValue::Undefined, JsValue::Null)
        | (JsValue::Undefined, JsValue::Undefined) => true,
        (JsValue::Num(a), JsValue::Num(b)) => a == b,
        (JsValue::Str(a), JsValue::Str(b)) => a == b,
        (JsValue::Bool(a), JsValue::Bool(b)) => a == b,
        (JsValue::Num(_), JsValue::Str(s)) => to_num(l) == s.parse::<f64>().unwrap_or(f64::NAN),
        (JsValue::Str(s), JsValue::Num(_)) => s.parse::<f64>().unwrap_or(f64::NAN) == to_num(r),
        (JsValue::Bool(b), _) => js_abstract_eq(&JsValue::Num(if *b { 1.0 } else { 0.0 }), r),
        (_, JsValue::Bool(b)) => js_abstract_eq(l, &JsValue::Num(if *b { 1.0 } else { 0.0 })),
        _ => false,
    }
}

pub fn is_valid_ident(s: &str) -> bool {
    if s.is_empty() { return false; }
    let mut chars = s.chars();
    let first = chars.next().unwrap();
    if !(first == '_' || first == '$' || first.is_ascii_alphabetic()) { return false; }
    chars.all(|c| c == '_' || c == '$' || c.is_ascii_alphanumeric())
}

pub fn atob_default(s: &str) -> Option<String> {
    use base64::Engine;
    let cleaned: String = s.chars().filter(|c| !c.is_whitespace()).collect();
    let bytes = base64::engine::general_purpose::STANDARD.decode(cleaned.as_bytes())
        .or_else(|_| base64::engine::general_purpose::STANDARD_NO_PAD.decode(cleaned.as_bytes()))
        .ok()?;
    Some(bytes.into_iter().map(|b| b as char).collect())
}

pub fn eval_expr(expr: &Expression, env: &HashMap<&str, JsValue>) -> Option<JsValue> {
    match expr {
        Expression::NumericLiteral(lit) => Some(JsValue::Num(lit.value)),
        Expression::StringLiteral(lit) => Some(JsValue::Str(lit.value.as_str().to_string())),
        Expression::BooleanLiteral(lit) => Some(JsValue::Bool(lit.value)),
        Expression::NullLiteral(_) => Some(JsValue::Null),
        Expression::Identifier(id) => {
            let name = id.name.as_str();
            match name {
                "undefined" => Some(JsValue::Undefined),
                "NaN" => Some(JsValue::Num(f64::NAN)),
                "Infinity" => Some(JsValue::Num(f64::INFINITY)),
                _ => env.get(name).cloned(),
            }
        }
        Expression::ArrayExpression(arr) => {
            let mut els = Vec::with_capacity(arr.elements.len());
            for el in &arr.elements {
                match el {
                    ArrayExpressionElement::SpreadElement(_) => return None,
                    ArrayExpressionElement::Elision(_) => els.push(JsValue::Undefined),
                    _ => els.push(eval_expr(el.to_expression(), env)?),
                }
            }
            Some(JsValue::Array(els))
        }
        Expression::UnaryExpression(unary) => {
            let arg = eval_expr(&unary.argument, env)?;
            apply_unary(unary.operator, &arg)
        }
        Expression::BinaryExpression(bin) => {
            let l = eval_expr(&bin.left, env)?;
            let r = eval_expr(&bin.right, env)?;
            apply_bin(bin.operator, &l, &r)
        }
        Expression::LogicalExpression(log) => {
            let l = eval_expr(&log.left, env)?;
            apply_logical(log.operator, l, |env| eval_expr(&log.right, env), env)
        }
        Expression::ConditionalExpression(cond) => {
            let test = eval_expr(&cond.test, env)?;
            if to_bool(&test) { eval_expr(&cond.consequent, env) } else { eval_expr(&cond.alternate, env) }
        }
        Expression::SequenceExpression(seq) => {
            let mut last = JsValue::Undefined;
            for e in &seq.expressions { last = eval_expr(e, env)?; }
            Some(last)
        }
        Expression::StaticMemberExpression(mem) => {
            let prop = mem.property.name.as_str();
            let obj = eval_expr(&mem.object, env)?;
            match (&obj, prop) {
                (JsValue::Array(arr), "length") => Some(JsValue::Num(arr.len() as f64)),
                (JsValue::Str(s), "length") => Some(JsValue::Num(s.chars().count() as f64)),
                _ => None,
            }
        }
        Expression::ComputedMemberExpression(mem) => {
            let obj = eval_expr(&mem.object, env)?;
            let key = eval_expr(&mem.expression, env)?;
            index_into(&obj, &key)
        }
        Expression::ParenthesizedExpression(p) => eval_expr(&p.expression, env),
        _ => None,
    }
}

fn apply_logical<F>(op: LogicalOperator, l: JsValue, right: F, env: &HashMap<&str, JsValue>) -> Option<JsValue>
where F: FnOnce(&HashMap<&str, JsValue>) -> Option<JsValue> {
    match op {
        LogicalOperator::And => if !to_bool(&l) { Some(l) } else { right(env) },
        LogicalOperator::Or => if to_bool(&l) { Some(l) } else { right(env) },
        LogicalOperator::Coalesce => if !l.is_nullish() { Some(l) } else { right(env) },
    }
}

pub fn typeof_str(v: &JsValue) -> &'static str {
    match v {
        JsValue::Num(_) => "number",
        JsValue::Str(_) => "string",
        JsValue::Bool(_) => "boolean",
        JsValue::Null | JsValue::Array(_) | JsValue::Object => "object",
        JsValue::Undefined => "undefined",
    }
}

fn apply_unary(op: UnaryOperator, v: &JsValue) -> Option<JsValue> {
    let pv = to_prim(v).unwrap_or_else(|| v.clone());
    match op {
        UnaryOperator::UnaryPlus => Some(JsValue::Num(to_num(&pv))),
        UnaryOperator::UnaryNegation => Some(JsValue::Num(-to_num(&pv))),
        UnaryOperator::LogicalNot => Some(JsValue::Bool(!to_bool(v))),
        UnaryOperator::BitwiseNot => Some(JsValue::Num(!(to_num(&pv) as i32) as f64)),
        UnaryOperator::Void => Some(JsValue::Undefined),
        UnaryOperator::Typeof => Some(JsValue::Str(typeof_str(v).into())),
        _ => None,
    }
}

pub fn index_into(obj: &JsValue, key: &JsValue) -> Option<JsValue> {
    let key_str = match key {
        JsValue::Str(s) => s.clone(),
        JsValue::Num(n) => match obj {
            JsValue::Array(arr) => {
                if *n >= 0.0 && n.fract() == 0.0 {
                    let idx = *n as usize;
                    return Some(arr.get(idx).cloned().unwrap_or(JsValue::Undefined));
                }
                js_to_string(key)
            }
            JsValue::Str(s) => {
                if *n >= 0.0 && n.fract() == 0.0 {
                    let idx = *n as usize;
                    let chs: Vec<char> = s.chars().collect();
                    return Some(chs.get(idx).map(|c| JsValue::Str(c.to_string())).unwrap_or(JsValue::Undefined));
                }
                js_to_string(key)
            }
            _ => js_to_string(key),
        },
        _ => js_to_string(&to_prim(key)?),
    };
    match obj {
        JsValue::Array(arr) => {
            if key_str == "length" { return Some(JsValue::Num(arr.len() as f64)); }
            if let Ok(idx) = key_str.parse::<usize>() {
                return Some(arr.get(idx).cloned().unwrap_or(JsValue::Undefined));
            }
            None
        }
        JsValue::Str(s) => {
            if key_str == "length" { return Some(JsValue::Num(s.chars().count() as f64)); }
            if let Ok(idx) = key_str.parse::<usize>() {
                let chs: Vec<char> = s.chars().collect();
                return Some(chs.get(idx).map(|c| JsValue::Str(c.to_string())).unwrap_or(JsValue::Undefined));
            }
            None
        }
        _ => None,
    }
}

fn apply_bin(op: BinaryOperator, l: &JsValue, r: &JsValue) -> Option<JsValue> {
    if op == BinaryOperator::Addition {
        let lp = to_prim(l)?;
        let rp = to_prim(r)?;
        if matches!(&lp, JsValue::Str(_)) || matches!(&rp, JsValue::Str(_)) {
            return Some(JsValue::Str(format!("{}{}", js_to_string(&lp), js_to_string(&rp))));
        }
        return Some(JsValue::Num(to_num(&lp) + to_num(&rp)));
    }
    match op {
        BinaryOperator::Equality => return Some(JsValue::Bool(js_abstract_eq(l, r))),
        BinaryOperator::Inequality => return Some(JsValue::Bool(!js_abstract_eq(l, r))),
        BinaryOperator::StrictEquality => return Some(JsValue::Bool(js_strict_eq(l, r))),
        BinaryOperator::StrictInequality => return Some(JsValue::Bool(!js_strict_eq(l, r))),
        _ => {}
    }
    let ln = to_num(&to_prim(l).unwrap_or_else(|| l.clone()));
    let rn = to_num(&to_prim(r).unwrap_or_else(|| r.clone()));
    match op {
        BinaryOperator::Subtraction => Some(JsValue::Num(ln - rn)),
        BinaryOperator::Multiplication => Some(JsValue::Num(ln * rn)),
        BinaryOperator::Division => Some(JsValue::Num(ln / rn)),
        BinaryOperator::Remainder => Some(JsValue::Num(ln % rn)),
        BinaryOperator::Exponential => Some(JsValue::Num(ln.powf(rn))),
        BinaryOperator::ShiftLeft => Some(JsValue::Num(((ln as i32) << ((rn as u32) & 0x1f)) as f64)),
        BinaryOperator::ShiftRight => Some(JsValue::Num(((ln as i32) >> ((rn as u32) & 0x1f)) as f64)),
        BinaryOperator::ShiftRightZeroFill => Some(JsValue::Num(((ln as u32) >> ((rn as u32) & 0x1f)) as f64)),
        BinaryOperator::BitwiseOR => Some(JsValue::Num(((ln as i32) | (rn as i32)) as f64)),
        BinaryOperator::BitwiseAnd => Some(JsValue::Num(((ln as i32) & (rn as i32)) as f64)),
        BinaryOperator::BitwiseXOR => Some(JsValue::Num(((ln as i32) ^ (rn as i32)) as f64)),
        BinaryOperator::LessThan => Some(JsValue::Bool(ln < rn)),
        BinaryOperator::LessEqualThan => Some(JsValue::Bool(ln <= rn)),
        BinaryOperator::GreaterThan => Some(JsValue::Bool(ln > rn)),
        BinaryOperator::GreaterEqualThan => Some(JsValue::Bool(ln >= rn)),
        _ => None,
    }
}

pub fn is_pure(expr: &Expression) -> bool {
    match expr {
        Expression::NumericLiteral(_) | Expression::StringLiteral(_)
        | Expression::BooleanLiteral(_) | Expression::NullLiteral(_) => true,
        Expression::Identifier(id) => {
            let n = id.name.as_str();
            n == "undefined" || n == "NaN" || n == "Infinity"
        }
        Expression::ArrayExpression(arr) => arr.elements.iter().all(|el| match el {
            ArrayExpressionElement::SpreadElement(_) => false,
            ArrayExpressionElement::Elision(_) => true,
            _ => is_pure(el.to_expression()),
        }),
        Expression::UnaryExpression(u) => !matches!(u.operator, UnaryOperator::Delete) && is_pure(&u.argument),
        Expression::BinaryExpression(b) => is_pure(&b.left) && is_pure(&b.right),
        Expression::LogicalExpression(l) => is_pure(&l.left) && is_pure(&l.right),
        Expression::ConditionalExpression(c) => is_pure(&c.test) && is_pure(&c.consequent) && is_pure(&c.alternate),
        Expression::SequenceExpression(s) => s.expressions.iter().all(|e| is_pure(e)),
        Expression::ParenthesizedExpression(p) => is_pure(&p.expression),
        _ => false,
    }
}

pub fn literal_truthy(expr: &Expression) -> Option<bool> {
    match expr {
        Expression::BooleanLiteral(b) => Some(b.value),
        Expression::NumericLiteral(n) => Some(n.value != 0.0 && !n.value.is_nan()),
        Expression::StringLiteral(s) => Some(!s.value.is_empty()),
        Expression::NullLiteral(_) => Some(false),
        Expression::Identifier(id) => match id.name.as_str() {
            "undefined" | "NaN" => Some(false),
            "Infinity" => Some(true),
            _ => None,
        },
        Expression::UnaryExpression(u) if matches!(u.operator, UnaryOperator::Void) => Some(false),
        Expression::UnaryExpression(u) if matches!(u.operator, UnaryOperator::UnaryNegation) => {
            if let Expression::NumericLiteral(n) = &u.argument { return Some(n.value != 0.0); }
            None
        }
        Expression::UnaryExpression(u) if matches!(u.operator, UnaryOperator::LogicalNot) => {
            literal_truthy(&u.argument).map(|b| !b)
        }
        _ => None,
    }
}

#[derive(Clone, Debug)]
pub enum MExpr {
    Lit(JsValue),
    Param(usize),
    Local(String),
    Assign(String, Box<MExpr>),
    Unary(UnaryOperator, Box<MExpr>),
    Bin(BinaryOperator, Box<MExpr>, Box<MExpr>),
    Logical(LogicalOperator, Box<MExpr>, Box<MExpr>),
    Cond(Box<MExpr>, Box<MExpr>, Box<MExpr>),
    Seq(Vec<MExpr>),
    SeqLet(Vec<MExpr>),
}

pub fn lower(e: &Expression, params: &[String]) -> Option<MExpr> {
    match e {
        Expression::NumericLiteral(n) => Some(MExpr::Lit(JsValue::Num(n.value))),
        Expression::StringLiteral(s) => Some(MExpr::Lit(JsValue::Str(s.value.as_str().to_string()))),
        Expression::BooleanLiteral(b) => Some(MExpr::Lit(JsValue::Bool(b.value))),
        Expression::NullLiteral(_) => Some(MExpr::Lit(JsValue::Null)),
        Expression::Identifier(id) => {
            let name = id.name.as_str();
            if let Some(i) = params.iter().position(|p| p == name) { return Some(MExpr::Param(i)); }
            match name {
                "undefined" => Some(MExpr::Lit(JsValue::Undefined)),
                "NaN" => Some(MExpr::Lit(JsValue::Num(f64::NAN))),
                "Infinity" => Some(MExpr::Lit(JsValue::Num(f64::INFINITY))),
                _ => Some(MExpr::Local(name.to_string())),
            }
        }
        Expression::AssignmentExpression(asn) => {
            if !matches!(asn.operator, oxc_syntax::operator::AssignmentOperator::Assign) { return None; }
            let AssignmentTarget::AssignmentTargetIdentifier(id) = &asn.left else { return None };
            let name = id.name.as_str().to_string();
            let val = lower(&asn.right, params)?;
            Some(MExpr::Assign(name, Box::new(val)))
        }
        Expression::UnaryExpression(u) => Some(MExpr::Unary(u.operator, Box::new(lower(&u.argument, params)?))),
        Expression::BinaryExpression(b) => Some(MExpr::Bin(b.operator, Box::new(lower(&b.left, params)?), Box::new(lower(&b.right, params)?))),
        Expression::LogicalExpression(l) => Some(MExpr::Logical(l.operator, Box::new(lower(&l.left, params)?), Box::new(lower(&l.right, params)?))),
        Expression::ConditionalExpression(c) => Some(MExpr::Cond(
            Box::new(lower(&c.test, params)?),
            Box::new(lower(&c.consequent, params)?),
            Box::new(lower(&c.alternate, params)?),
        )),
        Expression::SequenceExpression(s) => {
            let mut v = Vec::with_capacity(s.expressions.len());
            for e in &s.expressions { v.push(lower(e, params)?); }
            Some(MExpr::Seq(v))
        }
        Expression::ParenthesizedExpression(p) => lower(&p.expression, params),
        _ => None,
    }
}

pub fn eval(e: &MExpr, args: &[JsValue]) -> Option<JsValue> { eval_mba(e, args) }

pub fn eval_mba(e: &MExpr, args: &[JsValue]) -> Option<JsValue> {
    let mut locals: HashMap<String, JsValue> = HashMap::default();
    eval_mba_with(e, args, &mut locals)
}

fn eval_mba_with(e: &MExpr, args: &[JsValue], locals: &mut HashMap<String, JsValue>) -> Option<JsValue> {
    match e {
        MExpr::Lit(v) => Some(v.clone()),
        MExpr::Param(i) => args.get(*i).cloned(),
        MExpr::Local(name) => locals.get(name).cloned().or(Some(JsValue::Undefined)),
        MExpr::Assign(name, val) => {
            let v = eval_mba_with(val, args, locals)?;
            locals.insert(name.clone(), v.clone());
            Some(v)
        }
        MExpr::Unary(op, inner) => apply_unary(*op, &eval_mba_with(inner, args, locals)?),
        MExpr::Bin(op, l, r) => {
            let lv = eval_mba_with(l, args, locals)?;
            let rv = eval_mba_with(r, args, locals)?;
            apply_bin(*op, &lv, &rv)
        }
        MExpr::Logical(op, l, r) => {
            let lv = eval_mba_with(l, args, locals)?;
            match op {
                LogicalOperator::And => if !to_bool(&lv) { Some(lv) } else { eval_mba_with(r, args, locals) },
                LogicalOperator::Or => if to_bool(&lv) { Some(lv) } else { eval_mba_with(r, args, locals) },
                LogicalOperator::Coalesce => if !lv.is_nullish() { Some(lv) } else { eval_mba_with(r, args, locals) },
            }
        }
        MExpr::Cond(t, c, a) => {
            let tv = eval_mba_with(t, args, locals)?;
            if to_bool(&tv) { eval_mba_with(c, args, locals) } else { eval_mba_with(a, args, locals) }
        }
        MExpr::Seq(es) => {
            let mut last = JsValue::Undefined;
            for e in es { last = eval_mba_with(e, args, locals)?; }
            Some(last)
        }
        MExpr::SeqLet(es) => {
            let mut all = args.to_vec();
            for (i, e) in es.iter().enumerate() {
                let v = eval_mba_with(e, &all, locals)?;
                if i + 1 < es.len() { all.push(v); } else { return Some(v); }
            }
            Some(JsValue::Undefined)
        }
    }
}

#[derive(Clone, Debug)]
pub struct PureFn {
    pub params: Vec<String>,
    pub body: MExpr,
}

pub fn classify_pure_fn(fd: &Function) -> Option<PureFn> {
    let body = fd.body.as_ref()?;
    if body.statements.is_empty() { return None; }

    let mut params = Vec::with_capacity(fd.params.items.len());
    for p in &fd.params.items {
        let BindingPattern::BindingIdentifier(id) = &p.pattern else { return None };
        params.push(id.name.as_str().to_string());
    }

    let mut local_inits: Vec<MExpr> = Vec::new();
    let mut scope_names: Vec<String> = params.clone();
    for stmt in &body.statements[..body.statements.len() - 1] {
        let Statement::VariableDeclaration(vd) = stmt else { return None };
        for d in &vd.declarations {
            let BindingPattern::BindingIdentifier(id) = &d.id else { return None };
            let init = d.init.as_ref()?;
            let lowered = lower(init, &scope_names)?;
            local_inits.push(lowered);
            scope_names.push(id.name.as_str().to_string());
        }
    }

    let last = body.statements.last()?;
    let Statement::ReturnStatement(rs) = last else { return None };
    let arg = rs.argument.as_ref()?;
    let body_lowered = lower(arg, &scope_names)?;

    let body_ir = if local_inits.is_empty() {
        body_lowered
    } else {
        let mut seq = local_inits;
        seq.push(body_lowered);
        MExpr::SeqLet(seq)
    };
    Some(PureFn { params, body: body_ir })
}

pub fn call_padded(f: &PureFn, args: &[JsValue]) -> Option<JsValue> {
    let mut padded: Vec<JsValue> = args.to_vec();
    while padded.len() < f.params.len() { padded.push(JsValue::Undefined); }
    let mut locals: HashMap<String, JsValue> = HashMap::default();
    for (i, name) in f.params.iter().enumerate() {
        locals.insert(name.clone(), padded[i].clone());
    }
    eval_mba_with(&f.body, &padded, &mut locals)
}
