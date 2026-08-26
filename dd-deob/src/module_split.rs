use oxc_allocator::{Allocator, Vec as AVec};
use oxc_ast::AstBuilder;
use oxc_ast::ast::*;
use oxc_span::SPAN;
use oxc_syntax::operator::UnaryOperator;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BundleType {
    Captcha,
    Interstitial,
    Tags,
    Unknown,
}

impl BundleType {
    pub fn as_str(&self) -> &'static str {
        match self {
            BundleType::Captcha => "captcha",
            BundleType::Interstitial => "interstitial",
            BundleType::Tags => "tags",
            BundleType::Unknown => "unknown",
        }
    }
}

const INTERSTITIAL_LEGENDA: &[&str] = &[
    "reloader", "interstitial", "obfuscate", "helpers", "vm-obf", "localstorage",
    "new_file_1", "new_file_2", "new_file_3", "new_file_4", "new_file_5",
    "new_file_6", "new_file_7", "new_file_8", "new_file_9", "new_file_10",
];

pub struct SplitModule<'a> {
    pub name: String,
    pub program: Program<'a>,
}

pub fn split<'a>(program: &mut Program<'a>, alloc: &'a Allocator) -> (BundleType, Vec<SplitModule<'a>>) {
    let body: &mut AVec<'a, Statement<'a>> = &mut program.body;

    let bundler_idx = match find_bundler(body) {
        Some(b) => b,
        None => return (BundleType::Unknown, Vec::new()),
    };

    match bundler_idx.kind {
        BundleType::Interstitial => split_interstitial(body, bundler_idx.index, alloc),
        BundleType::Captcha => split_captcha(body, bundler_idx.index, alloc),
        BundleType::Tags => split_tags(body, bundler_idx.index, alloc),
        BundleType::Unknown => (BundleType::Unknown, Vec::new()),
    }
}

struct BundlerHit {
    index: usize,
    kind: BundleType,
}

fn find_bundler<'a>(body: &AVec<'a, Statement<'a>>) -> Option<BundlerHit> {
    for (i, stmt) in body.iter().enumerate() {
        if let Statement::VariableDeclaration(vd) = stmt {
            for d in &vd.declarations {
                let Some(Expression::CallExpression(call)) = &d.init else { continue };
                if !call.arguments.is_empty() { continue; }
                let body_len = match &call.callee {
                    Expression::ArrowFunctionExpression(af) => af.body.statements.len(),
                    Expression::FunctionExpression(fe) => fe.body.as_ref().map(|b| b.statements.len()).unwrap_or(0),
                    _ => 0,
                };
                if body_len >= 5 {
                    return Some(BundlerHit { index: i, kind: BundleType::Tags });
                }
            }
        }
        let Statement::ExpressionStatement(es) = stmt else { continue };
        match &es.expression {
            Expression::UnaryExpression(u) if u.operator == UnaryOperator::LogicalNot => {
                if let Expression::CallExpression(call) = &u.argument {
                    if matches!(&call.callee, Expression::FunctionExpression(_))
                        && !call.arguments.is_empty()
                    {
                        return Some(BundlerHit { index: i, kind: BundleType::Captcha });
                    }
                }
            }
            Expression::CallExpression(call) => {
                if let Expression::FunctionExpression(fe) = &call.callee {
                    if let Some(first) = fe.body.as_ref().and_then(|b| b.statements.first()) {
                        if let Statement::VariableDeclaration(vd) = first {
                            if let Some(d0) = vd.declarations.first() {
                                if matches!(d0.init.as_ref(), Some(Expression::ObjectExpression(o)) if !o.properties.is_empty()) {
                                    return Some(BundlerHit { index: i, kind: BundleType::Interstitial });
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

fn split_tags<'a>(
    body: &mut AVec<'a, Statement<'a>>,
    idx: usize,
    alloc: &'a Allocator,
) -> (BundleType, Vec<SplitModule<'a>>) {
    let ast = AstBuilder::new(alloc);
    let stmt = body.get_mut(idx).unwrap();
    let Statement::VariableDeclaration(vd) = stmt else { return (BundleType::Tags, Vec::new()) };
    let Some(d) = vd.declarations.first_mut() else { return (BundleType::Tags, Vec::new()) };
    let Some(init) = &mut d.init else { return (BundleType::Tags, Vec::new()) };
    let Expression::CallExpression(call) = init else { return (BundleType::Tags, Vec::new()) };

    let stmts: AVec<'a, Statement<'a>> = match &mut call.callee {
        Expression::ArrowFunctionExpression(af) => {
            let mut s = AVec::new_in(alloc);
            std::mem::swap(&mut s, &mut af.body.statements);
            s
        }
        Expression::FunctionExpression(fe) => {
            let Some(b) = fe.body.as_mut() else { return (BundleType::Tags, Vec::new()) };
            let mut s = AVec::new_in(alloc);
            std::mem::swap(&mut s, &mut b.statements);
            s
        }
        _ => return (BundleType::Tags, Vec::new()),
    };

    (BundleType::Tags, vec![SplitModule {
        name: "tags".into(),
        program: build_program(alloc, &ast, stmts),
    }])
}

fn split_interstitial<'a>(
    body: &mut AVec<'a, Statement<'a>>,
    idx: usize,
    alloc: &'a Allocator,
) -> (BundleType, Vec<SplitModule<'a>>) {
    let ast = AstBuilder::new(alloc);
    let mut out = Vec::new();
    let stmt = body.get_mut(idx).unwrap();
    let Statement::ExpressionStatement(es) = stmt else { return (BundleType::Interstitial, out) };
    let Expression::CallExpression(call) = &mut es.expression else { return (BundleType::Interstitial, out) };
    let Expression::FunctionExpression(fe) = &mut call.callee else { return (BundleType::Interstitial, out) };
    let Some(fbody) = fe.body.as_mut() else { return (BundleType::Interstitial, out) };

    let _ = ast;
    let main_body_len = fbody.statements.len();
    if main_body_len == 0 {
        return (BundleType::Interstitial, out);
    }

    let mut name_idx = 0;
    let mut seen: Vec<String> = Vec::new();
    if let Statement::VariableDeclaration(vd) = &mut fbody.statements[0] {
        if let Some(d0) = vd.declarations.first_mut() {
            if let Some(Expression::ObjectExpression(obj)) = d0.init.as_mut() {
                for prop_kind in obj.properties.iter_mut() {
                    let ObjectPropertyKind::ObjectProperty(prop) = prop_kind else { continue };
                    let key_name = match &prop.key {
                        PropertyKey::StringLiteral(s) => Some(s.value.as_str().to_string()),
                        PropertyKey::NumericLiteral(n) => Some(format!("{}", n.value as i64)),
                        PropertyKey::StaticIdentifier(id) => Some(id.name.as_str().to_string()),
                        _ => None,
                    };
                    let Some(k) = key_name else { continue };
                    if seen.contains(&k) { continue; }
                    seen.push(k);
                    let module_name = INTERSTITIAL_LEGENDA.get(name_idx).copied().unwrap_or("extra").to_string();
                    name_idx += 1;
                    if let Expression::FunctionExpression(value_fn) = &mut prop.value {
                        if let Some(vbody) = value_fn.body.as_mut() {
                            let mut stmts = AVec::new_in(alloc);
                            std::mem::swap(&mut stmts, &mut vbody.statements);
                            out.push(SplitModule { name: module_name, program: build_program(alloc, &ast, stmts) });
                        }
                    }
                }
            }
        }
    }

    let mut last_block_stmts: Option<AVec<Statement<'a>>> = None;
    if let Some(Statement::TryStatement(t)) = fbody.statements.get_mut(main_body_len - 1) {
        let mut stmts = AVec::new_in(alloc);
        std::mem::swap(&mut stmts, &mut t.block.body);
        last_block_stmts = Some(stmts);
    }
    if let Some(stmts) = last_block_stmts {
        out.push(SplitModule { name: "main".into(), program: build_program(alloc, &ast, stmts) });
    }

    (BundleType::Interstitial, out)
}

fn split_captcha<'a>(
    body: &mut AVec<'a, Statement<'a>>,
    idx: usize,
    alloc: &'a Allocator,
) -> (BundleType, Vec<SplitModule<'a>>) {
    let ast = AstBuilder::new(alloc);
    let mut out = Vec::new();

    let stmt = body.get_mut(idx).unwrap();
    let Statement::ExpressionStatement(es) = stmt else { return (BundleType::Captcha, out) };
    let Expression::UnaryExpression(u) = &mut es.expression else { return (BundleType::Captcha, out) };
    let Expression::CallExpression(call) = &mut u.argument else { return (BundleType::Captcha, out) };

    let mut entry_id: Option<i64> = None;
    if call.arguments.len() >= 3 {
        if let Argument::ArrayExpression(arr) = &call.arguments[2] {
            if let Some(first) = arr.elements.first() {
                if let ArrayExpressionElement::NumericLiteral(n) = first {
                    entry_id = Some(n.value as i64);
                } else if let ArrayExpressionElement::StringLiteral(s) = first {
                    if let Ok(n) = s.value.as_str().parse::<i64>() {
                        entry_id = Some(n);
                    }
                }
            }
        }
    }

    let mut legenda: rustc_hash::FxHashMap<i64, String> = Default::default();
    if let Expression::FunctionExpression(fe) = &call.callee {
        if let Some(fbody) = &fe.body {
            if let Some(Statement::VariableDeclaration(vd)) = fbody.statements.first() {
                for d in &vd.declarations {
                    if let Some(Expression::ObjectExpression(o)) = &d.init {
                        for pk in &o.properties {
                            if let ObjectPropertyKind::ObjectProperty(p) = pk {
                                if let Expression::ArrayExpression(arr) = &p.value {
                                    if arr.elements.len() == 2 {
                                        if let ArrayExpressionElement::ObjectExpression(deps) = &arr.elements[1] {
                                            for dpk in &deps.properties {
                                                if let ObjectPropertyKind::ObjectProperty(dp) = dpk {
                                                    let key_name = match &dp.key {
                                                        PropertyKey::StringLiteral(s) => Some(s.value.as_str().to_string()),
                                                        PropertyKey::StaticIdentifier(id) => Some(id.name.as_str().to_string()),
                                                        _ => None,
                                                    };
                                                    let val_num = match &dp.value {
                                                        Expression::NumericLiteral(n) => Some(n.value as i64),
                                                        Expression::StringLiteral(s) => s.value.as_str().parse::<i64>().ok(),
                                                        _ => None,
                                                    };
                                                    if let (Some(k), Some(v)) = (key_name, val_num) {
                                                        legenda.entry(v).or_insert(format_module_path(&k));
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    let module_obj = match call.arguments.first_mut() {
        Some(Argument::ObjectExpression(o)) => Some(o),
        _ => None,
    };
    let Some(module_obj) = module_obj else { return (BundleType::Captcha, out) };

    if legenda.is_empty() {
        for pk in &module_obj.properties {
            if let ObjectPropertyKind::ObjectProperty(p) = pk {
                if let Expression::ArrayExpression(arr) = &p.value {
                    if arr.elements.len() == 2 {
                        if let ArrayExpressionElement::ObjectExpression(deps) = &arr.elements[1] {
                            for dpk in &deps.properties {
                                if let ObjectPropertyKind::ObjectProperty(dp) = dpk {
                                    let key_name = match &dp.key {
                                        PropertyKey::StringLiteral(s) => Some(s.value.as_str().to_string()),
                                        PropertyKey::StaticIdentifier(id) => Some(id.name.as_str().to_string()),
                                        _ => None,
                                    };
                                    let val_num = match &dp.value {
                                        Expression::NumericLiteral(n) => Some(n.value as i64),
                                        Expression::StringLiteral(s) => s.value.as_str().parse::<i64>().ok(),
                                        _ => None,
                                    };
                                    if let (Some(k), Some(v)) = (key_name, val_num) {
                                        legenda.entry(v).or_insert(format_module_path(&k));
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    for pk in module_obj.properties.iter_mut() {
        let ObjectPropertyKind::ObjectProperty(p) = pk else { continue };
        let key_num: Option<i64> = match &p.key {
            PropertyKey::NumericLiteral(n) => Some(n.value as i64),
            PropertyKey::StringLiteral(s) => s.value.as_str().parse::<i64>().ok(),
            _ => None,
        };
        let Some(kn) = key_num else { continue };
        let module_name = if Some(kn) == entry_id {
            "main".to_string()
        } else {
            legenda.get(&kn).cloned().unwrap_or_else(|| format!("module_{}", kn))
        };
        if let Expression::ArrayExpression(arr) = &mut p.value {
            if let Some(ArrayExpressionElement::FunctionExpression(fe)) = arr.elements.first_mut() {
                if let Some(fbody) = fe.body.as_mut() {
                    let mut stmts = AVec::new_in(alloc);
                    std::mem::swap(&mut stmts, &mut fbody.statements);
                    out.push(SplitModule { name: module_name, program: build_program(alloc, &ast, stmts) });
                }
            }
        }
    }

    (BundleType::Captcha, out)
}

fn format_module_path(raw: &str) -> String {
    let mut s = raw.to_string();
    if s.starts_with("./") {
        let segs: Vec<&str> = s.split('/').collect();
        if segs.len() > 2 {
            s = format!("./{}", segs.last().unwrap());
        }
    } else if s.contains('/') {
        s = format!("./{}", s.split('/').last().unwrap());
    }
    s = s.trim_start_matches("./").to_string();
    if s.ends_with(".js") {
        s.truncate(s.len() - 3);
    }
    s
}

fn build_program<'a>(
    alloc: &'a Allocator,
    ast: &AstBuilder<'a>,
    stmts: AVec<'a, Statement<'a>>,
) -> Program<'a> {
    let directives = AVec::new_in(alloc);
    ast.program(
        SPAN,
        oxc_span::SourceType::cjs(),
        "",
        oxc_allocator::Vec::new_in(alloc),
        None,
        directives,
        stmts,
    )
}
