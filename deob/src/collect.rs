use oxc_ast::ast::*;
use oxc_ast_visit::{Visit, walk};
use oxc_semantic::{ScopeFlags, SymbolId};
use oxc_syntax::operator::{AssignmentOperator, BinaryOperator as Bin, UnaryOperator as Un};
use rustc_hash::{FxHashMap as Map, FxHashSet as Set};

pub struct Node {
    pub params: Vec<String>,
    pub locals: Vec<(String, Expr)>,
    pub body: Expr,
}

pub enum Expr {
    Lit(f64),
    Var(String),
    Assign(String, Box<Expr>),
    Un(Un, Box<Expr>),
    Bin(Bin, Box<Expr>, Box<Expr>),
    Seq(Vec<Expr>),
}

#[derive(Default)]
pub struct Collect {
    pub fns: Map<SymbolId, Node>,
    pub char_code: Set<SymbolId>,
    pub globals: Set<SymbolId>,
}

impl<'a> Visit<'a> for Collect {
    fn visit_function(&mut self, f: &Function<'a>, flags: ScopeFlags) {
        walk::walk_function(self, f, flags);
        if let (Some(id), Some(node)) = (&f.id, node(&f.params, f.body.as_deref())) {
            self.fns.insert(id.symbol_id(), node);
        }
    }

    fn visit_variable_declarator(&mut self, d: &VariableDeclarator<'a>) {
        walk::walk_variable_declarator(self, d);
        let (BindingPattern::BindingIdentifier(id), Some(init)) = (&d.id, &d.init) else { return };
        let id = id.symbol_id();
        match init {
            Expression::FunctionExpression(f) => {
                if let Some(n) = node(&f.params, f.body.as_deref()) {
                    self.fns.insert(id, n);
                }
            }
            Expression::StaticMemberExpression(m)
                if matches!(&m.object, Expression::Identifier(o) if o.name == "String")
                    && m.property.name == "fromCharCode" =>
            {
                self.char_code.insert(id);
            }
            Expression::Identifier(g) if matches!(&*g.name, "window" | "globalThis" | "self") => {
                self.globals.insert(id);
            }
            _ => {}
        }
    }
}

fn node(params: &FormalParameters, body: Option<&FunctionBody>) -> Option<Node> {
    let body = body?;
    let mut names = Vec::with_capacity(params.items.len());
    for p in &params.items {
        let BindingPattern::BindingIdentifier(b) = &p.pattern else { return None };
        names.push(b.name.to_string());
    }

    let mut locals = Vec::new();
    let mut result = None;
    for s in &body.statements {
        match s {
            Statement::VariableDeclaration(d) => {
                for v in &d.declarations {
                    let (BindingPattern::BindingIdentifier(b), Some(init)) = (&v.id, &v.init) else {
                        return None;
                    };
                    locals.push((b.name.to_string(), expr(init)?));
                }
            }
            Statement::ReturnStatement(r) => result = Some(expr(r.argument.as_ref()?)?),
            _ => return None,
        }
    }
    Some(Node { params: names, locals, body: result? })
}

fn expr(e: &Expression) -> Option<Expr> {
    Some(match e {
        Expression::NumericLiteral(n) => Expr::Lit(n.value),
        Expression::Identifier(i) => Expr::Var(i.name.to_string()),
        Expression::UnaryExpression(u) => Expr::Un(u.operator, Box::new(expr(&u.argument)?)),
        Expression::BinaryExpression(b) => {
            Expr::Bin(b.operator, Box::new(expr(&b.left)?), Box::new(expr(&b.right)?))
        }
        Expression::SequenceExpression(s) => {
            Expr::Seq(s.expressions.iter().map(expr).collect::<Option<_>>()?)
        }
        Expression::AssignmentExpression(a) if a.operator == AssignmentOperator::Assign => {
            let AssignmentTarget::AssignmentTargetIdentifier(t) = &a.left else { return None };
            Expr::Assign(t.name.to_string(), Box::new(expr(&a.right)?))
        }
        _ => return None,
    })
}
