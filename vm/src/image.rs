use oxc_allocator::Allocator;
use oxc_ast::ast::*;
use oxc_ast_visit::{Visit, walk};
use oxc_parser::{ParseOptions, Parser};
use oxc_span::SourceType;

pub struct Boot {
    pub api: crate::api::Api,
    pub consts: crate::konst::Consts,
    pub code: Vec<u8>,
    pub length: usize,
    pub lo: usize,
    pub hi: usize,
    pub seed: i32,
}

impl Boot {
    pub fn build(&self) -> Vec<i32> {
        let mut rng = Xorshift { state: self.seed, count: 0 };
        (0..self.length)
            .map(|i| {
                if (self.lo..self.hi).contains(&i) {
                    self.code[i - self.lo] as i32
                } else {
                    rng.next()
                }
            })
            .collect()
    }
}

struct Xorshift {
    state: i32,
    count: i32,
}

impl Xorshift {
    fn next(&mut self) -> i32 {
        let old = self.count;
        self.count = self.count.wrapping_add(1);
        if old & 3 == 0 {
            self.state ^= self.state << 13;
            self.state ^= self.state >> 17;
            self.state ^= self.state << 5;
        }
        (self.state >> ((self.count & 3) << 3)) % 256
    }
}

pub fn definitions(
    img: &[i32],
    lo: usize,
    define: u8,
) -> (std::collections::BTreeMap<u8, String>, usize) {
    let code = &img[lo..];
    let mut out = std::collections::BTreeMap::new();
    let mut ip = 0usize;
    while code.get(ip).copied() == Some(define as i32) {
        let op = code[ip + 1] as u8;
        let len = ((code[ip + 2] as usize) << 16) | ((code[ip + 3] as usize) << 8) | code[ip + 4] as usize;
        ip += 5;
        let src: String =
            code[ip..ip + len].iter().map(|c| char::from_u32(*c as u32).unwrap()).collect();
        ip += len;
        out.insert(op, src);
    }
    (out, ip)
}

pub fn boot(source: &str) -> Option<Boot> {
    let alloc = Allocator::default();
    let ret = Parser::new(&alloc, source, SourceType::mjs())
        .with_options(ParseOptions { preserve_parens: false, ..ParseOptions::default() })
        .parse();
    let mut find = Find { boot: None, seed: None };
    find.visit_program(&ret.program);
    let (code, length, lo, hi) = find.boot?;
    let scoping = oxc_semantic::SemanticBuilder::new().build(&ret.program).semantic.into_scoping();
    let api = crate::api::api(&ret.program, &scoping, lo as i64)?;
    let consts = crate::konst::consts(&ret.program)?;
    Some(Boot { code, length, lo, hi, seed: find.seed?, api, consts })
}

struct Find {
    boot: Option<(Vec<u8>, usize, usize, usize)>,
    seed: Option<i32>,
}

impl<'a> Visit<'a> for Find {
    fn visit_call_expression(&mut self, c: &CallExpression<'a>) {
        walk::walk_call_expression(self, c);
        let Expression::FunctionExpression(f) = &c.callee else { return };
        let [Argument::StringLiteral(payload)] = c.arguments.as_slice() else { return };
        let Some(body) = f.body.as_deref() else { return };

        let mut shape = Shape { length: None, window: None };
        shape.visit_function_body(body);
        let (Some(length), Some((lo, hi))) = (shape.length, shape.window) else { return };

        let Some(code) = decode(&payload.value) else { return };
        self.boot = Some((code, length, lo, hi));
    }

    fn visit_function(&mut self, f: &Function<'a>, flags: oxc_syntax::scope::ScopeFlags) {
        walk::walk_function(self, f, flags);
        let Some(body) = f.body.as_deref() else { return };
        let mut shifts = Shifts { seen: Vec::new() };
        shifts.visit_function_body(body);
        if !shifts.seen.windows(3).any(|w| w == [13.0, 17.0, 5.0]) {
            return;
        }
        let [Statement::VariableDeclaration(d), Statement::ReturnStatement(_)] =
            body.statements.as_slice()
        else {
            return;
        };
        if let Some(Expression::NumericLiteral(n)) = &d.declarations.first().and_then(|d| d.init.as_ref())
        {
            self.seed = Some(n.value as i64 as i32);
        }
    }
}

struct Shape {
    length: Option<usize>,
    window: Option<(usize, usize)>,
}

impl<'a> Visit<'a> for Shape {
    fn visit_object_property(&mut self, p: &ObjectProperty<'a>) {
        walk::walk_object_property(self, p);
        if let (PropertyKey::StaticIdentifier(k), Expression::NumericLiteral(n)) =
            (&p.key, &p.value)
            && k.name == "length"
        {
            self.length = Some(n.value as usize);
        }
    }

    fn visit_logical_expression(&mut self, l: &LogicalExpression<'a>) {
        walk::walk_logical_expression(self, l);
        if l.operator != LogicalOperator::And {
            return;
        }
        let (Expression::BinaryExpression(lo), Expression::BinaryExpression(hi)) =
            (&l.left, &l.right)
        else {
            return;
        };
        if lo.operator != BinaryOperator::GreaterEqualThan || hi.operator != BinaryOperator::LessThan
        {
            return;
        }
        let (Expression::NumericLiteral(a), Expression::NumericLiteral(b)) = (&lo.right, &hi.right)
        else {
            return;
        };
        self.window = Some((a.value as usize, b.value as usize));
    }
}

struct Shifts {
    seen: Vec<f64>,
}

impl<'a> Visit<'a> for Shifts {
    fn visit_binary_expression(&mut self, b: &BinaryExpression<'a>) {
        walk::walk_binary_expression(self, b);
        if matches!(b.operator, BinaryOperator::ShiftLeft | BinaryOperator::ShiftRight)
            && let Expression::NumericLiteral(n) = &b.right
        {
            self.seen.push(n.value);
        }
    }
}

fn decode(input: &str) -> Option<Vec<u8>> {
    const ALPHABET: &[u8] = b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
    let (mut bits, mut have) = (0u32, 0u32);
    let mut out = Vec::with_capacity(input.len() * 3 / 4);
    for ch in input.bytes() {
        if ch == b'=' {
            break;
        }
        let v = ALPHABET.iter().position(|c| *c == ch)? as u32;
        bits = bits << 6 | v;
        have += 6;
        if have >= 8 {
            have -= 8;
            out.push((bits >> have) as u8);
        }
    }
    Some(out)
}
