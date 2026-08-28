use oxc_allocator::Allocator;
use oxc_ast::ast::*;
use oxc_ast_visit::{Visit, walk};
use oxc_parser::{ParseOptions, Parser};
use oxc_span::SourceType;

#[derive(Debug)]
pub struct Spec {
    pub mixer: i64,
    pub empty: i32,
    pub clock: i64,
    pub second: i64,
    pub build: i64,
    pub open: u8,
    pub separator: u8,
    pub colon: u8,
    pub close: u8,
    pub alphabet: [i64; 8],
    pub skipped: Vec<String>,
    pub injected: Option<(String, f64)>,
}

pub fn configured(list: &ArrayExpression) -> Option<f64> {
    if list.elements.len() < 3 {
        return None;
    }
    let arrays = list
        .elements
        .iter()
        .take(2)
        .all(|element| matches!(element, ArrayExpressionElement::ArrayExpression(_)));
    if !arrays {
        return None;
    }
    let ArrayExpressionElement::ObjectExpression(config) = list.elements.last()? else {
        return None;
    };
    for entry in &config.properties {
        let ObjectPropertyKind::ObjectProperty(entry) = entry else { continue };
        if let Some(found) = number(&entry.value) {
            if found.abs() > 1e6 {
                return Some(found);
            }
        }
    }
    None
}

fn number(value: &Expression) -> Option<f64> {
    match value {
        Expression::NumericLiteral(literal) => Some(literal.value),
        Expression::UnaryExpression(unary) if unary.operator == UnaryOperator::UnaryNegation => {
            number(&unary.argument).map(|found| -found)
        }
        _ => None,
    }
}

fn literal(value: &Expression) -> Option<String> {
    match value {
        Expression::StringLiteral(text) => Some(text.value.to_string()),
        _ => None,
    }
}

fn leaves(value: &Expression, out: &mut Vec<f64>) {
    match value {
        Expression::BinaryExpression(binary) => {
            leaves(&binary.left, out);
            leaves(&binary.right, out);
        }
        other => {
            if let Some(found) = number(other) {
                out.push(found);
            }
        }
    }
}

struct Returns {
    found: Vec<f64>,
}

impl<'a> Visit<'a> for Returns {
    fn visit_return_statement(&mut self, node: &ReturnStatement<'a>) {
        if let Some(value) = node.argument.as_ref().and_then(number) {
            self.found.push(value);
        }
        walk::walk_return_statement(self, node);
    }
}

fn hashes(body: &FunctionBody) -> bool {
    struct Look {
        seen: bool,
    }
    impl<'a> Visit<'a> for Look {
        fn visit_call_expression(&mut self, node: &CallExpression<'a>) {
            let name = match &node.callee {
                Expression::StaticMemberExpression(member) => Some(member.property.name.to_string()),
                Expression::ComputedMemberExpression(member) => literal(&member.expression),
                _ => None,
            };
            if name.as_deref() == Some("charCodeAt") {
                self.seen = true;
            }
            walk::walk_call_expression(self, node);
        }
    }
    let mut look = Look { seen: false };
    look.visit_function_body(body);
    look.seen
}

#[derive(Default)]
struct Scan {
    mixer: Option<f64>,
    clock: Option<f64>,
    empty: Option<f64>,
    build: Option<f64>,
    open: Option<f64>,
    separator: Option<f64>,
    marks: Vec<f64>,
    mixed: Vec<f64>,
    alphabet: Vec<f64>,
    skipped: Vec<String>,
    injected: Option<(String, f64)>,
}

impl<'a> Visit<'a> for Scan {
    fn visit_binary_expression(&mut self, node: &BinaryExpression<'a>) {
        if node.operator == BinaryOperator::BitwiseXOR {
            if let Expression::BinaryExpression(shift) = &node.left {
                if shift.operator == BinaryOperator::ShiftRight
                    && number(&shift.right) == Some(3.0)
                {
                    self.clock = number(&node.right);
                }
            }
            if let (Some(found), Expression::CallExpression(_)) = (number(&node.left), &node.right) {
                self.mixed.push(found);
            }
        }
        if node.operator == BinaryOperator::Multiplication {
            if let Some(found) = number(&node.right) {
                if found > 1e9 {
                    self.mixer = Some(found);
                }
            }
        }
        walk::walk_binary_expression(self, node);
    }

    fn visit_function(&mut self, node: &Function<'a>, flags: oxc_syntax::scope::ScopeFlags) {
        if let Some(body) = &node.body {
            if self.empty.is_none() && hashes(body) {
                let mut look = Returns { found: Vec::new() };
                look.visit_function_body(body);
                self.empty = look.found.iter().copied().find(|found| found.abs() > 1e6);
            }
        }
        walk::walk_function(self, node, flags);
    }

    fn visit_return_statement(&mut self, node: &ReturnStatement<'a>) {
        if let Some(Expression::ConditionalExpression(chain)) = &node.argument {
            let mut found = Vec::new();
            let mut step = Some(chain);
            while let Some(level) = step {
                if let Expression::BinaryExpression(test) = &level.test {
                    if let Some(bound) = number(&test.left).or_else(|| number(&test.right)) {
                        found.push(bound);
                    }
                }
                leaves(&level.consequent, &mut found);
                match &level.alternate {
                    Expression::ConditionalExpression(next) => step = Some(next),
                    other => {
                        leaves(other, &mut found);
                        step = None;
                    }
                }
            }
            if found.len() == 8 && self.alphabet.is_empty() {
                self.alphabet = found;
            }
        }
        walk::walk_return_statement(self, node);
    }

    fn visit_call_expression(&mut self, node: &CallExpression<'a>) {
        if let Expression::StaticMemberExpression(member) = &node.callee {
            if member.property.name == "push" && node.arguments.len() == 1 {
                if let Some(Expression::BinaryExpression(mixed)) = node.arguments[0].as_expression()
                {
                    if mixed.operator == BinaryOperator::BitwiseXOR {
                        match (&mixed.left, &mixed.right) {
                            (Expression::ConditionalExpression(choice), _)
                            | (_, Expression::ConditionalExpression(choice)) => {
                                if let (Some(first), Some(rest)) =
                                    (number(&choice.consequent), number(&choice.alternate))
                                {
                                    self.separator = Some(first);
                                    self.open = Some(rest);
                                }
                            }
                            _ => {
                                let mut found = Vec::new();
                                leaves(&mixed.left, &mut found);
                                if let Some(mark) = found.first() {
                                    self.marks.push(*mark);
                                }
                            }
                        }
                    }
                }
            }
        }
        if node.arguments.len() == 2 {
            if let (Some(name), Some(Expression::LogicalExpression(choice))) = (
                node.arguments.first().and_then(|a| a.as_expression()).and_then(literal),
                node.arguments.get(1).and_then(|a| a.as_expression()),
            ) {
                if let Some(fallback) = number(&choice.right) {
                    self.injected = Some((name, fallback));
                }
            }
        }
        walk::walk_call_expression(self, node);
    }

    fn visit_logical_expression(&mut self, node: &LogicalExpression<'a>) {
        if node.operator == LogicalOperator::And {
            for side in [&node.left, &node.right] {
                if let Expression::BinaryExpression(compare) = side {
                    if compare.operator == BinaryOperator::StrictInequality {
                        if let Some(name) = literal(&compare.right) {
                            self.skipped.push(name);
                        }
                    }
                }
            }
        }
        walk::walk_logical_expression(self, node);
    }

    fn visit_array_expression(&mut self, node: &ArrayExpression<'a>) {
        if let Some(found) = configured(node) {
            self.build = Some(found);
        }
        walk::walk_array_expression(self, node);
    }
}

pub fn spec(source: &str) -> Option<Spec> {
    let allocator = Allocator::default();
    let parsed = Parser::new(&allocator, source, SourceType::mjs())
        .with_options(ParseOptions { preserve_parens: false, ..ParseOptions::default() })
        .parse();
    let mut scan = Scan::default();
    scan.visit_program(&parsed.program);

    let mut alphabet = [0i64; 8];
    for (slot, value) in alphabet.iter_mut().zip(&scan.alphabet) {
        *slot = *value as i64;
    }

    let colon = scan.marks.iter().copied().find(|mark| *mark == 58.0)?;
    let close = scan.marks.iter().copied().find(|mark| *mark == 125.0)?;
    let mixer = scan.mixer?;
    let second = scan.mixed.iter().copied().find(|found| *found != mixer && found.abs() > 1e6)?;

    Some(Spec {
        mixer: mixer as i64,
        empty: scan.empty? as i32,
        clock: scan.clock? as i64,
        second: second as i64,
        build: scan.build? as i64,
        open: scan.open? as u8,
        separator: scan.separator? as u8,
        colon: colon as u8,
        close: close as u8,
        alphabet,
        skipped: scan.skipped,
        injected: scan.injected,
    })
}

impl Spec {
    fn hash(&self, text: &str) -> i32 {
        if text.is_empty() {
            return self.empty;
        }
        let mut found: i32 = 0;
        for unit in text.encode_utf16() {
            found = found.wrapping_shl(5).wrapping_sub(found).wrapping_add(unit as i32);
        }
        if found == 0 { self.empty } else { found }
    }

    fn digit(&self, value: i64) -> u8 {
        let [a, b, c, d, e, f, g, h] = self.alphabet;
        let found = if value > a {
            b + value
        } else if value > c {
            d + value
        } else if value > e {
            f + value
        } else {
            g * value + h
        };
        found as u8
    }

    pub fn nonce(&self, now: f64) -> i32 {
        let shifted = (now as i64 >> 3) ^ self.clock;
        let scaled = (shifted as f64) * (self.mixer as f64);
        shift(shift(to_i32(scaled)))
    }
}

fn to_i32(value: f64) -> i32 {
    let wrapped = value % 4294967296.0;
    let wrapped = if wrapped < 0.0 { wrapped + 4294967296.0 } else { wrapped };
    wrapped as u32 as i32
}

fn shift(mut state: i32) -> i32 {
    state ^= state << 13;
    state ^= ((state as u32) >> 17) as i32;
    state ^ (state << 5)
}

struct Stream {
    state: i32,
    step: i32,
    nonce: i32,
    keyed: bool,
    held: Option<u8>,
}

impl Stream {
    fn new(state: i32) -> Self {
        Stream { state, step: -1, nonce: 0, keyed: false, held: None }
    }

    fn keyed(state: i32, nonce: i32) -> Self {
        Stream { state, step: -1, nonce, keyed: true, held: None }
    }

    fn take(&mut self, hold: bool) -> u8 {
        if let Some(found) = self.held.take() {
            return found;
        }
        self.step += 1;
        if self.step > 2 {
            self.state = shift(self.state);
            self.step = 0;
        }
        let mask = if self.keyed {
            self.nonce = self.nonce.wrapping_sub(1);
            self.nonce
        } else {
            0
        };
        let found = (((self.state >> (16 - 8 * self.step)) ^ mask) & 255) as u8;
        if hold {
            self.held = Some(found);
        }
        found
    }

    fn next(&mut self) -> u8 {
        self.take(false)
    }
}

fn quote(text: &str) -> Vec<u8> {
    let mut out = String::from("\"");
    for c in text.chars() {
        match c {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            c if (c as u32) < 0x20 => out.push_str(&format!("\\u{:04x}", c as u32)),
            c => out.push(c),
        }
    }
    out.push('"');
    out.into_bytes()
}

pub fn encode(spec: &Spec, key: &str, cid: &str, fields: &[(String, String)], now: f64) -> String {
    let nonce = spec.nonce(now);
    let seed = (spec.mixer as i32) ^ spec.hash(key) ^ (spec.build as i32);
    let mut stream = Stream::keyed(seed, nonce);

    let mut plain: Vec<u8> = Vec::new();
    for (at, (name, value)) in fields.iter().enumerate() {
        let mark = if at == 0 { spec.open } else { spec.separator };
        plain.push(stream.next() ^ mark);
        for byte in quote(name) {
            plain.push(byte ^ stream.next());
        }
        plain.push(spec.colon ^ stream.next());
        for byte in value.bytes() {
            plain.push(byte ^ stream.next());
        }
    }

    let mut second = Stream::new((spec.second as i32) ^ spec.hash(cid));
    let mut mixed: Vec<u8> = plain.iter().map(|byte| byte ^ second.next()).collect();
    mixed.push(spec.close ^ stream.take(true) ^ second.next());

    let mut counter = nonce;
    let mut pad = || {
        counter = counter.wrapping_sub(1);
        counter as u8
    };

    let mut out = String::new();
    let mut at = 0;
    while at < mixed.len() {
        let first = (pad() ^ mixed[at]) as i64;
        let middle = (pad() ^ mixed.get(at + 1).copied().unwrap_or(0)) as i64;
        let last = (pad() ^ mixed.get(at + 2).copied().unwrap_or(0)) as i64;
        at += 3;
        let group = (first << 16) | (middle << 8) | last;
        out.push(spec.digit(group >> 18 & 63) as char);
        out.push(spec.digit(group >> 12 & 63) as char);
        out.push(spec.digit(group >> 6 & 63) as char);
        out.push(spec.digit(group & 63) as char);
    }
    let extra = mixed.len() % 3;
    if extra != 0 {
        out.truncate(out.len() - (3 - extra));
    }
    out
}

pub fn decode(spec: &Spec, key: &str, cid: &str, payload: &str) -> Option<String> {
    let mut back = [255usize; 256];
    for value in 0..64i64 {
        back[spec.digit(value) as usize] = value as usize;
    }

    let digits: Vec<usize> = payload.bytes().map(|byte| back[byte as usize]).collect();
    if digits.iter().any(|found| *found == 255) {
        return None;
    }

    let mut mixed: Vec<u8> = Vec::new();
    for group in digits.chunks(4) {
        let mut packed = 0usize;
        for (at, value) in group.iter().enumerate() {
            packed |= value << (18 - 6 * at);
        }
        mixed.push((packed >> 16) as u8);
        if group.len() > 2 {
            mixed.push((packed >> 8) as u8);
        }
        if group.len() > 3 {
            mixed.push(packed as u8);
        }
    }

    let seed = (spec.mixer as i32) ^ spec.hash(key) ^ (spec.build as i32);
    let mut stream = Stream::new(seed);
    let mut second = Stream::new((spec.second as i32) ^ spec.hash(cid));
    let plain: Vec<u8> = mixed
        .iter()
        .map(|byte| byte ^ second.next() ^ stream.next())
        .collect();
    Some(String::from_utf8_lossy(&plain).to_string())
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Fold {
    Restrict,
    Allow,
    Plain,
}

#[derive(Clone, Debug)]
pub enum Piece {
    Literal(String),
    Value(String),
}

pub struct Field {
    pub key: String,
    pub raw: String,
    pub value: String,
    pub deferred: bool,
    pub caught: bool,
    pub scope: String,
    pub folds: Vec<(Fold, Vec<Piece>)>,
}

struct Walk<'s> {
    source: &'s str,
    scope: String,
    found: Vec<Field>,
    emitters: Vec<String>,
    locals: Vec<(String, String)>,
    deferred: usize,
    caught: usize,
}

impl<'a, 's> Visit<'a> for Walk<'s> {
    fn visit_function(&mut self, node: &Function<'a>, flags: oxc_syntax::scope::ScopeFlags) {
        self.deferred += 1;
        walk::walk_function(self, node, flags);
        self.deferred -= 1;
    }

    fn visit_arrow_function_expression(&mut self, node: &ArrowFunctionExpression<'a>) {
        self.deferred += 1;
        walk::walk_arrow_function_expression(self, node);
        self.deferred -= 1;
    }

    fn visit_catch_clause(&mut self, node: &CatchClause<'a>) {
        self.caught += 1;
        walk::walk_catch_clause(self, node);
        self.caught -= 1;
    }

    fn visit_variable_declarator(&mut self, node: &VariableDeclarator<'a>) {
        if let (BindingPattern::BindingIdentifier(name), Some(value)) = (&node.id, &node.init) {
            let span = oxc_span::GetSpan::span(value);
            let text = self.source[span.start as usize..span.end as usize].to_string();
            self.locals.push((name.name.to_string(), text));
        }
        walk::walk_variable_declarator(self, node);
    }

    fn visit_assignment_expression(&mut self, node: &AssignmentExpression<'a>) {
        if let AssignmentTarget::AssignmentTargetIdentifier(name) = &node.left {
            let span = oxc_span::GetSpan::span(&node.right);
            let text = self.source[span.start as usize..span.end as usize].to_string();
            self.locals.push((name.name.to_string(), text));
        }
        walk::walk_assignment_expression(self, node);
    }

    fn visit_call_expression(&mut self, node: &CallExpression<'a>) {
        if let Some(kind) = folding(node) {
            if let Some(argument) = node.arguments.first().and_then(|a| a.as_expression()) {
                let pieces = self.pieces(argument);
                if let Some(last) = self.found.last_mut() {
                    last.folds.push((kind, pieces));
                }
            }
        }
        match &node.callee {
            Expression::FunctionExpression(inner) => {
                for argument in &node.arguments {
                    self.visit_argument(argument);
                }
                if let Some(body) = &inner.body {
                    self.visit_function_body(body);
                }
                return;
            }
            Expression::ArrowFunctionExpression(inner) => {
                for argument in &node.arguments {
                    self.visit_argument(argument);
                }
                walk::walk_arrow_function_body(self, &inner.body);
                return;
            }
            _ => {}
        }
        if let Expression::Identifier(callee) = &node.callee {
            let emits = self.emitters.iter().any(|name| name == callee.name.as_str());
            let named = node.arguments.first().and_then(|a| a.as_expression()).and_then(literal);
            let plain = !node
                .arguments
                .iter()
                .any(|a| matches!(a.as_expression(), Some(Expression::FunctionExpression(_) | Expression::ArrowFunctionExpression(_))));
            if emits && node.arguments.len() == 2 && plain {
                if let Some(key) = named {
                    let value = node
                        .arguments
                        .get(1)
                        .and_then(|a| a.as_expression())
                        .map(|found| {
                            let span = oxc_span::GetSpan::span(found);
                            self.source[span.start as usize..span.end as usize].to_string()
                        })
                        .unwrap_or_default();
                    let raw = value.clone();
                    let value = self.expand(&value);
                    self.found.push(Field {
                        key,
                        raw,
                        value,
                        deferred: self.deferred > 0,
                        caught: self.caught > 0,
                        scope: self.scope.clone(),
                        folds: Vec::new(),
                    });
                }
            }
        }
        walk::walk_call_expression(self, node);
    }
}

fn folding(node: &CallExpression) -> Option<Fold> {
    let name = match &node.callee {
        Expression::ComputedMemberExpression(member) => literal(&member.expression)?,
        Expression::StaticMemberExpression(member) => member.property.name.to_string(),
        _ => return None,
    };
    if node.arguments.len() != 1 {
        return None;
    }
    match name.as_str() {
        "N" => Some(Fold::Restrict),
        "A" => Some(Fold::Allow),
        "p" => Some(Fold::Plain),
        _ => None,
    }
}

impl<'s> Walk<'s> {
    fn pieces(&self, value: &Expression) -> Vec<Piece> {
        match value {
            Expression::BinaryExpression(binary)
                if binary.operator == BinaryOperator::Addition =>
            {
                let mut out = self.pieces(&binary.left);
                out.extend(self.pieces(&binary.right));
                out
            }
            Expression::StringLiteral(text) => vec![Piece::Literal(text.value.to_string())],
            other => {
                let span = oxc_span::GetSpan::span(other);
                let text = self.source[span.start as usize..span.end as usize].to_string();
                vec![Piece::Value(self.expand(&text))]
            }
        }
    }

    fn lookup(&self, name: &str) -> Option<&str> {
        self.locals
            .iter()
            .rev()
            .find(|(local, _)| local == name)
            .map(|(_, text)| text.as_str())
    }

    fn expand(&self, value: &str) -> String {
        let mut found = value.trim().to_string();
        for _ in 0..4 {
            let simple = found.chars().all(|c| c.is_alphanumeric() || c == '_' || c == '$');
            if !simple {
                break;
            }
            match self.lookup(&found) {
                Some(text) if text != found => found = text.trim().to_string(),
                _ => break,
            }
        }
        if let Some(at) = found.find("](") {
            let head = &found[..at + 2];
            let tail = &found[at + 2..];
            if let Some(inner) = tail.strip_suffix(')') {
                if inner.chars().all(|c| c.is_alphanumeric() || c == '_' || c == '$') {
                    if let Some(text) = self.lookup(inner) {
                        return format!("{head}{text})");
                    }
                }
            }
        }
        found
    }
}

pub fn fields(source: &str) -> Vec<Field> {
    let allocator = Allocator::default();
    let parsed = Parser::new(&allocator, source, SourceType::mjs())
        .with_options(ParseOptions { preserve_parens: false, ..ParseOptions::default() })
        .parse();

    struct Lists<'a> {
        probes: Vec<*const Function<'a>>,
    }

    impl<'a> Visit<'a> for Lists<'a> {
        fn visit_array_expression(&mut self, node: &ArrayExpression<'a>) {
            if configured(node).is_some() {
                for element in &node.elements {
                    let ArrayExpressionElement::ArrayExpression(inner) = element else {
                        continue;
                    };
                    for probe in &inner.elements {
                        if let Some(Expression::FunctionExpression(probe)) = probe.as_expression() {
                            self.probes.push(&**probe as *const Function<'a>);
                        }
                    }
                }
            }
            walk::walk_array_expression(self, node);
        }
    }

    let mut lists = Lists { probes: Vec::new() };
    lists.visit_program(&parsed.program);

    let mut walk =
        Walk {
            source,
            found: Vec::new(),
            scope: String::new(),
            emitters: Vec::new(),
            locals: Vec::new(),
            deferred: 0,
            caught: 0,
        };
    for probe in lists.probes {
        let probe = unsafe { &*probe };
        walk.emitters.clear();
        walk.locals.clear();
        for (at, parameter) in probe.params.items.iter().enumerate() {
            if at < 2 {
                if let BindingPattern::BindingIdentifier(name) = &parameter.pattern {
                    walk.emitters.push(name.name.to_string());
                }
            }
        }
        walk.scope = source
            .get(probe.span.start as usize..probe.span.end as usize)
            .unwrap_or_default()
            .to_string();
        if let Some(body) = &probe.body {
            walk.visit_function_body(body);
        }
    }
    walk.found
}

fn resolve(
    profile: &crate::profile::Profile,
    session: &Session,
    trace: Option<&str>,
    text: &str,
) -> Option<String> {
    let found = match text {
        "I[\"innerWidth\"] || 0" | "I[\"innerWidth\"]" => profile.inner_width().to_string(),
        "I[\"innerHeight\"] || 0" | "I[\"innerHeight\"]" => profile.inner_height().to_string(),
        "I[\"screen\"][\"availWidth\"] || 0" | "I[\"screen\"][\"availWidth\"]" => {
            profile.avail_width().to_string()
        }
        "I[\"screen\"][\"availHeight\"] || 0" | "I[\"screen\"][\"availHeight\"]" => {
            profile.avail_height().to_string()
        }
        "I[\"navigator\"][\"language\"] || I[\"navigator\"][\"userLanguage\"] || I[\"navigator\"][\"browserLanguage\"] || I[\"navigator\"][\"systemLanguage\"] || \"\"" => {
            quoted(profile.language)
        }
        "I[\"navigator\"][\"gpu\"][\"getPreferredCanvasFormat\"]()" => quoted("bgra8unorm"),
        "\"mbs: \" + _[\"limits\"][\"maxBufferSize\"] + \", msbbs: \" + _[\"limits\"][\"maxStorageBufferBindingSize\"]" => {
            quoted(&format!("mbs: {}, msbbs: {}", profile.buffer, profile.binding))
        }
        "I[\"Array\"][\"from\"](_[\"values\"]())[\"join\"]()"
        | "I[\"Array\"][\"from\"](_[\"values\"]())[\"toString\"]()" => quoted(profile.features),
        "void 0 !== _[\"quota\"] ? _[\"quota\"] : -1" => session.quota.to_string(),
        "void 0 !== _[\"usage\"] ? _[\"usage\"] : -1" => session.usage.to_string(),
        "!!(I[\"HTMLVideoElement\"] && I[\"HTMLVideoElement\"][\"prototype\"] && I[\"HTMLVideoElement\"][\"prototype\"][\"getVideoPlaybackQuality\"])" => {
            boolean(true)
        }
        "!!(I[\"external\"] && I[\"external\"][\"toString\"] && I[\"external\"][\"toString\"]()[\"indexOf\"](\"Sequentum\") > -1)" => {
            boolean(false)
        }
        "\"undefined\" != typeof objectToInspect && null === objectToInspect && \"undefined\" != typeof result && !!result" => {
            boolean(false)
        }
        "_[\"plu\"]" => quoted(
            "PDF Viewer,Chrome PDF Viewer,Chromium PDF Viewer,Microsoft Edge PDF Viewer,WebKit built-in PDF",
        ),
        "_[\"mmt\"]" => quoted("application/pdf,text/pdf"),
        "_[\"l\"][\"M\"]" => boolean(true),
        "_[\"length\"] ? _[\"length\"] : 0" => "0".to_string(),
        "vC()" | "nC()" | "boxed()" | "bare()" => boolean(false),
        "tC()" | "spoofed()" => "0".to_string(),
        "_[\"I\"]" => quoted(""),
        "btoa(_[\"B\"][\"slice\"](0, 300))" | "btoa(_[\"i\"][\"slice\"](0, 300))" => quoted(""),
        "\"err\"" => quoted("err"),
        "I[\"screen\"][\"width\"]" => profile.width.to_string(),
        "I[\"screen\"][\"height\"]" => profile.height.to_string(),
        "I[\"screen\"][\"colorDepth\"]" | "I[\"screen\"][\"pixelDepth\"]" => {
            profile.depth.to_string()
        }
        "I[\"outerWidth\"] - I[\"innerWidth\"]" => {
            (profile.outer_width - profile.inner_width()).to_string()
        }
        "I[\"outerHeight\"] - I[\"innerHeight\"]" => {
            (profile.outer_height - profile.inner_height()).to_string()
        }
        "I[\"screen\"][\"width\"] - I[\"outerWidth\"]" => {
            (profile.width as i64 - profile.outer_width as i64).to_string()
        }
        "I[\"screen\"][\"height\"] - I[\"outerHeight\"]" => {
            (profile.height as i64 - profile.outer_height as i64).to_string()
        }
        "I[\"devicePixelRatio\"] || 0" | "I[\"devicePixelRatio\"]" => {
            shorten(profile.ratio, 17)
        }
        "I[\"navigator\"][\"vendor\"]" => quoted("Google Inc."),
        "I[\"navigator\"][\"buildID\"] || \"NA\"" => quoted("NA"),
        "!!I[\"navigator\"][\"brave\"]" => boolean(false),
        "I[\"navigator\"][\"connection\"] && I[\"navigator\"][\"connection\"][\"rtt\"]" => {
            session.rtt.to_string()
        }
        "I[\"navigator\"][\"mediaDevices\"] ? \"defined\" : \"NA\"" => quoted("defined"),
        "!!I[\"Object\"][\"getOwnPropertyDescriptor\"](I[\"navigator\"], \"platform\")" => {
            boolean(false)
        }
        "!(!I[\"Intl\"] || !I[\"Intl\"][\"DisplayNames\"])" => boolean(true),
        "\"undefined\" != typeof I[\"Promise\"] && !!I[\"Promise\"][\"try\"]" => {
            boolean(shows(profile, "Promise.try"))
        }
        "!!I[\"navigator\"][\"pdfViewerEnabled\"]" => boolean(true),
        "I[\"document\"][\"hasFocus\"]()" => boolean(true),
        "!!I[\"document\"][\"hidden\"]" => boolean(false),
        "!!I[\"Buffer\"]" => boolean(false),
        "!!I[\"process\"]" => boolean(false),
        "!!I[\"opener\"]" => boolean(false),
        "new I[\"Date\"]()[\"getTimezoneOffset\"]()" => profile.offset.to_string(),
        "I[\"XMLDocument\"][\"toString\"]()[\"length\"]" => {
            "function XMLDocument() { [native code] }".len().to_string()
        }
        "I[\"navigator\"][\"connection\"][\"effectiveType\"] || \"unsupported\"" => quoted("4g"),
        "I[\"navigator\"][\"connection\"][\"downlink\"] || -1" => shorten(session.downlink, 6),
        "I[\"navigator\"][\"connection\"][\"saveData\"] || false" => boolean(false),
        "_[\"nextHopProtocol\"]" => quoted(&session.protocol),
        "_[\"redirectCount\"]" => "0".to_string(),
        "_[\"initiatorType\"]" => quoted("navigation"),
        "_[\"domInteractive\"]" => shorten(session.timing[14], 17),
        "_[\"domComplete\"]" => shorten(session.timing[15], 17),
        "(a - t) / t" | "(o - i) / i" => shorten(session.timing[11], 17),
        other => {
            if other.starts_with("\"aptr:\" + ") {
                quoted(&format!(
                    "aptr:{}, ahvr:{}",
                    if profile.touch > 0 { "coarse" } else { "fine" },
                    if profile.touch > 0 { "none" } else { "hover" }
                ))
            } else if other.starts_with("\"cg:\" + ") {
                quoted(&format!("cg:{}, dr:{}, dm:browser", profile.gamut, profile.range))
            } else if other.ends_with("[\"baseLatency\"] || -1") {
                shorten(profile.latency, 17)
            } else if other.ends_with("[\"sampleRate\"] || -1") {
                profile.rate.to_string()
            } else if other.ends_with(".pressure") || other.ends_with("[\"pressure\"]") {
                shorten(session.pressure, 17)
            } else if other.contains("[\"decodeURI\"]") {
                boolean(false)
            } else if other.contains("[\"webdriver\"]") {
                boolean(false)
            } else if other.contains("[\"PermissionStatus\"]") {
                boolean(true)
            } else if other.starts_with("I[\"Math\"][\"round\"](") {
                shorten(session.spent.round(), 17)
            } else if other == "I[\"performance\"][\"now\"]()" {
                shorten(session.elapsed, 17)
            } else if other.starts_with("\"\" + (") && other.ends_with(">>> 0)") {
                return None
            } else if other.starts_with("I[\"Math\"][\"max\"](I[\"document\"][\"documentElement\"][\"clientWidth\"]") {
                profile.inner_width().to_string()
            } else if other.starts_with("I[\"Math\"][\"max\"](I[\"document\"][\"documentElement\"][\"clientHeight\"]") {
                profile.inner_height().to_string()
            } else if other.contains("[\"ContactsManager\"]") {
                boolean(false)
            } else if other.ends_with("[\"slice\"](0, 150)") {
                quoted(&trace?.chars().take(150).collect::<String>())
            } else if other.ends_with("[\"slice\"](-150)") {
                let all: Vec<char> = trace?.chars().collect();
                quoted(&all[all.len().saturating_sub(150)..].iter().collect::<String>())
            } else if other.contains(".substring(0, 150)") {
                let head: String = trace?.chars().take(150).collect();
                quoted(&base64(head.as_bytes()))
            } else if other.contains(".length - 150)") {
                let all: Vec<char> = trace?.chars().collect();
                let tail: String = all[all.len().saturating_sub(150)..].iter().collect();
                quoted(&base64(tail.as_bytes()))
            } else if other.contains("[\"orientation\"][\"type\"]") {
                quoted("landscape-primary")
            } else if other.contains("[\"cpuPerformance\"]") {
                "-1".to_string()
            } else if other.contains("1 >= I[\"outerHeight\"] - I[\"innerHeight\"]") {
                boolean(profile.outer_height - profile.inner_height() <= 1)
            } else if other.contains("display-mode: fullscreen") {
                boolean(false)
            } else if other.contains("FELLOU") || other.contains("genspark")
                || other.contains("--arc-palette-title")
                || other.contains("\"2147483647\" === ")
                || other.contains("__stagehandV3__")
                || other.contains("__pwInitScripts")
            {
                boolean(false)
            } else if other.contains("[I[\"Math\"], \"random\"]") {
                quoted("")
            } else if other.contains("!I[\"chrome\"]) return false") {
                boolean(false)
            } else if other.contains("I[\"outerWidth\"] - I[\"innerWidth\"] > 170") {
                boolean(false)
            } else if other.starts_with("fA(") || other.starts_with("y(") {
                boolean(false)
            } else if other.ends_with("(8 == D ? \"\" : \",\")") || other.contains("(8 != ") {
                quoted(
                    &session
                        .draws
                        .iter()
                        .map(|value| format!("{value:.2}"))
                        .collect::<Vec<_>>()
                        .join(","),
                )
            } else if let Some(name) = store(other) {
                match name.as_str() {
                    "pf" => quoted(profile.navigator),
                    "hc" => profile.cores.to_string(),
                    "br_oh" => profile.outer_height.to_string(),
                    "br_ow" => profile.outer_width.to_string(),
                    "ua" => quoted(&profile.agent()),
                    "wbd" => boolean(false),
                    "mtp" => profile.touch.to_string(),
                    "mob" => boolean(false),
                    "lgs" => quoted(&profile.language_list()),
                    "dvm" => profile.memory.to_string(),
                    "onL" => boolean(true),
                    _ => return None,
                }
            } else if other.ends_with("[\"color\"][\"slice\"](4, -1) || \"NA\"") {
                let draws = &session.draws;
                quoted(&format!(
                    "{}, {}, {}",
                    channel(draws[2] + draws[5] * draws[1] / draws[0] * draws[4] - draws[5]),
                    channel(draws[2] + draws[3] * draws[3] / draws[4] * draws[0] - draws[0]),
                    channel(draws[4] + draws[0] * draws[1] / draws[2] * draws[3] - draws[5])
                ))
            } else if other.ends_with("[\"transform\"][\"slice\"](9, -1) || \"NA\"") {
                quoted(&transform(&session.draws))
            } else if other.ends_with("[\"height\"] || \"NA\"") {
                quoted("15px")
            } else if other.starts_with("\"hidden\" === ") {
                boolean(false)
            } else if other.starts_with("\",\" + ") {
                quoted(",loadTimes,csi,app")
            } else if other.ends_with("[\"canPlayType\"][\"toString\"]()[\"indexOf\"](\"canPlayType\")")
                || other.starts_with("-1 === ")
            {
                boolean(false)
            } else if let Some(pair) = timing(other) {
                shorten(session.timing[pair.min(18)], 17)
            } else {
                return None;
            }
        }
    };
    Some(found)
}

pub fn base64(data: &[u8]) -> String {
    const SET: &[u8] = b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
    let mut out = String::new();
    for group in data.chunks(3) {
        let packed = (group[0] as u32) << 16
            | (group.get(1).copied().unwrap_or(0) as u32) << 8
            | group.get(2).copied().unwrap_or(0) as u32;
        out.push(SET[(packed >> 18 & 63) as usize] as char);
        out.push(SET[(packed >> 12 & 63) as usize] as char);
        out.push(if group.len() > 1 { SET[(packed >> 6 & 63) as usize] as char } else { '=' });
        out.push(if group.len() > 2 { SET[(packed & 63) as usize] as char } else { '=' });
    }
    out
}

fn store(text: &str) -> Option<String> {
    let (head, rest) = text.split_once("[\"")?;
    if head.len() > 2 || head.is_empty() || !head.chars().all(|c| c.is_alphanumeric()) {
        return None;
    }
    let inner = rest.strip_suffix("\"]")?;
    if inner.contains('"') || inner.contains('[') {
        return None;
    }
    Some(inner.to_string())
}

fn timing(text: &str) -> Option<usize> {
    let marks = [
        ("connectEnd", "connectStart", 0usize),
        ("domainLookupEnd", "domainLookupStart", 1),
        ("redirectEnd", "redirectStart", 2),
        ("firstInterimResponseStart", "requestStart", 3),
        ("responseStart", "requestStart", 4),
        ("requestStart", "secureConnectionStart", 5),
        ("responseEnd", "fetchStart", 6),
        ("fetchStart", "workerStart", 7),
        ("decodedBodySize", "encodedBodySize", 8),
        ("requestStart", "connectEnd", 9),
        ("secureConnectionStart", "connectStart", 10),
        ("loadEventEnd", "loadEventStart", 12),
        ("domContentLoadedEventEnd", "domContentLoadedEventStart", 13),
    ];
    for (left, right, slot) in marks {
        if text.contains(left) && text.contains(right) && text.contains(" - ") {
            return Some(slot);
        }
    }
    None
}

fn global(text: &str) -> Option<String> {
    let inner = text.strip_prefix("!!I[\"")?;
    let inner = inner.strip_suffix("\"]")?;
    if inner.contains('"') || inner.contains('[') {
        return None;
    }
    Some(inner.to_string())
}

fn between(text: &str, head: &str, tail: &str) -> Option<String> {
    let start = text.find(head)? + head.len();
    let rest = &text[start..];
    let end = rest.rfind(tail)?;
    Some(rest[..end].replace("\\\"", "\""))
}

fn parts(mime: &str) -> (String, Vec<String>) {
    let (head, rest) = mime.split_once(';').unwrap_or((mime, ""));
    let container = head.trim().to_ascii_lowercase();
    let mut codecs = Vec::new();
    if let Some(at) = rest.find("codecs=") {
        let tail = &rest[at + 7..];
        let list = tail.trim().trim_matches('"').trim_matches('\\');
        for entry in list.split(',') {
            let name = entry.trim().trim_matches('"').trim_matches('\\').trim();
            if !name.is_empty() {
                codecs.push(name.to_string());
            }
        }
    }
    (container, codecs)
}

fn known(profile: &crate::profile::Profile, container: &str, codec: &str) -> bool {
    let base = codec.split('.').next().unwrap_or(codec);
    match base {
        "vorbis" | "opus" => matches!(container, "audio/ogg" | "audio/webm" | "video/webm"),
        "theora" => container == "video/ogg" && profile.major < 123,
        "vp8" | "vp9" => matches!(container, "video/webm" | "audio/webm"),
        "avc1" | "mp4a" | "av01" | "vp09" => matches!(container, "video/mp4" | "audio/mp4"),
        "1" => container == "audio/wav",
        _ => false,
    }
}

fn plays(profile: &crate::profile::Profile, mime: &str) -> &'static str {
    let (container, codecs) = parts(mime);
    let implied = matches!(
        container.as_str(),
        "audio/mpeg" | "audio/mp3" | "audio/aac" | "audio/flac"
    );
    let carries = matches!(
        container.as_str(),
        "audio/ogg" | "audio/wav" | "audio/webm" | "audio/mp4" | "audio/x-m4a" | "video/webm"
            | "video/mp4"
    ) || implied
        || (container == "video/ogg" && profile.major < 123);
    if !carries {
        return "";
    }
    if !codecs.is_empty() {
        return if codecs.iter().all(|codec| known(profile, &container, codec)) {
            "probably"
        } else {
            ""
        };
    }
    if implied { "probably" } else { "maybe" }
}

fn streams(profile: &crate::profile::Profile, mime: &str) -> bool {
    let (container, codecs) = parts(mime);
    if container == "audio/mpeg" {
        return true;
    }
    if codecs.is_empty() {
        return false;
    }
    if !matches!(container.as_str(), "video/mp4" | "audio/mp4" | "video/webm" | "audio/webm") {
        return false;
    }
    codecs.iter().all(|codec| known(profile, &container, codec))
}

fn exposes(profile: &crate::profile::Profile, name: &str) -> bool {
    let major = profile.major;
    let mac = profile.platform == "macOS";
    match name {
        "MutationEvent" => major < 127,
        "Promise.try" => major >= 128,
        "PressureObserver" => major >= 125,
        "WebSocketStream" => major >= 124,
        "BarcodeDetector" => mac,
        "EyeDropper" => major >= 95,
        "AudioData" => major >= 94,
        "WritableStreamDefaultController" => true,
        "CSSCounterStyleRule" => true,
        "NavigatorUAData" => true,
        "PermissionStatus" => true,
        "Intl.DisplayNames" => true,
        "WebGLObject" => true,
        "HTMLVideoElement.getVideoPlaybackQuality" => true,
        "navigator.contacts" => false,
        "SVGDiscardElement" => false,
        "window.process" => false,
        "window.opener" => false,
        "window.Buffer" => false,
        _ => false,
    }
}

fn quoted(text: &str) -> String {
    String::from_utf8(quote(text)).unwrap_or_default()
}

fn boolean(value: bool) -> String {
    value.to_string()
}

const ABSENT: &[&str] = &[
    "log3", "sivd", "sirv", "bflw", "cld", "busH", "tbsd", "tbov", "cgbe", "psd", "slat", "slmk",
    "acqt", "acqtts", "xt1", "dffls", "cfpfe", "cffrb", "stcfp", "cfpp", "cfcpw", "cfse", "iccsH",
    "iccsV", "iwgl", "m_pp", "m_scw", "m_sch", "alm", "exp19", "grfp", "exp17", "dil", "opts",
    "xhr_opts", "wwlrv", "nowd", "sfex",
];

pub struct Session {
    pub heap: [u64; 3],
    pub seed: String,
    pub seed_env: String,
    pub spent: f64,
    pub elapsed: f64,
    pub pressure: f64,
    pub seconds: i64,
    pub built: i64,
    pub script: Option<String>,
    pub draws: [f64; 9],
    pub quota: i64,
    pub usage: i64,
    pub downlink: f64,
    pub rtt: i64,
    pub timing: [f64; 19],
    pub protocol: String,
}

fn shorten(value: f64, digits: usize) -> String {
    if value == 0.0 {
        return "0".to_string();
    }
    let magnitude = value.abs().log10().floor() as i32;
    let places = (digits as i32 - 1 - magnitude).max(0) as usize;
    let text = format!("{value:.places$}");
    let text = if text.contains('.') {
        text.trim_end_matches('0').trim_end_matches('.').to_string()
    } else {
        text
    };
    if text == "-0" { "0".to_string() } else { text }
}

type Matrix = [[f64; 4]; 4];

fn identity() -> Matrix {
    let mut found = [[0.0; 4]; 4];
    for at in 0..4 {
        found[at][at] = 1.0;
    }
    found
}

fn times(left: &Matrix, right: &Matrix) -> Matrix {
    let mut found = [[0.0; 4]; 4];
    for column in 0..4 {
        for row in 0..4 {
            let mut total = 0.0;
            for step in 0..4 {
                total += left[step][row] * right[column][step];
            }
            found[column][row] = total;
        }
    }
    found
}

fn spin(x: f64, y: f64, z: f64, radians: f64) -> Matrix {
    let length = (x * x + y * y + z * z).sqrt();
    if length == 0.0 {
        return identity();
    }
    let (x, y, z) = (x / length, y / length, z / length);
    let (cosine, sine) = (radians.cos(), radians.sin());
    let rest = 1.0 - cosine;
    let mut found = identity();
    found[0][0] = rest * x * x + cosine;
    found[0][1] = rest * x * y + sine * z;
    found[0][2] = rest * x * z - sine * y;
    found[1][0] = rest * x * y - sine * z;
    found[1][1] = rest * y * y + cosine;
    found[1][2] = rest * y * z + sine * x;
    found[2][0] = rest * x * z + sine * y;
    found[2][1] = rest * y * z - sine * x;
    found[2][2] = rest * z * z + cosine;
    found
}

fn transform(draws: &[f64; 9]) -> String {
    let mut found = identity();
    found[2][3] = -1.0 / draws[6];
    found = times(&found, &spin(draws[0], draws[1], draws[2], draws[7].to_radians()));
    let mut scale = identity();
    scale[0][0] = draws[3];
    scale[1][1] = draws[4];
    scale[2][2] = draws[5];
    found = times(&found, &scale);
    found = times(&found, &spin(1.0, 0.0, 0.0, draws[8] * std::f64::consts::TAU));
    let mut slide = identity();
    slide[3][2] = draws[6];
    found = times(&found, &slide);

    let mut out = Vec::with_capacity(16);
    for column in &found {
        for value in column {
            out.push(shorten(*value, 6));
        }
    }
    out.join(", ")
}

fn channel(value: f64) -> i64 {
    value.round().clamp(0.0, 255.0) as i64
}

pub fn build(
    profile: &crate::profile::Profile,
    fields: &[Field],
    session: &Session,
    checks: Option<&Checks>,
    trace: Option<&str>,
    roles: &[(String, String)],
    extras: &[(String, String)],
    spec: Option<&Spec>,
) -> (Vec<(String, String)>, Vec<String>) {
    let bchk = checks.map(|found| found.vector(profile));
    let alias = window(fields);
    let aides = roles
        .iter()
        .filter(|(slot, _)| slot.starts_with("fn:"))
        .map(|(slot, role)| (slot[3..].to_string(), role.clone()))
        .collect::<Vec<_>>();
    let holds = roles
        .iter()
        .filter(|(slot, _)| slot.starts_with("st:"))
        .map(|(slot, role)| (slot[3..].to_string(), role.clone()))
        .collect::<Vec<_>>();
    let mut out: Vec<(String, String)> = Vec::new();
    let mut open: Vec<String> = Vec::new();
    let mut seen: Vec<String> = Vec::new();

    let mut plain: Vec<(String, String)> = Vec::new();
    let mut restricted: u32 = 0;
    let mut allowed: u32 = 0;
    let mut combined: u32 = 0;

    let put = |key: &str, value: String, out: &mut Vec<(String, String)>| {
        out.push((key.to_string(), value));
    };

    put("nddc", "0".to_string(), &mut out);
    match spec.and_then(|found| found.injected.as_ref()) {
        Some((name, value)) => put(name, shorten(*value, 17), &mut out),
        None => put("r3n", "33".to_string(), &mut out),
    }
    put("exp8", "0".to_string(), &mut out);

    let draws = &session.draws;
    let mut registers: Vec<String> = Vec::new();
    let mut best: Vec<&Field> = Vec::new();
    let flagged: Vec<&str> = fields
        .iter()
        .filter(|found| {
            fields
                .iter()
                .filter(|other| other.key == found.key)
                .all(|site| site.raw.trim() == "true")
        })
        .map(|found| found.key.as_str())
        .collect();
    for field in fields.iter().filter(|found| !found.caught && !flagged.contains(&found.key.as_str())) {
        let rank = |found: &Field| -> u32 {
            let literal = found.value.starts_with('"')
                || found.value.parse::<f64>().is_ok()
                || found.value == "true"
                || found.value == "false";
            u32::from(!literal) * 2 + u32::from(!found.deferred)
        };
        match best.iter().position(|found| found.key == field.key) {
            Some(at) => {
                if rank(field) > rank(best[at]) {
                    best[at] = field;
                }
            }
            None => best.push(field),
        }
    }

    for field in best {
        if seen.contains(&field.key)
            || ABSENT.contains(&field.key.as_str())
            || spec.is_some_and(|found| found.skipped.contains(&field.key))
            || matches!(
                field.key.as_str(),
                "nhi" | "bci" | "bcl" | "bct" | "bdt" | "k_lyts" | "k_lytk" | "stqe" | "stqu"
                    | "wwl" | "glvd" | "glrd" | "tzp" | "emd"
            )
            || matches!(field.key.as_str(), "sgb" | "sgd" | "sgc" | "fph")
        {
            continue;
        }
        if field.value.starts_with("\"\" + (") && field.value.ends_with(">>> 0)") {
            registers.push(field.key.clone());
            continue;
        }
        let normal = tidy(&rename(&field.value, &alias), &aides, &holds);
        let probe = stored(&normal).unwrap_or_else(|| field.key.clone());
        let value = match probe.as_str() {
            "dp0" => Some(boolean(false)),
            "bfr" => Some(boolean(false)),
            "hdn" => Some(boolean(false)),
            "pf" => Some(quoted(profile.navigator)),
            "hc" => Some(profile.cores.to_string()),
            "mtp" => Some(profile.touch.to_string()),
            "br_oh" => Some(profile.outer_height.to_string()),
            "br_ow" => Some(profile.outer_width.to_string()),
            "ua" => Some(quoted(&profile.agent())),
            "wbd" => Some(boolean(false)),
            "ts_mtp" => Some(profile.touch.to_string()),
            "mob" => Some(boolean(false)),
            "lgs" => Some(quoted(&profile.language_list())),
            "dvm" => Some(profile.memory.to_string()),
            "mq" => Some(quoted(&format!(
                "aptr:{}, ahvr:{}",
                if profile.touch > 0 { "coarse" } else { "fine" },
                if profile.touch > 0 { "none" } else { "hover" }
            ))),
            "mq2" => Some(quoted(&format!(
                "cg:{}, dr:{}, dm:browser",
                profile.gamut, profile.range
            ))),
            "ocpt" => Some(boolean(false)),
            "muev" => Some(boolean(exposes(profile, "MutationEvent"))),
            "pro_t" => Some(boolean(exposes(profile, "Promise.try"))),
            "wglo" => Some(boolean(exposes(profile, "WebGLObject"))),
            "prso" => Some(boolean(exposes(profile, "PressureObserver"))),
            "wbst" => Some(boolean(exposes(profile, "WebSocketStream"))),
            "psn" => Some(boolean(exposes(profile, "PermissionStatus"))),
            "edp" => Some(boolean(exposes(profile, "EyeDropper"))),
            "addt" => Some(boolean(exposes(profile, "AudioData"))),
            "wsdc" => Some(boolean(exposes(profile, "WritableStreamDefaultController"))),
            "ccsr" => Some(boolean(exposes(profile, "CSSCounterStyleRule"))),
            "nuad" => Some(boolean(exposes(profile, "NavigatorUAData"))),
            "bcda" => Some(boolean(exposes(profile, "BarcodeDetector"))),
            "idn" => Some(boolean(exposes(profile, "Intl.DisplayNames"))),
            "capi" => Some(boolean(exposes(profile, "navigator.contacts"))),
            "svde" => Some(boolean(exposes(profile, "SVGDiscardElement"))),
            "vpbq" => Some(boolean(exposes(profile, "HTMLVideoElement.getVideoPlaybackQuality"))),
            "ecpc" => Some(boolean(exposes(profile, "window.process"))),
            "wop" => Some(boolean(exposes(profile, "window.opener"))),
            "csssp" => Some(quoted("")),
            "hcovdr" | "plovdr" | "ftsovdr" => Some(boolean(false)),
            "orf" => Some(quoted("")),
            "tz" => Some(profile.offset.to_string()),
            "ihdn" => Some(boolean(false)),
            "cdhf" => Some(boolean(true)),
            "eva" => Some("function XMLDocument() { [native code] }".len().to_string()),
            "cokys" => Some(quoted(",loadTimes,csi,app")),
            "niet" => Some(quoted("4g")),
            "nisd" => Some(boolean(false)),
            "wdifrm" => Some(boolean(false)),
            "npmtm" => Some(boolean(false)),
            "wdif" => Some(boolean(false)),
            "ucdv" => Some(boolean(false)),
            "isb" => Some(boolean(false)),
            "idp" => Some(boolean(true)),
            "vnd" => Some(quoted("Google Inc.")),
            "bid" => Some(quoted("NA")),
            "med" => Some(quoted("defined")),
            "pltod" => Some(boolean(false)),
            "lg" => Some(quoted(profile.language)),
            "spwn" | "emt" | "awe" | "phe" | "dat" | "nm" | "geb" | "sqt" => Some(boolean(false)),
            "plgod" | "plgof" | "plggt" => Some(boolean(false)),
            "plgne" | "plgre" => Some(boolean(true)),
            "mmt" => Some(quoted("application/pdf,text/pdf")),
            "plu" => Some(quoted(
                "PDF Viewer,Chrome PDF Viewer,Chromium PDF Viewer,Microsoft Edge PDF Viewer,WebKit built-in PDF",
            )),
            "plg" => Some("5".to_string()),
            "bchk" => bchk.as_ref().map(|found| quoted(found)),
            "ccsT" => trace.map(|found| quoted(&found.chars().take(150).collect::<String>())),
            "ccsB" => trace.map(|found| {
                let all: Vec<char> = found.chars().collect();
                quoted(&all[all.len().saturating_sub(150)..].iter().collect::<String>())
            }),
            "ccsH" => trace.map(|found| digest(found).to_string()),
            "ccsV" => session.script.as_ref().map(|found| quoted(found)),
            "crt" => Some(session.rtt.to_string()),
            "nid" => Some(shorten(session.downlink, 6)),
            "stqe" => Some(session.quota.to_string()),
            "stqu" => Some(session.usage.to_string()),
            "cssS" => Some(quoted(
                &draws.iter().map(|value| format!("{value:.2}")).collect::<Vec<_>>().join(","),
            )),
            "css0" => Some(quoted(&format!(
                "{}, {}, {}",
                channel(draws[2] + draws[5] * draws[1] / draws[0] * draws[4] - draws[5]),
                channel(draws[2] + draws[3] * draws[3] / draws[4] * draws[0] - draws[0]),
                channel(draws[4] + draws[0] * draws[1] / draws[2] * draws[3] - draws[5])
            ))),
            "css1" => Some(quoted(&transform(draws))),
            "cssH" => Some(quoted("15px")),
            "nt_tcp" => Some(shorten(session.timing[0], 17)),
            "nt_dns" => Some(shorten(session.timing[1], 17)),
            "nt_rd" => Some(shorten(session.timing[2], 17)),
            "nt_irt" => Some(shorten(session.timing[3], 17)),
            "nt_rt" => Some(shorten(session.timing[4], 17)),
            "nt_tls" => Some(shorten(session.timing[5], 17)),
            "nt_ttf" => Some(shorten(session.timing[6], 17)),
            "nt_swt" => Some(shorten(session.timing[7], 17)),
            "nt_csd" => Some(shorten(session.timing[8], 17)),
            "nt_nhp" => Some(quoted(&session.protocol)),
            "nt_rdc" => Some("0".to_string()),
            "nt_it" => Some(quoted("navigation")),
            "nt_prs" => Some(shorten(session.timing[9], 17)),
            "nt_esc" => Some(shorten(session.timing[10], 17)),
            "nt_ttrd" => Some(shorten(session.timing[11], 17)),
            "nt_le" => Some(shorten(session.timing[12], 17)),
            "nt_dcle" => Some(shorten(session.timing[13], 17)),
            "nt_di" => Some(shorten(session.timing[14], 17)),
            "nt_dc" => Some(shorten(session.timing[15], 17)),
            "br_w" | "br_iw" => Some(profile.inner_width().to_string()),
            "br_h" | "br_ih" => Some(profile.inner_height().to_string()),
            "ars_w" => Some(profile.avail_width().to_string()),
            "ars_h" => Some(profile.avail_height().to_string()),
            "rs_w" => Some(profile.width.to_string()),
            "rs_h" => Some(profile.height.to_string()),
            "rs_cd" => Some(profile.depth.to_string()),
            "cg_w" => Some((profile.outer_width - profile.inner_width()).to_string()),
            "cg_h" => Some((profile.outer_height - profile.inner_height()).to_string()),
            "sg_w" => Some((profile.width as i64 - profile.outer_width as i64).to_string()),
            "sg_h" => Some((profile.height as i64 - profile.outer_height as i64).to_string()),
            "pr" => Some(shorten(profile.ratio, 17)),
            "so" => Some(quoted("landscape-primary")),
            "ckwa" => Some(boolean(true)),
            "pw" | "pcb" | "arc" | "fai" | "gai" | "bbs3" => Some(boolean(false)),
            "cpup" => Some("-1".to_string()),
            "dt" => Some(boolean(
                (profile.outer_height - profile.inner_height() > 170)
                    && (profile.outer_width - profile.inner_width() > 170),
            )),
            "isf" => Some(boolean(profile.outer_height - profile.inner_height() <= 1)),
            "isf2" => Some(boolean(false)),
            "trrd" => Some(shorten(
                {
                    let first = (draws[0] / 15.0 * 2.0 - 1.0).abs().sqrt();
                    let second = draws[1] / 15.0;
                    second.atan2(first)
                },
                17,
            )),
            _ => {
                let text = normal.as_str();
                if field.scope.contains("[\"gpu\"]") {
                    let ready = profile.major >= 113;
                    let missing = text.trim() == "\"noGpu\"";
                    if ready == missing {
                        None
                    } else if missing {
                        Some(text.to_string())
                    } else if let Some(found) = resolve(profile, session, trace, text) {
                        Some(found)
                    } else if constant(text) {
                        Some(text.to_string())
                    } else {
                        None
                    }
                } else if let Some((_, found)) = extras.iter().find(|(spot, _)| spot == &field.value) {
                    Some(found.clone())
                } else if constant(text) {
                    Some(text.to_string())
                } else if let Some((_, role)) = roles.iter().find(|(slot, _)| slot == text) {
                    match role.as_str() {
                        "plu" => Some(quoted(
                            "PDF Viewer,Chrome PDF Viewer,Chromium PDF Viewer,Microsoft Edge PDF Viewer,WebKit built-in PDF",
                        )),
                        "mmt" => Some(quoted("application/pdf,text/pdf")),
                        "bchk" => bchk.as_ref().map(|found| quoted(found)),
                        _ => None,
                    }
                } else if let Some(found) = probed(profile, session, text, &field.scope) {
                    Some(found)
                } else if let Some(found) = resolve(profile, session, trace, text) {
                    Some(found)
                } else if let Some(name) = global(text) {
                    Some(boolean(exposes(profile, &name) || shows(profile, &name)))
                } else if field.scope.contains("xA()") && text.starts_with("hash(") {
                    trace.map(|found| digest(found).to_string())
                } else if text == "B + \"_\" + o + \"_\" + Q + \"_\" + K" {
                    Some(quoted(&format!(
                        "{}_{}_{}_{}",
                        session.seed_env, profile.outer_width, profile.outer_height, profile.cores
                    )))
                } else if text == "hash(_[\"join\"](\"\"))" {
                    bchk.as_ref().map(|found| {
                        let joined = [
                            profile.renderer.to_string(),
                            profile.vendor.to_string(),
                            profile.agent(),
                            profile.cores.to_string(),
                            profile.language_list(),
                            profile.touch.to_string(),
                            profile.navigator.to_string(),
                            profile.outer_height.to_string(),
                            profile.outer_width.to_string(),
                            "true".to_string(),
                            "PDF Viewer,Chrome PDF Viewer,Chromium PDF Viewer,Microsoft Edge PDF Viewer,WebKit built-in PDF".to_string(),
                            "application/pdf,text/pdf".to_string(),
                            found.clone(),
                            profile.memory.to_string(),
                        ]
                        .concat();
                        digest(&joined).to_string()
                    })
                } else if text == "_[\"bchk\"]" {
                    bchk.as_ref().map(|found| quoted(found))
                } else if text.starts_with("HA(LA)(") {
                    Some(boolean(false))
                } else if text.ends_with("= \"err\"") {
                    Some(quoted("err"))
                } else if text.starts_with("!!(") && text.contains("PluginArray") {
                    Some(boolean(true))
                } else if text.contains("[\"screenX\"]") && text.contains("[\"availLeft\"]") {
                    Some(boolean(false))
                } else if field.scope.contains("[\"userEnv\"]") && joined(text) {
                    Some(quoted(&format!(
                        "{}_{}_{}_{}",
                        session.seed_env, profile.outer_width, profile.outer_height, profile.cores
                    )))
                } else if text.starts_with("scripts(") {
                    Some(quoted(""))

                } else if text.contains("jsHeapSizeLimit") {
                    Some(session.heap[0].to_string())
                } else if text.contains("totalJSHeapSize") {
                    Some(session.heap[1].to_string())
                } else if text.contains("usedJSHeapSize") {
                    Some(session.heap[2].to_string())
                } else if text.contains("[\"performance\"][\"now\"]()") && text.contains(" - ") {
                    Some(shorten(session.spent, 17))
                } else if text == "_[\"charging\"]" {
                    Some(boolean(true))
                } else if text == "_[\"level\"]" {
                    Some("1".to_string())
                } else if text == "_[\"chargingTime\"]" {
                    Some("0".to_string())
                } else if text == "_[\"dischargingTime\"]" {
                    Some("null".to_string())
                } else if let Some(mime) = between(text, "[\"canPlayType\"](\"", "\")") {
                    Some(quoted(plays(profile, &mime)))
                } else if let Some(mime) = between(text, "[\"isTypeSupported\"](\"", "\")") {
                    Some(boolean(streams(profile, &mime)))
                } else {
                    None
                }
            }
        };
        match value {
            Some(found) => {
                seen.push(field.key.clone());
                let bare = strip(&found);
                plain.push((field.value.clone(), bare.clone()));
                for (kind, pieces) in &field.folds {
                    let mut text = String::new();
                    for piece in pieces {
                        match piece {
                            Piece::Literal(part) => text.push_str(part),
                            Piece::Value(part) => {
                                let found = plain
                                    .iter()
                                    .rev()
                                    .find(|(expression, _)| expression == part)
                                    .or_else(|| {
                                        plain.iter().rev().find(|(expression, _)| {
                                            expression.starts_with(part.as_str())
                                                || part.starts_with(expression.as_str())
                                        })
                                    });
                                match found {
                                    Some((_, value)) => text.push_str(value),
                                    None => text.push_str(part),
                                }
                            }
                        }
                    }
                    let mixed = digest(&text);
                    match kind {
                        Fold::Restrict => {
                            restricted ^= mixed;
                            combined ^= mixed;
                        }
                        Fold::Allow => {
                            allowed ^= mixed;
                            combined ^= mixed;
                        }
                        Fold::Plain => combined ^= mixed,
                    }
                }
                put(&field.key, found, &mut out);
            }
            None => {
                let note = format!("{}\t{}", field.key, normal.replace('\n', " "));
                if !open.contains(&note) {
                    open.push(note);
                }
            }
        }
    }
    for key in [
        "nhi", "bci", "bcl", "bct", "bdt", "k_lyts", "k_lytk", "m_scw", "m_sch", "stqe", "stqu",
        "wwl", "glvd", "glrd", "tzp",
    ] {
        let value = match key {
            "nhi" => quoted(&[
                profile.architecture,
                profile.bitness,
                "false",
                profile.model,
                profile.platform,
                profile.version,
                profile.full,
                "false",
            ]
            .join(",")),
            "bci" => boolean(true),
            "bcl" => "1".to_string(),
            "bct" => "0".to_string(),
            "bdt" => "null".to_string(),
            "k_lyts" => profile.layout.to_string(),
            "k_lytk" => quoted(profile.keys),
            "stqe" => session.quota.to_string(),
            "stqu" => session.usage.to_string(),
            "m_scw" => "0".to_string(),
            "m_sch" => profile.chrome_height.to_string(),
            "wwl" => boolean(false),
            "glvd" => quoted(profile.vendor),
            "glrd" => quoted(profile.renderer),
            "tzp" => quoted(profile.timezone),
            _ => continue,
        };
        let bare = strip(&value);
        match key {
            "nhi" => combined ^= digest(&bare),
            "glvd" | "glrd" => combined ^= digest(&bare),
            "tzp" => combined ^= digest(&format!("tzp{bare}")),
            _ => {}
        }
        put(key, value, &mut out);
    }

    if let Some(found) = &bchk {
        let joined = [
            String::new(),
            String::new(),
            profile.agent(),
            profile.cores.to_string(),
            profile.language_list(),
            profile.touch.to_string(),
            profile.navigator.to_string(),
            profile.outer_height.to_string(),
            profile.outer_width.to_string(),
            String::new(),
            "PDF Viewer,Chrome PDF Viewer,Chromium PDF Viewer,Microsoft Edge PDF Viewer,WebKit built-in PDF"
                .to_string(),
            "application/pdf,text/pdf".to_string(),
            found.clone(),
            profile.memory.to_string(),
        ]
        .concat();
        put("fph", digest(&joined).to_string(), &mut out);
    }
    let three = if registers.len() == 3 {
        [registers[0].clone(), registers[1].clone(), registers[2].clone()]
    } else {
        ["sgb".to_string(), "sgd".to_string(), "sgc".to_string()]
    };
    put(&three[0], quoted(&restricted.to_string()), &mut out);
    put(&three[1], quoted(&allowed.to_string()), &mut out);
    put(&three[2], quoted(&combined.to_string()), &mut out);
    put("jset", session.seconds.to_string(), &mut out);
    put("bpc", session.built.to_string(), &mut out);
    (out, open)
}

fn strip(value: &str) -> String {
    let trimmed = value.trim();
    if trimmed.len() >= 2 && trimmed.starts_with('"') && trimmed.ends_with('"') {
        let inner = &trimmed[1..trimmed.len() - 1];
        return inner.replace("\\\"", "\"").replace("\\\\", "\\");
    }
    trimmed.to_string()
}

pub fn digest(text: &str) -> u32 {
    let mut found: i32 = 0;
    for unit in text.encode_utf16() {
        found = found.wrapping_shl(5).wrapping_sub(found).wrapping_add(unit as i32);
    }
    (found as i64 + 2147483647 + 1) as u32
}

pub struct Checks {
    pub names: Vec<String>,
    pub present: String,
    pub absent: String,
}

impl Checks {
    pub fn vector(&self, profile: &crate::profile::Profile) -> String {
        let present: Vec<char> = self.present.chars().collect();
        let absent: Vec<char> = self.absent.chars().collect();
        self.names
            .iter()
            .enumerate()
            .filter_map(|(at, name)| {
                let table = if shows(profile, name) { &present } else { &absent };
                table.get(at).copied()
            })
            .collect()
    }
}

pub fn checks(source: &str) -> Option<Checks> {
    let allocator = Allocator::default();
    let parsed = Parser::new(&allocator, source, SourceType::mjs())
        .with_options(ParseOptions { preserve_parens: false, ..ParseOptions::default() })
        .parse();

    struct Find {
        names: Option<Vec<String>>,
        present: Option<String>,
        absent: Option<String>,
    }

    impl<'a> Visit<'a> for Find {
        fn visit_array_expression(&mut self, node: &ArrayExpression<'a>) {
            if self.names.is_none() && node.elements.len() > 60 {
                let mut listed = Vec::with_capacity(node.elements.len());
                let mut ok = true;
                for element in &node.elements {
                    match element {
                        ArrayExpressionElement::StringLiteral(text) => {
                            listed.push(text.value.to_string())
                        }
                        _ => {
                            ok = false;
                            break;
                        }
                    }
                }
                if ok && listed.iter().any(|name| name == "structuredClone") {
                    self.names = Some(listed);
                }
            }
            walk::walk_array_expression(self, node);
        }

        fn visit_conditional_expression(&mut self, node: &ConditionalExpression<'a>) {
            if let (Expression::StringLiteral(first), Expression::StringLiteral(second)) =
                (&node.consequent, &node.alternate)
            {
                if first.value.len() == 128 && second.value.len() == 128 {
                    self.absent = Some(first.value.to_string());
                    self.present = Some(second.value.to_string());
                }
            }
            if let (Some(first), Some(second)) =
                (indexed(&node.consequent), indexed(&node.alternate))
            {
                self.present = Some(first);
                self.absent = Some(second);
            }
            walk::walk_conditional_expression(self, node);
        }
    }

    fn indexed(node: &Expression) -> Option<String> {
        let Expression::ComputedMemberExpression(member) = node else { return None };
        let Expression::StringLiteral(text) = &member.object else { return None };
        if text.value.len() != 128 {
            return None;
        }
        Some(text.value.to_string())
    }

    let mut find = Find { names: None, present: None, absent: None };
    find.visit_program(&parsed.program);
    Some(Checks { names: find.names?, present: find.present?, absent: find.absent? })
}
fn shows(profile: &crate::profile::Profile, name: &str) -> bool {
    let major = profile.major;
    match name {
        "AppBannerPromptResult"                                      => false,
        "webkitRTCPeerConnection"                                    => true,
        "webkitAudioContext"                                         => false,
        "webkitRequestAnimationFrame"                                => true,
        "chrome.runtime"                                             => false,
        "chrome.webstore"                                            => false,
        "console.context"                                            => true,
        "InputMethodContext"                                         => false,
        "SVGAnimationElement"                                        => true,
        "SVGPathSegList"                                             => false,
        "PasswordCredential"                                         => true,
        "ViewTransition"                                             => major >= 111,
        "VisualViewport.prototype.segments"                          => false,
        "DeprecationReportBody"                                      => false,
        "MathMLElement"                                              => major >= 109,
        "opr"                                                        => false,
        "CSS2Properties.prototype.colorScheme"                       => false,
        "WebKitCSSMatrix"                                            => true,
        "SVGTextPositioningElement"                                  => true,
        "XMLHttpRequestEventTarget"                                  => true,
        "TextDecoderStream"                                          => true,
        "onloadend"                                                  => false,
        "WritableStream"                                             => true,
        "TransformStream"                                            => true,
        "TextTrackCue"                                               => true,
        "WeakRef"                                                    => true,
        "VisualViewport"                                             => true,
        "StyleSheet"                                                 => true,
        "RTCDtlsTransport"                                           => true,
        "Atomics"                                                    => true,
        "StaticRange"                                                => true,
        "UIEvent"                                                    => true,
        "VideoStreamTrack"                                           => false,
        "OfflineResourceList"                                        => false,
        "SVGGeometryElement"                                         => true,
        "RTCDataChannel"                                             => true,
        "VTTRegion"                                                  => false,
        "AbortController"                                            => true,
        "Controllers"                                                => false,
        "onanimationcancel"                                          => false,
        "SVGDocument"                                                => false,
        "IIRFilterNode"                                              => true,
        "RTCStatsReport"                                             => true,
        "MediaStreamTrack"                                           => true,
        "CSS2Properties.prototype.MozOsxFontSmoothing"               => false,
        "CropTarget"                                                 => major >= 104,
        "BatteryManager"                                             => true,
        "LaunchQueue"                                                => true,
        "CSSFontPaletteValuesRule"                                   => major >= 101,
        "PushSubscriptionOptions"                                    => true,
        "DOMSettableTokenList"                                       => false,
        "RTCTrackEvent"                                              => true,
        "MozSmsMessage"                                              => false,
        "ServiceWorkerContainer"                                     => true,
        "CanvasCaptureMediaStream"                                   => false,
        "DeviceStorage"                                              => false,
        "XPathNSResolver"                                            => false,
        "SmartCardEvent"                                             => false,
        "WeakSet"                                                    => true,
        "MozMobileMessageManager"                                    => false,
        "External.prototype.getHostEnvironmentValue"                 => false,
        "WindowUtils"                                                => false,
        "XPathNamespace"                                             => false,
        "SVGFEDropShadowElement"                                     => true,
        "SharedWorker"                                               => true,
        "WorkerMessageEvent"                                         => false,
        "CSS2Properties.prototype.MozOSXFontSmoothing"               => false,
        "AudioSinkInfo"                                              => major >= 110,
        "Notification.prototype.image"                               => true,
        "ContentVisibilityAutoStateChangeEvent"                      => major >= 107,
        "PerformanceResourceTiming.prototype.renderBlockingStatus"   => major >= 107,
        "console.createTask"                                         => major >= 109,
        "PerformanceServerTiming"                                    => true,
        "CanvasFilter"                                               => false,
        "structuredClone"                                            => major >= 98,
        "onslotchange"                                               => true,
        "EyeDropper"                                                 => major >= 95,
        "URLPattern"                                                 => major >= 95,
        "VideoFrame"                                                 => major >= 94,
        "WritableStreamDefaultController"                            => true,
        "SharedArrayBuffer"                                          => false,
        "CSSCounterStyleRule"                                        => major >= 91,
        "CustomStateSet"                                             => major >= 90,
        "ReadableStreamDefaultController"                            => true,
        "XMLDocument.prototype.hasStorageAccess"                     => false,
        "CryptoKey"                                                  => true,
        "SubmitEvent"                                                => true,
        "MediaMetadata"                                              => true,
        "VideoPlaybackQuality"                                       => true,
        "ReadableStreamDefaultReader"                                => true,
        "UserActivation"                                             => true,
        "FragmentDirective"                                          => true,
        "WebKitMediaKeyError"                                        => false,
        "RTCRtpTransceiver.prototype.stop"                           => true,
        "Scheduling"                                                 => true,
        "EventCounts"                                                => true,
        "VideoTrackList"                                             => false,
        "SourceBuffer"                                               => true,
        "RTCError"                                                   => true,
        "FontFaceSet"                                                => false,
        "CSSCharsetRule"                                             => false,
        "MediaDeviceInfo"                                            => true,
        "RTCPeerConnectionIceErrorEvent"                             => true,
        "RTCSctpTransport"                                           => true,
        "MediaSessionCoordinator"                                    => false,
        "XULPopupElement"                                            => false,
        "MediaSourceHandle"                                          => major >= 108,
        "RTCEncodedAudioFrame"                                       => major >= 86,
        "__REACT_DEVTOOLS_GLOBAL_HOOK__"                             => false,
        "ShadowRealm"                                                => false,
        "HTMLSlotElement"                                            => true,
        "DetachedViewControlEvent"                                   => false,
        "GeolocationPosition"                                        => true,
        "SiteBoundCredential"                                        => false,
        "MediaSource"                                                => true,
        "WebTransport"                                               => major >= 97,
        "GPUSupportedLimits"                                         => major >= 113,
        "ToggleEvent"                                                => major >= 114,
        _ => true,
    }
}

fn spot(call: &CallExpression) -> u32 {
    match &call.callee {
        Expression::Identifier(name) => name.span.start,
        other => oxc_span::GetSpan::span(other).end,
    }
}

fn named(value: &Expression) -> Option<String> {
    match value {
        Expression::ComputedMemberExpression(member) => literal(&member.expression),
        Expression::StaticMemberExpression(member) => Some(member.property.name.to_string()),
        _ => None,
    }
}

fn forwards(call: &CallExpression, parameter: &str) -> bool {
    named(&call.callee).as_deref() == Some("apply")
        && call.arguments.len() == 2
        && matches!(call.arguments.last(), Some(Argument::Identifier(name)) if name.name == "arguments")
        && match &call.callee {
            Expression::ComputedMemberExpression(member) => {
                matches!(&member.object, Expression::Identifier(name) if name.name == parameter)
            }
            _ => false,
        }
}

#[derive(Default)]
struct Survey {
    helper: Option<(String, u32)>,
    probes: Option<(String, String)>,
    wrapper: Option<(String, u32)>,
}

impl Survey {
    fn helper_of(&mut self, node: &Function) {
        let (Some(name), Some(body)) = (&node.id, &node.body) else { return };
        struct Thrown {
            at: Option<u32>,
            prepared: bool,
        }
        impl<'a> Visit<'a> for Thrown {
            fn visit_new_expression(&mut self, node: &NewExpression<'a>) {
                if named(&node.callee).as_deref() == Some("Error") {
                    self.at.get_or_insert(node.span.start);
                }
                walk::walk_new_expression(self, node);
            }
            fn visit_expression(&mut self, node: &Expression<'a>) {
                if named(node).as_deref() == Some("prepareStackTrace") {
                    self.prepared = true;
                }
                walk::walk_expression(self, node);
            }
        }
        let mut thrown = Thrown { at: None, prepared: false };
        thrown.visit_function_body(body);
        if let (Some(at), true) = (thrown.at, thrown.prepared) {
            self.helper = Some((name.name.to_string(), at));
        }
    }

    fn wrapper_of(&mut self, node: &Function) {
        let (Some(name), Some(body)) = (&node.id, &node.body) else { return };
        let [only] = &node.params.items[..] else { return };
        let BindingPattern::BindingIdentifier(parameter) = &only.pattern else { return };
        for statement in &body.statements {
            let Statement::ReturnStatement(given) = statement else { continue };
            let Some(Expression::FunctionExpression(inner)) = &given.argument else { continue };
            let Some(inner) = &inner.body else { continue };
            for statement in &inner.statements {
                let Statement::TryStatement(guarded) = statement else { continue };
                for statement in &guarded.block.body {
                    let Statement::ReturnStatement(given) = statement else { continue };
                    let Some(Expression::CallExpression(call)) = &given.argument else { continue };
                    if forwards(call, parameter.name.as_str()) {
                        self.wrapper = Some((name.name.to_string(), spot(call)));
                    }
                }
            }
        }
    }
}

impl<'a> Visit<'a> for Survey {
    fn visit_function(&mut self, node: &Function<'a>, flags: oxc_syntax::scope::ScopeFlags) {
        self.helper_of(node);
        self.wrapper_of(node);
        walk::walk_function(self, node, flags);
    }

    fn visit_assignment_expression(&mut self, node: &AssignmentExpression<'a>) {
        if let (
            AssignmentTarget::AssignmentTargetIdentifier(name),
            Expression::ArrayExpression(list),
        ) = (&node.left, &node.right)
        {
            if list.elements.len() >= 3 {
                if let Some(ArrayExpressionElement::ObjectExpression(config)) = list.elements.last()
                {
                    let mut flag = false;
                    let mut large = false;
                    let mut first = None;
                    for entry in &config.properties {
                        let ObjectPropertyKind::ObjectProperty(entry) = entry else { continue };
                        if first.is_none() {
                            first = entry.key.static_name().map(|found| found.to_string());
                        }
                        match &entry.value {
                            Expression::BooleanLiteral(_) => flag = true,
                            Expression::NumericLiteral(found) if found.value.abs() > 1e6 => {
                                large = true
                            }
                            _ => {}
                        }
                    }
                    if flag && large {
                        if let Some(key) = first {
                            self.probes = Some((name.name.to_string(), key));
                        }
                    }
                }
            }
        }
        walk::walk_assignment_expression(self, node);
    }
}

struct Reader {
    helper: String,
    at: Option<u32>,
}

impl<'a> Visit<'a> for Reader {
    fn visit_call_expression(&mut self, node: &CallExpression<'a>) {
        if matches!(&node.callee, Expression::Identifier(name) if name.name.as_str() == self.helper)
            && node.arguments.is_empty()
        {
            self.at.get_or_insert(spot(node));
        }
        walk::walk_call_expression(self, node);
    }
}

struct Sites {
    helper: String,
    wrapper: String,
    reader: Option<u32>,
    runner: Option<u32>,
}

impl<'a> Visit<'a> for Sites {
    fn visit_function_body(&mut self, node: &FunctionBody<'a>) {
        if self.reader.is_none() {
            struct Own {
                seen: bool,
            }
            impl<'a> Visit<'a> for Own {
                fn visit_call_expression(&mut self, node: &CallExpression<'a>) {
                    if matches!(
                        node.arguments.first().and_then(|a| a.as_expression()),
                        Some(Expression::StringLiteral(text)) if text.value == "ccsT"
                    ) {
                        self.seen = true;
                    }
                    walk::walk_call_expression(self, node);
                }
                fn visit_function(&mut self, _node: &Function<'a>, _flags: oxc_syntax::scope::ScopeFlags) {}
                fn visit_arrow_function_expression(&mut self, _node: &ArrowFunctionExpression<'a>) {}
            }
            let mut own = Own { seen: false };
            for statement in &node.statements {
                own.visit_statement(statement);
            }
            if own.seen {
                let mut inner = Reader { helper: self.helper.clone(), at: None };
                inner.visit_function_body(node);
                self.reader = inner.at;
            }
        }
        walk::walk_function_body(self, node);
    }

    fn visit_call_expression(&mut self, node: &CallExpression<'a>) {
        if self.runner.is_none() {
            if let Expression::CallExpression(inner) = &node.callee {
                let plain = node
                    .arguments
                    .iter()
                    .all(|argument| matches!(argument, Argument::Identifier(_)));
                if matches!(&inner.callee, Expression::Identifier(name) if name.name.as_str() == self.wrapper)
                    && inner.arguments.len() == 1
                    && node.arguments.len() == 4
                    && plain
                {
                    self.runner = Some(spot(node));
                }
            }
        }
        walk::walk_call_expression(self, node);
    }
}

fn place(source: &str, offset: u32) -> (usize, usize) {
    let mut line = 1;
    let mut column = 1;
    for (at, c) in source.char_indices() {
        if at as u32 >= offset {
            break;
        }
        if c == '\n' {
            line += 1;
            column = 1;
        } else {
            column += 1;
        }
    }
    (line, column)
}

pub fn stack(bundle: &str, url: &str) -> Option<String> {
    let allocator = Allocator::default();
    let program = deob::transform(&allocator, bundle).ok()?;

    let mut survey = Survey::default();
    survey.visit_program(&program);
    let (helper, throw) = survey.helper?;
    let (list, key) = survey.probes?;
    let (wrapper, guard) = survey.wrapper?;

    let mut sites = Sites { helper: helper.clone(), wrapper, reader: None, runner: None };
    sites.visit_program(&program);
    let reader = sites.reader?;
    let runner = sites.runner?;

    let at = |offset: u32| {
        let (line, column) = place(bundle, offset);
        format!("{url}:{line}:{column}")
    };

    Some(format!(
        "Error\nat {helper} ({})\nat {list}.{key} ({})\nat {}\nat {}",
        at(throw),
        at(reader),
        at(guard),
        at(runner)
    ))
}

pub fn sha256(data: &[u8]) -> String {
    const K: [u32; 64] = [
        0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1, 0x923f82a4,
        0xab1c5ed5, 0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3, 0x72be5d74, 0x80deb1fe,
        0x9bdc06a7, 0xc19bf174, 0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc, 0x2de92c6f,
        0x4a7484aa, 0x5cb0a9dc, 0x76f988da, 0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7,
        0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967, 0x27b70a85, 0x2e1b2138, 0x4d2c6dfc,
        0x53380d13, 0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85, 0xa2bfe8a1, 0xa81a664b,
        0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070, 0x19a4c116,
        0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
        0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208, 0x90befffa, 0xa4506ceb, 0xbef9a3f7,
        0xc67178f2,
    ];
    let mut state: [u32; 8] = [
        0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a, 0x510e527f, 0x9b05688c, 0x1f83d9ab,
        0x5be0cd19,
    ];
    let mut message = data.to_vec();
    let bits = (data.len() as u64).wrapping_mul(8);
    message.push(0x80);
    while message.len() % 64 != 56 {
        message.push(0);
    }
    message.extend_from_slice(&bits.to_be_bytes());

    for chunk in message.chunks_exact(64) {
        let mut words = [0u32; 64];
        for (at, part) in chunk.chunks_exact(4).enumerate() {
            words[at] = u32::from_be_bytes([part[0], part[1], part[2], part[3]]);
        }
        for at in 16..64 {
            let low = words[at - 15].rotate_right(7)
                ^ words[at - 15].rotate_right(18)
                ^ (words[at - 15] >> 3);
            let high = words[at - 2].rotate_right(17)
                ^ words[at - 2].rotate_right(19)
                ^ (words[at - 2] >> 10);
            words[at] = words[at - 16]
                .wrapping_add(low)
                .wrapping_add(words[at - 7])
                .wrapping_add(high);
        }
        let [mut a, mut b, mut c, mut d, mut e, mut f, mut g, mut h] = state;
        for at in 0..64 {
            let one = e.rotate_right(6) ^ e.rotate_right(11) ^ e.rotate_right(25);
            let pick = (e & f) ^ ((!e) & g);
            let first = h
                .wrapping_add(one)
                .wrapping_add(pick)
                .wrapping_add(K[at])
                .wrapping_add(words[at]);
            let zero = a.rotate_right(2) ^ a.rotate_right(13) ^ a.rotate_right(22);
            let most = (a & b) ^ (a & c) ^ (b & c);
            let second = zero.wrapping_add(most);
            h = g;
            g = f;
            f = e;
            e = d.wrapping_add(first);
            d = c;
            c = b;
            b = a;
            a = first.wrapping_add(second);
        }
        for (slot, value) in state.iter_mut().zip([a, b, c, d, e, f, g, h]) {
            *slot = slot.wrapping_add(value);
        }
    }
    state.iter().map(|word| format!("{word:08x}")).collect()
}

fn component(text: &str) -> String {
    let mut out = String::new();
    for byte in text.as_bytes() {
        let c = *byte as char;
        if c.is_ascii_alphanumeric() || "-_.!~*'()".contains(c) {
            out.push(c);
        } else {
            out.push_str(&format!("%{byte:02X}"));
        }
    }
    out
}

fn escaped(text: &str) -> String {
    let mut out = String::new();
    for byte in text.as_bytes() {
        let c = *byte as char;
        if c.is_ascii_alphanumeric() || "@*_+-./".contains(c) {
            out.push(c);
        } else {
            out.push_str(&format!("%{byte:02X}"));
        }
    }
    out
}

pub struct Counters {
    pub mousemove: u32,
    pub pointermove: u32,
    pub click: u32,
    pub scroll: u32,
    pub touchstart: u32,
    pub touchend: u32,
    pub touchmove: u32,
    pub keydown: u32,
    pub keyup: u32,
}

impl Counters {
    pub fn json(&self) -> String {
        format!(
            "{{\"mousemove\":{},\"pointermove\":{},\"click\":{},\"scroll\":{},\"touchstart\":{},\"touchend\":{},\"touchmove\":{},\"keydown\":{},\"keyup\":{}}}",
            self.mousemove,
            self.pointermove,
            self.click,
            self.scroll,
            self.touchstart,
            self.touchend,
            self.touchmove,
            self.keydown,
            self.keyup
        )
    }
}

pub fn body(
    payload: &str,
    counters: &Counters,
    kind: &str,
    cid: &str,
    key: &str,
    referer: &str,
    request: &str,
    response: &str,
    version: &str,
) -> String {
    let mut out = format!(
        "jspl={}&eventCounters={}&jsType={kind}",
        component(payload),
        component(&counters.json())
    );
    if !cid.is_empty() {
        out.push_str(&format!("&cid={}", component(cid)));
    }
    out.push_str(&format!("&ddk={}", escaped(&component(key))));
    out.push_str(&format!("&Referer={}", escaped(&component(referer))));
    out.push_str(&format!("&request={}", escaped(&component(request))));
    out.push_str(&format!("&responsePage={}", escaped(&component(response))));
    out.push_str(&format!("&ddv={version}"));
    out
}

pub fn slots(source: &str) -> Vec<(String, String)> {
    let allocator = Allocator::default();
    let parsed = Parser::new(&allocator, source, SourceType::mjs())
        .with_options(ParseOptions { preserve_parens: false, ..ParseOptions::default() })
        .parse();

    struct Roles<'s> {
        source: &'s str,
        found: Vec<(String, String)>,
    }

    impl<'a, 's> Visit<'a> for Roles<'s> {
        fn visit_assignment_expression(&mut self, node: &AssignmentExpression<'a>) {
            if let AssignmentTarget::ComputedMemberExpression(target) = &node.left {
                let span = oxc_span::GetSpan::span(&node.left);
                let name = self.source[span.start as usize..span.end as usize].to_string();
                if name.matches("[\"").count() == 2 && matches!(&target.object, Expression::ComputedMemberExpression(_))
                {
                    let body = oxc_span::GetSpan::span(&node.right);
                    let text = &self.source[body.start as usize..body.end as usize];
                    let role = if text.contains("[\"plugins\"]") {
                        "plu"
                    } else if text.contains("[\"mimeTypes\"]") {
                        "mmt"
                    } else if text.contains("\"structuredClone\"") {
                        "bchk"
                    } else {
                        return;
                    };
                    self.found.push((name, role.to_string()));
                }
            }
            walk::walk_assignment_expression(self, node);
        }
    }

    let mut roles = Roles { source, found: Vec::new() };
    roles.visit_program(&parsed.program);
    roles.found
}

pub struct Emit {
    pub key: String,
    pub argument: String,
}

pub fn emits(source: &str) -> Vec<Emit> {
    within(source, "sendPayload = function")
}

pub fn within(source: &str, anchor: &str) -> Vec<Emit> {
    let Some(head) = source.find(anchor) else { return Vec::new() };
    let rest = &source[head..];
    let Some(open) = rest.find('{') else { return Vec::new() };
    let mut depth = 0i32;
    let mut stop = rest.len();
    for (at, letter) in rest[open..].char_indices() {
        match letter {
            '{' => depth += 1,
            '}' => {
                depth -= 1;
                if depth == 0 {
                    stop = open + at;
                    break;
                }
            }
            _ => {}
        }
    }
    let body = &rest[open..stop];
    let mut found: Vec<Emit> = Vec::new();
    let mut at = 0usize;
    while let Some(spot) = body[at..].find('"') {
        let start = at + spot + 1;
        let Some(end) = body[start..].find('"') else { break };
        let key = &body[start..start + end];
        at = start + end + 1;
        if key.len() != 6 || !key.chars().all(|c| c.is_ascii_alphanumeric()) {
            continue;
        }
        if !body[at..].trim_start().starts_with(',') {
            continue;
        }
        let after = body[at..].trim_start().trim_start_matches(',');
        let argument = balanced(after);
        if found.iter().any(|emit| emit.key == key) {
            continue;
        }
        found.push(Emit { key: key.to_string(), argument });
    }
    found
}

fn balanced(body: &str) -> String {
    let mut depth = 0i32;
    let mut out = String::new();
    for letter in body.chars() {
        match letter {
            '(' | '[' | '{' => depth += 1,
            ')' | ']' | '}' => {
                if depth == 0 {
                    break;
                }
                depth -= 1;
            }
            ',' if depth == 0 => break,
            _ => {}
        }
        out.push(letter);
    }
    out.trim().to_string()
}


fn window(fields: &[Field]) -> String {
    let mut tally: std::collections::BTreeMap<String, usize> = std::collections::BTreeMap::new();
    for field in fields {
        let text = field.value.trim_start();
        let head: String = text.chars().take_while(|c| c.is_alphanumeric() || *c == '_').collect();
        if head.is_empty() || head.len() > 2 {
            continue;
        }
        if !text[head.len()..].starts_with("[\"") {
            continue;
        }
        *tally.entry(head).or_insert(0) += 1;
    }
    tally
        .into_iter()
        .max_by_key(|(_, count)| *count)
        .map(|(name, _)| name)
        .unwrap_or_else(|| "I".to_string())
}

fn rename(text: &str, alias: &str) -> String {
    blank(&swap(text, alias, "I"))
}

fn blank(text: &str) -> String {
    let raw: Vec<char> = text.chars().collect();
    let mut out = String::with_capacity(text.len());
    let mut at = 0usize;
    while at < raw.len() {
        let fresh = at == 0 || !(raw[at - 1].is_alphanumeric() || raw[at - 1] == '_' || raw[at - 1] == '.');
        if fresh && raw[at].is_alphabetic() {
            let mut end = at;
            while end < raw.len() && (raw[end].is_alphanumeric() || raw[end] == '_') {
                end += 1;
            }
            let name: String = raw[at..end].iter().collect();
            if name.len() <= 2 && name != "I" && end < raw.len() && raw[end] == '[' {
                out.push('_');
                at = end;
                continue;
            }
            out.push_str(&name);
            at = end;
            continue;
        }
        out.push(raw[at]);
        at += 1;
    }
    out
}

fn swap(text: &str, alias: &str, into: &str) -> String {
    if alias == into {
        return text.to_string();
    }
    let mut out = String::with_capacity(text.len());
    let raw: Vec<char> = text.chars().collect();
    let name: Vec<char> = alias.chars().collect();
    let mut at = 0usize;
    while at < raw.len() {
        let ahead = at + name.len();
        let matches = ahead <= raw.len()
            && raw[at..ahead] == name[..]
            && (at == 0 || !(raw[at - 1].is_alphanumeric() || raw[at - 1] == '_'))
            && ahead < raw.len()
            && raw[ahead] == '[';
        if matches {
            out.push_str(into);
            at = ahead;
            continue;
        }
        out.push(raw[at]);
        at += 1;
    }
    out
}

fn constant(text: &str) -> bool {
    let body = text.trim();
    if body == "true" || body == "false" || body == "null" {
        return true;
    }
    if body.parse::<f64>().is_ok() {
        return true;
    }
    if body.len() >= 2 && body.starts_with('"') && body.ends_with('"') {
        return !body[1..body.len() - 1].contains('"');
    }
    false
}

fn stored(text: &str) -> Option<String> {
    let body = text.trim();
    let inner = body.strip_prefix("_[\"")?.strip_suffix("\"]")?;
    if inner.is_empty() || !inner.chars().all(|c| c.is_ascii_lowercase() || c == '_' || c.is_ascii_digit()) {
        return None;
    }
    Some(inner.to_string())
}

fn probed(
    profile: &crate::profile::Profile,
    session: &Session,
    text: &str,
    scope: &str,
) -> Option<String> {
    if scope.contains("getOwnPropertyNames") && profile.globals != 0 {
        return Some(profile.globals.to_string());
    }
    if text == "O - U" || (text.contains(" - ") && scope.contains("[\"exports\"]")) {
        return Some(shorten(session.elapsed * 0.18, 17));
    }
    if text == "(B - o) / o" && scope.contains("secureConnectionStart") {
        let secure = session.timing[10];
        let rest = session.timing[9];
        if rest == 0.0 {
            return Some("0".to_string());
        }
        return Some(shorten((secure - rest) / rest, 17));
    }
    if text.starts_with("(_[\"length\"] ? \"k:\"") {
        if profile.devices.is_empty() {
            return None;
        }
        return Some(quoted(profile.devices));
    }
    if text.contains("SQRT2") && text.contains("atan2") {
        let mut seed = (session.seconds as u64) | 1;
        let mut roll = || {
            seed ^= seed << 13;
            seed ^= seed >> 7;
            seed ^= seed << 17;
            (seed >> 11) as f64 / (1u64 << 53) as f64
        };
        let pi = std::f64::consts::PI;
        let root = std::f64::consts::SQRT_2;
        let one = (pi / 90.0 * 100.0 - 40.0 * roll() * (pi / 180.0) / 2.0).sin();
        let two = (100.0 * root * (pi / 180.0)).cos()
            * (pi / 180.0 * 40.0 - 100.0 * roll() * (pi / 75.0) / 2.0).sin();
        let near = (one + two).abs().sqrt();
        let three = roll();
        let four = (40.0 * roll() * (pi / 90.0) - 100.0 * root * (pi / 180.0) / 2.0).sin();
        let five = (3.705_555_555_555_555_7f64).cos()
            * roll()
            * (pi / 180.0 * 60.0 - pi / 45.0 * 100.0 / 2.0).sin();
        let far = three * (1.0 - four + five).abs().sqrt();
        return Some(shorten(near.atan2(far), 17));
    }
    if scope.contains("getLayoutMap") {
        return match text {
            "_[\"size\"]" => Some(profile.layout.to_string()),
            "_[E]" if !profile.keys.is_empty() => Some(quoted(profile.keys)),
            _ => None,
        };
    }
    if text == "s > 0" && scope.contains("getComputedStyle") {
        return Some(boolean(false));
    }
    if text.contains("_[0] >>> 0") && text.contains("_[2] >>> 0") {
        let seeds: Vec<u32> = [profile.outer_width, profile.outer_height, profile.cores]
            .iter()
            .map(|value| wasm::attest::cyrb53(&session.seed_env, *value) as u32)
            .collect();
        return mixed(&field_text(text), &seeds).map(|found| found.to_string());
    }
    if scope.contains("Comic Sans MS") && text.ends_with("+ \",\"") {
        if profile.fonts.is_empty() {
            return None;
        }
        return Some(quoted(profile.fonts));
    }
    if scope.contains("small-caption") && text.contains("join") {
        if profile.families.is_empty() {
            return None;
        }
        return Some(quoted(profile.families));
    }
    if scope.contains("uaFullVersion") && scope.contains("platformVersion") && text.contains("join") {
        return Some(quoted(&[
            profile.architecture,
            profile.bitness,
            "false",
            profile.model,
            profile.platform,
            profile.version,
            profile.full,
            "false",
        ]
        .join(",")));
    }
    if scope.contains("Worker") || scope.contains("OffscreenCanvas") {
        return match text {
            "_[0]" => Some("7".to_string()),
            "_[3]" => Some(shorten(session.elapsed * 0.42, 17)),
            body if pairing(body) => {
                if profile.canvas.is_empty() {
                    None
                } else {
                    Some(quoted(profile.canvas))
                }
            }
            "_[1]" => {
                let mut sum: u32 = session.seed.chars().map(|letter| letter as u32).sum();
                sum %= 10;
                if sum == 0 {
                    sum = 1;
                }
                Some(sum.to_string())
            }
            body if body.contains("[\"pP\"]") => Some(quoted("default")),
            body if body.contains("[\"t\"]") => Some(shorten(0.3, 17)),
            body if body.starts_with("(null == ") || body.starts_with("(null != ") => None,
            _ => None,
        };
    }
    if text.starts_with("_[\"self\"] && _[\"self\"][\"get\"]") {
        return Some("undefined".to_string());
    }
    if pairing(text) && (scope.contains("Worker") || scope.contains("OffscreenCanvas")) {
        if profile.canvas.is_empty() {
            return None;
        }
        return Some(quoted(profile.canvas));
    }
    if scope.contains("getVoices") || scope.contains("localService") {
        let parts: Vec<&str> = profile.voices.split(',').filter(|piece| !piece.is_empty()).collect();
        if parts.is_empty() {
            return None;
        }
        return match text {
            "_[\"length\"]" => parts.first().map(|found| (*found).to_string()),
            body if body.starts_with("hash(") => parts.get(3).map(|found| (*found).to_string()),
            _ => None,
        };
    }
    let _ = profile;
    None
}


fn field_text(text: &str) -> String {
    text.replace("_[0]", "s[0]").replace("_[1]", "s[1]").replace("_[2]", "s[2]")
}

fn mixed(text: &str, values: &[u32]) -> Option<u32> {
    let allocator = Allocator::default();
    let wrapped = format!("({text})");
    let parsed = Parser::new(&allocator, &wrapped, SourceType::mjs())
        .with_options(ParseOptions { preserve_parens: false, ..ParseOptions::default() })
        .parse();
    if parsed.panicked {
        return None;
    }
    let Some(Statement::ExpressionStatement(first)) = parsed.program.body.first() else {
        return None;
    };
    evaluate(&first.expression, values).map(|found| found as i64 as u32)
}

fn evaluate(node: &Expression, values: &[u32]) -> Option<f64> {
    match node {
        Expression::NumericLiteral(found) => Some(found.value),
        Expression::ComputedMemberExpression(found) => {
            let Expression::NumericLiteral(index) = &found.expression else { return None };
            values.get(index.value as usize).map(|found| f64::from(*found))
        }
        Expression::UnaryExpression(found) => {
            let inner = evaluate(&found.argument, values)?;
            match found.operator {
                oxc_syntax::operator::UnaryOperator::UnaryNegation => Some(-inner),
                oxc_syntax::operator::UnaryOperator::UnaryPlus => Some(inner),
                oxc_syntax::operator::UnaryOperator::BitwiseNot => Some(f64::from(!signed(inner))),
                _ => None,
            }
        }
        Expression::BinaryExpression(found) => {
            let left = evaluate(&found.left, values)?;
            let right = evaluate(&found.right, values)?;
            use oxc_syntax::operator::BinaryOperator as Op;
            Some(match found.operator {
                Op::Addition => left + right,
                Op::Subtraction => left - right,
                Op::Multiplication => left * right,
                Op::BitwiseAnd => f64::from(signed(left) & signed(right)),
                Op::BitwiseOR => f64::from(signed(left) | signed(right)),
                Op::BitwiseXOR => f64::from(signed(left) ^ signed(right)),
                Op::ShiftLeft => f64::from(signed(left).wrapping_shl(unsigned(right) & 31)),
                Op::ShiftRight => f64::from(signed(left).wrapping_shr(unsigned(right) & 31)),
                Op::ShiftRightZeroFill => f64::from(unsigned(left) >> (unsigned(right) & 31)),
                _ => return None,
            })
        }
        _ => None,
    }
}

fn unsigned(value: f64) -> u32 {
    if !value.is_finite() {
        return 0;
    }
    let whole = value.trunc();
    let wrapped = whole.rem_euclid(4294967296.0);
    wrapped as u32
}

fn signed(value: f64) -> i32 {
    unsigned(value) as i32
}

pub fn helpers(source: &str) -> Vec<(String, &'static str)> {
    let mut found: Vec<(String, &'static str)> = Vec::new();
    let mut at = 0usize;
    while let Some(spot) = source[at..].find("function ") {
        let start = at + spot + 9;
        let name: String = source[start..]
            .chars()
            .take_while(|c| c.is_alphanumeric() || *c == '_' || *c == '$')
            .collect();
        at = start + name.len().max(1);
        if name.is_empty() || name.len() > 3 {
            continue;
        }
        let Some(open) = source[at..].find('{') else { break };
        let body = enclosed(source, at + open);
        let role = classify(body);
        if let Some(role) = role {
            if !found.iter().any(|(had, _)| had == &name) {
                found.push((name.clone(), role));
            }
        }
    }
    let mut at = 0usize;
    while let Some(spot) = source[at..].find(" = function(") {
        let head = at + spot;
        let name: String = source[..head]
            .chars()
            .rev()
            .take_while(|c| c.is_alphanumeric() || *c == '_' || *c == '$')
            .collect::<Vec<_>>()
            .into_iter()
            .rev()
            .collect();
        at = head + 12;
        if name.is_empty() || name.len() > 3 {
            continue;
        }
        let Some(open) = source[at..].find('{') else { break };
        let body = enclosed(source, at + open);
        if let Some(role) = classify(body) {
            if !found.iter().any(|(had, _)| had == &name) {
                found.push((name, role));
            }
        }
    }
    found
}

fn enclosed(source: &str, open: usize) -> &str {
    let raw = source.as_bytes();
    let mut depth = 0i32;
    let mut at = open;
    while at < raw.len() {
        match raw[at] {
            b'{' => depth += 1,
            b'}' => {
                depth -= 1;
                if depth == 0 {
                    return &source[open..=at];
                }
            }
            _ => {}
        }
        at += 1;
    }
    &source[open..]
}

fn classify(body: &str) -> Option<&'static str> {
    if body.contains("charCodeAt") && body.contains("<< 5") && body.contains("2147483647") {
        return Some("hash");
    }
    if body.contains("btoa") && body.contains("b_e") {
        return Some("btoa");
    }
    if body.contains("getScriptHash") {
        return Some("scripts");
    }
    if body.contains("% 240") {
        return Some("spread");
    }
    if body.contains("[\"screenX\"]") && body.contains("[\"availLeft\"]") {
        return Some("boxed");
    }
    if body.contains("getGamepads") && body.contains("RTCPeerConnection") {
        return Some("bare");
    }
    if body.contains("oscpu") && body.contains("Atlantic/Reykjavik") {
        return Some("spoofed");
    }
    None
}

pub fn stores(source: &str) -> Vec<(String, &'static str)> {
    let mut found = Vec::new();
    let mut at = 0usize;
    while let Some(spot) = source[at..].find("\"] = function()") {
        let head = at + spot;
        at = head + 15;
        let Some(open) = source[..head].rfind("[\"") else { continue };
        let prop = &source[open + 2..head];
        if prop.is_empty() || prop.len() > 3 {
            continue;
        }
        let Some(open) = source[at..].find('{') else { continue };
        let body = enclosed(source, at + open);
        let role = if body.contains("[\"plugins\"]") && body.contains("[\"name\"]") {
            "plu"
        } else if body.contains("[\"mimeTypes\"]") && body.contains("[\"type\"]") {
            "mmt"
        } else if body.contains("AppBannerPromptResult") {
            "bchk"
        } else {
            continue;
        };
        found.push((prop.to_string(), role));
    }
    found
}

fn tidy(text: &str, aides: &[(String, String)], holds: &[(String, String)]) -> String {
    let mut out = text.to_string();
    for (name, role) in aides {
        out = swap_call(&out, name, role);
    }
    for (prop, role) in holds {
        out = out.replace(&format!("[\"{prop}\"]"), &format!("[\"{role}\"]"));
        for letter in ["a", "b", "c", "l", "D", "L", "C", "h", "s", "t", "w", "A", "B"] {
            out = out.replace(
                &format!("_[\"{letter}\"][\"{role}\"]"),
                &format!("_[\"{role}\"]"),
            );
        }
    }
    out
}

fn swap_call(text: &str, name: &str, role: &str) -> String {
    let raw: Vec<char> = text.chars().collect();
    let want: Vec<char> = name.chars().collect();
    let mut out = String::with_capacity(text.len());
    let mut at = 0usize;
    while at < raw.len() {
        let ahead = at + want.len();
        let fresh = at == 0 || !(raw[at - 1].is_alphanumeric() || raw[at - 1] == '_');
        if fresh && ahead < raw.len() && raw[at..ahead] == want[..] && raw[ahead] == '(' {
            out.push_str(role);
            at = ahead;
            continue;
        }
        out.push(raw[at]);
        at += 1;
    }
    out
}

fn joined(text: &str) -> bool {
    let parts: Vec<&str> = text.split(" + ").collect();
    parts.len() == 7
        && parts.iter().skip(1).step_by(2).all(|piece| *piece == "\"_\"")
        && parts
            .iter()
            .step_by(2)
            .all(|piece| piece.chars().all(|c| c.is_alphanumeric() || c == '_'))
}

fn pairing(text: &str) -> bool {
    let Some((left, right)) = text.split_once(" || ") else { return false };
    let short = |piece: &str| {
        !piece.is_empty()
            && piece.len() <= 2
            && piece.chars().all(|c| c.is_alphanumeric() || c == '_')
    };
    short(left) && short(right)
}

pub fn unbase64(text: &str) -> Vec<u8> {
    const SET: &[u8] = b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
    let mut out = Vec::new();
    let mut held: u32 = 0;
    let mut bits = 0u32;
    for letter in text.bytes() {
        let Some(spot) = SET.iter().position(|found| *found == letter) else { continue };
        held = (held << 6) | spot as u32;
        bits += 6;
        if bits >= 8 {
            bits -= 8;
            out.push((held >> bits) as u8);
        }
    }
    out
}
