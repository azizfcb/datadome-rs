use std::collections::BTreeMap;

#[derive(Clone, Default)]
pub struct Node {
    pub tag: String,
    pub id: String,
    pub classes: Vec<String>,
    pub attributes: BTreeMap<String, String>,
    pub inline: Vec<(String, String)>,
    pub children: Vec<usize>,
    pub parent: Option<usize>,
    pub rank: usize,
    pub last: bool,
}

pub struct Tree {
    pub nodes: Vec<Node>,
    pub rules: Vec<(String, Vec<(String, String)>)>,
}

pub fn parse(body: &str) -> Tree {
    let mut rules = Vec::new();
    let mut markup = String::new();
    let mut rest = body;
    while let Some(open) = rest.find("<style") {
        markup.push_str(&rest[..open]);
        let Some(head) = rest[open..].find('>') else { break };
        let start = open + head + 1;
        let Some(stop) = rest[start..].find("</style>") else { break };
        rules.extend(sheet(&rest[start..start + stop]));
        rest = &rest[start + stop + 8..];
    }
    markup.push_str(rest);
    Tree { nodes: build(&markup), rules }
}

fn sheet(body: &str) -> Vec<(String, Vec<(String, String)>)> {
    let mut found = Vec::new();
    let mut rest = body;
    while let Some(open) = rest.find('{') {
        let Some(close) = rest[open..].find('}') else { break };
        let heads = rest[..open].trim().to_string();
        let block = declarations(&rest[open + 1..open + close]);
        for head in heads.split(',') {
            let head = head.trim();
            if !head.is_empty() {
                found.push((head.to_string(), block.clone()));
            }
        }
        rest = &rest[open + close + 1..];
    }
    found
}

fn declarations(body: &str) -> Vec<(String, String)> {
    let mut found = Vec::new();
    let mut depth = 0i32;
    let mut piece = String::new();
    for letter in body.chars() {
        match letter {
            '(' => {
                depth += 1;
                piece.push(letter);
            }
            ')' => {
                depth -= 1;
                piece.push(letter);
            }
            ';' if depth == 0 => {
                push(&mut found, &piece);
                piece.clear();
            }
            other => piece.push(other),
        }
    }
    push(&mut found, &piece);
    found
}

fn push(into: &mut Vec<(String, String)>, piece: &str) {
    if let Some((name, value)) = piece.split_once(':') {
        let name = name.trim();
        if !name.is_empty() {
            into.push((name.to_string(), value.trim().to_string()));
        }
    }
}

fn build(markup: &str) -> Vec<Node> {
    let mut nodes: Vec<Node> = Vec::new();
    let mut stack: Vec<usize> = Vec::new();
    let mut rest = markup;
    while let Some(open) = rest.find('<') {
        let Some(close) = rest[open..].find('>') else { break };
        let body = &rest[open + 1..open + close];
        rest = &rest[open + close + 1..];
        if body.starts_with('/') {
            stack.pop();
            continue;
        }
        if body.starts_with('!') {
            continue;
        }
        let closed = body.ends_with('/');
        let body = body.trim_end_matches('/');
        let tag = body.split(|c: char| c.is_whitespace()).next().unwrap_or("div").to_lowercase();
        let attributes = attributes(body);
        let mut node = Node {
            tag,
            id: attributes.get("id").cloned().unwrap_or_default(),
            classes: attributes
                .get("class")
                .map(|found| found.split_whitespace().map(|name| name.to_string()).collect())
                .unwrap_or_default(),
            inline: declarations(attributes.get("style").map(String::as_str).unwrap_or("")),
            attributes,
            children: Vec::new(),
            parent: stack.last().copied(),
            rank: 0,
            last: true,
        };
        let at = nodes.len();
        if let Some(parent) = node.parent {
            node.rank = nodes[parent].children.len();
            nodes[parent].children.push(at);
        }
        nodes.push(node);
        if !closed && !matches!(nodes[at].tag.as_str(), "br" | "img" | "input" | "meta" | "link") {
            stack.push(at);
        }
    }
    for at in 0..nodes.len() {
        let same: Vec<usize> = match nodes[at].parent {
            Some(parent) => nodes[parent]
                .children
                .iter()
                .copied()
                .filter(|kid| nodes[*kid].tag == nodes[at].tag)
                .collect(),
            None => vec![at],
        };
        nodes[at].last = same.last().copied() == Some(at);
    }
    nodes
}

fn attributes(body: &str) -> BTreeMap<String, String> {
    let mut found = BTreeMap::new();
    let mut rest = body;
    while let Some(spot) = rest.find('=') {
        let name = rest[..spot]
            .rsplit(|c: char| c.is_whitespace())
            .next()
            .unwrap_or("")
            .to_string();
        let after = rest[spot + 1..].trim_start();
        let quote = after.chars().next().unwrap_or(' ');
        if quote == '"' || quote == '\'' {
            let Some(end) = after[1..].find(quote) else { break };
            found.insert(name, after[1..1 + end].to_string());
            rest = &after[2 + end..];
        } else {
            let end = after.find(char::is_whitespace).unwrap_or(after.len());
            found.insert(name, after[..end].to_string());
            rest = &after[end..];
        }
    }
    found
}

impl Tree {
    pub fn find(&self, selector: &str) -> Option<usize> {
        (0..self.nodes.len()).find(|at| self.matches(*at, selector))
    }

    fn matches(&self, at: usize, selector: &str) -> bool {
        let parts = split(selector);
        self.chain(at, &parts)
    }

    fn chain(&self, at: usize, parts: &[(char, String)]) -> bool {
        let Some((join, simple)) = parts.last() else { return true };
        if !self.simple(at, simple) {
            return false;
        }
        if parts.len() == 1 {
            return true;
        }
        let head = &parts[..parts.len() - 1];
        match join {
            '>' => match self.nodes[at].parent {
                Some(parent) => self.chain(parent, head),
                None => false,
            },
            _ => {
                let mut walk = self.nodes[at].parent;
                while let Some(parent) = walk {
                    if self.chain(parent, head) {
                        return true;
                    }
                    walk = self.nodes[parent].parent;
                }
                false
            }
        }
    }

    fn simple(&self, at: usize, selector: &str) -> bool {
        let node = &self.nodes[at];
        let mut rest = selector;
        while !rest.is_empty() {
            let head = rest.chars().next().unwrap();
            let stop = rest[1..]
                .find(|c: char| c == '#' || c == '.' || c == '[' || c == ':')
                .map(|found| found + 1)
                .unwrap_or(rest.len());
            let piece = &rest[..stop];
            let ok = match head {
                '#' => node.id == piece[1..],
                '.' => node.classes.iter().any(|name| name == &piece[1..]),
                '[' => {
                    let inner = piece.trim_start_matches('[').trim_end_matches(']');
                    match inner.split_once('=') {
                        Some((name, value)) => {
                            node.attributes.get(name.trim()).map(String::as_str)
                                == Some(value.trim().trim_matches(|c| c == '"' || c == '\''))
                        }
                        None => node.attributes.contains_key(inner.trim()),
                    }
                }
                ':' => match &piece[1..] {
                    "last-of-type" => node.last,
                    "first-of-type" => node.rank == 0,
                    _ => false,
                },
                _ => piece == "*" || node.tag == piece,
            };
            if !ok {
                return false;
            }
            rest = &rest[stop..];
        }
        true
    }

    fn declared(&self, at: usize) -> Vec<(String, String)> {
        let mut found = Vec::new();
        for (selector, block) in &self.rules {
            if self.matches(at, selector) {
                found.extend(block.iter().cloned());
            }
        }
        found.extend(self.nodes[at].inline.iter().cloned());
        found
    }

    pub fn width(&self, at: usize) -> f64 {
        let variables = self.variables(at);
        let style = self.style(at, &variables);
        let padding = style.get("padding").and_then(|body| length(body, &variables)).unwrap_or(0.0);
        let border = style
            .get("border-width")
            .and_then(|body| length(body, &variables))
            .unwrap_or(0.0);
        let inner = match style.get("width").and_then(|body| length(body, &variables)) {
            Some(found) => found,
            None => self.content(at),
        };
        let low = style.get("min-width").and_then(|body| length(body, &variables)).unwrap_or(0.0);
        let high = style
            .get("max-width")
            .and_then(|body| length(body, &variables))
            .unwrap_or(f64::INFINITY);
        inner.max(low).min(high) + 2.0 * padding + 2.0 * border
    }

    fn content(&self, at: usize) -> f64 {
        self.nodes[at]
            .children
            .iter()
            .map(|kid| self.width(*kid))
            .fold(0.0, f64::max)
    }

    fn style(&self, at: usize, variables: &BTreeMap<String, String>) -> BTreeMap<String, String> {
        let mut found = BTreeMap::new();
        for (name, value) in self.declared(at) {
            if name.starts_with("--") {
                continue;
            }
            match expand(&value, variables, 0) {
                Some(resolved) => {
                    found.insert(name, resolved);
                }
                None => {
                    found.remove(&name);
                }
            }
        }
        found
    }

    fn variables(&self, at: usize) -> BTreeMap<String, String> {
        let mut chain = vec![at];
        let mut walk = self.nodes[at].parent;
        while let Some(parent) = walk {
            chain.push(parent);
            walk = self.nodes[parent].parent;
        }
        chain.reverse();
        let mut found: BTreeMap<String, String> = BTreeMap::new();
        for step in chain {
            for (name, value) in self.declared(step) {
                if !name.starts_with("--") {
                    continue;
                }
                match expand(&value, &found, 0) {
                    Some(resolved) => {
                        found.insert(name, resolved);
                    }
                    None => {
                        found.remove(&name);
                    }
                }
            }
        }
        found
    }
}

fn split(selector: &str) -> Vec<(char, String)> {
    let mut parts = Vec::new();
    let mut piece = String::new();
    let mut join = ' ';
    let mut pending = ' ';
    for letter in selector.chars() {
        match letter {
            '>' => {
                if !piece.trim().is_empty() {
                    parts.push((join, piece.trim().to_string()));
                    piece.clear();
                }
                join = '>';
                pending = ' ';
            }
            ' ' => {
                if !piece.trim().is_empty() {
                    pending = ' ';
                }
                if !piece.is_empty() {
                    parts.push((join, piece.trim().to_string()));
                    piece.clear();
                    join = ' ';
                }
            }
            other => {
                let _ = pending;
                piece.push(other);
            }
        }
    }
    if !piece.trim().is_empty() {
        parts.push((join, piece.trim().to_string()));
    }
    parts
}

fn expand(body: &str, variables: &BTreeMap<String, String>, depth: usize) -> Option<String> {
    if depth > 16 {
        return None;
    }
    let mut out = String::new();
    let mut rest = body;
    while let Some(spot) = rest.find("var(") {
        out.push_str(&rest[..spot]);
        let inner = balanced(&rest[spot + 3..])?;
        let after = spot + 3 + inner.len();
        let arguments = inner.trim_start_matches('(').trim_end_matches(')');
        let (name, fallback) = match arguments.split_once(',') {
            Some((name, rest)) => (name.trim(), Some(rest.trim())),
            None => (arguments.trim(), None),
        };
        let value = match variables.get(name) {
            Some(found) => found.clone(),
            None => match fallback {
                Some(found) => expand(found, variables, depth + 1)?,
                None => return None,
            },
        };
        out.push_str(&value);
        rest = &rest[after..];
    }
    out.push_str(rest);
    Some(out)
}

fn balanced(body: &str) -> Option<&str> {
    let mut depth = 0i32;
    for (at, letter) in body.char_indices() {
        match letter {
            '(' => depth += 1,
            ')' => {
                depth -= 1;
                if depth == 0 {
                    return Some(&body[..at + 1]);
                }
            }
            _ => {}
        }
    }
    None
}

pub fn length(body: &str, variables: &BTreeMap<String, String>) -> Option<f64> {
    let resolved = expand(body, variables, 0)?;
    value(resolved.trim())
}

fn value(body: &str) -> Option<f64> {
    let body = body.trim();
    for head in ["calc", "min", "max", "clamp"] {
        if let Some(rest) = body.strip_prefix(head) {
            let inner = balanced(rest)?;
            let arguments = inner.trim_start_matches('(').trim_end_matches(')');
            let parts = commas(arguments);
            let numbers: Option<Vec<f64>> = parts.iter().map(|part| value(part)).collect();
            let numbers = numbers?;
            return match head {
                "calc" => sum(arguments),
                "min" => numbers.iter().copied().reduce(f64::min),
                "max" => numbers.iter().copied().reduce(f64::max),
                _ => {
                    let mut sorted = numbers.clone();
                    sorted.sort_by(|a, b| a.partial_cmp(b).unwrap());
                    sorted.get(1).copied()
                }
            };
        }
    }
    if body.ends_with("px") {
        return body.trim_end_matches("px").trim().parse().ok();
    }
    if body == "0" {
        return Some(0.0);
    }
    if let Ok(found) = body.parse::<f64>() {
        return Some(found);
    }
    if body.contains('+') || body.contains('-') || body.contains('*') || body.contains('/') {
        return sum(body);
    }
    None
}

fn sum(body: &str) -> Option<f64> {
    let mut total = 0.0;
    let mut sign = 1.0;
    let mut piece = String::new();
    let mut depth = 0i32;
    let mut letters = body.chars().peekable();
    while let Some(letter) = letters.next() {
        match letter {
            '(' => {
                depth += 1;
                piece.push(letter);
            }
            ')' => {
                depth -= 1;
                piece.push(letter);
            }
            '+' | '-' if depth == 0 && !piece.trim().is_empty() => {
                total += sign * product(&piece)?;
                sign = if letter == '+' { 1.0 } else { -1.0 };
                piece.clear();
            }
            other => piece.push(other),
        }
    }
    total += sign * product(&piece)?;
    Some(total)
}

fn product(body: &str) -> Option<f64> {
    let mut total = 1.0;
    let mut divide = false;
    let mut piece = String::new();
    let mut depth = 0i32;
    for letter in body.chars() {
        match letter {
            '(' => {
                depth += 1;
                piece.push(letter);
            }
            ')' => {
                depth -= 1;
                piece.push(letter);
            }
            '*' | '/' if depth == 0 => {
                let found = value(&piece)?;
                total = if divide { total / found } else { total * found };
                divide = letter == '/';
                piece.clear();
            }
            other => piece.push(other),
        }
    }
    let found = value(&piece)?;
    Some(if divide { total / found } else { total * found })
}

fn commas(body: &str) -> Vec<String> {
    let mut parts = Vec::new();
    let mut depth = 0i32;
    let mut piece = String::new();
    for letter in body.chars() {
        match letter {
            '(' => {
                depth += 1;
                piece.push(letter);
            }
            ')' => {
                depth -= 1;
                piece.push(letter);
            }
            ',' if depth == 0 => {
                parts.push(piece.trim().to_string());
                piece.clear();
            }
            other => piece.push(other),
        }
    }
    parts.push(piece.trim().to_string());
    parts
}
