use std::cell::RefCell;
use std::collections::BTreeMap;
use std::rc::Rc;

use crate::exec::State;

#[derive(Clone)]
pub enum Value {
    Undefined,
    Null,
    Bool(bool),
    Num(f64),
    Text(Rc<String>),
    List(Rc<RefCell<Vec<Value>>>),
    Map(Rc<RefCell<BTreeMap<String, Value>>>),
    Host(&'static str),
    Method(&'static str, Rc<String>),
    Closure(Rc<Closure>),
    Prop(Box<Value>, Rc<String>),
    Bound(Rc<(Value, Vec<Value>)>),
    Slot(usize),
}

pub struct Closure {
    pub source: String,
    pub params: Vec<String>,
    pub env: Vec<(String, Value)>,
}

impl Value {
    pub fn number(&self) -> f64 {
        match self {
            Value::Num(found) => *found,
            Value::Bool(found) => f64::from(*found),
            Value::Null => 0.0,
            Value::Text(found) => {
                let trimmed = found.trim();
                if trimmed.is_empty() { 0.0 } else { trimmed.parse().unwrap_or(f64::NAN) }
            }
            _ => f64::NAN,
        }
    }

    pub fn kind(&self) -> &'static str {
        match self {
            Value::Undefined => "undefined",
            Value::Null => "null",
            Value::Bool(_) => "bool",
            Value::Num(_) => "num",
            Value::Text(_) => "text",
            Value::List(_) => "list",
            Value::Map(_) => "map",
            Value::Host(_) => "host",
            Value::Method(_, _) => "method",
            Value::Closure(_) => "closure",
            Value::Prop(_, _) => "prop",
            Value::Bound(_) => "bound",
            Value::Slot(_) => "slot",
        }
    }

    pub fn text(&self) -> String {
        match self {
            Value::Undefined => "undefined".to_string(),
            Value::Null => "null".to_string(),
            Value::Bool(found) => found.to_string(),
            Value::Num(found) => number(*found),
            Value::Text(found) => found.to_string(),
            Value::List(items) => items
                .borrow()
                .iter()
                .map(|item| match item {
                    Value::Undefined | Value::Null => String::new(),
                    other => other.text(),
                })
                .collect::<Vec<_>>()
                .join(","),
            Value::Map(_) => "[object Object]".to_string(),
            Value::Host(name) => format!("[{name}]"),
            Value::Method(base, name) => format!("[{base}.{name}]"),
            Value::Closure(_) => "[closure]".to_string(),
            Value::Prop(_, name) => format!("[method {name}]"),
            Value::Bound(inner) => format!("[bound {}]", inner.0.kind()),
            Value::Slot(at) => format!("[slot {at}]"),
        }
    }

    pub fn truthy(&self) -> bool {
        match self {
            Value::Undefined | Value::Null => false,
            Value::Bool(found) => *found,
            Value::Num(found) => *found != 0.0 && !found.is_nan(),
            Value::Text(found) => !found.is_empty(),
            _ => true,
        }
    }
}

pub fn number(value: f64) -> String {
    if value.is_nan() {
        return "NaN".to_string();
    }
    if value.is_infinite() {
        return if value > 0.0 { "Infinity" } else { "-Infinity" }.to_string();
    }
    if value == value.trunc() && value.abs() < 1e21 {
        return format!("{}", value as i64);
    }
    format!("{value}")
}

pub fn strict(left: &Value, right: &Value) -> bool {
    match (left, right) {
        (Value::Text(a), Value::Text(b)) => a == b,
        (Value::Undefined, Value::Undefined) | (Value::Null, Value::Null) => true,
        (Value::Bool(a), Value::Bool(b)) => a == b,
        (Value::Num(a), Value::Num(b)) => a == b,
        (Value::Host(a), Value::Host(b)) => a == b,
        (Value::List(a), Value::List(b)) => Rc::ptr_eq(a, b),
        (Value::Map(a), Value::Map(b)) => Rc::ptr_eq(a, b),
        (Value::Method(a, b), Value::Method(c, d)) => a == c && b == d,
        _ => false,
    }
}

pub fn same(left: &Value, right: &Value) -> bool {
    match (left, right) {
        (Value::Text(a), Value::Text(b)) => a == b,
        (Value::Undefined, Value::Undefined) | (Value::Null, Value::Null) => true,
        (Value::Bool(a), Value::Bool(b)) => a == b,
        (Value::Num(a), Value::Num(b)) => a == b,
        (Value::Host(a), Value::Host(b)) => a == b,
        (Value::Method(a, b), Value::Method(c, d)) => a == c && b == d,
        (Value::List(a), Value::List(b)) => Rc::ptr_eq(a, b),
        (Value::Map(a), Value::Map(b)) => Rc::ptr_eq(a, b),
        (Value::Text(a), other) | (other, Value::Text(a)) => match other {
            Value::Num(_) | Value::Bool(_) => a.as_str() == other.text(),
            _ => false,
        },
        _ => false,
    }
}

pub fn member(base: &Value, key: &Value, state: &mut State) -> Value {
    match base {
        Value::Host("api") => slot(key.number() as usize, state),
        Value::Host("memory") => {
            let name = key.text();
            if name.parse::<f64>().is_ok() {
                let at = key.number();
                if at < 0.0 || !at.is_finite() {
                    return state.spill.get(&(at as i64)).cloned().unwrap_or(Value::Undefined);
                }
                state.cell(at as usize)
            } else {
                Value::Prop(Box::new(Value::Host("memory")), Rc::new(name))
            }
        }
        Value::Host("result") => {
            state.result.borrow().get(&key.text()).cloned().unwrap_or(Value::Undefined)
        }
        Value::List(items) => {
            let name = key.text();
            if name == "length" {
                return Value::Num(items.borrow().len() as f64);
            }
            match name.parse::<usize>() {
                Ok(at) => items.borrow().get(at).cloned().unwrap_or(Value::Undefined),
                Err(_) => Value::Prop(Box::new(base.clone()), Rc::new(name)),
            }
        }
        Value::Map(entries) => {
            let name = key.text();
            match entries.borrow().get(&name) {
                Some(found) => found.clone(),
                None => Value::Prop(Box::new(base.clone()), Rc::new(name)),
            }
        }
        Value::Text(found) => {
            let name = key.text();
            if name == "length" {
                return Value::Num(found.chars().count() as f64);
            }
            match name.parse::<usize>() {
                Ok(at) => found
                    .chars()
                    .nth(at)
                    .map(|c| Value::Text(Rc::new(c.to_string())))
                    .unwrap_or(Value::Undefined),
                Err(_) => Value::Prop(Box::new(base.clone()), Rc::new(name)),
            }
        }
        Value::Host("document") => {
            let name = key.text();
            match name.as_str() {
                "body" | "head" | "documentElement" => element(state, "div"),
                "lastModified" => Value::Text(Rc::new("08/29/2026 00:00:00".to_string())),
                "location" => Value::Host("location"),
                "hidden" => Value::Bool(false),
                "visibilityState" => Value::Text(Rc::new("visible".to_string())),
                _ => Value::Method("document", Rc::new(name)),
            }
        }
        Value::Host("location") => {
            let name = key.text();
            let origin = state.host.origin.clone();
            let page = state.host.page.clone();
            match name.as_str() {
                "protocol" => Value::Text(Rc::new(
                    origin.split_once(':').map(|(head, _)| format!("{head}:")).unwrap_or_default(),
                )),
                "host" | "hostname" => Value::Text(Rc::new(
                    origin.trim_start_matches("https://").trim_start_matches("http://").to_string(),
                )),
                "pathname" => Value::Text(Rc::new(page)),
                "search" | "hash" | "port" => Value::Text(Rc::new(String::new())),
                "origin" => Value::Text(Rc::new(origin)),
                "href" => Value::Text(Rc::new(format!("{origin}{page}"))),
                _ => Value::Undefined,
            }
        }
        Value::Host("navigator") => {
            let name = key.text();
            match name.as_str() {
                "hardwareConcurrency" => Value::Num(state.host.cores),
                "deviceMemory" => Value::Num(state.host.memory),
                "maxTouchPoints" => Value::Num(state.host.touch),
                "language" => Value::Text(Rc::new(state.host.language.clone())),
                "languages" => Value::List(Rc::new(RefCell::new(
                    state.host.languages.iter().map(|tag| Value::Text(Rc::new(tag.clone()))).collect(),
                ))),
                "userAgent" => Value::Text(Rc::new(state.host.agent.clone())),
                "appVersion" => Value::Text(Rc::new(
                    state.host.agent.trim_start_matches("Mozilla/").to_string(),
                )),
                "appName" => Value::Text(Rc::new("Netscape".to_string())),
                "appCodeName" => Value::Text(Rc::new("Mozilla".to_string())),
                "product" => Value::Text(Rc::new("Gecko".to_string())),
                "productSub" => Value::Text(Rc::new("20030107".to_string())),
                "vendor" => Value::Text(Rc::new("Google Inc.".to_string())),
                "vendorSub" => Value::Text(Rc::new(String::new())),
                "platform" => Value::Text(Rc::new(state.host.platform.clone())),
                "onLine" => Value::Bool(true),
                "cookieEnabled" => Value::Bool(true),
                "webdriver" => Value::Bool(false),
                "pdfViewerEnabled" => Value::Bool(true),
                "doNotTrack" => Value::Null,
                "plugins" | "mimeTypes" => Value::List(Rc::new(RefCell::new(Vec::new()))),
                _ => Value::Method("navigator", Rc::new(name)),
            }
        }
        Value::Host("screen") => {
            let name = key.text();
            match name.as_str() {
                "width" => Value::Num(state.host.width),
                "height" => Value::Num(state.host.height),
                "availWidth" => Value::Num(state.host.avail_width),
                "availHeight" => Value::Num(state.host.avail_height),
                "availLeft" | "availTop" => Value::Num(0.0),
                "colorDepth" | "pixelDepth" => Value::Num(state.host.depth),
                _ => Value::Undefined,
            }
        }
        Value::Host("performance") => {
            let name = key.text();
            match name.as_str() {
                "timeOrigin" => Value::Num(state.clock - state.elapsed),
                "timing" => Value::Host("timing"),
                "memory" => Value::Host("memory-info"),
                _ => Value::Method("performance", Rc::new(name)),
            }
        }
        Value::Host("timing") => {
            let name = key.text();
            let start = state.clock - state.elapsed;
            match name.as_str() {
                "navigationStart" => Value::Num(start),
                "loadEventEnd" => Value::Num(start + 640.0),
                "domainLookupStart" => Value::Num(start + 4.0),
                _ => Value::Num(start),
            }
        }
        Value::Host("memory-info") => {
            let name = key.text();
            match name.as_str() {
                "usedJSHeapSize" => Value::Num(11_500_000.0),
                "totalJSHeapSize" => Value::Num(19_000_000.0),
                "jsHeapSizeLimit" => Value::Num(4_294_705_152.0),
                _ => Value::Undefined,
            }
        }
        Value::Host("window") => {
            let name = key.text();
            match name.as_str() {
                "Math" | "Date" | "Object" | "Array" | "String" | "JSON" | "Number"
                | "Uint8Array" | "performance" | "document" | "navigator" | "screen"
                | "crypto" | "window" | "Function" | "Error" | "Promise" => {
                    Value::Host(leak(&name))
                }
                "self" | "top" | "parent" | "frames" => Value::Host("window"),
                "innerWidth" => Value::Num(state.host.inner_width),
                "innerHeight" => Value::Num(state.host.inner_height),
                "outerWidth" => Value::Num(state.host.outer_width),
                "outerHeight" => Value::Num(state.host.outer_height),
                "devicePixelRatio" => Value::Num(state.host.ratio),
                "screenX" | "screenY" | "pageXOffset" | "pageYOffset" | "scrollX" | "scrollY" => {
                    Value::Num(0.0)
                }
                "location" => Value::Host("location"),
                "length" => Value::Num(0.0),
                "closed" => Value::Bool(false),
                "isSecureContext" => Value::Bool(true),
                "origin" => Value::Text(Rc::new(state.host.origin.clone())),
                "name" => Value::Text(Rc::new(String::new())),
                _ => Value::Method("window", Rc::new(name)),
            }
        }
        Value::Host("Function") => match key.text().as_str() {
            "prototype" => Value::Host("funproto"),
            name => Value::Method("Function", Rc::new(name.to_string())),
        },
        Value::Host("funproto") => match key.text().as_str() {
            "bind" => Value::Host("bindfn"),
            name => Value::Method("funproto", Rc::new(name.to_string())),
        },
        Value::Bound(inner) => member(&inner.0, key, state),
        Value::Host(name) => Value::Method(name, Rc::new(key.text())),
        Value::Method(_, _) | Value::Closure(_) | Value::Prop(_, _) => match key.text().as_str() {
            "name" => Value::Text(Rc::new(match base {
                Value::Method(_, label) => label.to_string(),
                _ => String::new(),
            })),
            "length" => Value::Num(0.0),
            "prototype" => Value::Host("funproto"),
            _ => Value::Prop(Box::new(base.clone()), Rc::new(key.text())),
        },
        _ => Value::Undefined,
    }
}

fn slot(at: usize, state: &mut State) -> Value {
    let api = &state.api;
    let index = at as u32;
    if Some(index) == api.image {
        return Value::Host("memory");
    }
    if Some(index) == api.result {
        return Value::Host("result");
    }
    if let Some(width) = api.readers.get(&index) {
        return Value::Host(match width {
            crate::api::Width::U8 => "read8",
            crate::api::Width::U16 => "read16",
            crate::api::Width::U24 => "read24",
            crate::api::Width::Const => "readconst",
        });
    }
    if Some(index) == api.strings {
        return Value::Host("decode");
    }
    if let Some(role) = api.roles.get(&index) {
        return Value::Host(match role {
            crate::api::Helper::Dispatch => "dispatch",
            crate::api::Helper::Nop => "nop",
            crate::api::Helper::PushGlobal => "pushglobal",
            crate::api::Helper::StoreGlobal => "storeglobal",
            crate::api::Helper::PopToAcc => "poptoacc",
            crate::api::Helper::MemberGet => "memberget",
            crate::api::Helper::MemberSet => "memberset",
        });
    }
    match api.cells.get(&index) {
        Some(found) => Value::Num(*found as f64),
        None => Value::Undefined,
    }
}

pub fn assign(base: &Value, key: &Value, value: Value, state: &mut State) {
    match base {
        Value::Host("memory") => {
            let at = key.number();
            if at < 0.0 || !at.is_finite() {
                state.spill.insert(at as i64, value);
            } else {
                state.put(at as usize, value);
            }
        }
        Value::Host("result") => {
            state.result.borrow_mut().insert(key.text(), value);
        }
        Value::List(items) => {
            if let Ok(at) = key.text().parse::<usize>() {
                let mut list = items.borrow_mut();
                if list.len() <= at {
                    list.resize(at + 1, Value::Undefined);
                }
                list[at] = value;
            }
        }
        Value::Map(entries) => {
            let name = key.text();
            if name == "innerHTML" && state.watch == 1 {
                let body = value.text();
                eprintln!("innerHTML {body}");
            }
            entries.borrow_mut().insert(name, value);
        }
        _ => {}
    }
}

pub fn host(name: &str) -> Value {
    match name {
        "Uint8Array" | "Array" | "String" | "Math" | "Object" | "JSON" | "Number" | "Date"
        | "window" | "performance" | "document" | "navigator" | "Function" | "Error"
        | "Promise" | "crypto" | "screen" => Value::Host(leak(name)),
        _ => Value::Undefined,
    }
}

fn leak(name: &str) -> &'static str {
    Box::leak(name.to_string().into_boxed_str())
}

pub fn construct(name: &str, arguments: Vec<Value>) -> Value {
    match name {
        "Uint8Array" | "Uint16Array" | "Uint32Array" | "Int32Array" | "Float64Array" => {
            match arguments.first() {
                Some(Value::List(items)) => {
                    let cells: Vec<Value> = items
                        .borrow()
                        .iter()
                        .map(|item| {
                            let raw = item.number();
                            Value::Num(if name == "Uint8Array" {
                                f64::from(raw as i64 as u8)
                            } else {
                                raw
                            })
                        })
                        .collect();
                    Value::List(Rc::new(RefCell::new(cells)))
                }
                other => {
                    let size = other.map_or(0.0, |found| found.number()) as usize;
                    Value::List(Rc::new(RefCell::new(vec![Value::Num(0.0); size.min(1 << 22)])))
                }
            }
        }
        "Array" => {
            let size = arguments.first().map_or(0.0, |found| found.number()) as usize;
            Value::List(Rc::new(RefCell::new(vec![Value::Undefined; size.min(1 << 22)])))
        }
        "Date" => crate::dom::date(arguments.first().map_or(0.0, |found| found.number())),
        "Error" | "TypeError" | "RangeError" => crate::dom::make(
            "Error",
            &[(
                "message",
                arguments.first().cloned().unwrap_or(Value::Text(Rc::new(String::new()))),
            )],
        ),
        _ => Value::Map(Rc::new(RefCell::new(BTreeMap::new()))),
    }
}

pub fn method(owner: &Value, name: &str, arguments: &[Value], state: &mut State) -> Value {
    let at = |index: usize| arguments.get(index).cloned().unwrap_or(Value::Undefined);
    match owner {
        Value::Host("memory") => match name {
            "slice" => {
                let from = at(0).number() as usize;
                let to = at(1).number() as usize;
                let mut items = Vec::new();
                for cell in from..to.max(from) {
                    items.push(state.cell(cell));
                }
                Value::List(Rc::new(RefCell::new(items)))
            }
            _ => Value::Undefined,
        },
        Value::List(items) => match name {
            "slice" => {
                let list = items.borrow();
                let size = list.len() as i64;
                let start = index(at(0), size, 0);
                let stop = if arguments.len() > 1 { index(at(1), size, size) } else { size };
                let picked: Vec<Value> = list
                    .get(start as usize..stop.max(start) as usize)
                    .map(|slice| slice.to_vec())
                    .unwrap_or_default();
                Value::List(Rc::new(RefCell::new(picked)))
            }
            "reverse" => {
                items.borrow_mut().reverse();
                Value::List(items.clone())
            }
            "push" => {
                let mut list = items.borrow_mut();
                for value in arguments {
                    list.push(value.clone());
                }
                Value::Num(list.len() as f64)
            }
            "unshift" => {
                let mut list = items.borrow_mut();
                for value in arguments.iter().rev() {
                    list.insert(0, value.clone());
                }
                Value::Num(list.len() as f64)
            }
            "pop" => items.borrow_mut().pop().unwrap_or(Value::Undefined),
            "join" => {
                let glue = if arguments.is_empty() { ",".to_string() } else { at(0).text() };
                let list = items.borrow();
                let parts: Vec<String> = list
                    .iter()
                    .map(|item| match item {
                        Value::Undefined | Value::Null => String::new(),
                        other => other.text(),
                    })
                    .collect();
                Value::Text(Rc::new(parts.join(&glue)))
            }
            "indexOf" => {
                let list = items.borrow();
                let found = list.iter().position(|item| same(item, &at(0)));
                Value::Num(found.map_or(-1.0, |found| found as f64))
            }
            "concat" => {
                let mut list = items.borrow().clone();
                for value in arguments {
                    match value {
                        Value::List(more) => list.extend(more.borrow().iter().cloned()),
                        other => list.push(other.clone()),
                    }
                }
                Value::List(Rc::new(RefCell::new(list)))
            }
            "fill" => {
                let mut list = items.borrow_mut();
                for slot in list.iter_mut() {
                    *slot = at(0).clone();
                }
                Value::List(items.clone())
            }
            "toString" => Value::Text(Rc::new(owner.text())),
            _ => Value::Undefined,
        },
        Value::Text(found) => match name {
            "charCodeAt" => {
                let position = at(0).number() as usize;
                match found.chars().nth(position) {
                    Some(c) => Value::Num(c as u32 as f64),
                    None => Value::Num(f64::NAN),
                }
            }
            "charAt" => {
                let position = at(0).number() as usize;
                Value::Text(Rc::new(found.chars().nth(position).map(|c| c.to_string()).unwrap_or_default()))
            }
            "slice" | "substring" => {
                let chars: Vec<char> = found.chars().collect();
                let size = chars.len() as i64;
                let start = index(at(0), size, 0);
                let stop = if arguments.len() > 1 { index(at(1), size, size) } else { size };
                let picked: String = chars
                    .get(start as usize..stop.max(start) as usize)
                    .map(|slice| slice.iter().collect())
                    .unwrap_or_default();
                Value::Text(Rc::new(picked))
            }
            "indexOf" => {
                let needle = at(0).text();
                Value::Num(found.find(&needle).map_or(-1.0, |position| {
                    found[..position].chars().count() as f64
                }))
            }
            "split" => {
                let glue = at(0).text();
                let parts: Vec<Value> = if glue.is_empty() {
                    found.chars().map(|c| Value::Text(Rc::new(c.to_string()))).collect()
                } else {
                    found.split(&glue).map(|part| Value::Text(Rc::new(part.to_string()))).collect()
                };
                Value::List(Rc::new(RefCell::new(parts)))
            }
            "toString" => Value::Text(found.clone()),
            "replace" => {
                let from = at(0).text();
                let to = at(1).text();
                Value::Text(Rc::new(found.replacen(&from, &to, 1)))
            }
            _ => Value::Undefined,
        },
        Value::Map(entries) => crate::dom::call(entries, name, arguments, state),
        Value::Method(_, label) => match name {
            "toString" => Value::Text(Rc::new(format!("function {label}() {{ [native code] }}"))),
            "valueOf" => owner.clone(),
            _ => Value::Undefined,
        },
        Value::Host(label) => match name {
            "toString" => Value::Text(Rc::new(format!("function {label}() {{ [native code] }}"))),
            "valueOf" => owner.clone(),
            _ => native(label, name, arguments.to_vec(), state),
        },
        Value::Bound(inner) => match name {
            "toString" => Value::Text(Rc::new("function () { [native code] }".to_string())),
            _ => method(&inner.0, name, arguments, state),
        },
        other => {
            let note = format!("{}.{name}/{}", other.text(), arguments.len());
            if !state.wanted.contains(&note) && state.wanted.len() < 300 {
                state.wanted.push(note);
            }
            Value::Undefined
        }
    }
}

fn index(value: Value, size: i64, fallback: i64) -> i64 {
    match value {
        Value::Undefined => fallback,
        other => {
            let found = other.number() as i64;
            if found < 0 { (size + found).max(0) } else { found.min(size) }
        }
    }
}

pub fn native(host: &str, name: &str, arguments: Vec<Value>, state: &mut State) -> Value {
    let at = |index: usize| arguments.get(index).cloned().unwrap_or(Value::Undefined);
    match (host, name) {
        ("Math", "floor") => Value::Num(at(0).number().floor()),
        ("Math", "ceil") => Value::Num(at(0).number().ceil()),
        ("Math", "round") => Value::Num((at(0).number() + 0.5).floor()),
        ("Math", "abs") => Value::Num(at(0).number().abs()),
        ("Math", "sqrt") => Value::Num(at(0).number().sqrt()),
        ("Math", "min") => Value::Num(
            arguments.iter().map(|found| found.number()).fold(f64::INFINITY, f64::min),
        ),
        ("Math", "max") => Value::Num(
            arguments.iter().map(|found| found.number()).fold(f64::NEG_INFINITY, f64::max),
        ),
        ("Math", "pow") => Value::Num(at(0).number().powf(at(1).number())),
        ("Math", "imul") => Value::Num(
            (at(0).number() as i64 as i32).wrapping_mul(at(1).number() as i64 as i32) as f64,
        ),
        ("Math", "random") => Value::Num(state.roll()),
        ("Math", "log") => Value::Num(at(0).number().ln()),
        ("Math", "sin") => Value::Num(at(0).number().sin()),
        ("Math", "cos") => Value::Num(at(0).number().cos()),
        ("Math", "atan2") => Value::Num(at(0).number().atan2(at(1).number())),
        ("String", "fromCharCode") => {
            let text: String = arguments
                .iter()
                .filter_map(|found| char::from_u32(found.number() as u32))
                .collect();
            Value::Text(Rc::new(text))
        }
        ("Object", "keys") => {
            let keys = match at(0) {
                Value::Map(entries) => entries
                    .borrow()
                    .keys()
                    .map(|key| Value::Text(Rc::new(key.clone())))
                    .collect(),
                Value::List(items) => (0..items.borrow().len())
                    .map(|at| Value::Text(Rc::new(at.to_string())))
                    .collect(),
                _ => Vec::new(),
            };
            Value::List(Rc::new(RefCell::new(keys)))
        }
        ("Array", "isArray") => Value::Bool(matches!(at(0), Value::List(_))),
        ("Array", "from") => match at(0) {
            Value::List(items) => Value::List(Rc::new(RefCell::new(items.borrow().clone()))),
            Value::Text(found) => Value::List(Rc::new(RefCell::new(
                found.chars().map(|c| Value::Text(Rc::new(c.to_string()))).collect(),
            ))),
            _ => Value::List(Rc::new(RefCell::new(Vec::new()))),
        },
        ("Date", "now") => {
            state.clock += 1.0;
            Value::Num(state.clock)
        }
        ("document", "createElement") | ("document", "createElementNS") => {
            let tag = arguments.last().map(|found| found.text()).unwrap_or_default();
            element(state, &tag)
        }
        ("document", "querySelector") => crate::dom::query(state, &at(0).text()),
        ("document", "getElementById") => {
            crate::dom::query(state, &format!("#{}", at(0).text()))
        }
        ("document", "querySelectorAll") | ("document", "getElementsByTagName")
        | ("document", "getElementsByClassName") => {
            let picked = crate::dom::query(state, &at(0).text());
            let items = match picked {
                Value::Null => Vec::new(),
                other => vec![other],
            };
            Value::List(Rc::new(RefCell::new(items)))
        }
        ("crypto", "randomUUID") => Value::Text(Rc::new(
            "3f1a2c64-9b17-4a2e-8c55-7d0e1b93a4f2".to_string(),
        )),
        ("performance", "now") => {
            state.elapsed += 0.1;
            Value::Num(state.elapsed)
        }
        ("Number", "") => Value::Num(at(0).number()),
        ("String", "") => Value::Text(Rc::new(at(0).text())),
        ("Boolean", "") => Value::Bool(at(0).truthy()),
        ("Number", "isNaN") | ("window", "isNaN") => Value::Bool(at(0).number().is_nan()),
        ("Number", "isInteger") => {
            let raw = at(0).number();
            Value::Bool(raw.is_finite() && raw == raw.trunc())
        }
        ("Number", "parseFloat") | ("window", "parseFloat") => {
            Value::Num(parse_head(&at(0).text(), false))
        }
        ("Number", "parseInt") | ("window", "parseInt") => {
            Value::Num(parse_head(&at(0).text(), true))
        }
        ("String", "raw") => Value::Text(Rc::new(at(0).text())),
        ("Object", "getOwnPropertyNames") => native("Object", "keys", arguments, state),
        ("Object", "values") => {
            let values = match at(0) {
                Value::Map(entries) => entries.borrow().values().cloned().collect(),
                Value::List(items) => items.borrow().clone(),
                _ => Vec::new(),
            };
            Value::List(Rc::new(RefCell::new(values)))
        }
        ("JSON", "stringify") => Value::Text(Rc::new(render(&at(0)))),
        ("window", "btoa") => {
            let raw: Vec<u8> = at(0).text().chars().map(|c| c as u32 as u8).collect();
            Value::Text(Rc::new(encode(&raw)))
        }
        ("window", "atob") => {
            let text = at(0).text();
            Value::Text(Rc::new(
                decode64(&text).iter().map(|byte| *byte as char).collect::<String>(),
            ))
        }
        _ => {
            let note = format!("{host}.{name}/{}", arguments.len());
            if !state.wanted.contains(&note) && state.wanted.len() < 300 {
                state.wanted.push(note);
            }
            Value::Undefined
        }
    }
}

pub fn element(state: &mut State, tag: &str) -> Value {
    crate::dom::element(state, tag)
}

pub fn encode(data: &[u8]) -> String {
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

pub fn decode64(text: &str) -> Vec<u8> {
    const SET: &[u8] = b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
    let mut back = [255u8; 256];
    for (at, byte) in SET.iter().enumerate() {
        back[*byte as usize] = at as u8;
    }
    let digits: Vec<u8> = text
        .bytes()
        .filter_map(|byte| {
            let found = back[byte as usize];
            if found == 255 { None } else { Some(found) }
        })
        .collect();
    let mut out = Vec::new();
    for group in digits.chunks(4) {
        let mut packed = 0u32;
        for (at, value) in group.iter().enumerate() {
            packed |= (*value as u32) << (18 - 6 * at);
        }
        out.push((packed >> 16) as u8);
        if group.len() > 2 {
            out.push((packed >> 8) as u8);
        }
        if group.len() > 3 {
            out.push(packed as u8);
        }
    }
    out
}

pub fn call(base: Value, key: Value, arguments: Vec<Value>, state: &mut State) -> Value {
    let target = match &base {
        Value::Undefined => key.clone(),
        other => member(other, &key, state),
    };
    if let Value::Method(host, name) = &target {
        return native(host, name, arguments, state);
    }
    if let Value::Prop(owner, name) = &target {
        return method(owner, name, &arguments, state);
    }
    match &target {
        Value::Host("read8") => Value::Num(state.read(1)),
        Value::Host("read16") => Value::Num(state.read(2)),
        Value::Host("read24") => Value::Num(state.read(3)),
        Value::Host("readconst") => constant(state),
        Value::Host("nop") | Value::Host("dispatch") => Value::Undefined,
        Value::Host("pushglobal") => {
            let offset = arguments.first().map_or(0.0, |found| found.number()) as usize;
            let base = state.base();
            let value = state.cell(base + offset);
            let sp = state.stack();
            state.put(sp, value);
            state.set_stack(sp + 1);
            Value::Undefined
        }
        Value::Host("storeglobal") => {
            let offset = arguments.first().map_or(0.0, |found| found.number()) as usize;
            let base = state.base();
            let sp = state.stack();
            let value = state.cell(sp - 1);
            state.put(base + offset, value);
            Value::Undefined
        }
        Value::Host("poptoacc") => {
            let sp = state.stack() - 1;
            state.set_stack(sp);
            let value = state.cell(sp);
            let acc = state.acc();
            state.put(acc, value);
            Value::Undefined
        }
        Value::Host("memberget") => {
            let sp = state.stack() - 1;
            state.set_stack(sp);
            let key = state.cell(sp);
            let holder = state.cell(sp - 1);
            let found = member(&holder, &key, state);
            state.put(sp - 1, found);
            Value::Undefined
        }
        Value::Host("memberset") => {
            let mut sp = state.stack() - 1;
            let key = state.cell(sp);
            sp -= 1;
            state.set_stack(sp);
            let holder = state.cell(sp);
            let value = state.cell(sp - 1);
            assign(&holder, &key, value, state);
            Value::Undefined
        }
        Value::Host("helper") => Value::Undefined,
        Value::Host("decode") => {
            let blob: Vec<u8> = match arguments.first() {
                Some(Value::List(items)) => {
                    items.borrow().iter().map(|item| item.number() as u8).collect()
                }
                other => {
                    let note = format!("decode table {} at step {}", other.map_or("none".to_string(), |v| v.text()), state.steps);
                    if !state.wanted.contains(&note) && state.wanted.len() < 200 {
                        state.wanted.push(note);
                    }
                    Vec::new()
                }
            };
            let index = arguments.get(1).map_or(0.0, |found| found.number()) as usize;
            let key = arguments.get(2).map_or(0.0, |found| found.number()) as u32;
            match crate::strings::decode(&blob, index, key) {
                Some(found) => Value::Text(Rc::new(found)),
                None => Value::Undefined,
            }
        }
        Value::Undefined => {
            let note = format!("undefined.{}", key.text());
            if !state.wanted.contains(&note) && state.wanted.len() < 300 {
                state.wanted.push(note);
            }
            Value::Undefined
        }
        _ => {
            let _ = arguments;
            Value::Undefined
        }
    }
}

fn constant(state: &mut State) -> Value {
    let tag = state.read(1) as i64;
    if tag & 128 != 0 {
        return Value::Num((tag & 127) as f64);
    }
    match state.tags.get(&(tag as u8)) {
        Some(crate::konst::Tag::Bool(found)) => Value::Bool(*found),
        Some(crate::konst::Tag::Null) => Value::Null,
        Some(crate::konst::Tag::Undefined) => Value::Undefined,
        Some(crate::konst::Tag::Int(width)) => {
            let width = *width;
            let raw = state.read(width);
            let bits = (width as u32) * 8;
            let limit = 2f64.powi(bits as i32);
            let signed = if raw >= limit / 2.0 { raw - limit } else { raw };
            Value::Num(signed)
        }
        Some(crate::konst::Tag::Float) => {
            let mut bits = 0u64;
            for _ in 0..8 {
                bits = (bits << 8) | state.read(1) as u64;
            }
            Value::Num(f64::from_bits(bits))
        }
        None => Value::Undefined,
    }
}

fn parse_head(body: &str, whole: bool) -> f64 {
    let trimmed = body.trim_start();
    let mut taken = String::new();
    for letter in trimmed.chars() {
        let keep = letter.is_ascii_digit()
            || (taken.is_empty() && (letter == '-' || letter == '+'))
            || (!whole && (letter == '.' || letter == 'e' || letter == 'E'));
        if !keep {
            break;
        }
        taken.push(letter);
    }
    taken.parse().unwrap_or(f64::NAN)
}

pub fn render(value: &Value) -> String {
    match value {
        Value::Undefined => "null".to_string(),
        Value::Null => "null".to_string(),
        Value::Bool(found) => found.to_string(),
        Value::Num(found) => {
            if found.is_finite() { number(*found) } else { "null".to_string() }
        }
        Value::Text(found) => quoted(found),
        Value::List(items) => {
            let parts: Vec<String> = items.borrow().iter().map(render).collect();
            format!("[{}]", parts.join(","))
        }
        Value::Map(entries) => {
            let parts: Vec<String> = entries
                .borrow()
                .iter()
                .filter(|(_, item)| !matches!(item, Value::Undefined))
                .map(|(key, item)| format!("{}:{}", quoted(key), render(item)))
                .collect();
            format!("{{{}}}", parts.join(","))
        }
        other => quoted(&other.text()),
    }
}

fn quoted(body: &str) -> String {
    let mut out = String::from("\"");
    for letter in body.chars() {
        match letter {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            other if (other as u32) < 0x20 => {
                out.push_str(&format!("\\u{:04x}", other as u32));
            }
            other => out.push(other),
        }
    }
    out.push('"');
    out
}
