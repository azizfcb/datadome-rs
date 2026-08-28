use std::cell::RefCell;
use std::collections::BTreeMap;
use std::rc::Rc;

use crate::exec::State;
use crate::run::Value;

pub fn make(tag: &str, fields: &[(&str, Value)]) -> Value {
    let mut entries = BTreeMap::new();
    entries.insert("tagName".to_string(), Value::Text(Rc::new(tag.to_string())));
    for (name, value) in fields {
        entries.insert((*name).to_string(), value.clone());
    }
    Value::Map(Rc::new(RefCell::new(entries)))
}

fn text(value: &Value) -> String {
    value.text()
}

pub fn element(state: &mut State, tag: &str) -> Value {
    let upper = tag.to_uppercase();
    if matches!(upper.as_str(), "PATH" | "RECT" | "CIRCLE" | "ELLIPSE" | "LINE" | "POLYGON" | "POLYLINE" | "TEXT" | "SVG") {
        return make(
            &upper,
            &[
                ("style", empty()),
                ("attributes", empty()),
                ("childNodes", Value::List(Rc::new(RefCell::new(Vec::new())))),
            ],
        );
    }
    let node = match upper.as_str() {
        "CANVAS" => make(
            "CANVAS",
            &[
                ("width", Value::Num(300.0)),
                ("height", Value::Num(150.0)),
                ("style", empty()),
            ],
        ),
        _ => make(
            &upper,
            &[
                ("style", empty()),
                ("offsetWidth", Value::Num(0.0)),
                ("offsetHeight", Value::Num(0.0)),
                ("clientWidth", Value::Num(state.host.inner_width)),
                ("clientHeight", Value::Num(state.host.inner_height)),
                ("innerHTML", Value::Text(Rc::new(String::new()))),
                ("childNodes", Value::List(Rc::new(RefCell::new(Vec::new())))),
            ],
        ),
    };
    node
}

fn empty() -> Value {
    Value::Map(Rc::new(RefCell::new(BTreeMap::new())))
}

fn field(entries: &Rc<RefCell<BTreeMap<String, Value>>>, name: &str) -> Value {
    entries.borrow().get(name).cloned().unwrap_or(Value::Undefined)
}

fn set(entries: &Rc<RefCell<BTreeMap<String, Value>>>, name: &str, value: Value) {
    entries.borrow_mut().insert(name.to_string(), value);
}

pub fn call(
    owner: &Rc<RefCell<BTreeMap<String, Value>>>,
    name: &str,
    arguments: &[Value],
    state: &mut State,
) -> Value {
    let at = |index: usize| arguments.get(index).cloned().unwrap_or(Value::Undefined);
    let tag = field(owner, "tagName").text();
    match (tag.as_str(), name) {
        ("CANVAS", "getContext") => {
            let kind = at(0).text();
            if kind == "2d" {
                let context = make(
                    "CanvasRenderingContext2D",
                    &[
                        ("canvas", Value::Map(owner.clone())),
                        ("fillStyle", Value::Text(Rc::new("#000000".to_string()))),
                        ("strokeStyle", Value::Text(Rc::new("#000000".to_string()))),
                        ("font", Value::Text(Rc::new("10px sans-serif".to_string()))),
                        ("globalAlpha", Value::Num(1.0)),
                        ("lineWidth", Value::Num(1.0)),
                        ("textBaseline", Value::Text(Rc::new("alphabetic".to_string()))),
                        ("shadowBlur", Value::Num(0.0)),
                        ("ops", Value::List(Rc::new(RefCell::new(Vec::new())))),
                    ],
                );
                set(owner, "context", context.clone());
                context
            } else {
                make(
                    "WebGLRenderingContext",
                    &[
                        ("canvas", Value::Map(owner.clone())),
                        ("drawingBufferWidth", field(owner, "width")),
                        ("drawingBufferHeight", field(owner, "height")),
                        ("ops", Value::List(Rc::new(RefCell::new(Vec::new())))),
                    ],
                )
            }
        }
        ("CANVAS", "toDataURL") => {
            let seal = seal(owner, state);
            Value::Text(Rc::new(format!("data:image/png;base64,{seal}")))
        }
        ("CanvasRenderingContext2D", "getImageData") => {
            let width = at(2).number().max(1.0) as usize;
            let height = at(3).number().max(1.0) as usize;
            let canvas = match field(owner, "canvas") {
                Value::Map(found) => found,
                _ => owner.clone(),
            };
            let pixels = raster(owner, &canvas, width, height, state);
            make(
                "ImageData",
                &[
                    ("width", Value::Num(width as f64)),
                    ("height", Value::Num(height as f64)),
                    ("data", Value::List(Rc::new(RefCell::new(pixels)))),
                ],
            )
        }
        ("CanvasRenderingContext2D", "measureText") => {
            let body = at(0).text();
            let size = font_size(&field(owner, "font").text());
            let width = body.chars().map(|c| advance(c)).sum::<f64>() * size;
            make(
                "TextMetrics",
                &[
                    ("width", Value::Num(width)),
                    ("actualBoundingBoxAscent", Value::Num(size * 0.72)),
                    ("actualBoundingBoxDescent", Value::Num(size * 0.21)),
                    ("actualBoundingBoxLeft", Value::Num(0.0)),
                    ("actualBoundingBoxRight", Value::Num(width)),
                ],
            )
        }
        ("CanvasRenderingContext2D", "createLinearGradient")
        | ("CanvasRenderingContext2D", "createRadialGradient") => {
            make("CanvasGradient", &[("stops", Value::List(Rc::new(RefCell::new(Vec::new()))))])
        }
        ("CanvasGradient", "addColorStop") => Value::Undefined,
        ("CanvasRenderingContext2D", "isPointInPath") => Value::Bool(false),
        ("CanvasRenderingContext2D", _) => {
            record(owner, name, arguments);
            Value::Undefined
        }
        (_, "getBoundingClientRect") => make(
            "DOMRect",
            &[
                ("x", Value::Num(0.0)),
                ("y", Value::Num(0.0)),
                ("width", field(owner, "offsetWidth")),
                ("height", field(owner, "offsetHeight")),
                ("top", Value::Num(0.0)),
                ("left", Value::Num(0.0)),
                ("right", field(owner, "offsetWidth")),
                ("bottom", field(owner, "offsetHeight")),
            ],
        ),
        (_, "appendChild") | (_, "removeChild") | (_, "remove") | (_, "setAttribute")
        | (_, "addEventListener") | (_, "removeEventListener") | (_, "insertBefore") => {
            at(0)
        }
        ("PATH", "getTotalLength")
        | ("RECT", "getTotalLength")
        | ("CIRCLE", "getTotalLength")
        | ("ELLIPSE", "getTotalLength")
        | ("LINE", "getTotalLength")
        | ("POLYGON", "getTotalLength")
        | ("POLYLINE", "getTotalLength") => Value::Num(path_length(owner, &tag)),
        (_, "getBBox") => {
            let box_of = path_box(owner, &tag);
            make(
                "SVGRect",
                &[
                    ("x", Value::Num(box_of.0)),
                    ("y", Value::Num(box_of.1)),
                    ("width", Value::Num(box_of.2)),
                    ("height", Value::Num(box_of.3)),
                ],
            )
        }
        ("WebGLRenderingContext", _) => gl(owner, name, arguments, state),
        ("Date", _) => clock(owner, name, arguments, state),
        (_, "getAttribute") => Value::Null,
        (_, "querySelector") | (_, "getElementById") => {
            let inner = field(owner, "innerHTML").text();
            if std::env::var("DD_CSS").is_ok() {
                eprintln!("select {tag} {} inner {}", at(0).text(), inner.len());
            }
            if inner.is_empty() { query(state, &at(0).text()) } else { pick(&inner, &at(0).text()) }
        }
        (_, "querySelectorAll") | (_, "getElementsByTagName")
        | (_, "getElementsByClassName") => {
            let picked = query(state, &at(0).text());
            let items = match picked {
                Value::Null => Vec::new(),
                other => vec![other],
            };
            Value::List(Rc::new(RefCell::new(items)))
        }
        (_, "toString") => Value::Text(Rc::new(format!("[object {tag}]"))),
        _ => {
            let note = format!("{tag}.{name}/{}", arguments.len());
            state.miss(&note);
            Value::Undefined
        }
    }
}

fn record(owner: &Rc<RefCell<BTreeMap<String, Value>>>, name: &str, arguments: &[Value]) {
    let Value::List(ops) = field(owner, "ops") else { return };
    let mut line = String::from(name);
    for value in arguments {
        line.push('|');
        line.push_str(&text(value));
    }
    if name == "fillText" || name == "strokeText" || name == "fillRect" {
        line.push('|');
        line.push_str(&field(owner, "fillStyle").text());
        line.push('|');
        line.push_str(&field(owner, "font").text());
    }
    ops.borrow_mut().push(Value::Text(Rc::new(line)));
}

fn font_size(font: &str) -> f64 {
    let mut digits = String::new();
    for part in font.split_whitespace() {
        if let Some(head) = part.strip_suffix("px") {
            digits = head.to_string();
            break;
        }
    }
    digits.parse().unwrap_or(10.0)
}

fn advance(letter: char) -> f64 {
    match letter {
        'i' | 'j' | 'l' | 'I' | '.' | ',' | '\'' | '!' | '|' => 0.28,
        'f' | 't' | 'r' | '(' | ')' | '[' | ']' | ' ' => 0.34,
        'm' | 'M' | 'W' | 'w' | '@' => 0.86,
        'A'..='Z' => 0.67,
        '0'..='9' => 0.556,
        _ => 0.52,
    }
}

fn seal(canvas: &Rc<RefCell<BTreeMap<String, Value>>>, state: &mut State) -> String {
    let context = match field(canvas, "context") {
        Value::Map(found) => found,
        _ => return String::new(),
    };
    let mut blob = String::new();
    if let Value::List(ops) = field(&context, "ops") {
        for item in ops.borrow().iter() {
            blob.push_str(&item.text());
            blob.push(';');
        }
    }
    blob.push_str(&state.host.renderer);
    crate::run::encode(digest(&blob).as_bytes())
}

fn raster(
    context: &Rc<RefCell<BTreeMap<String, Value>>>,
    canvas: &Rc<RefCell<BTreeMap<String, Value>>>,
    width: usize,
    height: usize,
    state: &mut State,
) -> Vec<Value> {
    let mut blob = String::new();
    if let Value::List(ops) = field(context, "ops") {
        for item in ops.borrow().iter() {
            blob.push_str(&item.text());
            blob.push(';');
        }
    }
    blob.push_str(&field(canvas, "width").text());
    blob.push('x');
    blob.push_str(&field(canvas, "height").text());
    blob.push_str(&state.host.renderer);
    let key = digest(&blob);
    let mut seed = 0u64;
    for byte in key.as_bytes() {
        seed = seed.wrapping_mul(1099511628211).wrapping_add(u64::from(*byte));
    }
    let mut out = Vec::with_capacity(width * height * 4);
    for _ in 0..width * height {
        for channel in 0..4 {
            seed ^= seed << 13;
            seed ^= seed >> 7;
            seed ^= seed << 17;
            let value = if channel == 3 { 255 } else { (seed >> 24) as u8 };
            out.push(Value::Num(f64::from(value)));
        }
    }
    out
}

fn digest(body: &str) -> String {
    let mut state: [u32; 8] = [
        0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a, 0x510e527f, 0x9b05688c, 0x1f83d9ab,
        0x5be0cd19,
    ];
    const ROUND: [u32; 64] = [
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
    let mut data = body.as_bytes().to_vec();
    let bits = (data.len() as u64) * 8;
    data.push(0x80);
    while data.len() % 64 != 56 {
        data.push(0);
    }
    data.extend_from_slice(&bits.to_be_bytes());
    for chunk in data.chunks(64) {
        let mut words = [0u32; 64];
        for (at, piece) in chunk.chunks(4).enumerate() {
            words[at] = u32::from_be_bytes([piece[0], piece[1], piece[2], piece[3]]);
        }
        for at in 16..64 {
            let a = words[at - 15];
            let b = words[at - 2];
            let x = a.rotate_right(7) ^ a.rotate_right(18) ^ (a >> 3);
            let y = b.rotate_right(17) ^ b.rotate_right(19) ^ (b >> 10);
            words[at] = words[at - 16]
                .wrapping_add(x)
                .wrapping_add(words[at - 7])
                .wrapping_add(y);
        }
        let mut work = state;
        for at in 0..64 {
            let s1 = work[4].rotate_right(6) ^ work[4].rotate_right(11) ^ work[4].rotate_right(25);
            let choice = (work[4] & work[5]) ^ (!work[4] & work[6]);
            let one = work[7]
                .wrapping_add(s1)
                .wrapping_add(choice)
                .wrapping_add(ROUND[at])
                .wrapping_add(words[at]);
            let s0 = work[0].rotate_right(2) ^ work[0].rotate_right(13) ^ work[0].rotate_right(22);
            let major = (work[0] & work[1]) ^ (work[0] & work[2]) ^ (work[1] & work[2]);
            let two = s0.wrapping_add(major);
            work[7] = work[6];
            work[6] = work[5];
            work[5] = work[4];
            work[4] = work[3].wrapping_add(one);
            work[3] = work[2];
            work[2] = work[1];
            work[1] = work[0];
            work[0] = one.wrapping_add(two);
        }
        for at in 0..8 {
            state[at] = state[at].wrapping_add(work[at]);
        }
    }
    state.iter().map(|word| format!("{word:08x}")).collect()
}

fn number(entries: &Rc<RefCell<BTreeMap<String, Value>>>, name: &str) -> f64 {
    field(entries, name).number()
}

fn points(owner: &Rc<RefCell<BTreeMap<String, Value>>>) -> Vec<(f64, f64)> {
    let body = field(owner, "d").text();
    let mut found = Vec::new();
    let mut digits = String::new();
    let mut pair: Vec<f64> = Vec::new();
    for letter in body.chars().chain(std::iter::once(' ')) {
        if letter.is_ascii_digit() || letter == '.' || (letter == '-' && digits.is_empty()) {
            digits.push(letter);
            continue;
        }
        if !digits.is_empty() {
            if let Ok(value) = digits.parse::<f64>() {
                pair.push(value);
                if pair.len() == 2 {
                    found.push((pair[0], pair[1]));
                    pair.clear();
                }
            }
            digits.clear();
        }
    }
    found
}

fn path_length(owner: &Rc<RefCell<BTreeMap<String, Value>>>, tag: &str) -> f64 {
    match tag {
        "RECT" => 2.0 * (number(owner, "width") + number(owner, "height")),
        "CIRCLE" => 2.0 * std::f64::consts::PI * number(owner, "r"),
        "LINE" => {
            let dx = number(owner, "x2") - number(owner, "x1");
            let dy = number(owner, "y2") - number(owner, "y1");
            (dx * dx + dy * dy).sqrt()
        }
        _ => {
            let path = points(owner);
            let mut total = 0.0;
            for pair in path.windows(2) {
                let dx = pair[1].0 - pair[0].0;
                let dy = pair[1].1 - pair[0].1;
                total += (dx * dx + dy * dy).sqrt();
            }
            total
        }
    }
}

fn path_box(owner: &Rc<RefCell<BTreeMap<String, Value>>>, tag: &str) -> (f64, f64, f64, f64) {
    match tag {
        "RECT" => (
            number(owner, "x"),
            number(owner, "y"),
            number(owner, "width"),
            number(owner, "height"),
        ),
        "CIRCLE" => {
            let r = number(owner, "r");
            (number(owner, "cx") - r, number(owner, "cy") - r, r * 2.0, r * 2.0)
        }
        _ => {
            let path = points(owner);
            if path.is_empty() {
                return (0.0, 0.0, 0.0, 0.0);
            }
            let left = path.iter().map(|p| p.0).fold(f64::INFINITY, f64::min);
            let right = path.iter().map(|p| p.0).fold(f64::NEG_INFINITY, f64::max);
            let top = path.iter().map(|p| p.1).fold(f64::INFINITY, f64::min);
            let bottom = path.iter().map(|p| p.1).fold(f64::NEG_INFINITY, f64::max);
            (left, top, right - left, bottom - top)
        }
    }
}

pub fn date(time: f64) -> Value {
    make("Date", &[("time", Value::Num(time))])
}

fn clock(
    owner: &Rc<RefCell<BTreeMap<String, Value>>>,
    name: &str,
    arguments: &[Value],
    state: &mut State,
) -> Value {
    let time = number(owner, "time");
    let offset = state.host.offset;
    let local = time - offset * 60_000.0;
    let day = (local / 86_400_000.0).floor();
    let inside = local - day * 86_400_000.0;
    match name {
        "getTime" | "valueOf" => Value::Num(time),
        "getTimezoneOffset" => Value::Num(offset),
        "getMilliseconds" => Value::Num(inside % 1000.0),
        "getSeconds" => Value::Num((inside / 1000.0).floor() % 60.0),
        "getMinutes" => Value::Num((inside / 60_000.0).floor() % 60.0),
        "getHours" => Value::Num((inside / 3_600_000.0).floor() % 24.0),
        "getDay" => Value::Num((day + 4.0) % 7.0),
        "getDate" | "getFullYear" | "getMonth" | "getYear" => {
            let (year, month, mday) = civil(day as i64);
            Value::Num(match name {
                "getFullYear" => year as f64,
                "getYear" => (year - 1900) as f64,
                "getMonth" => (month - 1) as f64,
                _ => mday as f64,
            })
        }
        "toString" | "toDateString" | "toTimeString" | "toLocaleString"
        | "toLocaleDateString" | "toLocaleTimeString" | "toISOString" | "toUTCString" => {
            let (year, month, mday) = civil(day as i64);
            let hour = (inside / 3_600_000.0).floor() as i64 % 24;
            let minute = (inside / 60_000.0).floor() as i64 % 60;
            let second = (inside / 1000.0).floor() as i64 % 60;
            let zone = state.host.timezone.clone();
            let body = match name {
                "toISOString" => format!(
                    "{year:04}-{month:02}-{mday:02}T{hour:02}:{minute:02}:{second:02}.000Z"
                ),
                "toLocaleDateString" => format!("{month}/{mday}/{year}"),
                "toLocaleTimeString" => format!("{hour:02}:{minute:02}:{second:02}"),
                "toLocaleString" => {
                    format!("{month}/{mday}/{year}, {hour:02}:{minute:02}:{second:02}")
                }
                _ => format!(
                    "{} {} {mday:02} {year} {hour:02}:{minute:02}:{second:02} GMT{}{:02}{:02} ({zone})",
                    weekday(day as i64),
                    month_name(month),
                    if offset <= 0.0 { "+" } else { "-" },
                    (offset.abs() / 60.0) as i64,
                    (offset.abs() % 60.0) as i64,
                ),
            };
            Value::Text(Rc::new(body))
        }
        _ => {
            let _ = arguments;
            state.miss(&format!("Date.{name}"));
            Value::Undefined
        }
    }
}

fn weekday(day: i64) -> &'static str {
    ["Thu", "Fri", "Sat", "Sun", "Mon", "Tue", "Wed"][(day.rem_euclid(7)) as usize]
}

fn month_name(month: i64) -> &'static str {
    [
        "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec",
    ][(month - 1).clamp(0, 11) as usize]
}

fn civil(day: i64) -> (i64, i64, i64) {
    let z = day + 719_468;
    let era = if z >= 0 { z } else { z - 146_096 } / 146_097;
    let doe = z - era * 146_097;
    let yoe = (doe - doe / 1460 + doe / 36_524 - doe / 146_096) / 365;
    let year = yoe + era * 400;
    let doy = doe - (365 * yoe + yoe / 4 - yoe / 100);
    let mp = (5 * doy + 2) / 153;
    let mday = doy - (153 * mp + 2) / 5 + 1;
    let month = if mp < 10 { mp + 3 } else { mp - 9 };
    (if month <= 2 { year + 1 } else { year }, month, mday)
}

fn gl(
    owner: &Rc<RefCell<BTreeMap<String, Value>>>,
    name: &str,
    arguments: &[Value],
    state: &mut State,
) -> Value {
    let at = |index: usize| arguments.get(index).cloned().unwrap_or(Value::Undefined);
    match name {
        "getParameter" => {
            let code = at(0).number() as u32;
            match code {
                0x1F00 => Value::Text(Rc::new("WebKit".to_string())),
                0x1F01 => Value::Text(Rc::new("WebKit WebGL".to_string())),
                0x1F02 => Value::Text(Rc::new("WebGL 1.0 (OpenGL ES 2.0 Chromium)".to_string())),
                0x8B8C => Value::Text(Rc::new(
                    "WebGL GLSL ES 1.0 (OpenGL ES GLSL ES 1.0 Chromium)".to_string(),
                )),
                0x9245 => Value::Text(Rc::new(state.host.vendor.clone())),
                0x9246 => Value::Text(Rc::new(state.host.renderer.clone())),
                0x0D33 => Value::Num(16384.0),
                0x851C => Value::Num(16384.0),
                0x8869 => Value::Num(32.0),
                0x8872 => Value::Num(16.0),
                0x8B4D => Value::Num(32.0),
                0x8B4C => Value::Num(16.0),
                0x8DFB => Value::Num(1024.0),
                0x8DFC => Value::Num(1024.0),
                0x8DFD => Value::Num(32.0),
                0x0D3A => Value::List(Rc::new(RefCell::new(vec![
                    Value::Num(32767.0),
                    Value::Num(32767.0),
                ]))),
                0x846D | 0x846E => Value::List(Rc::new(RefCell::new(vec![
                    Value::Num(1.0),
                    Value::Num(1.0),
                ]))),
                _ => Value::Null,
            }
        }
        "getSupportedExtensions" => Value::List(Rc::new(RefCell::new(
            EXTENSIONS.iter().map(|name| Value::Text(Rc::new((*name).to_string()))).collect(),
        ))),
        "getExtension" => {
            let wanted = at(0).text();
            if wanted == "WEBGL_debug_renderer_info" {
                make(
                    "WEBGL_debug_renderer_info",
                    &[
                        ("UNMASKED_VENDOR_WEBGL", Value::Num(37445.0)),
                        ("UNMASKED_RENDERER_WEBGL", Value::Num(37446.0)),
                    ],
                )
            } else if EXTENSIONS.contains(&wanted.as_str()) {
                make(&wanted, &[])
            } else {
                Value::Null
            }
        }
        "createShader" | "createProgram" | "createBuffer" | "createTexture"
        | "createFramebuffer" | "createRenderbuffer" | "createVertexArray" => {
            make("WebGLObject", &[("kind", Value::Text(Rc::new(name.to_string())))])
        }
        "getUniformLocation" => make("WebGLUniformLocation", &[]),
        "getAttribLocation" => Value::Num(0.0),
        "getShaderParameter" | "getProgramParameter" => Value::Bool(true),
        "getShaderInfoLog" | "getProgramInfoLog" => Value::Text(Rc::new(String::new())),
        "getShaderPrecisionFormat" => make(
            "WebGLShaderPrecisionFormat",
            &[
                ("rangeMin", Value::Num(127.0)),
                ("rangeMax", Value::Num(127.0)),
                ("precision", Value::Num(23.0)),
            ],
        ),
        "getContextAttributes" => make(
            "WebGLContextAttributes",
            &[
                ("alpha", Value::Bool(true)),
                ("antialias", Value::Bool(true)),
                ("depth", Value::Bool(true)),
                ("desynchronized", Value::Bool(false)),
                ("failIfMajorPerformanceCaveat", Value::Bool(false)),
                ("powerPreference", Value::Text(Rc::new("default".to_string()))),
                ("premultipliedAlpha", Value::Bool(true)),
                ("preserveDrawingBuffer", Value::Bool(false)),
                ("stencil", Value::Bool(false)),
                ("xrCompatible", Value::Bool(false)),
            ],
        ),
        "readPixels" => {
            let width = at(2).number().max(1.0) as usize;
            let height = at(3).number().max(1.0) as usize;
            if let Some(Value::List(target)) = arguments.get(6) {
                let mut blob = format!("{}|{}|{width}x{height}", state.host.vendor, state.host.renderer);
                if let Value::List(ops) = field(owner, "ops") {
                    for item in ops.borrow().iter() {
                        blob.push_str(&item.text());
                        blob.push(';');
                    }
                }
                let key = digest(&blob);
                let mut seed = 0u64;
                for byte in key.as_bytes() {
                    seed = seed.wrapping_mul(1099511628211).wrapping_add(u64::from(*byte));
                }
                let mut list = target.borrow_mut();
                for slot in list.iter_mut() {
                    seed ^= seed << 13;
                    seed ^= seed >> 7;
                    seed ^= seed << 17;
                    *slot = Value::Num(f64::from((seed >> 24) as u8));
                }
            }
            Value::Undefined
        }
        _ => {
            record(owner, name, arguments);
            Value::Undefined
        }
    }
}

const EXTENSIONS: [&str; 26] = [
    "ANGLE_instanced_arrays",
    "EXT_blend_minmax",
    "EXT_clip_control",
    "EXT_color_buffer_half_float",
    "EXT_depth_clamp",
    "EXT_disjoint_timer_query",
    "EXT_float_blend",
    "EXT_frag_depth",
    "EXT_polygon_offset_clamp",
    "EXT_shader_texture_lod",
    "EXT_texture_compression_bptc",
    "EXT_texture_compression_rgtc",
    "EXT_texture_filter_anisotropic",
    "EXT_texture_mirror_clamp_to_edge",
    "EXT_sRGB",
    "OES_element_index_uint",
    "OES_fbo_render_mipmap",
    "OES_standard_derivatives",
    "OES_texture_float",
    "OES_texture_float_linear",
    "OES_texture_half_float",
    "OES_texture_half_float_linear",
    "OES_vertex_array_object",
    "WEBGL_color_buffer_float",
    "WEBGL_debug_renderer_info",
    "WEBGL_debug_shaders",
];

pub fn query(state: &mut State, selector: &str) -> Value {
    let page = state.host.document.clone();
    if page.is_empty() {
        return Value::Null;
    }
    seek(&page, selector)
}

pub fn pick(page: &str, selector: &str) -> Value {
    let tree = crate::css::parse(page);
    let Some(at) = tree.find(selector) else { return Value::Null };
    let width = tree.width(at);
    if std::env::var("DD_CSS").is_ok() {
        eprintln!("css {selector} -> {width}");
    }
    let node = &tree.nodes[at];
    make(
        &node.tag.to_uppercase(),
        &[
            ("offsetWidth", Value::Num(width.round())),
            ("offsetHeight", Value::Num(0.0)),
            ("clientWidth", Value::Num(width.round())),
            ("clientHeight", Value::Num(0.0)),
            ("offsetLeft", Value::Num(0.0)),
            ("offsetTop", Value::Num(0.0)),
            ("id", Value::Text(Rc::new(node.id.clone()))),
            ("className", Value::Text(Rc::new(node.classes.join(" ")))),
            ("style", empty()),
            ("childNodes", Value::List(Rc::new(RefCell::new(Vec::new())))),
        ],
    )
}

fn seek(page: &str, selector: &str) -> Value {
    let want = selector.trim();
    let hit = match want.chars().next() {
        Some('#') => find(&page, "id", &want[1..]),
        Some('.') => find(&page, "class", &want[1..]),
        Some('[') => {
            let inner = want.trim_start_matches('[').trim_end_matches(']');
            match inner.split_once('=') {
                Some((name, value)) => {
                    find(&page, name, value.trim_matches(|c| c == '"' || c == '\''))
                }
                None => None,
            }
        }
        _ => tagged(&page, want),
    };
    match hit {
        Some((tag, attributes)) => node(page, &tag, &attributes),
        None => Value::Null,
    }
}

fn find(page: &str, attribute: &str, value: &str) -> Option<(String, String)> {
    let needle = format!("{attribute}=\"{value}\"");
    let mut at = 0usize;
    while let Some(spot) = page[at..].find(&needle) {
        let absolute = at + spot;
        if let Some(open) = page[..absolute].rfind('<') {
            if let Some(close) = page[absolute..].find('>') {
                let body = &page[open + 1..absolute + close];
                let tag = body.split(|c: char| c.is_whitespace()).next().unwrap_or("div");
                if attribute != "class" || classed(body, value) {
                    return Some((tag.to_string(), body.to_string()));
                }
            }
        }
        at = absolute + needle.len();
    }
    None
}

fn classed(body: &str, value: &str) -> bool {
    let Some(spot) = body.find("class=\"") else { return false };
    let rest = &body[spot + 7..];
    let Some(end) = rest.find('"') else { return false };
    rest[..end].split_whitespace().any(|name| name == value)
}

fn tagged(page: &str, name: &str) -> Option<(String, String)> {
    let needle = format!("<{name}");
    let spot = page.find(&needle)?;
    let close = page[spot..].find('>')?;
    Some((name.to_string(), page[spot + 1..spot + close].to_string()))
}

fn attribute(body: &str, name: &str) -> Option<String> {
    let needle = format!("{name}=\"");
    let spot = body.find(&needle)?;
    let rest = &body[spot + needle.len()..];
    let end = rest.find('"')?;
    Some(rest[..end].to_string())
}

fn node(page: &str, tag: &str, body: &str) -> Value {
    let style = attribute(body, "style").unwrap_or_default();
    let width = measure(&style, "width").unwrap_or_else(|| {
        attribute(body, "width").and_then(|found| found.parse().ok()).unwrap_or(0.0)
    });
    let height = measure(&style, "height").unwrap_or_else(|| {
        attribute(body, "height").and_then(|found| found.parse().ok()).unwrap_or(0.0)
    });
    let mut fields: Vec<(&str, Value)> = vec![
        ("offsetWidth", Value::Num(width)),
        ("offsetHeight", Value::Num(height)),
        ("clientWidth", Value::Num(width)),
        ("clientHeight", Value::Num(height)),
        ("offsetLeft", Value::Num(0.0)),
        ("offsetTop", Value::Num(0.0)),
        ("style", empty()),
        ("childNodes", Value::List(Rc::new(RefCell::new(Vec::new())))),
    ];
    let id = attribute(body, "id").unwrap_or_default();
    let names = attribute(body, "class").unwrap_or_default();
    let text = Value::Text(Rc::new(id));
    let list = Value::Text(Rc::new(names));
    fields.push(("id", text));
    fields.push(("className", list));
    let _ = page;
    make(&tag.to_uppercase(), &fields)
}

fn measure(style: &str, name: &str) -> Option<f64> {
    for rule in style.split(';') {
        let (key, value) = rule.split_once(':')?;
        if key.trim() == name {
            let raw = value.trim().trim_end_matches("px");
            return raw.parse().ok();
        }
    }
    None
}
