#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Val {
    I32,
    I64,
    F32,
    F64,
    V128,
    Func,
    Extern,
}

impl Val {
    pub fn name(self) -> &'static str {
        match self {
            Val::I32 => "i32",
            Val::I64 => "i64",
            Val::F32 => "f32",
            Val::F64 => "f64",
            Val::V128 => "v128",
            Val::Func => "funcref",
            Val::Extern => "externref",
        }
    }
}

#[derive(Clone, Debug)]
pub struct Type {
    pub params: Vec<Val>,
    pub results: Vec<Val>,
}

#[derive(Clone, Debug)]
pub struct Import {
    pub module: String,
    pub name: String,
    pub kind: Kind,
}

#[derive(Clone, Copy, Debug)]
pub enum Kind {
    Func(u32),
    Table,
    Memory,
    Global,
}

#[derive(Clone, Debug)]
pub struct Export {
    pub name: String,
    pub kind: Kind,
    pub index: u32,
}

#[derive(Clone, Debug)]
pub struct Global {
    pub ty: Val,
    pub mutable: bool,
    pub init: Vec<u8>,
}

#[derive(Clone, Debug)]
pub struct Body {
    pub locals: Vec<Val>,
    pub code: Vec<u8>,
}

#[derive(Clone, Debug)]
pub struct Data {
    pub offset: Option<Vec<u8>>,
    pub bytes: Vec<u8>,
}

#[derive(Clone, Debug)]
pub struct Element {
    pub funcs: Vec<u32>,
}

#[derive(Default, Debug)]
pub struct Module {
    pub types: Vec<Type>,
    pub imports: Vec<Import>,
    pub funcs: Vec<u32>,
    pub tables: Vec<(Val, u32, Option<u32>)>,
    pub memories: Vec<(u32, Option<u32>)>,
    pub globals: Vec<Global>,
    pub exports: Vec<Export>,
    pub start: Option<u32>,
    pub elements: Vec<Element>,
    pub bodies: Vec<Body>,
    pub data: Vec<Data>,
    pub names: Names,
}

#[derive(Default, Debug)]
pub struct Names {
    pub funcs: Vec<(u32, String)>,
    pub locals: Vec<(u32, Vec<(u32, String)>)>,
}

impl Module {
    pub fn imported_funcs(&self) -> usize {
        self.imports.iter().filter(|i| matches!(i.kind, Kind::Func(_))).count()
    }

    pub fn func_type(&self, index: u32) -> Option<&Type> {
        let imported = self.imported_funcs();
        let type_index = if (index as usize) < imported {
            match self.imports.iter().filter(|i| matches!(i.kind, Kind::Func(_))).nth(index as usize)?.kind {
                Kind::Func(t) => t,
                _ => return None,
            }
        } else {
            *self.funcs.get(index as usize - imported)?
        };
        self.types.get(type_index as usize)
    }

    pub fn func_name(&self, index: u32) -> String {
        if let Some((_, name)) = self.names.funcs.iter().find(|(i, _)| *i == index) {
            return name.clone();
        }
        let imported = self.imported_funcs();
        if (index as usize) < imported
            && let Some(import) =
                self.imports.iter().filter(|i| matches!(i.kind, Kind::Func(_))).nth(index as usize)
        {
            return format!("{}.{}", import.module, import.name);
        }
        if let Some(export) = self
            .exports
            .iter()
            .find(|e| matches!(e.kind, Kind::Func(_)) && e.index == index)
        {
            return export.name.clone();
        }
        format!("f{index}")
    }
}

pub struct Reader<'a> {
    pub bytes: &'a [u8],
    pub at: usize,
}

impl<'a> Reader<'a> {
    pub fn new(bytes: &'a [u8]) -> Self {
        Reader { bytes, at: 0 }
    }

    pub fn done(&self) -> bool {
        self.at >= self.bytes.len()
    }

    pub fn byte(&mut self) -> Option<u8> {
        let b = *self.bytes.get(self.at)?;
        self.at += 1;
        Some(b)
    }

    pub fn take(&mut self, n: usize) -> Option<&'a [u8]> {
        let s = self.bytes.get(self.at..self.at + n)?;
        self.at += n;
        Some(s)
    }

    pub fn uleb(&mut self) -> Option<u64> {
        let mut out: u64 = 0;
        let mut shift = 0;
        loop {
            let b = self.byte()?;
            out |= ((b & 0x7f) as u64) << shift;
            if b & 0x80 == 0 {
                return Some(out);
            }
            shift += 7;
            if shift > 63 {
                return None;
            }
        }
    }

    pub fn sleb(&mut self) -> Option<i64> {
        let mut out: i64 = 0;
        let mut shift = 0;
        loop {
            let b = self.byte()?;
            out |= ((b & 0x7f) as i64) << shift;
            shift += 7;
            if b & 0x80 == 0 {
                if shift < 64 && b & 0x40 != 0 {
                    out |= -1i64 << shift;
                }
                return Some(out);
            }
            if shift > 70 {
                return None;
            }
        }
    }

    pub fn u32(&mut self) -> Option<u32> {
        self.uleb().map(|v| v as u32)
    }

    pub fn name(&mut self) -> Option<String> {
        let n = self.uleb()? as usize;
        String::from_utf8(self.take(n)?.to_vec()).ok()
    }

    pub fn val(&mut self) -> Option<Val> {
        Some(match self.byte()? {
            0x7f => Val::I32,
            0x7e => Val::I64,
            0x7d => Val::F32,
            0x7c => Val::F64,
            0x7b => Val::V128,
            0x70 => Val::Func,
            0x6f => Val::Extern,
            _ => return None,
        })
    }

    fn limits(&mut self) -> Option<(u32, Option<u32>)> {
        let flags = self.byte()?;
        let min = self.u32()?;
        let max = if flags & 1 != 0 { Some(self.u32()?) } else { None };
        Some((min, max))
    }

    fn expr(&mut self) -> Option<Vec<u8>> {
        let start = self.at;
        let mut depth = 0usize;
        loop {
            let op = *self.bytes.get(self.at)?;
            match op {
                0x02 | 0x03 | 0x04 => depth += 1,
                0x0b if depth == 0 => {
                    let out = self.bytes.get(start..self.at)?.to_vec();
                    self.at += 1;
                    return Some(out);
                }
                0x0b => depth -= 1,
                _ => {}
            }
            crate::code::skip(self)?;
        }
    }
}

pub fn module(bytes: &[u8]) -> Option<Module> {
    let mut r = Reader::new(bytes);
    r.take(8)?;
    let mut m = Module::default();
    while !r.done() {
        let id = r.byte()?;
        let size = r.uleb()? as usize;
        let body = r.take(size)?;
        let mut s = Reader::new(body);
        match id {
            0 => custom(&mut s, &mut m),
            1 => types(&mut s, &mut m)?,
            2 => imports(&mut s, &mut m)?,
            3 => {
                for _ in 0..s.u32()? {
                    m.funcs.push(s.u32()?);
                }
            }
            4 => {
                for _ in 0..s.u32()? {
                    let ty = s.val()?;
                    let (min, max) = s.limits()?;
                    m.tables.push((ty, min, max));
                }
            }
            5 => {
                for _ in 0..s.u32()? {
                    m.memories.push(s.limits()?);
                }
            }
            6 => globals(&mut s, &mut m)?,
            7 => exports(&mut s, &mut m)?,
            8 => m.start = Some(s.u32()?),
            9 => elements(&mut s, &mut m)?,
            10 => code(&mut s, &mut m)?,
            11 => data(&mut s, &mut m)?,
            _ => {}
        }
    }
    Some(m)
}

fn types(s: &mut Reader, m: &mut Module) -> Option<()> {
    for _ in 0..s.u32()? {
        if s.byte()? != 0x60 {
            return None;
        }
        let mut params = Vec::new();
        for _ in 0..s.u32()? {
            params.push(s.val()?);
        }
        let mut results = Vec::new();
        for _ in 0..s.u32()? {
            results.push(s.val()?);
        }
        m.types.push(Type { params, results });
    }
    Some(())
}

fn imports(s: &mut Reader, m: &mut Module) -> Option<()> {
    for _ in 0..s.u32()? {
        let module = s.name()?;
        let name = s.name()?;
        let kind = match s.byte()? {
            0 => Kind::Func(s.u32()?),
            1 => {
                s.val()?;
                s.limits()?;
                Kind::Table
            }
            2 => {
                s.limits()?;
                Kind::Memory
            }
            3 => {
                s.val()?;
                s.byte()?;
                Kind::Global
            }
            _ => return None,
        };
        m.imports.push(Import { module, name, kind });
    }
    Some(())
}

fn globals(s: &mut Reader, m: &mut Module) -> Option<()> {
    for _ in 0..s.u32()? {
        let ty = s.val()?;
        let mutable = s.byte()? != 0;
        let start = s.at;
        s.expr()?;
        let init = s.bytes.get(start..s.at).unwrap_or_default().to_vec();
        m.globals.push(Global { ty, mutable, init });
    }
    Some(())
}

fn exports(s: &mut Reader, m: &mut Module) -> Option<()> {
    for _ in 0..s.u32()? {
        let name = s.name()?;
        let tag = s.byte()?;
        let index = s.u32()?;
        let kind = match tag {
            0 => Kind::Func(0),
            1 => Kind::Table,
            2 => Kind::Memory,
            _ => Kind::Global,
        };
        m.exports.push(Export { name, kind, index });
    }
    Some(())
}

fn elements(s: &mut Reader, m: &mut Module) -> Option<()> {
    for _ in 0..s.u32()? {
        let flags = s.u32()?;
        if flags & 1 == 0 {
            if flags & 2 != 0 {
                s.u32()?;
            }
            s.expr()?;
        }
        if flags & 3 == 3 || flags & 3 == 1 {
            s.byte()?;
        } else if flags & 2 != 0 {
            s.byte()?;
        }
        let mut funcs = Vec::new();
        for _ in 0..s.u32()? {
            funcs.push(s.u32()?);
        }
        m.elements.push(Element { funcs });
    }
    Some(())
}

fn code(s: &mut Reader, m: &mut Module) -> Option<()> {
    for _ in 0..s.u32()? {
        let size = s.uleb()? as usize;
        let body = s.take(size)?;
        let mut b = Reader::new(body);
        let mut locals = Vec::new();
        for _ in 0..b.u32()? {
            let count = b.u32()?;
            let ty = b.val()?;
            for _ in 0..count {
                locals.push(ty);
            }
        }
        m.bodies.push(Body { locals, code: b.bytes[b.at..].to_vec() });
    }
    Some(())
}

fn data(s: &mut Reader, m: &mut Module) -> Option<()> {
    for _ in 0..s.u32()? {
        let flags = s.u32()?;
        let offset = if flags & 1 == 0 {
            if flags & 2 != 0 {
                s.u32()?;
            }
            Some(s.expr()?)
        } else {
            None
        };
        let n = s.uleb()? as usize;
        m.data.push(Data { offset, bytes: s.take(n)?.to_vec() });
    }
    Some(())
}

fn custom(s: &mut Reader, m: &mut Module) {
    let Some(name) = s.name() else { return };
    if name != "name" {
        return;
    }
    while !s.done() {
        let Some(id) = s.byte() else { return };
        let Some(size) = s.uleb() else { return };
        let Some(body) = s.take(size as usize) else { return };
        let mut b = Reader::new(body);
        match id {
            1 => {
                let Some(count) = b.u32() else { continue };
                for _ in 0..count {
                    let (Some(i), Some(n)) = (b.u32(), b.name()) else { break };
                    m.names.funcs.push((i, n));
                }
            }
            2 => {
                let Some(count) = b.u32() else { continue };
                for _ in 0..count {
                    let Some(f) = b.u32() else { break };
                    let Some(inner) = b.u32() else { break };
                    let mut list = Vec::new();
                    for _ in 0..inner {
                        let (Some(i), Some(n)) = (b.u32(), b.name()) else { break };
                        list.push((i, n));
                    }
                    m.names.locals.push((f, list));
                }
            }
            _ => {}
        }
    }
}
