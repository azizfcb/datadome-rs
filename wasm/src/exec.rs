use crate::parse::{Kind, Module, Reader, Val};

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum Cell {
    I32(i32),
    I64(i64),
    F32(f32),
    F64(f64),
    Ref(u32),
}

impl Cell {
    pub fn i32(self) -> i32 {
        match self {
            Cell::I32(found) => found,
            Cell::I64(found) => found as i32,
            Cell::F32(found) => found as i32,
            Cell::F64(found) => found as i32,
            Cell::Ref(found) => found as i32,
        }
    }

    pub fn i64(self) -> i64 {
        match self {
            Cell::I32(found) => i64::from(found),
            Cell::I64(found) => found,
            Cell::F32(found) => found as i64,
            Cell::F64(found) => found as i64,
            Cell::Ref(found) => i64::from(found),
        }
    }

    pub fn f32(self) -> f32 {
        match self {
            Cell::F32(found) => found,
            Cell::F64(found) => found as f32,
            other => other.i32() as f32,
        }
    }

    pub fn f64(self) -> f64 {
        match self {
            Cell::F64(found) => found,
            Cell::F32(found) => f64::from(found),
            other => other.i64() as f64,
        }
    }

    fn zero(ty: Val) -> Cell {
        match ty {
            Val::I64 => Cell::I64(0),
            Val::F32 => Cell::F32(0.0),
            Val::F64 => Cell::F64(0.0),
            _ => Cell::I32(0),
        }
    }
}

pub trait Host {
    fn call(&mut self, module: &str, name: &str, arguments: &[Cell], memory: &mut Vec<u8>) -> Option<Cell>;
}

pub struct Machine<'m> {
    pub module: &'m Module,
    pub memory: Vec<u8>,
    pub globals: Vec<Cell>,
    pub tables: Vec<Vec<u32>>,
    pub fuel: usize,
    pub trap: Option<String>,
}

struct Frame {
    locals: Vec<Cell>,
    stack: Vec<Cell>,
}

enum Flow {
    Done,
    Branch(u32),
    Return,
}

impl<'m> Machine<'m> {
    pub fn new(module: &'m Module) -> Machine<'m> {
        let pages = module.memories.first().map_or(0, |found| found.0) as usize;
        let mut machine = Machine {
            module,
            memory: vec![0u8; pages * 65536],
            globals: Vec::new(),
            tables: module.tables.iter().map(|found| vec![0u32; found.1 as usize]).collect(),
            fuel: 40_000_000,
            trap: None,
        };
        for global in &module.globals {
            machine.globals.push(constant(&global.init, global.ty));
        }
        if machine.tables.is_empty() {
            machine.tables.push(Vec::new());
        }
        for element in &module.elements {
            let slot = &mut machine.tables[0];
            let mut at = 0usize;
            for target in &element.funcs {
                if at >= slot.len() {
                    slot.push(*target);
                } else {
                    slot[at] = *target;
                }
                at += 1;
            }
        }
        for piece in &module.data {
            let at = piece
                .offset
                .as_ref()
                .map(|body| constant(body, Val::I32).i32() as usize)
                .unwrap_or(0);
            if at + piece.bytes.len() > machine.memory.len() {
                machine.memory.resize(at + piece.bytes.len(), 0);
            }
            machine.memory[at..at + piece.bytes.len()].copy_from_slice(&piece.bytes);
        }
        machine
    }

    pub fn export(&self, name: &str) -> Option<u32> {
        self.module
            .exports
            .iter()
            .find(|found| found.name == name && matches!(found.kind, Kind::Func(_)))
            .map(|found| found.index)
    }

    pub fn run(&mut self, index: u32, arguments: &[Cell], host: &mut dyn Host) -> Option<Vec<Cell>> {
        let imported = self.module.imported_funcs() as u32;
        if index < imported {
            let entry = self
                .module
                .imports
                .iter()
                .filter(|found| matches!(found.kind, Kind::Func(_)))
                .nth(index as usize)?;
            let name = entry.name.clone();
            let owner = entry.module.clone();
            let found = host.call(&owner, &name, arguments, &mut self.memory);
            return Some(found.into_iter().collect());
        }
        let body = self.module.bodies.get((index - imported) as usize)?;
        let shape = self.module.func_type(index)?;
        let mut locals: Vec<Cell> = arguments.to_vec();
        while locals.len() < shape.params.len() {
            locals.push(Cell::I32(0));
        }
        for extra in &body.locals {
            locals.push(Cell::zero(*extra));
        }
        let results = shape.results.len();
        let mut frame = Frame { locals, stack: Vec::new() };
        let mut reader = Reader::new(&body.code);
        if self.block(&mut reader, &mut frame, host).is_none() {
            if self.trap.is_none() {
                self.trap = Some(format!("stopped in func {index} at {}", reader.at));
            }
            return None;
        }
        let mut out = Vec::new();
        let base = frame.stack.len().saturating_sub(results);
        out.extend_from_slice(&frame.stack[base..]);
        Some(out)
    }

    fn block(&mut self, reader: &mut Reader, frame: &mut Frame, host: &mut dyn Host) -> Option<Flow> {
        loop {
            if self.fuel == 0 {
                self.trap = Some("out of fuel".to_string());
                return None;
            }
            self.fuel -= 1;
            let op = reader.byte()?;
            match op {
                0x00 => {
                    self.trap = Some("unreachable".to_string());
                    return None;
                }
                0x01 => {}
                0x0b => return Some(Flow::Done),
                0x02 | 0x03 => {
                    let looping = op == 0x03;
                    skip_type(reader)?;
                    let start = reader.at;
                    let depth = frame.stack.len();
                    loop {
                        match self.block(reader, frame, host)? {
                            Flow::Done => break,
                            Flow::Return => return Some(Flow::Return),
                            Flow::Branch(0) => {
                                if looping {
                                    frame.stack.truncate(depth);
                                    reader.at = start;
                                    continue;
                                }
                                reader.at = skip_body(reader.bytes, reader.at)?;
                                break;
                            }
                            Flow::Branch(level) => {
                                reader.at = skip_body(reader.bytes, reader.at)?;
                                return Some(Flow::Branch(level - 1));
                            }
                        }
                    }
                }
                0x04 => {
                    skip_type(reader)?;
                    let taken = frame.stack.pop()?.i32() != 0;
                    let mut enter = true;
                    if !taken {
                        enter = else_at(reader)?;
                    }
                    if enter {
                        match self.block(reader, frame, host)? {
                            Flow::Done => {}
                            Flow::Return => return Some(Flow::Return),
                            Flow::Branch(0) => {
                                reader.at = skip_body(reader.bytes, reader.at)?;
                            }
                            Flow::Branch(level) => {
                                reader.at = skip_body(reader.bytes, reader.at)?;
                                return Some(Flow::Branch(level - 1));
                            }
                        }
                    }
                }
                0x05 => {
                    reader.at = skip_body(reader.bytes, reader.at)?;
                    return Some(Flow::Done);
                }
                0x0c => {
                    let level = reader.u32()?;
                    return Some(Flow::Branch(level));
                }
                0x0d => {
                    let level = reader.u32()?;
                    if frame.stack.pop()?.i32() != 0 {
                        return Some(Flow::Branch(level));
                    }
                }
                0x0e => {
                    let count = reader.u32()? as usize;
                    let mut targets = Vec::with_capacity(count);
                    for _ in 0..count {
                        targets.push(reader.u32()?);
                    }
                    let fallback = reader.u32()?;
                    let pick = frame.stack.pop()?.i32();
                    let level = if pick < 0 || pick as usize >= count {
                        fallback
                    } else {
                        targets[pick as usize]
                    };
                    return Some(Flow::Branch(level));
                }
                0x0f => return Some(Flow::Return),
                0x10 => {
                    let target = reader.u32()?;
                    self.invoke(target, frame, host)?;
                }
                0x11 => {
                    let _shape = reader.u32()?;
                    let table = reader.u32()? as usize;
                    let slot = frame.stack.pop()?.i32() as usize;
                    let target = *self.tables.get(table)?.get(slot)?;
                    self.invoke(target, frame, host)?;
                }
                0x1a => {
                    frame.stack.pop()?;
                }
                0x1b => {
                    let pick = frame.stack.pop()?.i32();
                    let other = frame.stack.pop()?;
                    let first = frame.stack.pop()?;
                    frame.stack.push(if pick != 0 { first } else { other });
                }
                0x1c => {
                    let count = reader.u32()? as usize;
                    for _ in 0..count {
                        reader.val()?;
                    }
                    let pick = frame.stack.pop()?.i32();
                    let other = frame.stack.pop()?;
                    let first = frame.stack.pop()?;
                    frame.stack.push(if pick != 0 { first } else { other });
                }
                0x20 => {
                    let slot = reader.u32()? as usize;
                    frame.stack.push(*frame.locals.get(slot)?);
                }
                0x21 => {
                    let slot = reader.u32()? as usize;
                    let value = frame.stack.pop()?;
                    if slot < frame.locals.len() {
                        frame.locals[slot] = value;
                    }
                }
                0x22 => {
                    let slot = reader.u32()? as usize;
                    let value = *frame.stack.last()?;
                    if slot < frame.locals.len() {
                        frame.locals[slot] = value;
                    }
                }
                0x23 => {
                    let slot = reader.u32()? as usize;
                    frame.stack.push(*self.globals.get(slot)?);
                }
                0x24 => {
                    let slot = reader.u32()? as usize;
                    let value = frame.stack.pop()?;
                    if slot < self.globals.len() {
                        self.globals[slot] = value;
                    }
                }
                0x25 => {
                    let table = reader.u32()? as usize;
                    let slot = frame.stack.pop()?.i32() as usize;
                    let found = self.tables.get(table).and_then(|list| list.get(slot)).copied();
                    frame.stack.push(Cell::Ref(found.unwrap_or(0)));
                }
                0x26 => {
                    let table = reader.u32()? as usize;
                    let value = frame.stack.pop()?;
                    let slot = frame.stack.pop()?.i32() as usize;
                    let list = self.tables.get_mut(table)?;
                    if slot >= list.len() {
                        list.resize(slot + 1, 0);
                    }
                    list[slot] = value.i32() as u32;
                }
                0x28..=0x3e => self.memory_op(op, reader, frame)?,
                0x3f => {
                    reader.byte()?;
                    frame.stack.push(Cell::I32((self.memory.len() / 65536) as i32));
                }
                0x40 => {
                    reader.byte()?;
                    let pages = frame.stack.pop()?.i32();
                    let had = (self.memory.len() / 65536) as i32;
                    if pages > 0 {
                        self.memory.resize(self.memory.len() + pages as usize * 65536, 0);
                    }
                    frame.stack.push(Cell::I32(had));
                }
                0x41 => {
                    let value = reader.sleb()?;
                    frame.stack.push(Cell::I32(value as i32));
                }
                0x42 => {
                    let value = reader.sleb()?;
                    frame.stack.push(Cell::I64(value));
                }
                0x43 => {
                    let raw = reader.take(4)?;
                    frame.stack.push(Cell::F32(f32::from_le_bytes([raw[0], raw[1], raw[2], raw[3]])));
                }
                0x44 => {
                    let raw = reader.take(8)?;
                    let mut bytes = [0u8; 8];
                    bytes.copy_from_slice(raw);
                    frame.stack.push(Cell::F64(f64::from_le_bytes(bytes)));
                }
                0xd0 => {
                    reader.val()?;
                    frame.stack.push(Cell::Ref(0));
                }
                0xd1 => {
                    let value = frame.stack.pop()?;
                    frame.stack.push(Cell::I32(i32::from(matches!(value, Cell::Ref(0)))));
                }
                0xd2 => {
                    let index = reader.u32()?;
                    frame.stack.push(Cell::Ref(index));
                }
                0xfc => {
                    let extra = reader.u32()?;
                    self.extended(extra, reader, frame)?;
                }
                _ => self.numeric(op, frame)?,
            }
        }
    }

    fn invoke(&mut self, target: u32, frame: &mut Frame, host: &mut dyn Host) -> Option<()> {
        let shape = self.module.func_type(target)?;
        let count = shape.params.len();
        let base = frame.stack.len().checked_sub(count)?;
        let arguments: Vec<Cell> = frame.stack.split_off(base);
        let results = self.run(target, &arguments, host)?;
        frame.stack.extend(results);
        Some(())
    }

    fn memory_op(&mut self, op: u8, reader: &mut Reader, frame: &mut Frame) -> Option<()> {
        let _align = reader.u32()?;
        let offset = reader.u32()? as usize;
        let load = op <= 0x35;
        if load {
            let at = frame.stack.pop()?.i32() as usize + offset;
            let value = match op {
                0x28 => Cell::I32(i32::from_le_bytes(self.four(at)?)),
                0x29 => Cell::I64(i64::from_le_bytes(self.eight(at)?)),
                0x2a => Cell::F32(f32::from_le_bytes(self.four(at)?)),
                0x2b => Cell::F64(f64::from_le_bytes(self.eight(at)?)),
                0x2c => Cell::I32(i32::from(*self.memory.get(at)? as i8)),
                0x2d => Cell::I32(i32::from(*self.memory.get(at)?)),
                0x2e => Cell::I32(i32::from(i16::from_le_bytes(self.two(at)?))),
                0x2f => Cell::I32(i32::from(u16::from_le_bytes(self.two(at)?))),
                0x30 => Cell::I64(i64::from(*self.memory.get(at)? as i8)),
                0x31 => Cell::I64(i64::from(*self.memory.get(at)?)),
                0x32 => Cell::I64(i64::from(i16::from_le_bytes(self.two(at)?))),
                0x33 => Cell::I64(i64::from(u16::from_le_bytes(self.two(at)?))),
                0x34 => Cell::I64(i64::from(i32::from_le_bytes(self.four(at)?))),
                _ => Cell::I64(i64::from(u32::from_le_bytes(self.four(at)?))),
            };
            frame.stack.push(value);
            return Some(());
        }
        let value = frame.stack.pop()?;
        let at = frame.stack.pop()?.i32() as usize + offset;
        match op {
            0x36 => self.write(at, &value.i32().to_le_bytes()),
            0x37 => self.write(at, &value.i64().to_le_bytes()),
            0x38 => self.write(at, &value.f32().to_le_bytes()),
            0x39 => self.write(at, &value.f64().to_le_bytes()),
            0x3a => self.write(at, &[value.i32() as u8]),
            0x3b => self.write(at, &(value.i32() as u16).to_le_bytes()),
            0x3c => self.write(at, &[value.i64() as u8]),
            0x3d => self.write(at, &(value.i64() as u16).to_le_bytes()),
            _ => self.write(at, &(value.i64() as u32).to_le_bytes()),
        }
        Some(())
    }

    fn write(&mut self, at: usize, bytes: &[u8]) {
        if at + bytes.len() > self.memory.len() {
            self.memory.resize(at + bytes.len(), 0);
        }
        self.memory[at..at + bytes.len()].copy_from_slice(bytes);
    }

    fn two(&self, at: usize) -> Option<[u8; 2]> {
        Some([*self.memory.get(at)?, *self.memory.get(at + 1)?])
    }

    fn four(&self, at: usize) -> Option<[u8; 4]> {
        let mut out = [0u8; 4];
        out.copy_from_slice(self.memory.get(at..at + 4)?);
        Some(out)
    }

    fn eight(&self, at: usize) -> Option<[u8; 8]> {
        let mut out = [0u8; 8];
        out.copy_from_slice(self.memory.get(at..at + 8)?);
        Some(out)
    }

    fn extended(&mut self, code: u32, reader: &mut Reader, frame: &mut Frame) -> Option<()> {
        match code {
            8 => {
                let _segment = reader.u32()?;
                reader.byte()?;
                let size = frame.stack.pop()?.i32() as usize;
                let _from = frame.stack.pop()?.i32() as usize;
                let to = frame.stack.pop()?.i32() as usize;
                if to + size > self.memory.len() {
                    self.memory.resize(to + size, 0);
                }
            }
            9 => {
                reader.u32()?;
            }
            10 => {
                reader.byte()?;
                reader.byte()?;
                let size = frame.stack.pop()?.i32() as usize;
                let from = frame.stack.pop()?.i32() as usize;
                let to = frame.stack.pop()?.i32() as usize;
                let slice: Vec<u8> = self.memory.get(from..from + size)?.to_vec();
                self.write(to, &slice);
            }
            11 => {
                reader.byte()?;
                let size = frame.stack.pop()?.i32() as usize;
                let byte = frame.stack.pop()?.i32() as u8;
                let to = frame.stack.pop()?.i32() as usize;
                let block = vec![byte; size];
                self.write(to, &block);
            }
            12 | 13 | 14 => {
                reader.u32()?;
                reader.u32()?;
            }
            15 => {
                let table = reader.u32()? as usize;
                let count = frame.stack.pop()?.i32() as usize;
                let value = frame.stack.pop()?.i32() as u32;
                let list = self.tables.get_mut(table)?;
                let had = list.len();
                list.resize(had + count, value);
                frame.stack.push(Cell::I32(had as i32));
            }
            16 => {
                let table = reader.u32()? as usize;
                let size = self.tables.get(table).map_or(0, |list| list.len());
                frame.stack.push(Cell::I32(size as i32));
            }
            17 => {
                let table = reader.u32()? as usize;
                let count = frame.stack.pop()?.i32() as usize;
                let value = frame.stack.pop()?.i32() as u32;
                let at = frame.stack.pop()?.i32() as usize;
                let list = self.tables.get_mut(table)?;
                if at + count > list.len() {
                    list.resize(at + count, 0);
                }
                for slot in at..at + count {
                    list[slot] = value;
                }
            }
            _ => return None,
        }
        Some(())
    }


    fn numeric(&mut self, op: u8, frame: &mut Frame) -> Option<()> {
        let value = match op {
            0x45 => Cell::I32(i32::from(frame.stack.pop()?.i32() == 0)),
            0x50 => Cell::I32(i32::from(frame.stack.pop()?.i64() == 0)),
            0x67 => Cell::I32(frame.stack.pop()?.i32().leading_zeros() as i32),
            0x68 => Cell::I32(frame.stack.pop()?.i32().trailing_zeros() as i32),
            0x69 => Cell::I32(frame.stack.pop()?.i32().count_ones() as i32),
            0x79 => Cell::I64(i64::from(frame.stack.pop()?.i64().leading_zeros())),
            0x7a => Cell::I64(i64::from(frame.stack.pop()?.i64().trailing_zeros())),
            0x7b => Cell::I64(i64::from(frame.stack.pop()?.i64().count_ones())),
            0xa7 => Cell::I32(frame.stack.pop()?.i64() as i32),
            0xa8 | 0xa9 => Cell::I32(frame.stack.pop()?.f32() as i32),
            0xaa | 0xab => Cell::I32(frame.stack.pop()?.f64() as i32),
            0xac => Cell::I64(i64::from(frame.stack.pop()?.i32())),
            0xad => Cell::I64(i64::from(frame.stack.pop()?.i32() as u32)),
            0xae | 0xaf => Cell::I64(frame.stack.pop()?.f32() as i64),
            0xb0 | 0xb1 => Cell::I64(frame.stack.pop()?.f64() as i64),
            0xb2 => Cell::F32(frame.stack.pop()?.i32() as f32),
            0xb3 => Cell::F32(frame.stack.pop()?.i32() as u32 as f32),
            0xb4 => Cell::F32(frame.stack.pop()?.i64() as f32),
            0xb5 => Cell::F32(frame.stack.pop()?.i64() as u64 as f32),
            0xb6 => Cell::F32(frame.stack.pop()?.f64() as f32),
            0xb7 => Cell::F64(f64::from(frame.stack.pop()?.i32())),
            0xb8 => Cell::F64(f64::from(frame.stack.pop()?.i32() as u32)),
            0xb9 => Cell::F64(frame.stack.pop()?.i64() as f64),
            0xba => Cell::F64(frame.stack.pop()?.i64() as u64 as f64),
            0xbb => Cell::F64(f64::from(frame.stack.pop()?.f32())),
            0xbc => Cell::I32(frame.stack.pop()?.f32().to_bits() as i32),
            0xbd => Cell::I64(frame.stack.pop()?.f64().to_bits() as i64),
            0xbe => Cell::F32(f32::from_bits(frame.stack.pop()?.i32() as u32)),
            0xbf => Cell::F64(f64::from_bits(frame.stack.pop()?.i64() as u64)),
            0xc0 => Cell::I32(i32::from(frame.stack.pop()?.i32() as i8)),
            0xc1 => Cell::I32(i32::from(frame.stack.pop()?.i32() as i16)),
            0xc2 => Cell::I64(i64::from(frame.stack.pop()?.i64() as i8)),
            0xc3 => Cell::I64(i64::from(frame.stack.pop()?.i64() as i16)),
            0xc4 => Cell::I64(i64::from(frame.stack.pop()?.i64() as i32)),
            0x8b => Cell::F32(frame.stack.pop()?.f32().abs()),
            0x8c => Cell::F32(-frame.stack.pop()?.f32()),
            0x8d => Cell::F32(frame.stack.pop()?.f32().ceil()),
            0x8e => Cell::F32(frame.stack.pop()?.f32().floor()),
            0x8f => Cell::F32(frame.stack.pop()?.f32().trunc()),
            0x90 => Cell::F32(round(f64::from(frame.stack.pop()?.f32())) as f32),
            0x91 => Cell::F32(frame.stack.pop()?.f32().sqrt()),
            0x99 => Cell::F64(frame.stack.pop()?.f64().abs()),
            0x9a => Cell::F64(-frame.stack.pop()?.f64()),
            0x9b => Cell::F64(frame.stack.pop()?.f64().ceil()),
            0x9c => Cell::F64(frame.stack.pop()?.f64().floor()),
            0x9d => Cell::F64(frame.stack.pop()?.f64().trunc()),
            0x9e => Cell::F64(round(frame.stack.pop()?.f64())),
            0x9f => Cell::F64(frame.stack.pop()?.f64().sqrt()),
            _ => return self.pair(op, frame),
        };
        frame.stack.push(value);
        Some(())
    }

    fn pair(&mut self, op: u8, frame: &mut Frame) -> Option<()> {
        let right = frame.stack.pop()?;
        let left = frame.stack.pop()?;
        let value = match op {
            0x46 => Cell::I32(i32::from(left.i32() == right.i32())),
            0x47 => Cell::I32(i32::from(left.i32() != right.i32())),
            0x48 => Cell::I32(i32::from(left.i32() < right.i32())),
            0x49 => Cell::I32(i32::from((left.i32() as u32) < right.i32() as u32)),
            0x4a => Cell::I32(i32::from(left.i32() > right.i32())),
            0x4b => Cell::I32(i32::from(left.i32() as u32 > right.i32() as u32)),
            0x4c => Cell::I32(i32::from(left.i32() <= right.i32())),
            0x4d => Cell::I32(i32::from(left.i32() as u32 <= right.i32() as u32)),
            0x4e => Cell::I32(i32::from(left.i32() >= right.i32())),
            0x4f => Cell::I32(i32::from(left.i32() as u32 >= right.i32() as u32)),
            0x51 => Cell::I32(i32::from(left.i64() == right.i64())),
            0x52 => Cell::I32(i32::from(left.i64() != right.i64())),
            0x53 => Cell::I32(i32::from(left.i64() < right.i64())),
            0x54 => Cell::I32(i32::from((left.i64() as u64) < right.i64() as u64)),
            0x55 => Cell::I32(i32::from(left.i64() > right.i64())),
            0x56 => Cell::I32(i32::from(left.i64() as u64 > right.i64() as u64)),
            0x57 => Cell::I32(i32::from(left.i64() <= right.i64())),
            0x58 => Cell::I32(i32::from(left.i64() as u64 <= right.i64() as u64)),
            0x59 => Cell::I32(i32::from(left.i64() >= right.i64())),
            0x5a => Cell::I32(i32::from(left.i64() as u64 >= right.i64() as u64)),
            0x5b => Cell::I32(i32::from(left.f32() == right.f32())),
            0x5c => Cell::I32(i32::from(left.f32() != right.f32())),
            0x5d => Cell::I32(i32::from(left.f32() < right.f32())),
            0x5e => Cell::I32(i32::from(left.f32() > right.f32())),
            0x5f => Cell::I32(i32::from(left.f32() <= right.f32())),
            0x60 => Cell::I32(i32::from(left.f32() >= right.f32())),
            0x61 => Cell::I32(i32::from(left.f64() == right.f64())),
            0x62 => Cell::I32(i32::from(left.f64() != right.f64())),
            0x63 => Cell::I32(i32::from(left.f64() < right.f64())),
            0x64 => Cell::I32(i32::from(left.f64() > right.f64())),
            0x65 => Cell::I32(i32::from(left.f64() <= right.f64())),
            0x66 => Cell::I32(i32::from(left.f64() >= right.f64())),
            0x6a => Cell::I32(left.i32().wrapping_add(right.i32())),
            0x6b => Cell::I32(left.i32().wrapping_sub(right.i32())),
            0x6c => Cell::I32(left.i32().wrapping_mul(right.i32())),
            0x6d => Cell::I32(guard(left.i32(), right.i32(), self)?),
            0x6e => {
                if right.i32() == 0 {
                    self.trap = Some("divide by zero".to_string());
                    return None;
                }
                Cell::I32(((left.i32() as u32) / (right.i32() as u32)) as i32)
            }
            0x6f => {
                if right.i32() == 0 {
                    self.trap = Some("divide by zero".to_string());
                    return None;
                }
                Cell::I32(left.i32().wrapping_rem(right.i32()))
            }
            0x70 => {
                if right.i32() == 0 {
                    self.trap = Some("divide by zero".to_string());
                    return None;
                }
                Cell::I32(((left.i32() as u32) % (right.i32() as u32)) as i32)
            }
            0x71 => Cell::I32(left.i32() & right.i32()),
            0x72 => Cell::I32(left.i32() | right.i32()),
            0x73 => Cell::I32(left.i32() ^ right.i32()),
            0x74 => Cell::I32(left.i32().wrapping_shl(right.i32() as u32)),
            0x75 => Cell::I32(left.i32().wrapping_shr(right.i32() as u32)),
            0x76 => Cell::I32(((left.i32() as u32).wrapping_shr(right.i32() as u32)) as i32),
            0x77 => Cell::I32((left.i32() as u32).rotate_left(right.i32() as u32 & 31) as i32),
            0x78 => Cell::I32((left.i32() as u32).rotate_right(right.i32() as u32 & 31) as i32),
            0x7c => Cell::I64(left.i64().wrapping_add(right.i64())),
            0x7d => Cell::I64(left.i64().wrapping_sub(right.i64())),
            0x7e => Cell::I64(left.i64().wrapping_mul(right.i64())),
            0x7f => {
                if right.i64() == 0 {
                    self.trap = Some("divide by zero".to_string());
                    return None;
                }
                Cell::I64(left.i64().wrapping_div(right.i64()))
            }
            0x80 => {
                if right.i64() == 0 {
                    self.trap = Some("divide by zero".to_string());
                    return None;
                }
                Cell::I64(((left.i64() as u64) / (right.i64() as u64)) as i64)
            }
            0x81 => {
                if right.i64() == 0 {
                    self.trap = Some("divide by zero".to_string());
                    return None;
                }
                Cell::I64(left.i64().wrapping_rem(right.i64()))
            }
            0x82 => {
                if right.i64() == 0 {
                    self.trap = Some("divide by zero".to_string());
                    return None;
                }
                Cell::I64(((left.i64() as u64) % (right.i64() as u64)) as i64)
            }
            0x83 => Cell::I64(left.i64() & right.i64()),
            0x84 => Cell::I64(left.i64() | right.i64()),
            0x85 => Cell::I64(left.i64() ^ right.i64()),
            0x86 => Cell::I64(left.i64().wrapping_shl(right.i64() as u32)),
            0x87 => Cell::I64(left.i64().wrapping_shr(right.i64() as u32)),
            0x88 => Cell::I64(((left.i64() as u64).wrapping_shr(right.i64() as u32)) as i64),
            0x89 => Cell::I64((left.i64() as u64).rotate_left(right.i64() as u32 & 63) as i64),
            0x8a => Cell::I64((left.i64() as u64).rotate_right(right.i64() as u32 & 63) as i64),
            0x92 => Cell::F32(left.f32() + right.f32()),
            0x93 => Cell::F32(left.f32() - right.f32()),
            0x94 => Cell::F32(left.f32() * right.f32()),
            0x95 => Cell::F32(left.f32() / right.f32()),
            0x96 => Cell::F32(left.f32().min(right.f32())),
            0x97 => Cell::F32(left.f32().max(right.f32())),
            0x98 => Cell::F32(left.f32().copysign(right.f32())),
            0xa0 => Cell::F64(left.f64() + right.f64()),
            0xa1 => Cell::F64(left.f64() - right.f64()),
            0xa2 => Cell::F64(left.f64() * right.f64()),
            0xa3 => Cell::F64(left.f64() / right.f64()),
            0xa4 => Cell::F64(left.f64().min(right.f64())),
            0xa5 => Cell::F64(left.f64().max(right.f64())),
            0xa6 => Cell::F64(left.f64().copysign(right.f64())),
            _ => {
                self.trap = Some(format!("opcode {op:#04x}"));
                return None;
            }
        };
        frame.stack.push(value);
        Some(())
    }
}


fn guard(left: i32, right: i32, machine: &mut Machine) -> Option<i32> {
    if right == 0 {
        machine.trap = Some("divide by zero".to_string());
        return None;
    }
    Some(left.wrapping_div(right))
}

fn round(value: f64) -> f64 {
    let near = value.round();
    if (value - value.trunc()).abs() == 0.5 && near % 2.0 != 0.0 { near - value.signum() } else { near }
}

fn constant(body: &[u8], ty: Val) -> Cell {
    let mut reader = Reader::new(body);
    match reader.byte() {
        Some(0x41) => Cell::I32(reader.sleb().unwrap_or(0) as i32),
        Some(0x42) => Cell::I64(reader.sleb().unwrap_or(0)),
        Some(0x43) => {
            let raw = reader.take(4).unwrap_or(&[0, 0, 0, 0]);
            Cell::F32(f32::from_le_bytes([raw[0], raw[1], raw[2], raw[3]]))
        }
        Some(0x44) => {
            let raw = reader.take(8).unwrap_or(&[0; 8]);
            let mut bytes = [0u8; 8];
            bytes.copy_from_slice(raw);
            Cell::F64(f64::from_le_bytes(bytes))
        }
        _ => Cell::zero(ty),
    }
}

fn skip_type(reader: &mut Reader) -> Option<()> {
    let peek = *reader.bytes.get(reader.at)?;
    if peek == 0x40 || (0x7b..=0x7f).contains(&peek) || peek == 0x6f || peek == 0x70 {
        reader.at += 1;
        return Some(());
    }
    reader.sleb()?;
    Some(())
}




fn else_at(reader: &mut Reader) -> Option<bool> {
    let bytes = reader.bytes;
    let mut scan = Reader::new(bytes);
    scan.at = reader.at;
    let mut depth = 0i32;
    loop {
        let at = scan.at;
        let op = scan.byte()?;
        match op {
            0x02 | 0x03 | 0x04 => {
                skip_type(&mut scan)?;
                depth += 1;
            }
            0x05 if depth == 0 => {
                reader.at = at + 1;
                return Some(true);
            }
            0x0b => {
                if depth == 0 {
                    reader.at = at + 1;
                    return Some(false);
                }
                depth -= 1;
            }
            _ => {
                crate::code::skip_at(&mut scan, op)?;
            }
        }
    }
}

fn skip_body(bytes: &[u8], from: usize) -> Option<usize> {
    let mut scan = Reader::new(bytes);
    scan.at = from;
    let mut depth = 0i32;
    loop {
        let at = scan.at;
        let op = scan.byte()?;
        match op {
            0x02 | 0x03 | 0x04 => {
                skip_type(&mut scan)?;
                depth += 1;
            }
            0x0b => {
                if depth == 0 {
                    return Some(at + 1);
                }
                depth -= 1;
            }
            _ => {
                crate::code::skip_at(&mut scan, op)?;
            }
        }
    }
}

