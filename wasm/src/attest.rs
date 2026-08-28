use crate::exec::{Cell, Host, Machine};
use crate::parse::Module;

pub struct Env {
    pub user_env: String,
    pub touch: u32,
    pub cores: u32,
    pub outer_height: u32,
}

pub fn cyrb53(body: &str, seed: u32) -> u64 {
    let mut one: u32 = 0xdeadbeef ^ seed;
    let mut two: u32 = 0x41c6ce57 ^ seed;
    for letter in body.encode_utf16() {
        let code = u32::from(letter);
        one = (one ^ code).wrapping_mul(2654435761);
        two = (two ^ code).wrapping_mul(1597334677);
    }
    one = (one ^ (one >> 16)).wrapping_mul(2246822507);
    one ^= (two ^ (two >> 13)).wrapping_mul(3266489909);
    two = (two ^ (two >> 16)).wrapping_mul(2246822507);
    two ^= (one ^ (one >> 13)).wrapping_mul(3266489909);
    4294967296u64 * u64::from(two & 2097151) + u64::from(one)
}

enum Obj {
    Null,
    Memory,
    Buffer,
    Slab { at: usize, len: usize },
    View { at: usize, len: usize },
    Input(Vec<u32>),
}

struct Glue<'e> {
    env: &'e Env,
    heap: Vec<Obj>,
    trap: Option<String>,
}

impl<'e> Glue<'e> {
    fn keep(&mut self, item: Obj) -> Cell {
        self.heap.push(item);
        Cell::Ref((self.heap.len() - 1) as u32)
    }

    fn at(&self, cell: Cell) -> &Obj {
        let index = match cell {
            Cell::Ref(found) => found as usize,
            other => other.i32() as usize,
        };
        self.heap.get(index).unwrap_or(&Obj::Null)
    }
}

fn field(env: &Env, code: u32) -> Option<u32> {
    match code {
        56694 | 3536 | 56608 | 56690 | 56676 if code == 56676 => Some(env.outer_height),
        56694 | 3536 | 56608 | 56690 => Some(env.touch),
        56616 | 56704 | 56594 => Some(env.outer_height),
        56640 | 56712 | 56644 => Some(env.cores),
        _ => None,
    }
}

impl<'e> Host for Glue<'e> {
    fn call(
        &mut self,
        _module: &str,
        name: &str,
        arguments: &[Cell],
        memory: &mut Vec<u8>,
    ) -> Option<Cell> {
        let head = name.split('_').nth(3).unwrap_or(name);
        let arg = |index: usize| arguments.get(index).copied().unwrap_or(Cell::I32(0));
        match head {
            "memory" => Some(self.keep(Obj::Memory)),
            "buffer" => Some(self.keep(Obj::Buffer)),
            "length" => {
                let value = match self.at(arg(0)) {
                    Obj::Input(items) => items.len(),
                    Obj::Slab { len, .. } => *len,
                    Obj::View { len, .. } => *len / 4,
                    _ => memory.len() / 4,
                };
                Some(Cell::I32(value as i32))
            }
            "byteLength" => {
                let value = match self.at(arg(0)) {
                    Obj::Input(items) => items.len() * 4,
                    Obj::Slab { len, .. } => *len * 4,
                    Obj::View { len, .. } => *len,
                    _ => memory.len(),
                };
                Some(Cell::I32(value as i32))
            }
            "grow" => {
                let pages = arg(1).i32().max(0) as usize;
                let had = memory.len() / 65536;
                memory.resize(memory.len() + pages * 65536, 0);
                Some(Cell::I32(had as i32))
            }
            "getUint32" => {
                let base = match self.at(arg(0)) {
                    Obj::View { at, .. } => *at,
                    _ => 0,
                };
                let spot = base + arg(1).i32().max(0) as usize;
                let mut raw = [0u8; 4];
                raw.copy_from_slice(memory.get(spot..spot + 4)?);
                Some(Cell::I32(u32::from_be_bytes(raw) as i32))
            }
            "set" => {
                let target = match self.at(arg(0)) {
                    Obj::Slab { at, .. } => *at,
                    Obj::View { at, .. } => *at,
                    _ => 0,
                };
                let source: Vec<u32> = match self.at(arg(1)) {
                    Obj::Input(items) => items.clone(),
                    _ => Vec::new(),
                };
                let start = target + arg(2).i32().max(0) as usize * 4;
                if start + source.len() * 4 > memory.len() {
                    memory.resize(start + source.len() * 4, 0);
                }
                for (step, value) in source.iter().enumerate() {
                    let spot = start + step * 4;
                    memory[spot..spot + 4].copy_from_slice(&value.to_le_bytes());
                }
                None
            }
            "throw" => {
                self.trap = Some("wasm threw".to_string());
                None
            }
            "init" => None,
            _ => {
                if name.contains("__wbg_new_e3b321dcfef89fc7") {
                    let len = memory.len() / 4;
                    return Some(self.keep(Obj::Slab { at: 0, len }));
                }
                if name.contains("__wbg_new_7e079fa25e135eb1") {
                    let at = arg(1).i32().max(0) as usize;
                    let len = arg(2).i32().max(0) as usize;
                    return Some(self.keep(Obj::View { at, len }));
                }
                let base = arg(1).i32().max(0) as usize;
                let code = arg(2).i32().max(0) as u32;
                if let Some(seed) = field(self.env, code) {
                    let value = cyrb53(&self.env.user_env, seed) as u32;
                    let spot = base + code as usize;
                    if spot + 4 <= memory.len() {
                        memory[spot..spot + 4].copy_from_slice(&value.to_be_bytes());
                    }
                }
                None
            }
        }
    }
}

pub fn run(
    module: &Module,
    env: &Env,
    seeds: &[u32],
    taking: &str,
    plain: &str,
) -> (Option<u32>, Option<i64>, Option<String>) {
    let mut machine = Machine::new(module);
    let mut glue = Glue {
        env,
        heap: vec![Obj::Null, Obj::Null, Obj::Null, Obj::Null],
        trap: None,
    };
    if let Some(start) = machine
        .module
        .exports
        .iter()
        .find(|found| found.name == "__wbindgen_start")
        .map(|found| found.index)
    {
        machine.run(start, &[], &mut glue);
    }
    let input = {
        glue.heap.push(Obj::Input(seeds.to_vec()));
        (glue.heap.len() - 1) as u32
    };
    let first = machine
        .export(taking)
        .and_then(|index| machine.run(index, &[Cell::Ref(input)], &mut glue))
        .and_then(|out| out.first().map(|cell| cell.i32() as u32));
    let second = machine
        .export(plain)
        .and_then(|index| machine.run(index, &[], &mut glue))
        .and_then(|out| out.first().map(|cell| cell.i64()));
    (first, second, machine.trap.clone().or(glue.trap))
}
