use crate::parse::Reader;

#[derive(Clone, Debug)]
pub enum Arg {
    None,
    Index(u32),
    Two(u32),
    Mem(u32, u32),
    I32(i32),
    I64(i64),
    F32(f32),
    F64(f64),
    Block(Block),
    Table(Vec<u32>, u32),
    Types,
}

#[derive(Clone, Copy, Debug)]
pub enum Block {
    Empty,
    Value,
    Type(u32),
}

#[derive(Clone, Debug)]
pub struct Op {
    pub name: &'static str,
    pub arg: Arg,
}

pub fn decode(r: &mut Reader) -> Option<Op> {
    let first = r.byte()?;
    if first == 0xfc || first == 0xfd || first == 0xfe {
        let sub = r.u32()?;
        let (name, arg) = extended(r, first, sub)?;
        return Some(Op { name, arg });
    }
    let (name, arg) = simple(r, first)?;
    Some(Op { name, arg })
}

pub fn skip(r: &mut Reader) -> Option<()> {
    decode(r).map(|_| ())
}

pub fn skip_at(r: &mut Reader, op: u8) -> Option<()> {
    if op == 0xfc || op == 0xfd || op == 0xfe {
        let sub = r.u32()?;
        extended(r, op, sub)?;
        return Some(());
    }
    simple(r, op)?;
    Some(())
}

fn block(r: &mut Reader) -> Option<Block> {
    let tag = *r.bytes.get(r.at)?;
    Some(match tag {
        0x40 => {
            r.at += 1;
            Block::Empty
        }
        0x7f | 0x7e | 0x7d | 0x7c | 0x7b | 0x70 | 0x6f => {
            r.val()?;
            Block::Value
        }
        _ => Block::Type(r.sleb()? as u32),
    })
}

fn mem(r: &mut Reader) -> Option<Arg> {
    let align = r.u32()?;
    let offset = r.u32()?;
    Some(Arg::Mem(align, offset))
}

fn simple(r: &mut Reader, op: u8) -> Option<(&'static str, Arg)> {
    Some(match op {
        0x00 => ("unreachable", Arg::None),
        0x01 => ("nop", Arg::None),
        0x02 => ("block", Arg::Block(block(r)?)),
        0x03 => ("loop", Arg::Block(block(r)?)),
        0x04 => ("if", Arg::Block(block(r)?)),
        0x05 => ("else", Arg::None),
        0x0b => ("end", Arg::None),
        0x0c => ("br", Arg::Index(r.u32()?)),
        0x0d => ("br_if", Arg::Index(r.u32()?)),
        0x0e => {
            let mut targets = Vec::new();
            for _ in 0..r.u32()? {
                targets.push(r.u32()?);
            }
            ("br_table", Arg::Table(targets, r.u32()?))
        }
        0x0f => ("return", Arg::None),
        0x10 => ("call", Arg::Index(r.u32()?)),
        0x11 => {
            let t = r.u32()?;
            r.u32()?;
            ("call_indirect", Arg::Two(t))
        }
        0x1a => ("drop", Arg::None),
        0x1b => ("select", Arg::None),
        0x1c => {
            for _ in 0..r.u32()? {
                r.val()?;
            }
            ("select_t", Arg::Types)
        }
        0x20 => ("local.get", Arg::Index(r.u32()?)),
        0x21 => ("local.set", Arg::Index(r.u32()?)),
        0x22 => ("local.tee", Arg::Index(r.u32()?)),
        0x23 => ("global.get", Arg::Index(r.u32()?)),
        0x24 => ("global.set", Arg::Index(r.u32()?)),
        0x25 => ("table.get", Arg::Index(r.u32()?)),
        0x26 => ("table.set", Arg::Index(r.u32()?)),
        0x28 => ("i32.load", mem(r)?),
        0x29 => ("i64.load", mem(r)?),
        0x2a => ("f32.load", mem(r)?),
        0x2b => ("f64.load", mem(r)?),
        0x2c => ("i32.load8_s", mem(r)?),
        0x2d => ("i32.load8_u", mem(r)?),
        0x2e => ("i32.load16_s", mem(r)?),
        0x2f => ("i32.load16_u", mem(r)?),
        0x30 => ("i64.load8_s", mem(r)?),
        0x31 => ("i64.load8_u", mem(r)?),
        0x32 => ("i64.load16_s", mem(r)?),
        0x33 => ("i64.load16_u", mem(r)?),
        0x34 => ("i64.load32_s", mem(r)?),
        0x35 => ("i64.load32_u", mem(r)?),
        0x36 => ("i32.store", mem(r)?),
        0x37 => ("i64.store", mem(r)?),
        0x38 => ("f32.store", mem(r)?),
        0x39 => ("f64.store", mem(r)?),
        0x3a => ("i32.store8", mem(r)?),
        0x3b => ("i32.store16", mem(r)?),
        0x3c => ("i64.store8", mem(r)?),
        0x3d => ("i64.store16", mem(r)?),
        0x3e => ("i64.store32", mem(r)?),
        0x3f => ("memory.size", Arg::Index(r.u32()?)),
        0x40 => ("memory.grow", Arg::Index(r.u32()?)),
        0x41 => ("i32.const", Arg::I32(r.sleb()? as i32)),
        0x42 => ("i64.const", Arg::I64(r.sleb()?)),
        0x43 => ("f32.const", Arg::F32(f32::from_le_bytes(r.take(4)?.try_into().ok()?))),
        0x44 => ("f64.const", Arg::F64(f64::from_le_bytes(r.take(8)?.try_into().ok()?))),
        0xd0 => {
            r.val()?;
            ("ref.null", Arg::Types)
        }
        0xd1 => ("ref.is_null", Arg::None),
        0xd2 => ("ref.func", Arg::Index(r.u32()?)),
        _ => (numeric(op)?, Arg::None),
    })
}

fn numeric(op: u8) -> Option<&'static str> {
    Some(match op {
        0x45 => "i32.eqz",
        0x46 => "i32.eq",
        0x47 => "i32.ne",
        0x48 => "i32.lt_s",
        0x49 => "i32.lt_u",
        0x4a => "i32.gt_s",
        0x4b => "i32.gt_u",
        0x4c => "i32.le_s",
        0x4d => "i32.le_u",
        0x4e => "i32.ge_s",
        0x4f => "i32.ge_u",
        0x50 => "i64.eqz",
        0x51 => "i64.eq",
        0x52 => "i64.ne",
        0x53 => "i64.lt_s",
        0x54 => "i64.lt_u",
        0x55 => "i64.gt_s",
        0x56 => "i64.gt_u",
        0x57 => "i64.le_s",
        0x58 => "i64.le_u",
        0x59 => "i64.ge_s",
        0x5a => "i64.ge_u",
        0x5b => "f32.eq",
        0x5c => "f32.ne",
        0x5d => "f32.lt",
        0x5e => "f32.gt",
        0x5f => "f32.le",
        0x60 => "f32.ge",
        0x61 => "f64.eq",
        0x62 => "f64.ne",
        0x63 => "f64.lt",
        0x64 => "f64.gt",
        0x65 => "f64.le",
        0x66 => "f64.ge",
        0x67 => "i32.clz",
        0x68 => "i32.ctz",
        0x69 => "i32.popcnt",
        0x6a => "i32.add",
        0x6b => "i32.sub",
        0x6c => "i32.mul",
        0x6d => "i32.div_s",
        0x6e => "i32.div_u",
        0x6f => "i32.rem_s",
        0x70 => "i32.rem_u",
        0x71 => "i32.and",
        0x72 => "i32.or",
        0x73 => "i32.xor",
        0x74 => "i32.shl",
        0x75 => "i32.shr_s",
        0x76 => "i32.shr_u",
        0x77 => "i32.rotl",
        0x78 => "i32.rotr",
        0x79 => "i64.clz",
        0x7a => "i64.ctz",
        0x7b => "i64.popcnt",
        0x7c => "i64.add",
        0x7d => "i64.sub",
        0x7e => "i64.mul",
        0x7f => "i64.div_s",
        0x80 => "i64.div_u",
        0x81 => "i64.rem_s",
        0x82 => "i64.rem_u",
        0x83 => "i64.and",
        0x84 => "i64.or",
        0x85 => "i64.xor",
        0x86 => "i64.shl",
        0x87 => "i64.shr_s",
        0x88 => "i64.shr_u",
        0x89 => "i64.rotl",
        0x8a => "i64.rotr",
        0x8b => "f32.abs",
        0x8c => "f32.neg",
        0x8d => "f32.ceil",
        0x8e => "f32.floor",
        0x8f => "f32.trunc",
        0x90 => "f32.nearest",
        0x91 => "f32.sqrt",
        0x92 => "f32.add",
        0x93 => "f32.sub",
        0x94 => "f32.mul",
        0x95 => "f32.div",
        0x96 => "f32.min",
        0x97 => "f32.max",
        0x98 => "f32.copysign",
        0x99 => "f64.abs",
        0x9a => "f64.neg",
        0x9b => "f64.ceil",
        0x9c => "f64.floor",
        0x9d => "f64.trunc",
        0x9e => "f64.nearest",
        0x9f => "f64.sqrt",
        0xa0 => "f64.add",
        0xa1 => "f64.sub",
        0xa2 => "f64.mul",
        0xa3 => "f64.div",
        0xa4 => "f64.min",
        0xa5 => "f64.max",
        0xa6 => "f64.copysign",
        0xa7 => "i32.wrap_i64",
        0xa8 => "i32.trunc_f32_s",
        0xa9 => "i32.trunc_f32_u",
        0xaa => "i32.trunc_f64_s",
        0xab => "i32.trunc_f64_u",
        0xac => "i64.extend_i32_s",
        0xad => "i64.extend_i32_u",
        0xae => "i64.trunc_f32_s",
        0xaf => "i64.trunc_f32_u",
        0xb0 => "i64.trunc_f64_s",
        0xb1 => "i64.trunc_f64_u",
        0xb2 => "f32.convert_i32_s",
        0xb3 => "f32.convert_i32_u",
        0xb4 => "f32.convert_i64_s",
        0xb5 => "f32.convert_i64_u",
        0xb6 => "f32.demote_f64",
        0xb7 => "f64.convert_i32_s",
        0xb8 => "f64.convert_i32_u",
        0xb9 => "f64.convert_i64_s",
        0xba => "f64.convert_i64_u",
        0xbb => "f64.promote_f32",
        0xbc => "i32.reinterpret_f32",
        0xbd => "i64.reinterpret_f64",
        0xbe => "f32.reinterpret_i32",
        0xbf => "f64.reinterpret_i64",
        0xc0 => "i32.extend8_s",
        0xc1 => "i32.extend16_s",
        0xc2 => "i64.extend8_s",
        0xc3 => "i64.extend16_s",
        0xc4 => "i64.extend32_s",
        _ => return None,
    })
}

fn extended(r: &mut Reader, prefix: u8, sub: u32) -> Option<(&'static str, Arg)> {
    if prefix == 0xfc {
        return Some(match sub {
            0 => ("i32.trunc_sat_f32_s", Arg::None),
            1 => ("i32.trunc_sat_f32_u", Arg::None),
            2 => ("i32.trunc_sat_f64_s", Arg::None),
            3 => ("i32.trunc_sat_f64_u", Arg::None),
            4 => ("i64.trunc_sat_f32_s", Arg::None),
            5 => ("i64.trunc_sat_f32_u", Arg::None),
            6 => ("i64.trunc_sat_f64_s", Arg::None),
            7 => ("i64.trunc_sat_f64_u", Arg::None),
            8 => {
                let d = r.u32()?;
                r.u32()?;
                ("memory.init", Arg::Two(d))
            }
            9 => ("data.drop", Arg::Index(r.u32()?)),
            10 => {
                let d = r.u32()?;
                r.u32()?;
                ("memory.copy", Arg::Two(d))
            }
            11 => ("memory.fill", Arg::Index(r.u32()?)),
            12 => {
                let e = r.u32()?;
                r.u32()?;
                ("table.init", Arg::Two(e))
            }
            13 => ("elem.drop", Arg::Index(r.u32()?)),
            14 => {
                let d = r.u32()?;
                r.u32()?;
                ("table.copy", Arg::Two(d))
            }
            15 => ("table.grow", Arg::Index(r.u32()?)),
            16 => ("table.size", Arg::Index(r.u32()?)),
            17 => ("table.fill", Arg::Index(r.u32()?)),
            _ => return None,
        });
    }
    if prefix == 0xfd {
        return Some(match sub {
            0..=11 => ("v128.load", mem(r)?),
            12 => {
                r.take(16)?;
                ("v128.const", Arg::Types)
            }
            13 => {
                let mut lanes = Vec::new();
                for _ in 0..16 {
                    lanes.push(r.byte()? as u32);
                }
                ("i8x16.shuffle", Arg::Table(lanes, 0))
            }
            84..=91 => {
                let a = mem(r)?;
                let Arg::Mem(align, offset) = a else { return None };
                r.byte()?;
                ("v128.load_lane", Arg::Mem(align, offset))
            }
            _ => ("v128.op", Arg::Index(sub)),
        });
    }
    Some(("atomic.op", Arg::Index(sub)))
}
