pub struct Picture {
    pub width: usize,
    pub height: usize,
    pub pixels: Vec<u8>,
}

impl Picture {
    pub fn at(&self, x: usize, y: usize) -> (u8, u8, u8) {
        let spot = (y * self.width + x) * 3;
        (self.pixels[spot], self.pixels[spot + 1], self.pixels[spot + 2])
    }

    pub fn grey(&self, x: usize, y: usize) -> f64 {
        let (r, g, b) = self.at(x, y);
        0.299 * f64::from(r) + 0.587 * f64::from(g) + 0.114 * f64::from(b)
    }
}

struct Huffman {
    lookup: Vec<(u16, u8, u8)>,
}

impl Huffman {
    fn build(counts: &[u8; 16], values: &[u8]) -> Huffman {
        let mut lookup = Vec::new();
        let mut code: u16 = 0;
        let mut at = 0usize;
        for length in 0..16u8 {
            for _ in 0..counts[length as usize] {
                if at < values.len() {
                    lookup.push((code, length + 1, values[at]));
                }
                at += 1;
                code = code.wrapping_add(1);
            }
            code <<= 1;
        }
        Huffman { lookup }
    }
}

struct Bits<'a> {
    body: &'a [u8],
    at: usize,
    slot: u32,
    held: u32,
}

impl<'a> Bits<'a> {
    fn bit(&mut self) -> Option<u32> {
        if self.held == 0 {
            let mut byte = *self.body.get(self.at)?;
            self.at += 1;
            if byte == 0xff {
                let next = *self.body.get(self.at)?;
                if next == 0 {
                    self.at += 1;
                } else if (0xd0..=0xd7).contains(&next) {
                    self.at += 1;
                    byte = *self.body.get(self.at)?;
                    self.at += 1;
                } else {
                    return None;
                }
            }
            self.slot = u32::from(byte);
            self.held = 8;
        }
        self.held -= 1;
        Some((self.slot >> self.held) & 1)
    }

    fn take(&mut self, count: u8) -> Option<i32> {
        let mut found = 0i32;
        for _ in 0..count {
            found = (found << 1) | self.bit()? as i32;
        }
        Some(found)
    }

    fn symbol(&mut self, table: &Huffman) -> Option<u8> {
        let mut code: u16 = 0;
        for length in 1..=16u8 {
            code = (code << 1) | self.bit()? as u16;
            for (found, size, value) in &table.lookup {
                if *size == length && *found == code {
                    return Some(*value);
                }
            }
        }
        None
    }
}

fn extend(value: i32, count: u8) -> i32 {
    if count == 0 {
        return 0;
    }
    if value < (1 << (count - 1)) { value - (1 << count) + 1 } else { value }
}

const ZIGZAG: [usize; 64] = [
    0, 1, 8, 16, 9, 2, 3, 10, 17, 24, 32, 25, 18, 11, 4, 5, 12, 19, 26, 33, 40, 48, 41, 34, 27,
    20, 13, 6, 7, 14, 21, 28, 35, 42, 49, 56, 57, 50, 43, 36, 29, 22, 15, 23, 30, 37, 44, 51, 58,
    59, 52, 45, 38, 31, 39, 46, 53, 60, 61, 54, 47, 55, 62, 63,
];

struct Part {
    id: u8,
    wide: usize,
    tall: usize,
    table: usize,
    dc: usize,
    ac: usize,
    plane: Vec<u8>,
    stride: usize,
}

pub fn decode(body: &[u8]) -> Option<Picture> {
    let mut quant = [[0u16; 64]; 4];
    let mut dc: Vec<Option<Huffman>> = (0..4).map(|_| None).collect();
    let mut ac: Vec<Option<Huffman>> = (0..4).map(|_| None).collect();
    let mut parts: Vec<Part> = Vec::new();
    let mut width = 0usize;
    let mut height = 0usize;
    let mut restart = 0usize;

    let mut at = 2usize;
    while at + 3 < body.len() {
        if body[at] != 0xff {
            at += 1;
            continue;
        }
        let marker = body[at + 1];
        if marker == 0xd8 || marker == 0x01 || (0xd0..=0xd7).contains(&marker) {
            at += 2;
            continue;
        }
        let size = ((body[at + 2] as usize) << 8) | body[at + 3] as usize;
        let block = body.get(at + 4..at + 2 + size)?;
        match marker {
            0xdb => {
                let mut spot = 0usize;
                while spot < block.len() {
                    let head = block[spot];
                    let slot = (head & 15) as usize;
                    let wide = head >> 4 == 1;
                    spot += 1;
                    for index in 0..64 {
                        quant[slot][index] = if wide {
                            let value = ((block[spot] as u16) << 8) | block[spot + 1] as u16;
                            spot += 2;
                            value
                        } else {
                            let value = block[spot] as u16;
                            spot += 1;
                            value
                        };
                    }
                }
            }
            0xc4 => {
                let mut spot = 0usize;
                while spot + 17 <= block.len() {
                    let head = block[spot];
                    let slot = (head & 15) as usize;
                    let alternating = head >> 4 == 1;
                    let mut counts = [0u8; 16];
                    counts.copy_from_slice(&block[spot + 1..spot + 17]);
                    let total: usize = counts.iter().map(|found| *found as usize).sum();
                    let values = block.get(spot + 17..spot + 17 + total)?;
                    let table = Huffman::build(&counts, values);
                    if alternating {
                        ac[slot] = Some(table);
                    } else {
                        dc[slot] = Some(table);
                    }
                    spot += 17 + total;
                }
            }
            0xc2 | 0xc3 | 0xc5 | 0xc6 | 0xc7 | 0xc9 | 0xca | 0xcb => return None,
            0xc0 | 0xc1 => {
                height = ((block[1] as usize) << 8) | block[2] as usize;
                width = ((block[3] as usize) << 8) | block[4] as usize;
                let count = block[5] as usize;
                for index in 0..count {
                    let head = 6 + index * 3;
                    parts.push(Part {
                        id: block[head],
                        wide: (block[head + 1] >> 4) as usize,
                        tall: (block[head + 1] & 15) as usize,
                        table: block[head + 2] as usize,
                        dc: 0,
                        ac: 0,
                        plane: Vec::new(),
                        stride: 0,
                    });
                }
            }
            0xdd => {
                restart = ((block[0] as usize) << 8) | block[1] as usize;
            }
            0xda => {
                if parts.is_empty() || width == 0 || height == 0 {
                    return None;
                }
                let count = block[0] as usize;
                for index in 0..count {
                    let id = block[1 + index * 2];
                    let tables = block[2 + index * 2];
                    if let Some(part) = parts.iter_mut().find(|found| found.id == id) {
                        part.dc = (tables >> 4) as usize;
                        part.ac = (tables & 15) as usize;
                    }
                }
                let scan = body.get(at + 2 + size..)?;
                return scanned(scan, &mut parts, &quant, &dc, &ac, width, height, restart);
            }
            0xd9 => break,
            _ => {}
        }
        at += 2 + size;
    }
    None
}

fn scanned(
    scan: &[u8],
    parts: &mut [Part],
    quant: &[[u16; 64]; 4],
    dc: &[Option<Huffman>],
    ac: &[Option<Huffman>],
    width: usize,
    height: usize,
    restart: usize,
) -> Option<Picture> {
    let wide = parts.iter().map(|found| found.wide).max()?;
    let tall = parts.iter().map(|found| found.tall).max()?;
    let across = width.div_ceil(8 * wide);
    let down = height.div_ceil(8 * tall);
    for part in parts.iter_mut() {
        part.stride = across * part.wide * 8;
        part.plane = vec![0u8; part.stride * down * part.tall * 8];
    }
    let mut bits = Bits { body: scan, at: 0, slot: 0, held: 0 };
    let mut last = vec![0i32; parts.len()];
    let mut done = 0usize;
    for row in 0..down {
        for column in 0..across {
            if restart > 0 && done > 0 && done % restart == 0 {
                bits.held = 0;
                while bits.at + 1 < bits.body.len() {
                    if bits.body[bits.at] == 0xff && (0xd0..=0xd7).contains(&bits.body[bits.at + 1])
                    {
                        bits.at += 2;
                        break;
                    }
                    bits.at += 1;
                }
                for slot in last.iter_mut() {
                    *slot = 0;
                }
            }
            done += 1;
            for (index, part) in parts.iter_mut().enumerate() {
                for piece in 0..part.wide * part.tall {
                    let mut block = [0i32; 64];
                    let table = dc.get(part.dc)?.as_ref()?;
                    let length = bits.symbol(table)?;
                    let raw = bits.take(length)?;
                    last[index] += extend(raw, length);
                    block[0] = last[index] * i32::from(quant[part.table][0]);
                    let alt = ac.get(part.ac)?.as_ref()?;
                    let mut spot = 1usize;
                    while spot < 64 {
                        let code = bits.symbol(alt)?;
                        let run = (code >> 4) as usize;
                        let size = code & 15;
                        if size == 0 {
                            if run == 15 {
                                spot += 16;
                                continue;
                            }
                            break;
                        }
                        spot += run;
                        if spot >= 64 {
                            break;
                        }
                        let raw = bits.take(size)?;
                        block[ZIGZAG[spot]] =
                            extend(raw, size) * i32::from(quant[part.table][spot]);
                        spot += 1;
                    }
                    let mut out = [0u8; 64];
                    idct(&block, &mut out);
                    let ox = (column * part.wide + piece % part.wide) * 8;
                    let oy = (row * part.tall + piece / part.wide) * 8;
                    for y in 0..8 {
                        for x in 0..8 {
                            let spot = (oy + y) * part.stride + ox + x;
                            if spot < part.plane.len() {
                                part.plane[spot] = out[y * 8 + x];
                            }
                        }
                    }
                }
            }
        }
    }
    Some(colour(parts, width, height, wide, tall))
}

fn idct(block: &[i32; 64], out: &mut [u8; 64]) {
    let mut work = [0f64; 64];
    for y in 0..8usize {
        for x in 0..8usize {
            let mut total = 0f64;
            for v in 0..8usize {
                for u in 0..8usize {
                    let cu = if u == 0 { std::f64::consts::FRAC_1_SQRT_2 } else { 1.0 };
                    let cv = if v == 0 { std::f64::consts::FRAC_1_SQRT_2 } else { 1.0 };
                    total += cu
                        * cv
                        * f64::from(block[v * 8 + u])
                        * (((2 * x + 1) as f64) * (u as f64) * std::f64::consts::PI / 16.0).cos()
                        * (((2 * y + 1) as f64) * (v as f64) * std::f64::consts::PI / 16.0).cos();
                }
            }
            work[y * 8 + x] = total / 4.0;
        }
    }
    for (slot, value) in out.iter_mut().zip(work) {
        *slot = (value + 128.0).round().clamp(0.0, 255.0) as u8;
    }
}

fn colour(parts: &[Part], width: usize, height: usize, wide: usize, tall: usize) -> Picture {
    let mut pixels = vec![0u8; width * height * 3];
    for y in 0..height {
        for x in 0..width {
            let sample = |part: &Part| -> f64 {
                let sx = x * part.wide / wide;
                let sy = y * part.tall / tall;
                let spot = sy * part.stride + sx;
                f64::from(part.plane.get(spot).copied().unwrap_or(0))
            };
            let luma = sample(&parts[0]);
            let (blue, red) = if parts.len() >= 3 {
                (sample(&parts[1]) - 128.0, sample(&parts[2]) - 128.0)
            } else {
                (0.0, 0.0)
            };
            let spot = (y * width + x) * 3;
            pixels[spot] = (luma + 1.402 * red).round().clamp(0.0, 255.0) as u8;
            pixels[spot + 1] =
                (luma - 0.344136 * blue - 0.714136 * red).round().clamp(0.0, 255.0) as u8;
            pixels[spot + 2] = (luma + 1.772 * blue).round().clamp(0.0, 255.0) as u8;
        }
    }
    Picture { width, height, pixels }
}

pub fn profile(picture: &Picture) -> Vec<f64> {
    let width = picture.width;
    let height = picture.height;
    let mut out = vec![0f64; width];
    for x in 2..width - 2 {
        let mut run = 0usize;
        let mut best = 0usize;
        for y in 2..height - 2 {
            let step = (picture.grey(x + 1, y) - picture.grey(x - 1, y)).abs();
            let flat = (picture.grey(x + 3, y) - picture.grey(x + 1, y)).abs();
            let calm = (picture.grey(x - 1, y) - picture.grey(x - 3, y)).abs();
            if step > 6.0 && step > flat * 1.4 && step > calm * 1.4 {
                run += 1;
                best = best.max(run);
            } else {
                run = 0;
            }
        }
        out[x] = best as f64;
    }
    out
}

fn banded(picture: &Picture, left: usize, span: usize) -> f64 {
    let height = picture.height;
    let right = (left + span).min(picture.width - 1);
    let mut best = 0.0f64;
    for y in 3..height.saturating_sub(3) {
        let mut run = 0usize;
        for x in left..right {
            let step = (picture.grey(x, y + 1) - picture.grey(x, y - 1)).abs();
            if step > 6.0 {
                run += 1;
            }
        }
        let share = run as f64 / (right - left).max(1) as f64;
        if share > best {
            best = share;
        }
    }
    best
}

pub fn notch(picture: &Picture, piece: usize) -> Option<(usize, usize)> {
    let energy = profile(picture);
    let width = picture.width;
    let mut best: Option<(usize, usize, f64)> = None;
    for left in 8..width.saturating_sub(piece / 2) {
        let here = energy[left];
        if here < 6.0 {
            continue;
        }
        for span in piece.saturating_sub(12)..piece + 12 {
            let Some(other) = energy.get(left + span) else { continue };
            if *other < 6.0 {
                continue;
            }
            let score = here + other - (span as f64 - piece as f64).abs() * 0.4
                + banded(picture, left, span) * 6.0;
            if best.map_or(true, |(_, _, had)| score > had) {
                best = Some((left, span, score));
            }
        }
    }
    best.map(|(left, span, _)| (left, span))
}
