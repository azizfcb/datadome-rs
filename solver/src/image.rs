pub struct Picture {
    pub width: usize,
    pub height: usize,
    pub stride: usize,
    pub plane: Vec<u8>,
}

struct Huffman {
    least: [i32; 17],
    most: [i32; 17],
    first: [usize; 17],
    quick: [(u8, u8); 256],
    values: Vec<u8>,
}

impl Huffman {
    fn build(counts: &[u8; 16], values: &[u8]) -> Huffman {
        let mut table = Huffman {
            least: [0; 17],
            most: [-1; 17],
            first: [0; 17],
            quick: [(0, 0); 256],
            values: values.to_vec(),
        };
        let mut code: i32 = 0;
        let mut at = 0usize;
        for length in 1..=16usize {
            let count = counts[length - 1] as usize;
            table.first[length] = at;
            table.least[length] = code;
            if count == 0 {
                table.most[length] = -1;
            } else {
                table.most[length] = code + count as i32 - 1;
                code += count as i32;
                at += count;
            }
            code <<= 1;
        }
        for length in 1..=8usize {
            if table.most[length] < 0 {
                continue;
            }
            for code in table.least[length]..=table.most[length] {
                let spot = table.first[length] + (code - table.least[length]) as usize;
                let Some(value) = values.get(spot).copied() else { continue };
                let head = (code as usize) << (8 - length);
                for step in 0..1usize << (8 - length) {
                    table.quick[head + step] = (value, length as u8);
                }
            }
        }
        table
    }
}

struct Bits<'a> {
    body: &'a [u8],
    at: usize,
    held: u64,
    bits: u32,
}

impl<'a> Bits<'a> {
    #[inline]
    fn fill(&mut self) {
        while self.bits <= 56 {
            let mut byte = 0u8;
            if self.at < self.body.len() {
                byte = self.body[self.at];
                self.at += 1;
                if byte == 0xff {
                    let next = self.body.get(self.at).copied().unwrap_or(0xd9);
                    if next == 0 {
                        self.at += 1;
                    } else if (0xd0..=0xd7).contains(&next) {
                        self.at += 1;
                        byte = self.body.get(self.at).copied().unwrap_or(0);
                        self.at += 1;
                    } else {
                        self.at -= 1;
                        byte = 0;
                    }
                }
            }
            self.held = (self.held << 8) | u64::from(byte);
            self.bits += 8;
        }
    }

    #[inline]
    fn need(&mut self, count: u32) {
        if self.bits < count {
            self.fill();
        }
    }

    #[inline]
    fn receive(&mut self, count: u8) -> i32 {
        if count == 0 {
            return 0;
        }
        self.need(u32::from(count));
        self.bits -= u32::from(count);
        let raw = ((self.held >> self.bits) & ((1u64 << count) - 1)) as i32;
        let low = ((raw >> (count - 1)) & 1).wrapping_sub(1);
        raw + (low & ((-1i32 << count) + 1))
    }

    #[inline]
    fn symbol(&mut self, table: &Huffman) -> Option<u8> {
        self.need(16);
        let peek = (self.held >> (self.bits - 16)) & 0xffff;
        let fast = table.quick[(peek >> 8) as usize];
        if fast.1 != 0 {
            self.bits -= u32::from(fast.1);
            return Some(fast.0);
        }
        for length in 9..=16usize {
            let code = (peek >> (16 - length)) as i32;
            if table.most[length] >= code {
                self.bits -= length as u32;
                let spot = table.first[length] + (code - table.least[length]) as usize;
                return table.values.get(spot).copied();
            }
        }
        None
    }

    fn seek(&mut self) -> usize {
        self.at - (self.bits / 8) as usize
    }
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
    for (index, part) in parts.iter_mut().enumerate() {
        part.stride = across * part.wide * 8;
        if index == 0 {
            part.plane = vec![0u8; part.stride * down * part.tall * 8];
        }
    }
    let basis = cosines();
    #[cfg(target_arch = "x86_64")]
    let wide_idct = is_x86_feature_detected!("avx2") && is_x86_feature_detected!("fma");
    let mut bits = Bits { body: scan, at: 0, held: 0, bits: 0 };
    let mut last = vec![0i32; parts.len()];
    let mut done = 0usize;
    for row in 0..down {
        for column in 0..across {
            if restart > 0 && done > 0 && done % restart == 0 {
                let mut spot = bits.seek();
                while spot + 1 < bits.body.len() {
                    if bits.body[spot] == 0xff && (0xd0..=0xd7).contains(&bits.body[spot + 1]) {
                        spot += 2;
                        break;
                    }
                    spot += 1;
                }
                bits.at = spot;
                bits.held = 0;
                bits.bits = 0;
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
                    last[index] += bits.receive(length);
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
                        block[ZIGZAG[spot]] =
                            bits.receive(size) * i32::from(quant[part.table][spot]);
                        spot += 1;
                    }
                    if index != 0 {
                        continue;
                    }
                    let mut out = [0u8; 64];
                    if spot == 1 {
                        let flat = block[0] as f32 * basis[0] * basis[0] + 128.5;
                        out.fill(if flat < 0.0 {
                            0
                        } else if flat > 255.0 {
                            255
                        } else {
                            flat as u8
                        });
                    } else {
                        #[cfg(target_arch = "x86_64")]
                        if wide_idct {
                            unsafe { idct_wide(&block, &basis, &mut out) };
                        } else {
                            idct(&block, &basis, &mut out);
                        }
                        #[cfg(not(target_arch = "x86_64"))]
                        idct(&block, &basis, &mut out);
                    }
                    let ox = (column * part.wide + piece % part.wide) * 8;
                    let oy = (row * part.tall + piece / part.wide) * 8;
                    for y in 0..8 {
                        let spot = (oy + y) * part.stride + ox;
                        let room = part.plane.len().saturating_sub(spot).min(8);
                        part.plane[spot..spot + room].copy_from_slice(&out[y * 8..y * 8 + room]);
                    }
                }
            }
        }
    }
    let luma = std::mem::take(&mut parts[0].plane);
    Some(Picture { width, height, stride: parts[0].stride, plane: luma })
}

fn cosines() -> [f32; 64] {
    let mut table = [0f32; 64];
    for u in 0..8usize {
        let scale = if u == 0 { 0.5 * std::f32::consts::FRAC_1_SQRT_2 } else { 0.5 };
        for x in 0..8usize {
            let angle = (2 * x + 1) as f32 * u as f32 * std::f32::consts::PI / 16.0;
            table[u * 8 + x] = scale * angle.cos();
        }
    }
    table
}

#[cfg(target_arch = "x86_64")]
#[target_feature(enable = "avx2,fma")]
unsafe fn idct_wide(block: &[i32; 64], basis: &[f32; 64], out: &mut [u8; 64]) {
    use core::arch::x86_64::*;
    let mut rows = [_mm256_setzero_ps(); 8];
    for v in 0..8usize {
        let line = &block[v * 8..v * 8 + 8];
        if line[1..].iter().all(|found| *found == 0) {
            rows[v] = _mm256_set1_ps(line[0] as f32 * basis[0]);
            continue;
        }
        let mut sum = _mm256_setzero_ps();
        for u in 0..8usize {
            let coeff = _mm256_set1_ps(line[u] as f32);
            let wave = _mm256_loadu_ps(basis.as_ptr().add(u * 8));
            sum = _mm256_fmadd_ps(coeff, wave, sum);
        }
        rows[v] = sum;
    }
    let bias = _mm256_set1_ps(128.5);
    for y in 0..8usize {
        let mut sum = bias;
        for v in 0..8usize {
            let scale = _mm256_set1_ps(basis[v * 8 + y]);
            sum = _mm256_fmadd_ps(scale, rows[v], sum);
        }
        let whole = _mm256_cvttps_epi32(sum);
        let narrow = _mm_packs_epi32(
            _mm256_castsi256_si128(whole),
            _mm256_extracti128_si256(whole, 1),
        );
        let bytes = _mm_packus_epi16(narrow, narrow);
        _mm_storel_epi64(out.as_mut_ptr().add(y * 8) as *mut __m128i, bytes);
    }
}

fn idct(block: &[i32; 64], basis: &[f32; 64], out: &mut [u8; 64]) {
    let mut rows = [0f32; 64];
    for v in 0..8usize {
        let line = &block[v * 8..v * 8 + 8];
        if line[1..].iter().all(|found| *found == 0) {
            let flat = line[0] as f32 * basis[0];
            rows[v * 8..v * 8 + 8].fill(flat);
            continue;
        }
        for x in 0..8usize {
            let mut total = 0f32;
            for u in 0..8usize {
                total += basis[u * 8 + x] * line[u] as f32;
            }
            rows[v * 8 + x] = total;
        }
    }
    for x in 0..8usize {
        for y in 0..8usize {
            let mut total = 0f32;
            for v in 0..8usize {
                total += basis[v * 8 + y] * rows[v * 8 + x];
            }
            let value = total + 128.5;
            out[y * 8 + x] = if value < 0.0 {
                0
            } else if value > 255.0 {
                255
            } else {
                value as u8
            };
        }
    }
}

pub const PIECE: usize = 57;
const KEEP: f32 = 0.3013;
const LIFT: f32 = 63.76;
const LOW: i32 = 46;
const HIGH: i32 = 159;
const BODY: (usize, usize, usize, usize) = (16, 18, 40, 55);
const HALO: usize = 5;
const SHORT: usize = 256;
const DRIFT: f32 = 40000.0;

fn sprite() -> [bool; PIECE * PIECE] {
    let mut shape = [false; PIECE * PIECE];
    let round = |cx: f32, cy: f32, x: usize, y: usize| {
        let dx = x as f32 - cx;
        let dy = y as f32 - cy;
        dx * dx + dy * dy <= 81.0
    };
    for y in 0..PIECE {
        for x in 0..PIECE {
            let body = x < 42 && (16..57).contains(&y);
            let head = round(20.5, 8.5, x, y);
            let ear = round(47.5, 36.5, x, y);
            let bite = round(4.5, 36.5, x, y);
            shape[y * PIECE + x] = (body || head || ear) && !bite;
        }
    }
    shape
}

struct Seam {
    inside: usize,
    outside: usize,
}

fn seams(width: usize) -> Vec<Seam> {
    let shape = sprite();
    let solid = |x: i32, y: i32| {
        (0..PIECE as i32).contains(&x)
            && (0..PIECE as i32).contains(&y)
            && shape[y as usize * PIECE + x as usize]
    };
    let mut list = Vec::new();
    for y in 0..PIECE as i32 {
        for x in 0..PIECE as i32 {
            if !solid(x, y) {
                continue;
            }
            for (dx, dy) in [(-1i32, 0i32), (1, 0), (0, -1), (0, 1)] {
                if solid(x + dx, y + dy) {
                    continue;
                }
                if !(0..PIECE as i32).contains(&(x + dx)) || !(0..PIECE as i32).contains(&(y + dy)) {
                    continue;
                }
                list.push(Seam {
                    inside: y as usize * width + x as usize,
                    outside: (y + dy) as usize * width + (x + dx) as usize,
                });
            }
        }
    }
    list
}

const INSIDE: f32 = ((BODY.2 - BODY.0) * (BODY.3 - BODY.1)) as f32;
const SPAN: usize = PIECE + 2 * HALO;
const AROUND: f32 = (SPAN * SPAN - PIECE * PIECE) as f32;

pub struct Finder {
    stray: Vec<u32>,
    swing: Vec<u32>,
    rough: Vec<f32>,
    pick: Vec<(f32, u32)>,
    seams: Vec<Seam>,
    stride: usize,
    wide: usize,
    furthest: usize,
    twice: [i32; 256],
    square: [i32; 256],
}

impl Finder {
    pub fn new() -> Finder {
        Finder {
            stray: Vec::new(),
            swing: Vec::new(),
            rough: Vec::new(),
            pick: Vec::with_capacity(SHORT * 2),
            seams: Vec::new(),
            stride: 0,
            wide: 0,
            furthest: 0,
            twice: std::array::from_fn(|value| {
                ((LIFT - (1.0 - KEEP) * value as f32) * 2.0).round() as i32
            }),
            square: std::array::from_fn(|value| {
                let want = LIFT - (1.0 - KEEP) * value as f32;
                (want * want).round() as i32
            }),
        }
    }

    #[cfg(target_arch = "x86_64")]
    #[target_feature(enable = "avx2")]
    unsafe fn crest(
        line: *const u8,
        width: usize,
        up: (*const u32, *const u32),
        down: (*mut u32, *mut u32),
    ) -> (usize, u32, u32) {
        use core::arch::x86_64::*;
        let floor = _mm_set1_epi8(LOW as i8);
        let ceiling = _mm_set1_epi8(HIGH as u8 as i8);
        let mut miss = 0u32;
        let mut flow = 0u32;
        let mut x = 0usize;
        while x + 17 <= width {
            let here = _mm_loadu_si128(line.add(x) as *const __m128i);
            let next = _mm_loadu_si128(line.add(x + 1) as *const __m128i);
            let off = _mm_max_epu8(_mm_subs_epu8(floor, here), _mm_subs_epu8(here, ceiling));
            let broad = _mm256_cvtepu8_epi16(off);
            let cost = _mm256_mullo_epi16(broad, broad);
            let step = _mm256_cvtepu8_epi16(_mm_or_si128(
                _mm_subs_epu8(next, here),
                _mm_subs_epu8(here, next),
            ));
            for half in 0..2usize {
                let cut = |v: __m256i| {
                    if half == 0 {
                        _mm256_castsi256_si128(v)
                    } else {
                        _mm256_extracti128_si256(v, 1)
                    }
                };
                let ramp = |v: __m256i, carry: u32| {
                    let mut sum = _mm256_add_epi32(v, _mm256_slli_si256(v, 4));
                    sum = _mm256_add_epi32(sum, _mm256_slli_si256(sum, 8));
                    let low = _mm256_permute2x128_si256(sum, sum, 0x08);
                    sum = _mm256_add_epi32(sum, _mm256_shuffle_epi32(low, 0xff));
                    _mm256_add_epi32(sum, _mm256_set1_epi32(carry as i32))
                };
                let sum = ramp(_mm256_cvtepu16_epi32(cut(cost)), miss);
                let run = ramp(_mm256_cvtepu16_epi32(cut(step)), flow);
                let spot = x + half * 8 + 1;
                _mm256_storeu_si256(
                    down.0.add(spot) as *mut __m256i,
                    _mm256_add_epi32(sum, _mm256_loadu_si256(up.0.add(spot) as *const __m256i)),
                );
                _mm256_storeu_si256(
                    down.1.add(spot) as *mut __m256i,
                    _mm256_add_epi32(run, _mm256_loadu_si256(up.1.add(spot) as *const __m256i)),
                );
                miss = _mm256_extract_epi32(sum, 7) as u32;
                flow = _mm256_extract_epi32(run, 7) as u32;
            }
            x += 16;
        }
        (x, miss, flow)
    }

    fn tables(&mut self, picture: &Picture) {
        let width = picture.width;
        let height = picture.height;
        let stride = picture.stride;
        let wide = width + 1;
        let cells = wide * (height + 1);
        if self.wide != wide || self.stray.len() != cells {
            self.stray = vec![0u32; cells];
            self.swing = vec![0u32; cells];
            self.wide = wide;
        }
        let mut cost = [0u32; 256];
        for value in 0..256usize {
            let off = (LOW - value as i32).max(value as i32 - HIGH).max(0) as u32;
            cost[value] = off * off;
        }
        #[cfg(target_arch = "x86_64")]
        let sharp = is_x86_feature_detected!("avx2");
        for y in 0..height {
            let line = &picture.plane[y * stride..y * stride + width];
            let (mut run, mut flow) = (0u32, 0u32);
            let above = y * wide;
            let here = (y + 1) * wide;
            let mut start = 0usize;
            #[cfg(target_arch = "x86_64")]
            if sharp {
                let up = unsafe {
                    (self.stray.as_ptr().add(above), self.swing.as_ptr().add(above))
                };
                let down = unsafe {
                    (self.stray.as_mut_ptr().add(here), self.swing.as_mut_ptr().add(here))
                };
                let (done, miss, sway) = unsafe { Finder::crest(line.as_ptr(), width, up, down) };
                start = done;
                run = miss;
                flow = sway;
            }
            for x in start..width - 1 {
                run += cost[line[x] as usize];
                flow += i32::from(line[x + 1]).abs_diff(i32::from(line[x]));
                self.stray[here + x + 1] = self.stray[above + x + 1] + run;
                self.swing[here + x + 1] = self.swing[above + x + 1] + flow;
            }
            run += cost[line[width - 1] as usize];
            self.stray[here + width] = self.stray[above + width] + run;
            self.swing[here + width] = self.swing[above + width] + flow;
        }
    }

    #[inline]
    fn patch(cells: &[u32], wide: usize, x0: usize, y0: usize, x1: usize, y1: usize) -> u32 {
        let top = y0 * wide;
        let bottom = y1 * wide;
        cells[bottom + x1].wrapping_add(cells[top + x0])
            .wrapping_sub(cells[bottom + x0])
            .wrapping_sub(cells[top + x1])
    }

    #[cfg(target_arch = "x86_64")]
    #[target_feature(enable = "avx2")]
    unsafe fn scan(
        &mut self,
        rows: (usize, usize, usize, usize, usize, usize),
        from: usize,
        upto: usize,
        out: usize,
    ) -> usize {
        use core::arch::x86_64::*;
        let (by, dy, hy, gy, ty, my) = rows;
        let wide = self.wide;
        let stray = self.stray.as_ptr();
        let swing = self.swing.as_ptr();
        let inside = _mm256_set1_ps(1.0 / INSIDE);
        let around = _mm256_set1_ps(KEEP / AROUND);
        let weight = _mm256_set1_ps(DRIFT);
        let sign = _mm256_castsi256_ps(_mm256_set1_epi32(0x7fff_ffff));
        let mut left = from;
        while left + 8 <= upto {
            let grab = |base: *const u32, row: usize, off: isize| {
                _mm256_loadu_si256(base.offset((row * wide + left) as isize + off) as *const __m256i)
            };
            let miss = _mm256_sub_epi32(
                _mm256_add_epi32(grab(stray, dy, BODY.2 as isize), grab(stray, by, BODY.0 as isize)),
                _mm256_add_epi32(grab(stray, dy, BODY.0 as isize), grab(stray, by, BODY.2 as isize)),
            );
            let near = _mm256_sub_epi32(
                _mm256_add_epi32(grab(swing, dy, BODY.2 as isize), grab(swing, by, BODY.0 as isize)),
                _mm256_add_epi32(grab(swing, dy, BODY.0 as isize), grab(swing, by, BODY.2 as isize)),
            );
            let halo = _mm256_sub_epi32(
                _mm256_add_epi32(
                    grab(swing, gy, (SPAN - HALO) as isize),
                    grab(swing, hy, -(HALO as isize)),
                ),
                _mm256_add_epi32(
                    grab(swing, gy, -(HALO as isize)),
                    grab(swing, hy, (SPAN - HALO) as isize),
                ),
            );
            let block = _mm256_sub_epi32(
                _mm256_add_epi32(grab(swing, my, PIECE as isize), grab(swing, ty, 0)),
                _mm256_add_epi32(grab(swing, my, 0), grab(swing, ty, PIECE as isize)),
            );
            let ridge = _mm256_mul_ps(_mm256_cvtepi32_ps(near), inside);
            let field = _mm256_mul_ps(
                _mm256_cvtepi32_ps(_mm256_sub_epi32(halo, block)),
                around,
            );
            let gap = _mm256_and_ps(_mm256_sub_ps(ridge, field), sign);
            let score = _mm256_sub_ps(
                _mm256_sub_ps(_mm256_setzero_ps(), _mm256_cvtepi32_ps(miss)),
                _mm256_mul_ps(weight, gap),
            );
            _mm256_storeu_ps(self.rough.as_mut_ptr().add(out + left), score);
            left += 8;
        }
        left
    }

    #[inline]
    fn one(&self, rows: (usize, usize, usize, usize, usize, usize), left: usize, hx: usize) -> f32 {
        let (by, dy, hy, gy, ty, my) = rows;
        let wide = self.wide;
        let miss = Finder::patch(&self.stray, wide, left + BODY.0, by, left + BODY.2, dy);
        let near = Finder::patch(&self.swing, wide, left + BODY.0, by, left + BODY.2, dy);
        let halo = Finder::patch(&self.swing, wide, hx, hy, hx + SPAN, gy);
        let block = Finder::patch(&self.swing, wide, left, ty, left + PIECE, my);
        let gap = near as f32 / INSIDE - KEEP * halo.wrapping_sub(block) as f32 / AROUND;
        -(miss as f32) - DRIFT * gap.abs()
    }

    pub fn find(&mut self, picture: &Picture) -> Option<(usize, usize)> {
        let width = picture.width;
        let height = picture.height;
        if width < SPAN || height < SPAN {
            return None;
        }
        self.tables(picture);
        let stride = picture.stride;
        if self.stride != stride || self.seams.is_empty() {
            self.seams = seams(stride);
            self.furthest = self
                .seams
                .iter()
                .map(|seam| seam.inside.max(seam.outside))
                .max()
                .unwrap_or(0);
            self.stride = stride;
        }
        let reach = width - PIECE;
        let drop = height - PIECE;
        let far_edge = width - SPAN;
        let low_edge = height - SPAN;
        let span = reach + 1;
        if self.rough.len() != span * (drop + 1) {
            self.rough = vec![f32::MIN; span * (drop + 1)];
        }
        #[cfg(target_arch = "x86_64")]
        let wide_scan = is_x86_feature_detected!("avx2");
        for top in 0..=drop {
            let hy = top.max(HALO).min(low_edge + HALO) - HALO;
            let rows = (top + BODY.1, top + BODY.3, hy, hy + SPAN, top, top + PIECE);
            let out = top * span;
            let mut left = 0usize;
            while left < HALO.min(span) {
                let hx = left.max(HALO).min(far_edge + HALO) - HALO;
                self.rough[out + left] = self.one(rows, left, hx);
                left += 1;
            }
            let cliff = (far_edge + HALO).min(reach) + 1;
            #[cfg(target_arch = "x86_64")]
            if wide_scan && cliff > left {
                left = unsafe { self.scan(rows, left, cliff, out) };
            }
            while left <= reach {
                let hx = left.max(HALO).min(far_edge + HALO) - HALO;
                self.rough[out + left] = self.one(rows, left, hx);
                left += 1;
            }
        }
        self.pick.clear();
        let mut bar = f32::MIN;
        for (spot, score) in self.rough.iter().enumerate() {
            if *score <= bar {
                continue;
            }
            self.pick.push((*score, spot as u32));
            if self.pick.len() == SHORT * 2 {
                self.pick.select_nth_unstable_by(SHORT - 1, |one, two| two.0.total_cmp(&one.0));
                self.pick.truncate(SHORT);
                bar = self.pick[SHORT - 1].0;
            }
        }
        if self.pick.len() > SHORT {
            self.pick.select_nth_unstable_by(SHORT - 1, |one, two| two.0.total_cmp(&one.0));
            self.pick.truncate(SHORT);
        }
        let mut best: Option<(usize, usize, f32)> = None;
        for (rated, packed) in self.pick.iter() {
            let left = *packed as usize % span;
            let top = *packed as usize / span;
            let origin = top * stride + left;
            let mut edge = [0i32; 4];
            if origin + self.furthest >= picture.plane.len() {
                continue;
            }
            for group in self.seams.chunks(4) {
                for (slot, seam) in group.iter().enumerate() {
                    let near = unsafe {
                        i32::from(*picture.plane.get_unchecked(origin + seam.inside))
                    };
                    let far = unsafe {
                        *picture.plane.get_unchecked(origin + seam.outside) as usize
                    };
                    edge[slot] += unsafe {
                        self.twice.get_unchecked(far) * (near - far as i32)
                            - self.square.get_unchecked(far)
                    };
                }
            }
            let score = (edge[0] + edge[1] + edge[2] + edge[3]) as f32 + rated;
            if best.map_or(true, |(_, _, had)| score > had) {
                best = Some((left, top, score));
            }
        }
        best.map(|(left, top, _)| (left, top))
    }
}

thread_local! {
    static SCOUT: std::cell::RefCell<Finder> = std::cell::RefCell::new(Finder::new());
}

pub fn locate(picture: &Picture) -> Option<(usize, usize)> {
    SCOUT.with(|scout| scout.borrow_mut().find(picture))
}
