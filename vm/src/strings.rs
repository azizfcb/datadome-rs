pub fn decode(blob: &[u8], index: usize, key: u32) -> Option<String> {
    let word = |i: usize| -> Option<i32> { blob.get(i).map(|b| *b as i32) };
    let seed = word(0)? | word(1)? << 8 | word(2)? << 16 | word(3)? << 24;

    let d = 4 + 6 * index;
    let at = (word(d)? | word(d + 1)? << 8 | word(d + 2)? << 16) as usize;
    let len = (word(d + 3)? | word(d + 4)? << 8) as usize;
    let utf8 = word(d + 5)? & 1 != 0;

    let bytes: Vec<u8> = (0..len)
        .map(|p| {
            let mask = (seed ^ key as i32 ^ (p as i32 + 1)) & 255;
            blob.get(at + p).map(|b| (*b as i32 ^ mask) as u8)
        })
        .collect::<Option<_>>()?;

    Some(if utf8 {
        String::from_utf8_lossy(&bytes).into_owned()
    } else {
        bytes.iter().map(|b| *b as char).collect()
    })
}
