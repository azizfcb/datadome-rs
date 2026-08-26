import re, hashlib, sys, pathlib

ROOT = pathlib.Path(__file__).resolve().parent.parent
VM = pathlib.Path('/tmp/vm-corpus')
labeled = (VM / 'vm_labeled.js').read_text()
disasm  = (VM / 'disasm.js').read_text()

ops = {}
for m in re.finditer(r"(\d+):\s*\['([A-Z_0-9]+)',\s*'([^']*)'\]", disasm):
    ops[int(m.group(1))] = (m.group(2), m.group(3))

slot_roles = {
    'stack_pointer': '$SP', 'instruction_pointer': '$IP', 'frame_base_pointer': '$FBP',
    'frame_base_counter': '$FBC', 'last_result': '$LR', 'exit_flag': '$EXIT',
    'current_opcode_handler': '$CH', 'current_opcode_id': '$CI',
    'stack_offset': '$STACK', 'vm_start': '$VMS',
}
helpers = {
    'readUint8': '$RU8', 'readUint16': '$RU16', 'getVal': '$RVAL',
    'register2stack': '$R2S', 'stack2register': '$S2R',
    'storeToLastResult': '$SLR', 'GET': '$GET', 'SET': '$SET', 'fetch': '$FETCH',
}

def extract_handlers(text):
    out = []
    for m in re.finditer(r'A\[(\d+)\]\s*=\s*function\s*\(\)\s*\{', text):
        idx = int(m.group(1))
        i = m.end(); depth = 1
        while i < len(text) and depth > 0:
            if text[i] == '{': depth += 1
            elif text[i] == '}': depth -= 1
            i += 1
        out.append((idx, text[m.end():i-1].strip()))
    return out

def normalize(body):
    s = body
    for n, t in slot_roles.items(): s = re.sub(r'\b' + n + r'\b', t, s)
    for n, t in helpers.items(): s = re.sub(r'\b' + n + r'\b', t, s)
    locals_seen = []
    for m in re.finditer(r'\bvar\s+(\w+)', s):
        n = m.group(1)
        if n not in locals_seen and not n.startswith('$') and n != 'A':
            locals_seen.append(n)
    for m in re.finditer(r'for\s*\(\s*var\s+(\w+)', s):
        n = m.group(1)
        if n not in locals_seen and not n.startswith('$'):
            locals_seen.append(n)
    for i, n in enumerate(locals_seen):
        s = re.sub(r'\b' + n + r'\b', f'$L{i}', s)
    s = re.sub(r'//[^\n]*', '', s)
    s = re.sub(r'/\*.*?\*/', '', s, flags=re.DOTALL)
    s = re.sub(r'\s+', ' ', s).strip()
    return s

out = ROOT / 'dd-deob' / 'src' / 'vm_db.rs'
lines = ['pub static KNOWN_OPCODES: &[(&str, &str, &str)] = &[']
seen = set()
for idx, body in extract_handlers(labeled):
    op_idx = idx - 4783
    name, fmt = ops.get(op_idx, (f'OP_{op_idx}', '?'))
    norm = normalize(body)
    h = hashlib.sha256(norm.encode()).hexdigest()[:16]
    if h in seen: continue
    seen.add(h)
    name_esc = name.replace('"', '\\"')
    fmt_esc = fmt.replace('"', '\\"')
    lines.append(f'    ("{h}", "{name_esc}", "{fmt_esc}"),')
lines.append('];')
out.write_text('\n'.join(lines) + '\n')
print(f'wrote {out} ({len(seen)} unique opcode shapes)')
