#include <algorithm>
#include <chrono>
#include <cmath>
#include <cstdint>
#include <cstdio>
#include <cstring>
#include <immintrin.h>
#include <limits>
#include <vector>

namespace {

struct Picture {
    size_t width = 0;
    size_t height = 0;
    size_t stride = 0;
    std::vector<uint8_t> plane;
};

struct Huffman {
    int32_t least[17] = {};
    int32_t most[17] = {};
    size_t first[17] = {};
    uint8_t quick[256][2] = {};
    std::vector<uint8_t> values;
    bool ready = false;

    void build(const uint8_t* counts, const uint8_t* source, size_t total) {
        values.assign(source, source + total);
        int32_t code = 0;
        size_t at = 0;
        for (size_t length = 1; length <= 16; ++length) {
            size_t count = counts[length - 1];
            first[length] = at;
            least[length] = code;
            if (count == 0) {
                most[length] = -1;
            } else {
                most[length] = code + int32_t(count) - 1;
                code += int32_t(count);
                at += count;
            }
            code <<= 1;
        }
        std::memset(quick, 0, sizeof(quick));
        for (size_t length = 1; length <= 8; ++length) {
            if (most[length] < 0) continue;
            for (int32_t spot = least[length]; spot <= most[length]; ++spot) {
                size_t index = first[length] + size_t(spot - least[length]);
                if (index >= values.size()) continue;
                size_t head = size_t(spot) << (8 - length);
                for (size_t step = 0; step < (size_t(1) << (8 - length)); ++step) {
                    quick[head + step][0] = values[index];
                    quick[head + step][1] = uint8_t(length);
                }
            }
        }
        ready = true;
    }
};

struct Bits {
    const uint8_t* body = nullptr;
    size_t size = 0;
    size_t at = 0;
    uint64_t held = 0;
    uint32_t bits = 0;

    void fill() {
        while (bits <= 56) {
            uint8_t byte = 0;
            if (at < size) {
                byte = body[at++];
                if (byte == 0xff) {
                    uint8_t next = at < size ? body[at] : 0xd9;
                    if (next == 0) {
                        ++at;
                    } else if (next >= 0xd0 && next <= 0xd7) {
                        ++at;
                        byte = at < size ? body[at] : 0;
                        ++at;
                    } else {
                        --at;
                        byte = 0;
                    }
                }
            }
            held = (held << 8) | uint64_t(byte);
            bits += 8;
        }
    }

    inline void need(uint32_t count) {
        if (bits < count) fill();
    }

    inline int32_t receive(uint8_t count) {
        if (count == 0) return 0;
        need(count);
        bits -= count;
        int32_t raw = int32_t((held >> bits) & ((uint64_t(1) << count) - 1));
        int32_t low = ((raw >> (count - 1)) & 1) - 1;
        return raw + (low & ((-1 << count) + 1));
    }

    inline bool symbol(const Huffman& table, uint8_t& out) {
        need(16);
        uint32_t peek = uint32_t((held >> (bits - 16)) & 0xffff);
        const uint8_t* fast = table.quick[peek >> 8];
        if (fast[1] != 0) {
            bits -= fast[1];
            out = fast[0];
            return true;
        }
        for (size_t length = 9; length <= 16; ++length) {
            int32_t code = int32_t(peek >> (16 - length));
            if (table.most[length] >= code) {
                bits -= uint32_t(length);
                size_t spot = table.first[length] + size_t(code - table.least[length]);
                if (spot >= table.values.size()) return false;
                bits += 0;
                out = table.values[spot];
                return true;
            }
        }
        return false;
    }

    size_t seek() const { return at - bits / 8; }
};

const size_t ZIGZAG[64] = {
    0, 1, 8, 16, 9, 2, 3, 10, 17, 24, 32, 25, 18, 11, 4, 5, 12, 19, 26, 33, 40, 48, 41, 34, 27,
    20, 13, 6, 7, 14, 21, 28, 35, 42, 49, 56, 57, 50, 43, 36, 29, 22, 15, 23, 30, 37, 44, 51, 58,
    59, 52, 45, 38, 31, 39, 46, 53, 60, 61, 54, 47, 55, 62, 63,
};

struct Part {
    uint8_t id = 0;
    size_t wide = 0;
    size_t tall = 0;
    size_t table = 0;
    size_t dc = 0;
    size_t ac = 0;
    size_t stride = 0;
    std::vector<uint8_t> plane;
};

void cosines(float* basis) {
    for (size_t u = 0; u < 8; ++u) {
        float scale = u == 0 ? 0.5f * float(M_SQRT1_2) : 0.5f;
        for (size_t x = 0; x < 8; ++x) {
            float angle = float(2 * x + 1) * float(u) * float(M_PI) / 16.0f;
            basis[u * 8 + x] = scale * std::cos(angle);
        }
    }
}

__attribute__((target("avx2,fma")))
void idct_wide(const int32_t* block, const float* basis, uint8_t* out) {
    __m256 rows[8];
    for (size_t v = 0; v < 8; ++v) {
        const int32_t* line = block + v * 8;
        bool flat = true;
        for (size_t u = 1; u < 8; ++u) {
            if (line[u] != 0) { flat = false; break; }
        }
        if (flat) {
            rows[v] = _mm256_set1_ps(float(line[0]) * basis[0]);
            continue;
        }
        __m256 sum = _mm256_setzero_ps();
        for (size_t u = 0; u < 8; ++u) {
            sum = _mm256_fmadd_ps(_mm256_set1_ps(float(line[u])),
                                  _mm256_loadu_ps(basis + u * 8), sum);
        }
        rows[v] = sum;
    }
    const __m256 bias = _mm256_set1_ps(128.5f);
    for (size_t y = 0; y < 8; ++y) {
        __m256 sum = bias;
        for (size_t v = 0; v < 8; ++v) {
            sum = _mm256_fmadd_ps(_mm256_set1_ps(basis[v * 8 + y]), rows[v], sum);
        }
        __m256i whole = _mm256_cvttps_epi32(sum);
        __m128i narrow = _mm_packs_epi32(_mm256_castsi256_si128(whole),
                                         _mm256_extracti128_si256(whole, 1));
        _mm_storel_epi64(reinterpret_cast<__m128i*>(out + y * 8),
                         _mm_packus_epi16(narrow, narrow));
    }
}

void idct(const int32_t* block, const float* basis, uint8_t* out) {
    float rows[64];
    for (size_t v = 0; v < 8; ++v) {
        const int32_t* line = block + v * 8;
        for (size_t x = 0; x < 8; ++x) {
            float total = 0.0f;
            for (size_t u = 0; u < 8; ++u) total += basis[u * 8 + x] * float(line[u]);
            rows[v * 8 + x] = total;
        }
    }
    for (size_t x = 0; x < 8; ++x) {
        for (size_t y = 0; y < 8; ++y) {
            float total = 0.0f;
            for (size_t v = 0; v < 8; ++v) total += basis[v * 8 + y] * rows[v * 8 + x];
            float value = total + 128.5f;
            out[y * 8 + x] = value < 0.0f ? 0 : (value > 255.0f ? 255 : uint8_t(value));
        }
    }
}

bool scanned(const uint8_t* scan, size_t size, std::vector<Part>& parts,
             const uint16_t quant[4][64], const Huffman* dc, const Huffman* ac,
             size_t width, size_t height, size_t restart, Picture& out) {
    size_t wide = 0, tall = 0;
    for (const Part& part : parts) {
        wide = std::max(wide, part.wide);
        tall = std::max(tall, part.tall);
    }
    if (wide == 0 || tall == 0) return false;
    size_t across = (width + 8 * wide - 1) / (8 * wide);
    size_t down = (height + 8 * tall - 1) / (8 * tall);
    for (size_t index = 0; index < parts.size(); ++index) {
        parts[index].stride = across * parts[index].wide * 8;
        if (index == 0) {
            parts[index].plane.assign(parts[index].stride * down * parts[index].tall * 8, 0);
        }
    }
    float basis[64];
    cosines(basis);
    bool sharp = __builtin_cpu_supports("avx2") && __builtin_cpu_supports("fma");
    Bits bits{scan, size, 0, 0, 0};
    std::vector<int32_t> last(parts.size(), 0);
    size_t done = 0;
    for (size_t row = 0; row < down; ++row) {
        for (size_t column = 0; column < across; ++column) {
            if (restart > 0 && done > 0 && done % restart == 0) {
                size_t spot = bits.seek();
                while (spot + 1 < size) {
                    if (bits.body[spot] == 0xff && bits.body[spot + 1] >= 0xd0 &&
                        bits.body[spot + 1] <= 0xd7) {
                        spot += 2;
                        break;
                    }
                    ++spot;
                }
                bits.at = spot;
                bits.held = 0;
                bits.bits = 0;
                std::fill(last.begin(), last.end(), 0);
            }
            ++done;
            for (size_t index = 0; index < parts.size(); ++index) {
                Part& part = parts[index];
                for (size_t piece = 0; piece < part.wide * part.tall; ++piece) {
                    int32_t block[64] = {};
                    if (part.dc >= 4 || !dc[part.dc].ready) return false;
                    uint8_t length = 0;
                    if (!bits.symbol(dc[part.dc], length)) return false;
                    last[index] += bits.receive(length);
                    block[0] = last[index] * int32_t(quant[part.table][0]);
                    if (part.ac >= 4 || !ac[part.ac].ready) return false;
                    size_t spot = 1;
                    while (spot < 64) {
                        uint8_t code = 0;
                        if (!bits.symbol(ac[part.ac], code)) return false;
                        size_t run = code >> 4;
                        uint8_t size_of = code & 15;
                        if (size_of == 0) {
                            if (run == 15) { spot += 16; continue; }
                            break;
                        }
                        spot += run;
                        if (spot >= 64) break;
                        block[ZIGZAG[spot]] = bits.receive(size_of) * int32_t(quant[part.table][spot]);
                        ++spot;
                    }
                    if (index != 0) continue;
                    uint8_t cell[64];
                    if (spot == 1) {
                        float flat = float(block[0]) * basis[0] * basis[0] + 128.5f;
                        uint8_t value = flat < 0.0f ? 0 : (flat > 255.0f ? 255 : uint8_t(flat));
                        std::memset(cell, value, 64);
                    } else if (sharp) {
                        idct_wide(block, basis, cell);
                    } else {
                        idct(block, basis, cell);
                    }
                    size_t ox = (column * part.wide + piece % part.wide) * 8;
                    size_t oy = (row * part.tall + piece / part.wide) * 8;
                    for (size_t y = 0; y < 8; ++y) {
                        size_t place = (oy + y) * part.stride + ox;
                        if (place >= part.plane.size()) break;
                        size_t room = std::min<size_t>(8, part.plane.size() - place);
                        std::memcpy(part.plane.data() + place, cell + y * 8, room);
                    }
                }
            }
        }
    }
    out.width = width;
    out.height = height;
    out.stride = parts[0].stride;
    out.plane = std::move(parts[0].plane);
    return true;
}

bool decode(const uint8_t* body, size_t size, Picture& out) {
    uint16_t quant[4][64] = {};
    Huffman dc[4];
    Huffman ac[4];
    std::vector<Part> parts;
    size_t width = 0, height = 0, restart = 0;
    size_t at = 2;
    while (at + 3 < size) {
        if (body[at] != 0xff) { ++at; continue; }
        uint8_t marker = body[at + 1];
        if (marker == 0xd8 || marker == 0x01 || (marker >= 0xd0 && marker <= 0xd7)) {
            at += 2;
            continue;
        }
        size_t chunk = (size_t(body[at + 2]) << 8) | body[at + 3];
        if (at + 2 + chunk > size) return false;
        const uint8_t* block = body + at + 4;
        size_t room = chunk - 2;
        switch (marker) {
            case 0xdb: {
                size_t spot = 0;
                while (spot < room) {
                    uint8_t head = block[spot++];
                    size_t slot = head & 15;
                    bool broad = (head >> 4) == 1;
                    for (size_t index = 0; index < 64; ++index) {
                        if (broad) {
                            quant[slot][index] = uint16_t((block[spot] << 8) | block[spot + 1]);
                            spot += 2;
                        } else {
                            quant[slot][index] = block[spot++];
                        }
                    }
                }
                break;
            }
            case 0xc4: {
                size_t spot = 0;
                while (spot + 17 <= room) {
                    uint8_t head = block[spot];
                    size_t slot = head & 15;
                    bool alternating = (head >> 4) == 1;
                    size_t total = 0;
                    for (size_t index = 0; index < 16; ++index) total += block[spot + 1 + index];
                    if (spot + 17 + total > room) return false;
                    if (alternating) ac[slot].build(block + spot + 1, block + spot + 17, total);
                    else dc[slot].build(block + spot + 1, block + spot + 17, total);
                    spot += 17 + total;
                }
                break;
            }
            case 0xc2: case 0xc3: case 0xc5: case 0xc6: case 0xc7:
            case 0xc9: case 0xca: case 0xcb:
                return false;
            case 0xc0: case 0xc1: {
                height = (size_t(block[1]) << 8) | block[2];
                width = (size_t(block[3]) << 8) | block[4];
                size_t count = block[5];
                for (size_t index = 0; index < count; ++index) {
                    const uint8_t* head = block + 6 + index * 3;
                    Part part;
                    part.id = head[0];
                    part.wide = head[1] >> 4;
                    part.tall = head[1] & 15;
                    part.table = head[2];
                    parts.push_back(std::move(part));
                }
                break;
            }
            case 0xdd:
                restart = (size_t(block[0]) << 8) | block[1];
                break;
            case 0xda: {
                if (parts.empty() || width == 0 || height == 0) return false;
                size_t count = block[0];
                for (size_t index = 0; index < count; ++index) {
                    uint8_t id = block[1 + index * 2];
                    uint8_t tables = block[2 + index * 2];
                    for (Part& part : parts) {
                        if (part.id == id) {
                            part.dc = tables >> 4;
                            part.ac = tables & 15;
                        }
                    }
                }
                size_t head = at + 2 + chunk;
                return scanned(body + head, size - head, parts, quant, dc, ac, width, height,
                               restart, out);
            }
            case 0xd9:
                return false;
            default:
                break;
        }
        at += 2 + chunk;
    }
    return false;
}

const size_t PIECE = 57;
const float KEEP = 0.3013f;
const float LIFT = 63.76f;
const int32_t LOW = 46;
const int32_t HIGH = 159;
const size_t BODY_X0 = 16, BODY_Y0 = 18, BODY_X1 = 40, BODY_Y1 = 55;
const size_t HALO = 5;
const size_t SHORT = 256;
const float DRIFT = 40000.0f;
const size_t SPAN = PIECE + 2 * HALO;
const float INSIDE = float((BODY_X1 - BODY_X0) * (BODY_Y1 - BODY_Y0));
const float AROUND = float(SPAN * SPAN - PIECE * PIECE);

void sprite(bool* shape) {
    auto round = [](float cx, float cy, size_t x, size_t y) {
        float dx = float(x) - cx;
        float dy = float(y) - cy;
        return dx * dx + dy * dy <= 81.0f;
    };
    for (size_t y = 0; y < PIECE; ++y) {
        for (size_t x = 0; x < PIECE; ++x) {
            bool body = x < 42 && y >= 16 && y < 57;
            bool head = round(20.5f, 8.5f, x, y);
            bool ear = round(47.5f, 36.5f, x, y);
            bool bite = round(4.5f, 36.5f, x, y);
            shape[y * PIECE + x] = (body || head || ear) && !bite;
        }
    }
}

struct Seam {
    size_t inside;
    size_t outside;
};

std::vector<Seam> seams(size_t width) {
    bool shape[PIECE * PIECE];
    sprite(shape);
    auto solid = [&](int32_t x, int32_t y) {
        return x >= 0 && y >= 0 && x < int32_t(PIECE) && y < int32_t(PIECE) &&
               shape[size_t(y) * PIECE + size_t(x)];
    };
    std::vector<Seam> list;
    const int32_t steps[4][2] = {{-1, 0}, {1, 0}, {0, -1}, {0, 1}};
    for (int32_t y = 0; y < int32_t(PIECE); ++y) {
        for (int32_t x = 0; x < int32_t(PIECE); ++x) {
            if (!solid(x, y)) continue;
            for (const auto& step : steps) {
                int32_t nx = x + step[0];
                int32_t ny = y + step[1];
                if (solid(nx, ny)) continue;
                if (nx < 0 || ny < 0 || nx >= int32_t(PIECE) || ny >= int32_t(PIECE)) continue;
                list.push_back({size_t(y) * width + size_t(x), size_t(ny) * width + size_t(nx)});
            }
        }
    }
    return list;
}

__attribute__((target("avx2")))
__m256i ramp(__m256i v, uint32_t carry) {
    __m256i sum = _mm256_add_epi32(v, _mm256_slli_si256(v, 4));
    sum = _mm256_add_epi32(sum, _mm256_slli_si256(sum, 8));
    __m256i low = _mm256_permute2x128_si256(sum, sum, 0x08);
    sum = _mm256_add_epi32(sum, _mm256_shuffle_epi32(low, 0xff));
    return _mm256_add_epi32(sum, _mm256_set1_epi32(int32_t(carry)));
}

__attribute__((target("avx2")))
inline __m256i grab(const uint32_t* base, size_t row, size_t wide, size_t left, ptrdiff_t off) {
    return _mm256_loadu_si256(
        reinterpret_cast<const __m256i*>(base + ptrdiff_t(row * wide + left) + off));
}

struct Finder {
    std::vector<uint32_t> stray;
    std::vector<uint32_t> swing;
    std::vector<float> rough;
    std::vector<std::pair<float, uint32_t>> pick;
    std::vector<Seam> outline;
    size_t stride = 0;
    size_t wide = 0;
    int32_t twice[256];
    int32_t square[256];

    Finder() {
        for (size_t value = 0; value < 256; ++value) {
            float want = LIFT - (1.0f - KEEP) * float(value);
            twice[value] = int32_t(std::lround(want * 2.0f));
            square[value] = int32_t(std::lround(want * want));
        }
        pick.reserve(SHORT * 2);
    }

    __attribute__((target("avx2")))
    static size_t crest(const uint8_t* line, size_t width, const uint32_t* upMiss,
                        const uint32_t* upFlow, uint32_t* downMiss, uint32_t* downFlow,
                        uint32_t& miss, uint32_t& flow) {
        const __m128i floor = _mm_set1_epi8(int8_t(LOW));
        const __m128i ceiling = _mm_set1_epi8(int8_t(HIGH));
        size_t x = 0;
        while (x + 17 <= width) {
            __m128i here = _mm_loadu_si128(reinterpret_cast<const __m128i*>(line + x));
            __m128i next = _mm_loadu_si128(reinterpret_cast<const __m128i*>(line + x + 1));
            __m128i off = _mm_max_epu8(_mm_subs_epu8(floor, here), _mm_subs_epu8(here, ceiling));
            __m256i broad = _mm256_cvtepu8_epi16(off);
            __m256i cost = _mm256_mullo_epi16(broad, broad);
            __m256i step = _mm256_cvtepu8_epi16(
                _mm_or_si128(_mm_subs_epu8(next, here), _mm_subs_epu8(here, next)));
            for (size_t half = 0; half < 2; ++half) {
                __m128i piece = half == 0 ? _mm256_castsi256_si128(cost)
                                          : _mm256_extracti128_si256(cost, 1);
                __m128i slice = half == 0 ? _mm256_castsi256_si128(step)
                                          : _mm256_extracti128_si256(step, 1);
                __m256i sum = ramp(_mm256_cvtepu16_epi32(piece), miss);
                __m256i run = ramp(_mm256_cvtepu16_epi32(slice), flow);
                size_t spot = x + half * 8 + 1;
                _mm256_storeu_si256(reinterpret_cast<__m256i*>(downMiss + spot),
                    _mm256_add_epi32(sum, _mm256_loadu_si256(
                        reinterpret_cast<const __m256i*>(upMiss + spot))));
                _mm256_storeu_si256(reinterpret_cast<__m256i*>(downFlow + spot),
                    _mm256_add_epi32(run, _mm256_loadu_si256(
                        reinterpret_cast<const __m256i*>(upFlow + spot))));
                miss = uint32_t(_mm256_extract_epi32(sum, 7));
                flow = uint32_t(_mm256_extract_epi32(run, 7));
            }
            x += 16;
        }
        return x;
    }

    void tables(const Picture& picture) {
        size_t width = picture.width;
        size_t height = picture.height;
        size_t stride = picture.stride;
        size_t cells = (width + 1) * (height + 1);
        if (wide != width + 1 || stray.size() != cells) {
            stray.assign(cells, 0);
            swing.assign(cells, 0);
            wide = width + 1;
        }
        uint32_t cost[256];
        for (size_t value = 0; value < 256; ++value) {
            int32_t off = std::max(std::max(LOW - int32_t(value), int32_t(value) - HIGH), 0);
            cost[value] = uint32_t(off * off);
        }
        bool sharp = __builtin_cpu_supports("avx2");
        for (size_t y = 0; y < height; ++y) {
            const uint8_t* line = picture.plane.data() + y * stride;
            uint32_t run = 0, flow = 0;
            size_t above = y * wide;
            size_t here = (y + 1) * wide;
            size_t start = 0;
            if (sharp) {
                start = crest(line, width, stray.data() + above, swing.data() + above,
                              stray.data() + here, swing.data() + here, run, flow);
            }
            for (size_t x = start; x + 1 < width; ++x) {
                run += cost[line[x]];
                flow += uint32_t(std::abs(int32_t(line[x + 1]) - int32_t(line[x])));
                stray[here + x + 1] = stray[above + x + 1] + run;
                swing[here + x + 1] = swing[above + x + 1] + flow;
            }
            run += cost[line[width - 1]];
            stray[here + width] = stray[above + width] + run;
            swing[here + width] = swing[above + width] + flow;
        }
    }

    static inline uint32_t patch(const uint32_t* cells, size_t wide, size_t x0, size_t y0,
                                 size_t x1, size_t y1) {
        return cells[y1 * wide + x1] + cells[y0 * wide + x0] - cells[y1 * wide + x0] -
               cells[y0 * wide + x1];
    }

    __attribute__((target("avx2")))
    size_t scan(size_t by, size_t dy, size_t hy, size_t gy, size_t ty, size_t my, size_t from,
                size_t upto, size_t out) {
        const uint32_t* miss = stray.data();
        const uint32_t* flow = swing.data();
        const __m256 inside = _mm256_set1_ps(1.0f / INSIDE);
        const __m256 around = _mm256_set1_ps(KEEP / AROUND);
        const __m256 weight = _mm256_set1_ps(DRIFT);
        const __m256 sign = _mm256_castsi256_ps(_mm256_set1_epi32(0x7fffffff));
        size_t left = from;
        while (left + 8 <= upto) {
            __m256i cost = _mm256_sub_epi32(
                _mm256_add_epi32(grab(miss, dy, wide, left, BODY_X1), grab(miss, by, wide, left, BODY_X0)),
                _mm256_add_epi32(grab(miss, dy, wide, left, BODY_X0), grab(miss, by, wide, left, BODY_X1)));
            __m256i near = _mm256_sub_epi32(
                _mm256_add_epi32(grab(flow, dy, wide, left, BODY_X1), grab(flow, by, wide, left, BODY_X0)),
                _mm256_add_epi32(grab(flow, dy, wide, left, BODY_X0), grab(flow, by, wide, left, BODY_X1)));
            __m256i halo = _mm256_sub_epi32(
                _mm256_add_epi32(grab(flow, gy, wide, left, ptrdiff_t(SPAN - HALO)),
                                 grab(flow, hy, wide, left, -ptrdiff_t(HALO))),
                _mm256_add_epi32(grab(flow, gy, wide, left, -ptrdiff_t(HALO)),
                                 grab(flow, hy, wide, left, ptrdiff_t(SPAN - HALO))));
            __m256i block = _mm256_sub_epi32(
                _mm256_add_epi32(grab(flow, my, wide, left, ptrdiff_t(PIECE)), grab(flow, ty, wide, left, 0)),
                _mm256_add_epi32(grab(flow, my, wide, left, 0), grab(flow, ty, wide, left, ptrdiff_t(PIECE))));
            __m256 ridge = _mm256_mul_ps(_mm256_cvtepi32_ps(near), inside);
            __m256 field = _mm256_mul_ps(
                _mm256_cvtepi32_ps(_mm256_sub_epi32(halo, block)), around);
            __m256 gap = _mm256_and_ps(_mm256_sub_ps(ridge, field), sign);
            __m256 score = _mm256_sub_ps(
                _mm256_sub_ps(_mm256_setzero_ps(), _mm256_cvtepi32_ps(cost)),
                _mm256_mul_ps(weight, gap));
            _mm256_storeu_ps(rough.data() + out + left, score);
            left += 8;
        }
        return left;
    }

    inline float one(size_t by, size_t dy, size_t hy, size_t gy, size_t ty, size_t my,
                     size_t left, size_t hx) {
        uint32_t cost = patch(stray.data(), wide, left + BODY_X0, by, left + BODY_X1, dy);
        uint32_t near = patch(swing.data(), wide, left + BODY_X0, by, left + BODY_X1, dy);
        uint32_t halo = patch(swing.data(), wide, hx, hy, hx + SPAN, gy);
        uint32_t block = patch(swing.data(), wide, left, ty, left + PIECE, my);
        float gap = float(near) / INSIDE - KEEP * float(halo - block) / AROUND;
        return -float(cost) - DRIFT * std::fabs(gap);
    }

    bool find(const Picture& picture, size_t& outLeft, size_t& outTop) {
        size_t width = picture.width;
        size_t height = picture.height;
        if (width < SPAN || height < SPAN) return false;
        tables(picture);
        if (stride != picture.stride || outline.empty()) {
            outline = seams(picture.stride);
            stride = picture.stride;
        }
        size_t reach = width - PIECE;
        size_t drop = height - PIECE;
        size_t farEdge = width - SPAN;
        size_t lowEdge = height - SPAN;
        size_t span = reach + 1;
        if (rough.size() != span * (drop + 1)) rough.assign(span * (drop + 1), 0.0f);
        bool sharp = __builtin_cpu_supports("avx2");
        for (size_t top = 0; top <= drop; ++top) {
            size_t hy = std::min(std::max(top, HALO), lowEdge + HALO) - HALO;
            size_t by = top + BODY_Y0;
            size_t dy = top + BODY_Y1;
            size_t out = top * span;
            size_t left = 0;
            while (left < std::min(HALO, span)) {
                size_t hx = std::min(std::max(left, HALO), farEdge + HALO) - HALO;
                rough[out + left] = one(by, dy, hy, hy + SPAN, top, top + PIECE, left, hx);
                ++left;
            }
            size_t cliff = std::min(farEdge + HALO, reach) + 1;
            if (sharp && cliff > left) {
                left = scan(by, dy, hy, hy + SPAN, top, top + PIECE, left, cliff, out);
            }
            while (left <= reach) {
                size_t hx = std::min(std::max(left, HALO), farEdge + HALO) - HALO;
                rough[out + left] = one(by, dy, hy, hy + SPAN, top, top + PIECE, left, hx);
                ++left;
            }
        }
        pick.clear();
        float bar = -std::numeric_limits<float>::max();
        for (size_t spot = 0; spot < rough.size(); ++spot) {
            if (rough[spot] <= bar) continue;
            pick.emplace_back(rough[spot], uint32_t(spot));
            if (pick.size() == SHORT * 2) {
                std::nth_element(pick.begin(), pick.begin() + (SHORT - 1), pick.end(),
                                 [](const auto& a, const auto& b) { return a.first > b.first; });
                pick.resize(SHORT);
                bar = pick[SHORT - 1].first;
            }
        }
        if (pick.size() > SHORT) {
            std::nth_element(pick.begin(), pick.begin() + (SHORT - 1), pick.end(),
                             [](const auto& a, const auto& b) { return a.first > b.first; });
            pick.resize(SHORT);
        }
        bool found = false;
        float bestScore = 0.0f;
        for (const auto& entry : pick) {
            size_t left = entry.second % span;
            size_t top = entry.second / span;
            size_t origin = top * picture.stride + left;
            int32_t edge[4] = {0, 0, 0, 0};
            size_t count = outline.size();
            size_t index = 0;
            for (; index + 4 <= count; index += 4) {
                for (size_t slot = 0; slot < 4; ++slot) {
                    const Seam& seam = outline[index + slot];
                    int32_t near = picture.plane[origin + seam.inside];
                    size_t far = picture.plane[origin + seam.outside];
                    edge[slot] += twice[far] * (near - int32_t(far)) - square[far];
                }
            }
            for (size_t slot = 0; index < count; ++index, ++slot) {
                const Seam& seam = outline[index];
                int32_t near = picture.plane[origin + seam.inside];
                size_t far = picture.plane[origin + seam.outside];
                edge[slot & 3] += twice[far] * (near - int32_t(far)) - square[far];
            }
            float score = float(edge[0] + edge[1] + edge[2] + edge[3]) + entry.first;
            if (!found || score > bestScore) {
                found = true;
                bestScore = score;
                outLeft = left;
                outTop = top;
            }
        }
        return found;
    }
};

void report(const char* name, std::vector<uint64_t>& taken) {
    std::sort(taken.begin(), taken.end());
    auto pick = [&](double share) {
        return double(taken[size_t(double(taken.size() - 1) * share)]) / 1000.0;
    };
    std::fprintf(stderr, "%s n=%zu min %.1f p50 %.1f p90 %.1f p99 %.1f max %.1f us\n", name,
                 taken.size(), double(taken.front()) / 1000.0, pick(0.50), pick(0.90), pick(0.99),
                 double(taken.back()) / 1000.0);
}

}  // namespace

int sheet(int argc, char** argv) {
    bool shape[PIECE * PIECE];
    sprite(shape);
    std::vector<uint8_t> canvas;
    size_t width = 0, tall = 0;
    Finder finder;
    for (int index = 2; index < argc; ++index) {
        std::FILE* handle = std::fopen(argv[index], "rb");
        if (!handle) continue;
        std::fseek(handle, 0, SEEK_END);
        long size = std::ftell(handle);
        std::fseek(handle, 0, SEEK_SET);
        std::vector<uint8_t> body;
        body.resize(size_t(size));
        if (std::fread(body.data(), 1, body.size(), handle) != body.size()) { std::fclose(handle); continue; }
        std::fclose(handle);
        Picture picture;
        if (!decode(body.data(), body.size(), picture)) continue;
        size_t left = 0, top = 0;
        if (!finder.find(picture, left, top)) continue;
        for (size_t y = 0; y < PIECE; ++y) {
            for (size_t x = 0; x < PIECE; ++x) {
                if (!shape[y * PIECE + x]) continue;
                bool rim = x == 0 || y == 0 || x + 1 == PIECE || y + 1 == PIECE ||
                           !shape[y * PIECE + x - 1] || !shape[y * PIECE + x + 1] ||
                           !shape[(y - 1) * PIECE + x] || !shape[(y + 1) * PIECE + x];
                if (!rim) continue;
                size_t spot = (top + y) * picture.stride + left + x;
                if (spot < picture.plane.size()) picture.plane[spot] = 255;
            }
        }
        width = picture.width;
        tall += picture.height;
        for (size_t y = 0; y < picture.height; ++y) {
            canvas.insert(canvas.end(), picture.plane.begin() + y * picture.stride,
                          picture.plane.begin() + y * picture.stride + picture.width);
        }
        std::fprintf(stderr, "%s %zu %zu\n", argv[index], left, top);
    }
    char head[64];
    int used = std::snprintf(head, sizeof(head), "P5\n%zu %zu\n255\n", width, tall);
    std::FILE* out = std::fopen("sheet.pgm", "wb");
    std::fwrite(head, 1, size_t(used), out);
    std::fwrite(canvas.data(), 1, canvas.size(), out);
    std::fclose(out);
    return 0;
}

int main(int argc, char** argv) {
    if (argc > 1 && std::strcmp(argv[1], "sheet") == 0) return sheet(argc, argv);
    if (argc < 2) {
        std::fprintf(stderr, "usage: notch <image.jpg> [rounds]\n");
        return 1;
    }
    std::FILE* handle = std::fopen(argv[1], "rb");
    if (!handle) {
        std::fprintf(stderr, "no image at %s\n", argv[1]);
        return 1;
    }
    std::fseek(handle, 0, SEEK_END);
    long size = std::ftell(handle);
    std::fseek(handle, 0, SEEK_SET);
    std::vector<uint8_t> body{};
    body.resize(size_t(size));
    if (std::fread(body.data(), 1, body.size(), handle) != body.size()) return 1;
    std::fclose(handle);
    size_t rounds = argc > 2 ? size_t(std::atol(argv[2])) : 2000;

    Picture picture;
    if (!decode(body.data(), body.size(), picture)) {
        std::fprintf(stderr, "decode failed\n");
        return 1;
    }
    uint32_t seal = 2166136261u;
    for (uint8_t byte : picture.plane) seal = (seal ^ byte) * 16777619u;
    std::fprintf(stderr, "image %zux%zu %zu bytes plane %08x\n", picture.width, picture.height,
                 body.size(), seal);
    Finder finder;
    size_t left = 0, top = 0;
    if (finder.find(picture, left, top)) {
        std::fprintf(stderr, "notch %zu %zu\n", left, top);
    } else {
        std::fprintf(stderr, "notch none\n");
    }

    std::vector<uint64_t> taken;
    taken.reserve(rounds);
    for (size_t round = 0; round < rounds + rounds / 10; ++round) {
        auto start = std::chrono::steady_clock::now();
        Picture again;
        bool done = decode(body.data(), body.size(), again);
        auto spent = std::chrono::steady_clock::now() - start;
        asm volatile("" : : "r"(&again), "r"(&done) : "memory");
        if (round >= rounds / 10) {
            taken.push_back(uint64_t(
                std::chrono::duration_cast<std::chrono::nanoseconds>(spent).count()));
        }
    }
    report("decode", taken);

    taken.clear();
    for (size_t round = 0; round < rounds + rounds / 10; ++round) {
        auto start = std::chrono::steady_clock::now();
        size_t x = 0, y = 0;
        bool done = finder.find(picture, x, y);
        auto spent = std::chrono::steady_clock::now() - start;
        asm volatile("" : : "r"(&x), "r"(&y), "r"(&done) : "memory");
        if (round >= rounds / 10) {
            taken.push_back(uint64_t(
                std::chrono::duration_cast<std::chrono::nanoseconds>(spent).count()));
        }
    }
    report("detect", taken);
    return 0;
}
