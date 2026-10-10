#include "lzrc.hh"
#include "compression.hh"
#include "decrunchlzrcstub.inc"
#include <algorithm>
#include <array>
#include <cmath>
#include <cstdlib>
#include <fstream>
#include <iostream>
#include <iterator>
#include <stdexcept>

/*! \file
 *
 * \brief lzrc, see lzrc.hh for the format.
 *
 * The compressor does an optimal parse (dynamic programming over the
 * positions) with a static cost model which is derived from the
 * statistics of the previous parse. This is repeated a few times and
 * the parse with the smallest real (adaptive) size wins.
 */

namespace {

// Layout of the probabilities.
const int NLIT = 2;		// literal contexts
const int P_LIT = 0;		// trees of 256
const int P_FLAG = 512;		// two contexts
const int P_REP = 514;
const int P_EG = 544;		// three Elias-gamma contexts of 32
const int EG_OFF = 0, EG_LENN = 1, EG_LENR = 2;
const int P_SIZE = 640;
const int MAXV = 65535;

struct Model {
  std::array<uint8_t, P_SIZE> p;
  Model() { p.fill(128); }
  static void update(uint8_t &p, int bit) {
    if(bit) {
      p = std::min(255, p + ((256 - p + 15) >> 4));
    } else {
      p = std::max(1, p - ((p + 15) >> 4));
    }
  }
};

//! Range encoder with carry propagation through the already produced bits, see squz.cc.
class Encoder {
  std::vector<char> bits;
  unsigned range = 0xFFFF;
  void add(unsigned v) {
    int carry = 0;
    const size_t n = bits.size();
    for(size_t i = 0; i < n; ++i) {
      const size_t idx = n - 1 - i;
      const int a = bits[idx] + (i < 16 ? ((v >> i) & 1) : 0) + carry;
      bits[idx] = a & 1;
      carry = a >> 1;
      if(i >= 16 && !carry) {
	break;
      }
    }
  }
public:
  Encoder() : bits(16, 0) {}
  void encode(int bit, unsigned p) {
    const unsigned bound = (range >> 8) * p;
    if(bit) {
      range = bound;
    } else {
      add(bound);
      range -= bound;
    }
    while(range < 0x8000) {
      range <<= 1;
      bits.push_back(0);
    }
  }
  std::vector<uint8_t> finish() {
    int k = 0;
    while((2u << k) <= range) {
      ++k;
    }
    const int kk = k > 0 ? k - 1 : 0;
    add((1u << kk) - 1);
    for(int i = 0; i < kk; ++i) {
      bits[bits.size() - 1 - i] = 0;
    }
    const size_t n = bits.size() - kk;
    std::vector<uint8_t> out((n + 7) / 8, 0);
    for(size_t i = 0; i < n; ++i) {
      if(bits[i]) {
	out[i / 8] |= 0x80 >> (i % 8);
      }
    }
    return out;
  }
};

class Decoder {
  const std::vector<uint8_t> &d;
  size_t pos = 0;
  unsigned range = 0xFFFF, code = 0;
  int padding;
  int getbit() {
    const int b = pos / 8 < d.size() ? (d[pos / 8] >> (7 - pos % 8)) & 1 : padding;
    ++pos;
    return b;
  }
public:
  Decoder(const std::vector<uint8_t> &s, int pad) : d(s), padding(pad) {
    for(int i = 0; i < 16; ++i) {
      code = code * 2 + getbit();
    }
  }
  int decode(unsigned p) {
    const unsigned bound = (range >> 8) * p;
    int bit;
    if(code < bound) {
      range = bound;
      bit = 1;
    } else {
      code -= bound;
      range -= bound;
      bit = 0;
    }
    while(range < 0x8000) {
      range <<= 1;
      code = (code << 1) | getbit();
    }
    return bit;
  }
  size_t bytes_read() const { return (pos + 7) / 8; }
};

enum Kind : uint8_t { LIT, MATCH, REP, END };

struct Tok {
  Kind kind;
  uint8_t byte;		// LIT
  uint16_t off;		// MATCH
  uint16_t len;		// MATCH, REP
};

int ilog2(unsigned v) {
  int k = 0;
  while(v >> (k + 1)) {
    ++k;
  }
  return k;
}

//! Code a token, bit(slot, value) is called for every bit.
template<class F> void code_tok(const Tok &t, int prev, F &&bit) {
  auto gamma = [&](int ctx, unsigned v) {
    const int base = P_EG + 32 * ctx;
    const int k = ilog2(v);
    for(int i = 0; i < k; ++i) {
      bit(base + i, 1);
    }
    bit(base + k, 0);
    for(int j = k - 1; j >= 0; --j) {
      bit(base + 17 + j, (v >> j) & 1);
    }
  };
  bit(P_FLAG + prev, t.kind != LIT);
  switch(t.kind) {
  case LIT: {
    int node = 1;
    for(int i = 7; i >= 0; --i) {
      const int b = (t.byte >> i) & 1;
      bit(P_LIT + 256 * prev + node, b);
      node = node * 2 + b;
    }
    break;
  }
  case REP:
    bit(P_REP, 1);
    gamma(EG_LENR, t.len);
    break;
  case MATCH:
    bit(P_REP, 0);
    gamma(EG_OFF, t.off);
    gamma(EG_LENN, t.len - 1);
    break;
  case END:
    bit(P_REP, 0);
    for(int i = 0; i < 16; ++i) {
      bit(P_EG + 32 * EG_OFF + i, 1);
    }
    bit(P_EG + 32 * EG_OFF + 16, 0);
    break;
  }
}

std::vector<uint8_t> encode(const std::vector<Tok> &toks) {
  Model m;
  Encoder enc;
  int prev = 0;
  for(const auto &t : toks) {
    code_tok(t, prev, [&](int slot, int b) {
      enc.encode(b, m.p[slot]);
      Model::update(m.p[slot], b);
    });
    prev = t.kind == LIT ? 0 : 1;
  }
  return enc.finish();
}

}

std::vector<uint8_t> decrunch_lzrc(const std::vector<uint8_t> &stream, long *lead, int padding) {
  Decoder dec(stream, padding);
  Model m;
  std::vector<uint8_t> out;
  auto bit = [&](int slot) {
    const int b = dec.decode(m.p[slot]);
    Model::update(m.p[slot], b);
    return b;
  };
  // Returns the number and the number of the unary ones.
  auto gamma = [&](int ctx, int *kout = nullptr) {
    const int base = P_EG + 32 * ctx;
    int k = 0;
    while(bit(base + k)) {
      ++k;
    }
    if(kout) {
      *kout = k;
    }
    if(k > 15) {
      return 0u;
    }
    unsigned v = 1;
    for(int j = k - 1; j >= 0; --j) {
      v = v * 2 + bit(base + 17 + j);
    }
    return v;
  };
  int prev = 0;
  unsigned off = 1;
  long leadmax = 0;
  while(true) {
    if(!bit(P_FLAG + prev)) {
      int node = 1;
      for(int i = 0; i < 8; ++i) {
	node = node * 2 + bit(P_LIT + 256 * prev + node);
      }
      out.push_back(node & 255);
      prev = 0;
    } else {
      unsigned len;
      if(bit(P_REP)) {
	len = gamma(EG_LENR);
      } else {
	int k;
	off = gamma(EG_OFF, &k);
	if(k == 16) {
	  break;
	}
	len = gamma(EG_LENN) + 1;
      }
      if(off > out.size() || len == 0) {
	throw std::runtime_error("lzrc: bad match");
      }
      for(unsigned i = 0; i < len; ++i) {
	out.push_back(out[out.size() - off]);
      }
      prev = 1;
    }
    leadmax = std::max(leadmax, static_cast<long>(out.size()) - static_cast<long>(dec.bytes_read()));
  }
  if(lead) {
    *lead = leadmax;
  }
  return out;
}

namespace {

//! Static cost model in bits.
struct Costs {
  std::array<std::array<float, 2>, P_SIZE> bit;
  std::array<std::array<float, 256>, NLIT> lit;
  std::vector<float> gamma[3];
  explicit Costs(const std::array<std::array<double, 2>, P_SIZE> &cnt) {
    for(int s = 0; s < P_SIZE; ++s) {
      const double a = cnt[s][0] + 0.4, b = cnt[s][1] + 0.4;
      bit[s][0] = -std::log2(a / (a + b));
      bit[s][1] = -std::log2(b / (a + b));
    }
    for(int c = 0; c < NLIT; ++c) {
      for(int v = 0; v < 256; ++v) {
	int node = 1;
	float cost = 0;
	for(int i = 7; i >= 0; --i) {
	  const int b = (v >> i) & 1;
	  cost += bit[P_LIT + 256 * c + node][b];
	  node = node * 2 + b;
	}
	lit[c][v] = cost;
      }
    }
    for(int ctx = 0; ctx < 3; ++ctx) {
      gamma[ctx].assign(MAXV + 1, 0);
      const int base = P_EG + 32 * ctx;
      for(int v = 1; v <= MAXV; ++v) {
	const int k = ilog2(v);
	float cost = 0;
	for(int i = 0; i < k; ++i) {
	  cost += bit[base + i][1];
	}
	cost += bit[base + k][0];
	for(int j = k - 1; j >= 0; --j) {
	  cost += bit[base + 17 + j][(v >> j) & 1];
	}
	gamma[ctx][v] = cost;
      }
    }
  }
};

struct Cand {
  int off, len;
};

struct Node {
  float cost = 1e30f;
  uint32_t from = 0;
  uint16_t len = 0, off = 0;
  uint16_t lastoff = 1;
  Kind kind = LIT;
  uint8_t lastt = 0;
};

//! Optimal parse for the given costs.
std::vector<Tok> parse(const std::vector<uint8_t> &in, const std::vector<std::vector<Cand>> &cands, const Costs &c) {
  const size_t n = in.size();
  const int CAP = 4096;
  std::vector<Node> node(n + 1);
  node[0].cost = 0;
  auto relax = [&](size_t i, size_t j, float cost, Kind kind, int len, int off, int lastoff, int lastt) {
    if(cost < node[j].cost) {
      Node &x = node[j];
      x.cost = cost;
      x.from = i;
      x.kind = kind;
      x.len = len;
      x.off = off;
      x.lastoff = lastoff;
      x.lastt = lastt;
    }
  };
  // Lengths to try: all up to 40, then the maximum and the lengths where the number of bits changes.
  auto lengths = [](int lo, int hi, auto &&f) {
    for(int l = lo; l <= std::min(hi, 40); ++l) {
      f(l);
    }
    for(int l = std::max(lo, 41); l <= hi; ++l) {
      if(l == hi || (l & (l - 1)) == 0 || ((l - 1) & (l - 2)) == 0) {
	f(l);
      }
    }
  };
  for(size_t i = 0; i < n; ++i) {
    const Node cur = node[i];
    const int pt = cur.lastt;
    // Literal.
    relax(i, i + 1, cur.cost + c.bit[P_FLAG + pt][0] + c.lit[pt][in[i]], LIT, 1, 0, cur.lastoff, 0);
    const float fm = cur.cost + c.bit[P_FLAG + pt][1];
    // Repeat.
    if(i >= cur.lastoff) {
      int l0 = 0;
      while(l0 < CAP && i + l0 < n && in[i + l0] == in[i + l0 - cur.lastoff]) {
	++l0;
      }
      const float base = fm + c.bit[P_REP][1];
      lengths(1, l0, [&](int l) {
	relax(i, i + l, base + c.gamma[EG_LENR][l], REP, l, cur.lastoff, cur.lastoff, 1);
      });
    }
    // New offset.
    int prevlen = 1;
    for(const auto &cd : cands[i]) {
      const float base = fm + c.bit[P_REP][0] + c.gamma[EG_OFF][cd.off];
      lengths(prevlen + 1, cd.len, [&](int l) {
	relax(i, i + l, base + c.gamma[EG_LENN][l - 1], MATCH, l, cd.off, cd.off, 1);
      });
      prevlen = cd.len;
    }
  }
  std::vector<Tok> toks;
  for(size_t j = n; j > 0;) {
    const Node &x = node[j];
    Tok t{x.kind, 0, 0, 0};
    if(x.kind == LIT) {
      t.byte = in[x.from];
    } else {
      t.len = x.len;
      t.off = x.off;
    }
    toks.push_back(t);
    j = x.from;
  }
  std::reverse(toks.begin(), toks.end());
  toks.push_back(Tok{END, 0, 0, 0});
  return toks;
}

//! For every position the matches with increasing length at increasing offsets.
std::vector<std::vector<Cand>> find_matches(const std::vector<uint8_t> &in, int chain) {
  const size_t n = in.size();
  const int CAP = 4096;
  std::vector<std::vector<Cand>> cands(n);
  std::vector<int> head(65536, -1), prev(n, -1);
  for(size_t i = 0; i + 1 < n; ++i) {
    const unsigned h = in[i] | (in[i + 1] << 8);
    int best = 1;
    int steps = 0;
    for(int j = head[h]; j >= 0 && i - j <= static_cast<size_t>(MAXV) && steps < chain; j = prev[j], ++steps) {
      int l = 2;
      while(l < CAP && i + l < n && in[j + l] == in[i + l]) {
	++l;
      }
      if(l > best) {
	cands[i].push_back(Cand{static_cast<int>(i - j), l});
	best = l;
	if(l >= CAP || i + l >= n) {
	  break;
	}
      }
    }
    prev[i] = head[h];
    head[h] = i;
  }
  return cands;
}

}

std::vector<uint8_t> crunch_lzrc(const Data &data) {
  const auto &in = data.get_dataref();
  if(in.size() > 0xFFFF) {
    throw std::runtime_error("lzrc: too much data");
  }
  int chain = 1024, passes = 8;
  if(const char *e = std::getenv("XIPZ_LZRC_CHAIN")) {
    chain = std::max(1, std::atoi(e));
  }
  if(const char *e = std::getenv("XIPZ_LZRC_PASSES")) {
    passes = std::max(1, std::atoi(e));
  }
  const auto cands = find_matches(in, chain);
  std::array<std::array<double, 2>, P_SIZE> cnt;
  for(auto &a : cnt) {
    a = {1, 1};
  }
  std::vector<uint8_t> best;
  for(int pass = 0; pass < passes; ++pass) {
    const Costs costs(cnt);
    const auto toks = parse(in, cands, costs);
    auto stream = encode(toks);
    if(best.empty() || stream.size() < best.size()) {
      best = stream;
    }
    for(auto &a : cnt) {
      a = {0, 0};
    }
    int prev = 0;
    for(const auto &t : toks) {
      code_tok(t, prev, [&](int slot, int b) { cnt[slot][b] += 1; });
      prev = t.kind == LIT ? 0 : 1;
    }
  }
  for(int padding = 0; padding < 2; ++padding) {
    const auto check = decrunch_lzrc(best, nullptr, padding);
    if(check.size() != in.size() || !std::equal(check.begin(), check.end(), in.begin())) {
      throw std::logic_error("lzrc: decrunched data differs from the input");
    }
  }
  return best;
}


Compressor::crunched_data_type LzrcCompressor::compress() {
  auto crunched = crunch_lzrc(data);
  long lead;
  decrunch_lzrc(crunched, &lead);
  std::cout << "LZRC: lead " << lead << " bytes\n";
  if(!cliargs.raw_flag) {
    // The decoder lives at $0100-$01C0 and $0334-$03E9, the tables at $0400-$067F.
    const unsigned load = data.get_loadaddr();
    const unsigned loadend = load + data.size();
    const unsigned instart = (static_cast<unsigned>(cliargs.page_arg) << 8) - crunched.size();
    if(load < 0x0800 && loadend > 0x0100) {
      throw std::runtime_error("lzrc: the data would overwrite the decoder or the tables ($0100-$07FF)");
    }
    if(instart < 0x0800) {
      throw std::runtime_error("lzrc: compressed data would be moved into $0100-$07FF, use a higher page");
    }
    if(static_cast<long>(instart) - static_cast<long>(load) < lead) {
      throw std::runtime_error("lzrc: the output would overtake the compressed data, use a higher page");
    }
  }
  return crunched;
}


std::ostream &write_lzrc_stub(std::ostream &out, uint16_t size, uint16_t loadaddr, uint16_t jmp, uint8_t pagehi) {
  // Positions from decrunchlzrcstub.prg.label. Add 2 for the load address.
  const int POS_OF_END_OF_CDATA = 0x42 + 2;
  const int POS_OF_PAGEHI = 0x41 + 2; // High page +1.
  const int POS_OF_JMP = 0x1A3 + 2;
  const int POS_OF_DEST_LOW = 0x33 + 2;
  const int POS_OF_DEST_HIGH = 0x37 + 2;
  std::vector<uint8_t> stub(decrunchlzrcstub_prg, decrunchlzrcstub_prg + decrunchlzrcstub_prg_len);

  // The stub contains $1000 as the page limit and $033C as the destination and jump address. If
  // they are not found the positions above are out of date.
  if(stub.at(POS_OF_PAGEHI) != 0x10 ||
     (stub.at(POS_OF_JMP) | (stub.at(POS_OF_JMP + 1) << 8)) != 0x033C ||
     stub.at(POS_OF_DEST_LOW) != 0x3C || stub.at(POS_OF_DEST_HIGH) != 0x03) {
    throw std::logic_error("lzrc: stub patch positions are out of date, see decrunchlzrcstub.prg.label");
  }
  // Assign new end of compressed data, it must point to the byte after the data.
  unsigned endptr = stub.at(POS_OF_END_OF_CDATA) | (stub.at(POS_OF_END_OF_CDATA + 1) << 8);
  endptr += size;
  stub.at(POS_OF_END_OF_CDATA) = endptr & 0xFF;
  stub.at(POS_OF_END_OF_CDATA + 1) = (endptr >> 8) & 0xFF;
  stub.at(POS_OF_PAGEHI) = pagehi;
  stub.at(POS_OF_JMP) = jmp & 0xFF;
  stub.at(POS_OF_JMP + 1) = (jmp >> 8) & 0xFF;
  stub.at(POS_OF_DEST_LOW) = loadaddr & 0xFF;
  stub.at(POS_OF_DEST_HIGH) = (loadaddr >> 8) & 0xFF;
  std::copy(stub.begin(), stub.end(), std::ostream_iterator<unsigned char>(out));
  return out;
}


void LzrcCompressor::write_stub(std::ofstream &out, const std::vector<uint8_t> &c,
				uint16_t load, uint16_t jmp) {
  write_lzrc_stub(out, c.size(), load, jmp, cliargs.page_arg);
}
