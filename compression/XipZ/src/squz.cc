#include "squz.hh"
#include "compression.hh"
#include "decrunchsquzstub.inc"
#include <algorithm>
#include <array>
#include <fstream>
#include <iostream>
#include <iterator>
#include <stdexcept>
#include <unordered_map>

/*! \file
 *
 * \brief squz, see squz.hh for the format.
 */

int squz_oplen(int op) {
  if((op & 0x9F) == 0) { // BRK, JSR, RTI, RTS
    return op == 0x20 ? 3 : 1;
  }
  const int cc = op & 3, bbb = (op >> 2) & 7;
  if(cc & 1) { // 01 and 11: bbb 3, 6, 7 take an absolute address
    return ((0xC8 >> bbb) & 1) ? 3 : 2;
  }
  // 00 and 10
  if((0x44 >> bbb) & 1) {
    return 1; // implied or accumulator
  }
  return ((0x88 >> bbb) & 1) ? 3 : 2;
}

namespace {

//! Adaptive probabilities, shared by encoder and decoder.
struct Model {
  std::array<std::array<uint8_t, 256>, 3> lit;  //!< Literal trees, node 1-255.
  std::array<std::array<uint8_t, 32>, 3> pair;  //!< Node 0 is the flag, 1-31 the pair tree.
  Model() {
    for(auto &a : lit) {
      a.fill(128);
    }
    for(auto &a : pair) {
      a.fill(128);
    }
  }
  static void update(uint8_t &p, int bit) {
    if(bit) {
      p = std::min(255, p + ((256 - p + 7) >> 3));
    } else {
      p = std::max(1, p - ((p + 7) >> 3));
    }
  }
};

//! Range encoder with carry propagation through the already produced bits.
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
  /*! Terminate the stream.
   *
   * A value inside the final interval is chosen so that the decoder
   * will decode the same data whatever bits follow the end of the
   * stream (it will read the bytes behind the stream, which are not
   * zero). Therefore the bits are only dropped as long as they could
   * change the value by less than half of the interval.
   */
  std::vector<uint8_t> finish() {
    int k = 0;
    while((2u << k) <= range) {
      ++k;
    }
    const int kk = k > 0 ? k - 1 : 0; // Free bits: [v, v+2^kk) is inside the interval.
    add((1u << kk) - 1); // Round up to a multiple of 2^kk.
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

//! Decoder, mirrors the 6502 routine.
class Decoder {
  const std::vector<uint8_t> &d;
  size_t pos = 0; // bit position
  unsigned range = 0xFFFF, code = 0;
  int padding; // Value of the bits behind the end of the stream.
  int getbit() {
    const int b = pos / 8 < d.size() ? (d[pos / 8] >> (7 - pos % 8)) & 1 : padding;
    ++pos;
    return b;
  }
public:
  Decoder(const std::vector<uint8_t> &s, size_t first, int pad = 0) : d(s), pos(first * 8), padding(pad) {
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

//! A token: literal byte (pair false) or pair index.
struct Token {
  bool pair;
  int value;
};

//! Bits of the pair tree, the same for any number of pairs to keep the decoder small.
int pair_bits(int) {
  return 5;
}

//! Position-inside-the-instruction state.
struct PosState {
  int pos = 0, len = 1;
  void advance(uint8_t b) {
    if(pos == 0) {
      len = squz_oplen(b);
    }
    if(++pos >= len) {
      pos = 0;
    }
  }
};

void encode_token(Encoder &enc, Model &m, int ctx, int k, const Token &t) {
  auto bit = [&](uint8_t &p, int b) {
    enc.encode(b, p);
    Model::update(p, b);
  };
  bit(m.pair[ctx][0], t.pair);
  if(!t.pair) {
    int node = 1;
    for(int i = 7; i >= 0; --i) {
      const int b = (t.value >> i) & 1;
      bit(m.lit[ctx][node], b);
      node = node * 2 + b;
    }
  } else {
    int node = 1;
    for(int i = k - 1; i >= 0; --i) {
      const int b = (t.value >> i) & 1;
      bit(m.pair[ctx][node], b);
      node = node * 2 + b;
    }
  }
}

Token decode_token(Decoder &dec, Model &m, int ctx, int k) {
  auto bit = [&](uint8_t &p) {
    const int b = dec.decode(p);
    Model::update(p, b);
    return b;
  };
  Token t;
  t.pair = bit(m.pair[ctx][0]);
  int node = 1;
  if(!t.pair) {
    for(int i = 0; i < 8; ++i) {
      node = node * 2 + bit(m.lit[ctx][node]);
    }
    t.value = node & 255;
  } else {
    for(int i = 0; i < k; ++i) {
      node = node * 2 + bit(m.pair[ctx][node]);
    }
    t.value = node - (1 << k);
  }
  return t;
}

struct PairDef {
  Token left, right;
  int depth;
};

//! Encode the dictionary and the tokens.
std::vector<uint8_t> encode_stream(const std::vector<PairDef> &dict, const std::vector<int> &seq,
				   const std::vector<std::vector<uint8_t>> &expansion) {
  const int n = dict.size();
  const int k = pair_bits(n);
  Model m;
  Encoder enc;
  for(const auto &p : dict) {
    encode_token(enc, m, 0, k, p.left);
    encode_token(enc, m, 0, k, p.right);
  }
  PosState ps;
  for(int t : seq) {
    encode_token(enc, m, ps.pos, k, t < 256 ? Token{false, t} : Token{true, t - 256});
    for(uint8_t b : expansion[t]) {
      ps.advance(b);
    }
  }
  encode_token(enc, m, ps.pos, k, Token{true, n});
  std::vector<uint8_t> out;
  out.push_back(n);
  const auto body = enc.finish();
  out.insert(out.end(), body.begin(), body.end());
  return out;
}

} // namespace

std::vector<uint8_t> decrunch_squz(const std::vector<uint8_t> &stream, int *maxdepth, long *lead, int padding) {
  if(stream.empty()) {
    throw std::runtime_error("squz: empty stream");
  }
  const int n = stream[0];
  if(n > SQUZ_MAX_PAIRS) {
    throw std::runtime_error("squz: bad header");
  }
  const int k = pair_bits(n);
  Model m;
  Decoder dec(stream, 1, padding);
  std::array<Token, 32> left, right;
  for(int i = 0; i < n; ++i) {
    left[i] = decode_token(dec, m, 0, k);
    right[i] = decode_token(dec, m, 0, k);
    if((left[i].pair && left[i].value >= i) || (right[i].pair && right[i].value >= i)) {
      throw std::runtime_error("squz: forward reference in dictionary");
    }
  }
  std::vector<uint8_t> out;
  PosState ps;
  int depthmax = 0;
  long leadmax = 0;
  for(;;) {
    Token t = decode_token(dec, m, ps.pos, k);
    if(t.pair && t.value == n) {
      break;
    }
    if(t.pair && t.value > n) {
      throw std::runtime_error("squz: bad pair index");
    }
    std::vector<Token> stack;
    for(;;) {
      while(t.pair) {
	stack.push_back(right[t.value]);
	t = left[t.value];
      }
      depthmax = std::max(depthmax, static_cast<int>(stack.size()));
      out.push_back(t.value);
      ps.advance(t.value);
      leadmax = std::max(leadmax, static_cast<long>(out.size()) - static_cast<long>(dec.bytes_read()));
      if(stack.empty()) {
	break;
      }
      t = stack.back();
      stack.pop_back();
    }
  }
  if(maxdepth) {
    *maxdepth = depthmax;
  }
  if(lead) {
    *lead = leadmax;
  }
  return out;
}

std::vector<uint8_t> crunch_squz(const Data &data) {
  const auto &in = data.get_dataref();
  std::vector<int> seq(in.begin(), in.end());
  std::vector<std::vector<uint8_t>> expansion(256);
  for(int i = 0; i < 256; ++i) {
    expansion[i] = {static_cast<uint8_t>(i)};
  }
  std::vector<PairDef> dict;
  std::vector<int> depth(256, 0);
  std::vector<uint8_t> best = encode_stream(dict, seq, expansion);
  size_t bestn = 0;
  while(dict.size() < SQUZ_MAX_PAIRS) {
    // Most frequent non-overlapping pair of tokens which is not too deep.
    std::unordered_map<long, int> counts;
    bool prev = false;
    for(size_t i = 0; i + 1 < seq.size(); ++i) {
      const long key = static_cast<long>(seq[i]) * 1000 + seq[i + 1];
      if(seq[i] == seq[i + 1] && prev) {
	prev = false;
	continue;
      }
      ++counts[key];
      prev = (seq[i] == seq[i + 1]);
    }
    long bestkey = -1;
    int bestcount = 2;
    for(const auto &kv : counts) {
      const int a = kv.first / 1000, b = kv.first % 1000;
      if(1 + std::max(depth[a], depth[b]) > SQUZ_MAX_DEPTH) {
	continue;
      }
      if(kv.second > bestcount || (kv.second == bestcount && bestkey >= 0 && kv.first < bestkey)) {
	bestcount = kv.second;
	bestkey = kv.first;
      }
    }
    if(bestkey < 0) {
      break;
    }
    const int a = bestkey / 1000, b = bestkey % 1000;
    const int code = 256 + dict.size();
    auto tok = [](int t) { return t < 256 ? Token{false, t} : Token{true, t - 256}; };
    dict.push_back(PairDef{tok(a), tok(b), 1 + std::max(depth[a], depth[b])});
    depth.push_back(dict.back().depth);
    expansion.push_back(expansion[a]);
    expansion.back().insert(expansion.back().end(), expansion[b].begin(), expansion[b].end());
    std::vector<int> merged;
    merged.reserve(seq.size());
    for(size_t i = 0; i < seq.size(); ++i) {
      if(i + 1 < seq.size() && seq[i] == a && seq[i + 1] == b) {
	merged.push_back(code);
	++i;
      } else {
	merged.push_back(seq[i]);
      }
    }
    seq.swap(merged);
    // The number of pairs is chosen by the real size.
    auto candidate = encode_stream(dict, seq, expansion);
    if(candidate.size() < best.size()) {
      best.swap(candidate);
      bestn = dict.size();
    }
  }
  std::cout << "SQUZ: " << bestn << " pairs\n";
  // The bytes behind the stream are unknown, the result must not depend on them.
  for(int padding = 0; padding < 2; ++padding) {
    const auto check = decrunch_squz(best, nullptr, nullptr, padding);
    if(check.size() != in.size() || !std::equal(check.begin(), check.end(), in.begin())) {
      throw std::logic_error("squz: decrunched data differs from the input");
    }
  }
  return best;
}


Compressor::crunched_data_type SquzCompressor::compress() {
  auto crunched = crunch_squz(data);
  int depth;
  long lead;
  decrunch_squz(crunched, &depth, &lead);
  std::cout << "SQUZ: nesting depth " << depth << ", lead " << lead << " bytes\n";
  if(!cliargs.raw_flag) {
    // The decoder lives at $0100-$01CA and $0334-$03FE, the tables at $0400-$07DF.
    const unsigned load = data.get_loadaddr();
    const unsigned loadend = load + data.size();
    const unsigned instart = (static_cast<unsigned>(cliargs.page_arg) << 8) - crunched.size();
    if(load < 0x0800 && loadend > 0x0100) {
      throw std::runtime_error("squz: the data would overwrite the decoder or the tables ($0100-$07FF)");
    }
    if(instart < 0x0800) {
      throw std::runtime_error("squz: compressed data would be moved into $0100-$07FF, use a higher page");
    }
    if(static_cast<long>(instart) - static_cast<long>(load) < lead) {
      throw std::runtime_error("squz: the output would overtake the compressed data, use a higher page");
    }
  }
  return crunched;
}


std::ostream &write_squz_stub(std::ostream &out, uint16_t size, uint16_t loadaddr, uint16_t jmp, uint8_t pagehi) {
  // Positions from decrunchsquzstub.prg.label. Add 2 for the load address.
  const int POS_OF_END_OF_CDATA = 0x42 + 2;
  const int POS_OF_PAGEHI = 0x41 + 2; // High page +1.
  const int POS_OF_JMP = 0xC4 + 2;
  const int POS_OF_DEST_LOW = 0x33 + 2;
  const int POS_OF_DEST_HIGH = 0x37 + 2;
  std::vector<uint8_t> stub(decrunchsquzstub_prg, decrunchsquzstub_prg + decrunchsquzstub_prg_len);

  // The stub contains $1000 as the page limit and $033C as the destination and jump address. If
  // they are not found the positions above are out of date.
  if(stub.at(POS_OF_PAGEHI) != 0x10 ||
     (stub.at(POS_OF_JMP) | (stub.at(POS_OF_JMP + 1) << 8)) != 0x033C ||
     stub.at(POS_OF_DEST_LOW) != 0x3C || stub.at(POS_OF_DEST_HIGH) != 0x03) {
    throw std::logic_error("squz: stub patch positions are out of date, see decrunchsquzstub.prg.label");
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


void SquzCompressor::write_stub(std::ofstream &out, const std::vector<uint8_t> &c,
				uint16_t load, uint16_t jmp) {
  write_squz_stub(out, c.size(), load, jmp, cliargs.page_arg);
}
