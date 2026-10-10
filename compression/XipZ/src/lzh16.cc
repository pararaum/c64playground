#include "lzh16.hh"
#include "compression.hh"
#include "decrunchlzh16stub.inc"
#include <algorithm>
#include <cstdlib>
#include <fstream>
#include <iostream>
#include <iterator>
#include <stdexcept>

/*! \file
 *
 * \brief lzh16, see lzh16.hh for the format.
 *
 * The compressor is a dynamic program over the input positions. The
 * cost of a token depends on the history, so every position keeps the
 * cheapest few distinct histories (a beam). From each state the
 * literal, the matches with the offsets of the history and the matches
 * with new offsets are tried; for the new offsets only the nearest
 * offset for every attainable length is considered. A match is tried
 * with its longest length and with the longest length of every smaller
 * gamma bucket.
 */

namespace {
// Bits of a history slot, for experiments only: the stub and the library
// decoder are fixed to 16 entries (4 bits), so a stream made with another
// value can only be checked with the reference decoder. Measured on three
// programs 4 is about optimal (3 and 5 are a little worse on average, a
// bigger history costs a bit on every history match and gains little).
#ifndef LZH16_SLOTBITS
#define LZH16_SLOTBITS 4
#endif
const int SLOTBITS = LZH16_SLOTBITS;
const int NSLOT = 1 << SLOTBITS;	// Entries of the history.
const int MINLEN = 2;
const int MAXLEN = 256;
const int MAXDIST = 255 * 256;	// Longest distance: (254*256 + 255 + 1).
const unsigned LITBITS = 9;

struct Hist {
  uint16_t o[NSLOT];		// Distance - 1.
  uint8_t next;
  bool operator==(const Hist &) const = default;
};

struct Cand {
  uint32_t cost;
  Hist h;
  uint32_t ppos;		// Position and index of the parent state.
  uint16_t pidx;
  uint8_t kind;			// 0 literal, 1 history, 2 new offset.
  uint16_t len;
  uint16_t val;			// Literal byte, slot or distance - 1.
};

/*! Bits of the interleaved gamma code of v. */
unsigned gam(unsigned v) {
  unsigned k = 0;
  while(v >> (k + 1)) {
    ++k;
  }
  return 2 * k + 1;
}

class BitWriter {
  std::vector<uint8_t> out;
  unsigned nbits = 0;
public:
  void bit(unsigned b) {
    if(nbits % 8 == 0) {
      out.push_back(0);
    }
    out.back() |= (b & 1) << (7 - nbits % 8);
    ++nbits;
  }
  void bits(unsigned v, unsigned n) {
    while(n--) {
      bit(v >> n);
    }
  }
  void gamma(unsigned v) {
    unsigned k = 0;
    while(v >> (k + 1)) {
      ++k;
    }
    while(k--) {
      bit(1);
      bit(v >> k);
    }
    bit(0);
  }
  std::vector<uint8_t> result() { return out; }
};

/*! Lengths to try for a match that can be L bytes long. */
std::vector<unsigned> lengths(unsigned L) {
  std::vector<unsigned> r;
  for(unsigned l = MINLEN; l <= std::min(L, 8u); ++l) {
    r.push_back(l);
  }
  for(unsigned l = 16; l <= L; l *= 2) {
    r.push_back(l);
  }
  if(L > 8 && (r.empty() || r.back() != L)) {
    r.push_back(L);
  }
  return r;
}

/*! Keep the cheapest beam states with distinct histories. */
void prune(std::vector<Cand> &v, size_t beam) {
  std::stable_sort(v.begin(), v.end(), [](const Cand &a, const Cand &b) { return a.cost < b.cost; });
  std::vector<Cand> r;
  for(const auto &c : v) {
    bool dup = false;
    for(const auto &k : r) {
      if(k.h == c.h) {
	dup = true;
	break;
      }
    }
    if(!dup) {
      r.push_back(c);
      if(r.size() >= beam) {
	break;
      }
    }
  }
  v.swap(r);
}

std::vector<uint8_t> search(const std::vector<uint8_t> &in, size_t beam, unsigned chain) {
  const uint32_t N = in.size();
  std::vector<std::vector<Cand>> st(N + 1);
  st[0].push_back(Cand{0, Hist{}, 0, 0, 0, 0, 0});
  std::vector<int32_t> head(65536, -1), prev(N, -1);

  auto matchlen = [&](uint32_t p, uint32_t d) {
    const uint32_t maxl = std::min<uint32_t>(MAXLEN, N - p);
    uint32_t l = 0;
    while(l < maxl && in[p + l] == in[p + l - d]) {
      ++l;
    }
    return l;
  };
  auto add = [&](uint32_t q, const Cand &c) {
    st[q].push_back(c);
    if(st[q].size() > beam * 24) {
      prune(st[q], beam);
    }
  };

  for(uint32_t p = 0; p < N; ++p) {
    prune(st[p], beam);
    // Nearest offsets for increasing lengths.
    std::vector<std::pair<uint32_t, uint32_t>> pareto;
    if(p + 1 < N) {
      uint32_t best = 1;
      const uint32_t maxl = std::min<uint32_t>(MAXLEN, N - p);
      unsigned n = chain;
      for(int32_t q = head[in[p] | (in[p + 1] << 8)]; q >= 0 && n--; q = prev[q]) {
	const uint32_t d = p - q;
	if(d > MAXDIST) {
	  break;
	}
	const uint32_t l = matchlen(p, d);
	if(l > best) {
	  best = l;
	  pareto.emplace_back(d, l);
	  if(l == maxl) {
	    break;
	  }
	}
      }
    }
    for(uint32_t i = 0; i < st[p].size(); ++i) {
      const Cand s = st[p][i];
      add(p + 1, Cand{s.cost + LITBITS, s.h, p, static_cast<uint16_t>(i), 0, 1, in[p]});
      for(int j = 0; j < NSLOT; ++j) {
	const uint32_t d = s.h.o[j] + 1u;
	if(d > p) {
	  continue;
	}
	bool dup = false;
	for(int k = 0; k < j; ++k) {
	  dup |= s.h.o[k] == s.h.o[j];
	}
	if(dup) {
	  continue;
	}
	const uint32_t L = matchlen(p, d);
	if(L < MINLEN) {
	  continue;
	}
	for(unsigned len : lengths(L)) {
	  add(p + len, Cand{s.cost + 2 + SLOTBITS + gam(len - 1), s.h, p, static_cast<uint16_t>(i), 1,
	      static_cast<uint16_t>(len), static_cast<uint16_t>(j)});
	}
      }
      for(const auto &[d, L] : pareto) {
	Hist h = s.h;
	h.o[h.next] = d - 1;
	h.next = (h.next + 1) % NSLOT;
	const unsigned hi = (d - 1) >> 8;
	for(unsigned len : lengths(L)) {
	  add(p + len, Cand{s.cost + 2 + gam(hi + 1) + 8 + gam(len - 1), h, p, static_cast<uint16_t>(i), 2,
	      static_cast<uint16_t>(len), static_cast<uint16_t>(d - 1)});
	}
      }
    }
    if(p + 1 < N) {
      const unsigned key = in[p] | (in[p + 1] << 8);
      prev[p] = head[key];
      head[key] = p;
    }
  }
  prune(st[N], beam);
  // Backtrack from the cheapest final state.
  std::vector<const Cand *> tokens;
  uint32_t pos = N;
  uint32_t idx = 0;
  while(pos != 0) {
    const Cand &c = st[pos][idx];
    tokens.push_back(&c);
    pos = c.ppos;
    idx = c.pidx;
  }
  std::reverse(tokens.begin(), tokens.end());
  BitWriter w;
  for(const Cand *c : tokens) {
    switch(c->kind) {
    case 0:
      w.bit(0);
      w.bits(c->val, 8);
      break;
    case 1:
      w.bit(1);
      w.bit(1);
      w.bits(c->val, SLOTBITS);
      w.gamma(c->len - 1);
      break;
    default:
      w.bit(1);
      w.bit(0);
      w.gamma((c->val >> 8) + 1);
      w.bits(c->val & 0xFF, 8);
      w.gamma(c->len - 1);
      break;
    }
  }
  w.bit(1);			// End marker.
  w.bit(0);
  for(int i = 0; i < 8; ++i) {
    w.bit(1);
    w.bit(0);
  }
  return w.result();
}
}

std::vector<uint8_t> decrunch_lzh16(const std::vector<uint8_t> &stream, long *lead) {
  std::vector<uint8_t> out;
  size_t rd = 0;
  unsigned nbits = 0;
  long leadmax = 0;
  auto getbit = [&]() -> unsigned {
    if(rd >= stream.size()) {
      throw std::runtime_error("lzh16: stream too short");
    }
    const unsigned b = (stream[rd] >> (7 - nbits)) & 1;
    if(++nbits == 8) {
      nbits = 0;
      ++rd;
    }
    return b;
  };
  auto getbits = [&](unsigned n) {
    unsigned v = 0;
    while(n--) {
      v = (v << 1) | getbit();
    }
    return v;
  };
  // Returns false on overflow of the register.
  auto gamma = [&](unsigned &v) {
    v = 1;
    while(getbit()) {
      const unsigned b = getbit();
      if(v & 0x80) {
	return false;
      }
      v = (v << 1) | b;
    }
    return true;
  };
  unsigned hist[NSLOT] = {};
  unsigned next = 0;
  // The decoder reads a byte when it needs the first bit of it, so a started byte is consumed.
  auto updatelead = [&]() {
    leadmax = std::max(leadmax, static_cast<long>(out.size()) - static_cast<long>(rd) - (nbits ? 1 : 0));
  };
  for(;;) {
    if(!getbit()) {
      out.push_back(getbits(8));
      updatelead();
      continue;
    }
    unsigned slot, v;
    if(getbit()) {
      slot = getbits(SLOTBITS);
    } else {
      if(!gamma(v)) {
	break;
      }
      slot = next;
      next = (next + 1) % NSLOT;
      hist[slot] = ((v - 1) << 8) | getbits(8);
    }
    if(!gamma(v)) {
      throw std::runtime_error("lzh16: bad length");
    }
    const unsigned len = v + 1, dist = hist[slot] + 1;
    if(dist > out.size()) {
      throw std::runtime_error("lzh16: offset before the start of the data");
    }
    for(unsigned i = 0; i < len; ++i) {
      out.push_back(out[out.size() - dist]);
    }
    updatelead();
  }
  if(lead) {
    *lead = leadmax;
  }
  return out;
}

std::vector<uint8_t> crunch_lzh16(const Data &data) {
  const auto &in = data.get_dataref();
  if(in.size() > 0xFFFF) {
    throw std::runtime_error("lzh16: too much data");
  }
  size_t beam = 16;
  unsigned chain = 256;
  if(const char *e = std::getenv("XIPZ_LZH16_BEAM")) {
    beam = std::max(1, std::atoi(e));
  }
  if(const char *e = std::getenv("XIPZ_LZH16_CHAIN")) {
    chain = std::max(1, std::atoi(e));
  }
  auto stream = search(in, beam, chain);
  const auto check = decrunch_lzh16(stream);
  if(check.size() != in.size() || !std::equal(check.begin(), check.end(), in.begin())) {
    throw std::logic_error("lzh16: decrunched data differs from the input");
  }
  return stream;
}


Compressor::crunched_data_type Lzh16Compressor::compress() {
  auto crunched = crunch_lzh16(data);
  long lead;
  decrunch_lzh16(crunched, &lead);
  std::cout << "LZH16: lead " << lead << " bytes\n";
  if(!cliargs.raw_flag) {
    // The decoder lives at $0334-$03FF, the history at $0400-$041F, the stack is used a little.
    const unsigned load = data.get_loadaddr();
    const unsigned loadend = load + data.size();
    const unsigned instart = (static_cast<unsigned>(cliargs.page_arg) << 8) - crunched.size();
    if((load < 0x0200 && loadend > 0x01F0) || (load < 0x0420 && loadend > 0x0334)) {
      throw std::runtime_error("lzh16: the data would overwrite the decoder, the history or the stack ($01F0-$01FF, $0334-$041F)");
    }
    if(instart < 0x0420) {
      throw std::runtime_error("lzh16: compressed data would be moved into $0334-$041F, use a higher page");
    }
    if(static_cast<long>(instart) - static_cast<long>(load) < lead) {
      throw std::runtime_error("lzh16: the output would overtake the compressed data, use a higher page");
    }
  }
  return crunched;
}


std::ostream &write_lzh16_stub(std::ostream &out, uint16_t size, uint16_t loadaddr, uint16_t jmp, uint8_t pagehi) {
  // Positions from decrunchlzh16stub.prg.label. Add 2 for the load address.
  const int POS_OF_END_OF_CDATA = 0x37 + 2;
  const int POS_OF_PAGEHI = 0x36 + 2; // High page +1.
  const int POS_OF_JMP = 0xBD + 2;
  const int POS_OF_DEST_LOW = 0x28 + 2;
  const int POS_OF_DEST_HIGH = 0x2C + 2;
  std::vector<uint8_t> stub(decrunchlzh16stub_prg, decrunchlzh16stub_prg + decrunchlzh16stub_prg_len);

  // The stub contains $1000 as the page limit and $033C as the destination and jump address. If
  // they are not found the positions above are out of date.
  if(stub.at(POS_OF_PAGEHI) != 0x10 ||
     (stub.at(POS_OF_JMP) | (stub.at(POS_OF_JMP + 1) << 8)) != 0x033C ||
     stub.at(POS_OF_DEST_LOW) != 0x3C || stub.at(POS_OF_DEST_HIGH) != 0x03) {
    throw std::logic_error("lzh16: stub patch positions are out of date, see decrunchlzh16stub.prg.label");
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


void Lzh16Compressor::write_stub(std::ofstream &out, const std::vector<uint8_t> &c,
				 uint16_t load, uint16_t jmp) {
  write_lzh16_stub(out, c.size(), load, jmp, cliargs.page_arg);
}
