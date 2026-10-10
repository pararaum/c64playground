#include "lzw.hh"
#include "compression.hh"
#include "decrunchlzwstub.inc"
#include <algorithm>
#include <cstdlib>
#include <cstring>
#include <fstream>
#include <iostream>
#include <iterator>
#include <stdexcept>

/*! \file
 *
 * \brief lzw, see lzw.hh for the format.
 *
 * The compressor simulates the decoder. A state of the search is the
 * position in the input and the dictionary (a ring of 254 entries),
 * all codes cost nine bits. The search runs layer by layer, layer t
 * holds the best states after t codes. At every state the longest
 * matches of the dictionary and the literal are tried (greedy parsing
 * is not optimal, as the choice changes the entries created later).
 * States which are further in the input are preferred. The whole search
 * is repeated for several intervals at which the dictionary is cleared
 * and the smallest result wins.
 */

namespace {
const int NENT = 254;		// Number of dictionary entries.
const int MAXLEN = 254;		// Longest string.
const int CODE_CLEAR = 0x1FE;
const int CODE_END = 0x1FF;

struct Entry {
  uint16_t start;		// Position of the string in the output.
  uint16_t len;
};

struct State {
  uint32_t pos = 0;		// Next input position.
  uint32_t ps = 0;		// Start of the previous string.
  uint32_t node = 0;		// Index of the code path.
  uint16_t since = 0;		// Codes since the last clear.
  uint16_t pl = 0;		// Length of the previous string, 0 = none.
  uint8_t next = 0;
  uint8_t nfill = 0;
  Entry ring[NENT];

  // Entry for the previous string plus the first byte of the current one.
  void add() {
    if(pl == 0) {
      return;
    }
    ring[next] = Entry{static_cast<uint16_t>(ps), static_cast<uint16_t>(pl + 1)};
    if(nfill < NENT) {
      ++nfill;
    }
    next = (next + 1) % NENT;
  }
};

struct Node {
  uint32_t parent;
  uint16_t code;
};

struct Cand {
  uint32_t parent;
  uint32_t newpos;
  uint16_t code;
  uint16_t len;
  uint16_t pl;
};

/*! Pack codes into the stream, the end code is added. */
std::vector<uint8_t> pack(std::vector<uint16_t> codes) {
  codes.push_back(CODE_END);
  std::vector<uint8_t> out;
  for(size_t i = 0; i < codes.size(); i += 8) {
    uint8_t hi = 0;
    const size_t n = std::min<size_t>(8, codes.size() - i);
    for(size_t j = 0; j < n; ++j) {
      hi |= ((codes[i + j] >> 8) & 1) << (7 - j);
    }
    out.push_back(hi);
    for(size_t j = 0; j < n; ++j) {
      out.push_back(codes[i + j] & 0xFF);
    }
  }
  return out;
}

/*! Beam search, returns the codes (without the end code). */
std::vector<uint16_t> search(const std::vector<uint8_t> &in, unsigned clearint, size_t beam, size_t maxcand) {
  const uint32_t N = in.size();
  std::vector<Node> nodes;
  nodes.push_back(Node{0, 0});
  std::vector<State> cur(1), nxt;
  while(true) {
    for(const auto &s : cur) {
      if(s.pos == N) {
	std::vector<uint16_t> codes;
	for(uint32_t n = s.node; n != 0; n = nodes[n].parent) {
	  codes.push_back(nodes[n].code);
	}
	std::reverse(codes.begin(), codes.end());
	return codes;
      }
    }
    std::vector<Cand> cands;
    for(uint32_t p = 0; p < cur.size(); ++p) {
      const State &s = cur[p];
      if(clearint && s.since >= clearint) {
	cands.push_back(Cand{p, s.pos, CODE_CLEAR, 0, 0});
	continue;
      }
      State t = s;
      t.add();
      // Longest matches, one per length.
      std::vector<Cand> mine;
      auto offer = [&](uint16_t code, uint16_t len) {
	for(auto &c : mine) {
	  if(c.len == len) {
	    return;
	  }
	}
	mine.push_back(Cand{p, s.pos + len, code, len, len});
	// Keep the longest ones, the literal is length one.
	std::sort(mine.begin(), mine.end(), [](const Cand &a, const Cand &b) { return a.len > b.len; });
	if(mine.size() > maxcand) {
	  mine.pop_back();
	}
      };
      offer(in[s.pos], 1);
      for(int j = 0; j < t.nfill; ++j) {
	const Entry &e = t.ring[j];
	if(e.len > MAXLEN || s.pos + e.len > N || in[e.start] != in[s.pos]) {
	  continue;
	}
	if(std::memcmp(&in[e.start], &in[s.pos], e.len) == 0) {
	  offer(256 + j, e.len);
	}
      }
      cands.insert(cands.end(), mine.begin(), mine.end());
    }
    std::sort(cands.begin(), cands.end(), [](const Cand &a, const Cand &b) {
      if(a.newpos != b.newpos) {
	return a.newpos > b.newpos;
      }
      return a.pl > b.pl;
    });
    if(cands.size() > beam) {
      cands.resize(beam);
    }
    nxt.clear();
    for(const auto &c : cands) {
      State ns = cur[c.parent];
      if(c.code == CODE_CLEAR) {
	ns.next = 0;
	ns.nfill = 0;
	ns.pl = 0;
	ns.since = 0;
      } else {
	ns.add();
	ns.ps = ns.pos;
	ns.pl = c.len;
	ns.pos += c.len;
	++ns.since;
      }
      nodes.push_back(Node{cur[c.parent].node, c.code});
      ns.node = nodes.size() - 1;
      nxt.push_back(ns);
    }
    cur.swap(nxt);
  }
}
}

std::vector<uint8_t> decrunch_lzw(const std::vector<uint8_t> &stream, long *lead) {
  std::vector<uint8_t> out;
  Entry ring[NENT] = {};
  unsigned nfill = 0, next = 0, ps = 0, pl = 0;
  size_t rd = 0;
  long leadmax = 0;
  auto getbyte = [&]() -> uint8_t {
    if(rd >= stream.size()) {
      throw std::runtime_error("lzw: stream too short");
    }
    return stream[rd++];
  };
  unsigned bits = 0;
  for(unsigned n = 0;; ++n) {
    if(n % 8 == 0) {
      bits = getbyte();
    }
    const unsigned code = getbyte() | (((bits >> (7 - n % 8)) & 1) << 8);
    if(code == CODE_END) {
      break;
    }
    if(code == CODE_CLEAR) {
      nfill = next = pl = 0;
      continue;
    }
    if(pl) {
      ring[next] = Entry{static_cast<uint16_t>(ps), static_cast<uint16_t>(pl + 1)};
      nfill = std::min<unsigned>(nfill + 1, NENT);
      next = (next + 1) % NENT;
    }
    unsigned start, len;
    if(code < 256) {
      out.push_back(code);
      start = out.size() - 1;
      len = 1;
    } else {
      const unsigned slot = code - 256;
      if(slot >= nfill) {
	throw std::runtime_error("lzw: unused dictionary entry");
      }
      start = ring[slot].start;
      len = ring[slot].len;
      if(len > MAXLEN || start > out.size()) {
	throw std::runtime_error("lzw: bad dictionary entry");
      }
      for(unsigned i = 0; i < len; ++i) {
	out.push_back(out.at(start + i));
      }
      start = out.size() - len;
    }
    ps = start;
    pl = len;
    leadmax = std::max(leadmax, static_cast<long>(out.size()) - static_cast<long>(rd));
  }
  if(lead) {
    *lead = leadmax;
  }
  return out;
}

std::vector<uint8_t> crunch_lzw(const Data &data) {
  const auto &in = data.get_dataref();
  if(in.size() > 0xFFFF) {
    throw std::runtime_error("lzw: too much data");
  }
  size_t beam = 16, maxcand = 4;
  if(const char *e = std::getenv("XIPZ_LZW_BEAM")) {
    beam = std::max(1, std::atoi(e));
  }
  if(const char *e = std::getenv("XIPZ_LZW_CAND")) {
    maxcand = std::max(1, std::atoi(e));
  }
  std::vector<uint8_t> best;
  unsigned bestint = 0;
  for(unsigned clearint : {0u, 250u, 300u, 400u, 600u, 1000u, 2000u}) {
    auto stream = pack(search(in, clearint, beam, maxcand));
    if(best.empty() || stream.size() < best.size()) {
      best.swap(stream);
      bestint = clearint;
    }
  }
  std::cout << "LZW: clear interval " << bestint << "\n";
  const auto check = decrunch_lzw(best);
  if(check.size() != in.size() || !std::equal(check.begin(), check.end(), in.begin())) {
    throw std::logic_error("lzw: decrunched data differs from the input");
  }
  return best;
}


Compressor::crunched_data_type LzwCompressor::compress() {
  auto crunched = crunch_lzw(data);
  long lead;
  decrunch_lzw(crunched, &lead);
  std::cout << "LZW: lead " << lead << " bytes\n";
  if(!cliargs.raw_flag) {
    // The decoder lives at $0334-$03xx, the tables at $0400-$07FF, the stack is used a little.
    const unsigned load = data.get_loadaddr();
    const unsigned loadend = load + data.size();
    const unsigned instart = (static_cast<unsigned>(cliargs.page_arg) << 8) - crunched.size();
    if(load < 0x0800 && loadend > 0x01F0) {
      throw std::runtime_error("lzw: the data would overwrite the decoder or the tables ($01F0-$07FF)");
    }
    if(instart < 0x0800) {
      throw std::runtime_error("lzw: compressed data would be moved into $0100-$07FF, use a higher page");
    }
    if(static_cast<long>(instart) - static_cast<long>(load) < lead) {
      throw std::runtime_error("lzw: the output would overtake the compressed data, use a higher page");
    }
  }
  return crunched;
}


std::ostream &write_lzw_stub(std::ostream &out, uint16_t size, uint16_t loadaddr, uint16_t jmp, uint8_t pagehi) {
  // Positions from decrunchlzwstub.prg.label. Add 2 for the load address.
  const int POS_OF_END_OF_CDATA = 0x37 + 2;
  const int POS_OF_PAGEHI = 0x36 + 2; // High page +1.
  const int POS_OF_JMP = 0xAE + 2;
  const int POS_OF_DEST_LOW = 0x28 + 2;
  const int POS_OF_DEST_HIGH = 0x2C + 2;
  std::vector<uint8_t> stub(decrunchlzwstub_prg, decrunchlzwstub_prg + decrunchlzwstub_prg_len);

  // The stub contains $1000 as the page limit and $033C as the destination and jump address. If
  // they are not found the positions above are out of date.
  if(stub.at(POS_OF_PAGEHI) != 0x10 ||
     (stub.at(POS_OF_JMP) | (stub.at(POS_OF_JMP + 1) << 8)) != 0x033C ||
     stub.at(POS_OF_DEST_LOW) != 0x3C || stub.at(POS_OF_DEST_HIGH) != 0x03) {
    throw std::logic_error("lzw: stub patch positions are out of date, see decrunchlzwstub.prg.label");
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


void LzwCompressor::write_stub(std::ofstream &out, const std::vector<uint8_t> &c,
			       uint16_t load, uint16_t jmp) {
  write_lzw_stub(out, c.size(), load, jmp, cliargs.page_arg);
}
