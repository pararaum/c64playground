#include "bpe.hh"
#include "compression.hh"
#include "decrunchbpestub.inc"
#include <algorithm>
#include <array>
#include <fstream>
#include <iostream>
#include <iterator>
#include <stdexcept>

/*! \file
 *
 * \brief Byte pair encoding, see bpe.hh for the format.
 */

//! Sequence elements >= this value are raw bytes (escaped), they are never merged.
static const int RAW = 256;
//! Address of the left table, the right table follows (the text screen).
static const unsigned BPE_TABLE = 0x0400;

namespace {
struct PairDef {
  uint8_t code, left, right;
};
}

std::vector<uint8_t> decrunch_bpe(const std::vector<uint8_t> &stream, long *lead, int *stack) {
  std::vector<uint8_t> out;
  std::array<uint8_t, 256> left, right;
  size_t pos = 0;
  long maxlead = 0;
  int maxstack = 0;
  auto get = [&]() -> uint8_t {
    if(pos >= stream.size()) {
      throw std::runtime_error("bpe: stream ends unexpectedly");
    }
    return stream[pos++];
  };
  for(int i = 0; i < 256; ++i) {
    left[i] = right[i] = i;
  }
  const uint8_t esc = get();
  const uint8_t end = get();
  for(int code = 0; code < 256; code += 8) {
    const uint8_t bitmap = get();
    for(int bit = 0; bit < 8; ++bit) {
      if(bitmap & (1 << bit)) {
	left[code + bit] = get();
	right[code + bit] = get();
      }
    }
  }
  auto note = [&]() { maxlead = std::max(maxlead, static_cast<long>(out.size()) - static_cast<long>(pos)); };
  for(;;) {
    uint8_t t = get();
    if(t == esc) {
      t = get();
      if(t == end) {
	break;
      }
      out.push_back(t);
      note();
      continue;
    }
    std::vector<uint8_t> pending;
    for(;;) {
      while(right[t] != t) {
	pending.push_back(right[t]);
	t = left[t];
	if(pending.size() > 1000) {
	  throw std::runtime_error("bpe: expansion does not terminate");
	}
      }
      out.push_back(t);
      note();
      // Stack: sentinel, return address of the put call and the pending bytes.
      maxstack = std::max(maxstack, static_cast<int>(pending.size()) + 3);
      if(pending.empty()) {
	break;
      }
      t = pending.back();
      pending.pop_back();
    }
  }
  if(lead) {
    *lead = maxlead;
  }
  if(stack) {
    *stack = maxstack;
  }
  return out;
}

std::vector<uint8_t> crunch_bpe(const Data &data) {
  const auto &in = data.get_dataref();
  std::array<unsigned long, 256> hist{};
  for(uint8_t b : in) {
    ++hist[b];
  }
  // The escape byte: an unused value is free, otherwise the rarest.
  int esc = -1;
  for(int v = 0; v < 256 && esc < 0; ++v) {
    if(hist[v] == 0) {
      esc = v;
    }
  }
  if(esc < 0) {
    esc = std::min_element(hist.begin(), hist.end()) - hist.begin();
  }
  std::array<bool, 256> literal{};  // Value is currently a literal token.
  std::array<bool, 256> operand{};  // Literal is used in a pair.
  std::array<int, 256> depth{};
  std::vector<int> freevals;       // Unused values which can be pair codes.
  int literals = 0;
  for(int v = 255; v >= 0; --v) {
    if(v == esc) {
      continue;
    }
    if(hist[v] == 0) {
      freevals.push_back(v);
    } else {
      literal[v] = true;
      ++literals;
    }
  }
  std::vector<int> seq;
  seq.reserve(in.size());
  for(uint8_t b : in) {
    seq.push_back(b == esc ? RAW + b : b);
  }
  std::vector<PairDef> pairs;
  std::vector<uint32_t> counts(65536, 0);
  std::vector<unsigned> touched;
  for(;;) {
    // Count the non-overlapping occurrences of every pair.
    touched.clear();
    bool prev = false;
    for(size_t i = 0; i + 1 < seq.size(); ++i) {
      const int a = seq[i], b = seq[i + 1];
      if(a >= RAW || b >= RAW) {
	prev = false;
	continue;
      }
      if(a == b && prev) {
	prev = false;
	continue;
      }
      const unsigned key = (a << 8) | b;
      if(counts[key]++ == 0) {
	touched.push_back(key);
      }
      prev = (a == b);
    }
    unsigned best = 0;
    uint32_t bestcount = 0;
    for(unsigned key : touched) {
      const int d = 1 + std::max(depth[key >> 8], depth[key & 255]);
      if(d <= BPE_MAX_DEPTH && (counts[key] > bestcount || (counts[key] == bestcount && key < best))) {
	best = key;
	bestcount = counts[key];
      }
    }
    for(unsigned key : touched) {
      counts[key] = 0;
    }
    if(bestcount <= BPE_PAIR_COST) {
      break;
    }
    const int a = best >> 8, b = best & 255;
    int code;
    if(!freevals.empty()) {
      code = freevals.back();
      freevals.pop_back();
    } else {
      // No free code: sacrifice the rarest literal, it is escaped from now on.
      int victim = -1;
      unsigned long victimcount = 0;
      std::array<unsigned long, 256> occ{};
      for(int e : seq) {
	if(e < RAW) {
	  ++occ[e];
	}
      }
      for(int v = 0; v < 256; ++v) {
	if(literal[v] && !operand[v] && v != a && v != b && (victim < 0 || occ[v] < victimcount)) {
	  victim = v;
	  victimcount = occ[v];
	}
      }
      if(victim < 0 || literals <= 2 || bestcount <= BPE_PAIR_COST + victimcount) {
	break;
      }
      for(int &e : seq) {
	if(e == victim) {
	  e = RAW + victim;
	}
      }
      literal[victim] = false;
      --literals;
      code = victim;
    }
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
    operand[a] = operand[b] = true;
    depth[code] = 1 + std::max(depth[a], depth[b]);
    pairs.push_back(PairDef{static_cast<uint8_t>(code), static_cast<uint8_t>(a), static_cast<uint8_t>(b)});
  }
  int endbyte = -1;
  for(int v = 0; v < 256 && endbyte < 0; ++v) {
    if(literal[v]) {
      endbyte = v;
    }
  }
  if(endbyte < 0) {
    throw std::logic_error("bpe: no literal left for the end marker");
  }
  std::vector<uint8_t> out;
  out.push_back(esc);
  out.push_back(endbyte);
  std::array<const PairDef *, 256> bycode{};
  for(const auto &p : pairs) {
    bycode[p.code] = &p;
  }
  for(int code = 0; code < 256; code += 8) {
    uint8_t bitmap = 0;
    for(int bit = 0; bit < 8; ++bit) {
      if(bycode[code + bit]) {
	bitmap |= 1 << bit;
      }
    }
    out.push_back(bitmap);
    for(int bit = 0; bit < 8; ++bit) {
      if(bycode[code + bit]) {
	out.push_back(bycode[code + bit]->left);
	out.push_back(bycode[code + bit]->right);
      }
    }
  }
  for(int e : seq) {
    if(e >= RAW) {
      out.push_back(esc);
      out.push_back(e - RAW);
    } else {
      out.push_back(e);
    }
  }
  out.push_back(esc);
  out.push_back(endbyte);
  std::cout << "BPE: " << pairs.size() << " pairs, escape $" << std::hex << esc << ", end $" << endbyte << std::dec
	    << ", " << literals << " literals, header " << (34 + pairs.size() * BPE_PAIR_COST) << " bytes\n";
  // Check the result.
  const auto check = decrunch_bpe(out);
  if(check.size() != in.size() || !std::equal(check.begin(), check.end(), in.begin())) {
    throw std::logic_error("bpe: decrunched data differs from the input");
  }
  return out;
}


Compressor::crunched_data_type BpeCompressor::compress() {
  auto crunched = crunch_bpe(data);
  long lead;
  int stack;
  decrunch_bpe(crunched, &lead, &stack);
  std::cout << "BPE: lead " << lead << " bytes, stack " << stack << " bytes\n";
  if(!cliargs.raw_flag) {
    // The decoder is placed in the stack page, so the stack must have room.
    if(stack > 0x100 - 0xB0) {
      throw std::runtime_error("bpe: expansion needs too much stack");
    }
    const unsigned load = data.get_loadaddr();
    const unsigned loadend = load + data.size();
    const unsigned instart = (static_cast<unsigned>(cliargs.page_arg) << 8) - crunched.size();
    const unsigned tableend = BPE_TABLE + 0x200;
    if(load < tableend && loadend > BPE_TABLE) {
      throw std::runtime_error("bpe: the data would overwrite the table in the text screen ($0400-$05FF)");
    }
    if(instart < tableend) {
      throw std::runtime_error("bpe: compressed data would be moved over the table in the text screen, use a higher page");
    }
    if(static_cast<long>(instart) - static_cast<long>(load) < lead) {
      throw std::runtime_error("bpe: the output would overtake the compressed data, use a higher page");
    }
  }
  return crunched;
}


std::ostream &write_bpe_stub(std::ostream &out, uint16_t size, uint16_t loadaddr, uint16_t jmp, uint8_t pagehi) {
  // Positions from decrunchbpestub.prg.label. Add 2 for the load address.
  const int POS_OF_END_OF_CDATA = 0x34 + 2;
  const int POS_OF_PAGEHI = 0x33 + 2; // High page +1.
  const int POS_OF_JMP = 0xB6 + 2;
  const int POS_OF_DEST_LOW = 0x28 + 2;
  const int POS_OF_DEST_HIGH = 0x2C + 2;
  std::vector<uint8_t> stub(decrunchbpestub_prg, decrunchbpestub_prg + decrunchbpestub_prg_len);

  // The stub contains $1000 as the page limit and $033C as the destination and jump address. If
  // they are not found the positions above are out of date.
  if(stub.at(POS_OF_PAGEHI) != 0x10 ||
     (stub.at(POS_OF_JMP) | (stub.at(POS_OF_JMP + 1) << 8)) != 0x033C ||
     stub.at(POS_OF_DEST_LOW) != 0x3C || stub.at(POS_OF_DEST_HIGH) != 0x03) {
    throw std::logic_error("bpe: stub patch positions are out of date, see decrunchbpestub.prg.label");
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


void BpeCompressor::write_stub(std::ofstream &out, const std::vector<uint8_t> &c,
			       uint16_t load, uint16_t jmp) {
  write_bpe_stub(out, c.size(), load, jmp, cliargs.page_arg);
}
