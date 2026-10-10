#include <algorithm>
#include <cstring>
#include <iterator>
#include <stdexcept>
#include <vector>
#include "data.hh"
#include "ratz.hh"
#include "decrunchratzstub.inc"

/*! \file
 *
 * \brief Parser and reference decoder for ratz, see \ref ratz.hh.
 *
 * Forward dynamic programming over the positions with costs in
 * bytes. A match costs 1 (last offset), 2 (near) or 3 (far) bytes
 * whatever its length, a literal run of k bytes costs 1 + k. As the
 * last offset is part of the state, every position keeps the K
 * cheapest arrivals with different last offsets (a beam search). New
 * matches come from a byte-pair hash chain, keeping for each length
 * the nearest offset.
 */

namespace {

constexpr int MAX_LIT = 127;      //!< longest literal run
constexpr int REP_BASE = 0x80;    //!< control bytes of matches at the last offset
constexpr int REP_N = 32;         //!< lengths 1..32
constexpr int NEAR_BASE = 0xA0;   //!< control bytes of matches with a one byte offset
constexpr int NEAR_N = 64;        //!< lengths 2..65
constexpr int NEAR_MAX_OFF = 256;
constexpr int FAR_BASE = 0xE0;    //!< control bytes of matches with a two byte offset
constexpr int FAR_N = 32;         //!< lengths 3..34
constexpr int FAR_MAX_OFF = 65536;
constexpr int MAX_CHAIN = 8192;   //!< longest walk through a hash chain
constexpr int INF = 0x3fffffff;

enum TokenType : uint8_t { T_LIT, T_REP, T_NEAR, T_FAR };

//! One way to arrive at a position.
struct Arrival {
  int cost = INF;       //!< bytes needed to reach this position
  int off = 0;          //!< last offset, 0 = none yet
  int nlit = 0;         //!< length of the literal run ending here
  int from_pos = -1;    //!< previous position
  int from_slot = -1;   //!< arrival at the previous position
  int len = 0;          //!< bytes covered by the last token
  TokenType type = T_LIT;
};

class Parser {
public:
  Parser(const std::vector<uint8_t> &d, int beam)
    : d(d), n(static_cast<int>(d.size())), K(beam), arr(static_cast<size_t>(n + 1) * beam) {}

  std::vector<uint8_t> run();

private:
  const std::vector<uint8_t> &d;
  int n;
  int K;
  std::vector<Arrival> arr;

  Arrival *slots(int pos) { return &arr[static_cast<size_t>(pos) * K]; }
  void insert(int pos, const Arrival &a);
  int matchlen(int i, int off, int cap) const;
};

/*! Offer an arrival to a position.
 *
 * Arrivals with the same last offset are the same state, only the
 * cheaper one is kept. The list stays sorted by cost.
 */
void Parser::insert(int pos, const Arrival &a) {
  Arrival *b = slots(pos);
  int i;
  for(i = 0; i < K && b[i].cost < INF; ++i) {
    if(b[i].off == a.off) {
      if(a.cost > b[i].cost || (a.cost == b[i].cost && a.nlit <= b[i].nlit)) {
	return;
      }
      std::memmove(b + i, b + i + 1, (K - 1 - i) * sizeof(Arrival));
      b[K - 1].cost = INF;
      break;
    }
  }
  if(b[K - 1].cost <= a.cost) {
    return; // Beam is full and this is not better.
  }
  for(i = K - 1; i > 0 && b[i - 1].cost > a.cost; --i) {
    b[i] = b[i - 1];
  }
  b[i] = a;
}

//! Length of the match between position i and i-off, at most cap bytes.
int Parser::matchlen(int i, int off, int cap) const {
  int l = 0;
  cap = std::min(cap, n - i);
  while(l < cap && d[i + l] == d[i + l - off]) {
    ++l;
  }
  return l;
}

std::vector<uint8_t> Parser::run() {
  // Byte-pair chains: prev[p] is the previous position with the same two bytes.
  std::vector<int> head(65536, -1), prev(n + 1, -1);
  for(int p = 0; p + 1 < n; ++p) {
    int h = d[p] << 8 | d[p + 1];
    prev[p] = head[h];
    head[h] = p;
  }

  slots(0)[0] = Arrival{0, 0, 0, -1, -1, 0, T_LIT};
  const int max_len = std::max(NEAR_N + 1, FAR_N + 2);
  std::vector<int> cand_off, cand_len;

  for(int i = 0; i < n; ++i) {
    Arrival *b = slots(i);
    if(b[0].cost >= INF) {
      continue;
    }
    for(int j = 0; j < K && b[j].cost < INF; ++j) {
      const Arrival s = b[j];
      // Literal: one byte, plus a control byte when a new run starts.
      Arrival lit{s.cost + 1 + (s.nlit % MAX_LIT == 0), s.off, s.nlit + 1, i, j, 1, T_LIT};
      insert(i + 1, lit);
      // Match at the last offset.
      if(s.off && i >= s.off) {
	int rl = matchlen(i, s.off, REP_N);
	for(int l = 1; l <= rl; ++l) {
	  insert(i + l, Arrival{s.cost + 1, s.off, 0, i, j, l, T_REP});
	}
      }
    }
    // New matches. Their cost does not depend on the state, so start from the cheapest arrival.
    cand_off.clear();
    cand_len.clear();
    int best = 1;
    int steps = 0;
    if(i + 1 < n) {
      for(int p = prev[i]; p >= 0 && steps < MAX_CHAIN; p = prev[p], ++steps) {
	int off = i - p;
	if(off > FAR_MAX_OFF) {
	  break;
	}
	int l = matchlen(i, off, max_len);
	if(l > best) {
	  cand_off.push_back(off);
	  cand_len.push_back(l);
	  best = l;
	  if(l >= max_len) {
	    break;
	  }
	}
      }
    }
    for(size_t c = 0; c < cand_off.size(); ++c) {
      int off = cand_off[c];
      int ml = cand_len[c];
      if(off <= NEAR_MAX_OFF) {
	for(int l = 2; l <= ml && l <= NEAR_N + 1; ++l) {
	  insert(i + l, Arrival{b[0].cost + 2, off, 0, i, 0, l, T_NEAR});
	}
      }
      for(int l = 3; l <= ml && l <= FAR_N + 2; ++l) {
	insert(i + l, Arrival{b[0].cost + 3, off, 0, i, 0, l, T_FAR});
      }
    }
  }

  // Backtrack the cheapest arrival at the end.
  std::vector<int> tpos, tslot;
  for(int p = n, s = 0; p > 0;) {
    const Arrival &a = slots(p)[s];
    tpos.push_back(p);
    tslot.push_back(s);
    s = a.from_slot;
    p = a.from_pos;
  }
  auto arrival = [&](int t) -> const Arrival & { return slots(tpos[t])[tslot[t]]; };

  std::vector<uint8_t> out;
  for(int t = static_cast<int>(tpos.size()) - 1; t >= 0;) {
    const Arrival &a = arrival(t);
    int start = tpos[t] - a.len;
    if(a.type == T_LIT) {
      // Gather consecutive literals into runs.
      int end = t;
      while(end >= 0 && arrival(end).type == T_LIT) {
	--end;
      }
      int len = t - end;
      int from = start;
      while(len > 0) {
	int k = std::min(len, MAX_LIT);
	out.push_back(static_cast<uint8_t>(k));
	out.insert(out.end(), d.begin() + from, d.begin() + from + k);
	from += k;
	len -= k;
      }
      t = end;
      continue;
    }
    if(a.type == T_REP) {
      out.push_back(static_cast<uint8_t>(REP_BASE + a.len - 1));
    } else if(a.type == T_NEAR) {
      out.push_back(static_cast<uint8_t>(NEAR_BASE + a.len - 2));
      out.push_back(static_cast<uint8_t>(a.off - 1));
    } else {
      out.push_back(static_cast<uint8_t>(FAR_BASE + a.len - 3));
      out.push_back(static_cast<uint8_t>((a.off - 1) & 0xFF));
      out.push_back(static_cast<uint8_t>((a.off - 1) >> 8));
    }
    --t;
  }
  out.push_back(0);
  return out;
}

} // namespace


std::vector<uint8_t> decrunch_ratz(const std::vector<uint8_t> &stream) {
  std::vector<uint8_t> out;
  size_t i = 0;
  int last = 0;

  auto next = [&]() -> int {
    if(i >= stream.size()) {
      throw std::runtime_error("ratz: stream ends without end token");
    }
    return stream[i++];
  };
  auto copy = [&](int off, int len) {
    if(off < 1 || static_cast<size_t>(off) > out.size()) {
      throw std::runtime_error("ratz: offset points before the start of the data");
    }
    for(int k = 0; k < len; ++k) {
      out.push_back(out[out.size() - off]);
    }
  };

  for(;;) {
    int b = next();
    if(b == 0) {
      return out;
    } else if(b <= MAX_LIT) {
      for(int k = 0; k < b; ++k) {
	out.push_back(static_cast<uint8_t>(next()));
      }
    } else if(b < NEAR_BASE) {
      copy(last, b - REP_BASE + 1);
    } else if(b < FAR_BASE) {
      last = next() + 1;
      copy(last, b - NEAR_BASE + 2);
    } else {
      int lo = next();
      last = (lo | next() << 8) + 1;
      copy(last, b - FAR_BASE + 3);
    }
  }
}


std::vector<uint8_t> crunch_ratz(const Data &data, int beam) {
  const std::vector<uint8_t> &raw = data.get_dataref();
  std::vector<uint8_t> out = Parser(raw, std::max(beam, 1)).run();
  if(decrunch_ratz(out) != raw) {
    throw std::logic_error("ratz: wrong data after decompression");
  }
  return out;
}


std::ostream &write_ratz_stub(std::ostream &out, uint16_t size, uint16_t loadaddr, uint16_t jmp, uint8_t pagehi) {
  // Positions from decrunchratzstub.prg.label. Add 2 for the load address.
  // al 000032 .stubendofcdata_offset
  // al 000031 .stubpageHI
  // al 000068 .stubjump_offset
  // al 000026 .stubdestination_offsetLO
  // al 00002A .stubdestination_offsetHI
  const int POS_OF_END_OF_CDATA = 0x32 + 2;
  const int POS_OF_PAGEHI = 0x31 + 2; // High page +1.
  const int POS_OF_JMP = 0x68 + 2;
  const int POS_OF_DEST_LOW = 0x26 + 2;
  const int POS_OF_DEST_HIGH = 0x2A + 2;
  // Create a local copy.
  std::vector<uint8_t> stub(decrunchratzstub_prg, decrunchratzstub_prg + decrunchratzstub_prg_len);

  // The stub contains $1000 as the page limit and $033C as the destination and jump address. If
  // they are not found the positions above are out of date.
  if(stub.at(POS_OF_PAGEHI) != 0x10 ||
     (stub.at(POS_OF_JMP) | (stub.at(POS_OF_JMP + 1) << 8)) != 0x033C ||
     stub.at(POS_OF_DEST_LOW) != 0x3C || stub.at(POS_OF_DEST_HIGH) != 0x03) {
    throw std::logic_error("ratz: stub patch positions are out of date, see decrunchratzstub.prg.label");
  }
  // Assign new end of compressed data, it must point to the byte after the data.
  unsigned endptr = stub.at(POS_OF_END_OF_CDATA) | (stub.at(POS_OF_END_OF_CDATA + 1) << 8);
  endptr += size;
  stub.at(POS_OF_END_OF_CDATA) = endptr & 0xFF;
  stub.at(POS_OF_END_OF_CDATA + 1) = (endptr >> 8) & 0xFF;
  // Set the maximal read position high-byte.
  stub.at(POS_OF_PAGEHI) = pagehi;
  // Assign the new jmp position.
  stub.at(POS_OF_JMP) = jmp & 0xFF;
  stub.at(POS_OF_JMP + 1) = (jmp >> 8) & 0xFF;
  // Assign destination address.
  stub.at(POS_OF_DEST_LOW) = loadaddr & 0xFF;
  stub.at(POS_OF_DEST_HIGH) = (loadaddr >> 8) & 0xFF;
  // Now copy the modified stub.
  std::copy(stub.begin(), stub.end(), std::ostream_iterator<unsigned char>(out));
  return out;
}
