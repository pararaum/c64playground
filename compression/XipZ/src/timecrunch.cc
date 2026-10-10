#include "timecrunch.hh"
#include <algorithm>
#include <cstdlib>
#include <iostream>
#include <stdexcept>

/*! \file
 *
 * \brief tc, see timecrunch.hh for the format.
 *
 * Crunching: the matches are found with hash chains over pairs of
 * bytes. A chain is walked from the nearest position outwards and
 * every candidate that is longer than all the nearer ones is a point of
 * the (length, nearest distance) frontier: this is all that is needed
 * as the cost of a match only depends on the length and on the size of
 * the offset, so for every length the nearest distance is the best.
 * A dynamic program over the positions then finds the cheapest parse.
 * It knows the rules of the format: a run of literals is always
 * followed by a match when decoding (so it follows a match when
 * looking at the file from the start), the costs of the three kinds of
 * runs and of the lengths.
 */

namespace {
const unsigned MAXLEN = 255;	// The length is a byte in the decoder: (w+1)+3 with w <= 251.
const unsigned MAXRUN = 255;
const unsigned MAXDIST = 65535 + MAXLEN; // offset 16 bits plus length.
const unsigned INF = 0x7FFFFFFF;
const unsigned ESCAPE_EXTRA = 12; // Estimate of the bits of an escape for another run in the parse.

/*! Matches at a position: for the lengths (previous maxlen, maxlen] the nearest distance is dist. */
struct Seg {
  uint16_t maxlen;
  uint32_t dist;
};

struct Matches {
  std::vector<uint32_t> first; // Index into segs for every position, one more.
  std::vector<Seg> segs;
};

Matches find_matches(const std::vector<uint8_t> &in, unsigned chain) {
  const size_t n = in.size();
  Matches m;
  m.first.assign(n + 1, 0);
  std::vector<int32_t> head(65536, -1), prev(n, -1);
  for(size_t i = 0; i < n; ++i) {
    m.first[i] = m.segs.size();
    if(i + 1 < n) {
      const unsigned h = in[i] | (in[i + 1] << 8);
      const unsigned cap = std::min<size_t>(MAXLEN, n - i);
      unsigned best = 1;
      unsigned depth = 0;
      for(int32_t j = head[h]; j >= 0 && depth < chain && best < cap; j = prev[j], ++depth) {
	const size_t d = i - j;
	if(d > MAXDIST) {
	  break;
	}
	if(d < 2) {
	  continue;
	}
	const unsigned lim = std::min<size_t>(cap, d); // The match must not overlap itself.
	if(lim <= best || in[i + best] != in[j + best]) {
	  continue;
	}
	const auto mm = std::mismatch(in.begin() + i, in.begin() + i + lim, in.begin() + j);
	const unsigned ml = mm.first - (in.begin() + i);
	if(ml > best) {
	  best = ml;
	  m.segs.push_back({static_cast<uint16_t>(ml), static_cast<uint32_t>(d)});
	}
      }
      prev[i] = head[h];
      head[h] = i;
    }
  }
  m.first[n] = m.segs.size();
  return m;
}

/*! Bits of a run of literals without the literal bytes. */
unsigned run_bits(unsigned m) {
  return m <= 5 ? 3 : m <= 21 ? 7 : 11;
}

/*! Bits of a match without the token (see the format), length 2 is always near. */
unsigned match_bits(unsigned len, bool far, int step) {
  const unsigned off = far ? 8 + step : 8;
  if(len == 2) {
    return 1 + 8;
  } else if(len == 3) {
    return 3 + off;
  } else if(len <= 19) {
    return 8 + off;
  } else {
    return 12 + off;
  }
}

struct Piece {
  bool lit;
  size_t start;
  unsigned len;
  unsigned dist;
};

/*! Cheapest parse for a STEP. Returns false if there is none. */
bool parse(const std::vector<uint8_t> &in, const Matches &mt, int step, std::vector<Piece> &out, unsigned long &bits) {
  const size_t n = in.size();
  std::vector<unsigned> E(n + 1, INF), T(n + 1, INF), A(n + 1, INF);
  std::vector<uint32_t> efrom(n + 1, 0);
  std::vector<uint16_t> elen(n + 1, 0), tm(n + 1, 0), am(n + 1, 0);
  std::vector<uint8_t> basee(n + 1, 0);
  const unsigned maxoff = (1u << (8 + step)) - 1;
  E[0] = 0; // The start, the first run of literals follows it.
  for(size_t i = 0; i <= n; ++i) {
    unsigned base = INF;
    if(i >= 1) {
      for(size_t m = 1; m <= std::min<size_t>(MAXRUN, i); ++m) {
	if(E[i - m] != INF) {
	  const unsigned c = E[i - m] + 8 * m + run_bits(m);
	  if(c < T[i]) {
	    T[i] = c;
	    tm[i] = m;
	  }
	}
      }
      A[i] = T[i];
      for(size_t m = 1; m <= std::min<size_t>(MAXRUN, i - 1); ++m) {
	if(A[i - m] != INF) {
	  const unsigned c = A[i - m] + 8 * m + run_bits(m) + ESCAPE_EXTRA;
	  if(c < A[i]) {
	    A[i] = c;
	    am[i] = m;
	  }
	}
      }
      // A match after a match needs a token of zero.
      const unsigned be = E[i] == INF ? INF : E[i] + 3;
      base = std::min(be, A[i]);
      basee[i] = be <= A[i];
    }
    if(i < n && base != INF) {
      unsigned len = 2;
      for(uint32_t s = mt.first[i]; s < mt.first[i + 1]; ++s) {
	for(; len <= mt.segs[s].maxlen; ++len) {
	  const unsigned d = mt.segs[s].dist;
	  const unsigned off = d - len; // d >= len is guaranteed.
	  if(off > maxoff || (len == 2 && off > 255)) {
	    continue;
	  }
	  const unsigned c = base + match_bits(len, off > 255, step);
	  if(c < E[i + len]) {
	    E[i + len] = c;
	    efrom[i + len] = i;
	    elen[i + len] = len;
	  }
	}
      }
    }
  }
  const unsigned bestE = E[n];
  if(std::min(bestE, A[n]) == INF) {
    return false;
  }
  bits = std::min(bestE, A[n]);
  std::vector<Piece> rev;
  size_t pos = n;
  bool inE = bestE <= A[n];
  while(pos > 0) {
    if(inE) {
      const size_t j = efrom[pos];
      const unsigned len = elen[pos];
      rev.push_back({false, j, len, 0});
      pos = j;
      inE = basee[pos];
    } else {
      const size_t end = pos;
      while(am[pos] > 0) {
	pos -= am[pos];
      }
      pos -= tm[pos];
      rev.push_back({true, pos, static_cast<unsigned>(end - pos), 0});
      inE = true;
    }
  }
  out.assign(rev.rbegin(), rev.rend());
  // Fill in the distances.
  for(auto &p : out) {
    if(!p.lit) {
      unsigned dist = 0;
      unsigned len = 2;
      for(uint32_t s = mt.first[p.start]; s < mt.first[p.start + 1]; ++s) {
	if(p.len <= mt.segs[s].maxlen && p.len >= len) {
	  dist = mt.segs[s].dist;
	  break;
	}
	len = mt.segs[s].maxlen + 1;
      }
      p.dist = dist;
    }
  }
  return true;
}

/*! The bit stream in the order of decoding, the bytes are fetched on demand. */
class Stream {
public:
  std::vector<uint8_t> bytes; // In the order of decoding, the first is at the highest address.
  void put(unsigned value, unsigned count) {
    for(unsigned i = count; i-- > 0; ) {
      if(left_ == 0) {
	cur_ = bytes.size();
	bytes.push_back(0);
	left_ = 8;
      }
      --left_;
      bytes[cur_] |= ((value >> i) & 1) << left_;
    }
  }
private:
  unsigned left_ = 0;	// Free bits in the byte that is being filled.
  size_t cur_ = 0;	// Index of this byte, literals may have been added behind it.
};

/*! Write the parse. The data and the pieces are in the order of decoding. */
TcResult emit(const std::vector<uint8_t> &rin, const std::vector<Piece> &pieces, int step) {
  Stream st;
  long minrw = 0; // Minimum of read bytes - written bytes.
  size_t written = 0;
  auto update = [&](unsigned count) {
    written += count;
    minrw = std::min(minrw, static_cast<long>(st.bytes.size()) - static_cast<long>(written));
  };
  for(size_t idx = 0; idx < pieces.size(); ++idx) {
    const Piece &p = pieces[idx];
    if(p.lit) {
      if(idx + 1 < pieces.size() && pieces[idx + 1].lit) {
	throw std::logic_error("tc: two runs of literals follow each other");
      }
      const unsigned k = (p.len + MAXRUN - 1) / MAXRUN;
      if(k > 1) {
	unsigned x = 0;
	while(((k - 1) >> (x + 1)) != 0) {
	  ++x;
	}
	if(x > 5) {
	  throw std::runtime_error("tc: too many literals without a match");
	}
	st.put(7, 3);
	st.put(250 + x, 8);
	st.put(k - 1, x + 1);
      }
      size_t pos = p.start;
      for(unsigned r = 0; r < k; ++r) {
	const unsigned n = r + 1 < k ? MAXRUN : p.len - MAXRUN * (k - 1);
	if(n <= 5) {
	  st.put(n, 3);
	} else if(n <= 21) {
	  st.put(6, 3);
	  st.put(n - 6, 4);
	} else {
	  st.put(7, 3);
	  st.put(n - 6, 8);
	}
	// The first byte of the run in the order of decoding is at the highest address.
	for(unsigned i = 0; i < n; ++i) {
	  st.bytes.push_back(rin[pos + i]);
	}
	pos += n;
	update(n);
      }
    } else {
      if(idx == 0 || !pieces[idx - 1].lit) {
	st.put(0, 3);
      }
      const unsigned len = p.len;
      const unsigned off = p.dist - len;
      const bool far = off > 255;
      if(len == 2) {
	st.put(1, 1);
	st.put(off, 8);
      } else {
	st.put(0, 1);
	if(len == 3) {
	  st.put(0, 1);
	} else {
	  st.put(1, 1);
	  if(len - 4 <= 15) {
	    st.put(0, 1);
	    st.put(len - 4, 4);
	  } else {
	    st.put(1, 1);
	    st.put(len - 4, 8);
	  }
	}
	st.put(far, 1);
	st.put(off, far ? 8 + step : 8);
      }
      update(len);
    }
  }
  TcResult r;
  r.data.assign(st.bytes.rbegin(), st.bytes.rend());
  r.step = step;
  r.gap = minrw;
  return r;
}
}

std::vector<uint8_t> decrunch_tc(const std::vector<uint8_t> &c, int step, long gap, size_t outsize) {
  const long plen = c.size();
  const long O = plen + outsize + 16; // Output start in the memory image.
  std::vector<uint8_t> mem(O + outsize + 16, 0xA5);
  const long P = O + static_cast<long>(outsize) - plen + gap;
  if(P < 0) {
    throw std::logic_error("tc: bad gap");
  }
  std::copy(c.begin(), c.end(), mem.begin() + P);
  long fc = P + plen - 1;
  long fe = O + static_cast<long>(outsize) - 1;
  unsigned fa = 0, fb = 0, f9 = 0;
  struct Exit {};
  const unsigned tab[3] = {3, 7, static_cast<unsigned>(step + 7)};
  auto readbits = [&](unsigned x) { // Reads x+1 bits.
    unsigned a = 0;
    for(;; --x) {
      if(fb == 0) {
	fa = mem.at(fc);
	fb = 8;
	const bool end = (fc == P - 1);
	--fc;
	if(end) {
	  throw Exit();
	}
      }
      a = (a << 1) | ((fa >> 7) & 1);
      fa = (fa << 1) & 0xFF;
      --fb;
      if(x == 0) {
	break;
      }
    }
    return a;
  };
  auto copy = [&](unsigned len, long src) { // src is the address of the byte below the first source byte.
    fe -= len;
    for(unsigned y = len; y >= 1; --y) {
      mem.at(fe + y) = mem.at(src + y);
    }
  };
  try {
    for(;;) {
      unsigned a = readbits(2);
      if(a != 0) {
	unsigned n;
	if(a < 6) {
	  n = a;
	} else {
	  unsigned v = readbits(tab[a & 1]);
	  n = v + 6;
	  if(n > 255) {
	    f9 = readbits(n - 256);
	    continue;
	  }
	}
	fc -= n;
	copy(n, fc);
      }
      if(f9 != 0) {
	--f9;
	continue;
      }
      unsigned len;
      long src;
      if(readbits(0)) {
	len = 2;
	unsigned off = readbits(tab[1]);
	src = fe + off;
	// fall to the copy below
      } else {
	if(readbits(0) == 0) {
	  len = 3;
	} else {
	  unsigned w = readbits(tab[readbits(0)]);
	  len = w + 4;
	  if(len > 255) {
	    throw std::logic_error("tc: the length overflows the byte of the decoder");
	  }
	}
	unsigned c2 = readbits(0);
	unsigned off = readbits(tab[c2 + 1]);
	src = fe + off;
      }
      copy(len, src);
    }
  } catch(const Exit &) {
  }
  if(fe != O - 1) {
    throw std::logic_error("tc: the decoder stopped early");
  }
  return std::vector<uint8_t>(mem.begin() + O, mem.begin() + O + outsize);
}

TcResult crunch_tc(const std::vector<uint8_t> &in, bool verbose) {
  if(in.empty()) {
    throw std::runtime_error("tc: no data");
  }
  if(in.size() > 0xFFFF) {
    throw std::runtime_error("tc: too much data");
  }
  unsigned chain = 4096;
  if(const char *e = std::getenv("XIPZ_TC_CHAIN")) {
    chain = std::max(1, std::atoi(e));
  }
  // The decoder works backwards and copies from the data it has written
  // before, so everything is done on the reversed data.
  const std::vector<uint8_t> rin(in.rbegin(), in.rend());
  const Matches mt = find_matches(rin, chain);
  // The longer offsets are only needed up to the largest offset that is used, all larger steps are worse.
  unsigned needed = 0;
  for(size_t i = 0; i < in.size(); ++i) {
    unsigned len = 2;
    for(uint32_t s = mt.first[i]; s < mt.first[i + 1]; ++s) {
      needed = std::max(needed, mt.segs[s].dist - len);
      len = mt.segs[s].maxlen + 1;
    }
  }
  int maxstep = 1;
  while(maxstep < 8 && (needed >> (8 + maxstep)) != 0) {
    ++maxstep;
  }
  bool have = false;
  TcResult best;
  for(int step = 1; step <= maxstep; ++step) {
    std::vector<Piece> pieces;
    unsigned long bits;
    if(!parse(rin, mt, step, pieces, bits)) {
      continue;
    }
    TcResult r = emit(rin, pieces, step);
    if(verbose && std::getenv("XIPZ_TC_DEBUG")) {
      for(const auto &p : pieces) {
	std::cout << (p.lit ? "L " : "M ") << p.start << " len " << p.len << " dist " << p.dist << "\n";
      }
    }
    if(verbose) {
      std::cout << "TC: step " << step << ": " << r.data.size() << " bytes, gap " << r.gap << "\n";
    }
    if(!have || r.data.size() < best.data.size()) {
      best = r;
      have = true;
    }
  }
  if(!have) {
    throw std::runtime_error("tc: no way to crunch the data");
  }
  const auto check = decrunch_tc(best.data, best.step, best.gap, in.size());
  if(check != in) {
    throw std::logic_error("tc: decrunched data differs from the input");
  }
  return best;
}
