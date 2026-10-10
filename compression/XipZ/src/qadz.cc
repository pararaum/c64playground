#include <algorithm>
#include <fstream>
#include <iostream>
#include <iterator>
#include <vector>
#include <boost/format.hpp>
#include <stdio.h>
#include <stdint.h>
#include <limits.h>
#include "data.hh"
#include "decrunchqadzstub.inc"

/*! \file
 *
 * \brief LZ77-like compression routine.
 * 
 * Simpe LZ77 like compression routine. It is optimised to use while
 * bytes as using nybbles is quite expensinve on an architecture like
 * the 6502. This makes LZ4 painful.
 */

#define LOOK_BACK 255
#define MAX_LEN 128
#define MAX_PLAIN_LEN 127
//! Shortest match to use, the stub also copes with matches of two bytes.
#define MIN_MATCH_LEN 2

using namespace std;
using boost::format;

int decrunch_main(int argc, char **argv) {
  FILE *inpf;
  int c, i;
  char buf[1024];
  long bufidx = 0;

  switch(argc) {
  case 1:
    inpf = stdin;
    break;
  case 2:
    if((inpf = fopen(argv[1], "r")) == NULL) {
      perror("Can not open file");
      return 2;
    }
    break;
  default:
    fprintf(stderr, "Usage: qadd [FILENAME]\n");
    return 1;
  }
  while((c = fgetc(inpf)) != EOF) {
    signed char code = c;
    if(code > 0) {
      while(code-- > 0) {
	c = fgetc(inpf);
	buf[bufidx++ % sizeof(buf)] = c;
	putchar(c);
      }
    } else if(code < 0) {
      unsigned char pos = fgetc(inpf);
      for(i = code; i < 0; ++i) {
	c = buf[(bufidx - pos) % sizeof(buf)];
	putchar(c);
	buf[bufidx++ % sizeof(buf)] = c;
      }
    } else
      break;
  }
  if(inpf != stdin) if(fclose(inpf) == EOF) perror("Error closing file");
  return 0;
}


std::ostream &write_qadz_stub(std::ostream &out, uint16_t size, uint16_t loadaddr, uint16_t jmp, uint8_t pagehi) {
  // Create a local copy.
  std::vector<uint8_t> stub(decrunchqadzstub_prg, decrunchqadzstub_prg + decrunchqadzstub_prg_len);
// 00000000  01 08 0a 08 02 03 9e 32  30 36 31 00 00 00 a2 08  |.......2061.....|
// 00000010  bd 31 08 95 58 ca 10 f8  20 bf a3 78 b9 3a 08 99  |.1..X... ..x.:..|
// 00000020  f7 00 c8 d0 f7 e6 59 a9  3c 85 26 a9 03 85 27 4c  |......Y.<.&...'L|
// 00000030  f7 00 00 10 a7 08 54 37  44 a7 08 a0 00 b1 58 30  |......T7D.....X0|
// 00000040  1d d0 03 4c 3c 03 aa a8  e6 58 d0 02 e6 59 b1 58  |...L<....X...Y.X|
// 00000050  91 26 88 10 f9 20 4a 01  20 57 01 4c f7 00 49 ff  |.&... J. W.L..I.|
// 00000060  aa 48 e8 c8 b1 58 8d 29  01 a5 26 38 e9 00 85 28  |.H...X.)..&8...(|
// 00000070  a5 27 e9 00 85 29 a0 00  b1 28 91 26 c8 ca 10 f8  |.'...)...(.&....|
// 00000080  68 aa e8 20 57 01 a2 02  20 4a 01 4c f7 00 8a 18  |h.. W... J.L....|
// 00000090  65 58 85 58 a5 59 69 00  85 59 60 8a 18 65 26 85  |eX.X.Yi..Y`..e&.|
// 000000a0  26 a5 27 69 00 85 27 60                           |&.'i..'`|

// Remember to add 2 to accomodate for the load address.
// al 000037 .stubbeginofcdata_offset
// al 00002A .stubdestination_offsetHI
// al 000026 .stubdestination_offsetLO
// al 000032 .stubendofcdata_offset
// al 000042 .stubjump_offset
// al 000031 .stubpageHI
// al 000030 .stubpageLO
// al 000030 .stubparameters_offset

  const int POS_OF_JMP = 0x44;
  const int POS_OF_END_OF_CDATA = 0x34;
  const int POS_OF_DEST_LOW = 0x28;
  const int POS_OF_DEST_HIGH = 0x2c;
  unsigned endptr = stub.at(POS_OF_END_OF_CDATA) | (stub.at(POS_OF_END_OF_CDATA + 1) << 8);
  const int POS_OF_PAGEHI = 0x33; //High page +1.
  
  // Assign new end of compressed data.
  endptr += size; // Add number of bytes of compressed data.
  endptr += 1; // End pointer must point to the byte *after* the data.
  stub.at(POS_OF_END_OF_CDATA) = endptr & 0xFF;
  stub.at(POS_OF_END_OF_CDATA + 1) = (endptr >> 8) & 0xFF;
  // Assign the new jmp position.
  stub.at(POS_OF_JMP) = jmp & 0xFF;
  stub.at(POS_OF_JMP + 1) = (jmp >> 8) & 0xFF;
  // Assign destination address.
  stub.at(POS_OF_DEST_LOW) = loadaddr & 0xFF;
  stub.at(POS_OF_DEST_HIGH) = (loadaddr >> 8) & 0xFF;
  // Set the maximal read position high-byte.
  stub.at(POS_OF_PAGEHI) = pagehi; // Todo: configurable!
  // Now copy the modified stub.
  std::copy(stub.begin(), stub.end(), std::ostream_iterator<unsigned char>(out));
  return out;
}



std::vector<uint8_t> crunch_qadz(const Data &data) {
  long datasize = static_cast<long>(data.size());
  struct Outclass {
    std::vector<uint8_t> buf; //!< temporary buffer to collect plain tokens
    std::vector<uint8_t> out;

    Outclass() {}
    /*! finalise date and return output reference
     *
     */
    std::vector<uint8_t> &finalize() {
      flush();
      // Zero marks the end!
      out.push_back(0);
      return out;
    }
    void flush() { 
      if(!buf.empty()) {
	out.push_back(static_cast<uint8_t>(buf.size()));
	copy(buf.begin(), buf.end(), back_inserter(out));
	buf.clear();
      }
    }
    void putc(uint8_t c) {
      buf.push_back(c);
      if(buf.size() == MAX_PLAIN_LEN) {
	flush();
      }
    }
    void puttoken(int pos, int len) { 
      flush();
      out.push_back(static_cast<uint8_t>(-len));
      out.push_back(static_cast<uint8_t>(pos));
    }
  } outclass;

  /* Optimal parse. A match always costs two bytes whatever its offset
     and a literal run of k bytes costs 1+k bytes, so a shortest path
     over the positions gives the smallest possible stream. */

  // Longest match (length and offset) starting at each position.
  vector<int> matchlen(datasize, 0), matchoff(datasize, 0);
  for(long pos = 0; pos < datasize; ++pos) {
    for(int off = 1; off <= LOOK_BACK && off <= pos; ++off) {
      int len = 0;
      while(len < MAX_LEN && pos + len < datasize && data[pos + len] == data[pos + len - off]) {
	++len;
      }
      if(len > matchlen[pos]) {
	matchlen[pos] = len;
	matchoff[pos] = off;
      }
    }
  }

  // cost[i] = bytes needed for the first i bytes, step[i] = last token
  // (k > 0: literal run of k bytes, -l: match of length l).
  vector<long> cost(datasize + 1, LONG_MAX);
  vector<int> step(datasize + 1, 0);
  cost[0] = 0;
  for(long pos = 0; pos < datasize; ++pos) {
    for(int k = 1; k <= MAX_PLAIN_LEN && pos + k <= datasize; ++k) {
      if(cost[pos] + 1 + k < cost[pos + k]) {
	cost[pos + k] = cost[pos] + 1 + k;
	step[pos + k] = k;
      }
    }
    // Every prefix of the longest match is a match, too.
    for(int len = MIN_MATCH_LEN; len <= matchlen[pos]; ++len) {
      if(cost[pos] + 2 < cost[pos + len]) {
	cost[pos + len] = cost[pos] + 2;
	step[pos + len] = -len;
      }
    }
  }

  // Walk back from the end to collect the tokens, then emit them.
  vector<long> ends;
  for(long end = datasize; end > 0; end -= abs(step[end])) {
    ends.push_back(end);
  }
  for(auto it = ends.rbegin(); it != ends.rend(); ++it) {
    long end = *it;
    long begin = end - abs(step[end]);
    if(step[end] > 0) {
      for(long i = begin; i < end; ++i) {
	outclass.putc(data[i]);
      }
      outclass.flush();
    } else {
      outclass.puttoken(matchoff[begin], -step[end]);
    }
  }
  return outclass.finalize();
}
