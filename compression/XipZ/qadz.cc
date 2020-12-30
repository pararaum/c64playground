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
 * Simpe LZ77 like compression routine. It is optimised to use while
 * bytes as using nybbles is quite expensinve on an architecture like
 * the 6502. This make LZ4 painful.
 */

#define LOOK_BACK 255
#define MAX_LEN 128
#define MAX_PLAIN_LEN 127

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


std::ostream &write_qadz_stub(std::ostream &out, uint16_t size, uint16_t loadaddr, uint16_t jmp) {
  // Create a local copy.
  std::vector<uint8_t> stub(decrunchqadzstub, decrunchqadzstub + decrunchqadzstub_len);
// 00000000  01 08 0a 08 02 03 9e 32  30 36 31 00 00 00 a2 08  |.......2061.....|
// 00000010  bd 31 08 95 58 ca 10 f8  20 bf a3 78 b9 3a 08 99  |.1..X... ..x.:..|
// 00000020  f7 00 c8 d0 f7 e6 59 a9  3c 85 26 a9 03 85 27 4c  |......Y.<.&...'L|
// 00000030  f7 00 00 10 a2 08 54 37  44 a2 08 a0 00 b1 58 30  |......T7D.....X0|
// 00000040  1d d0 03 4c 3c 03 aa a8  e6 58 d0 02 e6 59 b1 58  |...L<....X...Y.X|
// 00000050  91 26 88 10 f9 20 45 01  20 52 01 4c f7 00 49 ff  |.&... E. R.L..I.|
// 00000060  aa e8 c8 b1 58 8d 28 01  a5 26 38 e9 00 85 28 a5  |....X.(..&8...(.|
// 00000070  27 e9 00 85 29 8a a8 b1  28 91 26 88 10 f9 20 52  |'...)...(.&... R|
// 00000080  01 a2 02 20 45 01 4c f7  00 8a 18 65 58 85 58 a5  |... E.L....eX.X.|
// 00000090  59 69 00 85 59 60 8a 18  65 26 85 26 a5 27 69 00  |Yi..Y`..e&.&.'i.|
// 000000a0  85 27 60                                          |.'`|
  const int POS_OF_JMP = 0x44;
  const int POS_OF_END_OF_CDATA = 0x34;
  //const int POS_OF_BEGIN_OF_CDATA = 0x39;
  const int POS_OF_DEST_LOW = 0x28;
  const int POS_OF_DEST_HIGH = 0x2c;
  unsigned endptr = stub.at(POS_OF_END_OF_CDATA) | (stub.at(POS_OF_END_OF_CDATA + 1) << 8);
  // unsigned beginptr = stub.at(POS_OF_BEGIN_OF_CDATA) | (stub.at(POS_OF_BEGIN_OF_CDATA + 1) << 8);
  
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
  //stub.at(POS_OF_STOPREADING) = 0x10; // Todo: configurable!
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
    ~Outclass() {
      putc(0);
      flush();
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

  /* This is not the fastest nor the smartest way to perform the
     search. Improve in future! */
  for(long pos = 0; pos < datasize; ++pos) {
    long posi = LONG_MIN;
    int cmpi = INT_MIN;
    int cmpj;
    int max_look_back;
    int match = INT_MIN;
    // Move backwards from the current position.
    if(pos >= LOOK_BACK) {
      // There is enough space to do a full look back.
      max_look_back = LOOK_BACK;
    } else {
      // Only go back to the beginning.
      max_look_back = pos;
    }
#ifdef DEBUG
    std::cout << "pos: " << pos << " max_look_back: " << max_look_back << std::endl;
#endif
    for(cmpj = max_look_back; cmpj > 0; --cmpj) {
      // Inner loop for comparison.
      for(cmpi = 0; cmpi < (cmpj < MAX_LEN ? cmpj : MAX_LEN); ++cmpi) {
	if(pos + cmpi >= datasize) {
	  // End of data reached.
	  break;
	}
	if(data[pos - cmpj + cmpi] != data[pos + cmpi]) {
	  break;
	}
	//std::cout << format("pos=%lu '%c' cmpi=%d cmpj=%d\n") % pos % data[pos] % cmpi % cmpj;
      }
      //std::cout << format("cmpi=%d match=%lu → ") % cmpi % match;
      if(cmpi > match) {
	posi = cmpj;
	match = cmpi;
      }
      //std::cout << format("cmpi=%d match=%lu\n") % cmpi % match;
    }
    if(match < 3) {
      outclass.putc(data[pos]);
    } else {
      outclass.puttoken(posi, match);
      //std::cout << format("[pos=%ld, posi=%ld, match=%ld]") % pos % posi % match;
      pos += match - 1;
    }
  }
  outclass.flush();
  return outclass.out;
}
