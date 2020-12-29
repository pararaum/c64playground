#include <algorithm>
#include <fstream>
#include <iostream>
#include <iterator>
#include <vector>
#include <boost/format.hpp>
#include <stdio.h>
#include "data.hh"

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

std::vector<uint8_t> crunch_qadz(const Data &data) {
  vector<uint8_t> lookback(LOOK_BACK);
  struct Outclass {
    long out_bytes;
    vector<char> buf;
    std::vector<uint8_t> out;

    Outclass() : out_bytes(0), buf(MAX_PLAIN_LEN) {}
    ~Outclass() {
      putc(0);
      flush();
    }
    void flush() { 
      if(out_bytes > 0) {
	out.push_back(static_cast<uint8_t>(out_bytes));
	copy(buf.begin(), buf.begin() + out_bytes, back_inserter(out));
	out_bytes = 0;
      }
    }
    void putc(char c) {
      buf[out_bytes++] = c;
      if(out_bytes == MAX_PLAIN_LEN) {
	flush();
      }
    }
    void puttoken(long pos, long len) { 
      out.push_back(static_cast<uint8_t>(-len));
      out.push_back(static_cast<uint8_t>(-pos));
    }	   
  } outclass;

  //Todo: Fix this! No look back at the beginning! This will fail if
  //there are a lot of zeros in the 6502 decompressor!
  std::copy(data.begin(), data.end(), std::back_inserter(lookback));
  for(unsigned long pos = LOOK_BACK; pos < lookback.size(); ++pos) {
    unsigned long cmpi, cmpj, posi, match = 0;
    for(cmpj = 0; cmpj < LOOK_BACK; cmpj++) {
      for(cmpi = 0; cmpi < MAX_LEN; ++cmpi)
	if(pos + cmpi >= lookback.size() || cmpi + cmpj >= LOOK_BACK || lookback[pos - LOOK_BACK + cmpj + cmpi] != lookback[pos + cmpi]) {
	  break;
	}
      //cout << format("cmpi=%ld\n") % cmpi;
      if(cmpi > match) {
	posi = cmpj - LOOK_BACK;
	match = cmpi;
      }
    }
    if(match < 3) {
      outclass.putc(lookback[pos]);
    } else {
      outclass.flush();
      outclass.puttoken(posi, match);
      //cout << format("[pos=%ld, posi=%ld, match=%ld]") % pos % posi % match;
      pos += match - 1;
    }
  }
  return outclass.out;
}
