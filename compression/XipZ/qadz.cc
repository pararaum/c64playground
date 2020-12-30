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
    void puttoken(long pos, long len) { 
      flush();
      out.push_back(static_cast<uint8_t>(-len));
      out.push_back(static_cast<uint8_t>(-pos));
    }
  } outclass;

  //Todo: Fix this! No look back at the beginning! This will fail if
  //there are a lot of zeros in the 6502 decompressor!
  for(long pos = 0; pos < data.size(); ++pos) {
    long posi;
    unsigned long match = 0;
    // Move backwards from the current position.
    for(int cmpj = LOOK_BACK; cmpj >= 0; --cmpj) {
      // Inner loop for comparison.
      for(int cmpi = 0; cmpi < MAX_LEN; ++cmpi) {
	if(pos - cmpj + cmpi < 0) {
	  // We are out of bounds.
	  break;
	}
	if(pos + cmpi >= data.size()) {
	  // We are out of bounds.
	  break;
	}
	if(data[pos - cmpj + cmpi] != data[pos + cmpi]) {
	  break;
	}
	std::cout << format("pos=%lu '%c' cmpi=%lu cmpj=%lu match=%lu\n") % pos % data[pos] % cmpi % cmpj % match;
	if(cmpi > match) {
	  posi = cmpj - LOOK_BACK;
	  match = cmpi;
	}
      }
    }
    if(match < 3) {
      outclass.putc(data[pos]);
    } else {
      outclass.puttoken(posi, match);
      //std::cout << format("[pos=%ld, posi=%ld, match=%ld]") % pos % posi % match;
      pos += match - 1;
    }
  }
  return outclass.out;
}
