#include <algorithm>
#include <fstream>
#include <iostream>
#include <iterator>
#include <vector>
#include <boost/format.hpp>
#include <stdio.h>

#define LOOK_BACK 255
#define MAX_LEN 128
#define MAX_PLAIN_LEN 127

using namespace std;
using boost::format;
/*! \file xipz-qadc.cc
 *
 * An experimental alternative compression routine which should lead
 * to a very small decompression program.
 */


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

int crunch_main(int argc, char **argv) {
  vector<char> data(LOOK_BACK);
  ifstream in;
  istream *inptr; //Easier to handle stdin and file this way...
  struct Outclass {
    long out_bytes;
    vector<char> buf;

    Outclass() : out_bytes(0), buf(MAX_PLAIN_LEN) {}
    ~Outclass() { putc(0); flush(); }
    void flush() { 
      if(out_bytes > 0) {
	cout << static_cast<char>(out_bytes);
	copy(buf.begin(), buf.begin() + out_bytes, ostream_iterator<char>(cout));
	out_bytes = 0;
      }
    }
    void putc(char c) { buf[out_bytes++] = c; if(out_bytes == MAX_PLAIN_LEN) flush(); }
    void puttoken(long pos, long len) { 
      cout << static_cast<signed char>(-len) << static_cast<unsigned char>(-pos);
    }	   
  } outclass;

  switch(argc) {
  case 1:
    inptr = &cin;
    break;
  case 2:
    in.open(argv[1]);
    if(!in) { cerr << "Error opening file!\n"; return 2; }
    inptr = &in;
    break;
  default:
    cerr << "Usage: qadc [FILENAME]\n";
    return 1;
  }
  copy(istreambuf_iterator<char>(*inptr), istreambuf_iterator<char>(), back_inserter(data));
  //copy(data.begin(), data.end(), ostream_iterator<signed char>(cout));
  for(long pos = LOOK_BACK; pos < data.size(); ++pos) {
    long cmpi, cmpj, posi, match = 0;
    for(cmpj = 0; cmpj < LOOK_BACK; cmpj++) {
      for(cmpi = 0; cmpi < MAX_LEN; ++cmpi)
	if(pos + cmpi >= data.size() || cmpi + cmpj >= LOOK_BACK || data.at(pos - LOOK_BACK + cmpj + cmpi) != data.at(pos + cmpi)) break;
      //cout << format("cmpi=%ld\n") % cmpi;
      if(cmpi > match) {
	posi = cmpj - LOOK_BACK;
	match = cmpi;
      }
    }
    if(match < 3) {
      outclass.putc(data[pos]);
    } else {
      outclass.flush();
      outclass.puttoken(posi, match);
      //cout << format("[pos=%ld, posi=%ld, match=%ld]") % pos % posi % match;
      pos += match - 1;
    }
  }
  return 0;
}
