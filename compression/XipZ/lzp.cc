#include "data.hh"
#include "lzp.hh"
#include <iostream>
#include <array>
#include <ranges>

#define MODELSIZE 8

std::vector<uint8_t> crunch_lzp(const Data &data) {
  std::array<uint8_t,1<<MODELSIZE> model; // This is the buffer and the hash table aka the model.
  std::vector<uint8_t> output;
  unsigned long hash = 0;
  unsigned int maskidx = 0;
  output.push_back(0); // Add a mask, preinitialised with zeroes.
  uint8_t *mask = &output.back(); // And get a reference to our mask.
  auto hashfun = [&hash](uint8_t x) {
    hash = ((hash << 3) ^ x) % (1 << MODELSIZE);
  };
  auto nextmask = [&mask, &maskidx, &output](bool set1) {
    if(set1) {
      *mask |= 1 << maskidx;
    }
    if(++maskidx >= 8) {
      output.push_back(0);
      mask = &output.back();
      maskidx = 0;
    }
  };
    

  model.fill(0);
  for(unsigned long pos = 0; pos < data.size(); ) {
    unsigned int runlength = 0;

    while(runlength < 256) {
      if(pos + runlength < data.size()) {
	if(data[pos + runlength] == model[hash]) { // Still a match?
	  hashfun(data[pos + runlength]);
	} else {
	  break; // No go to next step.
	}
      } else {
	break;
      }
      ++runlength;
    }
    if(runlength > 0) {
      output.push_back(runlength);
      nextmask(1);
      pos += runlength;
    }
    if(pos < data.size()) {
      output.push_back(data[pos]);
      nextmask(0);
      model[hash] = data[pos];
      hashfun(data[pos]);
      ++pos;
    }
  }
  output.push_back(0); // Zero length is EOF.
  nextmask(1);
  return output;
}


std::ostream &write_lzp_stub(std::ostream &out, uint16_t size, uint16_t loadaddr, uint16_t jmp) {
  return out;
}
