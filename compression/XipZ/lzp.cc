#include "data.hh"
#include "lzp.hh"
#include <iostream>
#include <array>
#include <ranges>
#include <list>

#define MODELSIZE 8
#define MAX_RUNLENGTH 255

std::vector<uint8_t> crunch_lzp(const Data &data) {
  std::array<uint8_t,1<<MODELSIZE> model; // This is the buffer and the hash table aka the model.
  std::list<uint8_t> output; // A list of output elements, a list is needed for the following trick to work: see mask.
  unsigned long hash = 0;
  unsigned int maskidx = 0;
  // Pushing the zero and getting a pointer to the value is a nice
  // trick as we can modify the mask when elements are later on added
  // without having to worry about splicing this values. But we have
  // to use a list as a vector invalidates references and therefore
  // pointers when the capacity is exhausted.
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

    while(runlength < MAX_RUNLENGTH) {
      if(pos + runlength < data.size()) {
	uint8_t byte = data[pos + runlength];
	hashfun(byte);
	if(byte != model[hash]) { // Still a match?
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
    } else if(pos < data.size()) {
      output.push_back(data[pos]);
      nextmask(0);
      model[hash] = data[pos];
      hashfun(data[pos]);
      ++pos;
    }
  }
  output.push_back(0); // Zero length is EOF.
  nextmask(1);
  // Use C++23 feature to return a vector instead of the list.
  // Not supported by my compiler version: return output | std::ranges::to<std::vector>();
  // Use range constructor instead:
  return std::vector<uint8_t>(output.begin(), output.end());
}


std::ostream &write_lzp_stub(std::ostream &out, uint16_t size, uint16_t loadaddr, uint16_t jmp) {
  return out;
}
