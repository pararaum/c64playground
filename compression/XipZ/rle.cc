#include "data.hh"
#include "compression.hh"
#include <fstream>

Compressor::crunched_data_type RleCompressor::compress() {
  crunched_data_type crunched;

  if(data.size() < 2) {
    throw std::out_of_range("less than two bytes for RLE");
  }
  auto dataend = data.end();
  for(auto current = data.begin(); current != dataend; ++current) {
    auto nextiter = std::next(current);
    unsigned count = 0; // Number of run minus one! First element is current.
    while(nextiter < dataend) {
      if(*current == *nextiter) {
	++count;
      } else {
	break;
      }
      ++nextiter;
    }
    if(count == 0) { // No duplicates
      crunched.push_back(*current);
    } else {
      while(count > 255) {
	crunched.push_back(*current);
	crunched.push_back(*current);
	crunched.push_back(255);
	count -= 256;
      }
      if(count > 0) { // It may be zero after the output of run greater than 255.
	crunched.push_back(*current);
	crunched.push_back(*current);
	crunched.push_back(count);
      }
      current = nextiter;
      if(current == dataend) { // The run ended et EOF.
	break;
      }
    }
  }
  auto last_element = crunched.back();
  if(last_element != 0) {
    crunched.push_back(0);
    crunched.push_back(0);
  } else {
    crunched.push_back(0xff);
    crunched.push_back(0xff);
  }
  crunched.push_back(0);
  return crunched;
}


void RleCompressor::write_stub(std::ofstream&, const std::vector<uint8_t>&,
			       uint16_t, uint16_t) {
  throw std::logic_error("no stub for RLE");
}


