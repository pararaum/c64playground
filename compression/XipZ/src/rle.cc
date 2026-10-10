#include "data.hh"
#include "compression.hh"
#include "decrunchrlestub.inc"
#include <fstream>
#include <iterator>

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
	if(count == 0) { // Exactly one byte of the run is left over.
	  crunched.push_back(*current);
	}
      }
      if(count > 0) { // It may be zero after the output of run greater than 255.
	crunched.push_back(*current);
	crunched.push_back(*current);
	crunched.push_back(count);
      }
      current = std::prev(nextiter); // The loop increment moves to the first byte after the run.
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




std::ostream &write_rle_stub(std::ostream &out, uint16_t size, uint16_t loadaddr, uint16_t jmp, uint8_t pagehi) {
  // Positions from decrunchrlestub.prg.label. Add 2 for the load address.
  // al 000032 .stubendofcdata_offset
  // al 000031 .stubpageHI
  // al 000080 .stubjump_offset
  // al 000026 .stubdestination_offsetLO
  // al 00002A .stubdestination_offsetHI
  const int POS_OF_END_OF_CDATA = 0x32 + 2;
  const int POS_OF_PAGEHI = 0x31 + 2; // High page +1.
  const int POS_OF_JMP = 0x80 + 2;
  const int POS_OF_DEST_LOW = 0x26 + 2;
  const int POS_OF_DEST_HIGH = 0x2A + 2;
  // Create a local copy.
  std::vector<uint8_t> stub(decrunchrlestub_prg, decrunchrlestub_prg + decrunchrlestub_prg_len);

  // The stub contains $1000 as the page limit and $033C as the destination and jump address. If
  // they are not found the positions above are out of date.
  if(stub.at(POS_OF_PAGEHI) != 0x10 ||
     (stub.at(POS_OF_JMP) | (stub.at(POS_OF_JMP + 1) << 8)) != 0x033C ||
     stub.at(POS_OF_DEST_LOW) != 0x3C || stub.at(POS_OF_DEST_HIGH) != 0x03) {
    throw std::logic_error("rle: stub patch positions are out of date, see decrunchrlestub.prg.label");
  }
  // Assign new end of compressed data, it must point to the byte after the data.
  unsigned endptr = stub.at(POS_OF_END_OF_CDATA) | (stub.at(POS_OF_END_OF_CDATA + 1) << 8);
  endptr += size;
  stub.at(POS_OF_END_OF_CDATA) = endptr & 0xFF;
  stub.at(POS_OF_END_OF_CDATA + 1) = (endptr >> 8) & 0xFF;
  stub.at(POS_OF_PAGEHI) = pagehi;
  stub.at(POS_OF_JMP) = jmp & 0xFF;
  stub.at(POS_OF_JMP + 1) = (jmp >> 8) & 0xFF;
  stub.at(POS_OF_DEST_LOW) = loadaddr & 0xFF;
  stub.at(POS_OF_DEST_HIGH) = (loadaddr >> 8) & 0xFF;
  std::copy(stub.begin(), stub.end(), std::ostream_iterator<unsigned char>(out));
  return out;
}


void RleCompressor::write_stub(std::ofstream &out, const std::vector<uint8_t> &c,
			       uint16_t load, uint16_t jmp) {
  write_rle_stub(out, c.size(), load, jmp, cliargs.page_arg);
}
