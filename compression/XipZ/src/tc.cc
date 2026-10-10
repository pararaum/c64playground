#include "compression.hh"
#include "timecrunch.hh"
#include "decrunchtcstub.inc"
#include <algorithm>
#include <iostream>
#include <iterator>
#include <stdexcept>

/*! \file
 *
 * \brief The compressor class for tc, see timecrunch.hh for the format.
 *
 * The stub lives in the text screen ($0400-$0553, it is 339 bytes long
 * but two pages are copied). The packed data is moved to the bottom of
 * the area of the output, so the area below $0600 must not be used by
 * the output and by the moved data.
 */

namespace {
//! The first address that the output and the moved data may use.
const long LOWEST = 0x0600;
}

Compressor::crunched_data_type TcCompressor::compress() {
  const TcResult r = crunch_tc(data.get_dataref(), cliargs.verbose_flag);
  step_ = r.step;
  const long n = data.size();
  const long plen = r.data.size();
  std::cout << "TC: STEP " << r.step << ", the data is " << plen << " bytes\n";
  if(cliargs.raw_flag) {
    // The byte below the data is the STEP, the decoder reads it from there.
    std::vector<uint8_t> raw;
    raw.push_back(r.step);
    raw.insert(raw.end(), r.data.begin(), r.data.end());
    return raw;
  }
  const long load = data.get_loadaddr();
  if(load < LOWEST) {
    throw std::runtime_error("tc: the data would overwrite the decoder ($0400-$05FF), the load address must be $0600 or higher");
  }
  const long start = load + n - plen + r.gap; // Where the packed data is moved to.
  std::cout << "TC: the packed data is moved to $" << std::hex << start << std::dec
	    << " (" << (load - start) << " bytes below the load address)\n";
  if(start < LOWEST) {
    throw std::runtime_error("tc: the packed data would be moved into the area of the decoder ($0400-$05FF)");
  }
  if(start + plen > 0x10000 || load + n > 0x10000) {
    throw std::runtime_error("tc: the data does not fit into memory");
  }
  dest_ = start;
  return r.data;
}

void TcCompressor::write_stub(std::ofstream &out, const std::vector<uint8_t> &c,
			      uint16_t load, uint16_t jmp) {
  // Offsets from decrunchtcstub.prg.label (*_offset) plus two for the load address.
  const int POS_OF_JUMP_TO = 0x16D + 2;
  const int POS_OF_MVDSTLO = 0x2E + 2;
  const int POS_OF_MVDSTHI = 0x32 + 2;
  const int POS_OF_SRCLO = 0x9C + 2;
  const int POS_OF_SRCHI = 0xA0 + 2;
  const int POS_OF_DSTLO = 0xA4 + 2;
  const int POS_OF_DSTHI = 0xA8 + 2;
  const int POS_OF_ENDLO = 0x15F + 2;
  const int POS_OF_ENDHI = 0x165 + 2;
  const int POS_OF_STEP7 = 0x171 + 2;
  const int POS_OF_MVPAGES = 0x172 + 2;
  const int POS_OF_MVREST = 0x173 + 2;
  std::vector<uint8_t> stub(decrunchtcstub_prg, decrunchtcstub_prg + decrunchtcstub_prg_len);
  // Everything that is patched is zero in the stub, if not the positions are out of date.
  for(int pos : {POS_OF_MVDSTLO, POS_OF_MVDSTHI, POS_OF_SRCLO, POS_OF_SRCHI, POS_OF_DSTLO, POS_OF_DSTHI,
		 POS_OF_ENDLO, POS_OF_ENDHI, POS_OF_STEP7, POS_OF_MVPAGES, POS_OF_MVREST}) {
    if(stub.at(pos) != 0) {
      throw std::logic_error("tc: stub patch positions are out of date, see decrunchtcstub.prg.label");
    }
  }
  const unsigned plen = c.size();
  const unsigned start = dest_;
  const unsigned last = (start + plen - 1) & 0xFFFF; // The first byte that is read.
  const unsigned outlast = (load + data.size() - 1) & 0xFFFF; // The first byte that is written.
  // The decoder is finished when it fetches the byte below the data. If its
  // low byte is zero the high byte of the pointer was already decremented.
  const unsigned below = start - 1;
  const unsigned endhi = (below >> 8) - ((below & 0xFF) == 0 ? 1 : 0);
  auto put = [&stub](int pos, unsigned v) { stub.at(pos) = v & 0xFF; };
  stub.at(POS_OF_JUMP_TO) = jmp & 0xFF;
  stub.at(POS_OF_JUMP_TO + 1) = (jmp >> 8) & 0xFF;
  put(POS_OF_MVDSTLO, start);
  put(POS_OF_MVDSTHI, start >> 8);
  put(POS_OF_SRCLO, last);
  put(POS_OF_SRCHI, last >> 8);
  put(POS_OF_DSTLO, outlast);
  put(POS_OF_DSTHI, outlast >> 8);
  put(POS_OF_ENDLO, below);
  put(POS_OF_ENDHI, endhi);
  put(POS_OF_STEP7, step_ + 7);
  put(POS_OF_MVPAGES, plen >> 8);
  put(POS_OF_MVREST, plen);
  std::copy(stub.begin(), stub.end(), std::ostream_iterator<unsigned char>(out));
}
