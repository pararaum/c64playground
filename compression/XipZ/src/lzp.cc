#include "data.hh"
#include "lzp.hh"
#include <iostream>
#include <array>
#include <map>
#include <ranges>
#include <list>
#include <algorithm>
#include <format>
#include "decrunchlzpstub.inc"
#include "decrunchlzp2stub.inc"
#include "decrunchlzp3stub.inc"
#include "decrunchlzp4stub.inc"
#include "decrunchlzp5stub.inc"
#include "compression.hh"

/*
 * When testing a little it seemed (but only a single testfile, needs
 * more investigation) that 12 Bits could be used for better
 * compression. Restarting the hash before checking for each run
 * actually worsened the compression ratio.
 */
#ifndef LZPMODELSIZE
#define LZPMODELSIZE 8
#endif
#define MAX_RUNLENGTH 255

#include <iostream>
#include <iomanip>
#include <vector>
#include <string>
#include <cstdint>
#include <algorithm>

template<unsigned int MODELSIZE>
class LZPModel {
protected:
  std::array<uint8_t, 1 << MODELSIZE> model_;
  unsigned long hash_;

public:
  LZPModel() : hash_(0) { model_.fill(0); }

  // Advance the hash with the given byte and update the model.
  // Call this for confirmed literals.
  void update(uint8_t byte) {
    model_[hash_] = byte;
    advance(byte);
  }

  // Predict the next expected byte at the current hash position.
  uint8_t predict() const { return model_[hash_]; }

  // Advance the hash only, without updating the model.
  // Call this for bytes that were part of a confirmed run.
  virtual void advance(uint8_t byte) {
    //best?
    //hash_ = ((hash_ << 3) ^ byte) % (1 << MODELSIZE);
    //sometimes better, sometimes worse:
    hash_ = ((hash_ << 3) + byte) % (1 << MODELSIZE);
    //good? hash_ = ((hash_ << 5) ^ byte) % (1 << MODELSIZE);
    //very bad: hash_ = (((hash_ << 5) | (hash_ >> 3)) ^ byte) % (1 << MODELSIZE);
    //good: hash_ = ((hash_ << 3) + byte) % (1 << MODELSIZE);
    //very bad: hash_ = (hash_ + byte) % (1 << MODELSIZE);
  }

  // Current hash value, useful for debugging.
  unsigned long hash() const { return hash_; }
};


template<unsigned int MODELSIZE>
class LZP2Model : public LZPModel<MODELSIZE> {
protected:
  // In templates, names from a dependent base class aren't found by
  // unqualified lookup, there use `using`.
  using LZPModel<MODELSIZE>::hash_;
public:
  virtual void advance(uint8_t byte) override {
    // Seems to work fine, moving four bits to the left worked nearly as good.
    hash_ = ((hash_ << 5) ^ byte) % (1 << MODELSIZE);
  }
};


void hexdump_side_by_side(
    const std::vector<uint8_t>& original,
    const std::vector<uint8_t>& decompressed,
    size_t bytes_per_row = 16)
{
    size_t rows = (std::max(original.size(), decompressed.size()) + bytes_per_row - 1) / bytes_per_row;

    // Header
    std::cout << std::string(8,' ') << "  Original"
              << std::string(bytes_per_row*3 - 7, ' ')
              << "  Decompressed\n";
    std::cout << std::string(8+2+bytes_per_row*3+2+bytes_per_row*3, '-') << "\n";

    for (size_t row = 0; row < rows; ++row) {
        size_t offset = row * bytes_per_row;

        // Print offset
        std::cout << std::hex << std::setw(6) << std::setfill('0') << offset << ":  ";

        // Helper to print one row of hex bytes, padding with spaces if past end
        auto print_row = [&](const std::vector<uint8_t>& data) {
            for (size_t i = 0; i < bytes_per_row; ++i) {
                if (offset + i < data.size())
                    std::cout << std::hex << std::setw(2) << std::setfill('0')
                              << static_cast<int>(data[offset + i]) << " ";
                else
                    std::cout << "   "; // padding for short buffers
            }
        };

        print_row(original);
        std::cout << "  ";
        print_row(decompressed);

        // Mark rows that differ with '<'
        bool row_differs = false;
        for (size_t i = 0; i < bytes_per_row; ++i) {
            size_t idx = offset + i;
            uint8_t a = idx < original.size()     ? original[idx]     : 0xFF;
            uint8_t b = idx < decompressed.size() ? decompressed[idx] : 0xFF;
            if (a != b) { row_differs = true; break; }
        }
        if (row_differs) std::cout << " <";

        std::cout << "\n";
    }

    std::cout << std::dec; // restore decimal output
}

std::vector<uint8_t> decrunch_lzp(const std::vector<uint8_t>& compressed) {
  LZPModel<LZPMODELSIZE> model;
  std::vector<uint8_t> output;
  unsigned int maskidx = 0;
  size_t pos = 0;

  // Read the first mask byte
  if(pos >= compressed.size()) return output;
  uint8_t mask = compressed[pos++];

  while(pos < compressed.size()) {
    if(maskidx >= 8) {
      if(pos >= compressed.size()) break;
      mask = compressed[pos++];
      maskidx = 0;
    }
    bool is_run = (mask >> maskidx++) & 1;
    uint8_t length = compressed[pos++];

    if(is_run) {
      if(length == 0) break; // EOF marker

      // Replay 'length' bytes from the model
      for(unsigned int i = 0; i < length; ++i) {
	uint8_t byte = model.predict();
	output.push_back(byte);
	model.advance(byte); // advance hash second, same as compressor
	// model is not updated during runs, same as compressor
      }
    } else {
      // Literal byte
      uint8_t byte = length; // the "length" field carries the literal
      output.push_back(byte);
      model.update(byte);
    }
  }

  return output;
}

std::vector<uint8_t> crunch_lzp(const Data &data, bool verbose) {
  LZPModel<LZPMODELSIZE> model;
  // This map contains just the run-lengths and how often they do occur.
  std::map<int, unsigned int> runlength_counts;
  std::list<uint8_t> output; // A list of output elements, a list is needed for the following trick to work: see mask.
  unsigned int maskidx = 0;
  // Pushing the zero and getting a pointer to the value is a nice
  // trick as we can modify the mask when elements are later on added
  // without having to worry about splicing this values. But we have
  // to use a list as a vector invalidates references and therefore
  // pointers when the capacity is exhausted.
  output.push_back(0); // Add a mask, preinitialised with zeroes.
  uint8_t *mask = &output.back(); // And get a reference to our mask.
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

  for(unsigned long pos = 0; pos < data.size(); ) {
    unsigned int runlength = 0;

    while(runlength < MAX_RUNLENGTH) {
      if(pos + runlength < data.size()) {
	uint8_t byte = data[pos + runlength];
	if(byte != model.predict()) { // Still a match?
	  break; // No go to next step.
	}
      } else {
	break;
      }
      model.advance(data[pos + runlength]);
      ++runlength;
    }
    if(runlength > 0) {
      output.push_back(runlength);
      nextmask(1);
      pos += runlength;
      runlength_counts[runlength] += 1;
    } else if(pos < data.size()) {
      output.push_back(data[pos]);
      nextmask(0);
      model.update(data[pos]);
      ++pos;
    }
  }
  output.push_back(0); // Zero length is EOF.
  nextmask(1);
  // Use C++23 feature to return a vector instead of the list.
  // Not supported by my compiler version: return output | std::ranges::to<std::vector>();
  // Use range constructor instead:
  auto outputvec = std::vector<uint8_t>(output.begin(), output.end());
  auto decompressed = decrunch_lzp(outputvec);
  if(decompressed != data.get_dataref()) {
    hexdump_side_by_side(data.get_dataref(), decompressed);
    std::cout << "Compressed data was " << output.size() << " bytes long.\n";
    throw std::logic_error("wrong data after decompression");
  }
  if(verbose) {
    std::cout << std::format("{:>10} {:>9}", "run length", "count") << std::endl;
    for( auto [rlen, rlc] : runlength_counts ) {
      std::cout << std::format("{:10} {:9}", rlen, rlc) << std::endl;
    }
  }
  return outputvec;
}


std::ostream &write_lzp_stub(std::ostream &out, uint16_t size, uint16_t loadaddr, uint16_t jmp) {
  const int POS_OF_JUMP_TO = 0x90 + 2;
  const int POS_OF_MINUSLENLO = 0x1B + 2;
  const int POS_OF_MINUSLENHI = 0x1F + 2;
  const int POS_OF_DSTDATAPTR = 0xA4 + 2;
  const int POS_OF_UPCOPYSTC = 0x39 + 2;
  const long minuslen = -static_cast<long>(size);

  // Create a local copy.
  std::vector<uint8_t> stub(decrunchlzpstub_prg, decrunchlzpstub_prg + decrunchlzpstub_prg_len);

  stub.at(POS_OF_JUMP_TO) = jmp & 0xFF;
  stub.at(POS_OF_JUMP_TO + 1) = (jmp >> 8) & 0xFF;
  stub.at(POS_OF_MINUSLENLO) = minuslen & 0xFF;
  stub.at(POS_OF_MINUSLENHI) = (minuslen >> 8) & 0xFF;
  stub.at(POS_OF_DSTDATAPTR) = loadaddr & 0xFF;
  stub.at(POS_OF_DSTDATAPTR + 1) = (loadaddr >> 8) & 0xFF;
  long upcopystc = stub.at(POS_OF_UPCOPYSTC) | (stub.at(POS_OF_UPCOPYSTC + 1) << 8);
  upcopystc += size; // Add size of data.
  stub.at(POS_OF_UPCOPYSTC) = upcopystc & 0xFF;
  stub.at(POS_OF_UPCOPYSTC + 1) = (upcopystc >> 8) & 0xFF;
  
  // Now copy the modified stub.
  std::copy(stub.begin(), stub.end(), std::ostream_iterator<unsigned char>(out));
  return out;
}


std::vector<uint8_t> Lzp2Compressor::compress() {
  LZP2Model<LZPMODELSIZE> model;
  std::list<uint8_t> output; // A list of output elements, a list is needed for the following trick to work: see mask.
  unsigned int maskidx = 0;
  // Pushing the zero and getting a pointer to the value is a nice
  // trick as we can modify the mask when elements are later on added
  // without having to worry about splicing this values. But we have
  // to use a list as a vector invalidates references and therefore
  // pointers when the capacity is exhausted.
  output.push_back(0); // Add a mask, preinitialised with zeroes.
  uint8_t *mask = &output.back(); // And get a reference to our mask.
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

  for(unsigned long pos = 0; pos < data.size(); ++pos) {
    uint8_t byte = data[pos];
    if(byte == model.predict()) { // Does the model predict correctly?
      nextmask(1);
      model.advance(byte);
    } else { // No, new byte.
      output.push_back(byte);
      nextmask(0);
      model.update(byte);
    }
  }
  /* TODO:
   * 
   * Mark for EOF? Probably we will have to pass the number of output
   * bytes...
   */
  // Use C++23 feature to return a vector instead of the list.
  // Not supported by my compiler version: return output | std::ranges::to<std::vector>();
  // Use range constructor instead:
  auto outputvec = std::vector<uint8_t>(output.begin(), output.end());
  /* TODO: decompression and check needed... */
  // auto decompressed = decrunch_lzp(outputvec);
  // if(decompressed != data.get_dataref()) {
  //   hexdump_side_by_side(data.get_dataref(), decompressed);
  //   std::cout << "Compressed data was " << output.size() << " bytes long.\n";
  //   throw std::logic_error("wrong data after decompression");
  // }
  return outputvec;
}

void Lzp2Compressor::write_stub(std::ofstream& out, const std::vector<uint8_t>& c,
				uint16_t load, uint16_t jmp) {
  // Offsets from decrunchlzp2stub.prg.label (*_offset) plus two for the load address.
  const int POS_OF_JUMP_TO = 0x84 + 2;
  const int POS_OF_MINUSLENLO = 0x1B + 2;
  const int POS_OF_MINUSLENHI = 0x1F + 2;
  const int POS_OF_DSTDATAPTR = 0x9B + 2;
  const int POS_OF_UPCOPYSTC = 0x39 + 2;
  const int POS_OF_ENDLO = 0xA9 + 2;
  const int POS_OF_ENDHI = 0xB0 + 2;
  const long minuslen = -static_cast<long>(c.size());

  // Create a local copy.
  std::vector<uint8_t> stub(decrunchlzp2stub_prg, decrunchlzp2stub_prg + decrunchlzp2stub_prg_len);
  
  stub.at(POS_OF_JUMP_TO) = jmp & 0xFF;
  stub.at(POS_OF_JUMP_TO + 1) = (jmp >> 8) & 0xFF;
  stub.at(POS_OF_MINUSLENLO) = minuslen & 0xFF;
  stub.at(POS_OF_MINUSLENHI) = (minuslen >> 8) & 0xFF;
  stub.at(POS_OF_DSTDATAPTR) = load & 0xFF;
  stub.at(POS_OF_DSTDATAPTR + 1) = (load >> 8) & 0xFF;
  // The stub stops when the destination pointer reaches this address.
  const unsigned endaddr = (load + data.size()) & 0xFFFF;
  stub.at(POS_OF_ENDLO) = endaddr & 0xFF;
  stub.at(POS_OF_ENDHI) = (endaddr >> 8) & 0xFF;
  long upcopystc = stub.at(POS_OF_UPCOPYSTC) | (stub.at(POS_OF_UPCOPYSTC + 1) << 8);
  upcopystc += c.size(); // Add size of data.
  stub.at(POS_OF_UPCOPYSTC) = upcopystc & 0xFF;
  stub.at(POS_OF_UPCOPYSTC + 1) = (upcopystc >> 8) & 0xFF;
  
  // Now copy the modified stub.
  std::copy(stub.begin(), stub.end(), std::ostream_iterator<unsigned char>(out));
}



/* Flag bits of lzp3, lzp4 and lzp5. They are used MSB first from a mask
 * byte which is fetched when its first bit is needed, like in the
 * stubs. So the mask byte is placed behind the literal of the event
 * before. A set sentinel bit marks the end of the mask in the stubs.
 */
namespace {
class MaskWriter {
  std::list<uint8_t> &output_; // A list as the mask is modified later, see Lzp2Compressor::compress().
  unsigned idx_ = 0;
  uint8_t *mask_ = nullptr;
public:
  explicit MaskWriter(std::list<uint8_t> &output) : output_(output) {}
  void bit(bool set1) {
    if(idx_ == 0) {
      output_.push_back(0);
      mask_ = &output_.back();
    }
    if(set1) {
      *mask_ |= 0x80 >> idx_;
    }
    idx_ = (idx_ + 1) % 8;
  }
};

class StreamReader {
  const std::vector<uint8_t> &c_;
  size_t pos_ = 0;
  unsigned mask_ = 0;
public:
  explicit StreamReader(const std::vector<uint8_t> &c) : c_(c) {}
  uint8_t byte() { return c_.at(pos_++); }
  bool bit() {
    mask_ <<= 1;
    if((mask_ & 0xFF) == 0) { // Only the sentinel was left.
      mask_ = (byte() << 1) | 1;
    }
    bool b = mask_ & 0x100;
    mask_ &= 0xFF;
    return b;
  }
};
}

/* LZP3: two models, a hashed order-2 model and an order-1 model. The
 * order-2 prediction is tried first, a zero entry means unknown and
 * the order-1 prediction is used instead. If the first prediction
 * missed and the order-1 prediction is another byte then it gets a
 * second chance. The bits are read MSB first from a mask byte (like
 * in the stub, the byte is read when its first bit is needed).
 */
namespace {
class LZP3Model {
  std::array<uint8_t, 256> model1_{};
  std::array<uint8_t, 256> model2_{};
  uint8_t byte1_ = 0, byte2_ = 0;
public:
  uint8_t hash() const { return static_cast<uint8_t>((byte2_ << 4) ^ byte1_); }
  uint8_t pred2() const { return model2_[hash()]; }
  uint8_t pred1() const { return model1_[byte1_]; }
  // Primary prediction, order 2 if known.
  uint8_t primary() const { return pred2() ? pred2() : pred1(); }
  // Is there a second guess and what is it?
  bool has_alternative() const { return pred2() && pred1() != pred2(); }
  void update(uint8_t byte) {
    model2_[hash()] = byte;
    model1_[byte1_] = byte;
    byte2_ = byte1_;
    byte1_ = byte;
  }
};
}

static std::vector<uint8_t> decrunch_lzp3(const std::vector<uint8_t>& c, size_t outsize) {
  LZP3Model model;
  StreamReader in(c);
  std::vector<uint8_t> out;
  while(out.size() < outsize) {
    uint8_t byte = model.primary();
    if(!in.bit()) {
      bool second = false;
      if(model.has_alternative()) {
	byte = model.pred1();
	second = in.bit();
      }
      if(!second) {
	byte = in.byte();
      }
    }
    out.push_back(byte);
    model.update(byte);
  }
  return out;
}

std::vector<uint8_t> Lzp3Compressor::compress() {
  LZP3Model model;
  std::list<uint8_t> output;
  MaskWriter flags(output);
  for(uint8_t byte : data.get_dataref()) {
    if(byte == model.primary()) {
      flags.bit(1);
    } else {
      flags.bit(0);
      bool second = false;
      if(model.has_alternative()) {
	second = (byte == model.pred1());
	flags.bit(second);
      }
      if(!second) {
	output.push_back(byte);
      }
    }
    model.update(byte);
  }
  auto outputvec = std::vector<uint8_t>(output.begin(), output.end());
  if(decrunch_lzp3(outputvec, data.size()) != data.get_dataref()) {
    throw std::logic_error("lzp3: wrong data after decompression");
  }
  return outputvec;
}

void Lzp3Compressor::write_stub(std::ofstream& out, const std::vector<uint8_t>& c,
				uint16_t load, uint16_t jmp) {
  // Offsets from decrunchlzp3stub.prg.label (*_offset) plus two for the load address.
  const int POS_OF_JUMP_TO = 0xB8 + 2;
  const int POS_OF_MINUSLENLO = 0x19 + 2;
  const int POS_OF_MINUSLENHI = 0x1D + 2;
  const int POS_OF_DSTDATAPTR = 0xBB + 2;
  const int POS_OF_UPCOPYSTC = 0x40 + 2;
  const int POS_OF_ENDLO = 0xC9 + 2;
  const int POS_OF_ENDHI = 0xD0 + 2;
  const long minuslen = -static_cast<long>(c.size());

  std::vector<uint8_t> stub(decrunchlzp3stub_prg, decrunchlzp3stub_prg + decrunchlzp3stub_prg_len);

  stub.at(POS_OF_JUMP_TO) = jmp & 0xFF;
  stub.at(POS_OF_JUMP_TO + 1) = (jmp >> 8) & 0xFF;
  stub.at(POS_OF_MINUSLENLO) = minuslen & 0xFF;
  stub.at(POS_OF_MINUSLENHI) = (minuslen >> 8) & 0xFF;
  stub.at(POS_OF_DSTDATAPTR) = load & 0xFF;
  stub.at(POS_OF_DSTDATAPTR + 1) = (load >> 8) & 0xFF;
  // The stub stops when the destination pointer reaches this address.
  const unsigned endaddr = (load + data.size()) & 0xFFFF;
  stub.at(POS_OF_ENDLO) = endaddr & 0xFF;
  stub.at(POS_OF_ENDHI) = (endaddr >> 8) & 0xFF;
  long upcopystc = stub.at(POS_OF_UPCOPYSTC) | (stub.at(POS_OF_UPCOPYSTC + 1) << 8);
  upcopystc += c.size(); // Add size of data.
  stub.at(POS_OF_UPCOPYSTC) = upcopystc & 0xFF;
  stub.at(POS_OF_UPCOPYSTC + 1) = (upcopystc >> 8) & 0xFF;

  std::copy(stub.begin(), stub.end(), std::ostream_iterator<unsigned char>(out));
}


/* LZP4: the original LZP. A hash of the last bytes selects a table
 * entry, which holds the position in the output where this context was
 * seen last. If the following bytes are the same as the bytes at that
 * position then a run is copied: a set flag bit and a length byte
 * (length - LZP4_MINLEN) are written. Otherwise a cleared flag bit and
 * the literal are written. The table is updated for every byte. The
 * flag bits are used like in lzp3 (MSB first, mask byte fetched with
 * its first bit).
 */
#ifndef LZP4_SHIFT
#define LZP4_SHIFT 4
#endif
#ifndef LZP4_MINLEN
#define LZP4_MINLEN 2
#endif
#define LZP4_MAXLEN 255

namespace {
class LZP4Model {
  std::array<uint16_t, 256> pos_{}; // Positions in the output, initially zero.
  uint8_t hash_ = 0;
public:
  uint16_t predicted() const { return pos_[hash_]; }
  // Called for every output byte, cur is the position of the byte.
  void update(uint16_t cur, uint8_t byte) {
    pos_[hash_] = cur;
    hash_ = static_cast<uint8_t>((hash_ << LZP4_SHIFT) ^ byte);
  }
};
}

static std::vector<uint8_t> decrunch_lzp4(const std::vector<uint8_t>& c, size_t outsize) {
  LZP4Model model;
  StreamReader in(c);
  std::vector<uint8_t> out;
  while(out.size() < outsize) {
    if(in.bit()) {
      size_t src = model.predicted();
      unsigned len = in.byte() + LZP4_MINLEN;
      while(len--) {
	uint8_t byte = out.at(src++);
	model.update(out.size(), byte);
	out.push_back(byte);
      }
    } else {
      uint8_t byte = in.byte();
      model.update(out.size(), byte);
      out.push_back(byte);
    }
  }
  return out;
}

std::vector<uint8_t> Lzp4Compressor::compress() {
  const auto &in = data.get_dataref();
  if(in.size() > 0x10000) {
    throw std::runtime_error("lzp4: input is longer than 64 KB");
  }
  LZP4Model model;
  std::list<uint8_t> output;
  MaskWriter flags(output);
  size_t cur = 0;
  while(cur < in.size()) {
    size_t p = model.predicted();
    size_t len = 0;
    if(p < cur) { // The initial entries do not predict anything at position zero.
      while(cur + len < in.size() && len < LZP4_MAXLEN && in[p + len] == in[cur + len]) {
	++len;
      }
    }
    if(len >= LZP4_MINLEN) {
      flags.bit(1);
      output.push_back(len - LZP4_MINLEN);
    } else {
      len = 1;
      flags.bit(0);
      output.push_back(in[cur]);
    }
    for(size_t i = 0; i < len; ++i, ++cur) {
      model.update(cur, in[cur]);
    }
  }
  auto outputvec = std::vector<uint8_t>(output.begin(), output.end());
  if(decrunch_lzp4(outputvec, in.size()) != in) {
    throw std::logic_error("lzp4: wrong data after decompression");
  }
  return outputvec;
}

void Lzp4Compressor::write_stub(std::ofstream& out, const std::vector<uint8_t>& c,
				uint16_t load, uint16_t jmp) {
  // Offsets from decrunchlzp4stub.prg.label (*_offset) plus two for the load address.
  const int POS_OF_JUMP_TO = 0xD9 + 2;
  const int POS_OF_MINUSLENLO = 0x19 + 2;
  const int POS_OF_MINUSLENHI = 0x1D + 2;
  const int POS_OF_DSTDATAPTR = 0xA7 + 2;
  const int POS_OF_UPCOPYSTC = 0x43 + 2;
  const int POS_OF_ENDLO = 0xB5 + 2;
  const int POS_OF_ENDHI = 0xBC + 2;
  const long minuslen = -static_cast<long>(c.size());

  std::vector<uint8_t> stub(decrunchlzp4stub_prg, decrunchlzp4stub_prg + decrunchlzp4stub_prg_len);

  stub.at(POS_OF_JUMP_TO) = jmp & 0xFF;
  stub.at(POS_OF_JUMP_TO + 1) = (jmp >> 8) & 0xFF;
  stub.at(POS_OF_MINUSLENLO) = minuslen & 0xFF;
  stub.at(POS_OF_MINUSLENHI) = (minuslen >> 8) & 0xFF;
  stub.at(POS_OF_DSTDATAPTR) = load & 0xFF;
  stub.at(POS_OF_DSTDATAPTR + 1) = (load >> 8) & 0xFF;
  // The stub stops when the destination pointer reaches this address.
  const unsigned endaddr = (load + data.size()) & 0xFFFF;
  stub.at(POS_OF_ENDLO) = endaddr & 0xFF;
  stub.at(POS_OF_ENDHI) = (endaddr >> 8) & 0xFF;
  long upcopystc = stub.at(POS_OF_UPCOPYSTC) | (stub.at(POS_OF_UPCOPYSTC + 1) << 8);
  upcopystc += c.size(); // Add size of data.
  stub.at(POS_OF_UPCOPYSTC) = upcopystc & 0xFF;
  stub.at(POS_OF_UPCOPYSTC + 1) = (upcopystc >> 8) & 0xFF;

  std::copy(stub.begin(), stub.end(), std::ostream_iterator<unsigned char>(out));
}


/* LZP5: a real order-1 context, the table is indexed by the last byte
 * (no hash), it holds the byte which followed it last. A set flag bit
 * says that the byte is the predicted one, a cleared flag bit is
 * followed by the literal. This is lzp2 without the hash: the rolling
 * hash of lzp2 (shift 5, 8 bits) only keeps three bits of the
 * second-last byte, which is just noise for a 256 byte table; every
 * order-2 and order-3 hash tried compressed worse.
 */
static std::vector<uint8_t> decrunch_lzp5(const std::vector<uint8_t>& c, size_t outsize) {
  std::array<uint8_t, 256> model{};
  StreamReader in(c);
  std::vector<uint8_t> out;
  uint8_t last = 0;
  while(out.size() < outsize) {
    uint8_t byte = model[last];
    if(!in.bit()) {
      byte = in.byte();
    }
    out.push_back(byte);
    model[last] = byte;
    last = byte;
  }
  return out;
}

std::vector<uint8_t> Lzp5Compressor::compress() {
  std::array<uint8_t, 256> model{};
  std::list<uint8_t> output;
  MaskWriter flags(output);
  uint8_t last = 0;
  for(uint8_t byte : data.get_dataref()) {
    if(byte == model[last]) {
      flags.bit(1);
    } else {
      flags.bit(0);
      output.push_back(byte);
      model[last] = byte;
    }
    last = byte;
  }
  auto outputvec = std::vector<uint8_t>(output.begin(), output.end());
  if(decrunch_lzp5(outputvec, data.size()) != data.get_dataref()) {
    throw std::logic_error("lzp5: wrong data after decompression");
  }
  return outputvec;
}

void Lzp5Compressor::write_stub(std::ofstream& out, const std::vector<uint8_t>& c,
				uint16_t load, uint16_t jmp) {
  // Offsets from decrunchlzp5stub.prg.label (*_offset) plus two for the load address.
  const int POS_OF_JUMP_TO = 0x83 + 2;
  const int POS_OF_MINUSLENLO = 0x19 + 2;
  const int POS_OF_MINUSLENHI = 0x1D + 2;
  const int POS_OF_DSTDATAPTR = 0x86 + 2;
  const int POS_OF_UPCOPYSTC = 0x3B + 2;
  const int POS_OF_ENDLO = 0x94 + 2;
  const int POS_OF_ENDHI = 0x9B + 2;
  const long minuslen = -static_cast<long>(c.size());

  std::vector<uint8_t> stub(decrunchlzp5stub_prg, decrunchlzp5stub_prg + decrunchlzp5stub_prg_len);

  stub.at(POS_OF_JUMP_TO) = jmp & 0xFF;
  stub.at(POS_OF_JUMP_TO + 1) = (jmp >> 8) & 0xFF;
  stub.at(POS_OF_MINUSLENLO) = minuslen & 0xFF;
  stub.at(POS_OF_MINUSLENHI) = (minuslen >> 8) & 0xFF;
  stub.at(POS_OF_DSTDATAPTR) = load & 0xFF;
  stub.at(POS_OF_DSTDATAPTR + 1) = (load >> 8) & 0xFF;
  // The stub stops when the destination pointer reaches this address.
  const unsigned endaddr = (load + data.size()) & 0xFFFF;
  stub.at(POS_OF_ENDLO) = endaddr & 0xFF;
  stub.at(POS_OF_ENDHI) = (endaddr >> 8) & 0xFF;
  long upcopystc = stub.at(POS_OF_UPCOPYSTC) | (stub.at(POS_OF_UPCOPYSTC + 1) << 8);
  upcopystc += c.size(); // Add size of data.
  stub.at(POS_OF_UPCOPYSTC) = upcopystc & 0xFF;
  stub.at(POS_OF_UPCOPYSTC + 1) = (upcopystc >> 8) & 0xFF;

  std::copy(stub.begin(), stub.end(), std::ostream_iterator<unsigned char>(out));
}
