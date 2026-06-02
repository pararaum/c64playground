#include "data.hh"
#include "lzp.hh"
#include <iostream>
#include <array>
#include <ranges>
#include <list>
#include <algorithm>
#include "decrunchlzpstub.inc"

/*
 * When testing a little it seemed (but only a single testfile, needs
 * more investigation) that 12 Bits could be used for better
 * compression. Restarting the hash before checking for each run
 * actually worsened the compression ratio.
 */
#define LZPMODELSIZE 8
#define MAX_RUNLENGTH 255

#include <iostream>
#include <iomanip>
#include <vector>
#include <string>
#include <cstdint>
#include <algorithm>

template<unsigned int MODELSIZE>
class LZPModel {
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
  void advance(uint8_t byte) {
    //best?
    hash_ = ((hash_ << 3) ^ byte) % (1 << MODELSIZE);
    //good? hash_ = ((hash_ << 5) ^ byte) % (1 << MODELSIZE);
    //very bad: hash_ = (((hash_ << 5) | (hash_ >> 3)) ^ byte) % (1 << MODELSIZE);
    //good: hash_ = ((hash_ << 3) + byte) % (1 << MODELSIZE);
    //very bad: hash_ = (hash_ + byte) % (1 << MODELSIZE);
  }

  // Current hash value, useful for debugging.
  unsigned long hash() const { return hash_; }

private:
  std::array<uint8_t, 1 << MODELSIZE> model_;
  unsigned long hash_;
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

std::vector<uint8_t> crunch_lzp(const Data &data) {
  LZPModel<LZPMODELSIZE> model;
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
    std::cerr << "Compressed data was " << output.size() << " bytes long.\n";
    throw std::logic_error("wrong data after decompression");
  }
  return outputvec;
}


std::ostream &write_lzp_stub(std::ostream &out, uint16_t size, uint16_t loadaddr, uint16_t jmp) {
  // Create a local copy.
  std::vector<uint8_t> stub(decrunchlzpstub, decrunchlzpstub + decrunchlzpstub_len);

  // Now copy the modified stub.
  std::copy(stub.begin(), stub.end(), std::ostream_iterator<unsigned char>(out));
  return out;
}
