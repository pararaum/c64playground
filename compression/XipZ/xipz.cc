#include <inttypes.h>
#include <iostream>
#include <fstream>
#include <vector>
#include <iterator>
#include <stdexcept>
#include <algorithm>
#include <numeric>
#include <boost/format.hpp>

/*
00000000  01 08 0a 08 02 03 9e 32  30 36 31 00 00 00 a2 08  |.......2061.....|
00000010  bd 37 08 95 58 ca 10 f8  20 bf a3 78 b9 40 08 99  |.7..X... ..x.@..|
00000020  f7 00 c8 d0 f7 a5 58 18  69 ff 8d 18 01 a5 59 69  |......X.i.....Yi|
End address to shift to: $38
Pointer to End of Data: $3a. Set to $…+table_size+length_of_compressed_data.
Pointer to Begin of Data: $3f. Set to $…+table_size
00000030  00 8d 19 01 8a 4c ff 00  00 10 7f 08 54 37 44 7f  |.....L......T7D.|
N: $48
00000040  08 a8 f0 03 a0 08 2c a0  05 46 2b d0 14 66 2b e8  |......,..F+..f+.|
High byte to stop reading at: $5a
Jump: $5e
00000050  d0 0f ee 19 01 48 ad 19  01 c9 10 d0 03 4c e2 fc  |.....H.......L..|
00000060  68 1e 00 00 2a 88 30 d9  d0 df b0 04 a8 b9 36 01  |h...*.0.......6.|
Destination address: $71
00000070  8d 3c 03 a0 00 98 ee 27  01 d0 ce ee 28 01 d0 c9  |.<.....'....(...|

*/
#include "decrunchxipzstub.inc"
#define POS_OF_END_OF_CDATA 0x3a
#define POS_OF_BEGIN_OF_CDATA 0x3f
#define POS_OF_N 0x48
#define POS_OF_DEST 0x71
#define POS_OF_JMP 0x5e
#define POS_OF_STOPREADING 0x5a


//! \brief Array to hold the histogram.
typedef std::array<unsigned long, 256> HistoArray;

struct Bits {
  unsigned data;
  unsigned bits;

  Bits() : data(0), bits(0) {}
  Bits(unsigned data_, unsigned n) : data(data_), bits(n) {}
};
std::ostream &operator<<(std::ostream &o, const Bits &x) {
  for(int i = x.bits - 1; i >= 0; --i) {
    if((x.data & (1 << i)) != 0) {
      o << '1';
    } else {
      o << '0';
    }
  }
  return o;
}
  

class HistEntry {
public:
  uint8_t byte;
  unsigned long freq;

  bool operator<(const HistEntry &x) const { return freq > x.freq; }
};

typedef std::array<Bits, 256> CompressionBits;

class Data {
protected:
  uint16_t loadaddr;
public:
  std::vector<uint8_t> data;

  Data(const std::vector<uint8_t> &inp) {
    if(inp.size() < 3) {
      throw std::underflow_error("not enough bytes");
    }
    loadaddr = (static_cast<uint16_t>(inp.at(1)) << 8) | inp.at(0);
    data.resize(inp.size() - 2);
    std::copy(inp.begin() + 2, inp.end(), data.begin());
  }
  uint16_t get_loadaddr() const { return loadaddr; }
  std::vector<uint8_t>::size_type size() const { return data.size(); }
};

Data read_data(const char *fname) {
  std::vector<uint8_t> data;
  std::ifstream inp(fname, std::ios::binary);
  uint8_t tmp;

  //So such iterator? std::copy(std::istreambuf_iterator<uint8_t>(inp), std::istreambuf_iterator<uint8_t>(), std::back_inserter(data));
  do {
    tmp = inp.get();
    if(!inp.eof()) {
      data.push_back(tmp);
    }
  } while(inp);
  return Data(data);
}

HistoArray calc_histo(const std::vector<uint8_t> &data) {
  HistoArray histo;

  histo.fill(0);
  for(uint8_t i : data) {
    ++histo[i];
  }
  return histo;
}

std::ostream &operator<<(std::ostream &o, const HistEntry &x) {
  o << '(' << x.freq << " * $" << std::hex << (int)x.byte << std::dec << ')';
  return o;
}


std::vector<HistEntry> sort_histo(const HistoArray &harr) {
  std::vector<HistEntry> shisto;

  for(int i = 0; i < 256; ++i) {
    shisto.push_back(HistEntry{static_cast<uint8_t>(i), harr[i]});
  }
  std::sort(shisto.begin(), shisto.end());
  return shisto;
}


/*! \brief calculate compressed bytes
 *
 * Calculate the number of compressed bytes at a given n.
 *
 * \param n number of bits
 * \param data file data
 * \param hitarr sorted histogram data
 * \return number of compressed bytes (with fractional part)
 */
float calc_comp(int n, const Data &data, const std::vector<HistEntry> &histarr) {
  int two_n = 1 << n;
  unsigned long compressed_bytes = 0;
  unsigned long total_bytes = data.size();
  
  std::for_each(histarr.begin(), histarr.begin() + two_n, [&compressed_bytes](const HistEntry &x) { compressed_bytes += x.freq; });
  unsigned long literals = total_bytes - compressed_bytes;
  float after_compression = 8 * literals; // Number of literal *bits*.
  after_compression += n * compressed_bytes; // Compressed bits.
  after_compression += total_bytes; // A bit for every compressed data token (compressed/literal)?
  after_compression /= 8; // Calculate bytes.
  std::cout << boost::format("Total compressed bytes (n=%d): %5.3f (%.8e:1)\n") % n % after_compression % (total_bytes / after_compression);
  return after_compression;
}


CompressionBits create_compression_bits(const std::vector<HistEntry> &compressable, int n) {
  CompressionBits bits;
  int i;

  for(i = 0; i < 256; ++i) {
    bits[i] = Bits(i, 8);
  }
  for(i = 0; i < (1 << n); ++i) {
    bits[compressable[i].byte] = Bits(i, n);
  }
  return bits;
}

std::ostream &write_stub(std::ostream &out, int n, uint16_t size, uint16_t jmp) {
  // Create a local copy.
  std::vector<uint8_t> stub(decrunchxipzstub, decrunchxipzstub + decrunchxipzstub_len);
  unsigned endptr = stub.at(POS_OF_END_OF_CDATA) | (stub.at(POS_OF_END_OF_CDATA + 1) << 8);
  unsigned beginptr = stub.at(POS_OF_BEGIN_OF_CDATA) | (stub.at(POS_OF_BEGIN_OF_CDATA + 1) << 8);
  
  // Assign number of bits.
  stub.at(POS_OF_N) = n;
  // Assign new begin of compressed data.
  beginptr += 1 << n; // Add the current table size.;
  stub.at(POS_OF_BEGIN_OF_CDATA) = beginptr & 0xFF;
  stub.at(POS_OF_BEGIN_OF_CDATA + 1) = (beginptr >> 8) & 0xFF;
  // Assign new end of compressed data.
  endptr += 1 << n; // Add the current table size.
  endptr += size; // Add number of bytes of compressed data.
  endptr += 1; // End pointer must point to the byte *after* the data.
  stub.at(POS_OF_END_OF_CDATA) = endptr & 0xFF;
  stub.at(POS_OF_END_OF_CDATA + 1) = (endptr >> 8) & 0xFF;
  // Assign the new jmp position.
  stub.at(POS_OF_JMP) = jmp & 0xFF;
  stub.at(POS_OF_JMP + 1) = (jmp >> 8) & 0xFF;
  // Assign the destination position (currently equals the jump).
  stub.at(POS_OF_DEST) = jmp & 0xFF;
  stub.at(POS_OF_DEST + 1) = (jmp >> 8) & 0xFF;
  // Set the maximal read position high-byte.
  stub.at(POS_OF_STOPREADING) = 0x10; // Todo: configurable!
  // Now copy the modified stub.
  std::copy(stub.begin(), stub.end(), std::ostream_iterator<unsigned char>(out));
  return out;
}


std::ostream &write_compression_table(std::ostream &out, int n, const std::vector<HistEntry> &histe) {
  for(int i = 0; i < (1 << n); ++i) {
    const HistEntry &curr = histe.at(i);
    out << curr.byte;
  }
  return out;
}


std::ostream &write_compressed_data(std::ostream &out, const std::vector<uint8_t> &data) {
  std::copy(data.begin(), data.end(), std::ostream_iterator<unsigned char>(out));
  return out;
}


std::vector<uint8_t> create_compressed_data(const Data &data, const CompressionBits &compbits) {
  unsigned long bitstore = 0;
  int bit = 0;
  std::vector<uint8_t> out;

  for(auto i: data.data) {
    bitstore <<= 1; // Move one bit to the left.
    ++bit;
    if(compbits[i].bits == 8) {
      // This is uncompressed!
      bitstore |= 1;
    }
    // Output the data.
    bitstore <<= compbits[i].bits;
    bit += compbits[i].bits;
    bitstore |= compbits[i].data;
    while(bit >= 8) {
      out.push_back(static_cast<char>(bitstore >> (bit - 8)));
      bit -= 8;
    }
#ifdef DEBUG
    std::cout << "█" << std::hex << (int)i << std::dec<< "🠢 " << compbits[i];
#endif
  }
  // Write remaining bits...
  if(bit > 0) {
    int fillbits = (bit % 8);
    bitstore <<= fillbits;
    bit += fillbits;
  }
  while(bit >= 8) {
    out.push_back(static_cast<char>(bitstore >> (bit - 8)));
    bit -= 8;
  }
  return out;
}


void compress(const std::string &oldname, const Data &data, const CompressionBits &compbits, int n, const std::vector<HistEntry> &shisto) {
  std::ofstream out(oldname + ".out", std::ios::binary);
  std::vector<uint8_t> cdata(create_compressed_data(data, compbits));

  std::cout << "Writing decrunching stub...\n";
  write_stub(out, n, cdata.size(), data.get_loadaddr());
  std::cout << boost::format("Writing table, %d bytes...\n") % (1 << n);
  write_compression_table(out, n, shisto);
  std::cout << boost::format("Writing %u bytes compressed data...\n") % cdata.size();
  write_compressed_data(out, cdata);
#ifdef DEBUG
  std::cout << "\n⌲ ";
  std::for_each(cdata.begin(), cdata.end(), [](uint8_t x) {
      std::cout << boost::format("%02X") % (int)x;
    });
  std::cout << std::endl;
#endif
}


void output_64_common(const std::vector<HistEntry> &shisto) {
  int count = 0;

  std::cout << "64 most common bytes:\n\t";
  std::for_each(shisto.begin(), shisto.begin() + 64, [&count](const HistEntry &i) {
      if(count++ >= 8) {
	std::cout << "\n\t";
	count = 0;
      }
      if(i.freq > 0) {
	std::cout << ' ' << i;
      }
    });
  std::cout << std::endl;
}

/*! \brief choose optimal number of bits
 *
 * Calculate the compressed data for each number of bytes and return
 * the optimal number of bits.
 *
 * \param data file data
 * \param shisto sortet histogram aka frequencies
 * \return optimal number of bits
 */
int choose_optimal_n(const Data &data, const std::vector<HistEntry> &shisto) {
  int n = 0;
  float minsize = data.data.size();
  float f;

  for(int i = 1; i <= 6; ++i) {
    f = calc_comp(i, data, shisto);
    if(f < minsize) {
      n = i;
      minsize = f;
    }
  }
  return n;
}

int main(int argc, char **argv) {
  // Parse CLI
  if(argc < 2) {
    std::cerr << "xipz <filename>\n";
  } else {
    Data data(read_data(argv[1]));
    std::cout << "Bytes read (without load address): " << data.data.size() << std::endl;
    std::cout << "Load address: " << data.get_loadaddr() << std::endl;
    HistoArray histo(calc_histo(data.data));
    std::vector<HistEntry> shisto(sort_histo(histo));
    output_64_common(shisto);
    int n = choose_optimal_n(data, shisto);
    std::cout << "Optimal number of bits: N=" << n << std::endl;
    CompressionBits compbits(create_compression_bits(shisto, n));
    compress(argv[1], data, compbits, n, shisto);
  }
}
