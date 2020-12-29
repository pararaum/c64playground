#include <inttypes.h>
#include <iostream>
#include <fstream>
#include <vector>
#include <iterator>
#include <stdexcept>
#include <algorithm>
#include <numeric>
#include <sstream>
#include <boost/format.hpp>
#include "cmdline.h"
#include "data.hh"

/*! \file xipz.cc
 *
 * Source file containing the main function.
 */

/*
00000000  01 08 0a 08 02 03 9e 32  30 36 31 00 00 00 a2 08  |.......2061.....|
00000010  bd 35 08 95 58 ca 10 f8  20 bf a3 78 b9 3e 08 99  |.5..X... ..x.>..|
00000020  f7 00 c8 d0 f7 a5 58 18  69 ff aa a5 59 69 00 8d  |......X.i...Yi..|
End address to shift to: $36
Pointer to End of Data: $38. Set to $…+table_size+length_of_compressed_data.
Pointer to Begin of Data: $3d. Set to $…+table_size
00000030  19 01 98 4c ff 00 00 10  7d 08 54 37 44 7d 08 a8  |...L....}.T7D}..|
N: $46
00000040  f0 03 a0 08 2c a0 05 46  2b d0 14 66 2b e8 d0 0f  |....,..F+..f+...|
High byte to stop reading at: $58
Jump: $5c
00000050  ee 19 01 48 ad 19 01 c9  10 d0 03 4c e2 fc 68 1e  |...H.......L..h.|
Destination address: $6f
00000060  00 00 2a 88 30 d9 d0 df  b0 04 a8 b9 36 01 8d 3c  |..*.0.......6..<|
00000070  03 a0 00 98 ee 27 01 d0  ce ee 28 01 d0 c9        |.....'....(...|
*/
#include "decrunchxipzstub.inc"
//! Position in the stub where the end of the compressed data is stored.
#define POS_OF_END_OF_CDATA 0x38
//! Position in the stub where the beginning of the compressed data is stored.
#define POS_OF_BEGIN_OF_CDATA 0x3d
//! Position in the stub where the end of the number of bits for the compression is stored.
#define POS_OF_N 0x46
//! Position in the stub where the address is stored at which the data is decompressed.
#define POS_OF_DEST 0x6f
//! Position in the stub where the address is stored at which the final JMP is performed.
#define POS_OF_JMP 0x5c
//! Position in the stub where the high byte of the destination address is stored at which the decompression process stops.
#define POS_OF_STOPREADING 0x58


//! \brief Array to hold the histogram.
typedef std::array<unsigned long, 256> HistoArray;


/* \brief Simple structure to store bits.
 *
 * This structure is used to store the bits for the compressor, it is
 * used in the \ref CompressionBits array.
 */
struct Bits {
  unsigned data; //!< Storage area for the bits.
  unsigned bits; //!< Number of bits stored.

  //! Default Constructor
  Bits() : data(0), bits(0) {}
  /*! Constructor with predefined data
   *
   * \param data_ bit data to initialise
   * \param n number of bits in the data
   */
  Bits(unsigned data_, unsigned n) : data(data_), bits(n) {}
};


/*! \brief Output operator for Bits
 *
 * This function will print the bits stored in a human-readable
 * manner.
 */
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
  

/*! \brief Histogram entry
 *
 * Stores a mapping of byte to byte frequency/count.
 */
class HistEntry {
public:
  uint8_t byte;
  unsigned long freq;

  bool operator<(const HistEntry &x) const { return freq > x.freq; }
};


//! Convenience type definition for an array of 256 byte counts.
typedef std::array<Bits, 256> CompressionBits;


/*! \brief Read data from a file
 *
 * Input is read and an exception is thrown if the file can not be
 * opened.
 *
 * \param fname file name
 * \return Data object with loaded binary data
 */
Data read_data(const std::string &fname) {
  std::vector<uint8_t> data;
  std::ifstream inp(fname, std::ios::binary);
  uint8_t tmp;

  if(!inp) {
    std::ostringstream out;
    out << "can not open file '" << fname << '\'';
    throw std::runtime_error(out.str());
  };
  //So such iterator? std::copy(std::istreambuf_iterator<uint8_t>(inp), std::istreambuf_iterator<uint8_t>(), std::back_inserter(data));
  do {
    tmp = inp.get();
    if(!inp.eof()) {
      data.push_back(tmp);
    }
  } while(inp);
  return Data(data);
}


/*! \brief calculate the histogram
 *
 * For each byte in the input data the frequency is calculated.
 *
 * \param data binary data
 * \return An array of frequencies of type \ref HistoArray
 */
HistoArray calc_histo(const Data &data) {
  HistoArray histo;

  histo.fill(0);
  for(uint8_t i : data) {
    ++histo[i];
  }
  return histo;
}


/*! \brief Output a histogram entry
 *
 * This is used to display the nice table of the 64 most common bytes.
 *
 * \param o output stream to write to
 * \param x \ref HistEntry to output
 * \return output stream for stl-conforming usage
 */
std::ostream &operator<<(std::ostream &o, const HistEntry &x) {
  o << '(' << x.freq << " * $" << std::hex << (int)x.byte << std::dec << ')';
  return o;
}


/*! \brief Sort histogram entries
 *
 * The histogram entries are sorted according to their
 * frequencies. The most common bytes are moved to teh beginning of
 * the array.
 *
 * \param harr array of histogram entries
 * \return vector of histogram entries, sorted
 */
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
 * \param histarr sorted histogram data
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


/*! \brief create compression table
 *
 * Generates a table of 256 entries where the \ref n bits are stored
 * according to their frequency or the eight bits of the byte value if
 * not compressed.
 *
 * \param compressable histogram entries for the generation of the compression table
 * \param n how many bits to use
 * \return Table of 256 entries containing the bits for each byte, type is \ref CompressionBits
 */
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


/*! \brief write the decrunch stub
 *
 * The decrunching stub is written and all parameters like decrunching
 * address and jump address are adjusted. Please take care to adjust
 * all the defines above otherwise the code will probably just
 * crash. It may also fry your cat so be careful!
 *
 * \param out output stream to write to
 * \param n number of bits
 * \param size number of compressed bytes
 * \param jmp jump address to jump after decrunching
 * \return output stream
 */
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


/*! \brief write compression table
 *
 * This table contains the 2^n most common bytes in order of
 * decreasing frequency.
 *
 * \param out output stream to write to
 * \param n number of bits
 * \param histe histogram entries, they contain the sorted list of bytes
 * \return output stream
 */
std::ostream &write_compression_table(std::ostream &out, int n, const std::vector<HistEntry> &histe) {
  for(int i = 0; i < (1 << n); ++i) {
    const HistEntry &curr = histe.at(i);
    out << curr.byte;
  }
  return out;
}


/*! \brief write the compressed data
 *
 * Write the binary compressed data into the output stream.
 *
 * \param out output stream to write to
 * \param data binary data to write
 * \return output stream
 */
std::ostream &write_compressed_data(std::ostream &out, const std::vector<uint8_t> &data) {
  std::copy(data.begin(), data.end(), std::ostream_iterator<unsigned char>(out));
  return out;
}


/*! \brief Create compressed data 
 *
 * The table of compression bits (\ref CompressionBits) is all that is
 * needed to compress the data in \ref data. Every output token is
 * prepended with a '1' bit if it is a literal token (aka byte) or
 * with a '0' bit if it is compressed data. In this case only the bits
 * in the compbits table are written.
 *
 * If not a whole byte is filled at the end, then zero bits are
 * appended.
 *
 * \param data binary data to crunch
 * \param compbits compression table
 * \return compressed data
 */
std::vector<uint8_t> create_compressed_data(const Data &data, const CompressionBits &compbits) {
  unsigned long bitstore = 0;
  int bit = 0;
  std::vector<uint8_t> out;

  for(auto i: data) {
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


/*! \brief Perform the whole crunching process.
 *
 * The following operations are performed:
 *  - the compressed data is generated
 *  - the decrunching stub is written
 *  - the compression table is written
 *  - finally the compressed data is written
 *
 * \param outname output filename
 * \param data original data
 * \param compbits the compression bits table
 * \param n number of bits to use for compression
 * \param shisto the sorted histogram array is needed
 * \param raw should the compressed data be written raw (without decompression stub)
 */
void crunch(const std::string &outname, const Data &data, const CompressionBits &compbits, int n, const std::vector<HistEntry> &shisto, bool raw) {
  std::ofstream out(outname, std::ios::binary);
  std::vector<uint8_t> cdata(create_compressed_data(data, compbits));

  if(raw) {
    std::cout << "Skipping writing the decrunching stub!\n";
  } else {
    std::cout << "Writing decrunching stub...\n";
    write_stub(out, n, cdata.size(), data.get_loadaddr());
  }
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


/*! Output the most common bytes and their frequencies.
 *
 * \param shisto sorted histogram array
 */
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
  float minsize = data.size();
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


/*!\brief main function using xip
 *
 * Main function for compression using the xip algorithm.
 *
 * \param inputname input filename
 * \param outputname outout filename
 * \param raw should the compressed data be written raw (without decompression stub)
 */
int main_xip(const std::string &inputname, const std::string &outputname, bool raw) {
  std::cout << "XiZ Version " << CMDLINE_PARSER_VERSION << std::endl;
  Data data(read_data(inputname));
  std::cout << "Bytes read (without load address): " << data.size() << std::endl;
  std::cout << "Load address: " << data.get_loadaddr() << std::endl;
  HistoArray histo(calc_histo(data));
  std::vector<HistEntry> shisto(sort_histo(histo));
  output_64_common(shisto);
  int n = choose_optimal_n(data, shisto);
  std::cout << "Optimal number of bits: N=" << n << std::endl;
  CompressionBits compbits(create_compression_bits(shisto, n));
  crunch(outputname, data, compbits, n, shisto, raw);
  return 0;
}

/*!\brief main function using xip
 *
 * Main function which takes a single file name as an argument.
 */
int main(int argc, char **argv) {
  gengetopt_args_info args;
  int ret = -1;

  if(cmdline_parser(argc, argv, &args) != 0) {
    return 1;
  } else {
    if(args.inputs_num < 1) {
      std::cerr << "At least one filename must be provided!\n";
      return 1;
    }
    try {
      std::string inpnam(args.inputs[0]);
      std::string outnam;
      if(args.inputs_num < 2) {
	outnam = inpnam + (args.raw_given ? ".raw" : ".prg");
      } else {
	outnam = args.inputs[1];
      }
      ret = main_xip(inpnam, outnam, args.raw_given);
    }
    catch(const std::exception &e) {
      std::cerr << "Exception: " << e.what() << std::endl;
      ret = -1;
    }
  }
  return ret;
}
