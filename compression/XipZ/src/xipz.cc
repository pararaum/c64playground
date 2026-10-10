#include <cmath>
#include <inttypes.h>
#include <vector>
#include <iterator>
#include <algorithm>
#include <numeric>
#include <sstream>
#include <boost/format.hpp>
#include <format>
#include "compression.hh"
#include "qadz.hh"
#include "ratz.hh"
#include "lzp.hh"
#include "bpe.hh"
#include "squz.hh"
#include "lzw.hh"
#include "lzrc.hh"
#include "lzh16.hh"

/*! \file xipz.cc
 *
 * \brief Source file containing the main function.
 *
 * Currently contains the main function of the program and the xipz
 * compression algorithm.
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
//! Position in the stub where the page high-byte is set, see POS_OF_STOPREADING.
#define POS_OF_PAGEHI 0x37

enum Program_Return_Values
  {
   RETURN_NO_FILENAME = 1,
   RETURN_DATA_BELOW_ROM,
   RETURN_NO_ALGORITHM,
   RETURN_UNKNOWN_EXCEPTION = -1
};

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

/*! \brief Convenience type for a histogram array
 *
 * Used to store the \ref HistEntry entries. Mostly this table is
 * sorted so that the most common entries are first.
 */
typedef std::vector<HistEntry> HistorgramArray;

//! Convenience type definition for an array of 256 byte counts.
typedef std::array<Bits, 256> CompressionBits;


/*! \brief calculate the histogram
 *
 * For each byte in the input data the frequency is calculated.
 *
 * The histogram entries are sorted according to their
 * frequencies. The most common bytes are moved to teh beginning of
 * the array.
 *
 * \param data binary data
 * \return vector of histogram entries, sorted
 */
static HistorgramArray calc_histo(const Data &data) {
  std::array<unsigned long, 256> histo;
  HistorgramArray shisto;

  // Clear the histogram array (every body occurs zero times).
  histo.fill(0);
  // For each byte increment the occurrence counter.
  for(uint8_t i : data) {
    ++histo[i];
  }
  for(int i = 0; i < 256; ++i) {
    shisto.push_back(HistEntry{static_cast<uint8_t>(i), histo[i]});
  }
  std::sort(shisto.begin(), shisto.end());
  return shisto;
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


/*! \brief calculate compressed bytes
 *
 * Calculate the number of compressed bytes at a given n.
 *
 * \param n number of bits
 * \param data file data
 * \param histarr sorted histogram data
 * \return number of compressed bytes (with fractional part)
 */
float calc_comp(int n, const Data &data, const HistorgramArray &histarr) {
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
static CompressionBits create_compression_bits(const HistorgramArray &compressable, int n) {
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
 * \param loadaddr original load address
 * \param jmp jump address to jump after decrunching
 * \param pagehi maximum page to use +1
 * \return output stream
 */
std::ostream &write_stub(std::ostream &out, int n, uint16_t size, uint16_t loadaddr, uint16_t jmp, uint8_t pagehi) {
  // Create a local copy.
  std::vector<uint8_t> stub(decrunchxipzstub_prg, decrunchxipzstub_prg + decrunchxipzstub_prg_len);
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
  // The end pointer must not point beyond the data: the stub stops when it reads past the
  // last byte, an extra byte would be decoded and written after the program.
  stub.at(POS_OF_END_OF_CDATA) = endptr & 0xFF;
  stub.at(POS_OF_END_OF_CDATA + 1) = (endptr >> 8) & 0xFF;
  // Assign the new jmp position.
  stub.at(POS_OF_JMP) = jmp & 0xFF;
  stub.at(POS_OF_JMP + 1) = (jmp >> 8) & 0xFF;
  // Assign the destination position (currently equals the jump).
  stub.at(POS_OF_DEST) = loadaddr & 0xFF;
  stub.at(POS_OF_DEST + 1) = (loadaddr >> 8) & 0xFF;
  // Set the maximal read position high-byte.
  stub.at(POS_OF_STOPREADING) = pagehi;
  stub.at(POS_OF_PAGEHI) = pagehi;
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
std::ostream &write_compression_table(std::ostream &out, int n, const HistorgramArray &histe) {
  for(int i = 0; i < (1 << n); ++i) {
    const HistEntry &curr = histe.at(i);
    out << curr.byte;
  }
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
    int fillbits = 8 - bit;
    // Pad with ones: a one is a literal flag needing eight more bits, which
    // can never be read, so the stub stops without decoding a spurious token.
    bitstore = (bitstore << fillbits) | ((1UL << fillbits) - 1);
    bit += fillbits;
  }
  while(bit >= 8) {
    out.push_back(static_cast<char>(bitstore >> (bit - 8)));
    bit -= 8;
  }
  return out;
}


/*! Output the most common bytes and their frequencies.
 *
 * \param shisto sorted histogram array
 */
static void output_64_common(const HistorgramArray &shisto) {
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
static int choose_optimal_n(const Data &data, const HistorgramArray &shisto) {
  // n=0 is not decodable by the stub, so n is always in 1..6. The 2^n
  // byte table stored with the bitstream is part of the size.
  int n = 1;
  float minsize = 0;

  for(int i = 1; i <= 6; ++i) {
    float f = std::ceil(calc_comp(i, data, shisto)) + (1 << i);
    if(i == 1 || f < minsize) {
      n = i;
      minsize = f;
    }
  }
  return n;
}


/*! LZP compressor class
 *
 */
class LzpCompressor : public Compressor {
public:
  using Compressor::Compressor;

protected:
    std::vector<uint8_t> compress() override {
        return crunch_lzp(data, cliargs.verbose_flag);
    }
    void write_stub(std::ofstream& out, const std::vector<uint8_t>& c,
                    uint16_t load, uint16_t jmp) override {
        write_lzp_stub(out, c.size(), load, jmp);
    }
};


/*! QADZ compressor class
 *
 */
class QadzCompressor : public Compressor {
public:
  using Compressor::Compressor;
protected:
    std::vector<uint8_t> compress() override {
        return crunch_qadz(data);
    }
    void write_stub(std::ofstream& out, const std::vector<uint8_t>& c,
                    uint16_t load, uint16_t jmp) override {
	write_qadz_stub(out, c.size(), load, jmp, cliargs.page_arg);
	
    }
};


/*! RATZ compressor class
 *
 */
class RatzCompressor : public Compressor {
public:
  using Compressor::Compressor;
protected:
  std::vector<uint8_t> compress() override {
    return crunch_ratz(data);
  }
  void write_stub(std::ofstream& out, const std::vector<uint8_t>& c,
		  uint16_t load, uint16_t jmp) override {
    write_ratz_stub(out, c.size(), load, jmp, cliargs.page_arg);
  }
};


/*! XipZ compressor class
 *
 */
class XipzCompressor : public Compressor {
public:
  using Compressor::Compressor;

protected:
  void pre_compress() override {
    shisto = calc_histo(data);
    output_64_common(shisto);
    optimal_bits = choose_optimal_n(data, shisto);
    std::cout << "Optimal number of bits: N=" << optimal_bits << std::endl;
    compbits = create_compression_bits(shisto, optimal_bits);
  }
  /* In raw mode there is no stub that knows n and carries the table, so
     * the stream starts with n (one byte) and the 2^n byte table. */
  std::vector<uint8_t> compress() override {
    std::vector<uint8_t> bits = create_compressed_data(data, compbits);
    if(!cliargs.raw_flag) {
      return bits;
    }
    std::vector<uint8_t> raw;
    raw.push_back(static_cast<uint8_t>(optimal_bits));
    for(int i = 0; i < (1 << optimal_bits); ++i) {
      raw.push_back(shisto.at(i).byte);
    }
    raw.insert(raw.end(), bits.begin(), bits.end());
    return raw;
  }
  void write_stub(std::ofstream& out, const std::vector<uint8_t>& c,
		  uint16_t load, uint16_t jmp) override {
    ::write_stub(out, optimal_bits, c.size(), load, jmp, cliargs.page_arg);
    std::cout << boost::format("Writing table, %d bytes...\n") % (1 << optimal_bits);
    write_compression_table(out, optimal_bits, shisto);
  }
  HistorgramArray shisto;
  CompressionBits compbits;
  int optimal_bits;
};


/*! \brief Create the compressor for an algorithm
 *
 * \param algorithm the algorithm to use
 * \param inpnam input file name
 * \param outnam output file name
 * \param args command line arguments
 * \return the compressor
 */
static std::unique_ptr<Compressor> make_compressor(enum enum_algorithm algorithm,
						  const std::string &inpnam,
						  const std::string &outnam,
						  const gengetopt_args_info &args) {
  auto warn_page = [&args](const char *name) {
    if(args.page_given) {
      std::cerr << "Warning! Page is ignored by " << name << ".\n";
    }
  };
  switch(algorithm) {
  case algorithm_arg_xipz:
    return std::make_unique<XipzCompressor>(inpnam, outnam, args);
  case algorithm_arg_qadz:
    return std::make_unique<QadzCompressor>(inpnam, outnam, args);
  case algorithm_arg_ratz:
    return std::make_unique<RatzCompressor>(inpnam, outnam, args);
  case algorithm_arg_lzp:
    warn_page("LZP");
    return std::make_unique<LzpCompressor>(inpnam, outnam, args);
  case algorithm_arg_lzp2:
    warn_page("LZP2");
    return std::make_unique<Lzp2Compressor>(inpnam, outnam, args);
  case algorithm_arg_lzp3:
    warn_page("LZP3");
    return std::make_unique<Lzp3Compressor>(inpnam, outnam, args);
  case algorithm_arg_lzp4:
    warn_page("LZP4");
    return std::make_unique<Lzp4Compressor>(inpnam, outnam, args);
  case algorithm_arg_lzp5:
    warn_page("LZP5");
    return std::make_unique<Lzp5Compressor>(inpnam, outnam, args);
  case algorithm_arg_tc:
    warn_page("TC");
    return std::make_unique<TcCompressor>(inpnam, outnam, args);
  case algorithm_arg_rle:
    return std::make_unique<RleCompressor>(inpnam, outnam, args);
  case algorithm_arg_bpe:
    return std::make_unique<BpeCompressor>(inpnam, outnam, args);
  case algorithm_arg_squz:
    return std::make_unique<SquzCompressor>(inpnam, outnam, args);
  case algorithm_arg_lzw:
    return std::make_unique<LzwCompressor>(inpnam, outnam, args);
  case algorithm_arg_lzrc:
    return std::make_unique<LzrcCompressor>(inpnam, outnam, args);
  case algorithm_arg_lzh16:
    return std::make_unique<Lzh16Compressor>(inpnam, outnam, args);
  case algorithm__NULL:
    throw std::logic_error("algorithm vanished");
  }
  throw std::logic_error("mismatch between command line and code");
}

/*! \brief Encode data as base64 (RFC 4648, with padding)
 */
static std::string base64_encode(const std::vector<uint8_t> &data) {
  static const char alphabet[] = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
  std::string out;
  out.reserve((data.size() + 2) / 3 * 4);
  for(size_t i = 0; i < data.size(); i += 3) {
    uint32_t v = data[i] << 16;
    if(i + 1 < data.size()) v |= data[i + 1] << 8;
    if(i + 2 < data.size()) v |= data[i + 2];
    out += alphabet[(v >> 18) & 0x3F];
    out += alphabet[(v >> 12) & 0x3F];
    out += i + 1 < data.size() ? alphabet[(v >> 6) & 0x3F] : '=';
    out += i + 2 < data.size() ? alphabet[v & 0x3F] : '=';
  }
  return out;
}

/*! \brief Debug output: crunch the input with several algorithms
 *
 * The input is crunched with every algorithm of a comma separated
 * list (or "all") and a JSON object is printed, which maps the name of
 * the algorithm to the base64 encoded crunched data. No decrunching
 * stub is written, the data is the same as with the option -r. This
 * is mostly useful for raw data (option -d). An algorithm that fails
 * is reported on stderr and has the value null.
 *
 * \param list comma separated algorithm names or "all"
 * \param inpnam input file name
 * \param args command line arguments
 * \return return value for the CLI
 */
static int debug_json(const std::string &list, const std::string &inpnam,
		      const gengetopt_args_info &args) {
  std::vector<std::string> names;
  std::istringstream iss(list);
  for(std::string name; std::getline(iss, name, ','); ) {
    if(name == "all") {
      for(int i = 0; cmdline_parser_algorithm_values[i]; ++i) {
	names.push_back(cmdline_parser_algorithm_values[i]);
      }
    } else if(!name.empty()) {
      names.push_back(name);
    }
  }
  // Remove duplicates, a JSON object should not have the same key twice.
  std::vector<std::string> unique;
  for(const auto &name : names) {
    if(std::find(unique.begin(), unique.end(), name) == unique.end()) {
      unique.push_back(name);
    }
  }
  if(unique.empty()) {
    std::cerr << "Error! No algorithm given for --debug-json.\n";
    return RETURN_NO_ALGORITHM;
  }
  // Only the crunched data is wanted, as with -r.
  gengetopt_args_info rawargs = args;
  rawargs.raw_flag = 1;
  // The algorithms are chatty, keep stdout clean for the JSON.
  std::streambuf *coutbuf = std::cout.rdbuf(std::cerr.rdbuf());
  std::ostringstream json;
  json << "{";
  int ret = 0;
  bool first = true;
  for(const auto &name : unique) {
    json << (first ? "\n  \"" : ",\n  \"") << name << "\": ";
    first = false;
    int idx = -1;
    for(int i = 0; cmdline_parser_algorithm_values[i]; ++i) {
      if(name == cmdline_parser_algorithm_values[i]) {
	idx = i;
      }
    }
    try {
      if(idx < 0) {
	throw std::runtime_error("unknown algorithm");
      }
      auto compressor = make_compressor(static_cast<enum enum_algorithm>(idx), inpnam, "", rawargs);
      const std::string encoded = base64_encode(compressor->crunch());
      json << "\"" << encoded << "\"";
    }
    catch(const std::exception &e) {
      std::cerr << "Exception (" << name << "): " << e.what() << std::endl;
      json << "null";
      ret = RETURN_UNKNOWN_EXCEPTION;
    }
  }
  json << "\n}\n";
  std::cout.rdbuf(coutbuf);
  std::cout << json.str();
  return ret;
}

/*!\brief main function using xip
 *
 * Main function which takes a single file name as an argument.
 */
int main(int argc, char **argv) {
  gengetopt_args_info args;
  std::unique_ptr<Compressor> compmain;
  int ret = RETURN_UNKNOWN_EXCEPTION; // See below!

  if(cmdline_parser(argc, argv, &args) != 0) {
    return 1;
  } else {
    if(args.inputs_num < 1) {
      std::cerr << "At least one filename must be provided!\n";
      return RETURN_NO_FILENAME;
    }
    if(args.debug_json_given) {
      try {
	return debug_json(args.debug_json_arg, args.inputs[0], args);
      }
      catch(const std::exception &e) {
	std::cerr << "Exception: " << e.what() << std::endl;
	return RETURN_UNKNOWN_EXCEPTION;
      }
    }
    std::cout << "XipZ Version " << CMDLINE_PARSER_VERSION << std::endl;
    if(args.page_arg > 0xa0) {
      // In this case part of the data will end up under the ROM. As
      // we are using a routine from BASIC ROM and the ROM is not
      // switched off during decrunching this is considered an error.
      std::cerr << "Error! Data may end up below the ROM aborting!\n";
      return RETURN_DATA_BELOW_ROM;
    }
    try {
      std::string inpnam(args.inputs[0]);
      std::string outnam;
      if(args.inputs_num < 2) {
	outnam = inpnam + (args.raw_given ? ".raw" : ".prg");
      } else {
	outnam = args.inputs[1];
      }
      compmain = make_compressor(args.algorithm_arg, inpnam, outnam, args);
      if(!compmain) {
	throw std::logic_error("compression main has been lost");
      }
      ret = compmain->run();
    }
    catch(const std::exception &e) {
      std::cerr << "Exception: " << e.what() << std::endl;
      // Return value defaults to RETURN_UNKNOWN_EXCEPTION.
    }
  }
  return ret;
}
