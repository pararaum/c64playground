#ifndef __COMPRESSION_HH_20260615__
#define __COMPRESSION_HH_20260615__
#include "cmdline.h"
#include "data.hh"
#include <string>
#include <vector>
#include <fstream>

//! Abstract compressor base — defines the contract
//
// This contains the general functions to be used in all compression
// algorithms.
class Compressor {
public:
  typedef std::vector<uint8_t> crunched_data_type;

/*!\brief Constructor
 *
 * \param input input filename
 * \param output outout filename
 * \param verbose true if more verbose output
 * \param raw should the compressed data be written raw (without decompression stub)
 * \param jump jump address, -1 = equal to load address
 * \param pagehi maximum page to use +1
 * \param exloadaddr extract load address from data?
 */
  Compressor(const std::string& input, const std::string& output,
	     const gengetopt_args_info &cliargs);

  virtual ~Compressor() = default;

  /*! Run the compressor
   *
   * \return return value for the CLI
   */
  int run();

protected:
  std::string inputname, outputname;
  const gengetopt_args_info &cliargs;
  Data data;

  virtual void pre_compress() {}
  virtual crunched_data_type compress() = 0;
  virtual void write_stub(std::ofstream&, const std::vector<uint8_t>&,
			  uint16_t loadaddr, uint16_t jumpaddr) = 0;
};


/*! RLE compressor class
 *
 * This is a variant of a run-length compression algorithm. This
 * algorithm will compress repeating bytes but it will not use a
 * special marker byte, instead if two consecutive bytes are the same
 * this is the marker that a compression run starts. After two
 * consecutive bytes the following byte gives the number of copies
 * minus one. So a 1 indicates a pair of bytes and a 255 indicates a
 * run of 256 bytes. A length of zero indicates the end of file.
 */
class RleCompressor : public Compressor {
public:
  using Compressor::Compressor;

protected:
  crunched_data_type compress() override;
  void write_stub(std::ofstream&, const std::vector<uint8_t>&,
		  uint16_t, uint16_t) override;
};


/*! LZP2 compressor class
 *
 */
class Lzp2Compressor : public Compressor {
public:
  using Compressor::Compressor;

protected:
  std::vector<uint8_t> compress() override;
  void write_stub(std::ofstream& out, const std::vector<uint8_t>& c,
		  uint16_t load, uint16_t jmp) override;
};

#endif
