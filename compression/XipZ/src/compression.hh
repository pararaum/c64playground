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

  /*! Crunch the data only
   *
   * \return the crunched data without any stub
   */
  crunched_data_type crunch() {
    pre_compress();
    return compress();
  }

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

/*! \brief write the decrunch stub for RLE
 *
 * The patch positions are taken from decrunchrlestub.prg.label, see
 * the comment in rle.cc.
 *
 * \param out output stream to write to
 * \param size number of compressed bytes
 * \param loadaddr original load address of the data
 * \param jmp jump address to jump after decrunching
 * \param pagehi maximum page to use +1
 * \return output stream
 */
std::ostream &write_rle_stub(std::ostream &out, uint16_t size, uint16_t loadaddr, uint16_t jmp, uint8_t pagehi);


/*! BPE compressor class
 *
 * Byte pair encoding with one global table, see bpe.hh. The decoder
 * lives in the stack page and the table in the text screen
 * ($0400-$05FF).
 */
class BpeCompressor : public Compressor {
public:
  using Compressor::Compressor;

protected:
  crunched_data_type compress() override;
  void write_stub(std::ofstream&, const std::vector<uint8_t>&,
		  uint16_t, uint16_t) override;
};


/*! squz compressor class
 *
 * Byte pair encoding with an adaptive binary range coder, see squz.hh.
 * The decoder lives in the stack page and the cassette buffer, the
 * tables in the text screen ($0400-$07DF).
 */
class SquzCompressor : public Compressor {
public:
  using Compressor::Compressor;

protected:
  crunched_data_type compress() override;
  void write_stub(std::ofstream&, const std::vector<uint8_t>&,
		  uint16_t, uint16_t) override;
};


/*! lzw compressor class
 *
 * LZW whose dictionary points into the output, see lzw.hh. The decoder
 * lives in the cassette buffer, the dictionary in the text screen
 * ($0400-$07FF).
 */
class LzwCompressor : public Compressor {
public:
  using Compressor::Compressor;

protected:
  crunched_data_type compress() override;
  void write_stub(std::ofstream&, const std::vector<uint8_t>&,
		  uint16_t, uint16_t) override;
};


/*! lzrc compressor class
 *
 * LZ77 with an adaptive binary range coder (the one of squz), see
 * lzrc.hh. The decoder lives in the stack page and the cassette
 * buffer, the probabilities in the text screen.
 */
class LzrcCompressor : public Compressor {
public:
  using Compressor::Compressor;

protected:
  crunched_data_type compress() override;
  void write_stub(std::ofstream&, const std::vector<uint8_t>&,
		  uint16_t, uint16_t) override;
};


/*! lzh16 compressor class
 *
 * LZ77 with a bit stream and a history of 16 offsets, see lzh16.hh.
 * The decoder lives in the cassette buffer, the history in the text
 * screen ($0400-$041F).
 */
class Lzh16Compressor : public Compressor {
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

/*! LZP3 compressor class
 *
 * Order-2 prediction with an order-1 prediction as a fallback and a
 * second chance.
 */
class Lzp3Compressor : public Compressor {
public:
  using Compressor::Compressor;

protected:
  std::vector<uint8_t> compress() override;
  void write_stub(std::ofstream& out, const std::vector<uint8_t>& c,
		  uint16_t load, uint16_t jmp) override;
};

/*! LZP4 compressor class
 *
 * The original LZP, a hash table of positions in the output predicts
 * runs.
 */
class Lzp4Compressor : public Compressor {
public:
  using Compressor::Compressor;

protected:
  std::vector<uint8_t> compress() override;
  void write_stub(std::ofstream& out, const std::vector<uint8_t>& c,
		  uint16_t load, uint16_t jmp) override;
};

/*! LZP5 compressor class
 *
 * One byte predicted by an order-1 context, a 256 byte table indexed
 * by the last byte.
 */
class Lzp5Compressor : public Compressor {
public:
  using Compressor::Compressor;

protected:
  std::vector<uint8_t> compress() override;
  void write_stub(std::ofstream& out, const std::vector<uint8_t>& c,
		  uint16_t load, uint16_t jmp) override;
};

/*! tc compressor class
 *
 * The format of the Time Cruncher V5, an LZ77 that is decrunched
 * backwards, see timecrunch.hh. The stub lives in the text screen.
 */
class TcCompressor : public Compressor {
public:
  using Compressor::Compressor;

protected:
  crunched_data_type compress() override;
  void write_stub(std::ofstream& out, const std::vector<uint8_t>& c,
		  uint16_t load, uint16_t jmp) override;

private:
  unsigned dest_ = 0;	//!< where the packed data is moved to
  int step_ = 1;	//!< STEP, see timecrunch.hh
};

#endif
