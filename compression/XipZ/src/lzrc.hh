#ifndef __LZRC_HH_20261009__
#define __LZRC_HH_20261009__
#include <cstdint>
#include <ostream>
#include <vector>
#include "data.hh"

/*! \file
 *
 * \brief lzrc: LZ77 with an adaptive binary range coder.
 *
 * The stream is an arithmetic coded sequence of tokens, there is no
 * header. The range coder and the probabilities are the ones of squz
 * (see squz.hh) but with a slower adaptation: eight bit probabilities
 * starting at 128, updated by p += (256-p+15)>>4 for a one and
 * p -= (p+15)>>4 for a zero (kept in 1..255), a 16 bit
 * range and code, the initial code are the first 16 bits of the stream.
 *
 * A token starts with the flag "match" (context: the previous token was
 * a match). A zero is followed by a literal: the eight bits of the byte
 * in a binary tree (msb first) of 255 probabilities, the tree is chosen
 * by the same context as the flag.
 *
 * A one is followed by the flag "repeat". If it is set the match uses
 * the offset of the previous match (initially 1) and the length is
 * the Elias-gamma number len (at least 1) coded with the length-repeat
 * context. Otherwise the offset is the Elias-gamma number offset (1 to
 * 65535) coded with the offset context, followed by the number len-1
 * (len is at least 2) with the length-new context. An offset number with
 * 16 bits (the unary part has 16 ones) marks the end of the data, no
 * length follows.
 *
 * An Elias-gamma number v >= 1 with k = floor(log2(v)) is coded as k
 * ones and a zero (the unary part, every position has its own
 * probability, 0..16) and then the k bits below the leading one, msb
 * first, every bit position has its own probability (0..14). A
 * context therefore has 32 probabilities.
 *
 * The match copies len bytes from offset bytes back, forward, the
 * areas may overlap.
 */

/*! Compress data using lzrc.
 *
 * The result is decoded again and compared with the input, a
 * std::logic_error is thrown if they differ.
 *
 * \param data binary data to compress
 * \return compressed data
 */
std::vector<uint8_t> crunch_lzrc(const Data &data);

/*! Decompress a lzrc stream.
 *
 * Reference implementation of the format, used to check the encoder.
 *
 * \param stream compressed data
 * \param lead if not null, receives the maximum of (bytes written - bytes read)
 * \param padding value (0 or 1) of the bits read behind the end of the stream
 * \return decompressed data
 * \throw std::runtime_error if the stream is invalid
 */
std::vector<uint8_t> decrunch_lzrc(const std::vector<uint8_t> &stream, long *lead = nullptr, int padding = 0);

/*! \brief write the decrunch stub
 *
 * The patch positions are taken from decrunchlzrcstub.prg.label, see
 * the comment in the implementation.
 *
 * \param out output stream to write to
 * \param size number of compressed bytes
 * \param loadaddr original load address of the data
 * \param jmp jump address to jump after decrunching
 * \param pagehi maximum page to use +1
 * \return output stream
 */
std::ostream &write_lzrc_stub(std::ostream &out, uint16_t size, uint16_t loadaddr, uint16_t jmp, uint8_t pagehi);
#endif
