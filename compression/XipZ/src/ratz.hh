#ifndef __RATZ_HH_20261009__
#define __RATZ_HH_20261009__
#include <cstdint>
#include <ostream>
#include <vector>
#include "data.hh"

/*! \file
 *
 * \brief Crunching for ratz, a fast byte-aligned LZ format.
 *
 * Every token starts with one control byte \c b, the stream ends with
 * the token $00. Matches are copied forwards byte by byte, therefore
 * an offset smaller than the length repeats data like a run-length
 * encoding.
 *
 * | Control byte | Token                | Bytes after it           | Length        |
 * |--------------|----------------------|--------------------------|---------------|
 * | $00          | end of stream        | -                        | -             |
 * | $01-$7F      | literal run          | b raw bytes              | b (1-127)     |
 * | $80-$9F      | match, last offset   | -                        | b-$80+1 (1-32)|
 * | $A0-$DF      | match, near          | offset-1 (1 byte)        | b-$A0+2 (2-65)|
 * | $E0-$FF      | match, far           | offset-1 (lo, hi)        | b-$E0+3 (3-34)|
 *
 * "Last offset" is the offset of the most recent match of any kind. A
 * match at the last offset is not allowed before the first match.
 * Near offsets are 1-256, far offsets are 1-65536.
 */

//! Number of arrivals kept per position by the parser (beam width).
#define RATZ_BEAM 16

/*! Compress data using the ratz format.
 *
 * The result is decoded again and compared with the input, a
 * std::logic_error is thrown if they differ.
 *
 * \param data binary data to compress
 * \param beam beam width of the parser, a larger value may be smaller but slower
 * \return compressed data
 */
std::vector<uint8_t> crunch_ratz(const Data &data, int beam = RATZ_BEAM);

/*! Decompress a ratz stream.
 *
 * Reference implementation of the format, used to check the encoder.
 *
 * \param stream compressed data including the end token
 * \return decompressed data
 * \throw std::runtime_error if the stream is invalid
 */
std::vector<uint8_t> decrunch_ratz(const std::vector<uint8_t> &stream);

/*! \brief write the decrunch stub
 *
 * The decrunching stub is written and all parameters like decrunching
 * address and jump address are adjusted. The patch positions are
 * taken from decrunchratzstub.prg.label, see the comment in the
 * implementation.
 *
 * \param out output stream to write to
 * \param size number of compressed bytes
 * \param loadaddr original load address of the data
 * \param jmp jump address to jump after decrunching
 * \param pagehi maximum page to use +1
 * \return output stream
 */
std::ostream &write_ratz_stub(std::ostream &out, uint16_t size, uint16_t loadaddr, uint16_t jmp, uint8_t pagehi);
#endif
