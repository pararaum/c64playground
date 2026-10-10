#ifndef __BPE_HH_20261009__
#define __BPE_HH_20261009__
#include <cstdint>
#include <ostream>
#include <vector>
#include "data.hh"

/*! \file
 *
 * \brief Crunching with byte pair encoding (global table).
 *
 * The stream consists of:
 *
 *  - the escape byte \c ESC,
 *  - the end byte \c END,
 *  - the pair table: 32 bitmap bytes, bit b of byte i is set if the
 *    value 8*i+b is a pair code. Every bitmap byte is directly followed
 *    by the (left, right) bytes of the pair codes it marks, in
 *    ascending order of the codes,
 *  - the tokens, and finally \c ESC \c END.
 *
 * Every byte value is a token. A token which is not defined in the
 * list is a literal and stands for itself. A pair token expands to
 * its left token followed by its right token, recursively. The escape
 * byte is not a token, \c ESC followed by a byte writes this byte raw
 * (needed for the values used as pair codes and for \c ESC itself).
 * \c END is a literal, therefore \c ESC \c END is never a raw byte
 * and marks the end of the stream.
 *
 * The decoder uses the hardware stack to expand a token, so the
 * expansion depth of a pair is limited to BPE_MAX_DEPTH.
 */

//! Maximum nesting depth of a pair token, a literal has the depth zero.
#define BPE_MAX_DEPTH 64
//! Number of bytes needed to describe one pair in the header (the bitmap is a fixed cost).
#define BPE_PAIR_COST 2

/*! Compress data using byte pair encoding.
 *
 * The result is decoded again and compared with the input, a
 * std::logic_error is thrown if they differ.
 *
 * \param data binary data to compress
 * \return compressed data
 */
std::vector<uint8_t> crunch_bpe(const Data &data);

/*! Decompress a bpe stream.
 *
 * Reference implementation of the format, used to check the encoder.
 *
 * \param stream compressed data including the end marker
 * \param lead if not null, the maximum of (bytes written - bytes read) is stored here
 * \param stack if not null, the maximum number of stack bytes needed is stored here
 * \return decompressed data
 * \throw std::runtime_error if the stream is invalid
 */
std::vector<uint8_t> decrunch_bpe(const std::vector<uint8_t> &stream, long *lead = nullptr, int *stack = nullptr);

/*! \brief write the decrunch stub
 *
 * The patch positions are taken from decrunchbpestub.prg.label, see
 * the comment in the implementation.
 *
 * \param out output stream to write to
 * \param size number of compressed bytes
 * \param loadaddr original load address of the data
 * \param jmp jump address to jump after decrunching
 * \param pagehi maximum page to use +1
 * \return output stream
 */
std::ostream &write_bpe_stub(std::ostream &out, uint16_t size, uint16_t loadaddr, uint16_t jmp, uint8_t pagehi);
#endif
