#ifndef __SQUZ_HH_20261009__
#define __SQUZ_HH_20261009__
#include <cstdint>
#include <ostream>
#include <vector>
#include "data.hh"

/*! \file
 *
 * \brief squz: byte pair encoding with an adaptive binary range coder.
 *
 * The stream is a header byte followed by an arithmetic coded
 * sequence of tokens.
 *
 * Header byte: the number of pairs n (0-31). The pair tree has always
 * five bits.
 *
 * A token is one flag bit (0 = literal, 1 = pair) and then either the
 * eight bits of the literal byte, msb first, or the five bits of the
 * pair index. The pair index n is the end of data token. Every bit
 * is coded with its own adaptive probability, selected by the
 * context (the position inside the current 6502 instruction, 0-2),
 * the kind of the token and the number of the node in the binary
 * tree of the token bits.
 *
 * The coded tokens are: the dictionary (for every pair its left and
 * right operand token, which refer to literals or earlier pairs),
 * then the data tokens, then the end of data token. Dictionary
 * tokens use the context zero.
 *
 * Probabilities are eight bits (probability of a one bit times 256),
 * start at 128 and are updated with (p += (256-p+7)>>3 for a one,
 * p -= (p+7)>>3 for a zero, kept in 1..255). The range coder has a
 * 16 bit range and code. For a bit, bound = (range>>8)*p; a one is
 * decoded if code < bound (range = bound), else code -= bound and
 * range -= bound. The range is renormalised bit by bit while it is
 * below $8000. The initial range is $FFFF, the code starts with the
 * first 16 bits of the stream.
 *
 * The position inside an instruction is advanced for every output
 * byte: at position zero the instruction length is derived from the
 * opcode (see squz_oplen), the position then counts up and wraps at
 * the length.
 */

//! Maximum number of pairs.
#define SQUZ_MAX_PAIRS 31
//! Maximum nesting depth of a pair (a literal has depth zero).
#define SQUZ_MAX_DEPTH 8

/*! Length of the 6502 instruction with the given opcode.
 *
 * Illegal opcodes are treated like their legal neighbours, this is
 * only used as context so it does not have to be exact.
 */
int squz_oplen(int opcode);

/*! Compress data using squz.
 *
 * The result is decoded again and compared with the input, a
 * std::logic_error is thrown if they differ.
 *
 * \param data binary data to compress
 * \return compressed data
 */
std::vector<uint8_t> crunch_squz(const Data &data);

/*! Decompress a squz stream.
 *
 * Reference implementation of the format, used to check the encoder.
 *
 * \param stream compressed data
 * \param maxdepth if not null, receives the maximum number of nested pairs seen
 * \param lead if not null, receives the maximum of (bytes written - bytes read)
 * \param padding value (0 or 1) of the bits read behind the end of the stream
 * \return decompressed data
 * \throw std::runtime_error if the stream is invalid
 */
std::vector<uint8_t> decrunch_squz(const std::vector<uint8_t> &stream, int *maxdepth = nullptr, long *lead = nullptr, int padding = 0);

/*! \brief write the decrunch stub
 *
 * The patch positions are taken from decrunchsquzstub.prg.label, see
 * the comment in the implementation.
 *
 * \param out output stream to write to
 * \param size number of compressed bytes
 * \param loadaddr original load address of the data
 * \param jmp jump address to jump after decrunching
 * \param pagehi maximum page to use +1
 * \return output stream
 */
std::ostream &write_squz_stub(std::ostream &out, uint16_t size, uint16_t loadaddr, uint16_t jmp, uint8_t pagehi);
#endif
