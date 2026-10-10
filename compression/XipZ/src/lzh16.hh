#ifndef __LZH16_HH_20261009__
#define __LZH16_HH_20261009__
#include <cstdint>
#include <ostream>
#include <vector>
#include "data.hh"

/*! \file
 *
 * \brief lzh16: LZ77 with a bit stream and a history of 16 offsets.
 *
 * A small and fast to decode LZ77 without an entropy coder. The bits
 * of the stream are read MSB first, the stream has no header. A token
 * is one of
 *
 * \code
 * 0 <8 bits>                         literal
 * 1 0 <offset> <length>              match with a new offset
 * 1 1 <4 bits slot> <length>         match with an offset of the history
 * \endcode
 *
 * The history is a ring of 16 distances (initially all 1) and a
 * counter (initially 0). A match with a new offset is stored in the
 * slot of the counter (the oldest entry) and the counter is incremented
 * modulo 16. A match with a history slot does not change the history.
 *
 * Numbers are interleaved Elias-gamma codes: a number v >= 1 with k =
 * floor(log2(v)) is written as the k pairs "1 b" (b are the bits below
 * the leading one, msb first) followed by a zero. The decoder starts
 * with one and shifts in the bits in an eight bit register, so v is
 * at most 255. If the register overflows (eight pairs) the number is
 * invalid.
 *
 * The length is the gamma number len-1, so len is 2 to 256. The offset
 * is coded as the gamma number hi+1 followed by the low byte lo as eight
 * raw bits; the distance is (hi*256 + lo + 1), so 1 to 65280 bytes back.
 * The end of the data is marked by a match with a new offset where the
 * gamma number has the eight pairs "1 0", no length follows.
 *
 * The match copies len bytes from distance bytes back, forward, the
 * areas may overlap. Bits behind the end marker are padding.
 */

/*! Compress data using lzh16.
 *
 * The compressor is an optimal parse that keeps the best few
 * histories at every position. The result is decoded again and compared
 * with the input, a std::logic_error is thrown if they differ.
 *
 * \param data binary data to compress
 * \return compressed data
 */
std::vector<uint8_t> crunch_lzh16(const Data &data);

/*! Decompress a lzh16 stream.
 *
 * Reference implementation of the format, used to check the encoder.
 *
 * \param stream compressed data
 * \param lead if not null, receives the maximum of (bytes written - bytes read)
 * \return decompressed data
 * \throw std::runtime_error if the stream is invalid
 */
std::vector<uint8_t> decrunch_lzh16(const std::vector<uint8_t> &stream, long *lead = nullptr);

/*! \brief write the decrunch stub
 *
 * The patch positions are taken from decrunchlzh16stub.prg.label, see
 * the comment in the implementation.
 *
 * \param out output stream to write to
 * \param size number of compressed bytes
 * \param loadaddr original load address of the data
 * \param jmp jump address to jump after decrunching
 * \param pagehi maximum page to use +1
 * \return output stream
 */
std::ostream &write_lzh16_stub(std::ostream &out, uint16_t size, uint16_t loadaddr, uint16_t jmp, uint8_t pagehi);
#endif
