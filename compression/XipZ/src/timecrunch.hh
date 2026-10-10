#ifndef __TIMECRUNCH_HH_20261010__
#define __TIMECRUNCH_HH_20261010__
#include <cstdint>
#include <vector>

/*! \file
 *
 * \brief tc: the format of the Time Cruncher V5 (Matcham/Network, Tim Rogers).
 *
 * The format was taken from the disassembly of the decruncher on
 * https://codebase64.net/doku.php?id=base:2mhz_time_crunch_v5_disassembled
 * The crunching is new, the original brute force searches the matches in
 * the memory, here the matches come from hash chains and the parse is
 * optimal.
 *
 * It is an LZ77 where the data is decrunched backwards: the decoder
 * reads the packed data downwards and writes the output downwards
 * starting with the last byte. The packed data is therefore placed at
 * the bottom of the output area and the output overtakes it from above
 * (the "gap" that is needed is computed by the compressor).
 *
 * The stream consists of bits (read MSB first from bytes, the byte
 * is fetched when the first of its bits is needed) and of raw literal
 * bytes. The literal bytes are not part of the bit stream: they are
 * taken from the stream at the moment when they are needed, below the
 * last byte that was fetched for bits. In the order of decoding there
 * are tokens
 *
 * \code
 * token (3 bits)
 *   0      no literals, go on with the match
 *   1..5   a run of this many literal bytes
 *   6      a run of 6 + 4 bits literal bytes (6 to 21)
 *   7      8 bits v: 6 + v literal bytes (6 to 255) if v < 250,
 *          otherwise the escape: v-249 bits give a number a, the next
 *          a+1 literal runs are not followed by a match
 * match (after a run of literals or a token of zero)
 *   1 <8 bits off>                       length 2
 *   0 0 c <off>                          length 3
 *   0 1 0 <4 bits w> c <off>             length w+4
 *   0 1 1 <8 bits w> c <off>             length w+4 (w <= 251, the length
 *                                        is a byte in the decoder)
 *   c is 0 and off has 8 bits or c is 1 and off has 8+STEP bits;
 *   the length 2 has only the 8 bit offset.
 * \endcode
 *
 * The distance of a match is off + length, so the match never overlaps
 * its destination. STEP (1 to 8) is a parameter that is stored in the
 * decoder.
 *
 * The decoder stops when it needs a byte below the packed data. The
 * unused bits in the last byte are zero.
 */

/*! \brief Result of crunching. */
struct TcResult {
  std::vector<uint8_t> data; //!< crunched data in memory order, the decoder starts at the last byte
  int step;		     //!< STEP, the long offsets have 8+step bits
  long gap;		     //!< data start = end of the output - size + gap (gap <= 0)
};

/*! \brief Crunch data.
 *
 * \param in data to crunch
 * \param verbose print some statistics
 * \return the crunched data
 */
TcResult crunch_tc(const std::vector<uint8_t> &in, bool verbose);

/*! \brief Reference decoder.
 *
 * The packed data is put into a memory image where the output is
 * written, so the overlap is tested, too.
 *
 * \param c crunched data
 * \param step STEP
 * \param gap gap as returned by crunch_tc
 * \param outsize number of bytes of the output
 * \return the output
 */
std::vector<uint8_t> decrunch_tc(const std::vector<uint8_t> &c, int step, long gap, size_t outsize);

#endif
