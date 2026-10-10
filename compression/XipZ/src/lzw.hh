#ifndef __LZW_HH_20261009__
#define __LZW_HH_20261009__
#include <vector>
#include <iostream>
#include <cstdint>
#include "data.hh"

/*! \file
 * \brief lzw: LZW with the dictionary pointing into the output.
 *
 * The compressed data is a sequence of 9 bit codes:
 *  - 0..255 are literal bytes,
 *  - 256..509 are the dictionary entries 0..253,
 *  - 510 clears the dictionary,
 *  - 511 ends the data.
 *
 * After every code except the first one after a clear, an entry is
 * written to the dictionary slot NEXT (NEXT counts 0..253 and wraps
 * around, the oldest entry is overwritten): the string of the previous
 * code plus the first byte of the string of the current code. It is
 * added before the current code is decoded, so the current code may
 * refer to it. As both strings are adjacent in the output an entry is
 * just the output address and the length of the previous string plus
 * one, and a string is decoded by copying forward from the output (the
 * areas may overlap). Strings are at most 254 bytes long.
 *
 * Stream layout: eight codes at a time. First a byte with the ninth
 * bits of the codes (the first code in bit 7), then the eight low
 * bytes. The last group may be shorter.
 *
 * The decoder keeps no other state, so the compressor is free to choose
 * any sequence of codes which reproduces the data. See lzw.cc for the
 * search.
 */

/*! Compress data using lzw.
 *
 * The result is decoded again and compared with the input, a
 * std::logic_error is thrown if they differ.
 *
 * \param data binary data to compress
 * \return compressed data
 */
std::vector<uint8_t> crunch_lzw(const Data &data);

/*! Decompress a lzw stream.
 *
 * Reference implementation of the format, used to check the encoder.
 *
 * \param stream compressed data
 * \param lead if not null, receives the maximum of (bytes written - bytes read)
 * \return decompressed data
 * \throw std::runtime_error if the stream is invalid
 */
std::vector<uint8_t> decrunch_lzw(const std::vector<uint8_t> &stream, long *lead = nullptr);

/*! \brief write the decrunch stub
 *
 * The patch positions are taken from decrunchlzwstub.prg.label, see
 * the comment in the implementation.
 *
 * \param out output stream to write to
 * \param size number of compressed bytes
 * \param loadaddr original load address of the data
 * \param jmp jump address to jump after decrunching
 * \param pagehi maximum page to use +1
 * \return output stream
 */
std::ostream &write_lzw_stub(std::ostream &out, uint16_t size, uint16_t loadaddr, uint16_t jmp, uint8_t pagehi);
#endif
