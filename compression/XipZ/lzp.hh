#ifndef __LZP_HH_20260527__
#define __LZP_HH_20260527__

/*! \file
 *
 * \brief Crunching and writing stub for LZP.
 */

/*! Compress data using the LZ77/LZP-alike compression.
 *
 * \param data binary data to compress
 * \param verbose be more verbose
 * \return compressed data
 */
std::vector<uint8_t> crunch_lzp(const Data &data, bool verbose);

/*! \brief write the decrunch stub
 *
 * The decrunching stub is written and all parameters like decrunching
 * address and jump address are adjusted. Please take care to adjust
 * all the defines above otherwise the code will probably just
 * crash. It may also fry your cat so be careful!
 *
 * \param out output stream to write to
 * \param size number of compressed bytes
 * \param loadaddr original load address of the data
 * \param jmp jump address to jump after decrunching
 * \return output stream
 */
std::ostream &write_lzp_stub(std::ostream &out, uint16_t size, uint16_t loadaddr, uint16_t jmp);
#endif
