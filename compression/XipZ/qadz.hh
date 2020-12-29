#ifndef __QADZ_HH_20201229__
#define __QADZ_HH_20201229__

/*! Compress data using the LZ77-alike compression.
 *
 * \param data binary data to compress
 * \return compressed data
 */
std::vector<uint8_t> crunch_qadz(const Data &data);
#endif
