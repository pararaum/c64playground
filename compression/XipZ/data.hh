#ifndef __DATA_HH_2W020__
#define __DATA_HH_2W020__
#include <vector>
#include <inttypes.h>

/*! \file
 *
 * \brief Binary data handling.
 * 
 * Data structure to keep the binary data in memory. It has provisions
 * for the handling the load address of a C64 binary.
 */


/*! \brief Input data type.
 *
 * This class stores the loaded data and extracts the original load
 * address. The data can be accessed via the public data member data.
 */
class Data {
protected:
  uint16_t loadaddr; //!< original load address
  std::vector<uint8_t> data; //!< binary data without the load address

public:
  /*! \brief Constructor with raw binary data as input.
   *
   * Constructs the objects with the given binary data. The load
   * address is extracted immediately.
   *
   * If exloadaddr is set to false the load address is *not* extracted
   * but set to zero. This may be useful for raw binary data.
   *
   * \param inp raw binary data, must be at least three bytes long
   * \param exloadaddr extract load address from data?
   */
  Data(const std::vector<uint8_t> &inp, bool exloadaddr) {
    if(inp.size() < 3) {
      throw std::underflow_error("not enough bytes for Data");
    }
    if(exloadaddr) {
      loadaddr = (static_cast<uint16_t>(inp.at(1)) << 8) | inp.at(0);
      data.resize(inp.size() - 2);
      std::copy(inp.begin() + 2, inp.end(), data.begin());
    } else {
      loadaddr = 0;
      data.resize(inp.size());
      std::copy(inp.begin(), inp.end(), data.begin());
    }
  }
  /*! \brief get load address of data
   *
   * Just returns the deducted load address.
   *
   * \return load address as an 16 bit unsigned integer.
   */
  uint16_t get_loadaddr() const { return loadaddr; }
  /*! \brief get data size
   *
   * Return the number of bytes in the data structure.
   *
   * \return number of bytes
   */
  std::vector<uint8_t>::size_type size() const { return data.size(); }
  uint8_t operator[](unsigned int i) const { return data.at(i); }
  std::vector<uint8_t>::const_iterator begin() const { return data.begin(); }
  std::vector<uint8_t>::const_iterator end() const { return data.end(); }
};


#endif
