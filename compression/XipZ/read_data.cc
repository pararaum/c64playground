#include "data.hh"
#include <sstream>
#include <fstream>
#include <format>

/*! \brief Read data from a file
 *
 * Input is read and an exception is thrown if the file can not be
 * opened.
 *
 * \param fname file name
 * \param exloadaddr extract load address from data?
 * \return Data object with loaded binary data
 */
Data read_data(const std::string &fname, bool exloadaddr) {
  std::vector<uint8_t> rawdata;
  std::ifstream inp(fname, std::ios::binary);
  uint8_t tmp;

  if(!inp) {
    std::ostringstream out;
    out << "can not open file '" << fname << '\'';
    throw std::runtime_error(out.str());
  };
  //So such iterator? std::copy(std::istreambuf_iterator<uint8_t>(inp), std::istreambuf_iterator<uint8_t>(), std::back_inserter(rawdata));
  do {
    tmp = inp.get();
    if(!inp.eof()) {
      rawdata.push_back(tmp);
    }
  } while(inp);
  Data data(rawdata, exloadaddr);
  if(exloadaddr) {
    std::cout << std::format("Bytes read (without load address): {0:d} ${0:04X}\n", data.size());
    std::cout << "Load address: " << data.get_loadaddr() << std::endl;
  } else {
    std::cout << std::format("Bytes read: {0:d} ${0:04X}\n", data.size());
  }
  return data;
}
