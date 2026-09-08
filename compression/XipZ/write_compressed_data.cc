#include "data.hh"
#include <sstream>
#include <iterator>

std::ostream &write_compressed_data(std::ostream &out, const std::vector<uint8_t> &data) {
  std::copy(data.begin(), data.end(), std::ostream_iterator<unsigned char>(out));
  return out;
}


