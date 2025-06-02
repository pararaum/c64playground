#include <inttypes.h>
#include <iostream>
#include <deque>
#include <vector>
#include <optional>
#include <sstream>
#include <fstream>
#include <iterator>
#include <regex>
#include <boost/format.hpp>
#include "cmdline.h"
#include "stub-copyeor.inc"
#include "stub-copyeorstack.inc"
#include "stub-donotspread.inc"
#include "stub-scrambler16.inc"
#include "stub-autostart326.inc"
#include "stub-vcclogo.inc"
#include "stub-t7dlogo.inc"

/*! \file
 *
 * Prepend a stub to C64 .prg-files. See README.md for description.
 */

/*! \brief Input data type.
 *
 * This class stores the loaded data and extracts the optional
 * original load address.
 */
class Data {
public:
  typedef std::deque<uint8_t> container_type; //!< Container type for the data, a std::deque is used in order to handle prepending and appending smoothly.
  typedef container_type::const_iterator const_iterator; //!< Constant interator taken from conatainer_type.
  typedef container_type::iterator iterator; //!< Interator taken from conatainer_type.
  typedef container_type::size_type size_type; //!< Size type taken from conatainer_type.
protected:
  std::optional<uint16_t> loadaddr; //!< Original load address if given.
  container_type data; //!< Binary data without the load address.

public:
  /*! \brief Default Constructor
   */
  Data() { }
  /*! \brief Constructor with raw binary data as input.
   *
   * Constructs the objects with the given binary data.
   *
   * \param inp raw binary data
   */
  Data(const std::vector<uint8_t> &inp) : data(inp.begin(), inp.end()) { }
  /*! \brief Constructor with raw binary data as input.
   *
   * Constructs the objects with the given binary data.
   *
   * \param beginitr pointer to the first byte
   * \param enditr pointer after the last byte
   */
  Data(const unsigned char *beginitr, const unsigned char *enditr) : data(enditr - beginitr) {
    std::copy(beginitr, enditr, data.begin());
  }
  /*! \brief Constructor with raw binary data as input.
   *
   * Constructs the objects with the given binary data.
   *
   * \param inp raw binary data
   */
  Data(const container_type &inp) : data(inp) { }
  //
  /* \brief load data from stream
   *
   * \param inp input stream
   * \return Data object
   */
  static Data load(std::istream &inp) {
    std::vector<uint8_t> rawdata;
    int tmp;

    do {
      tmp = inp.get();
      if(!inp.eof()) {
	rawdata.push_back(tmp);
      }
    } while(inp);
    Data data(rawdata);
    return data;
  }
  /*! \brief load data from filename
   *
   * \param fname file name to read data from
   * \return Data object
   */
  static Data load(const std::string &fname) {
    std::ifstream inp(fname, std::ios::binary);
    if(!inp) {
      std::ostringstream out;
      out << "can not open file '" << fname << '\'';
      throw std::runtime_error(out.str());
    }
    return load(inp);
  }
  /*! \brief Join data with other data block.
   *
   * Extends the self data block to fit both myself and the other data
   * block. Then the data is copied from the other block into the self
   * block *overwriting* the data in self.
   *
   * \param other other data block
   * \return self
   */
  Data &join(const Data &other) {
    if(data.empty()) {
      *this = other;
      return *this;
    }
    // Get full space that is needed.
    auto [myfirst, mylast] = get_firstlast();
    auto [ofirst, olast] = other.get_firstlast();
    uint16_t lower = std::min(myfirst, ofirst);
    uint16_t higher = std::max(mylast, olast);
    // Now join the data.
    container_type newdata(65536);
    for(unsigned int i = myfirst; i <= mylast; ++i) {
      newdata.at(i) = data.at(i - myfirst);
    }
    for(unsigned int i = ofirst; i <= olast; ++i) {
      newdata.at(i) = other.data.at(i - ofirst);
    }
    // Now put the full 64K into our own data.
    std::swap(data, newdata);
    // Remove the bytes until the load address.
    if(lower > 0) {
      data.erase(data.begin(), data.begin() + lower);
    }
    data.resize(higher - lower + 1);
    loadaddr = lower;
    return *this;
  }
  //
  /*! assign a load address to the data
  *
  * \param laddr the load address
  * \param force overwrite a previously extract load addr
  */
  void assign_loadaddr(uint16_t laddr, bool force = false) {
    if(loadaddr.has_value() and !force) {
      throw std::logic_error("reassigning load address");
    }
    loadaddr = laddr;
  }
  /*! \brief extract the load address from the binary data
   *
   * The first two bytes are interpreted as LO and HI and the load
   * address is returned. It is also stored in Data::loadaddr.
   *
   * After this function has been called the first two bytes are
   * *removed* from the data so that only the payload is in the data
   * structure.
   * 
   * \param inp raw binary data
   * \return load addr, extracted
   */
  uint16_t extract_loadaddr() {
    if(loadaddr.has_value()) {
      return loadaddr.value();
    }
    if(data.size() < 3) {
      throw std::underflow_error("not enough bytes");
    }
    uint16_t laddr = data.front();
    data.pop_front();
    laddr |= static_cast<uint16_t>(data.front()) << 8;
    data.pop_front();
    loadaddr = laddr;
    return laddr;
  }
  /*! \brief get load address of data
   *
   * Just returns the deducted load address.
   *
   * \return load address as an 16 bit unsigned integer.
   */
  std::optional<uint16_t> get_loadaddr() const { return loadaddr; }
  /*! \brief get address of first and last byte
   *
   * \return a pair of first and last address used by the data
   */
  std::pair<uint16_t, uint16_t> get_firstlast() const {
    uint16_t first = get_loadaddr().value();
    uint16_t last = first + data.size() - 1;
    if(data.empty()) {
      throw std::runtime_error("no first, last for empty data");
    }
    return std::make_pair(first, last);
  }
  /*! \brief get data size
   *
   * Return the number of bytes in the data structure.
   *
   * \return number of bytes
   */
  size_type size() const { return data.size(); }
  const_iterator begin() const { return data.begin(); }
  const_iterator end() const { return data.end(); }
  iterator begin() { return data.begin(); }
  iterator end() { return data.end(); }
  // Access
  uint8_t operator[](unsigned int i) const { return data.at(i); }
  uint8_t &operator[](unsigned int i) { return data.at(i); }
  uint8_t &back() { return data.back(); }
  /*! Peek a 16 bit value
   *
   * The data is peeked as a little endian value.
   *
   * \param addr offset into the data
   */
  uint16_t peekw(unsigned int addr) const {
    uint16_t ret = operator[](addr + 1); // Get HI byte.
    ret <<= 8; // Move to the right bit position.
    ret |= operator[](addr); // Get LO byte.
    return ret;
  }
  void poke(unsigned int addr, uint8_t val) {
    data.at(addr) = val;
  }
  void pokew(unsigned int addr, uint16_t val) {
    data[addr] = val;
    data[addr  + 1] = val >> 8;
  }
  /*! \brief save binary data
   *
   * Write the bytes into the output stream. If a load address is
   * available it is prepended. The writing of the load address can be
   * skipped by setting skiploadaddr to true.
   *
   * \param out output stream
   * \param skiploadaddr skip writing of load address?
   * \return output stream
   */
  std::ostream &save(std::ostream &out, bool skiploadaddr = false) const {
    if(!skiploadaddr) {
      if(loadaddr.has_value()) {
	out.put(loadaddr.value() & 0xFF);
	out.put((loadaddr.value() >> 8) & 0xFF);
      }
    }
    std::copy(data.begin(), data.end(), std::ostream_iterator<unsigned char>(out));
    return out;
  }
  void drop_front(unsigned int skip) {
    if(skip >= size()) {
      throw std::invalid_argument(str(boost::format("trying to skip %u bytes of %u") % skip % size()));
    }
    if(skip > 0) {
      data.erase(data.begin(), data.begin() + skip);
    }
  }
  void resize(unsigned int newsize) {
    if(newsize == 0) {
      throw std::invalid_argument("can not shrink to zero size");
    }
    if(newsize > size()) {
      throw std::invalid_argument("shrinking to a size which is bigger than the amount of bytes available");
    }
    data.resize(newsize);
  }
};


/*! \brief Base function with interface for the prepender
 *
 * All prependers have to be derived from this class.
 */
class PrependerBase {
public:
  virtual uint16_t get_jump_address(const Data &data) = 0;
  virtual void transmogrify_data(Data &) {}
  virtual Data customise_stub(uint16_t jump, const Data &data) = 0;
  /*! \brief prepend the stub and write the data
   *
   * \param out output stream
   * \param data read data via call-by-value (some prependers may need to change it)
   */
  virtual void prepend(std::ostream &out, Data data) {
    uint16_t jump = get_jump_address(data);
    transmogrify_data(data);
    Data stub(customise_stub(jump, data));
    stub.save(out);
    data.save(out, true);
  }
  virtual void prepend(std::string &fname, const Data &data) {
    std::ofstream out(fname, std::ios::binary);
    prepend(out, data);
  };
  virtual std::optional<uint16_t> get_SYS_addr(const Data &data) {
    std::optional<uint16_t> ret;
    enum States {
      Find_SYS,
      Skip_Space,
      Collect_SYS,
      Found,
      Done
    } state = Find_SYS;
    Data::const_iterator first_SYS, last_SYS;
    
    for(auto itr = data.begin(); state != Done && itr < data.end(); ++itr) {
      switch(state) {
      case Find_SYS:
	{
	  auto found = std::find(data.begin(), data.end(), 0x9e); // Look for the SYS.
	  if(found == data.end()) {
	    return ret; // No SYS, therefore we bail out.
	  }
	  itr = found;
	}
	state = Skip_Space;
	break;
      case Skip_Space:
	if(*itr != ' ') { // Found a non-space character.
	  if(!std::isdigit(*itr)) { // But it is really a number?
	    // No, there we bail out.
	    state = Done;
	  }
	  first_SYS = itr;
	  state = Collect_SYS;
	}
	break;
      case Collect_SYS:
	if(!std::isdigit(*itr)) { // Last iterator of the SYS.
	  last_SYS = itr;
	  state = Found;
	}
	break;
      case Found:
	{
	  std::string addrstr(first_SYS, last_SYS); // Copy the characters to the address.
	  auto val = std::stoul(addrstr);
	  if(val > 65535) {
	    std::cerr << boost::format("Warning! Using only the lowest 16 bits of %d.\n") % val;
	  }
	  ret = val;
	}
	state = Done;
	break;
      case Done:
	break;
      }
    }
    return ret;
  }
};


/*! \brief Base class for Prependers which extract their jump address.
 *
 * This prepender will extract a jump adddress from the BASIC SYS-line
 * and will use this address to jump to after copying.
 */
class PrependerWithJump : public PrependerBase {
protected:
  std::optional<uint16_t> jmp; //!< Jump address. If not set it is deduced from the SYS line.
public:
  /*! \brief set the jump address to use
   *
   * \param x the address to JMP to after the stub is finished
   */
  void set_jump(uint16_t x) { jmp = x; }

  /*! \brief Get the jump address from SYS.
   *
   * \throws std::runtime_error if the SYS line could not be parsed or found
   * \param data binary data to parse
   * \return jump address 
   */
  virtual uint16_t get_jump_address(const Data &data) {
    if(!jmp && (data.get_loadaddr() == 0x0801)) {
      jmp = get_SYS_addr(data);
      if(jmp) {
	std::cout
	  << boost::format("Determined a value of $%04X (%d) for JMP from SYS line.\n")
	  % jmp.value()
	  % jmp.value();
      }
    }
    if(!jmp) {
      std::cerr << "Error! Could not determine the value for JMP, no SYS?\n";
      throw std::runtime_error("no jump destination");
    }
    return jmp.value();
  }
};


/*! \brief Prepend a T7D logo
 *
 * Prepend a monochrome T7D Logo.
 */
class PrependT7DLogo : public PrependerWithJump {
public:
  PrependT7DLogo() { }

  virtual void transmogrify_data(Data &data) {
  }
  virtual Data customise_stub(uint16_t jump, const Data &data) {
    Data stub(&stub_t7dlogo[0], &stub_t7dlogo[stub_t7dlogo_len]); //!< The stub itself.
    stub.extract_loadaddr();
    // Add the size of the file to the value in the stub. This has to
    // point to the byte after the file.
    stub.pokew(STUBT7DLOGOstubsptr_dataend_offset, stub.peekw(STUBT7DLOGOstubsptr_dataend_offset) + data.size());
    // Negated size of the file, only the 16 lowest bits are used.
    stub.pokew(STUBT7DLOGOstub10000_minus_datalen_offset, -data.size());
    // Fix the destination address.
    stub.pokew(STUBT7DLOGOstubdptrDATADEST_offset, data.get_loadaddr().value());
    // Fix the JMP address.
    stub.pokew(STUBT7DLOGOstubjmp_offset, jump);
    return stub;
  }
};


class PrependCopyXor : public PrependerWithJump {
protected:
  uint8_t eor; //!< EOR value, if zero then nothing is done.

public:
  PrependCopyXor() { }

  /*! \brief set eor value
   *
   * \param x new eor value
   */
  void set_eor(uint8_t x) { eor = x; }

  virtual void transmogrify_data(Data &data) {
    if(eor != 0) {
      std::transform(data.begin(),
		     data.end(),
		     data.begin(),
		     [eor = eor](uint8_t x) -> uint8_t {
		       return x ^ eor;
		     });
    }
  }
  virtual Data customise_stub(uint16_t jump, const Data &data) {
    Data stub(&stub_copyeor[0], &stub_copyeor[stub_copyeor_len]); //!< The stub itself.
    stub.extract_loadaddr();
    // Add the size of the file to the value in the stub. This has to
    // point to the byte after the file.
    stub.pokew(STUBCOPYEORstubsptr_dataend_offset, stub.peekw(STUBCOPYEORstubsptr_dataend_offset) + data.size());
    // Negated size of the file, only the 16 lowest bits are used.
    stub.pokew(STUBCOPYEORstub10000_minus_datalen_offset, -data.size());
    // Fix the destination address.
    stub.pokew(STUBCOPYEORstubdptrDATADEST_offset, data.get_loadaddr().value());
    // Fix the JMP address.
    stub.pokew(STUBCOPYEORstubjmp_offset, jump);
    // Set the EOR value.
    stub[STUBCOPYEORstubeor_offset] = eor;
    return stub;
  }
};


class PrependCopyXorStack : public PrependCopyXor {
  virtual Data customise_stub(uint16_t jump, const Data &data) {
    Data stub(&stub_copyeorstack[0], &stub_copyeorstack[stub_copyeorstack_len]); //!< The stub itself.
    stub.extract_loadaddr();
    // Add the size of the file to the value in the stub. This has to
    // point to the byte after the file.
    stub.pokew(STUBCOPYEORSTACKstubsptr_dataend_offset, stub.peekw(STUBCOPYEORstubsptr_dataend_offset) + data.size());
    // Negated size of the file, only the 16 lowest bits are used.
    stub.pokew(STUBCOPYEORSTACKstub10000_minus_datalen_offset, -data.size());
    // Fix the destination address.
    stub.pokew(STUBCOPYEORSTACKstubdptrDATADEST_offset, data.get_loadaddr().value());
    // Fix the JMP address.
    stub.pokew(STUBCOPYEORSTACKstubjmp_offset, jump);
    // Set the EOR value.
    stub[STUBCOPYEORSTACKstubeor_offset] = eor;
    return stub;
  }
};


class PrependDoNotSpread : public PrependerWithJump {
public:
  uint8_t eor; //!< EOR value, if zero then nothing is done.
  virtual void transmogrify_data(Data &data) {
    if(eor != 0) {
      std::transform(data.begin(),
		     data.end(),
		     data.begin(),
		     [eor = eor](uint8_t x) -> uint8_t {
		       return x ^ eor;
		     });
    }
  }
  virtual Data customise_stub(uint16_t jump, const Data &data) {
    Data stub(&stub_donotspread[0], &stub_donotspread[stub_donotspread_len]);
    stub.extract_loadaddr(); // The stub has a load address, we need
			     // to extract this otherwise the offsets
			     // are wrong!
    // Add the size of the file to the value in the stub. This has to
    // point to the byte after the file.
    stub.pokew(STUBDONOTSPREADstubsptr_data_end_offset, stub.peekw(STUBDONOTSPREADstubsptr_data_end_offset) + data.size());
    // Negated size of the file, only the 16 lowest bits are used.
    stub.pokew(STUBDONOTSPREADstub10000_minus_datalen_offset, -data.size());
    // Fix the destination address.
    stub.pokew(STUBDONOTSPREADstubdptrDATADEST_offset, data.get_loadaddr().value());
    // Fix the JMP address.
    stub.pokew(STUBDONOTSPREADstubjump_to_offset, jump);
    // Set the EOR value.
    stub[STUBDONOTSPREADstubeorvalue_offset] = eor;
    return stub;
  }
};


class PrependScrambler16 : public PrependerWithJump {
protected:
  uint16_t lfsrregister;
  uint16_t lfsrfeedback;
public:
  PrependScrambler16() : lfsrregister(0xF77Du), lfsrfeedback(0x8117u) { }
  virtual void transmogrify_data(Data &data) {
    auto lreg = lfsrregister;
    for(auto &x : data) {
      x ^= lreg;
      unsigned bit = lreg & 1;
      lreg = (lreg >> 1) ^ (bit ? lfsrfeedback : 0);
    }
  }
  virtual Data customise_stub(uint16_t jump, const Data &data) {
    Data stub(&stub_scrambler16[0], &stub_scrambler16[stub_scrambler16_len]);
    stub.extract_loadaddr();
    // Negated size of the file, only the 16 lowest bits are used.
    stub.pokew(STUBSCRAMBLER16stub10000_minus_datalen_offset, -data.size());
    // Add the size of the file to the value in the stub. This has to
    // point to the byte after the file.
    stub.pokew(STUBSCRAMBLER16stubsptr_dataend_offset, stub.peekw(STUBSCRAMBLER16stubsptr_dataend_offset) + data.size());
    // Fix the destination address.
    stub.pokew(STUBSCRAMBLER16stubdptrDATADEST_offset, data.get_loadaddr().value());
    // Fix the JMP address.
    stub.pokew(STUBSCRAMBLER16stubjmp_offset, jump);
    return stub;
  }
};


class PrependAutostart326 : public PrependerWithJump {
public:
  PrependAutostart326() { }
  virtual Data customise_stub(uint16_t jump, const Data &data) {
    Data stub(&stub_autostart326[0], &stub_autostart326[stub_autostart326_len]);
    stub.extract_loadaddr();
    stub.pokew(STUBAUTOSTART326stubMVDEST_offset, data.get_loadaddr().value());
    stub.pokew(STUBAUTOSTART326stubMVELEN_offset, data.size());
    // Fix the JMP address.
    stub.pokew(STUBAUTOSTART326stubJUMPDEST_offset, jump);
    return stub;
  }
};


class PrependVCCLogo : public PrependerWithJump {
public:
  PrependVCCLogo() { }
  virtual Data customise_stub(uint16_t jump, const Data &data) {
    Data stub(&stub_vcclogo[0], &stub_vcclogo[stub_vcclogo_len]);
    stub.extract_loadaddr();
    stub.pokew(STUBVCCLOGOstubMVDEST_offset, data.get_loadaddr().value());
    stub.pokew(STUBVCCLOGOstubMVELEN_offset, data.size());
    // Fix the JMP address. This time, we use the return via RTS trick!
    auto jumpviarts = jump - 1;
    stub.poke(STUBVCCLOGOstubJUMPVIARTSHI_offset, jumpviarts >> 8);
    stub.poke(STUBVCCLOGOstubJUMPVIARTSLO_offset, jumpviarts & 0xFF);
    return stub;
  }
};


unsigned long string2ul(const std::string &text) {
  try {
    if(text.starts_with("0x")) {
      return std::stoul(text, nullptr, 16);
    } else if(text.starts_with("$")) {
      return std::stoul(text.substr(1), nullptr, 16);
    }
    return std::stoul(text);
  }
  catch(const std::invalid_argument &) {
    std::ostringstream out;
    throw std::invalid_argument(str(boost::format("invalid argument when converting '%s' to unsigned long") % text));
  }
}


/*! load a file with linker features
 *
 * The `cld` manual has the following option, we will copy them.
 * ```
 * Loading: (file -> memory buffer)
 *   <file>[,,[<offs>][,<len>]]        - normal
 *   <file>,<addr>[,[<offs>][,<len>]]  - override load address
 *   <file>@<addr>[,[<offs>][,<len>]]  - raw file
 * ```
 *
 * The filename is passed by value as we may need to modify the file
 * name string when load address, etc. are given.
 *
 * \param fname file name to load from
 * \return load data
 */
Data load_with_linking_option(std::string fname) {
  std::regex pattern_addr_offs_len("(.*)(,|@)([0-9a-fA-Fx]*),([0-9a-fA-Fx]+),([0-9a-fA-Fx]+)");
  std::regex pattern_addr_offs("(.*)(,|@)([0-9a-fA-Fx]*),([0-9a-fA-Fx]+)");
  std::regex pattern_addr("(.*)(,|@)([0-9a-fA-Fx]*)");
  std::smatch match;
  std::optional<uint16_t> override_loadaddr;
  std::optional<unsigned long> offset;
  std::optional<uint16_t> length;
  char addresschar = '\0';

  if(std::regex_match(fname, match, pattern_addr_offs_len)) {
    std::cerr << "full match\n";
    addresschar = match[2].str()[0];
    // for(std::size_t i = 0; i < match.size(); ++i) {
    //   std::cout << i << '\t' << match[i].str() << std::endl;
    // }
    fname = match[1].str();
    if(!(match[3].str().empty())) {
      override_loadaddr = string2ul(match[3].str());
    }
    offset = string2ul(match[4].str());
    length = string2ul(match[5].str());
  } else if (std::regex_match(fname, match, pattern_addr_offs)) {
    std::cerr << "addr offs match\n";
    addresschar = match[2].str()[0];
    fname = match[1];
    if(!(match[3].str().empty())) {
      override_loadaddr = string2ul(match[3].str());
    }
    offset = string2ul(match[4].str());
  } else if (std::regex_match(fname, match, pattern_addr)) {
    std::cerr << "addr match\n";
    addresschar = match[2].str()[0];
    fname = match[1];
    if(!(match[3].str().empty())) {
      override_loadaddr = string2ul(match[3].str());
    }
  }
  auto data = Data::load(fname);
  if(override_loadaddr) {
    switch(addresschar) {
    case '@':
      data.assign_loadaddr(override_loadaddr.value());
      break;
    case ',':
      data.extract_loadaddr();
      data.assign_loadaddr(override_loadaddr.value(), true);
      break;
    default:
      throw std::logic_error(std::string("unknown address character: ") + addresschar);
    }
  } else {
    // File name does not contain the special characters, just read whole.
    data.extract_loadaddr();
  }
  if(offset) {
    data.drop_front(offset.value());
  }
  if(length) {
    data.resize(length.value());
  }
  std::cout << boost::format("Load address is $%04X.\n") % data.extract_loadaddr();
  return data;
}

/*! main entry point
 *
 * Parse the command-line parameters, see \ref cmdline.h, initialise
 * the corresponding prepender object and link together all files
 * given on the commandline. Then prepend the stub and write the
 * output.
 */
int main(int argc, char **argv) {
  gengetopt_args_info args;
  int ret = -1;

  if(cmdline_parser(argc, argv, &args) != 0) {
    return 1;
  } else {
    if(args.inputs_num < 1) {
      std::cerr << "At least one filename must be provided!\n";
      return 1;
    }
    std::cout << "Prepender64 Version " << CMDLINE_PARSER_VERSION << std::endl;
    try {
      std::string inpnam(args.inputs[0]); // First input name, this one is used to generate the output name.
      // If there are special characters in the name used for linking then cut them off.
      inpnam = inpnam.substr(0, inpnam.find_first_of("@,"));
      std::string outnam(inpnam + ".prep");
      Data data;
      if(args.output_given) {
	outnam = args.output_arg;
      }
      for(unsigned int idx = 0; idx < args.inputs_num; ++idx) {
	// auto currdata = Data::load(args.inputs[idx]);
	// std::cout << currdata.size() << " bytes have been read.\n";
	// std::cout << boost::format("Load address is $%04X.\n") % currdata.extract_loadaddr();
	auto currdata = load_with_linking_option(args.inputs[idx]);
	data.join(currdata);
	auto [first, last] = data.get_firstlast();
	auto size = last - first + 1;
	std::cout << boost::format("Data is from $%04x to $%04x (%d $%04x bytes).\n") % first % last % size % size;
      }
      std::unique_ptr<PrependerWithJump> prepender;
      if(args.copy_eor_mode_counter) {
	auto newprepender = new PrependCopyXor;
	if(args.eor_given) {
	  auto eor = args.eor_arg & 0xFF;
	  
	  std::cout << boost::format("Using an EOR value of $%02X.\n") % eor;
	  newprepender->set_eor(args.eor_arg);
	}
	prepender = std::unique_ptr<PrependerWithJump>(newprepender);
      } else if(args.copy_eor_stack_mode_counter) {
	auto newprepender = new PrependCopyXorStack;
	if(args.eor_given) {
	  auto eor = args.eor_arg & 0xFF;
	  
	  std::cout << boost::format("Using an EOR value of $%02X.\n") % eor;
	  newprepender->set_eor(args.eor_arg);
	}
	prepender = std::unique_ptr<PrependerWithJump>(newprepender);
      } else if(args.do_not_spread_mode_counter) {
	auto newprepender = new PrependDoNotSpread;
	newprepender->eor = args.eor_arg;
	prepender = std::unique_ptr<PrependerWithJump>(newprepender);
      } else if(args.scrambler16_mode_counter) {
	auto newprepender = new PrependScrambler16;
	prepender = std::unique_ptr<PrependScrambler16>(newprepender);
      } else if(args.autostart_$326_mode_counter) {
	auto newprepender = new PrependAutostart326;
	prepender = std::unique_ptr<PrependAutostart326>(newprepender);
      } else if(args.vcclogo_mode_counter) {
	auto newprepender = new PrependVCCLogo;
	prepender = std::unique_ptr<PrependVCCLogo>(newprepender);
      } else if(args.t7dlogo_mode_counter) {
	auto newprepender = new PrependT7DLogo;
	prepender = std::unique_ptr<PrependT7DLogo>(newprepender);
      }
      if(args.jump_given) {
	auto jmp = args.jump_arg & 0xFFFF;
	std::cout << boost::format("Using a JMP value of $%04X.\n") % jmp;
	prepender->set_jump(args.jump_arg);
      } else if(args.loadjump_given) {
	auto jmp = data.get_loadaddr().value();
	std::cout << boost::format("Using load address $%04X as jump address.\n") % jmp;
	prepender->set_jump(jmp);
      }
      if(prepender) {
	prepender->prepend(outnam, data);
	std::cout << "🎵I am the great prepender🎶...\n";
	ret = 0;
      } else {
	std::cerr << "Error! A mode must be selected!\n";
      }
    }
    catch(const std::exception &e) {
      std::cerr << "Exception: " << e.what() << std::endl;
    }
  }
  return ret;
}
