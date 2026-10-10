#include "compression.hh"


Compressor::Compressor(const std::string& input, const std::string& output,
		       const gengetopt_args_info &cliargs)
  : inputname(input), outputname(output),
    cliargs(cliargs),
    data(read_data(input, !cliargs.data_flag))
{
}

int Compressor::run() {
  pre_compress();
  auto compressed = compress();
  std::cout << "Compressed size: " << compressed.size() << "\n";

  std::ofstream out(outputname, std::ios::binary);
  if (!cliargs.raw_flag) {
    uint16_t jumpaddr = cliargs.jump_arg >= 0 ? cliargs.jump_arg : data.get_loadaddr();
    std::cout << "Writing decrunching stub...\n";
    write_stub(out, compressed, data.get_loadaddr(), jumpaddr);
  } else {
    std::cout << "Skipping writing of decrunching stub.\n";
  }
  write_compressed_data(out, compressed);
  return 0;
}
