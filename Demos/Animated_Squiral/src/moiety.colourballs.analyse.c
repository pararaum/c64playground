#include <string.h>
#include "moiety.colourballs.h"

unsigned char Colourball_Table[RNR_height][2*MAXCOLSPERROW];

void __fastcall__ analyse_colourballs(unsigned char *colours) {
  unsigned char column, row;
  int offset;
  unsigned char col;

  memset(Colourball_Table, 0x80, sizeof(Colourball_Table));
  for(row = 0; row < RNR_height; ++row) {
    offset = 0;
    for(column = 0; column < RNR_width; ++column) {
      col = colours[column + row * RNR_width];
      if(col != RNR_textcolour) {
	// We found a different colour.
	Colourball_Table[row][offset++] = col;
	Colourball_Table[row][offset++] = column;
      }
    }
    Colourball_Table[row][offset] = 0xff;
  }
}
