#ifndef __COLOURBALLS_H__
#define __COLOURBALLS_H__
#include "../globals.h"

#define MAXCOLSPERROW 16

/*
 * Per row there are pairs of colour, position tuples, end of line is
 * at a negative colour value. Each row is at most MAXCOLSPERROW*2
 * bytes long.
 *
 * Warning! In moiety.colourball.draw.s in the draw_colourballs
 * subroutine it is assumed that each row is 32 bytes long. If
 * MAXCOLSPERROW is changed this has to be kept in sync.
 */
extern unsigned char Colourball_Table[RNR_height][2*MAXCOLSPERROW];

#endif
