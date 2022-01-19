#ifndef __HELPERS_H__
#define __HELPERS_H__
#include "data-structures.h"

/*! \brief put character in current logical line
 *
 * This will put a single character at position pos in the current line.
 *
 * \param pos offset to beginning of current line
 * \param ch character to put there (only PETSCII 32 to 127)
 */
void __fastcall__ put_char_in_logical_line(unsigned char pos, char ch);

/*! Clear the current line
 *
 * This function uses 0xd1.
 */
void __fastcall__ clear_logical_line(void);

/*! \brief allocate a line (node) from a text string
 *
 * \param text pointer to the string
 * \return initialised node with an allocated string (srtdup)
 */
TextLine *allocate_line(char *text);


#endif
