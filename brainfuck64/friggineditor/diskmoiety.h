#ifndef __DISKMOIETY_HH__
#define __DISKMOIETY_HH__
#include "data-structures.h"

/*! \brief load a file into memory
 *
 * \return pointer to the head of the read file lines
 */
TextLine *load_file(void);

/*! \brief save a file onto disk
 *
 * \param head pointer to the first line to be saved on disk
 */
void save_file(TextLine *head);

#endif
