#ifndef __DATASTRUCTURES_H__
#define __DATASTRUCTURES_H__
#include <t7d/doublili.h>

struct TextLine {
  DoubliliNode node;
  char *line;
};
typedef struct TextLine TextLine;

#define EDIT_LINE_BUF_SIZE 255
extern char edit_line_buf[EDIT_LINE_BUF_SIZE]; //!< Maximum length is 254 anyway. This is the global buffer in which the line is actually edited.




#endif
