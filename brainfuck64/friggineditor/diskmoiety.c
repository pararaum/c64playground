#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "diskmoiety.h"
#include "helpers.h"

TextLine *load_file(void) {
  TextLine *node = NULL;
  TextLine *newnode = NULL;
  TextLine *head = NULL;
  FILE *inp;
  char *buf;
  int ch;

  inp = fopen("output,seq", "r");
  if(inp) {
    while(!feof(inp)) {
      buf = &edit_line_buf[0]; // Position at beginning of buffer.
      do { // Read a line into the buffer.
	ch = fgetc(inp);
	*buf++ = ch;
      } while((ch != EOF) && (ch != '\n') && (buf - &edit_line_buf[0] < 254));
      --buf; // Go back one character.
      *buf = 0; // End the string.
      newnode = allocate_line(edit_line_buf);
      printf("'%s' @ %04X\n", edit_line_buf, newnode);
      if(head == NULL) {
	head = newnode; // Remember the first node.
      }
      node = (void *)doublili_insertafter(node, newnode); // This will handle allocation errors, too.
    }
    fclose(inp);
  } else {
    perror("Error opening file");
  }
  return head;
}

void save_file(TextLine *head) {
  TextLine *node = NULL;
  FILE *out;

  out = fopen("output,seq", "w");
  if(out) {
    node = head;
    while(node != NULL) {
      fprintf(out, "%s\n", node->line);
      node = doublili_next(node);
    }
    fclose(out);
  }
}
