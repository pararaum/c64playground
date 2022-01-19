#include "helpers.h"
#include <stdlib.h>
#include <string.h>

TextLine *allocate_line(char *text) {
  TextLine *node;

  node = malloc(sizeof(TextLine));
  if(node) {
    node->node.prev = NULL;
    node->node.next = NULL;
    node->line = strdup(text);
  }
  return node;
}
