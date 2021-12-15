#include <t7d/doublili.h>
#include <stddef.h>

void *doublili_prev(void *node) {
  DoubliliNode *_node = node;

  if(node == NULL) {
    return NULL;
  }
  return _node->prev;
}
