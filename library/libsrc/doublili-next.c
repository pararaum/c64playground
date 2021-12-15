#include <t7d/doublili.h>
#include <stddef.h>

void *doublili_next(void *node) {
  DoubliliNode *_node = node;

  if(node == NULL) {
    return NULL;
  }
  return _node->next;
}
