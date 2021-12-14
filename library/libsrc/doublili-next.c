#include <t7d/doublili.h>

void *doublili_next(void *node) {
  DoubliliNode *_node = node;

  return _node->next;
}
