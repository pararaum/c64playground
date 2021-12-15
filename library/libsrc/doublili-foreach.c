#include <t7d/doublili.h>

void doublili_foreach(void *head, void (*functor)(void *)) {
  DoubliliNode *_node = head;

  while(_node) {
    functor(_node);
    _node = doublili_next(_node);
  }
}

