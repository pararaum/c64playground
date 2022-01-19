#include <stddef.h>
#include <t7d/doublili.h>

DoubliliNode *doublili_insertafter(void *_node, void *_new) {
  DoubliliNode *node = _node;
  DoubliliNode *new = _new;
  DoubliliNode *next;

  if(new == NULL) {
    return NULL;
  }
  if(node == NULL) {
    return new;
  } else {
    next = node->next;
  }
  node->next = new;
  if(next != NULL) { // Check if there actually is another node.
    next->prev = new;
  }
  new->prev = node;
  new->next = next;
  return new;
}
