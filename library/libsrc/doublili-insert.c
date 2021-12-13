#include <stddef.h>
#include <t7d/doublili.h>

DoubliliNode *doublili_insert(void **head, void *node) {
  DoubliliNode *_node = node;
  DoubliliNode *_head = *head;

  if(_head != NULL) {
    _head->prev = node;
  }
  _node->prev = NULL;
  _node->next = _head;
  *head = node;
}
