#include <stddef.h>
#include <stdlib.h>
#include <t7d/doublili.h>

DoubliliNode *doublili_remove(void **head, void *node) {
  DoubliliNode *_node = node;
  DoubliliNode *_head = *head;

  if(_node == _head) {
    // Head is pointing to the element to be removed.
    if(_node->next != NULL) {
      *head = _node->next; // Point to the next element.
      _node->next->prev = NULL; // Next element shall not have a previous element.
    } else {
      // Only a single element in linked list.
      *head = NULL;
    }
  } else {
    if(_node->prev) {
      _node->prev->next = _node->next;
    }
    if(_node->next) {
      _node->next->prev = _node->prev;
    }
  }
  _head = _node->next; // Keep next element safe.
  free(node);
  return _head;
}
