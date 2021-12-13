#ifndef __DOUBLILI_H_20211213__
#define __DOUBLILI_H_20211213__
/*
 * Simple double linked list library.
 *
 * Every item aka the struct must have as its first element the
 * prev/next pointers, so the first four bytes are used for linking
 * the list. We chose to be invasive as multiple allocation are
 * probably not such a good idea on a machine with 64KB of memory.
 */

struct doublili_node_t {
  struct doublili_node_t *prev;
  struct doublili_node_t *next;
};
typedef struct doublili_node_t DoubliliNode;

/*! \brief insert a node into the doubly linked list as first element of the head
 *
 * \param head pointer to the head pointer
 * \param node pointer to node, must be allocated beforehand; may be NULL if no elements
 */
DoubliliNode *doublili_insert(void **head, void *node);

void *doublili_next(void *node);

#endif
