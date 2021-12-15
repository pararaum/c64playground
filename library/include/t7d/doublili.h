#ifndef __DOUBLILI_H_20211213__
#define __DOUBLILI_H_20211213__
/*
 * Simple doubly linked list library.
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
 * \return new head
 */
DoubliliNode *doublili_insert(void **head, void *node);

/*! \brief remove a node from the doubly linked list
 *
 * If the last element was removed, head is set to NULL. For the
 * removed node free() is called!
 * 
 * \param head pointer to the head pointer
 * \param node pointer to the node, which will be removed
 * \return next element, may be NULL
 */
DoubliliNode *doublili_remove(void **head, void *node);

/*! \brief call a function for each element
 *
 * This function will call the supplied function for each element of the doubly linked list. Starting at the given head and moving in the direction of the next element.
 *
 * \param head head to start at
 * \param function function will be called with a pointer to each node
 */
void doublili_foreach(void *head, void (*functor)(void *));


void *doublili_next(void *node);

#endif
