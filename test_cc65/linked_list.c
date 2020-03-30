#include <stdio.h>
#include <stdlib.h>

typedef struct LinkedList {
  struct LinkedList *next;
  unsigned long value;
} linkedlist_t;

linkedlist_t *add_one(linkedlist_t *begin) {
  linkedlist_t *curr = begin;
  do {
    curr->value += 1;
    curr = curr->next;
  } while(curr != begin);
  return curr;
}

int main(void) {
  linkedlist_t *beginptr;

  beginptr = malloc(sizeof(linkedlist_t));
  beginptr->next = beginptr;
  beginptr->value = 0;
  add_one(beginptr);
  free(beginptr);
  return 0;
}

