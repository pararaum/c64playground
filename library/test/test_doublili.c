#include <stdio.h>
#include <stdlib.h>
#include <t7d/doublili.h>

typedef struct IntegerList {
  DoubliliNode node;
  int data;
} IntegerList;

IntegerList *head = NULL;

void insertdata(int num) {
  int i;
  IntegerList *node;

  for(i = 0; i < num; ++i) {
    node = malloc(sizeof(IntegerList));
    node->data = i;
    doublili_insert((void*)&head, node);
    printf("node=%04X, head=%04X, head.prev=%04x, head.next=%04x.\n", node, head, head->node.prev, head->node.next);
  }
}

int count(void *node) {
  int count = 0;
  while(node) {
    node = doublili_next(node);
    ++count;
  }
  return count;
}

void print(void) {
  IntegerList *node;

  node = head;
  while(node) {
    printf("node=%04x, data=%d, node.prev=%04x, node.next=%04x\n", node, node->data, node->node.prev, node->node.next);
    node = doublili_next(node);
  }
}

int main(void) {
  int tmp;

  puts("testing doubly linked list.");
  insertdata(13);
  print();
  tmp = count(head);
  printf("head=%x, count=%d\n", head, tmp);
  if(tmp != 13) {
    printf("count is wrong, %d!=%d\n", tmp, 13);
    return 1;
  }
  printf("%X\n", doublili_next(doublili_next(doublili_next(head))));
  doublili_remove((void*)&head, doublili_next(doublili_next(doublili_next(head))));
  print();
  printf("%X\n", head);
  doublili_remove((void*)&head, head);
  print();
  while(count(head) > 2) {
    doublili_remove((void*)&head, head);
  }
  printf("%X\n", head);
  if(head->data != 1) {
    printf("head->data = %d?\n", head->data);
    return 1;
  }
  if(((IntegerList*)(head->node.next))->data != 0) {
    printf("head->next->data = %d?\n", ((IntegerList*)(head->node.next))->data);
    return 1;
  }
  print();
  doublili_remove((void*)&head, doublili_next(head));
  print();
  printf("%X\n", head);
  doublili_remove((void*)&head, head);
  printf("%X\n", head);
  return 0;
}
