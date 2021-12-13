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

  puts("testing double linked list.");
  insertdata(13);
  print();
  tmp = count(head);
  printf("head=%x, count=%d\n", head, tmp);
  if(tmp != 13) {
    printf("count is wrong, %d!=%d\n", tmp, 13);
    return 1;
  }
  return 0;
}
