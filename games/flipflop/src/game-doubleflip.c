#include <stdint.h>
#include <stdbool.h>
#include <peekpoke.h>
#include <t7d/xorshift.h>


extern unsigned char flipflopstatus[11];

// Additional (!) flips!
struct Fliptable {
  unsigned char first[11];
  unsigned char second[11];
  unsigned char alternativefirst[11];
  unsigned char alternativesecond[11];
} *globalfliptable = (void*)0x200;

extern void game_double_flip(void *, bool samekey, int8_t slot) {
  flipflopstatus[slot] ^= 1;
  if(!samekey) {
    flipflopstatus[globalfliptable->first[slot]] ^= 1;
    flipflopstatus[globalfliptable->second[slot]] ^= 1;
  } else {

  }
}

extern void game_heavydouble_flip(void *, bool samekey, int8_t slot) {
  flipflopstatus[slot] ^= 1;
  if(!samekey) {
    flipflopstatus[globalfliptable->first[slot]] ^= 1;
    flipflopstatus[globalfliptable->second[slot]] ^= 1;
  } else {
    flipflopstatus[globalfliptable->alternativefirst[slot]] ^= 1;
    flipflopstatus[globalfliptable->alternativesecond[slot]] ^= 1;
  }
}

void *game_double_init(void) {
  int i, s1, s2;
  xorshift_seed(PEEK(0xd012) << 8 | PEEK(0xdc04));
  for(i = 1; i <= 10; ++i) {
    do {
      s1 = xorshift() % 11;
    } while(s1 == i);
    globalfliptable->first[i] = s1;
    do {
      s2 = xorshift() % 11;
    } while((s2 == i) || (s2 == s1));
    globalfliptable->second[i] = s2;
    // Now alternative...
    do {
      s1 = xorshift() % 11;
    } while(s1 == i);
    globalfliptable->alternativefirst[i] = s1;
    do {
      s2 = xorshift() % 11;
    } while((s2 == i) || (s2 == s1));
    globalfliptable->alternativesecond[i] = s2;
  }
  return globalfliptable;
}
