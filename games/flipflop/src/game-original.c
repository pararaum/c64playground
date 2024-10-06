#include <stdint.h>
#include <stdbool.h>

extern unsigned char flipflopstatus[11];

extern unsigned char * __fastcall__ mkfliptable(void);

void game_original_flip(void *fliptable_, bool samekey, int8_t slot) {
  unsigned char *fliptable = fliptable_;
  flipflopstatus[slot] ^= 1;
  if(!samekey) {
    flipflopstatus[fliptable[slot]] ^= 1;
  } else {
    flipflopstatus[fliptable[slot + 0x10]] ^= 1;
  }
}

unsigned char *game_original_init(void) {
  return mkfliptable();
}
