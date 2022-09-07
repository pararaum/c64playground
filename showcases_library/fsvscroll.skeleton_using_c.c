#include <stdio.h>
#include <peekpoke.h>
#include <t7d/scroller/fsvscroll.h>

extern void (*muzak)(unsigned char song);
extern void fastcall muzak_init(unsigned char song);
extern void fastcall muzak_play(void);

extern unsigned char long_frame[];

void init(void) {
  FSVScroll_InitScroll fsvs_init = {
    &long_frame[2],
    (void*)0x400,
    (void*)0x800,
    80
  };
  POKE(53280u, long_frame[0]);
  POKE(53281u, long_frame[1]);
  muzak_init(0);
  fsvscroll_init(&fsvs_init);
};

void main(void) {
  POKE(0xd018u, 0x15); // We need all uppercase characters.
  puts("hello world!");
  printf("%u\n", &muzak);
  printf("%u\n", &muzak_init);
  printf("%u\n", &long_frame[0]);
  printf("%u\n", &long_frame[2]);
  init();
  do {
    while(PEEK(0xd012) != 250) {
    }
    muzak_play();
    fsvscroll_update_softscroll_up_1();
  } while(1);
}
