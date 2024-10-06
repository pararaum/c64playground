#include <peekpoke.h>
#include <conio.h>
#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>

#define DISPLAYX -3
#define DISPLAYY 11
#define TEXTCOLOR 3
#define BACKGROUND 0
#define BORDER 0

extern volatile unsigned short framecounter;

extern unsigned char selectscreen0000[];
extern unsigned char creditsscreen0000[];

extern unsigned short __fastcall__ get_framecounter(void);
extern void display_gamescreen(void);
extern void game_original_flip(void *fliptable, bool samekey, int8_t slot);
extern void game_double_flip(void *fliptable, bool samekey, int8_t slot);
extern void game_heavydouble_flip(void *fliptable, bool samekey, int8_t slot);
extern void *game_original_init(void);
extern void *game_double_init(void);
extern short titlescreen(void);
extern short helpscreen(void);
extern short creditsscreen(void);

/*! flip flop status data
 *
 * Contains the current status of the flip flop data in positions 1
 * (!) to 10. Any value not equal to zero is considered a set
 * flipflop.
 */
unsigned char flipflopstatus[11];


short __fastcall__ waitframes(unsigned short x) {
  unsigned short end = get_framecounter() + x;

  while(get_framecounter() < end) {
    if(kbhit()) {
      return (unsigned char)cgetc();
    }
  }
  return -1;
}


short credits(void) {
  POKE(0xd020, BACKGROUND);
  POKE(0xd021, BORDER);
  textcolor(BACKGROUND);
  clrscr();
  memcpy((void*)0x400, &creditsscreen0000[2], 1000);
  memset((void*)(0x400+1024-8), 0, 8);
  return creditsscreen();
}


short selectscreen(void) {
  memcpy((void*)0x400, &selectscreen0000[2], 1000);
  memcpy((void*)0xd800, &selectscreen0000[2+1000], 1000);
  return waitframes(400);
}


void display(unsigned char *flipflopstatus) {
  int8_t i;
  int xpos;

  for(i = 1; i <= 10; ++i) {
    xpos = DISPLAYX + 4*i;
    if(i > 5) {
      xpos += 1;
    }
    if(flipflopstatus[i]) {
      cputcxy(xpos, DISPLAYY, '#');
    } else {
      cputcxy(xpos, DISPLAYY, ' ');
    }
    //cputcxy(DISPLAYX + i, 8, i < 10 ? '0' + i : '0');
  }
}


int8_t orall(unsigned char *flipflopstatus) {
  int8_t i;
  int8_t result = 0;

  for(i = 1; i <= 10; ++i) {
    result |= flipflopstatus[i];
  }
  return result;
}


void flipflop(unsigned char *fliptable, void(*flipfunction)(void *, bool, int8_t) ) {
  int8_t i;
  char ch;
  char oldch = 0;
  unsigned int turns = 0;
  int slot;
  char buf[41];

  for(i = 0; i < sizeof(flipflopstatus); ++i) {
    flipflopstatus[i] = 1;
  }
  do {
    display(flipflopstatus);
    ch = cgetc();
    switch(ch) {
    case 0x30:
    case 0x31:
    case 0x32:
    case 0x33:
    case 0x34:
    case 0x35:
    case 0x36:
    case 0x37:
    case 0x38:
    case 0x39:
      slot = 1 + ((ch + 1) % 10);
      flipfunction(fliptable, ch == oldch, slot);
      break;
    case '#':
      for(i = 0; i < 89; ++i) {
	POKE(0x400 + i, fliptable[i]);
      }
      break;
    case 0x03:
      goto runstoppressed;
    default:
      POKE(0xd020, 2);
      waitframes(10);
      POKE(0xd020, BACKGROUND);
      continue;
    }
    oldch = ch;
    turns++;
  } while(orall(flipflopstatus));
  clrscr();
 runstoppressed:
  if(ch == 0x03) {
    sprintf(buf, "you gave up!");
  } else {
    sprintf(buf, "you needed %d turns!", turns);
  }
  cputsxy(0, 24, buf);
  waitframes(200);
}


void play_game(short key) {
  unsigned char *fliptable;
  if(key < 0) {
    return;
  }
  textcolor(TEXTCOLOR);
  clrscr();
  cputs("generating fliptable!");
  switch(key) {
  case 0x85: // F1
    fliptable = game_original_init();
    break;
  case 0x86: // F3
  case 0x87: // F5
    fliptable = game_double_init();
    break;
  }
  display_gamescreen();
  switch(key) {
  case 0x85: // F1
    flipflop(fliptable, &game_original_flip);
    break;
  case 0x86: // F3
    flipflop(fliptable, &game_double_flip);
    break;
  case 0x87: // F5
    flipflop(fliptable, &game_heavydouble_flip);
    break;
  }
}


void __fastcall__ mainloop(void) {
  clrscr();
  do {
    play_game(titlescreen());
    play_game(selectscreen());
    play_game(helpscreen());
    play_game(credits());
  } while(1);
}
