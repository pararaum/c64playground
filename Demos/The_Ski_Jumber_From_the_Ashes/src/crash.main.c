#include <stddef.h>
#include <inttypes.h>
#include <peekpoke.h>
#include <conio.h>
#include <string.h>
#include <t7d/theatre.h>
#include <t7d/xorshift.h>
#include "globals.h"

// Maximum number of animation screens to show.
#define MAXSCREENS 7
#define STARCHAR Crash_starchar
#define CRASHPOINTX 10
#define CRASHPOINTY 14

extern void call_animation(uint8_t frame);

extern void starfield(void);
extern volatile unsigned char starfield_finished;

extern char jumper_static[];

/* extern char muzak_init[]; */
/* extern char muzak_play[]; */

struct TheatreSceneConfiguration emptyscene = { 0, 0 };

struct TheatreSceneConfiguration flyscene = {
  &starfield, // ISR
  0, // Interpicture
};


static void nothing(void) {
}


static int delayfun(char c) {
  switch(c) {
  case ',':
  case '.':
  case '!':
    return 13;
  case ':':
    return 49;
  case '\n':
    return 59;
  default:
    return 1;
  }
}


static void welcome(void) {
  unsigned int i;
  
  memcpy(THEATRE_TEXT, &jumper_static[0], 1000);
  memcpy((void*)0xd800, &jumper_static[1000], 1000);

  theatre_copy_text2spare();

  theatre_wait_frames(50*1);
  textcolor(3);
  theatre_switch_to_windowed(4, 2, 36, 10, 15, "our hero is rocketing\ninto the sky\n\ngalaxies are hurtling\nand dashing by!\n\nthe flight seems to last forever\n\nthe destination can not be reached ever...\n", &delayfun);
  theatre_wait_frames(50*4);

  theatre_copy_spare2text();
  disable_chrout2window();
  for(i = (unsigned int)THEATRE_TEXT; i < 1000 + (unsigned int)THEATRE_TEXT; ++i) {
    if(PEEK(i) == STARCHAR) {
      POKE(i, 32);
    }
  }
}


static void goodbye(void) {
  theatre_copy_text2spare();
  theatre_wait_frames(50*3);
  theatre_switch_to_windowed(8, 1, 32, 2, 3, "thank you for watching!", &delayfun);
  theatre_wait_frames(50*3);
  theatre_switch_to_windowed(8, 1, 32, 2, 3, "enjoy transmission 64!", &delayfun);
  theatre_wait_frames(50*3);
  theatre_copy_spare2text();
}


static void last_words(void) {
  uint8_t i;
  unsigned int offset = 19*40+5;  // dirty ski repair in the air
  POKE(THEATRE_TEXT + offset, jumper_static[offset]);
  POKE(0xD800 + offset, jumper_static[1000 + offset]);

  // fix downwards
  offset =  CRASHPOINTX + (CRASHPOINTY + 1) * 40;
  for (i=0; i<5; i++)
  {
    if (PEEK(THEATRE_TEXT + offset)==32 || PEEK(THEATRE_TEXT + offset)==96
        || (PEEK(0xD800 + offset)&0x0f)==0)
    {
      POKE(THEATRE_TEXT + offset, jumper_static[offset]);
      POKE(0xD800 + offset, jumper_static[1000 + offset]);
    }
    offset += 40;
  }

  // fix downwards right
  offset =  CRASHPOINTX + 1 + (CRASHPOINTY + 1) * 40;
  for (i=0; i<5; i++)
  {
    if (PEEK(THEATRE_TEXT + offset)==32 || PEEK(THEATRE_TEXT + offset)==96
        || (PEEK(0xD800 + offset)&0x0f)==0)
    {
      POKE(THEATRE_TEXT + offset, jumper_static[offset]);
      POKE(0xD800 + offset, jumper_static[1000 + offset]);
    }
    offset += 41;
  }

  // fix right
  offset =  CRASHPOINTX + 1 + (CRASHPOINTY) * 40;
  for (i=0; i<5; i++)
  {
    if (PEEK(THEATRE_TEXT + offset)==32 || PEEK(THEATRE_TEXT + offset)==96
        || (PEEK(0xD800 + offset)&0x0f)==0)
    {
      POKE(THEATRE_TEXT + offset, jumper_static[offset]);
      POKE(0xD800 + offset, jumper_static[1000 + offset]);
    }
    offset += 1;
  }

  theatre_copy_text2spare();
  theatre_switch_to_windowed(16, 16, 38, 21, 3, "our hero came to a stop!\n\nthis definitely is not the top!\n", &delayfun);
  theatre_wait_frames(50*4);
  theatre_switch_to_windowed(3, 3, 15, 8, 3, "for another replay\n\nyou do not need to pay...", &delayfun);
  theatre_wait_frames(50*4);
  theatre_copy_spare2text();
}


static void animation(void) {
  uint8_t i;

  call_animation(0);
  for(i = 0; i < MAXSCREENS; ++i) {
    theatre_wait_frames(50 * 1 + 5);
    call_animation(i);
  }
}


static void wait4stars2end(void) {
  while(starfield_finished == 0) {
  }
}


static void show_crash(void) {
  unsigned int l;
  unsigned int offset;
  static const char boomchars[] = {
    35, 42, 43, 46,
    65, 90, 83, 88,
    81, 87, 86, 91,
    102, 230, 127, 255
  };

  for(l = 0; l < 421; ++l) {
    unsigned char rndval = xorshift();
    switch(l & 3) {
    case 0:
      *(THEATRE_TEXT + CRASHPOINTX - 2 + (CRASHPOINTY - 2 + (xorshift() & 0x3)) * 40 + (xorshift() & 0x3)) = ' ';
      POKE(0xd800  + CRASHPOINTX - 2 + (CRASHPOINTY - 2 + (xorshift() & 0x3)) * 40 + (xorshift() & 0x3), ' ');
      break;
    default:
      *(THEATRE_TEXT + CRASHPOINTX - 2 + (CRASHPOINTY - 2 + (xorshift() & 0x3)) * 40 + (xorshift() & 0x3)) = boomchars[rndval & 15];
      POKE(0xd800  + CRASHPOINTX - 2 + (CRASHPOINTY - 2 + (xorshift() & 0x3)) * 40 + (xorshift() & 0x3), rndval >> 4);
    }
    switch(l & 7) {
    case 0:
      offset =  CRASHPOINTX + (CRASHPOINTY - 2) * 40;
      POKE(THEATRE_TEXT + offset - 160, PEEK(THEATRE_TEXT + offset - 120));
      POKE(THEATRE_TEXT + offset - 120, PEEK(THEATRE_TEXT + offset - 80));
      POKE(THEATRE_TEXT + offset - 80, PEEK(THEATRE_TEXT + offset - 40));
      POKE(THEATRE_TEXT + offset - 40, PEEK(THEATRE_TEXT + offset));
      POKE(0xD800 + offset - 160, PEEK(0xD800 + offset - 120));
      POKE(0xD800 + offset - 120, PEEK(0xD800 + offset - 80));
      POKE(0xD800 + offset - 80, PEEK(0xD800 + offset - 40));
      POKE(0xD800 + offset - 40, PEEK(0xD800 + offset));
      break;
    case 1:
      offset =  CRASHPOINTX + (CRASHPOINTY + 1) * 40;
      POKE(THEATRE_TEXT + offset + 160, PEEK(THEATRE_TEXT + offset + 120));
      POKE(THEATRE_TEXT + offset + 120, PEEK(THEATRE_TEXT + offset + 80));
      POKE(THEATRE_TEXT + offset + 80, PEEK(THEATRE_TEXT + offset + 40));
      POKE(THEATRE_TEXT + offset + 40, PEEK(THEATRE_TEXT + offset));
      POKE(0xD800 + offset + 160, PEEK(0xD800 + offset + 120));
      POKE(0xD800 + offset + 120, PEEK(0xD800 + offset + 80));
      POKE(0xD800 + offset + 80, PEEK(0xD800 + offset + 40));
      POKE(0xD800 + offset + 40, PEEK(0xD800 + offset));
      break;
    case 2:
      offset =  CRASHPOINTX + 1 + CRASHPOINTY * 40;
      POKE(THEATRE_TEXT + offset + 6, PEEK(THEATRE_TEXT + offset + 5));
      POKE(THEATRE_TEXT + offset + 5, PEEK(THEATRE_TEXT + offset + 4));
      POKE(THEATRE_TEXT + offset + 4, PEEK(THEATRE_TEXT + offset + 3));
      POKE(THEATRE_TEXT + offset + 3, PEEK(THEATRE_TEXT + offset + 2));
      POKE(THEATRE_TEXT + offset + 2, PEEK(THEATRE_TEXT + offset + 1));
      POKE(THEATRE_TEXT + offset + 1, PEEK(THEATRE_TEXT + offset + 0));
      POKE(0xD800 + offset + 6, PEEK(0xD800 + offset + 5));
      POKE(0xD800 + offset + 5, PEEK(0xD800 + offset + 4));
      POKE(0xD800 + offset + 4, PEEK(0xD800 + offset + 3));
      POKE(0xD800 + offset + 3, PEEK(0xD800 + offset + 2));
      POKE(0xD800 + offset + 2, PEEK(0xD800 + offset + 1));
      POKE(0xD800 + offset + 1, PEEK(0xD800 + offset + 0));
      break;
    case 3:
      offset =  CRASHPOINTX - 1 + CRASHPOINTY * 40;
      POKE(THEATRE_TEXT + offset - 6, PEEK(THEATRE_TEXT + offset - 5));
      POKE(THEATRE_TEXT + offset - 5, PEEK(THEATRE_TEXT + offset - 4));
      POKE(THEATRE_TEXT + offset - 4, PEEK(THEATRE_TEXT + offset - 3));
      POKE(THEATRE_TEXT + offset - 3, PEEK(THEATRE_TEXT + offset - 2));
      POKE(THEATRE_TEXT + offset - 2, PEEK(THEATRE_TEXT + offset - 1));
      POKE(THEATRE_TEXT + offset - 1, PEEK(THEATRE_TEXT + offset - 0));
      POKE(0xD800 + offset - 6, PEEK(0xD800 + offset - 5));
      POKE(0xD800 + offset - 5, PEEK(0xD800 + offset - 4));
      POKE(0xD800 + offset - 4, PEEK(0xD800 + offset - 3));
      POKE(0xD800 + offset - 3, PEEK(0xD800 + offset - 2));
      POKE(0xD800 + offset - 2, PEEK(0xD800 + offset - 1));
      POKE(0xD800 + offset - 1, PEEK(0xD800 + offset - 0));
      break;
    case 4:
      offset =  CRASHPOINTX + (CRASHPOINTY - 1) * 40;
      POKE(THEATRE_TEXT + offset - 4*39, PEEK(THEATRE_TEXT + offset - 3*39));
      POKE(THEATRE_TEXT + offset - 3*39, PEEK(THEATRE_TEXT + offset - 2*39));
      POKE(THEATRE_TEXT + offset - 2*39, PEEK(THEATRE_TEXT + offset - 39));
      POKE(THEATRE_TEXT + offset - 39, PEEK(THEATRE_TEXT + offset));
      POKE(0xD800 + offset - 4*39, PEEK(0xD800 + offset - 3*39));
      POKE(0xD800 + offset - 3*39, PEEK(0xD800 + offset - 2*39));
      POKE(0xD800 + offset - 2*39, PEEK(0xD800 + offset - 39));
      POKE(0xD800 + offset - 39, PEEK(0xD800 + offset));
      break;
    case 5:
      offset =  CRASHPOINTX + (CRASHPOINTY - 1) * 40;
      POKE(THEATRE_TEXT + offset - 4*41, PEEK(THEATRE_TEXT + offset - 3*41));
      POKE(THEATRE_TEXT + offset - 3*41, PEEK(THEATRE_TEXT + offset - 2*41));
      POKE(THEATRE_TEXT + offset - 2*41, PEEK(THEATRE_TEXT + offset - 41));
      POKE(THEATRE_TEXT + offset - 41, PEEK(THEATRE_TEXT + offset));
      POKE(0xD800 + offset - 4*41, PEEK(0xD800 + offset - 3*41));
      POKE(0xD800 + offset - 3*41, PEEK(0xD800 + offset - 2*41));
      POKE(0xD800 + offset - 2*41, PEEK(0xD800 + offset - 41));
      POKE(0xD800 + offset - 41, PEEK(0xD800 + offset));
      break;
    case 6:
      offset =  CRASHPOINTX - 1 + (CRASHPOINTY + 1) * 40;
      POKE(THEATRE_TEXT + offset + 4*39, PEEK(THEATRE_TEXT + offset + 3*39));
      POKE(THEATRE_TEXT + offset + 3*39, PEEK(THEATRE_TEXT + offset + 2*39));
      POKE(THEATRE_TEXT + offset + 2*39, PEEK(THEATRE_TEXT + offset + 39));
      POKE(THEATRE_TEXT + offset + 39, PEEK(THEATRE_TEXT + offset));
      POKE(0xD800 + offset + 4*39, PEEK(0xD800 + offset + 3*39));
      POKE(0xD800 + offset + 3*39, PEEK(0xD800 + offset + 2*39));
      POKE(0xD800 + offset + 2*39, PEEK(0xD800 + offset + 39));
      POKE(0xD800 + offset + 39, PEEK(0xD800 + offset));
      break;
    case 7:
      offset =  CRASHPOINTX + 1 + (CRASHPOINTY + 1) * 40;
      POKE(THEATRE_TEXT + offset + 4*41, PEEK(THEATRE_TEXT + offset + 3*41));
      POKE(THEATRE_TEXT + offset + 3*41, PEEK(THEATRE_TEXT + offset + 2*41));
      POKE(THEATRE_TEXT + offset + 2*41, PEEK(THEATRE_TEXT + offset + 41));
      POKE(THEATRE_TEXT + offset + 41, PEEK(THEATRE_TEXT + offset));
      POKE(0xD800 + offset + 4*41, PEEK(0xD800 + offset + 3*41));
      POKE(0xD800 + offset + 3*41, PEEK(0xD800 + offset + 2*41));
      POKE(0xD800 + offset + 2*41, PEEK(0xD800 + offset + 41));
      POKE(0xD800 + offset + 41, PEEK(0xD800 + offset));
      break;
    }
  }
}


void main(void) {
  theatre_init(&nothing, (void*)0x1003, 0);
  act_init(THEATRE_COPY_CHARGEN, NULL, 0, 0, NULL);
  scene_init(&emptyscene, welcome, NULL);
  // The main character is flying...
  scene_init(&flyscene, wait4stars2end, show_crash, NULL);
  scene_init(&emptyscene, last_words, animation, goodbye, NULL);
}
