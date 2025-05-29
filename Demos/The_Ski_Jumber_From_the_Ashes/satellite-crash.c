#include <stdio.h>
#include <peekpoke.h>
#include <t7d/theatre.h>
#include "satellite-crash.h"

extern char muzak_init[];
extern char muzak_play[];

struct TheatreSceneConfiguration emptyscene = { NULL, NULL };


void endless(void) {
  asm("jmp *");
}

void main(void) {
  theatre_init(muzak_init, muzak_play, 0);
  scene_resurrection();
  asm("jmp *");
}
