#include "satellite-crash.h"
#include <stdio.h>
#include <conio.h>
#include <inttypes.h>
#include <peekpoke.h>
#include <t7d/theatre.h>


void welcome(void) {
  POKE(1,0x35); // Disable Kernal.
  asm("jsr $e000");
  POKE(1,0x36); // Enable Kernal.
  theatre_wait_frames(75);
  //DO NOT USE! Spare is at $fc00...: theatre_copy_text2spare();
  textcolor(6);
  theatre_switch_to_windowed(3, 5, 24, 18, 6, "in the last demo we have shown you the ski jumper full of energy!\n\nit ended badly for the ski jumper when he hit the high-voltage power-line...\n\nand only ashes were left...", NULL);
  disable_chrout2window();
  theatre_wait_frames(200);
}

void animation(void) {
  const int waittime = 50;
  uint8_t old = PEEK(1);
  POKE(1,0x35); // Disable Kernal.

  asm("jsr $e000");
  theatre_wait_frames(waittime);
  asm("jsr $e000+3");
  theatre_wait_frames(waittime);
  asm("jsr $e000+6");
  theatre_wait_frames(waittime);
  asm("jsr $e000+9");
  theatre_wait_frames(waittime);
  asm("jsr $e000+12");
  theatre_wait_frames(waittime);
  asm("jsr $e000+15");
  theatre_wait_frames(waittime);
  asm("jsr $e000+18");
  theatre_wait_frames(waittime);
  asm("jsr $e000+21");
  theatre_wait_frames(waittime);
  asm("jsr $e000+24");
  theatre_wait_frames(waittime);
  asm("jsr $e000+27");
  theatre_wait_frames(waittime);
  asm("jsr $e000+30");
  theatre_wait_frames(waittime);
  asm("jsr $e000+33");
  theatre_wait_frames(waittime);
  asm("jsr $e000+36");
  theatre_wait_frames(waittime);
  asm("jsr $e000+39");
  theatre_wait_frames(waittime*2);
  POKE(1, old);
}


void animation2(void) {
  const int waittime = 6;
  uint8_t old = PEEK(1);
  POKE(1,0x35); // Disable Kernal.

  asm("jsr $e000+42");
  theatre_wait_frames(waittime*4);
  asm("jsr $e000+45");
  theatre_wait_frames(waittime);
  asm("jsr $e000+48");
  theatre_wait_frames(waittime);
  asm("jsr $e000+51");
  theatre_wait_frames(waittime);
  asm("jsr $e000+54");
  theatre_wait_frames(waittime);
  asm("jsr $e000+57");
  theatre_wait_frames(waittime);
  asm("jsr $e000+60");
  theatre_wait_frames(waittime);
  asm("jsr $e000+63");
  theatre_wait_frames(waittime);
  asm("jsr $e000+66");
  theatre_wait_frames(waittime);
  asm("jsr $e000+69");
  theatre_wait_frames(waittime);
  asm("jsr $e000+72");
  theatre_wait_frames(waittime);
  asm("jsr $e000+75");
  theatre_wait_frames(waittime);
  asm("jsr $e000+78");
  theatre_wait_frames(waittime);
  asm("jsr $e000+81");
  theatre_wait_frames(waittime);
  asm("jsr $e000+84");
  theatre_wait_frames(waittime);
  asm("jsr $e000+87");
  POKE(1, old);
}

void scene_resurrection(void) {
  act_init(THEATRE_COPY_CHARGEN, NULL, 1, 1, NULL);
  scene_init(&emptyscene, welcome, animation, animation2, NULL);
}
