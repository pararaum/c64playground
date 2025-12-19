#include <stddef.h>
#include <inttypes.h>
#include <peekpoke.h>
#include <conio.h>
#include <t7d/theatre.h>

extern char animation_base_addr[];
extern char copy_major_tom_to_1000[];

struct TheatreSceneConfiguration emptyscene = { 0, 0 };


static void welcome(void) {
  void (*frameinit)(void) = (void*)&animation_base_addr;

  frameinit();
  POKE(1,0x35); // Disable Kernal.
  __asm__("jsr $E000");
  POKE(1,0x36); // Enable Kernal.
}


static void call_animation(int no) {
  char *iptr = &animation_base_addr[0];
  void (*funptr)(void);

  /*
   * The trick is that iptr is a pointer to a character which takes
   * one byte in memory, each jump in the jumptable is three bytes and
   * we will increment. No error checking so make sure you get the
   * right entry otherwise chaos will ensue.
   */
  iptr += no * 3;
  funptr = (void*)iptr;
  funptr();
}

static void animation(void) {
  const int waittime = 35;
  uint8_t old = PEEK(1);
  int i;
  POKE(1,0x35); // Disable Kernal.

  for(i = 0; i < 27; ++i) {
    call_animation(i);
    theatre_wait_frames(waittime);
  }
  POKE(1, old);
}

static void nothing(void) {
  __asm__("cli");
}


void main(void) {
  theatre_init(&nothing, (void *)0x1003, 0);
  act_init(THEATRE_COPY_CHARGEN, NULL, 0, 0, &copy_major_tom_to_1000[0]);
  scene_init(&emptyscene, welcome, animation, NULL);
}
