#include <string.h>
#include <assert.h>
#include <conio.h>
#include <peekpoke.h>
#include <t7d/theatre.h>
#include <t7d/sprite.h>
#include <t7d/libT7D.h>
#include <t7d/xorshift.h>

void __fastcall__ fader_multiplexer(uint8_t *xpos, uint8_t *ypos);
void __fastcall__ colour_ram_filler(uint8_t color, uint8_t column);

extern uint8_t asset_credits_screen[];

uint8_t sprite_y_pos[] = {
  47, 51, 55, 59, 63, 67, 71, 75, 79, 83, 87, 91, 95, 99, 103, 107, 111, 115, 119, 123, 127, 131, 135, 139, 143, 147, 151, 155, 159, 163, 167, 171, 175, 179, 183, 187, 191, 195, 199, 203, 207, 211, 215, 219, 223, 227, 231, 235, 239, 243, 247,
  0
};
uint8_t sprite_x_pos[sizeof(sprite_y_pos)];

short __fastcall__ waitframes(unsigned short x);

void fader_run(uint8_t colour) {
  uint8_t i;
  int fader_start_pos = 333/2;
  int column;

  for(i = 0; i < sizeof(sprite_y_pos) - 1; ++i) {
    sprite_x_pos[i] = fader_start_pos - (xorshift() % 9);
  }
  sprite_x_pos[i] = 0;
  POKE(0xd015, 0xff); // Sprites on.
  while(fader_start_pos-- > -27) {
    column = (fader_start_pos - 6)/4;
    if((column >= 0) && (column < 40)) {
      colour_ram_filler(colour, column);
    }
    //POKE(0xd020, 1);
    fader_multiplexer(&sprite_x_pos[0], &sprite_y_pos[0]);
    //POKE(0xd020, 3);
    for(i = 0; i < 2; ++i) {
      sprite_x_pos[xorshift() % sizeof(sprite_x_pos)] += 1;
    }
    //POKE(0xd020, 0);
  }
}

static void credits_screen_on(void) {
  fader_run(1);
}

static void credits_screen_off(void) {
  fader_run(0);
}

short creditsscreen(void) {
  short ret = -1;
  POKE(0xd015, 0xFF); // Sprites off.
  set_all_sprites_colour(0);
  xorshift_seed(0xF1A3); // Seed the random number generator.
  credits_screen_on();
  ret = waitframes(200);
  credits_screen_off();
  POKE(0xd015, 0); // Sprites off.
  return ret;
}
