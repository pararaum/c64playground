#include "xorshiftprng.h"

static uint16_t xorshift_internal_state = 0xAAAA;

uint16_t xorshiftprng(void) {
  xorshift_internal_state ^= xorshift_internal_state << 7;
  xorshift_internal_state ^= xorshift_internal_state >> 9;
  xorshift_internal_state ^= xorshift_internal_state << 8;
  return xorshift_internal_state;
}
