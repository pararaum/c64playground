#ifndef __XORSHIFTPRNG_H__2020
#define __XORSHIFTPRNG_H__2020
#include <inttypes.h>

/*! \brief Very simple PRNG
 *
 * Base on:
 * * https://codebase64.org/doku.php?id=base:16bit_xorshift_random_generator
 * * http://www.retroprogramming.com/2017/07/xorshift-pseudorandom-numbers-in-z80.html
 *
 * \return net random number
 */
uint16_t xorshiftprng(void);

#endif
