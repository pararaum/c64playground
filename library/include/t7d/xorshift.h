#ifndef __XORSHIFT_H__
#define __XORSHIFT_H__

/*! \brief Get the next value from the PRNG.
 *
 * \return next random value 
 */
unsigned int xorshift(void);

/*! \brief Initialise the PRNG
 *
 * \param seed seed value for random number generator
 */
void xorshift_seed(unsigned int seed);

#endif
