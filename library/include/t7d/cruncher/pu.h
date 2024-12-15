#ifndef __PU_CRUNCHER_202407_h__
#define __PU_CRUNCHER_202407_h__

/*! Decrunch the pu-crunched data at address pudata
 *
 * This function does uses its own zeropage addresses.
 *
 * \param pudata pointer to compressed data (with pu header)
 * \param exec address from pu cruncher
 */
void * __fastcall__ pudecrunch_default(char *pudata);

#endif
