#ifndef __FSVSCROLL_H_20220906__
#define __FSVSCROLL_H_20220906__

typedef struct FSVScroll_InitScroll {
  void *framedata;
  void *screenaddr;
  void *spareaddr;
  unsigned char height;
} FSVScroll_InitScroll;

void fastcall fsvscroll_init(const FSVScroll_InitScroll *initdata);

void fsvscroll_update_softscroll_up_1(void);


#endif
