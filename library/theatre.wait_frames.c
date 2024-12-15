#include <t7d/theatre.h>

void theatre_wait_frames(int count) {
  do {
    theatre_wait_frame();
  } while(count-- > 0);
}
