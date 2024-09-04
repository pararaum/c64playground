#include <t7d/theatre.h>
#include <stdio.h>
#include <conio.h>
#include <string.h>
#include <stdlib.h>

static void putstr_controlled(const char *s, Theatre_control_before_char funptr) {
  int delay = -1;

  while(*s) {
    if(funptr) {
      delay = funptr(*s);
    }
    putchar(*s++);
    if(delay > 0) {
      theatre_wait_frames(delay);
    }
  }
}


static void output_wrapped(const char *s, Theatre_control_before_char funptr, int8_t with_sep) {
  size_t l;
  int rightws;
  char ch;
  
  if(s != NULL) {
    if(*s == '\n') {
      /* The string starts with a CR we assume that no wrapping is
	 needed, if the string is broader than the screen it is kaputt
	 anyway. There for we can just output the string and hope for
	 the best. */
    } else {
      l = strlen(s);
      // Find the  whitespaces on the right side of the string.
      for(rightws = l - 1; rightws >= 0; --rightws) {
	ch = s[rightws];
	if(!((ch == ' ') || (ch == '\n'))) {
	  break;
	}
      }
      //rightws is zero if string consists only of whitespaces...
      /*
       * wherex() is the current cursor position, add one for the
       * space and add the number of characters up to the last
       * non-whitespace character. This should be still within the
       * window.
       */
      if(wherex() + 1 + rightws > window_x2) { // We need wrapping.
	putchar('\n');
      } else {
	// Otherwise just output a space if a separator is needed. No
	// separator in the first column, though!
	if(with_sep && (wherex() != window_x1)) {
	  putchar(' ');
	}
      }
    }
    putstr_controlled(s, funptr); // Just output.
  }
}


void theatre_output_wrapped(const char *s, Theatre_control_before_char funptr) {
  char *buf;
  size_t len; // Length of the whole string.
  char *ptr;
  const char *emergency_ptr;

  len = strlen(s); // Length of the whole string.
  if(len == 0) {
    return; // Empty string?
  }
  buf = strdup(s);
  if(buf == NULL) { // We failed to acquire memory...
    for(emergency_ptr = s; *emergency_ptr; ++emergency_ptr) {
      putchar(*emergency_ptr);
    }
  } else {
    ptr = strtok(buf, " _");
    output_wrapped(ptr, funptr, 0);
    ptr = strtok(NULL, " _");
    while(ptr != NULL) {
      output_wrapped(ptr, funptr, 1);
      ptr = strtok(NULL, " _");
    }
  }
  if(s[len - 1] == ' ') {
    // We have a space at the end? Then print it to have nicer
    // output...
    putchar(' ');
  }
  free(buf);
}


void theatre_switch_to_windowed(uint8_t x1, uint8_t y1, uint8_t x2, uint8_t y2, int8_t fc, const char *text, Theatre_control_before_char funptr) {
  window_x1 = x1;
  window_y1 = y1;
  window_x2 = x2;
  window_y2 = y2;
  if(fc >= 0) {
    if(fc < 16) {
      frame_color = fc;
    }
    draw_frame_sr();
  }
  enable_chrout2window();
  putchar(0x93); /*clear screen*/
  theatre_output_wrapped(text, funptr);
}
