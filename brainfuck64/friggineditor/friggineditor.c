#include <stdio.h>
#include <peekpoke.h>
#include <conio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "helpers.h"
#include "diskmoiety.h"

struct LogicalLine {
  unsigned char beginningY_line; //!< y position of a logical line, =255 means unused
  unsigned char endY_line; //!< last y position of a logical line, =255 means unused
  char *scrptr; //!< pointer to the screen RAM for this line.
  TextLine *node; //!< pointer to the node of this line.
};


void init_screen() {
  POKE(53280u, 6);
  POKE(53281u, 6);
  putchar(0x9f); // Cyan
}


struct LogicalLine *display_lines(TextLine *head) {
  char *s;
  unsigned char i = 0;
  unsigned char y = 0;
  static struct LogicalLine all_lines[24];

  memset(all_lines, 0xFF, sizeof(all_lines)); // Clear everything with illegal values.
  while((head != NULL) && (wherey() < 24)) {
    gotoxy(0, y); // Move to the top left of the logical line.
    all_lines[i].scrptr = (unsigned char*)0x400 + 40 * y;
    all_lines[i].beginningY_line = y; // Store current Y position in data structure.
    all_lines[i].node = head;
    for(s = head->line; *s; ++s) { // Output the string character by character.
      putchar(*s);
      if(wherey() > 23) {
	head = NULL; // Break the loop.
	break;
      }
    }
    putchar(0x5f); // left arrow
    all_lines[i].endY_line = wherey();
    y = wherey() + 1;
    head = doublili_next(head);
    ++i;
  }
  return all_lines;
}

void output_statusline(const char *s) {
  char *screen = (char *)0x400 + 24 * 40;
  int ch;

  for(; *s; ++s, ++screen) {
    ch = *s;
    if(ch < 64) {
    } else if(ch < 96) {
      ch -= 64;
    } else if(ch < 128) {
      ch -= 32;
    }
    *screen = ch | 0x80;
  }
}

/*! \brief calculate the number of lines an amount of characters needs
 *
 * \param crsrx logical x-position, may be larger than 80
 * \return number of lines this amount of characters needs
 */
int calc_lines_from_chars(int *crsrx) {
  int y = 0;

  while(*crsrx >= 40) {
    y += 1;
    *crsrx -= 40;
  }
  return y;
}

#define EDIT_LINE__OVERFLOWN -1
/*! \brief edit a single line
 *
 * The line is edited and modified. After this function leaves the
 * string has been updated.
 *
 * \param text pointer to the string pointer
 * \param crsry pointer to the screen line the text should begin
 * \param logxbp logical x position aka byte position of the cursor in the whole buffer 
 * \return character read which stopped the editing
 */
int edit_line(char **text, int *crsry, int *logxbp) {
  int crsrx = 0; //!< cursor screen coordinates x
  int ch = 0;
  int linelength = strlen(*text); // Actual length of the text line.
  int maxchars_before_expand = ((linelength / 40) + 1) * 40 - 1;
  char *loglineptr = (char*)(0x400) + *crsry * 40;
  unsigned char beginningY = *crsry; // Y position where the logical line begins.
  char statusline[40];

  // ECF0, screen lines low-byte
  // D1 screenptr
  // D9-F2
  // F3 colourramptr
  bzero(edit_line_buf, sizeof(edit_line_buf));
  strcpy(edit_line_buf, *text);
  if(*logxbp > linelength) {
    *logxbp = linelength;
  }
  crsrx = *logxbp;
  *crsry += calc_lines_from_chars(&crsrx);
  gotoxy(crsrx, *crsry);
  while((ch = cgetc()) != 3) {
    put_char_in_logical_line(linelength, 0x5f);
    put_char_in_logical_line(maxchars_before_expand, 0x9f);    
    POKE(53280u, PEEK(653));
    if(ch == 13) {
      edit_line_buf[linelength] = 0; // End line for safety.
      break;
    } else if(ch == 0x14) { // Back space aka delete.
      if(*logxbp > 0) {
	*logxbp -= 1;
	memmove(loglineptr + *logxbp, loglineptr + *logxbp + 1, linelength - *logxbp);
	memmove(edit_line_buf + *logxbp, edit_line_buf + *logxbp + 1, linelength - *logxbp);
	put_char_in_logical_line(linelength, ' ');
	ch = 0x9d; //Cursor to the left.
	--linelength; // Line looses one character.
      } else {
	ch = 3; // Do nothing.
      }
    } else if(ch == 148) { // Insert
	memmove(loglineptr + *logxbp + 1, loglineptr + *logxbp, linelength - *logxbp);
	memmove(edit_line_buf + *logxbp + 1, edit_line_buf + *logxbp, linelength - *logxbp);
	loglineptr[*logxbp] = ' ';
	edit_line_buf[*logxbp] = ' ';
	linelength++;
	*logxbp += 1;
	ch = ' ';
    } else if(ch == 0x9d) { // Cursor left
      if(*logxbp > 0) {
	*logxbp -= 1;
      } else {
	ch = 0; // We are at the leftmost position, no movement, please.
      }
    } else if(ch == 0x1d) { // Cursor right
      if(*logxbp < linelength) {
	*logxbp += 1; // Move cursor to the right if not end of line.
	putchar(0x1d);
      }
      ch = 0; // No character treatment.
    } else if(ch == 19) { // Home
      gotoxy(0, beginningY);
      *logxbp = 0;
      ch = 0; // No character treatment.
    } else if(ch == 5) { // CBM+E
      *logxbp = linelength; // Move to end of logical line.
      crsrx = *logxbp; // Preload x-position
      *crsry += calc_lines_from_chars(&crsrx); // Adjust X,Y-position.
      gotoxy(crsrx, *crsry); // And move the cursor.
      ch = 0;
    } else if((ch < ' ') || ((ch >= 0x80) && (ch < 0xa0))) { // Unknown control character.
      //printf("#%d#\n", ch);
      POKE(53280u, 3);
      break;
    } else {
      edit_line_buf[*logxbp] = ch;
      *logxbp += 1;
    }
    if(*logxbp > linelength) {
      linelength = *logxbp; // Expand the current line.
    }
    if(ch != 0) { // Should the character be handled.
      putchar(ch);
      if(*logxbp > maxchars_before_expand) { // Line has grown...
	// Clear next line.
	memset(loglineptr + maxchars_before_expand + 1, ' ', 40);
	maxchars_before_expand += 40;
	ch = EDIT_LINE__OVERFLOWN;
	break;
      }
    }
    crsrx = wherex();
    *crsry = wherey();
    sprintf(statusline, " %4d %4d %d/%d $%04X ", crsrx, *crsry, *logxbp, linelength, edit_line_buf);
    output_statusline(statusline);
  }
  free(*text);
  *text = strdup(edit_line_buf);
  return ch;
}

TextLine *editor(TextLine *head) {
  int ch;
  int crsry; //!< cursor screen coordinates
  int logxbp = 0;
  TextLine *node;
  TextLine *newnode;
  char redraw = '\0'; // Flag for redrawing the screen.
  struct LogicalLine *loglines; // Pointer to the logical lines
  unsigned char current_log_line = 0; // We do beginn at the beginning of the screen.

  clrscr();
  loglines = display_lines(head); // Display all lines.
  cursor(1); // Display a cursor.
  node = head;
  do {
    gotoxy(0,20);
    printf("\nch=%d, currentline: %d, logxbp: %d.\n", ch, current_log_line, (int)logxbp);
    assert(loglines[current_log_line].beginningY_line != 0xFF);
    crsry = loglines[current_log_line].beginningY_line;
    node = loglines[current_log_line].node;
    ch = edit_line(&node->line, &crsry, &logxbp);
    switch(ch) {
    case 13: // RETURN
      newnode = allocate_line(&edit_line_buf[logxbp]); // Create a new line from the current position.
      node->line[logxbp] = 0; // Finish current line at this byte.
      node = (void *)doublili_insertafter(node, newnode); // Insert into list of nodes.
      current_log_line++; // Next line.
      redraw = 'R';
      logxbp = 0; // Move to beginning of line.
      break;
    case 17: // Cursor down
      if(loglines[++current_log_line].beginningY_line != 0xFF) {
      } else {
	--current_log_line; // No way to go down.
      }
      break;
    case 145: // Cursor up
      if(current_log_line > 0) {
	--current_log_line;
      }
      break;
    case EDIT_LINE__OVERFLOWN:
      redraw = 'R';
      break;
    default:
      break;
    }
    if(redraw) {
      clrscr();
      loglines = display_lines(head);
      redraw = 0;
    }
  } while(ch != 3); // Until RUN/STOP?
  return head;
}

int main(void) {
  char running = 1;
  char menu = 1;
  int ch;
  TextLine *head = NULL;

  do {
    init_screen();
    printf("editor buffer: %04X\n", edit_line_buf);
    do {
      puts("F1 = edit, F3 = load, F5 = save, F7 = setup, RUN/STOP=exit");
      ch = cgetc();
      switch(ch) {
      case 3: //RUN/STOP
	running = 0;
	menu = 0;
	break;
      case 133: //F1
	if(head == NULL) {
	  head = allocate_line(""); // Insert empty line if nothing.
	}
	editor(head);
	printf("text at $%04X\n", head);
	break;
      case 134: //F3
	head = load_file();
	printf("text at $%04X\n", head);
	break;
      case 135: //F5
	save_file(head);
	break;
      case 136: //F7
	POKE(53280u, PEEK(53280u) + 1);
	break;
      };
    } while(menu);
  } while(running);
  return 0;
}
