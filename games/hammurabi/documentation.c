#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <conio.h>
#include <peekpoke.h>

#define MAXCOLUMN 39

const char *documentation = "This game is a port of the 1973 BASIC game 'Hammurabi' to C. It has been enhanced with some graphics and music.\n"
  "In this game you impersonate Hammurabi, the administrator of Sumeria. You start of with 1000 acres, 100 people and 3000 bushels of grain. The games lasts for ten turns (years).\n"
  "At each turn you may buy or sell land, the price will vary each turn.\n"
  "Then the land must be sown with grain so that a harvest is possible. For seeding a sufficient number of people is needed.\n"
  "Most importantly the people need to be fed. Hunger leads to starvation and dead people. If too many people die, an uprising may occur. So choose wisely, Hammurabi!\n"
  "With increasing difficulty level the variation of prices and even the number of grains needed for different actions will increasingly vary.\n"
  ;

static void any_key_to_continue(void) {
  unsigned char c = textcolor(6);
  printf("\n      Press any key for next page!");
  (void)cgetc();
  textcolor(c);
}

static void gio_word_wrap_string(const char *s) {
  size_t l;

  if(s != NULL) {
    l = strlen(s);
    if(wherex() + l + 1 >= MAXCOLUMN) { // We need wrapping.
      putchar('\n');
    }
    if(wherey() >= 22) {
      any_key_to_continue();
      putchar('\f');
    }
    printf("%s", s); // Just output.
  }
}

void gio_putstr(const char *s) {
  char *buf = strdup(s);
  size_t len = strlen(s); // Length of the whole string.
  char *ptr;
  
  if(len == 0) {
    return; // Empty string?
  }
  if(buf == NULL) { // We failed to acquire memory...
    printf("%s", s);
  } else {
    ptr = strtok(buf, " _");
    gio_word_wrap_string(ptr);
    ptr = strtok(NULL, " _");
    while(ptr != NULL) {
      putchar(' ');
      gio_word_wrap_string(ptr);
      ptr = strtok(NULL, " _");
    }
  }
  free(buf);
}


void putparagraph(const char *s) {
  char *buf = strdup(s);
  char *ptr;
  char *oldptr;
  
  if(buf == NULL) { // We failed to acquire memory...
    printf("%s", s);
  } else {
    // We need to write our own version of strtok here, as it is used above and it is *not* reentrant!
    for(oldptr = buf, ptr = strchr(buf, '\n'); ptr != NULL; oldptr = ++ptr, ptr = strchr(oldptr, '\n')) {
      *ptr = 0; // Terminate the string.
      gio_putstr(oldptr);
      putchar('\n');
      if(wherey() >= 22) {
	any_key_to_continue();
	putchar('\f');
      } else {
	puts("\n");
      }
    }
  }
  free(buf);
}


int main(char, char **) {
  char *ptr;
  char c;

  *(unsigned char *)(198)=0;	//emtpy the keyboard buffer
  POKE(0xd020, 1);
  POKE(0xd021, 1);
  cputsxy(5, 11, "Read the introduction (Y/N)?");
  do 
    c = cgetc();
  while (c != 'Y' && c != 'y' && c != 'N' && c != 'n');
    
  if(c == 'Y' || c == 'y') {
    /* do { */
    /*   ptr = malloc(512); */
    /*   printf("ptr=$%x\n", (unsigned)ptr); */
    /* } while(ptr != NULL); */
    putchar('\f');
    putparagraph(documentation);
    any_key_to_continue();
  }

  putchar('\f');
  
  return 0;
}
