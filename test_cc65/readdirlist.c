/*
 * cl65 -I ../library/include/ readdirlist.c ../library/libT7D.lib 
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <t7d/doublili.h>

typedef struct direntry_t {
  DoubliliNode node;
  unsigned int size; //!< blocks
  const char *name;
  const char *type;
} DirEntry;

char linebuf[254];

void skip_link(FILE *f) {
  fgetc(f);
  fgetc(f);
}

unsigned int read16bit(FILE *f) {
  unsigned int v;

  v = fgetc(f) << 8;
  v |= fgetc(f);
  return v;
}

char *read_until_char(FILE *f, char end) {
  unsigned u;
  int ch;

  for(u = 0; u < sizeof(linebuf); ++u) {
    ch = fgetc(f);
    linebuf[u] = ch;
    if(ch == end) {
      break;
    } else if(ch == EOF) {
      break;
    }
  }
  linebuf[u] = 0;
  return strdup(linebuf);
}

void skip_until_22(FILE *f) {
  int c;

  do {
    c = fgetc(f);
  } while((c != '"') && (c != 0) && (c != EOF));
}

void skip_ws(FILE *f) {
  int c;
  do {
    c = fgetc(f);
  } while((c != ' ') && (c != 0) && (c != EOF));
}

void read_header(FILE *f) {
  int c;
  unsigned int s;
  char *buf;

  // skip load address
  fgetc(f);
  fgetc(f);
  skip_link(f);
  s = read16bit(f); // Read the zero as binary.
  printf("%u: \n", s);
  skip_until_22(f);
  buf = read_until_char(f, '"');
  printf("'%s' ", buf);
  free(buf);
  skip_ws(f);
  buf = read_until_char(f, 0);
  printf("%s\n", buf);
  free(buf);
  while((c = fgetc(f)) != EOF) {
    printf("%02x", c);
  }
}

void main(void) {
  FILE *f;

  f = fopen("$", "rb");
  if(!f) {
    puts("Error while opening directory!");
  } else {
    read_header(f);
    fclose(f);
  }
}
