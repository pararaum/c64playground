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
  char *name;
  char *type;
} DirEntry;

char linebuf[254];

void skip_link(FILE *f) {
  /* int c1, c2; */
  /* c1 = fgetc(f); */
  /* c2 = fgetc(f); */
  /* printf("skip_link %d %d\n", c1, c2); */
  fgetc(f);
  fgetc(f);
}

unsigned int read16bit(FILE *f) {
  unsigned int v;

  v = fgetc(f);
  v |= (unsigned char)fgetc(f) << 8;
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

/* \brief skip characters in stream until '"' occurs
 *
 * \param f file stream 
 * \return '\0', '"', or EOF, only '"' means succesful
 */
int skip_until_22(FILE *f) {
  int c;

  do {
    c = fgetc(f);
  } while((c != '"') && (c != 0) && (c != EOF));
  return c;
}

void skip_ws(FILE *f) {
  int c;
  do {
    c = fgetc(f);
    if((c == 0) || (c == ' ')) {
    } else {
      ungetc(c, f); // Push back to stream.
      break;
    }
  } while(c != EOF);
}

/*
 * \param f file stream to read from
 * \param size pointer where to store the file size in blocks
 * \param type pointer to store the type string
 * \return file name or NULL
 */
char *read_dir_line(FILE *f, unsigned int *size, char **type) {
  char *fname;

  // First two bytes are the link, usually just $01 $01.
  skip_link(f);
  *size = read16bit(f); // Read the size as binary.
  if(skip_until_22(f) != '"') { // EOF or blocks free...
    return NULL;
  }
  fname = read_until_char(f, '"');
  skip_ws(f);
  *type = read_until_char(f, 0);
  return fname;
}


void print_dirline(void *node) {
  DirEntry *_node = node;

  printf("%u '%s',%s\n", _node->size, _node->name, _node->type);
}

DirEntry *read_dir_lines(FILE *f) {
  DirEntry *curr;
  DirEntry *head = NULL;

  while(!feof(f)) {
    curr = malloc(sizeof(DirEntry));
    curr->name = read_dir_line(f, &(curr->size), &(curr->type));
    //printf("read %04X %s %u %s\n", curr->name, curr->name, curr->size, curr->type);
    if(curr->name == NULL) { // EOF or so...
      free(curr); // Free again, as no luck here.
      break;
    } else {
      doublili_insert((void*)&head, curr);
    }
  }
  return head;
}
  

void read_header(FILE *f) {
  int c;
  unsigned int s;
  char *diskname;
  char *disktype;
  DirEntry *head;

  // skip load address
  fgetc(f);
  fgetc(f);
  diskname = read_dir_line(f, &s, &disktype);
  printf("%u: '%s', '%s'\n", s, diskname, disktype);
  free(diskname);
  free(disktype);
  puts("Reading Dir");
  head = read_dir_lines(f);
  puts("Reading Dir Done");
  printf("head = %04X\n", head);
  doublili_foreach(head, &print_dirline);
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
