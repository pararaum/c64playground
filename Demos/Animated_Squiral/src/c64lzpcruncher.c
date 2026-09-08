#include <conio.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <cbm.h>
#include <peekpoke.h>
#include <dirent.h>

// Logical file numbers for input and output.
#define INPUT_LFN 8
#define OUTPUT_LFN 9



extern void init_asm(void);
extern void crunchLZP(unsigned short size, int infile, int outfile);

unsigned char *testfun(unsigned char *dest, const unsigned char *src) {
  printf("src %p\n", src);
  printf("dest %p\n", dest);
  *dest++ = *src++;
  return dest;
}

char *read_string(const char *allowed, int maxlen) {
  int pos;
  int ch;
  unsigned char xpos;
  char *string = calloc(maxlen + 1, 1);

  if(string) {
    for(pos = 0; pos < maxlen; ) {
      ch = cgetc();
      switch(ch) {
      case 0x14: // Delete
	if(pos > 0) {
	  xpos = wherex() - 1;
	  cputcxy(xpos, wherey(), ' ');
	  gotox(xpos);
	  --pos;
	}
	break;
      case 0x0d: // Return
	string[pos] = 0;
	return string;
      default:
	if(strchr(allowed, ch)) {
	  cputc(ch);
	  string[pos++] = ch;
	} else {
	  cprintf("#%02X#", ch);
	}
      }
    }
  }
  return NULL;
}

char *input_at(const char *inptext, int x, int y, int maxlen) {
  gotoxy(x, y);
  cprintf("%18s : ", inptext);
  return read_string(" !\"#$%&\'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~", maxlen);
}
  

/*
 * crunch_lzp – simplified C port of the LZP compressor from
 *   pararaum/c64playground  compression/XipZ/lzp.cc
 *
 * Algorithm summary
 * -----------------
 * LZP (Lempel-Ziv Prediction) keeps a small hash-indexed model that
 * predicts the next byte based on the recent byte history.  The
 * output is a stream of (mask, payload) groups:
 *
 *   - Every group starts with a mask byte whose 8 bits describe the
 *     next 8 payload items, LSB first:
 *       0 → literal byte  (bit clear)
 *       1 → run length    (bit set)
 *
 *   - A literal byte is emitted as-is; the model is then updated.
 *
 *   - A run means the decoder should replay <length> bytes from the
 *     model (i.e. the predicted bytes).  The model hash is advanced
 *     for each replayed byte, but the table entry is NOT updated.
 *
 *   - A run of length 0 is the EOF marker (always written at the
 *     end as a set-bit item).
 *
 * Output strategy – 9-byte group buffer
 * --------------------------------------
 * Because the mask byte must precede the payload bytes it describes,
 * but we only know each mask bit at the moment we emit the payload,
 * we accumulate a complete group in a small buffer before writing:
 *
 *   buf[0]       – mask byte (bits filled in as items are added)
 *   buf[1..n]    – payload bytes (up to 8)
 *
 * The buffer is flushed to outfile when all 8 payload slots are used
 * or when the stream ends.  No fseek/ftell is needed.
 *
 * Parameters that match the original
 * -----------------------------------
 *   MODELSIZE     8   → model table has 2^8 = 256 entries
 *   MAX_RUNLENGTH 255 → maximum run length per token
 *   hash update   ((h << 3) + byte) % (1 << MODELSIZE)
 *
 * Return value
 *   Number of bytes written to outfile, or -1 on I/O error.
 */

#include <stdio.h>
#include <stdint.h>
#include <string.h>

/* ------------------------------------------------------------------ */
/* Tuneable constants                                                   */
/* ------------------------------------------------------------------ */

#define MODELSIZE     8                  /* hash bits; table = 2^MODELSIZE entries */
#define MODEL_ENTRIES (1u << MODELSIZE)  /* 256 */
#define MAX_RUNLENGTH 255

/* ------------------------------------------------------------------ */
/* LZP prediction model                                                 */
/* ------------------------------------------------------------------ */

typedef struct {
    uint8_t  *table;
    unsigned hash;
} LZPModel;

// Global due to C64 constraints.
LZPModel model;

static void model_init(LZPModel *m)
{
  m->table = (void*)0x400;
  memset(m->table, 0, 0x100);
  m->hash = 0;
}

/* Advance the rolling hash (does NOT touch the model table). */
static void model_advance(LZPModel *m, uint8_t byte)
{
    m->hash = ((m->hash << 3) + byte) % MODEL_ENTRIES;
}

/* What byte does the model expect next? */
static uint8_t model_predict(const LZPModel *m)
{
    return m->table[m->hash];
}

/* Confirm a literal: store it in the table, then advance the hash. */
static void model_update(LZPModel *m, uint8_t byte)
{
    m->table[m->hash] = byte;
    model_advance(m, byte);
}

/* ------------------------------------------------------------------ */
/* 9-byte group buffer writer                                           */
/* ------------------------------------------------------------------ */

/*
 * Layout of buf[9]:
 *   buf[0]        mask byte  (bit N is set when item N is a run)
 *   buf[1]        payload of item 0
 *   buf[2]        payload of item 1
 *   ...
 *   buf[8]        payload of item 7
 *
 * items_in_group counts how many payload bytes have been added (0-8).
 * When it reaches 8 the group is complete and is flushed immediately.
 */

typedef struct {
    FILE    *outfile;
    uint8_t  buf[9];         /* buf[0] = mask, buf[1..8] = payload */
    unsigned items_in_group; /* how many payload slots are filled   */
    long     bytes_written;
    int      error;
} Writer;

static void writer_init(Writer *w, FILE *outfile)
{
    w->outfile       = outfile;
    w->buf[0]        = 0;    /* mask starts clear */
    w->items_in_group = 0;
    w->bytes_written = 0;
    w->error         = 0;
}

/* Write every byte of the current group to outfile and reset it. */
static void flush_group(Writer *w)
{
    unsigned n;

    if (w->error || w->items_in_group == 0)
        return;

    /* Write mask + payload bytes (1 + items_in_group bytes total). */
    n = 1 + w->items_in_group;
    if (fwrite(w->buf, 1, n, w->outfile) != n) {
        w->error = 1;
        return;
    }
    w->bytes_written += (long)n;

    /* Reset for the next group. */
    w->buf[0]         = 0;
    w->items_in_group = 0;
}

/*
 * Add one item to the current group.
 *   is_run  – 1 if this is a run token, 0 if it is a literal
 *   payload – the run-length or literal byte value
 *
 * The mask bit for this item's position is set when is_run == 1.
 * Once 8 items have accumulated the group is flushed automatically.
 */
static void emit_item(Writer *w, int is_run, uint8_t payload)
{
    if (w->error)
        return;

    if (is_run)
        w->buf[0] |= (uint8_t)(1u << w->items_in_group);

    w->buf[1 + w->items_in_group] = payload;
    w->items_in_group++;

    if (w->items_in_group == 8)
        flush_group(w);
}

/* ------------------------------------------------------------------ */
/* Main compressor                                                      */
/* ------------------------------------------------------------------ */

/*
 * crunch_lzp
 *
 *   Reads all bytes from infile, compresses them with LZP, and writes
 *   the compressed stream to outfile.
 *
 *   Returns the number of compressed bytes written, or -1 on error.
 */
long crunch_lzp(FILE *infile, FILE *outfile)
{
    Writer   w;
    int      c;
    uint8_t  cur;

    model_init(&model);
    writer_init(&w, outfile);

    /* Prime the pump: read the very first byte. */
    c = fgetc(infile);
    if (c == EOF) {
        /* Empty input – write just the EOF token. */
        emit_item(&w, 1, 0);
        flush_group(&w);
        return w.error ? -1 : w.bytes_written;
    }
    cur = (uint8_t)c;

    while (1) {
        /*
         * Try to build a run starting at 'cur'.
         * A run extends while the current byte matches the model's
         * prediction, up to MAX_RUNLENGTH bytes.
         */
        unsigned runlength = 0;
        uint8_t  next = cur;

        while (runlength < MAX_RUNLENGTH) {
            if (next != model_predict(&model))
                break;                  /* prediction miss – end of run */

            model_advance(&model, next);
            runlength++;

            c = fgetc(infile);
            if (c == EOF) {
                next = 0;               /* won't be used as cur after this */
                break;
            }
            next = (uint8_t)c;
        }

        if (runlength > 0) {
            emit_item(&w, 1, (uint8_t)runlength);

            if (c == EOF) {
                /* All remaining input was consumed inside the run. */
                goto write_eof;
            }
            cur = next;                 /* first byte that broke the run */
        } else {
            /* No run: emit cur as a literal. */
            emit_item(&w, 0, cur);
            model_update(&model, cur);

            c = fgetc(infile);
            if (c == EOF)
                goto write_eof;
            cur = (uint8_t)c;
        }
    }

write_eof:
    /* A run token with length 0 signals end-of-stream to the decoder. */
    emit_item(&w, 1, 0);
    flush_group(&w);

    return w.error ? -1 : w.bytes_written;
}


typedef struct Node {
  struct Node *next;
  char *filename;
  int blocks;
  unsigned char type;
} Node;

// Insert at head (O(1))
Node* insert(Node *head, const char *fname, int blocks, unsigned char type) {
  Node *new_node = malloc(sizeof(Node));
  if(new_node) {
    new_node->filename = strdup(fname);
    new_node->blocks = blocks;
    new_node->type = type;
    new_node->next = head;
  }
  return new_node;
}


void swap_data(Node *nx, Node *ny) {
  Node tmp;

  tmp = *nx;
  nx->filename = ny->filename;
  nx->blocks = ny->blocks;
  nx->type = ny->type;
  ny->filename = tmp.filename;
  ny->blocks = tmp.blocks;
  ny->type = tmp.type;
}

// Selection sort (O(n^2) time, O(1) space)
void selection_sort(Node *head) {
  Node *i;
  Node *j;
  for (i = head; i != NULL; i = i->next) {
    Node *min = i;
    for (j = i->next; j != NULL; j = j->next) {
      if (strcmp(j->filename, min->filename) < 0) min = j;
    }
    if (min != i) {
      swap_data(min, i);
    }
  }
}

// Free entire list (O(n))
void free_list(Node *head) {
  while (head != NULL) {
    Node *tmp = head;
    head = head->next;
    free(tmp->filename);
    free(tmp);
  }
}


void read_dir(void) {
  struct dirent *dirent;
  DIR *dir = opendir(".");
  struct Node *wholedir = NULL;
  struct Node *node;

  if(dir) {
    while(dirent = readdir(dir)) {
      printf("%s %d %d\n", dirent->d_name, dirent->d_blocks, (int)(dirent->d_type));
      wholedir = insert(wholedir, dirent->d_name, dirent->d_blocks, dirent->d_type);
    }
    closedir(dir);
    selection_sort(wholedir);
    for(node = wholedir; node; node = node->next) {
      printf("%p: %s %d\n", (void*)node, node->filename, node->blocks);
    }
    free_list(wholedir);
  } else {
    puts("Can not open dir!\n");
  }
}


int main (void) {
  char *inpfnam, *outfnam;
  FILE *infile, *outfile;
  int i, ch;

  init_asm();
  bgcolor(3);
  bordercolor(5);
  textcolor(11);
  clrscr();
  read_dir();
  return 0;
  
  cputsxy(14, 1, "LZP-Cruncher");
  chlinexy(0, 3, 40);
  cursor(1);
  inpfnam = input_at("Input File Name", 0, 5, 20);
  outfnam = input_at("Output File Name", 0, 6, 20);
  chlinexy(0, 8, 40);
  gotoxy(0,10);
  //infile = fopen(inpfnam, "rb");
  if((i = cbm_open(INPUT_LFN, 8, CBM_READ, inpfnam)) != 0) {
    printf("Error = %d! Can not open input!\n", i);
  } else {
    printf("Openend '%s' for input.\n", inpfnam);
    //outfile = fopen(outfnam, "wb");
    if(cbm_open(OUTPUT_LFN, 8, CBM_WRITE, outfnam) != 0) {
      puts("Error! Can not open output!\n");
    } else {
      printf("Openend '%s' for output.\n", outfnam, outfile);
      crunchLZP(0xaabbU, INPUT_LFN, OUTPUT_LFN);
      //crunch_lzp(infile, outfile);
      cbm_close(OUTPUT_LFN);
    }
    cbm_close(INPUT_LFN);
  }
  return 0;
}
