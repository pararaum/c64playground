#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>
#include <peekpoke.h>
#include <conio.h>

/*! \file
 *
 * This contains the main game loop and game logic.
 */

//! The number of Mugwumps.
#define NO_MUGWUMPS 4
//! Maximum number of turns. After this number of turns the game is considered lost.
#define MAXTURNS 10

//! The PRNG function
#define PRNGFUN rand

typedef struct Mugwumps {
  int pos[NO_MUGWUMPS][2]; //!< x,y-positions of all mugwumps
  uint8_t alive; //!< number of mugwumps alive
} Mugwumps_t;


/*! \brief Create the mugwumps
 *
 * This function allocates a Mugwumps structure and positions the
 * Mugwumps randomly.
 *
 * \param gridsize number of grid points in each direction
 */
static Mugwumps_t *create_mugwumps(uint8_t gridsize) {
  Mugwumps_t *mw;
  int x, y;
  uint8_t i, j;

  mw = calloc(sizeof(Mugwumps_t), 1);
  if(mw != NULL) {
    // Do some initialisation.
    for(i = 0; i < NO_MUGWUMPS; ++i) {
      x = PRNGFUN() % gridsize;
      y = PRNGFUN() % gridsize;
#ifdef DEBUG
      printf("PRNG x=%d, y=%d\n", x, y);
#endif
      mw->pos[i][0] = x;
      mw->pos[i][1] = y;
      for(j = 0; j < i; ++j) {
	if((mw->pos[j][0] == x) && (mw->pos[j][1] == y)) { // Another mugwump has the same position!
	  --i; // Backtrace one step.
	  break; // Inner loop.
	}
      }
    }
    mw->alive = NO_MUGWUMPS;
  }
  return mw;
}

/*! \brief Display the distances to the mugwumps
 *
 * Calculate and display the mugwump distances
 *
 * \param x current x-position
 * \param y current y-position
 * \param mw pointer to the mugwumps structure
 */
static void display_distances(uint8_t x, uint8_t y, Mugwumps_t *mw) {
  int i;
  int distance;

  for(i = 0; i < NO_MUGWUMPS; ++i) {
    if(mw->pos[i][0] >= 0) {
      int dx = x - mw->pos[i][0];
      int dy = y - mw->pos[i][1];
      distance = dx * dx + dy * dy;
      printf("(%d, %d) is %d units^2 from mugwump %d.\n", x, y, distance, i+1);
    }
  }
}

/*! \brief Check if a mugwump was found.
 *
 * Loop through the mugwumps and check if one was found.
 *
 * This function will also decrement the number of mugwumps.
 *
 * \param mw pointer to the mugwumps structure
 * \param x x-position to search
 * \param y y-position to search
 * \return -1 = no mugwump found, [0;NO_MUGWUMPS-1] = mugwump found
 */
int8_t check_mugwump_find(Mugwumps_t *mw, uint8_t x, uint8_t y) {
  uint8_t i;

  for(i = 0; i < NO_MUGWUMPS; ++i) {
    int mx = mw->pos[i][0];
    int my = mw->pos[i][1];
    if(mx >= 0) {
      if((mx == x) && (my == y)) {
        mw->pos[i][0] = -1;
        --mw->alive;
        return i;
      }
    }
  }
  return -1;
}

uint8_t read_numb(uint8_t from, uint8_t to)
{
  unsigned char c;
  cursor(1);
  do 
  {
    c = cgetc();
  } while (c<from+48 || c>to+48);
  cputc(c);
  cursor(0);
  return c-48;  
}

int read_two_ints2(int *x, int *y) {
  *x = read_numb(0,9);
  cputc(' ');
  *y = read_numb(0,9);
  puts("");
  return 2;
}

/*! \brief Single game run
 */
void game(void) {
  Mugwumps_t *mw;
  unsigned int turn;
  int x, y;
  int8_t i;

  mw = create_mugwumps(10);
  if(mw == NULL) {
    abort();
  } else {
#ifdef DEBUG
    printf("mw = %p\n", mw);
    for(x = 0; x < NO_MUGWUMPS; ++x) {
      printf("\t%d %d\n", mw->pos[x][0], mw->pos[x][1]);
    }
#endif
  }
  for(turn = 1; (mw->alive > 0) && (turn <= MAXTURNS); ++turn) {
    printf("Turn #%d -- What is your guess?\n", turn);
    if(read_two_ints2(&x, &y) < 2) {
      puts("Please enter two numbers!");
      --turn;
      continue;
    }
    i = check_mugwump_find(mw, x, y);
    if(i < 0) {
      puts("There was no mugwump there!");
    } else {
      printf("You have found mugwump %d.\n", (int) i+1);
    }
    display_distances(x, y, mw);
    puts("");
  }
  if(mw->alive > 0) {
    puts("You lost! There are still mugwumps\nhiding out!");
  } else {
    puts("Congratulation! You found all mugwumps.");
  }
  free(mw);
}

/*!\brief Gameloop
 *
 * Basically it calls the game function and asks if another game is
 * wanted.
 */
void gameloop(void) {
  char ch;

  do {
    game();
    puts("Do you like to play again?");
    cursor(1);
    ch = cgetc();
    cputc(ch);
    puts("\n");
    cursor(0);
  } while((ch != 'n') && (ch != 'N') && (ch != EOF));
}

int main(int argc, char **argv) {
  srand((PEEK(0xdc04) << 8) | PEEK(0xd012));
  puts("\fMugwump\n");
  puts("The objective of this game is to find\nfour mugwumps hidden on a 10 times 10\ngrid. The homebase is at position (0,0).\n");
  puts("You get 10 tries. After each try, you\nwill be informed how far you are from\neach mugwump.\n");
  gameloop();
  return 0;
}
