#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>
#include <peekpoke.h>
#include <conio.h>

/*! \file
 *
 * This contains the main game loop and game logic.
 */

//! The number of Astronauts.
#define NO_ASTRONAUTS 4
//! Maximum number of turns. After this number of turns the game is considered lost.
#define MAXTURNS 10

int xcoor, ycoor;

typedef struct Astronauts {
  int pos[NO_ASTRONAUTS][2]; //!< x,y-positions of all astronauts
  uint8_t alive; //!< number of astronauts alive
} Astronauts_t;


/*! \brief Create the astronauts
 *
 * This function allocates a Astronauts structure and positions the
 * Astronauts randomly.
 *
 * \param gridsize number of grid points in each direction
 */
static Astronauts_t *create_astronauts(uint8_t gridsize) {
  Astronauts_t *mw;
  int x, y;
  uint8_t i, j;

  mw = calloc(sizeof(Astronauts_t), 1);
  if(mw != NULL) {
    // Do some initialisation.
    for(i = 0; i < NO_ASTRONAUTS; ++i) {
      x = rand() % gridsize;
      y = rand() % gridsize;
#ifdef DEBUG
      printf("PRNG x=%d, y=%d\n", x, y);
#endif
      mw->pos[i][0] = x;
      mw->pos[i][1] = y;
      for(j = 0; j < i; ++j) {
	if((mw->pos[j][0] == x) && (mw->pos[j][1] == y)) { // Another astronaut has the same position!
	  --i; // Backtrace one step.
	  break; // Inner loop.
	}
      }
    }
    mw->alive = NO_ASTRONAUTS;
  }
  return mw;
}

char *get_name(int i) {
  char *name;
  switch(i) {
  case 0:
    name = "Neil";
    break;
  case 1:
    name = "Buzz";
    break;
  case 2:
    name = "Juri";
    break;
  case 3:
    name = "Valentina";
    break;
  default:
    name = "***ERROR***";
  }
  return name;
}

/*! \brief Display the distances to the astronauts
 *
 * Calculate and display the astronaut distances
 *
 * \param x current x-position
 * \param y current y-position
 * \param mw pointer to the astronauts structure
 */
static void display_distances(uint8_t x, uint8_t y, Astronauts_t *mw) {
  int i;
  int distance;

  for(i = 0; i < NO_ASTRONAUTS; ++i) {
    if(mw->pos[i][0] >= 0) {
      int dx = x - mw->pos[i][0];
      int dy = y - mw->pos[i][1];
      distance = dx * dx + dy * dy;
      printf("(%d, %d) is %d units^2 from %s.\n", x, y, distance, get_name(i));
    }
  }
}

/*! \brief Check if a astronaut was found.
 *
 * Loop through the astronauts and check if one was found.
 *
 * This function will also decrement the number of astronauts.
 *
 * \param mw pointer to the astronauts structure
 * \param x x-position to search
 * \param y y-position to search
 * \return -1 = no astronaut found, [0;NO_ASTRONAUTS-1] = astronaut found
 */
int8_t check_astronaut_find(Astronauts_t *mw, uint8_t x, uint8_t y) {
  uint8_t i;

  for(i = 0; i < NO_ASTRONAUTS; ++i) {
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
  Astronauts_t *mw;
  unsigned int turn;
  int x, y;
  int8_t i;

  mw = create_astronauts(10);
  if(mw == NULL) {
    abort();
  } else {
#ifdef DEBUG
    printf("mw = %p\n", mw);
    for(x = 0; x < NO_ASTRONAUTS; ++x) {
      printf("\t%d %d\n", mw->pos[x][0], mw->pos[x][1]);
    }
#endif
  }
  for(turn = 1; (mw->alive > 0) && (turn <= MAXTURNS); ++turn) {
    printf("\nTurn #%d -- You are at coordinates %d,%d.\nWhat is your guess?\n", turn, xcoor, ycoor);
    if(read_two_ints2(&x, &y) < 2) {
      puts("Please enter two numbers!");
      --turn;
      continue;
    }
    if((x-xcoor)*(x-xcoor) + (y-ycoor)*(y-ycoor) > 25) {
      puts("That is too far away!");
      --turn;
      continue;
    }
    xcoor = x;
    ycoor = y;
    i = check_astronaut_find(mw, x, y);
    if(i < 0) {
      puts("There was no astronaut there!");
    } else {
      printf("You have found astronaut %s.\n", get_name(i));
    }
    display_distances(x, y, mw);
    puts("");
  }
  if(mw->alive > 0) {
    puts("You lost! There are still astronauts\nto be rescued!");
  } else {
    puts("Congratulation! You found all astronauts.");
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

  xcoor = 5;
  ycoor = 5;
  do {
    game();
    puts("Do you like to play again (Y/N)?");
    cursor(1);
    ch = cgetc();
    cputc(ch);
    puts("\n");
    cursor(0);
  } while((ch != 'n') && (ch != 'N') && (ch != EOF));
}

int main(int argc, char **argv) {
  srand((PEEK(0xdc04) << 8) | PEEK(0xd012));
  puts("\fThe moon lander has crashed on the Moon,but fortunately, it appears that all\nfour crew members have survived. You arepart of the hastily assembled rescue\nmission tasked with saving the crew.\nFollowing a rough landing, your mission is to navigate the moon buggy to locate the stranded astronauts. The potential\nsearch area can be divided into a 10 by 10 grid.");
  puts("Equipped with an locator device that provides information on the crew members'\nproximity, you can employ the device up to 10 times before the astronauts' air\nsupply becomes critical. You are allowedto move up to 5 distance units between\neach trial.");
  puts("You get 10 tries. After each try, you\nwill be informed how far you are from\neach astronaut.\n");
  puts("\nPress any key to start game!");
  (void)cgetc();
  gameloop();
  return 0;
}
