#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>
#include <peekpoke.h>
#include <conio.h>

#include "mugwump-petscii-work.c"
#include <string.h>

void copy_assets(void);
void init_assets(void);
void init_music_1(void);

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
  uint8_t dead[NO_MUGWUMPS]; //!< alive status of all mugwumps
  int guess[MAXTURNS][2]; //!< x-y position of guesses
  uint8_t alive; //!< number of mugwumps alive
} Mugwumps_t;

Mugwumps_t *g_mw;
uint8_t g_current_turn;
uint8_t g_current_guess;  // displayed turn, when using +/-

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
      gotoxy(0,7+i);
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
  g_mw = mw;
  return mw;
}

void copyPart(unsigned char* img, unsigned char x, unsigned char y, unsigned char w, unsigned char h, unsigned char destX, unsigned char destY)
{
	uint16_t i;
	for (i=0; i<h; i++)
	{
		memcpy((void *)(0x400 +(i+destY)*40+destX),&img[2+     (i+y)*40+x],w);
		memcpy((void *)(0xd800+(i+destY)*40+destX),&img[2+1000+(i+y)*40+x],w);
	}
}

void show_mugwump(uint8_t no){
  switch (no)
  {
    case 0:
      copyPart(frame0002, 0, 0, 9, 6, 3, 17); //links
      break;
    case 1:
      copyPart(frame0002, 0, 6, 9, 6, 14,19); //unten
      break;
    case 2:
      copyPart(frame0002, 9, 6, 9, 6, 20,15); //rechts
      break;
    case 3:
      copyPart(frame0002, 9, 0, 9, 6, 10,13); //oben
      break;
  }
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

  gotoxy(0,9);

  for(i = 0; i < NO_MUGWUMPS; ++i) {
    if(!mw->dead[i]) {
      int dx = x - mw->pos[i][0];
      int dy = y - mw->pos[i][1];
      distance = dx * dx + dy * dy;
      printf("dist mungwup %d: %d units^2  \n", i+1, distance);
    }
    else
      printf("                            \n");
  }
  printf("from: %d,%d", x, y);
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
    if(!mw->dead[i]) {
      if((mx == x) && (my == y)) {
        mw->dead[i] = 1;
        show_mugwump(4-mw->alive);
        --mw->alive;
        return i;
      }
    }
  }
  return -1;
}

uint8_t read_numb(uint8_t from, uint8_t to, uint8_t x, uint8_t y)
{
  unsigned char c;
  gotoxy(x,y);
  cursor(1);
  do 
  {
    c = cgetc();
    if (c == '+' || c == '-')
    {
      if (!g_current_guess && g_current_turn>1)
      {
        g_current_guess = g_current_turn-1;
      }
      if (g_current_guess)
      {
        if (c == '-' && g_current_guess>1)
        {
          g_current_guess--;
        }
        if (c == '+' && g_current_guess<g_current_turn+1)
        {
          g_current_guess++;
        }
        if (g_current_guess<g_current_turn)
        {
          //display_distances(g_current_guess,g_current_guess,g_mw);
          display_distances(g_mw->guess[g_current_guess-1][0],g_mw->guess[g_current_guess-1][1],g_mw);
          gotoxy(8,5);
          printf(" [%d]", g_current_guess);
          gotoxy(x,y);
        }
      }
    }
  } while (c<from+48 || c>to+48);
  cputc(c);
  cursor(0);
  return c-48;  
}

int read_two_ints2(int *x, int *y) {
  gotoxy(7,7);
  *x = read_numb(0,9, 7,7);
  cputc(' ');
  gotoxy(9,7);
  *y = read_numb(0,9, 9,7);
  puts("");
  return 2;
}

void cleanOutputWindow(void)
{
  uint8_t i;
  for (i=0; i<8; i++)
  {
    memset((void*)(0x400+7*40+i*40), 32, 29);
  }
}

void cleanOutputWindow2(void)
{
  uint8_t i;
  for (i=0; i<6; i++)
  {
    memset((void*)(0x400+7*40+i*40), 32, 29);
  }
}

void showImg(unsigned char* img)
{
  *(char *)0xd020=img[0];
  *(char *)0xd021=img[1];
  //clrscr();
  memcpy((void *)0x400,&img[2],1000);
  memcpy((void *)0xd800,&img[2+1000],1000);
}

/*! \brief Single game run
 */
void game(void) {
  Mugwumps_t *mw;
  unsigned int turn;
  int x, y, delay;
  int8_t i;

  showImg(frame0001); // start with a 'clean' screen
  mw = create_mugwumps(10);
  if(mw == NULL) {
    abort();
  } else {
#ifdef DEBUG
    printf("mw = %p\n", mw);
    for(x = 0; x < NO_MUGWUMPS; ++x) {
      gotoxy(0,11+x);
      printf("\t%d %d\n", mw->pos[x][0], mw->pos[x][1]);
    }
    cgetc();
#endif
  }
  g_current_guess = 0;
  for(turn = 1; (mw->alive > 0) && (turn <= MAXTURNS); ++turn) {
    g_current_turn = turn;
    if (turn==1)
      cleanOutputWindow();
    revers(1);
    memset((void*)0x4A0, 0x64, 7);
    gotoxy(0,5);
    printf("turn #%2d", turn);
    revers(0);
    cputsxy(0,7, "guess: _ _");

    if(read_two_ints2(&x, &y) < 2) {
      puts("Please enter two numbers!");
      --turn;
      continue;
    }
    // animation here
    cleanOutputWindow();
    // hammer down
    // z4-15 s19-32
    copyPart(frame0000, 19, 4, 14, 12, 19, 4);
    for(delay=0; delay<5000; delay++); 
    // hammer up
    copyPart(frame0001, 19, 4, 14, 12, 19, 4);
    for(delay=0; delay<500; delay++); 
    // animation end
    i = check_mugwump_find(mw, x, y);
    mw->guess[turn-1][0] = x;
    mw->guess[turn-1][1] = y;

    if(i < 0) {
      puts("there was no mugwump there!");
    } else {
      printf("you have found mugwump %d.  \n", (int) i+1);
    }
    display_distances(x, y, mw);
    gotoxy(9,13);
    printf(" [%d]", turn);
    puts("");
  }
  cgetc();
  if(mw->alive > 0) {
    cleanOutputWindow();
    gotoxy(0,8);
    puts("you lost!\n");
    puts("there are still mugwumps\nhiding out!\n");
  } else {
    cleanOutputWindow();
    gotoxy(0,8);
    puts("congratulation!\n");
    puts("you found all mugwumps.\n");
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
    puts("do you like to play again?");
    gotoxy(27,13);
    if (!g_mw->alive)
      gotoxy(27,12);
    cursor(1);
    ch = cgetc();
    cputc(ch);
    puts("\n");
    cursor(0);
  } while((ch != 'n') && (ch != 'N') && (ch != 0x00));
}

int main() {
  putchar(8); //Disable changing of charset.
  copy_assets();
  init_assets();
  srand((PEEK(0xdc04) << 8) | PEEK(0xd012));  srand((PEEK(0xdc04) << 8) | PEEK(0xd012));
  showImg(frame0001);
  // //move logo up
  // memcpy((void*)0x400, (void*)0x428, 160);
  // clear line bellow logo
  // memset((void*)0x4A0, 32, 40);
       // memcpy((void *)0x400, & frame0001[2+40],160);
       // memcpy((void *)0xd800, &frame0001[2+1040],160);
//cputsxy(0, 8,"----x----x----x----x----x---|");
  cleanOutputWindow();
  cputsxy(0, 8,"the objective of this game   ");
  cputsxy(0, 9,"is to find four mugwumps     ");
  cputsxy(0,10,"hidden on a 10 times 10 grid.");
  cputsxy(0,11,"the homebase is at position  ");
  cputsxy(0,12,"(0,0).");
  cputsxy(0,13,"                <press space>");
  cgetc();
  cputsxy(0, 8,"you get 10 tries.            ");
  cputsxy(0, 9,"after each try, you will be  ");
  cputsxy(0,10,"informed how far you are from");
  cputsxy(0,11,"each mugwump.                ");
  cputsxy(0,12,"      ");
#ifdef DEBUG
  show_mugwump(0);
  show_mugwump(1);
  show_mugwump(2);
  cgetc();
  show_mugwump(3);
  cgetc();
  showImg(frame0001);
#endif
  cgetc();
  cleanOutputWindow();
  init_music_1();
  gameloop();
  asm("jmp 64738");
  return 0;
}
