#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <conio.h>
#include <peekpoke.h>
#include <stdint.h>

#define MAXCOLUMN 39

enum Levels {
  LvlEnd, // We use this to mark the end of the game.
  LvlEasy,
  LvlNormal,
  LvlHard
};

int year; //!< current year
long starvation_percentage_p1; //!< added starvation percentage for evaulating success
long starvation_dead_d1; //!< added death toll for evaulating success
long people; //!< current number of people
long bushels; //!< current number of bushels in stock
long land_price; //!< land price in bushels/acre
long acres; //!< currently owned land [acres]
int people_starved; //!< people starved last turn
int babies; //!< people burned last turn
long yield; //!< bushels per acre last turn
long planted; //!< number of acres planted
long rats_share; //!< people ate that much last turn
long fed_to_people; //!< bushels fed to people
long acres_per_person; //!< acres per person as a measure of success
long people_per_acre; //!< number of people needed per acre for seeding

int level; //!< current game level

long bushels_per_acres; //!< in order to feed all people
long ratspercentage; //!< percentage of a rats plague
int minimumnewpeople; //!< minimum number of people per turn
long horrorpercentage; //!< probability of a plague

static long rand1to(long to) {
  return rand() * to / RAND_MAX + 1;
}

static long rand1to5(void) {
  return rand1to(5);
}

static long rand1to100(void) {
  return rand1to(100);
}

void init(enum Levels level) {
  year = 0;
  starvation_percentage_p1 = 0;
  starvation_dead_d1 = 0;
  people = 95;
  bushels = 3000;
  land_price = 3;
  acres = bushels/land_price;
  people_starved = 0;
  babies = 5;
  yield = 3;
  planted = 0;
  rats_share = 200;
  switch(level) {
  case LvlEnd:
    abort();
  case LvlEasy:
    ratspercentage = 14;
    minimumnewpeople = 2;
    horrorpercentage = 10;
    bushels_per_acres = 20; // from the original game
    people_per_acre = 10; // from the original game
    break;
  case LvlNormal:
    ratspercentage = 20;
    minimumnewpeople = 1;
    horrorpercentage = 15;
    bushels_per_acres = 18 + rand1to5(); // a little harder as it is random
    people_per_acre = 8 + rand1to5();
    break;
  case LvlHard:
    ratspercentage = 24;
    minimumnewpeople = 1;
    horrorpercentage = 18;
    bushels_per_acres = 20; // fix again but changes every time, see calc_new_people()
    people_per_acre = 10;
    break;
  }
}

static void gio_word_wrap_string(const char *s) {
  size_t l;

  if(s != NULL) {
    l = strlen(s);
    if(wherex() + l + 1 >= MAXCOLUMN) { // We need wrapping.
      putchar('\n');
    }
    printf("%s", s); // Just output.
  }
}

static void gio_putstr(const char *s) {
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

static void hammurabi_printf(const char *format, ...) {
  char buf[121];
  va_list ap;

  if(format[0] == ' ') {
    putchar(' ');
  }
  va_start(ap, format);
  vsprintf(buf, format, ap);
  gio_putstr(buf);
  va_end(ap);
}

static void hammurabi_puts(const char *s) {
  gio_putstr(s);
  putchar('\n');
}

static bool rand_check_percent(long per) {
  return rand1to(100) < per;
}

static long read_long(const char *msg) {
  long l;
  char buf[81];

  for(;;) {
    hammurabi_puts(msg);
    if(scanf("%ld", &l) == 1) {
      return l;
    }
    scanf("%s", buf);
    printf("I do not know what to do with '%s'!\n", buf);
  }
  return l;
}

static void  buy_land() {
  long ltb; // land to buy
  do {
    ltb = read_long("How many acres do you wish to buy?");
    if(ltb < 0) {
      hammurabi_puts("We are buying, not selling!");
    } else if(ltb * land_price > bushels) {
      hammurabi_printf("You only have %ld bushels!\n", bushels);
    } else {
      acres += ltb;
      bushels -= ltb * land_price;
      break;
    }
  } while(true);
}

void sell_land(void) {
  long lts; // land to sell
  do {
    lts = read_long("How many acres do you wish to sell?");
    if(lts < 0) {
      hammurabi_puts("We are selling, not buying!");
    } else if(lts > acres) {
      hammurabi_printf("You only have %ld acres!\n", acres);
    } else {
      acres -= lts;
      bushels += lts * land_price;
      break;
    }
  } while(true);
}

long feed_people(void) {
  long l;

  do {
    l = read_long("How many bushels do you wish to feed to your people?");
    if(l < 0) {
      hammurabi_puts("Negative feeding?");
    } else if(l > bushels) {
      hammurabi_printf("You only have %ld bushels.\n", bushels);
    } else {
      return l;
    }
  } while(true);
}

void seed_acre(void) {
  long l;
  long seeds_per_acre_times2;
  long people_per_acre_times4 = people_per_acre;
  switch(level) {
  case LvlEasy:
    seeds_per_acre_times2 = 2 * 2;
    people_per_acre_times4 *= 4;
    break;
  case LvlNormal:
    seeds_per_acre_times2 = 2 * 2;
    people_per_acre_times4 *= 4;
    break;
  case LvlHard:
    seeds_per_acre_times2 = rand1to5() + 1;
    people_per_acre_times4 *= /*40*/ 40 - 11 + rand1to100() / 4;
    break;
  default:
    abort();
  };
  do {
    l = read_long("How many acres do you wish to plant with seed?");
    if(l < 0) {
      hammurabi_puts("Negative seeds?");
      /*l / 2  > bushels*/
    } else if(l >  bushels * seeds_per_acre_times2 / 2) {
      hammurabi_printf("Hammurabi, think again! You only have %ld bushels.\n", bushels);
      /*l / 10 > people*/
    } else if(l > people * people_per_acre_times4 / 4) {
      hammurabi_printf("But you only have %ld people to tend the fields!\n", people);
    } else if(l > acres) {
      hammurabi_printf("Hammurabi, you only own %ld acres!\n", acres);
    } else {
      planted = l;
      return;
    }
  } while(true);
}

void horror(void) {
  if(rand_check_percent(horrorpercentage)) {
    hammurabi_puts("\nA horrible plague struck! Half of the people died.");
    people /= 2;
  }
}

long calc_land_price(void) {
  long ret = (27L - 17) * rand() / RAND_MAX + 17;
  switch(level) {
  case LvlEasy:
  case LvlNormal:
    break;
  case LvlHard:
    ret = rand() * (28 + rand1to5() - 15) / RAND_MAX + 15;
    break;
  default:
    abort();
  }
  return ret;
}

int calc_new_people(void) {
  long bpa = bushels_per_acres;
  if(level == LvlHard) {
    bpa += rand1to(8) - 4; // Changes bushels need every(!) turn!
  }
  if(people_starved == 0) {
    babies = rand() * (bpa * acres + bushels) / RAND_MAX / people / 100 + minimumnewpeople;
  } else {
    babies = 0;
  }
  return babies;
}

void calculate_harvest(void) {
  bushels -= planted / 2;
  yield = rand1to5();
}

void rats_are_running_wild(void) {
  /*
   * March on, straight to hell, with the signs of death in hand!
   */
  if(rand_check_percent(ratspercentage)) {
    // Rats!
    rats_share = bushels / rand1to5();
  } else {
    rats_share = 0;
  }
}


void play_the_game(void) {
  hammurabi_puts("Hammurabi\n=========\n");
  for(year = 1; year <= 10; ++year) {
    hammurabi_printf("Hammurabi:  I beg to report to you, in year %d,", year);
    if(people_starved == 0) {
      hammurabi_printf(" no people starved and");
    } else if(people_starved == 1) {
      hammurabi_printf(" one person starved and");
    } else {
      hammurabi_printf(" %d people starved and", people_starved);
    }
    if(babies == 0) {
      hammurabi_printf(" none came to the city.");
    } else if(babies == 1) {
      hammurabi_printf(" one came to the city.");
    } else {
      hammurabi_printf(" %d came to the city.", babies);
    }
    people += babies;
    calc_new_people();
    if(year > 1) {
      horror();
    }
    hammurabi_printf(" So the population is now %ld.\n", people);
    hammurabi_printf("The city now owns %ld acres.\n", acres);
    if(yield == 0) {
      hammurabi_puts("The harvest did not yield any grain.");
    } else if(yield == 1) {
      hammurabi_puts("You harvested one bushel per acre.");
    } else {
      hammurabi_printf("You harvested %ld bushels per acre.\n", yield);
    }
    if(rats_share == 0) {
      hammurabi_puts("The rats did not eat any grain.");
    } else {
      hammurabi_printf("The rats ate %ld bushels.\n", rats_share);
    }
    hammurabi_printf("You now have %ld bushels in store.\n", bushels);
    land_price = calc_land_price();
    hammurabi_printf("Land is trading at %ld bushels per acre.\n", land_price);
    buy_land();
    sell_land();
    fed_to_people = feed_people();
    bushels -= fed_to_people;
    seed_acre();
    calculate_harvest();
    rats_are_running_wild();
    bushels += yield * planted;
    bushels -= rats_share;
    people_starved = 0;
    if(people >= fed_to_people/20) {
      people_starved = people - fed_to_people/20;
      if(people_starved < 45L * people / 100) {
	people -= people_starved;
      } else {
	hammurabi_printf("You starved %d people in one year! Due to this extreme mismanagement you have not only been impeached and thrown out of office but a mob is trying to kill you.\n", people_starved);
	break;
      }
      starvation_percentage_p1 = ((year - 1) * starvation_percentage_p1 + people_starved * 100 / people) / year;
      starvation_dead_d1 += people_starved;
    }
  }
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if(year >= 11) {
    hammurabi_printf("In your 10-year term of office, %ld percent of the people starved per year on the average, i.e. a total of %ld people died.\n", starvation_percentage_p1, starvation_dead_d1);
    acres_per_person = acres / people;
    hammurabi_printf("You started with 10 acres per person and ended with %ld acres per person.\n", acres_per_person);
    if((starvation_percentage_p1 > 33) || (acres_per_person < 7)) {
      hammurabi_puts("A very bad performance, a revolution is brewing!");
    } else if((starvation_percentage_p1 > 10) || (acres_per_person < 7)) {
      hammurabi_puts("Your heavy-handed performance smacks of Nero and Ivan IV. The remaining people find you an unpleasant ruler, and, frankly, hate your guts!");
    }  else if((starvation_percentage_p1 > 10) || (acres_per_person < 7)) {
      hammurabi_printf("Your performance could have been somewhat better, but really wasn't too bad at all. %ld people would dearly like to see you assassinated but we all have our trivial problems.\n", rand() * 4L * people / 5);
    } else {
      hammurabi_puts("A fantastic performance! Charlemagne, Disraeli, and Jefferson combined could not have done better.");
    }
  }
}


#include "hammurabi-petscii.c"
enum Levels titlescreen(void) {
  char c;
  uint8_t oldD018;
  enum Levels ret = LvlEnd;
  uint8_t running = 1;

  oldD018 = PEEK(0xd018); // Save the old charset selection.
  POKE(0xd018, (oldD018 & 0xf0) | (3 << 1));
  POKE(0xd020, frame0000[0]);
  POKE(0xd021, frame0000[1]);
  memcpy((char*)0xcc00, &frame0000[2], 1000);
  memcpy((char*)0xd800, frame0000+1002, 1000);
  for(; running == 1;) {
    c = cgetc();
    switch(c) {
    case 0x85:
      ret = LvlEasy;
      running = 0;
      break;
    case 0x86:
      ret = LvlNormal;
      running = 0;
      break;
    case 0x87:
      ret = LvlHard;
      running = 0;
      break;
    case 0x88:
      ret = LvlEnd;
      running = 0;
      //    default:
    }
  }
  POKE(0xd018, oldD018);
  POKE(0xd020, 1);
  POKE(0xd021, 1);
  return ret;
}

int main(int, char **) {
  srand((PEEK(0xdc04) << 8) | PEEK(0xd012));
  for(;;) {
    level = titlescreen();
    if(level == 0) {
      break;
    }
    init(level);
    putchar('\f');
    play_the_game();
    puts("\nPress any key to go back to the menu!");
    (void)cgetc();
  }

  return 0;
}
