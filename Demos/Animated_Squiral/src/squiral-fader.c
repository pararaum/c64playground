
#include <peekpoke.h>

#define SCREEN_RAM 0x0400

unsigned char round = 0;

void pause(unsigned char num)
{
    unsigned int i;
    for (i = 0; i < num; ++i) {
        // do nothing
    }
}

void myPoke(unsigned int address, unsigned char value, unsigned char waits)
{
    if (address < 1024 || address > 2024) {
        return; // prevent poking outside of screen memory
    }
    if (round == 1) {
        value += 128; // inverted
    } else if (round == 2) {
        value = 160;
    }
    POKE(address, value);
    pause(waits);
}

void drawSquiral(unsigned int a, unsigned char size)
{
    unsigned char d = 0;
    unsigned char i = 0;
    unsigned char j = 0;
    unsigned char frames = 22;

    myPoke(a, 81, frames); // "Q"
    for (j = 0; j < size; ++j) {

        frames = 22-j; // speed up the animation as it goes on

        //up
        for (i = 0; i < d; ++i) {
            a -= 40;
            myPoke(a, 66, frames); // "I"
        }
        a -= 40;
        myPoke(a, 112, frames); // lo

        //right
        for (i = 0; i < d; ++i) {
            a += 1;
            myPoke(a, 64, frames); // "-"
        }
        a += 1;
        myPoke(a, 110, frames);   // ro

        d++;

        //down
        for (i = 0; i < d; ++i) {
            a += 40;
            myPoke(a, 66, frames); // "I"
        }
        a += 40;
        myPoke(a, 125, frames);   // ru

        //left
        for (i = 0; i < d; ++i) {
            a -= 1;
            myPoke(a, 64, frames); // "-"
        }
        a -= 1;
        myPoke(a, 109, frames);   // lu

        d++;
    }
}

void main_squiral_fader(void)
{
    unsigned int a;

    for (round = 0; round < 3; ++round) {
        a = SCREEN_RAM + 12*40 + 19;// start in the middle of the screen
        drawSquiral(a, 20);
    }
}
