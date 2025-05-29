#ifndef __THEATRE_H__
#define __THEATRE_H__
#include <stdint.h>

/* \file theatre.h
 *
 * A framework for producing theatre plays, visual novels and the
 * like. Each play will have acts which will define backgrounds,
 * sprites, data, etc. Each act consists of several scenes, and each
 * scene consists of several pictures.
 *
 * Initialisation of the theatre is done using the theatre_init()
 * function. After initialisation the interrupt is set to rasterline
 * 250 and the screen is blanked.
 *
 * After calling scene_init() the display is activated, the
 * configuration (isr and inter-picture functions) is set and finally
 * the picture functions called.
 *
 * In each picture actions on the screen can be taken, sprites can be
 * moved and text screens displayed. Using the
 * teatre_switch_to_windowed() function a text window can be opened in
 * which text can be displayed.
 *
 * The THEATRE_???? constants define how the usual memory layout is
 * expected to be.
 */

#define THEATRE_TEXT (unsigned char*)0xC000
#define THEATRE_COLR (unsigned char*)0xC400
#define THEATRE_SPRT (unsigned char*)0xC800
#define THEATRE_ASPR (unsigned char*)0xD000
#define THEATRE_FONT (unsigned char*)0xD800
#define THEATRE_GPHX (unsigned char*)0xE000

#define THEATRE_WRAPPED_REUSELASTFRAMECOLOR 16

// Keep in sync with animation code or else...
struct SpriteAnimationEntry {
  uint8_t speed_current;
  uint8_t animation_speed;
  uint8_t animation_current;
  uint8_t animation_delta;
  uint8_t animation_min;
  uint8_t animation_max;
  uint8_t animation_flags;
};

struct SpriteAnimation {
  struct SpriteAnimationEntry spr_anim[8];
};

struct TheatreSceneConfiguration {
  void (*isr)(void); //!< Interrupt Service Routine to be called after the theatre routine, use NULL if no routine needed.
  void (*interpicture)(void); //!< This function is called after each picture, use NULL if no function should be called.
};

typedef int (*Theatre_control_before_char)(char c);

/*! frame counter for the theatre system
 *
 * This frame counter is incremented each frame in the interrupt
 * routine.
 */
extern volatile uint16_t theatre_frame_counter;

/*! universal zeropage pointer
 *
 * This pointer is used for various operations within the theatre
 * framework.
 */
extern uint8_t *theatre_universal_pointer;
#pragma zpsym ("theatre_universal_pointer");  /* is in the zeropage */

/*! Initialise the Theatre™.
 *
 * The play routine is called at the end of each interrupt, the memory
 * configuration is set to I/O and RAM.
 * 
 * \param muzak_init function to call at initialisation (music init)
 * \param muzak_play function to call at each interrupt (music play)
 * \param songnumber load A with this value before calling song initialisation
 *
 * This will set up the VIC, initialise the music, and hook the interrupt.
 */
void __fastcall__ theatre_init(void *muzak_init, void *muzak_play, uint8_t songnumber);

#define THEATRE_COPY_CHARGEN 0x01
#define THEATRE_COPY_FONT0800 0x08
#define THEATRE_BITMAP_MODE 0x02
#define THEATRE_MULTICOL_MODE 0x04
#define THEATRE_ACT_BLANK 0x10

/*! Initialise an act.
 *
 * Each act has a predefined sprite animation, background and
 * foreground color, and crunched data to be uncompressed to $c000 and
 * higher.
 *
 * \param flags additional actions flags
 * \param sprianim pointer to sprite animation structure or NULL
 * \param background background colour
 * \param border border colour
 * \param cruncheddata pointer to crunched data or NULL
 */
void __fastcall__ act_init(unsigned char flags, struct SpriteAnimation *sprianim, unsigned char background, unsigned char border, char *cruncheddata);

/*! Initialise a scence.
 *
 * Each scene uses the data provided by the act. The pictures are just
 * functions to be called so that actions can be performed in
 * order. After the last picture a NULL pointer is needed to signalise
 * that all pictures have been handled.
 *
 * \param confptr pointer to some configuration for this scene
 * \param pictures pointer to picture functions (until NULL), variable number of parameters
 */
void __cdecl__ scene_init(const struct TheatreSceneConfiguration *confptr, void (*pictures)(void), ...);

/*! Function to wait a single frame.
 *
 */
void theatre_wait_frame(void);

/*! Function to wait a multiple frames.
 *
 * \param number number of frames to wait
 */
void theatre_wait_frames(int number);

/*! Function to wait until a specific frame.
 *
 * If the program is delayed and the specified frame is already passed
 * the function will return immediately.
 *
 * \param number wait until number frame is reached
 */
void theatre_until_frame(uint16_t number);

/*! Set the sprite at a position
 *
 * All positions must be multiplied by 64.
 */
void __fastcall__ theatre_sprite_pos64(uint16_t x, uint16_t y, unsigned char sprite);

void __fastcall__ theatre_copy_text2spare(void);
void __fastcall__ theatre_copy_spare2text(void);

/*! copy a PETSCII frame to the text area
 *
 * Background/frame color and color RAM is copied. During the copy
 * process the memoryconfig is set to I/O and RAM only meaning that
 * data beneath the KERNAL can be copied, too.
 *
 * \param frame pointer to the PETSCII frame
 */
void __fastcall__ theatre_copy_frame2text(unsigned char *frame);

/*! copy a compressed PETSCII frame
 *
 * The frame has to be compressed with qadz and will be decompressed
 * to a staging area. It can then be copied to the screen via
 * theatre_copy_frame2text().
 *
 * During decompression the memoryconfig is set to RAM only.
 *
 * \warning The \ref theatre_universal_pointer has to be set to the
 * location of the compressed frame data. First pointers to the actual
 * compressed frame are expected and after the last frame must be
 * followed by a NULL pointer.
 * 
 * \param framestaging the frame is decompressed into this staging area
 * \param frameno number of current frame (< 128)
 * \return pointer to the compressed frame data or NULL if last frame
 */
unsigned char*__fastcall__ theatre_copy_compressed_frame(unsigned char *framestaging, unsigned char frameno);

/*! swith to the default text screen
 */
void theatre_switch_to_text(void);

/*! switched to windowed display mode
 *
 * Enable the windowed output, draw the frame, and output the string.
 *
 * \param x1 top left cursor position
 * \param y1 top left cursor position
 * \param x2 bottom right cursor position
 * \param y2 bottom right cursor position
 * \param fc foreground color, if <0 the no frame is drawn, if >=16 then the last color is reused
 * \param text pointer to the string for output
 * \param funptr function to be called before the output of each character, the delay is applied after the character has been print
 */
void theatre_switch_to_windowed(uint8_t x1, uint8_t y1, uint8_t x2, uint8_t y2, int8_t fc, const char *text, Theatre_control_before_char funptr);

/*! output a string in word wrapped mode
 *
 * \param text pointer to the string for output
 * \param funptr function to be called before the output of each character, the delay is applied after the character has been print
 */
void theatre_output_wrapped(const char *text, Theatre_control_before_char funptr);

void enable_chrout2window(void);
void disable_chrout2window(void);
void draw_frame_sr(void);

extern uint8_t window_x1;
extern uint8_t window_y1;
extern uint8_t window_x2;
extern uint8_t window_y2;
extern uint8_t frame_color;

#endif
