; -*- mode: asm -*-

;;; This skips the header of the SpritePad data.
.define SPRITEPADDATAOFFSET     9

;;; Copies the sprite data into a destination buffer.
;;; Input: sprdataptr=pointer to the spridepad data
;;;	   num=which sprite number in the spritepad data
;;;	   destination=destination buffer
;;; Output: -
;;; Modifies: A, X
.macro copySPadData sprdataptr, num, destination
        .local  @loop
        ldx     #63-1
@loop:  lda     sprdataptr+SPRITEPADDATAOFFSET+64*num,x
        sta     destination,x
        dex
        bpl     @loop
.endmacro

;;; Get the colour of a sprite.
;;; Input: sprdataptr=pointer to the spridepad data
;;;        num=which sprite number in the spritepad data
;;; Output: A=colour of the sprite
;;; Modifies: A
.macro  getSPadColour   sprdataptr, num
        lda     sprdataptr+SPRITEPADDATAOFFSET+64*num+63
.endmacro

;;; Get MC colour 1 from spritepad data.
;;; Input: sprdataptr=pointer to the spridepad data
;;; Output: A=Sprite Multi Colour 1
;;; Modifies: A
.macro  getSPadMCColour1   sprdataptr
        lda     sprdataptr+7
.endmacro

;;; Get MC colour 2 from spritepad data.
;;; Input: sprdataptr=pointer to the spridepad data
;;; Output: A=Sprite Multi Colour 2
;;; Modifies: A
.macro  getSPadMCColour2   sprdataptr
        lda     sprdataptr+8
.endmacro
