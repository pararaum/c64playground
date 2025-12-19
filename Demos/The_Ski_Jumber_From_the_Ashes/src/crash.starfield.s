	.include	"t7d/theatre/consts.i"
	.include	"zeropage.inc"
	.include	"LAMAlib-macros16.inc"
	.include	"t7d/memoryfunctions.i"
	.include	"globals.i"
	.macpack	longbranch

	.export	_starfield, _starfield_finished

	;; (/ 25 8)3 3 6 9 12 15 18 21 
	.define	STARADDRESSES	THEATRE_TEXT, THEATRE_TEXT+40*15, THEATRE_TEXT+40*3, THEATRE_TEXT+40*18, THEATRE_TEXT+40*6, THEATRE_TEXT+40*21, THEATRE_TEXT+40*9, THEATRE_TEXT+40*6
	;; (/ 40 8)5 0 5 10 15 20 25 30 35 40
	.define	STARYPOS	1, 15, 29, 6, 19, 37, 10, 25, 38
	STARCOUNT=8
	STARCHAR=Crash_starchar

	JUMPERWIDTH=20
	JUMPERHEIGHT=10
	JUMPERY=14
	JUMPERPOS=THEATRE_TEXT+JUMPERY*40

	SATELLITEWIDTH=13
	SATELLITEHEIGHT=4
	SATELLITEOFFSET=10*40
	SATELLITEPOS=THEATRE_TEXT+25
	SATELLITEPOS2=THEATRE_TEXT+2

	.rodata
petsciisprites:	.incbin	"temporary.jumper_in_orbit_crash.petscii.bin", 3*2002, 1*2002
	;; [int(20-9+10*math.cos(i/250*math.pi*2-math.pi)) for i in range(250)]
jumpersinusitis:	.byte	1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 7, 7, 7, 7, 8, 8, 8, 8, 9, 9, 9, 9, 10, 10, 10, 10, 11, 11, 11, 11, 12, 12, 12, 12, 13, 13, 13, 13, 14, 14, 14, 14, 15, 15, 15, 15, 15, 16, 16, 16, 16, 16, 17, 17, 17, 17, 17, 18, 18, 18, 18, 18, 18, 19, 19, 19, 19, 19, 19, 19, 19, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 19, 19, 19, 19, 19, 19, 19, 19, 18, 18, 18, 18, 18, 18, 17, 17, 17, 17, 17, 16, 16, 16, 16, 16, 15, 15, 15, 15, 15, 14, 14, 14, 14, 13, 13, 13, 13, 12, 12, 12, 12, 11, 11, 11, 11, 10, 10, 10, 10, 9, 9, 9, 9, 8, 8, 8, 8, 7, 7, 7, 7, 6, 6, 6, 6, 6, 5, 5, 5, 5, 5, 4, 4, 4, 4, 4, 3, 3, 3, 3, 3, 3, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1

	.data
_starfield_finished:	.byte	0 ; This is set to $FF if we have reached the final stage.
starcolours:	.byte	3
	STARCOLOURXOR=3^7
jumpersinidx:	.byte	0
	JUMPERSINCOUNT=250
satellitepos:	.word	SATELLITEPOS

	;; Address of the stars...
addresslo:	.lobytes	STARADDRESSES	
addresshi:	.hibytes	STARADDRESSES	
starypos:	.byte	STARYPOS

	.code
;;; Modifies: A
.proc	incptr1
	lda	ptr1
	clc
	adc	#40
	sta	ptr1
	bcc	noovl
	inc	ptr1+1
noovl:	lda	#4+>(THEATRE_TEXT)
	cmp	ptr1+1
	bne	out
	lda	#>THEATRE_TEXT
	sta	ptr1+1
out:	rts
.endproc

;;; Modifies: A
.proc	drawnincrement
	lda	(ptr1),y
	cmp	#STARCHAR
	bne	noremove
	lda	#' '
	sta	(ptr1),y
noremove:
	jsr	incptr1
	lda	(ptr1),y
	cmp	#' '
	bne	nodraw
	lda	#STARCHAR
	sta	(ptr1),y
	lda	ptr1+1
	eor	#(>THEATRE_TEXT)^$d8
	sta	ptr1+1
	lda	starcolours
	eor	#STARCOLOURXOR
	sta	starcolours
	sta	(ptr1),y
	lda	ptr1+1
	eor	#(>THEATRE_TEXT)^$d8
	sta	ptr1+1
nodraw:	rts
.endproc



;;; Function to copy memory with strides.
	.bss
memcpy_w_holestrided_srcwidth:	.res	1
memcpy_w_holestrided_srcheight:	.res	1
memcpy_w_holestrided_srcstride:	.res	1
memcpy_w_holestrided_dststride:	.res	1

	.zeropage
srctxt:	.res	2
dsttxt:	.res	2
srccol:	.res	2
dstcol:	.res	2

	.code
memcpy_w_holestrided:
	;; X=number of lines still to copy.
	ldx	memcpy_w_holestrided_srcheight
@l1:
	ldy	memcpy_w_holestrided_srcwidth
	dey			; Range is 0..memcpy_w_holestrided_srcwidth.
@smallloop:
	lda	(srctxt),y
	;cmp	#$20		; Is space?
	;beq	@nocopy
	sta	(dsttxt),y
	lda	(srccol),y
	sta	(dstcol),y
@nocopy:
	dey
	bpl	@smallloop
	.macro	incbig	aptr,incvar
	.local @skip
	lda	incvar
	clc
	adc	aptr
	sta	aptr
	bcc	@skip
	inc	aptr+1
	@skip:
	.endmacro
	incbig	srctxt,memcpy_w_holestrided_srcstride
	incbig	srccol,memcpy_w_holestrided_srcstride
	incbig	dsttxt,memcpy_w_holestrided_dststride
	incbig	dstcol,memcpy_w_holestrided_dststride
	dex
	bne	@l1
	rts


.proc	draw_jumper
	lda	#JUMPERWIDTH
	sta	memcpy_w_holestrided_srcwidth
	lda	#JUMPERHEIGHT
	sta	memcpy_w_holestrided_srcheight
	lda	#40
	sta	memcpy_w_holestrided_srcstride
	sta	memcpy_w_holestrided_dststride
	ldax	#petsciisprites+2
	stax	srctxt
	ldax	#petsciisprites+2+1000
	stax	srccol
	ldax	#JUMPERPOS
	stax	dsttxt
	ldx	jumpersinidx
	lda	jumpersinusitis,x
	clc
	adc	dsttxt
	sta	dsttxt
	bcc	skip
	inc	dsttxt+1
skip:	ldax	dsttxt
	addax	#-THEATRE_TEXT+$D800
	stax	dstcol
	jsr	memcpy_w_holestrided
	rts
.endproc


.proc	draw_satellite
	lda	#SATELLITEWIDTH
	sta	memcpy_w_holestrided_srcwidth
	lda	#SATELLITEHEIGHT
	sta	memcpy_w_holestrided_srcheight
	lda	#40
	sta	memcpy_w_holestrided_srcstride
	sta	memcpy_w_holestrided_dststride
	ldax	#petsciisprites+2+SATELLITEOFFSET
	stax	srctxt
	ldax	#petsciisprites+2+1000+SATELLITEOFFSET
	stax	srccol
	ldax	satellitepos
	stax	dsttxt
skip:	ldax	dsttxt
	addax	#-THEATRE_TEXT+$D800
	stax	dstcol
	jsr	memcpy_w_holestrided
	rts
.endproc


.proc	move_satellite
	inc16	satellitepos,40
	lda	satellitepos+1
	cmp	#>(THEATRE_TEXT)+4
	bne	out
	ldax	#SATELLITEPOS
	stax	satellitepos
	lda	#0
out:	rts
.endproc


.proc	move_satellite_14times
	lda	#13
	CNTR=*-1
	dec	CNTR
	beq	out
	jmp	move_satellite
out:	rts
.endproc

;;; Modifies: X
;;; Output: A=next index, Z=1 if next index is zero
.proc	nextjumpersinus
	ldx	jumpersinidx
	inx
	cpx	#JUMPERSINCOUNT
	bne	skip2
	ldx	#0
skip2:	stx	jumpersinidx
	txa
	rts
.endproc


.proc	starry_night
	ldx	#STARCOUNT-1
loop:	lda	addresslo,x
	sta	ptr1
	lda	addresshi,x
	sta	ptr1+1
	ldy	starypos,x
	jsr	drawnincrement
	lda	ptr1
	sta	addresslo,x
	lda	ptr1+1
	sta	addresshi,x
	dex
	bpl	loop
	rts
.endproc


.proc	wait0
	lda	#0
	rts
.endproc


.proc	wait20
	dec	cntr
	bne	out
	lda	#100
	sta	cntr
	lda	#0
out:	rts
	.pushseg
	.data
cntr:	.byte	20
	.popseg
.endproc


.proc	wait50
	dec	cntr
	bne	out
	lda	#100
	sta	cntr
	lda	#0
out:	rts
	.pushseg
	.data
cntr:	.byte	50
	.popseg
.endproc


.proc	wait100
	dec	cntr
	bne	out
	lda	#100
	sta	cntr
	lda	#0
out:	rts
	.pushseg
	.data
cntr:	.byte	100
	.popseg
.endproc


.macro	CallAndNext target, routine, final
	.local	@next
	jsr	routine
	jne	final
	lda	#<(@next)
	sta	target
	lda	#>(@next)
	sta	target+1
	jmp	final
	@next:
.endmacro


.proc	_starfield
	jsr	draw_jumper
	jmp	call
	JMPPTR=*-2
call:
	CallAndNext	JMPPTR, nextjumpersinus, endisr
	CallAndNext	JMPPTR, wait100, endisr
	CallAndNext	JMPPTR, nextjumpersinus, endisr
	jsr	draw_satellite
	CallAndNext	JMPPTR, move_satellite, endisr
	CallAndNext	JMPPTR, wait50, endisr
	CallAndNext	JMPPTR, nextjumpersinus, endisr
	jsr	draw_satellite
	CallAndNext	JMPPTR, move_satellite, endisr
	CallAndNext	JMPPTR, nextjumpersinus, endisr
	CallAndNext	JMPPTR, wait20, endisr
	ldax	#SATELLITEPOS2
	stax	satellitepos
	CallAndNext	JMPPTR, wait0, endisr
	jsr	draw_satellite
	CallAndNext	JMPPTR, move_satellite_14times, endisr
	jsr	draw_satellite
	lda	#$ff
	sta	_starfield_finished
	lda	#$60		; RTS
	sta	_starfield
endisr:
	jsr	starry_night
	rts
.endproc
