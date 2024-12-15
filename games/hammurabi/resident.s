	.include	"t7d/pseudo/loadstorereg.i"
	.include	"t7d/compression/zx02.i"
	.include	"dload-labels.inc"
	.include	"globals.i"

	.importzp	ptr1, ptr2, ptr3
	.import	__LOADADDR__

	LOADERRESIDENT := LdLoc

	jmp	load_the_game
	jmp	zx02decrunch
load_the_game:
	.macro	LoadOrFail name
	 .local end_of_data
	 Load16	x,y,#.ident(name) ; Filename.
	 lda	#end_of_data-.ident(name) ; Length of filename.
	 jsr	loadorfail
	 .pushseg
	 .rodata
.ident(name):
	 .byte	name
end_of_data:
	 .popseg
	.endmacro
	LoadOrFail	"title"
	LoadOrFail	"font"
	LoadOrFail	"documentation"
	LoadOrFail	"main"
	lda	#3
	sta	$d011
	sta	$d020
	jmp	64738

	.code
.proc	loadorfail
	pha			; Save length of filename.
	lda	#$80		; Destination Address to load to.
	sta	LdZp_PresetAE
	lda	#<COMPPARTLOAD
	sta	LdLAE
	lda	#>COMPPARTLOAD
	sta	LdLAE+1
	pla			; Get length of filename.
	jsr	LOADERRESIDENT
	bcc	okload
@loop:	inc	$d020
	dec	$d020
	jmp	@loop
okload:
	jsr	zx02decrunch_ip
	.word	COMPPARTLOAD
	.word	PARTSTART
	jmp	PARTSTART
.endproc

	.data
	.word	RESIDENTADDR
	.word	__LOADADDR__
