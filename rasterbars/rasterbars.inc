;parameters to set before including this file
; current_rasterline .set nnn
; delay_lines        .set nnn  ;this will be 0 in most cases

calc_linecycles current_rasterline

;line 0: pre-load A and X, set $d018 at the end
	lda #col_1       ; 2
	ldy front_back   ; 4
	delay_cycles	 lc-6-4-7-3

current_rasterline .set current_rasterline+1

busy_wait current_rasterline,current_rasterline+delay_lines
current_rasterline .set current_rasterline+delay_lines

	sty $d018

calc_linecycles current_rasterline

;line 1
	sta $d020	; 4
	sta $d021	; 4
	lda #col_2	; 2
	delay_cycles     lc-10

current_rasterline .set current_rasterline+1
calc_linecycles current_rasterline

;line 2
	sta $d020	; 4
	sta $d021	; 4
	lda #col_3	; 2
	delay_cycles     lc-10

current_rasterline .set current_rasterline+1
calc_linecycles current_rasterline
	
;line 3
	sta $d020	; 4
	sta $d021	; 4
	lda #col_4	; 2
	delay_cycles     lc-10

current_rasterline .set current_rasterline+1
calc_linecycles current_rasterline

;line 4
	sta $d020	; 4
	sta $d021	; 4
	lda #col_5	; 2
	delay_cycles     lc-10

current_rasterline .set current_rasterline+1
calc_linecycles current_rasterline

;line 5
	sta $d020	; 4
	sta $d021	; 4
	lda #col_6	; 2
	delay_cycles     lc-10

current_rasterline .set current_rasterline+1
calc_linecycles current_rasterline

;line 6
	sta $d020	; 4
	sta $d021	; 4
	lda #col_7	; 2
	delay_cycles     lc-10

current_rasterline .set current_rasterline+1
calc_linecycles current_rasterline

;line 7
	sta $d020	; 4
	sta $d021	; 4
	lda #col_8	; 2
	delay_cycles     lc-10

current_rasterline .set current_rasterline+1
calc_linecycles current_rasterline

;line 8
	sta $d020	; 4
	sta $d021	; 4
	lda #col_9	; 2
	delay_cycles     lc-10

current_rasterline .set current_rasterline+1
calc_linecycles current_rasterline

;line 9
	sta $d020	; 4
	sta $d021	; 4
	lda #col_10	; 2
	delay_cycles     lc-10

current_rasterline .set current_rasterline+1
calc_linecycles current_rasterline

;line 10
	sta $d020	; 4
	sta $d021	; 4
	lda #col_11	; 2
	delay_cycles     lc-10

current_rasterline .set current_rasterline+1
calc_linecycles current_rasterline

;line 11
	sta $d020	; 4
	sta $d021	; 4
	lda #col_12	; 2
	delay_cycles     lc-10

current_rasterline .set current_rasterline+1
calc_linecycles current_rasterline

;line 12
	sta $d020	; 4
	sta $d021	; 4
	lda #col_13	; 2
	delay_cycles     lc-10

current_rasterline .set current_rasterline+1
calc_linecycles current_rasterline

;line 13
	sta $d020	; 4
	sta $d021	; 4
	lda #col_14	; 2
	delay_cycles     lc-10

current_rasterline .set current_rasterline+1
calc_linecycles current_rasterline

;line 14
	sta $d020	; 4
	sta $d021	; 4
	lda #col_15	; 2
	delay_cycles     lc-10

current_rasterline .set current_rasterline+1
calc_linecycles current_rasterline

;last line
	sta $d020	 ; 4
	sta $d021	 ; 4
	lda #col_border  ; 2
	ldy #chars_shown ; 2
	delay_cycles      lc-16
	sty $d018        ; 4
	sta $d020	 ; 4
	sta $d021	 ; 4
