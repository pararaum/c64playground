cycle0:
lda #14
ldx #6
do
  cpy $d012
loop while pl
sta $d020
stx $d021
stx $d022
stx $d023
stx $d024
iny  ;increase Y to go to next rasterline
lda #14
ldx #6
do
  cpy $d012
loop while pl
sta $d020
stx $d021
stx $d022
stx $d023
lda #1
sta $d024
iny  ;increase Y to go to next rasterline
lda #14
ldx #6
do
  cpy $d012
loop while pl
sta $d020
stx $d021
stx $d022
lda #1
sta $d023
lda #3
sta $d024
iny  ;increase Y to go to next rasterline
lda #14
ldx #6
do
  cpy $d012
loop while pl
sta $d020
stx $d021
lda #1
sta $d022
lda #3
sta $d023
lda #14
sta $d024
iny  ;increase Y to go to next rasterline
lda #1
ldx #3
do
  cpy $d012
loop while pl
sta $d020
sta $d021
stx $d022
lda #14
sta $d023
lda #6
sta $d024
iny  ;increase Y to go to next rasterline
lda #3
ldx #14
do
  cpy $d012
loop while pl
sta $d020
sta $d021
stx $d022
lda #6
sta $d023
sta $d024
iny  ;increase Y to go to next rasterline
lda #14
ldx #6
do
  cpy $d012
loop while pl
sta $d020
sta $d021
stx $d022
stx $d023
stx $d024
iny  ;increase Y to go to next rasterline
lda #6
do
  cpy $d012
loop while pl
sta $d020
sta $d021
sta $d022
sta $d023
sta $d024
rts


cycle1:
lda #14
ldx #6
do
  cpy $d012
loop while pl
sta $d020
stx $d021
stx $d022
stx $d023
stx $d024
iny  ;increase Y to go to next rasterline
lda #14
ldx #6
do
  cpy $d012
loop while pl
sta $d020
stx $d021
stx $d022
lda #1
sta $d023
sta $d024
iny  ;increase Y to go to next rasterline
lda #14
ldx #6
do
  cpy $d012
loop while pl
sta $d020
stx $d021
lda #1
sta $d022
lda #3
sta $d023
sta $d024
iny  ;increase Y to go to next rasterline
lda #1
ldx #3
do
  cpy $d012
loop while pl
sta $d020
sta $d021
stx $d022
lda #14
sta $d023
sta $d024
iny  ;increase Y to go to next rasterline
lda #3
ldx #14
do
  cpy $d012
loop while pl
sta $d020
sta $d021
stx $d022
lda #6
sta $d023
sta $d024
iny  ;increase Y to go to next rasterline
lda #14
ldx #6
do
  cpy $d012
loop while pl
sta $d020
sta $d021
stx $d022
stx $d023
stx $d024
iny  ;increase Y to go to next rasterline
lda #6
do
  cpy $d012
loop while pl
sta $d020
sta $d021
sta $d022
sta $d023
sta $d024
iny  ;increase Y to go to next rasterline
lda #6
do
  cpy $d012
loop while pl
sta $d020
sta $d021
sta $d022
sta $d023
sta $d024
rts


cycle2:
lda #14
ldx #6
do
  cpy $d012
loop while pl
sta $d020
stx $d021
stx $d022
stx $d023
stx $d024
iny  ;increase Y to go to next rasterline
lda #14
ldx #6
do
  cpy $d012
loop while pl
sta $d020
stx $d021
lda #1
sta $d022
sta $d023
stx $d024
iny  ;increase Y to go to next rasterline
lda #1
ldx #3
do
  cpy $d012
loop while pl
sta $d020
sta $d021
stx $d022
stx $d023
sta $d024
iny  ;increase Y to go to next rasterline
lda #3
ldx #14
do
  cpy $d012
loop while pl
sta $d020
sta $d021
stx $d022
stx $d023
sta $d024
iny  ;increase Y to go to next rasterline
lda #14
ldx #6
do
  cpy $d012
loop while pl
sta $d020
sta $d021
stx $d022
stx $d023
sta $d024
iny  ;increase Y to go to next rasterline
lda #6
do
  cpy $d012
loop while pl
sta $d020
sta $d021
sta $d022
sta $d023
sta $d024
iny  ;increase Y to go to next rasterline
lda #6
do
  cpy $d012
loop while pl
sta $d020
sta $d021
sta $d022
sta $d023
sta $d024
iny  ;increase Y to go to next rasterline
lda #6
do
  cpy $d012
loop while pl
sta $d020
sta $d021
sta $d022
sta $d023
sta $d024
rts


cycle3:
lda #14
ldx #6
do
  cpy $d012
loop while pl
sta $d020
stx $d021
stx $d022
stx $d023
stx $d024
iny  ;increase Y to go to next rasterline
lda #1
ldx #6
do
  cpy $d012
loop while pl
sta $d020
sta $d021
sta $d022
stx $d023
stx $d024
iny  ;increase Y to go to next rasterline
lda #3
ldx #1
do
  cpy $d012
loop while pl
sta $d020
sta $d021
sta $d022
stx $d023
lda #6
sta $d024
iny  ;increase Y to go to next rasterline
lda #14
ldx #3
do
  cpy $d012
loop while pl
sta $d020
sta $d021
sta $d022
stx $d023
lda #1
sta $d024
iny  ;increase Y to go to next rasterline
lda #6
ldx #14
do
  cpy $d012
loop while pl
sta $d020
sta $d021
sta $d022
stx $d023
lda #3
sta $d024
iny  ;increase Y to go to next rasterline
lda #6
ldx #14
do
  cpy $d012
loop while pl
sta $d020
sta $d021
sta $d022
sta $d023
stx $d024
iny  ;increase Y to go to next rasterline
lda #6
do
  cpy $d012
loop while pl
sta $d020
sta $d021
sta $d022
sta $d023
sta $d024
iny  ;increase Y to go to next rasterline
lda #6
do
  cpy $d012
loop while pl
sta $d020
sta $d021
sta $d022
sta $d023
sta $d024
rts


cycle4:
lda #14
ldx #6
do
  cpy $d012
loop while pl
sta $d020
stx $d021
stx $d022
stx $d023
stx $d024
iny  ;increase Y to go to next rasterline
lda #1
ldx #6
do
  cpy $d012
loop while pl
sta $d020
sta $d021
stx $d022
stx $d023
stx $d024
iny  ;increase Y to go to next rasterline
lda #3
ldx #1
do
  cpy $d012
loop while pl
sta $d020
sta $d021
stx $d022
lda #6
sta $d023
sta $d024
iny  ;increase Y to go to next rasterline
lda #14
ldx #3
do
  cpy $d012
loop while pl
sta $d020
sta $d021
stx $d022
lda #1
sta $d023
lda #6
sta $d024
iny  ;increase Y to go to next rasterline
lda #6
ldx #14
do
  cpy $d012
loop while pl
sta $d020
sta $d021
stx $d022
lda #3
sta $d023
lda #1
sta $d024
iny  ;increase Y to go to next rasterline
lda #6
ldx #14
do
  cpy $d012
loop while pl
sta $d020
sta $d021
sta $d022
stx $d023
lda #3
sta $d024
iny  ;increase Y to go to next rasterline
lda #6
ldx #14
do
  cpy $d012
loop while pl
sta $d020
sta $d021
sta $d022
sta $d023
stx $d024
iny  ;increase Y to go to next rasterline
lda #6
do
  cpy $d012
loop while pl
sta $d020
sta $d021
sta $d022
sta $d023
sta $d024
rts


cycle5:
lda #14
ldx #6
do
  cpy $d012
loop while pl
sta $d020
stx $d021
stx $d022
stx $d023
stx $d024
iny  ;increase Y to go to next rasterline
lda #14
ldx #6
do
  cpy $d012
loop while pl
sta $d020
stx $d021
stx $d022
stx $d023
stx $d024
iny  ;increase Y to go to next rasterline
lda #1
ldx #6
do
  cpy $d012
loop while pl
sta $d020
sta $d021
stx $d022
stx $d023
stx $d024
iny  ;increase Y to go to next rasterline
lda #3
ldx #1
do
  cpy $d012
loop while pl
sta $d020
sta $d021
stx $d022
lda #6
sta $d023
sta $d024
iny  ;increase Y to go to next rasterline
lda #14
ldx #3
do
  cpy $d012
loop while pl
sta $d020
sta $d021
stx $d022
lda #1
sta $d023
sta $d024
iny  ;increase Y to go to next rasterline
lda #6
ldx #14
do
  cpy $d012
loop while pl
sta $d020
sta $d021
stx $d022
lda #3
sta $d023
sta $d024
iny  ;increase Y to go to next rasterline
lda #6
ldx #14
do
  cpy $d012
loop while pl
sta $d020
sta $d021
sta $d022
stx $d023
stx $d024
iny  ;increase Y to go to next rasterline
lda #6
do
  cpy $d012
loop while pl
sta $d020
sta $d021
sta $d022
sta $d023
sta $d024
rts


cycle6:
lda #14
ldx #6
do
  cpy $d012
loop while pl
sta $d020
stx $d021
stx $d022
stx $d023
stx $d024
iny  ;increase Y to go to next rasterline
lda #14
ldx #6
do
  cpy $d012
loop while pl
sta $d020
stx $d021
stx $d022
stx $d023
stx $d024
iny  ;increase Y to go to next rasterline
lda #14
ldx #6
do
  cpy $d012
loop while pl
sta $d020
stx $d021
stx $d022
stx $d023
stx $d024
iny  ;increase Y to go to next rasterline
lda #1
ldx #6
do
  cpy $d012
loop while pl
sta $d020
sta $d021
stx $d022
stx $d023
sta $d024
iny  ;increase Y to go to next rasterline
lda #3
ldx #1
do
  cpy $d012
loop while pl
sta $d020
sta $d021
stx $d022
stx $d023
sta $d024
iny  ;increase Y to go to next rasterline
lda #14
ldx #3
do
  cpy $d012
loop while pl
sta $d020
sta $d021
stx $d022
stx $d023
sta $d024
iny  ;increase Y to go to next rasterline
lda #6
ldx #14
do
  cpy $d012
loop while pl
sta $d020
sta $d021
stx $d022
stx $d023
sta $d024
iny  ;increase Y to go to next rasterline
lda #6
do
  cpy $d012
loop while pl
sta $d020
sta $d021
sta $d022
sta $d023
sta $d024
rts


cycle7:
lda #14
ldx #6
do
  cpy $d012
loop while pl
sta $d020
stx $d021
stx $d022
stx $d023
stx $d024
iny  ;increase Y to go to next rasterline
lda #14
ldx #6
do
  cpy $d012
loop while pl
sta $d020
stx $d021
stx $d022
stx $d023
stx $d024
iny  ;increase Y to go to next rasterline
lda #14
ldx #6
do
  cpy $d012
loop while pl
sta $d020
stx $d021
stx $d022
stx $d023
lda #1
sta $d024
iny  ;increase Y to go to next rasterline
lda #14
ldx #6
do
  cpy $d012
loop while pl
sta $d020
stx $d021
stx $d022
lda #1
sta $d023
lda #3
sta $d024
iny  ;increase Y to go to next rasterline
lda #1
ldx #3
do
  cpy $d012
loop while pl
sta $d020
sta $d021
sta $d022
stx $d023
lda #14
sta $d024
iny  ;increase Y to go to next rasterline
lda #3
ldx #14
do
  cpy $d012
loop while pl
sta $d020
sta $d021
sta $d022
stx $d023
lda #6
sta $d024
iny  ;increase Y to go to next rasterline
lda #14
ldx #6
do
  cpy $d012
loop while pl
sta $d020
sta $d021
sta $d022
stx $d023
stx $d024
iny  ;increase Y to go to next rasterline
lda #6
do
  cpy $d012
loop while pl
sta $d020
sta $d021
sta $d022
sta $d023
sta $d024
rts


