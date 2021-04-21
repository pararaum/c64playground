
	.export	map_data
	.export map_colour_data
	.export SCREEN_BACKGROUND_COLOUR
	.export SCREEN_BORDER_COLOUR

SCREEN_BACKGROUND_COLOUR = 6
SCREEN_BORDER_COLOUR = 6
CHAR_MULTICOLOUR_MODE = 0
MAP_WID_CHRS = 40
MAP_HEI_CHRS = 25
MAP_WID_PXLS = 320
MAP_HEI_PXLS = 200


; Data block size constants:-
SZ_CHAR_MAP_DATA        = 1000
SZ_CHAR_MAP_COLOUR_DATA = 1000




; MAP DATA : 1 (40x25) map : total size is 1000 ($03e8) bytes.

	.data
map_data:

.byte $20,$20,$20,$2e,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
.byte $51,$40,$46,$43,$46,$44,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
.byte $20,$20,$20,$20,$51,$52,$46,$43,$2e,$20,$20,$20,$20,$20,$20,$51
.byte $52,$46,$43,$44,$45,$20,$2e,$20,$20,$20,$20,$20,$20,$20,$20,$20
.byte $20,$20,$20,$2e,$20,$20,$2e,$20,$20,$20,$20,$2e,$20,$20,$20,$20
.byte $6f,$46,$40,$20,$2e,$20,$20,$66,$66,$20,$20,$20,$20,$20,$20,$20
.byte $20,$20,$20,$20,$20,$2a,$40,$44,$45,$77,$20,$20,$20,$20,$20,$20
.byte $20,$20,$20,$20,$20,$20,$2e,$20,$20,$20,$20,$64,$20,$20,$66,$66
.byte $20,$66,$20,$20,$20,$20,$20,$20,$64,$20,$20,$20,$20,$20,$20,$20
.byte $2e,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
.byte $20,$e9,$a0,$a0,$df,$66,$66,$20,$68,$66,$66,$2a,$40,$44,$45,$77
.byte $63,$20,$20,$20,$20,$20,$20,$20,$20,$5a,$20,$20,$2e,$20,$20,$51
.byte $40,$46,$43,$46,$44,$20,$20,$20,$e9,$a0,$a0,$a0,$a0,$df,$7b,$20
.byte $20,$66,$20,$20,$20,$66,$20,$20,$20,$20,$20,$2e,$20,$20,$20,$20
.byte $20,$20,$20,$20,$20,$20,$20,$20,$20,$2e,$20,$20,$20,$20,$20,$20
.byte $a0,$a0,$a0,$e2,$c6,$c4,$a0,$20,$66,$66,$66,$66,$68,$66,$66,$66
.byte $5c,$20,$20,$64,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$64,$20
.byte $20,$20,$20,$20,$20,$20,$20,$64,$c4,$f2,$e3,$a0,$a0,$a0,$a0,$a0
.byte $df,$66,$66,$e6,$e6,$5c,$20,$e6,$e6,$44,$77,$63,$20,$20,$20,$20
.byte $20,$51,$40,$44,$45,$77,$63,$20,$20,$20,$2a,$40,$44,$45,$77,$63
.byte $a0,$ed,$c6,$c5,$c3,$f9,$a0,$a0,$a0,$7b,$e6,$e6,$e6,$e6,$5c,$20
.byte $e6,$e6,$20,$20,$51,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
.byte $20,$20,$20,$20,$20,$e9,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$c7,$e3,$e3
.byte $fe,$a0,$df,$66,$e6,$e6,$e6,$e6,$5c,$e6,$e6,$6c,$a0,$df,$20,$20
.byte $20,$20,$20,$20,$20,$2e,$20,$20,$20,$20,$20,$20,$20,$7c,$a0,$a0
.byte $a0,$a0,$a0,$a0,$ce,$a0,$a0,$a0,$a0,$a0,$a0,$df,$20,$66,$e6,$e6
.byte $e6,$20,$20,$e5,$a0,$a0,$68,$68,$20,$20,$20,$20,$20,$20,$20,$20
.byte $20,$20,$20,$20,$e9,$a0,$a0,$a0,$c4,$c3,$c3,$c5,$a0,$a0,$a0,$a0
.byte $a0,$a0,$a0,$a0,$a0,$df,$66,$e6,$e6,$e6,$e6,$e5,$a0,$89,$e6,$e6
.byte $e6,$68,$68,$20,$20,$20,$20,$20,$20,$20,$20,$f8,$a0,$a0,$a0,$a0
.byte $a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$7b,$20
.byte $66,$e6,$e6,$a0,$a0,$f6,$e6,$e6,$20,$e6,$e6,$68,$e6,$e6,$68,$20
.byte $20,$20,$e9,$a0,$a0,$a0,$ce,$e3,$a0,$a0,$a0,$a0,$a0,$a0,$df,$a0
.byte $a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$df,$20,$20,$a0,$a0,$e7,$20,$e6
.byte $20,$66,$66,$66,$20,$20,$e6,$e6,$e9,$e8,$e4,$d2,$c3,$c5,$a0,$a0
.byte $a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0
.byte $a0,$a0,$a0,$f6,$fe,$4c,$4d,$e6,$66,$66,$e6,$e6,$e6,$66,$e9,$a0
.byte $a0,$a0,$dc,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0
.byte $a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$52,$61,$a0,$a0,$a0,$a0
.byte $62,$6f,$68,$68,$68,$68,$a0,$a0,$a0,$a0,$a0,$fe,$a0,$a0,$a0,$a0
.byte $e6,$66,$66,$e6,$66,$66,$66,$66,$66,$66,$66,$66,$66,$66,$66,$66
.byte $66,$e4,$e4,$e4,$a0,$a0,$a0,$66,$66,$66,$66,$66,$66,$e6,$e6,$66
.byte $66,$66,$e6,$e6,$66,$66,$e6,$66,$66,$66,$66,$66,$66,$66,$66,$66
.byte $66,$66,$66,$e6,$66,$66,$66,$66,$a0,$a0,$e4,$e4,$e4,$e4,$a0,$a0
.byte $a0,$a0,$a0,$a0,$66,$66,$66,$66,$e6,$e6,$e6,$e6,$e6,$e6,$66,$66
.byte $66,$e6,$66,$e6,$66,$e6,$e6,$66,$66,$e6,$e6,$e6,$66,$66,$66,$66
.byte $a0,$a0,$a0,$a0,$a0,$a0,$e0,$e4,$e4,$e4,$e4,$a0,$a0,$66,$68,$66
.byte $68,$66,$66,$66,$66,$66,$66,$66,$66,$66,$66,$66,$66,$66,$66,$66
.byte $66,$66,$66,$66,$66,$66,$66,$7f,$a0,$e0,$a0,$a0,$a0,$e0,$e0,$a0
.byte $a0,$a0,$a0,$e0,$a0,$66,$e8,$e8,$66,$e6,$e6,$66,$e6,$66,$66,$66
.byte $66,$66,$66,$e6,$e6,$66,$66,$e6,$66,$e6,$e6,$66,$66,$66,$a0,$a0
.byte $a0,$e0,$e0,$e0,$e0,$e0,$e0,$e0,$a0,$a0,$e0,$e0,$a0,$a0,$66,$66
.byte $66,$66,$66,$66,$e6,$66,$e6,$66,$e6,$68,$66,$e6,$e6,$66,$66,$66
.byte $e6,$e6,$66,$ff,$66,$a0,$a0,$e0,$e0,$e0,$69,$e0,$e0,$ec,$e0,$e0
.byte $e0,$e4,$cc,$e0,$e0,$a0,$a0,$a0,$a0,$66,$66,$66,$e6,$66,$66,$66
.byte $66,$66,$66,$66,$e6,$e6,$66,$e6,$66,$66,$66,$66,$7f,$a0,$e0,$e0
.byte $e0,$e7,$e0,$e0,$f2,$cb,$e0,$e0,$e9,$e0,$e0,$e3,$e0,$e0,$e0,$e0
.byte $a0,$66,$66,$66,$e6,$e6,$66,$66,$e6,$e6,$e6,$66,$66,$e6,$66,$e6
.byte $e6,$66,$66,$66,$a0,$a0,$e0,$e0,$e0,$dd,$e0,$e0,$e0,$e0,$e0,$e0
.byte $e0,$e9,$e0,$e0,$e0,$e0,$e0,$e0,$a0,$a0,$66,$e8,$66,$66,$66,$66
.byte $e6,$e6,$e6,$e6,$66,$66,$66,$66,$66,$66,$66,$7f,$a0,$a0,$a0,$a0
.byte $a0,$c8,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0
.byte $a0,$a0,$7f,$66,$66,$66,$66,$66

;  MAP COLOUR DATA : 1 (40x25) map : total size is 1000 ($03e8) bytes.

	.data
map_colour_data:

.byte $0e,$0e,$0e,$03,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e
.byte $07,$07,$07,$07,$07,$07,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e
.byte $0e,$0e,$0e,$0e,$07,$07,$07,$07,$07,$0e,$0e,$0e,$0e,$0e,$0e,$07
.byte $07,$07,$07,$07,$07,$0e,$03,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e
.byte $0e,$0e,$0e,$03,$0e,$0e,$07,$0e,$0e,$0e,$0e,$0a,$0e,$0e,$0e,$0e
.byte $07,$07,$07,$0e,$0a,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e
.byte $0e,$0e,$0e,$0e,$0e,$07,$07,$07,$07,$07,$0e,$0e,$0e,$0e,$0e,$0e
.byte $0e,$0e,$0e,$0e,$0e,$0e,$03,$0e,$06,$06,$06,$0c,$0e,$0e,$0e,$0e
.byte $06,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$07,$0e,$0e,$0e,$0e,$0e,$0e,$0e
.byte $07,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e
.byte $06,$0c,$0c,$0c,$0c,$0e,$0e,$06,$0e,$0e,$0e,$07,$07,$07,$07,$07
.byte $07,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$03,$0e,$0e,$0a,$0e,$0e,$07
.byte $07,$07,$07,$07,$07,$0e,$0e,$0e,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0b
.byte $06,$0b,$0b,$0b,$0b,$0e,$0e,$0e,$0e,$0e,$0e,$07,$0e,$0e,$0e,$0e
.byte $0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$03,$0e,$0e,$0e,$0e,$0e,$0e
.byte $0c,$0c,$0c,$0c,$0c,$0c,$0c,$0b,$0b,$0b,$0b,$0e,$0e,$0e,$0e,$0e
.byte $0e,$0e,$0e,$07,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$07,$0e
.byte $0e,$0e,$0e,$0e,$0e,$0e,$0e,$07,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c
.byte $0c,$0e,$0b,$0e,$0e,$0e,$06,$0e,$0e,$07,$07,$07,$0e,$0e,$0e,$0e
.byte $0e,$07,$07,$07,$07,$07,$07,$0e,$0e,$0e,$07,$07,$07,$07,$07,$07
.byte $0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0e,$0e,$0e,$0e,$0e,$06
.byte $0e,$0e,$0e,$0e,$00,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e
.byte $0e,$0e,$0e,$0e,$0e,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c
.byte $0c,$0c,$0c,$0b,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$00,$00,$00,$0e,$0e
.byte $0e,$0e,$0e,$0e,$0e,$03,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0c,$0c,$0c
.byte $0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$06,$0b,$0e,$0e
.byte $0e,$0e,$0e,$00,$00,$00,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e
.byte $0e,$0e,$0e,$0e,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c
.byte $0c,$0c,$0c,$0c,$0c,$0c,$0b,$0e,$0e,$0e,$0e,$00,$00,$00,$0e,$0e
.byte $0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0c,$0c,$0c,$0c,$0c
.byte $0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$06
.byte $0b,$0e,$0e,$00,$00,$00,$0e,$0e,$06,$0e,$0e,$0e,$0e,$0e,$0e,$0e
.byte $0e,$0e,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c
.byte $0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0e,$0e,$00,$00,$00,$0e,$0e
.byte $0e,$0b,$0b,$0b,$04,$04,$0e,$0e,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c
.byte $0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c
.byte $0c,$0c,$0c,$00,$00,$00,$00,$0e,$0b,$0b,$0e,$0e,$0e,$0b,$0c,$0c
.byte $0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c
.byte $0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$00,$00,$0f,$0f,$0c,$0c
.byte $0c,$0c,$04,$04,$04,$04,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c
.byte $04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04
.byte $04,$0f,$0f,$0f,$0f,$0f,$0f,$04,$04,$04,$04,$04,$04,$04,$04,$04
.byte $04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04
.byte $04,$04,$04,$04,$04,$04,$04,$04,$0f,$0b,$0f,$0f,$0f,$0f,$0f,$0f
.byte $0f,$0f,$0f,$0f,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04
.byte $04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04
.byte $0f,$0c,$0b,$0b,$0b,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$04,$04,$04
.byte $04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04
.byte $04,$04,$04,$04,$04,$04,$04,$04,$0f,$0f,$0f,$0c,$0c,$0f,$0f,$0b
.byte $0b,$0b,$0b,$0f,$0f,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04
.byte $04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$0f,$0f
.byte $0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0c,$0c,$0f,$0f,$0f,$0f,$04,$04
.byte $04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04
.byte $04,$04,$04,$04,$04,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f
.byte $0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$04,$04,$04,$04,$04,$04,$04
.byte $04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$0f,$0f,$0f
.byte $0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f
.byte $0f,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04
.byte $04,$04,$04,$04,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f
.byte $0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$04,$04,$04,$04,$04,$04
.byte $04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$0f,$0f,$0f,$0f
.byte $0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f
.byte $0f,$0f,$04,$04,$04,$04,$04,$04

