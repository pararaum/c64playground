; -*- mode: asm -*_

FLDIRQLINE=56
INITIALIRQLINE=300	
NORMALIRQLINE=256
SCROLLLINE=2
scrollvalue=$d016

	.global	fldirq
