;;; Taken from https://www.codebase64.org/doku.php?id=base:16bit_xorshift_random_generator

	.export	xorshift
	.export	xorshift_seed
	.export	_xorshift
	.export	_xorshift_seed

_xorshift_seed=xorshift_seed
_xorshift=xorshift

	.zeropage
prng_state:	.res	2

	.code
	rng_zp_low = prng_state
	rng_zp_high = prng_state+1

.proc	xorshift_seed
        sta	rng_zp_low
        stx	rng_zp_high
	rts
.endproc

.proc	xorshift
	lda	rng_zp_high
        lsr
        lda	rng_zp_low
        ror
        eor	rng_zp_high
        sta	rng_zp_high	; high part of x ^= x << 7 done
        ror			; a has now x >> 9 and high bit comes from low byte
        eor	rng_zp_low
        sta	rng_zp_low	; x ^= x >> 9 and the low part of x ^= x << 7 done
        eor	rng_zp_high 
        sta	rng_zp_high	; x ^= x << 8 done
	tax
	lda	rng_zp_low
        rts
.endproc
	
