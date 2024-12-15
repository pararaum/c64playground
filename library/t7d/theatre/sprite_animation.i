; -*- mode: asm -*-

	.struct	SpriteAnimationEntry

	speed_current	.byte
	animation_speed	.byte
	animation_current .byte
	animation_delta	.byte
	animation_min	.byte
	animation_max	.byte
	animation_flags	.byte

	.endstruct
