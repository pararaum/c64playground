.include "LAMAlib.inc"

.import _frame_upper_left
.import _frame_upper_right
.import _frame_lower_left
.import _frame_lower_right
.import _frame_vertical
.import _frame_horizontal
.import _frame_fillchar
.import _frame_color

.import _window_x1,_window_y1,_window_x2,_window_y2

.import _draw_window_frame_sr

	jsr _draw_window_frame_sr
	rts