#ifndef __libT7D_H__2024__
#define __libT7D_H__2024__

/*! Busy wait for a frame.
 *
 * This is achieved by polling $d011. The pm variant uses BPL/BMI.
 *
 */
void busywait_frame_pm(void);

/*! Busy wait for a frame.
 *
 * This is achieved by polling $d011. The mp variant uses BMI/BPL.
 *
 */
void busywait_frame_mp(void);

#endif
