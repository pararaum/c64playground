# needed shift: (screen+padding)*8
# maximum speed=8

# 50*8=400
# 32 acc, 18 sus, 32 dec

from __future__ import print_function
import sys

acc_steps=32
sus_steps=18
dec_steps=32


x=0
speed=0
acc=1
for i in range(acc_steps):
    x+=speed
    if i % 4==0:
        speed+=acc
for i in range(sus_steps):
    x+=speed
print("max speed=",speed)        
for i in range(dec_steps):
    x+=speed
    if i % 4==0:
        speed-=acc
print("x=",x)
print("last speed=",speed)
