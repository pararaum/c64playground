#! /usr/bin/python3

"""Compute's MLX

This is a simple program for entering programs using hex codes.

See the following links::
 - http://www.atarimagazines.com/compute/issue67/313_1_The_New_MLX.php
 - http://www.atarimagazines.com/compute/issue92/275_1_Sprite_Grabber_For_The_64.php

"""


#This is an example session.
## 
## display data
## 
## starting at? c000
## 
## press: space to pause, return to break
## 
## c000:a9 0c 8d 15 d0 a9 ff 8d 17
## c008:3b 63 8d 3c 63 a9 01 8d c6
## c010:00 00 00 00 00 00 00 00 91
## c018:3b af f7 3a 7e ff fd da 90
## c020:bc 37 76 af 3f fd 5e ff 46
## 
## ** end of data **
## 
## 
##            mlx command menu
## 
##              enter data
##              display data
##              load data
##              save file
##              quit
## 

def out(addr, data, ck):
    out = [" %02x" % i for i in data]
    return "%04X:%s\t%02X" % (addr, ' '.join(out), ck)

def checksum(addr, data):
    """ Checkum algorithm for MLX

    370 CK=int(AD/Z6):CK=AD-Z4*CK+Z5*(CK>Z7):goto390
    380 CK=CK*Z2+Z5*(CK>Z7)+A
    390 CK=CK+Z5*(CK>Z5):return
    """
    def maxck(ck_):
        return ck_ - Z5 * (ck_ > Z5)
    Z4=254
    Z5=255
    Z6=256
    Z7=127
    #Initialise checksum from address.
    ck = int(addr/Z6)
    ck = addr - Z4 * ck - Z5 * (ck > Z7)
    #Handle overflow.
    ck = maxck(ck)
    #Checksum the data
    for a in data:
        ck = ck * 2 - Z5 * (ck > Z7) + a
        #Handle overflow.
        ck = maxck(ck)
    return ck

data = """c000:a9 0c 8d 15 d0 a9 ff 8d
c008:3b 63 8d 3c 63 a9 01 8d
c010:00 00 00 00 00 00 00 00
c018:3b af f7 3a 7e ff fd da
c020:bc 37 76 af 3f fd 5e ff"""

mem = dict()
for line in data.split('\n'):
    addr, data = line.split(':')
    addr = int(addr, 16)
    data = [int(i, 16) for i in data.split()]
    mem[addr] = data
print(mem)
for addr in sorted(mem.keys()):
    print(out(addr, mem[addr], checksum(addr, mem[addr])))

