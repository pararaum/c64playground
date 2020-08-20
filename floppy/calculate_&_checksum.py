#! /usr/bin/env python3

import sys

def main(fname):
    data = open(fname, "rb").read()
    summ = 0
    for char in data:
        summ += char
        if summ >= 256:
            summ = (summ & 0xFF) + 1
    with open('&' + fname + ",usr", "wb") as out:
        out.write(data)
        out.write(chr(summ).encode("iso-8859-1"))
    
    

if __name__ == "__main__":
    main(sys.argv[1])
