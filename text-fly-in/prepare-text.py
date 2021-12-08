#!/usr/bin/python
# -*- coding: utf-8 -*-
##
## Read in text lines to be shown on screen and convert
## to an assembler file
##
## this code is released under a CC-0 license
## by Wil in January 2021

from __future__ import print_function
import argparse
import sys
import struct

versioninfo="Prepare-text by Wil, Version 1.0 November 2021"

class ShowVersionInfo(argparse.Action):
    def __call__(self, parser, namespace, values, option_string):
        print(versioninfo)
        parser.exit() # exits the program with no more arg parsing and checkin

# Parse command-line arguments

parser = \
    argparse.ArgumentParser(description='Read in text lines to be shown on screen and convert to an assembler file.'
                            )
parser.add_argument('filename', help='File to be converted.',
                    default='clocktower50.c')
parser.add_argument('-o', '--outfile', help='save converted image' , default='outfile.s')
parser.add_argument('-p', '--padding', 
                    help='amount of extra padding between two screens'
                    , default=10)
parser.add_argument('-l', '--leadpadding', 
                    help='amount of padding spaces before first text'
                    , default=9)
parser.add_argument('-w', '--width',
                    help='screen width (for centering)'
                    , default=40)
parser.add_argument('-c', '--case',
                    help='select to force a case change to "uppercase " or "lowercase"'
                    , default='lowercase')
parser.add_argument('-v', '--version', nargs=0, action=ShowVersionInfo,
                    help='display version info')

args = parser.parse_args()

prepad=args.leadpadding
with open(args.filename) as fp:
    while True:
        line = fp.readline().strip()
        if not line:
            break
        if args.case.lower()=="lowercase":
            line=line.lower()
        elif args.case.lower()=="uppercase":
            line=line.upper()
        padding=args.width - len(line)
        leftpadding=int(padding/2)
        rightpadding=padding-leftpadding
        prepad+=leftpadding
        if prepad>0:
            print ("  .byte 0,",prepad)
        print ('  scrcode "'+str(line)+'"')
        prepad=rightpadding+args.padding
prepad+=args.width+args.padding-args.leadpadding
if prepad>0:
    print ("  .byte 0,",prepad)
print ("  .byte 0,0")
