#!/usr/bin/python
# -*- coding: utf-8 -*-
##
## petscii2x is a companion tool for PETSCIIs designed with Marq's PETSCII editor
##
## Code by Wil
##
## petscii2x takes a saved PETSCII (filetype .c) as input and converts it depending on the -f format option:
##
## BASIC          a BASIC program containing PRINT commands (see options for linenumber and increment)
## BASICSLIDES    a BASIC program that waits for a key press after each screen
## DATA           a BASIC program with data lines containing the PETSCII string by string
## LIST           a BASIC program with hidden REM lines that displays the PETSCII when listed
##                (see option -s for adding a SYS line that jumps right after the REMs)
## ESCAPEDSTRING  PETSCII as a sequence of control commands in ca65 format that can be sent to $FFD2
## SEQ            sequence of control commands to produce the PETSCII in a BBS.
## ASM            a simple RLE-compressed form of the PETSCII that can be reproduced with the Assembler
##                routine displayPETSCII.s. Decompression is very fast. Compression depends on
##                the complexity of the image and is 50 to 75%
## BIN            convert each frame to a file with 1000 byte char data and 1000 byte color data
##
## Except for BIN, the program detects and leaves out unnecessary color switches or whitespace trailing a line.
##
## from Version 2.5 on, petscii2x replaces the previously published PETSCII2BASIC tool

from __future__ import print_function
import argparse,sys,os,tempfile,struct,subprocess

VERSIONINFO="petscii2x by Wil, Version 3.01a May 2025"

'''
Changelist:
3.01a
VCC animation decrunch effect
3.01
bugfix on repeated sequences
3.00
added repeated sequences compression
2.99
avoid color changes with whitespace characters
2.97
control character frequencies output in verbose mode
2.96
changed output to avoid step with data bytes eor'd with markerbyte
2.95
option to align line numbers to a full 100 or 1000 with each new image
2.94
added case to repeat a single byte 1 time if its code is a marker byte
2.93
fixed usage text
2.92
option listsys instead of -f list --sys
LIST with crunched file attached
2.91
omitting closing quotes on BASIC lines to save on byte per line
fixed bug where short lines where not properly covered in LIST mode
added --case option for forced upper or lower case
2.9
LIST mode now properly handles full lines with 40 PETSCII characters by using a LEFT+INSERT+CHAR sequence
fixed number of cover characters in LIST mode
2.8
labelname option, also affects number_of_images
support multiple input files
fixed bug with first color in last line when using BASICSLIDES mode
removed unnecessary color changes in BASIC export
2.7
added frameblend function
2.611
fixed typo in generated label "numer_of_imgages"
2.61
updated usage message
2.6
-d Option now adds number of pics in first byte
2.51+2.52
When no outfile is specified, a generic outfile with the proper file extension is used
2.5
integrated ASM and BASIC exports into a single tool
LISTSYS replaed by an option for LIST
fixed coverage bug in LIST with SYS option
2.42
new export: SEQ
better handling of empty parts in the pic
2.41
fixed error when writing out unknown filename
added simulation of repetition count slicing
added code to escape characters from marker block
2.4
added version and "newer" option, removed basic option
2.3
option to add a directory with pointers at the start of the file
2.2
reduce maximum repetition to 15
add rep,00 as endcode
2.1
added support for reduced pic height (-y option)
2.11
new export types ESCAPEDSTRING, PRG
improvement considering the already set color from previous row
2.0
LISTART support
1.0 first release as PETSCII2BASIC tool
'''

basic_prg = []
codelines = []
basic_start = 0x801
lastptr = -1

CHR_QUOTE = 0x22
CHR_UP = 0x91
CHR_DOWN = 0x11
CHR_LEFT = 0x9d
CHR_RIGHT = 0x1d
CHR_DELETE = 0x14
CHR_RETURN = 0x0d
CHR_LOWERCASE = 0x0e
CHR_UPPERCASE = 0x8e
CHR_SHIFTRETURN = 0x8d
CHR_COMMA = 0x2c
CHR_SPACE = 0x20
CHR_COLORCODES = [
    0x90, #BLK
    0x05, #WHT
    0x1c, #RED
    0x9f, #CYN
    0x9c, #PUR
    0x1e, #GRN
    0x1f, #BLU
    0x9e, #YEL
    0x81, #ORANGE
    0x95, #BROWN
    0x96, #LRED
    0x97, #DGRAY
    0x98, #MGRAY
    0x99, #LGRN
    0x9a, #LBLU
    0x9b, #LGRAY
    ]
CHR_RVSON = 0x12
CHR_RVSOFF = 0x92
CHR_CLRSCR = 147
CHR_INSERT = 148

TOKEN_PRINT = 0x99
TOKEN_POKE = 0x97
TOKEN_CHR = 0xc7
TOKEN_DATA = 0x83
TOKEN_PLUS = 0xAA
TOKEN_EQ = 0xB2
TOKEN_AND = 0xAF
TOKEN_OR = 0xB0
TOKEN_GET = 0xA1
TOKEN_IF = 0x8B
TOKEN_THEN = 0xA7
TOKEN_SYS = 0x9e
TOKEN_REM = 0x8f

ASM_INDENT = '        '

QUOTESTR = [TOKEN_CHR, ord('('), ord('3'), ord('4'), ord(')'), TOKEN_PLUS, TOKEN_CHR, ord('('), ord('2'), ord('0'), ord(')'), TOKEN_PLUS,
            TOKEN_CHR, ord('('), ord('3'), ord('4'), ord(')')]

SPACES = [chr(32),chr(96)]

QUOTELST = [CHR_QUOTE,CHR_QUOTE,CHR_DELETE]

def load_petscii_c(filename):
    frames = []
    with open(filename) as fp:
        while True:
            tmp = fp.readline()
            if tmp[:7] == '// META':
                break

            # parse border and background color

            (bordercol, bgcol) = [int(x) for x in
                                  fp.readline().strip().rstrip(','
                                  ).split(',')]

            # parse character values

            tmp = ''
            for x in range(25):
                tmp = tmp + fp.readline().strip()
            chars = [int(x) for x in tmp.rstrip(',').split(',')]

            # parse color values

            tmp = ''
            for x in range(25):
                tmp = tmp + fp.readline().strip()
            cols = [int(x) for x in tmp.rstrip(',').split(',')]
            tmp = fp.readline()
            petscii = [bordercol, bgcol, chars, cols]
            frames.append(petscii)
    return frames


def savePrg(filename):
    outfile = open(filename, 'wb')

    # write startaddress
    outfile.write(struct.pack('<H', 0x801))
    for b in basic_prg:
        outfile.write(struct.pack('B', b))

def saveBin(filename):
    outfile = open(filename, 'wb')
    # bin has no startaddress
    for b in basic_prg:
        outfile.write(struct.pack('B', b))


def closeLine():
    global lastptr, basic_prg
    if lastptr >= 0:
        addr = len(basic_prg) + 0x801 + 1
        basic_prg[lastptr] = addr & 0xFF
        basic_prg[lastptr + 1] = addr >> 8
        lastptr = -1
        basic_prg += [0]


def closePrg():
    global basic_prg
    closeLine()
    basic_prg += [0, 0]


def addLine():
    global linenr, basic_prg, lastptr
    closeLine()
    lastptr = len(basic_prg)
    basic_prg += [0, 0]
    basic_prg += [linenr & 0xFF]
    basic_prg += [linenr >> 8]
    oldlinenr = linenr
    linenr += lineInc
    return oldlinenr


def addDATA():
    global basic_prg
    basic_prg += [TOKEN_DATA]

def addREM():
    global basic_prg
    basic_prg += [TOKEN_REM]

def addHackedREM():
    global basic_prg
    addREM()
    basic_prg += [CHR_QUOTE,CHR_SHIFTRETURN,CHR_UP]

def addPRINT():
    global basic_prg
    basic_prg += [TOKEN_PRINT]


def addPOKE(a, b):
    global basic_prg
    basic_prg += [TOKEN_POKE]
    addNumber(a)
    addChars(',')
    addNumber(b)

def addSYS(a):
    global basic_prg
    basic_prg += [TOKEN_SYS]
    addNumber(a)

def addNumber(n):
    addChars(str(n))


def addQuotedString(str):
    global basic_prg
    basic_prg += [CHR_QUOTE]
    basic_prg += str
    basic_prg += [CHR_QUOTE]


def addString(str):
    global basic_prg
    basic_prg += str


def addChars(s):
    global basic_prg
    for ch in s:
        basic_prg += [ord(ch)]


def addByte(b):
    global basic_prg
    basic_prg += [b]


def dollarHex(n):
    return '$' + hex(n)[2:]


def removeByte():
    global basic_prg
    last_byte = basic_prg[-1]
    del basic_prg[-1]
    return last_byte


def isPrintable(c):
    if isinstance(c, str):
        c = ord(c)
    return (c & 127) > 31


def countPrintable(s):
    return sum(1 for c in s if isPrintable(c))


def decodeLine(f, y, lastlinehack = False):
    global current_color
    if lastlinehack:
        firstlinepart = []
    rev = False
    full = True
    c = []
    empty = True
    for x in range(40):
        char = f[2][x + y * 40]
        col = f[3][x + y * 40]
        if char not in [32,96]:
            empty = False
            if col != current_color:
                if lastlinehack and x == 39:
                    ext1 += [CHR_COLORCODES[col]]
                else:
                    c += [CHR_COLORCODES[col]]
                current_color = col
        v = char > 0x7F
        c2 = char
        char = char & 0x7F
        char = char - 32 + (char & 96) + (char < 32) * 96
        if rev - v:
            if lastlinehack and x == 39:
                if v:
                    ext1 += [CHR_RVSON]
                else:
                    ext1 += [CHR_RVSOFF]
            else:
                if v:
                    c += [CHR_RVSON]
                else:
                    c += [CHR_RVSOFF]
            rev = v
        if char == 0x22:
            if targetformat == 'data':
                c += [39]  # in data lines we need to replace double quotes with single quotes, sorry
            else:
                c += [0x22, ord('Q'), ord('$'), 0x22]
        else:
            c += [char]
        if lastlinehack:
            if x == 37:
                firstlinepart = c
                c = []
            if x == 38:
                ext1 = c
                c = []
    if rev:
        c += [CHR_RVSOFF]
    elif not lastlinehack:
        while len(c) > 0 and (c[-1] in [32,96,160,224]):
            del c[-1]
            full = False
    if lastlinehack:
        return (firstlinepart, ext1, c)
    if empty:
        return ([], False)
    else:
        return (c, full)

def getLastLineNotEmpty(f):
    global current_color
    current_color = -1
    for lastline in range(24,-1,-1):
        (c, full) = decodeLine(f, lastline)
        if not c == []:
            current_color = -1
            return lastline
        #if not all(c in SPACES for c in enteredpass1):
        #    return lastline
    current_color = -1
    return -1

def convertPETSCII2DATA(frames):
    global linenr, lineInc,current_color
    for f in frames:
        current_color = -1
        for y in range(25):
            (c, full) = decodeLine(f, y)
            addLine()
            addDATA()
            addQuotedString(c)
            closeLine()
    closePrg()

def convertPETSCII2LIST(frames,sysflag):
    global linenr, lineInc, basic_prg,current_color
    if sysflag:
        toadd = [chr(TOKEN_SYS),'1','2','3','4',':']
    else:
        toadd= []
    prevlineno=linenr
    for f in frames:
        current_color = -1
        lastline = getLastLineNotEmpty(f)
        if lastline == -1:
            continue

        for y in range(lastline+1):
            old_current_color=current_color
            (c, full) = decodeLine(f, y)
            lastlinehack = False
            addLine()
            addChars(toadd)
            addHackedREM()

            if full:
                # we need to introduce a hack to avoid screen scrolling
                # flip last two characters and colors
                (f[2][38 + y * 40], f[2][39 + y * 40]) = (f[2][39 + y
                        * 40], f[2][38 + y * 40])
                (f[3][38 + y * 40], f[3][39 + y * 40]) = (f[3][39 + y
                        * 40], f[3][38 + y * 40])
                current_color=old_current_color
                (c, ext1, ext2) = decodeLine(f, y, True)
                addString(c + ext1 + [CHR_LEFT] + [CHR_INSERT] + ext2)
            else: #line is not full
                toCover=5+len(str(linenr))
                if toadd!=[]:
                    toCover+=10
                toadd=[]
                addString(c)
                if (countPrintable(c)<toCover):
                    addString([CHR_SPACE]*(toCover-countPrintable(c)))
            if y==lastline:
                addByte(CHR_COLORCODES[14])
            closeLine()
    closePrg()
    file_end_address= 0x801+len(basic_prg)
    if sysflag:
      basic_prg[5:9]=[ord(x) for x in str(file_end_address)]
    return file_end_address

def petscii_to_char(c):
    if c>=65 and c<91:
        c=c+32
    elif c>=97 and c<123:
        c=c-32
    if c>=32 and c<127:
        return chr(c)
    return "%"+hex(256+c)[3:]

def convertPETSCII2ESCAPEDSTRING(frames):
    global linenr, lineInc, basic_prg, current_color
    toadd= []
    prevlineno=linenr
    for f in frames:
        lastline = getLastLineNotEmpty(f)
        if lastline == -1:
            continue
        for y in range(lastline+1):
            (c, full) = decodeLine(f, y)
            for ch in c:
                print(petscii_to_char(ch),end="")
            if y<lastline and full==False:
                print(petscii_to_char(10),end="")
            closeLine()
    closePrg()

def convertPETSCII2SEQ(frames, filename):
    global linenr, lineInc, basic_prg, current_color
    toadd = []
    prevlineno = linenr

    with open(filename, 'wb') as output_file:
        if args.case == 'lower':
            output_file.write(bytes([CHR_LOWERCASE]))
        elif args.case == 'upper':
            output_file.write(bytes([CHR_UPPERCASE]))
        for f in frames:
            lastline = getLastLineNotEmpty(f)
            if lastline == -1:
                continue
            for y in range(lastline + 1):
                (c, full) = decodeLine(f, y)
                for ch in c:
                    if 96 <= ch <= 127:
                        ch += 96
                    elif 160 <= ch <= 190:
                        ch += 64
                    output_file.write(bytes([ch]))
                if y < lastline and not full:
                    output_file.write(bytes([13]))
                closeLine()
    closePrg()

def convertPETSCII2BIN(frames):
    for f in frames:
        for y in range(25):
            for x in range(40):
                char = f[2][x + y * 40]
                addByte(char)
        for y in range(25):
            for x in range(40):
                col = f[3][x + y * 40]
                addByte(col)

def convertPETSCII2PRINT(frames, slidemode):
    global linenr, lineInc, current_color
    bordercol = -1
    bgcol = -1
    prevlineno=linenr
    for f in frames:
        current_color = -1
        currentlineno=linenr
        if f[0] != bordercol or f[1] != bgcol or slidemode:
            bordercol = f[0]
            bgcol = f[1]
            addLine()
            addPOKE(53280, bordercol)
            addChars(':')
            addPOKE(53281, bgcol)
            if 0x22 in f[2] or 128 + 0x22 in f[2]:
                addChars(':Q$')
                addString([TOKEN_EQ])
                addString(QUOTESTR)
            closeLine()
        toadd = [CHR_CLRSCR]
        if args.case == 'lower':
            toadd+=[CHR_LOWERCASE]
        elif args.case == 'upper':
            toadd+=[CHR_UPPERCASE]
        lastlinehack = False
        for y in range(25):
            old_current_color=current_color
            (c, full) = decodeLine(f, y)
            if c == []:
                toadd += [0x11]  # crsr down
                continue
            addLine()
            addPRINT()
            if slidemode and y == 24 and full:
                # we need to introduce a hack to avoid screen scrolling
                # flip last two characters and colors
                (f[2][38 + y * 40], f[2][39 + y * 40]) = (f[2][39 + y
                        * 40], f[2][38 + y * 40])
                (f[3][38 + y * 40], f[3][39 + y * 40]) = (f[3][39 + y
                        * 40], f[3][38 + y * 40])
                current_color=old_current_color
                (c, ext1, ext2) = decodeLine(f, y, True)
                lastlinehack = True
            addQuotedString(toadd + c)
            toadd = []
            if lastlinehack:
                addChars(';')
                closeLine()
                addLine()
                addPRINT()
                addQuotedString(toadd + ext1 + [CHR_LEFT])
                addByte(TOKEN_CHR)
                addChars('(148)')
                addQuotedString(ext2)
                addChars(';')
            elif full or y == 24:
                addChars(';')
            else:
                removeByte()    #remove the closing "
            closeLine()
        if slidemode:
            currentline = addLine()
            addByte(TOKEN_GET)
            addChars('A$:')
            addByte(TOKEN_IF)
            addChars('A$')
            addByte(TOKEN_EQ)
            addChars('""')
            addByte(TOKEN_THEN)
            addChars(str(currentline))
            closeLine()
            if len(frames)>1:
                #add code for flipping back
                addLine()
                addByte(TOKEN_IF)
                addChars('A$')
                addByte(TOKEN_EQ)
                addQuotedString([CHR_LEFT])
                addByte(TOKEN_OR)
                addChars('A$')
                addByte(TOKEN_EQ)
                addQuotedString([CHR_UP])
                addByte(TOKEN_THEN)
                if prevlineno<currentlineno:
                    addChars(str(prevlineno))
                else:
                    addChars(str(currentline))
                closeLine()
        prevlineno=currentlineno
        if not linenr % alignlines == 0:
          linenr = (linenr // alignlines + 1) * alignlines  # Align linenr
    closePrg()

def convertPETSCII2ASM(frames):
   global codelines,pic_height,add_dir

   if add_dir:
      codelines.append(labelname + "num: .byte "+str(len(frames)))
      imgno=0
      petscii_dir="        .word "
      sep=""
      for f in frames:
         petscii_dir += sep + labelname + "img"+str(imgno)
         sep=","
         imgno+=1
      codelines.append(labelname + "dir:   ;list of pointers to compressed PETSCII images")
      codelines.append(petscii_dir)
      codelines.append("")

   imgno=0
   for f in frames:
      codelines.append(labelname + "img"+str(imgno)+":")
      imgno+=1


      #find out which 32 byte block is the least used
      ttblocks=[0]*8
      lastch=-1
      lastcol=-1
      count=0
      for i in range(pic_height*40):
          if f[2][i]==lastch and (f[3][i]==lastcol or (args.ignorespacecolor and (lastch in [32,96]))):
              count+=1
              continue
          #simulate write out of sequence
          while(count>1):
              n=min(count,15)
              count-=n
          if count==1:
              ttblocks[int(lastch/32)]+=1
          lastch=f[2][i]
          lastcol=f[3][i]
          count=1
      #select the least used block as marker
      marker=ttblocks.index(min(ttblocks))
      markerbyte=marker*32
      #elaborate stream of databytes with RLE encoding
      lastch=-1
      lastcol=-1
      count=0
      databytes=[]
      controlCharFrequencies=[0]*32
      for i in range(pic_height*40):
          if f[2][i]==lastch and (f[3][i]==lastcol or (args.ignorespacecolor and (lastch in [32,96]))):
              count+=1
              continue
          #write out last char or char sequence
          while(count>13):
              n=min(count,256)
              databytes.append(14)  #14 is the longrep code
              databytes.append(n & 0xff)                        
              databytes.append(lastch)
              count-=n
              controlCharFrequencies[14]+=1
          while(count>2):
              n=min(count,13)
              databytes.append(n)
              databytes.append(lastch)
              count-=n
              controlCharFrequencies[n]+=1
          if 0<count<=2:
              if lastch//32==marker:
                #escape character from marker block
                databytes.append(count)
                databytes.append(lastch)
                controlCharFrequencies[1]+=1
              else:
                #write out databyte normally
                for j in range(count):
                  databytes.append(lastch^markerbyte)
          #if color changed, write out new color
          if not (args.ignorespacecolor and (f[2][i] in [32,96])):         
              if f[3][i]!=lastcol:
                  databytes.append(16+f[3][i])
                  controlCharFrequencies[16+f[3][i]]+=1
                  lastcol=f[3][i]
          lastch=f[2][i]          
          count=1
      #write out last char or char sequence
      while(count>13):
          n=min(count,256)
          databytes.append(14)  #14 is the longrep code
          databytes.append(n & 0xff)                        
          databytes.append(lastch)
          count-=n
          controlCharFrequencies[14]+=1          
      while(count>2):
          n=min(count,13)   #because 14,15 are control codes
          databytes.append(n)
          databytes.append(lastch)
          count-=n
          controlCharFrequencies[n]+=1
      if 0<count<=2:
          if lastch//32==marker:
            #escape character from marker block
            databytes.append(count)
            databytes.append(lastch)
            controlCharFrequencies[1]+=1
          else:
            #write out databyte normally
            for j in range(count):
              databytes.append(lastch^markerbyte)

      #add end code
      databytes.append(15) #control code 15 = end of pic
      controlCharFrequencies[0]+=1
      
      if args.verbose:
        print("Control character frequencies:")
        print("Escape byte (index 0):", controlCharFrequencies[0])
        print("Repetition markers (indices 1 to 15):", controlCharFrequencies[1:16])
        print("Color codes (indices 16 to 31):", controlCharFrequencies[16:32])

      if args.verbose:
        print("Applying sliding window repeated sequences compression...")
        print(f"Compressed {len(databytes)} bytes to ", end="")
      databytes = compress_repeated_sequences(databytes, markerbyte)
      if args.verbose:
        print(len(databytes), " bytes after compression.")
                         
      hexbytes = [f"${num:02x}" for num in databytes]
      
      #assemble hexbytes into lines
      imagesize=2+len(hexbytes)
      compressionrate=int(100*(2+len(hexbytes)) / (2+2*pic_height*40))
      codelines[-1]+="         ;compressed image size "+str(imagesize)+" bytes, compressed to "+str(compressionrate)+"%"
      currentline=ASM_INDENT+".byte "+dollarHex(f[0])+","+dollarHex(marker*32+f[1])+" ;border and bg color"
      for i in range(len(hexbytes)):
          if i % 32==0:
              codelines.append(currentline)
              currentline=ASM_INDENT+".byte "
              sep=""
          currentline+=sep+hexbytes[i]
          sep=","
      codelines.append(currentline)
      codelines.append("")
      
def compress_repeated_sequences(databytes, markerbyte):
    # Replaces repeated sequences of length > 3 by repeat_ctrl_code, number of bytes, -offset
    # Note that bytes in [markerbyte + 1, markerbyte + 13] are always a union with the following byte
    # and markerbyte + 14 is a union with the following 2 bytes
    
    # Generate stop byte markers
    stop_byte = [0] * len(databytes)  # 0 breakable, 1 not breakable, 2 not breakable, no further iteration
    i = 0
    
    while i < len(databytes):
        stop_byte[i] = 0
        if 1 <= databytes[i] <= 13:  # Treat repeat atomic with its argument
            stop_byte[i + 1] = 1
            i += 1
        elif databytes[i] == 14:  # Treat long repeat atomic with its arguments
            stop_byte[i + 1] = 1
            stop_byte[i + 2] = 1
            i += 2
        i += 1

    compressed = []
    i = 0
    
    while i < len(databytes):
        j = len(compressed)
        # Check if there is a sequence
        bestcount = 0
        bestback = 0       
        
        for back in range(1, 254):
            if j < back:
                continue
            
            c = 0
            while i + c < len(databytes) and c + j - back < len(compressed) and databytes[i + c] == compressed[j - back + c]:
                c += 1
                if c == 255:
                    break
            
            while i+c < len(databytes) and stop_byte[i + c]:
                c -= 1
            
            if c > bestcount:
                bestcount = c
                bestback = back
        
        if bestcount > 3:
            compressed.append(0)
            #remove stop_byte from bestcount
            stops=0
            for j in range(i, i+bestcount):
                if stop_byte[j]:
                    stops += 1
            compressed.append(bestcount-stops)
            compressed.append(256-bestback-2)
            i += bestcount
        else:
            compressed.append(databytes[i])
            i += 1
            while i < len(databytes) and stop_byte[i] != 0:
                compressed.append(databytes[i])
                i += 1

    return compressed  # Return the compressed data instead of the original databytes

def frameblend(frames, f1_idx, f2_idx):
    global codelines
    char_change_values = []
    col_change_values = []
    col_change_mapped_values = []
    f1 = frames[f1_idx]
    f2 = frames[f2_idx]

    codelines.append("; Code to blend from frame " + str(f1_idx) + " to frame " + str(f2_idx))
    codelines.append(".ifndef SCREEN_BASE")
    codelines.append("  SCREEN_BASE=$400")
    codelines.append(".endif")
    codelines.append(".ifndef COLRAM_BASE")
    codelines.append("  COLRAM_BASE=$d800")
    codelines.append(".endif")
    codelines.append("")
    codelines.append("blend_" + str(f1_idx) + "to" + str(f2_idx) + ":")

    for i in range(25 * 40):
        if f1[2][i] != f2[2][i]:
            char_change_values.append((f2[2][i], i))
        if f2[2][i]!=32 and f2[2][i]!=96:
            if (f1[3][i] != f2[3][i]) or (f1[2][i]==32 or f1[2][i]==96):
                col_change_values.append((f2[3][i], i))

    # Sort by the value (first element of the tuple)
    char_change_values.sort(key=lambda x: x[0])
    col_change_values.sort(key=lambda x: x[0])

    # map colors to char values whenever possible
    for col_val, col_index in col_change_values[:]:
        for char_val, char_index in char_change_values:
            if col_val == (char_val & 0x0F):
                col_change_mapped_values.append((char_val, col_index))
                col_change_values.remove((col_val, col_index))
                break

    col_change_mapped_values.sort(key=lambda x: x[0])
    print (col_change_values)
    print (col_change_mapped_values)

    # Traverse through char_change_values from low to high
    current_x = -999
    for x_val in range(256):
        for char_val, index in char_change_values:
            if x_val==char_val:
                if x_val!=current_x:
                    if x_val==current_x+1:
                        codelines.append("        inx")
                    else:
                        codelines.append("        ldx #"+str(x_val))
                    current_x=x_val
                codelines.append("        stx SCREEN_BASE+"+str(index))
        #check for mapped color values
        for col_val, col_index in col_change_mapped_values:
            if x_val == col_val:
                codelines.append("        stx COLRAM_BASE+" + str(col_index))

        # Check if (current_x & 0x0F) is in col_change_values
        for col_val, col_index in col_change_values:  # Iterate over a copy of the list
            if x_val == col_val:
                if x_val!=current_x:
                    if x_val == current_x + 1:
                        codelines.append("        inx")
                    else:
                        codelines.append("        ldx #" + str(x_val))
                    current_x = x_val
                codelines.append("        stx COLRAM_BASE+" + str(col_index))
                # Remove the entry from col_change_values
                #col_change_values.remove((col_val, col_index))
    codelines.append("        rts")

def saveAsmPrg(filename):
    outfile = open(filename,"w")
    for l in codelines:
        outfile.write(l+"\n")
    outfile.close()

def saveAsmPrg(filename):
    outfile = open(filename, 'w')
    for l in codelines:
        outfile.write(l + '\n')
    outfile.close()

def loadPETSCII(filenames):
    all_frames = []
    for filename in filenames:
        frames = load_petscii_c(filename)
        if args.page and args.page > 0:
            all_frames.extend([frames[args.page - 1]])
        else:
            all_frames.extend(frames)
    return all_frames


def append_exomized_prg(prg_file, load_addr):
    global basic_prg

    tmpfile_name = tempfile.mktemp()
    command = (
    f"exomizer sfx sys "
    f"-s \"lda #22  sta 2021  lda #3  sta 2022  sta 2023  sta 56293  lda #1  sta 56294  sta 56295\" "
    f"-x \"dec 2  bne lbl00  lda 56293  pha  lda 56295  sta 56293  lda 56294  sta 56295  pla  sta 56294  lbl00:\" "
    f"-B -C -Di_load_addr={load_addr} {prg_file} -o {tmpfile_name}"
    )
    print (command)
    try:
        os.system(command)
    except Exception as e:
        print(f"Something went wrong while calling exomizer: {str(e)}")
        print("Is exomizer installed and in the PATH?")
        return

    with open(tmpfile_name, 'rb') as tmpfile_data:
        tmpfile_data.seek(2)
        data_to_append = tmpfile_data.read()

    basic_prg += list(data_to_append)
    os.remove(tmpfile_name)


def append_dalicompressed_prg(prg_file, load_addr):
    global basic_prg

    # Create the first temporary file with the correct 12-byte preamble
    tmpfile1 = tempfile.mktemp()
    with open(tmpfile1, 'wb') as f:
        # Write the code to start a BASIC program as preamble
        f.write(bytes([
            0xF4, 0x07,        # Load address 2036 (0x7F4)
            0x20, 0x44, 0xE5,  # JSR $E544 (clrscr)
            0x20, 0x33, 0xA5,  # JSR $A533 (relink)
            0x20, 0x59, 0xA6,  # JSR $A659 (CLR)
            0x4C, 0xAE, 0xA7,  # JMP $A7AE (RUN)
            0x00               # Null byte to have a valid BASIC program after this
        ]))

        # Append the PRG file (without the first two bytes)
        with open(prg_file, 'rb') as prg:
            prg.seek(2)  # Skip the first two bytes
            f.write(prg.read())

    # Create the second temporary file using dali for compression
    tmpfile2 = tempfile.mktemp()
    try:
        os.system(f"dali.exe --cli --relocate-sfx {load_addr} --sfx 2036 -o {tmpfile2} {tmpfile1}")
    except Exception as e:
        print(f"Something went wrong while calling dali: {str(e)}")
        print("Is dali.exe installed and in the PATH?")
        return

    # Read the compressed data from tmpfile2
    with open(tmpfile2, 'rb') as tmpfile_data:
        tmpfile_data.seek(2)  # Skip the first two bytes of the compressed file
        data_to_append = tmpfile_data.read()

    # Append the data to basic_prg
    basic_prg += list(data_to_append)

    # Clean up temporary files
    os.remove(tmpfile1)
    os.remove(tmpfile2)


class ShowVersionInfo(argparse.Action):
    def __call__(self, parser, namespace, values, option_string):
        print(VERSIONINFO)
        parser.exit() # exits the program with no more arg parsing and checkin

class StoreWithDefaultTracking(argparse.Action):
    def __init__(self, *args, **kwargs):
        self.is_explicit = False
        super().__init__(*args, **kwargs)

    def __call__(self, parser, namespace, values, option_string=None):
        self.is_explicit = True
        setattr(namespace, self.dest, values)

formats_str="BASIC, BASICSLIDES, BIN, DATA, LIST, LISTSYS, ESCAPEDSTRING, SEQ, ASM"
formats = [format.strip().lower() for format in formats_str.split(',')]

# Parse command-line arguments
parser = \
    argparse.ArgumentParser(description="Convert PETSCII images from Marq's PETSCII Editor to various C64 formats.")
parser.add_argument('filenames', nargs='+', help='Files to be converted.')
parser.add_argument('-o', '--outfile', help='save converted image' )
parser.add_argument("-n", "--newer", action="store_true", default=False, help="do not convert if a target newer than the source file exists")
parser.add_argument("-d", "--dir", action='store_true', help="for ASM output; add a directory of pointers at the start of the file")
parser.add_argument('-p', '--page', nargs='?', type=int, const=0,
                    help='select a specific page from the PETSCII source, otherwise all pages are converted. Page numbers start at 1')
parser.add_argument("-y", "--yheight", help="image height in lines, default=25", default=25)
parser.add_argument('--labelname', help='Set the base name for generated labels (default: petscii)', default='petscii')
parser.add_argument('-l', '--linenumber',
                    help='set starting linenumber for BASIC program, default=100',
                    default=100)
parser.add_argument('-i', '--increment',
                    help='set linenumber increment, default=5',
                    default=5)
parser.add_argument('--align', type=int, default=100,
                    help='Align each frame to a multiple of the given line number (e.g., 100, 1000)')
parser.add_argument('-f', '--format',
                    help='set output format: '+formats_str,
                    default='basicslides')
parser.add_argument('--ignorespacecolor', action='store_false', default=True,
                    help='Ignore color of whitespace characters (32 and 96) in ASM mode.')                    
parser.add_argument('--use-cruncher', nargs='?', const='exomizer', choices=['exomizer', 'dali'],
                    help="Select the cruncher (default: exomizer)")
parser.add_argument('--append-prg',
                    help=("Appends a crunched version of the PRG file. Only works with formats: LIST, LISTSYS. When using LISTSYS, the file is decrunched and run after running the output."),
                    metavar='PRG_FILE')
parser.add_argument('--case', choices=['upper', 'lower'], dest='case', default=None,
                    help='Set the case transformation: "upper" for all caps, "lower" for all lowercase. Default: no change.')
parser.add_argument('--frameblend', nargs=2, metavar=('from_frame', 'to_frame'), type=int,
                    help='Generate code for fast transition between two frames, specifying from_frame and to_frame')
parser.add_argument('-V', '--verbose', action='store_true', default=False,
                    help='Enable verbose output for detailed information on the working steps.')
parser.add_argument('-v', '--version', nargs=0, action=ShowVersionInfo,
                    help='display version info')

args = parser.parse_args()

targetformat=args.format.lower()

if not args.format:
    sys.stderr.write('Please specify the output format.\n')
    sys.exit(1)
if not targetformat in formats:
    sys.stderr.write('Unknown or unsupported format, please specify any of '+formats_str+'.\n')
    sys.exit(1)

labelname=args.labelname

if args.outfile:
    outfile = args.outfile
else:
    if args.frameblend:
        outfile = '.'.join(args.filenames[0].split('.')[:-1]) + '_' + str(args.frameblend[0]) + 'to'+ str(args.frameblend[1]) +'.asm'
    else:
        if targetformat == 'seq':
            outfile = 'screen.seq'
        elif targetformat=='asm':
            # Use the first filename as the base for the output
            outfile = '.'.join(args.filenames[0].split('.')[:-1]) + '.asm'
        elif targetformat == 'escapedstring':
            outfile = '.'.join(args.filenames[0].split('.')[:-1]) + '.txt'
        else:
            outfile = '.'.join(args.filenames[0].split('.')[:-1]) + '.prg'

if args.append_prg:
    if not targetformat in ['list','listsys']:
        sys.stderr.write('File appending is only possible for formats LIST and LISTSYS.\n')
        sys.exit(1)

linenr = int(args.linenumber)
lineInc = int(args.increment)
alignlines= int(args.align)

for filename in args.filenames:
    if not os.path.isfile(filename):
        sys.stderr.write(f"Source file {filename} not found!\n")
        sys.exit(1)

pic_height=int(args.yheight)

add_dir=args.dir

if args.newer:
    if os.path.isfile(outfile):
        # Check if any input file is newer than the output file
        trg_time = os.path.getmtime(outfile)
        src_newer = any(os.path.getmtime(filename) >= trg_time for filename in args.filenames)
        if not src_newer:
            sys.exit(0)

frames = loadPETSCII(args.filenames)


if args.frameblend:
    frameblend(frames,args.frameblend[0],args.frameblend[1])
    saveAsmPrg(outfile)
    print("File saved as "+outfile)
elif targetformat == 'basic':
    convertPETSCII2PRINT(frames, slidemode=False)
    savePrg(outfile)
    print("File saved as "+outfile)
elif targetformat == 'basicslides':
    convertPETSCII2PRINT(frames, slidemode=True)
    savePrg(outfile)
    print("File saved as "+outfile)
elif targetformat == 'bin':
    convertPETSCII2BIN(frames)
    saveBin(outfile)
    print("File saved as "+outfile)
elif targetformat == 'data':
    convertPETSCII2DATA(frames)
    savePrg(outfile)
    print("File saved as "+outfile)
elif targetformat == 'list':
    file_end_address = convertPETSCII2LIST(frames, False)
    if args.append_prg:
        if args.use_cruncher == 'dali':
            append_dalicompressed_prg(args.append_prg, file_end_address)
        else:
            append_exomized_prg(args.append_prg, file_end_address)
    savePrg(outfile)
    print("File saved as " + outfile)
elif targetformat == 'listsys':
    file_end_address = convertPETSCII2LIST(frames, True)
    if args.append_prg:
        if args.use_cruncher == 'dali':
            append_dalicompressed_prg(args.append_prg, file_end_address)
        else:
            append_exomized_prg(args.append_prg, file_end_address)
    savePrg(outfile)
    print("File saved as " + outfile)
elif targetformat == 'escapedstring':
    if args.outfile:
        with open(outfile, 'w') as file:
            sys.stdout=file
            convertPETSCII2ESCAPEDSTRING(frames)
            # Restore the original stdout after writing to the file
            sys.stdout = sys.__stdout__
            print("File saved as "+outfile)
    else:
        convertPETSCII2ESCAPEDSTRING(frames)
elif targetformat == 'seq':
    convertPETSCII2SEQ(frames, outfile)
    print("File saved as "+outfile)
elif targetformat=='asm':
    convertPETSCII2ASM(frames)
    saveAsmPrg(outfile)
    print("File saved as "+outfile)
