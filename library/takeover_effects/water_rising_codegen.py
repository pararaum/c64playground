#==========================================================
# Water Rising takeover effect
# Code generator for ECM rasterbars
#
# Code by Wil 2022
# Version 0.1
# License: The Unlicense
#==========================================================

colorsequence=[-1,-1,-1,-1,1,3,14,6,6,6,6,6,6,6]
defaultcols=[14,6]; #border color, background color

cycles=[0,1,2,3]

def getcol(cyc, line, background):
    if cyc>3:
        cyc=7-cyc
        
    if colorsequence[cyc+line]==-1:
        return defaultcols[background];
    return colorsequence[cyc+line]

def printpoke(addrstr,val):
    global A,X
    if val==A:
        print("sta "+addrstr)
        return
    if val==X:
        print("stx "+addrstr)
        return
    if val==(A & X):
        print("sax "+addrstr)
        return
    print("lda #"+str(val))
    A=val    
    print("sta "+addrstr)
    return        

# generates the code for one rasterline
def gen(line):
    global A,X
    cols=[getcol(cycles[0], line, 0)]
    for bgcols in range(4):
        newcol=getcol(cycles[bgcols], line, 1)
        if not newcol in cols:
            cols.append(newcol)
    print("lda #"+str(cols[0]))
    A=cols[0]
    if len(cols)>1:
        print("ldx #"+str(cols[1]))
        X=cols[1]
    print("do")
    print("  cpy $d012")
    print("loop while cs")
    printpoke("$d020",getcol(cycles[0], line,0))
    for bgcols in range(4):
        printpoke("$d02"+str(bgcols+1),getcol(cycles[bgcols], line,1))
    if (line<7):
        print("iny  ;increase Y to go to next rasterline")

for cyc in range(8):
    print("cycle"+str(cyc)+":")
    for bgcols in range(4):
        cycles[bgcols]=(cyc+bgcols) % 8    
    A=-1
    X=-1   #invalidate values of A and X    
    for line in range(8):
        gen(line)
    print("rts")
    print("\n")
    
