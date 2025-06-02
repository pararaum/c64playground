# Makefile

# Compiler and tools

AS = ca65 -t c64 -I../../library
CC = cl65 -t c64
ASS = ass
CL65 = cl65
EXOMIZER = exomizer
X64 = x64sc

# Source files
PETSCII_SRC = vcc32.petscii.c
PETSCII_ASM = vcc32.petscii.s
PETSCII_VCC_SRCS = vcc01.petscii.c vcc11.petscii.c vcc21.petscii.c vcc31.petscii.c 
PETSCII_VCC_ASM = vcc-petsciis.s

#targets
MAIN_PRG= vcc32.prg
SCROLLTEXT_PRG = scrolltext.prg
OUTPUT_PRG = vcc32-demo.prg
DAUMENKINO_PRG = vcc-daumenkino.prg
PART3 = part-rock-n-scroll_4400.prg

# Check OS
ifeq ($(OS),Windows_NT)
    PYTHON = python
    ifneq ($(shell where rm 2>nul),)
        RM = rm -f
    else
        RM = del /q
    endif
else
    PYTHON = python3
    RM = rm -f
endif

.PHONY: all run clean part-rock-n-scroll_minimal release

all: $(OUTPUT_PRG)

$(PART3): rocknroll.o moiety.rocknroll.o rock-n-roll.petscii.o
	cl65 -m /dev/stdout -C c64-asm.cfg --start-addr 0x4400 -o $@ $+ ../../library/libT7D.lib

rocknroll.prg:	rocknroll.o moiety.rocknroll.o rock-n-roll.petscii.o
	cl65 -m /dev/stdout -C c64-asm.cfg --start-addr 0x4400 -o $@.tmp $+ ../../library/libT7D.lib
	prepender64 -j 0x4400 --copy-eor -e 0x32 -o $@.tmp $@.tmp birthday-song.prg
	pucrunch $@.tmp $@

$(PETSCII_ASM): $(PETSCII_SRC)
	$(PYTHON) petscii2x.py -f ASM -p 0 -o $@ $^

$(PETSCII_VCC_ASM): $(PETSCII_VCC_SRCS)
	$(PYTHON) petscii2x.py -d -f ASM -p 0 -o $@ $^

$(DAUMENKINO_PRG): $(basename $(DAUMENKINO_PRG)).s $(PETSCII_VCC_ASM)
	$(ASS) $(basename $(DAUMENKINO_PRG)).s 0x2800

$(SCROLLTEXT_PRG): $(basename $(SCROLLTEXT_PRG)).s
	$(CL65) $< -C c64-asm.cfg --start-addr 0x1710 -Ln scrolltext-labels.txt -o $@

$(MAIN_PRG): $(basename $(MAIN_PRG)).s  $(PETSCII_ASM)
	$(ASS)  $(basename $(MAIN_PRG)).s 

$(OUTPUT_PRG): $(MAIN_PRG) scrolltext.prg $(DAUMENKINO_PRG) $(PART3)
	$(EXOMIZER) sfx sys -C -q vcc32.prg birthday-song.prg scrolltext.prg $(DAUMENKINO_PRG) $(PART3) -o $@

release: $(OUTPUT_PRG)
	cld vcc32.prg birthday-song.prg scrolltext.prg $(DAUMENKINO_PRG) $(PART3) -o tmp.prg
	$(PYTHON) petscii2x.py -f listsys -y 16 --append-prg tmp.prg -o $@.prg listart.c
	$(RM) tmp.prg

run: $(OUTPUT_PRG)
	$(X64) $(OUTPUT_PRG)

clean:
	$(RM) $(PETSCII_ASM) $(PETSCII_VCC_ASM) $(SCROLLTEXT_PRG) $(MAIN_PRG) $(DAUMENKINO_PRG) $(OUTPUT_PRG) *labels.txt
	$(RM) $(PART3)
	$(RM) *.o *.tmp
