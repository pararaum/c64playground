@echo off
setlocal enabledelayedexpansion

set LINE_NUM=0
set OUTPUT_FILE=all_greetings_exo.inc
set OUTPUT_TMP_FILE=all_exo_tmp.inc

REM Clear the contents of all_exo.inc or create it if it doesn't exist
rm -f %OUTPUT_TMP_FILE%

for /f "usebackq delims=" %%a in ("greetings_list.txt") do (
    set /a LINE_NUM+=1
    
    REM Pad line number to 2 digits
    set "PADDED=0!LINE_NUM!"
    set "PADDED=!PADDED:~-2!"
    
    echo Processing line !LINE_NUM!: %%a
    
    REM Run text-scaler.py
    python text-scaler.py --trapezoid 176:192 -r -t "%%a" -o !PADDED!.bin --scalingfactor 0.9 --nsprites 8 --iterations 14 --scale smart --align center
    
    REM Combine with e000.bin
    copy /b e000.bin+!PADDED!.bin tmp.prg
    
    REM Run exomizer
    exomizer level -B -f tmp.prg -o !PADDED!.exo
    
    REM Add the entry to all_exo.inc
    echo greeting!PADDED!_exo:   .incbin "!PADDED!.exo" >> "%OUTPUT_TMP_FILE%"
)

echo N_CREDITS=!LINE_NUM! > %OUTPUT_FILE%
echo. >> %OUTPUT_FILE%
cat %OUTPUT_TMP_FILE% >> %OUTPUT_FILE%
rm -f %OUTPUT_TMP_FILE%

echo Done! Processed !LINE_NUM! lines.
del tmp.prg
