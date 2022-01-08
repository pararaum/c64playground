windowchrout
Prints characters that are output via $FFD2 within a window

Wilfried Elmenreich
License: The Unlicense

Routine is almost twice as fast as the KERNAL routine

Note that control character keypresses in direct mode are not handled via FFD2, therefore pressing for example CLR/HOME will leave the window.

The page of the textscreen (stored in $288 / 648) is used to determine the output screen, but if you change the screen, the routine _enable_chrout2window needs to be called again.

limitations: no backspace, no insert

=== some test programs in BASIC ===

0 rem *** editor
1 sys49152
2 wait198,1:geta$:printa$;:goto1



0 rem *** performance test
1 gosub10
2 sys49152
10 ti$="000000"
11 printchr$(147)
12 fori=0to100:print"s123456789123456781234567812345678":next
13 printti/60
14 wait198,1:poke198,0:return


0 rem *** window size setting test
10 wb=49152+86
20 pokewb,10
30 pokewb+1,10
40 pokewb+2,30
50 pokewb+3,20
60 fori=1024toi+999:pokei,iand31:next
70 sys49152:rem enable window system
80 printchr$(147)
85 ti$="000000"
90 fori=0to100:?left$("hello world, what's up?",(iand15)+1)" ";:next
95 ?:?ti/60
100 wait198,1
110 sys49152+75:rem disable window system
