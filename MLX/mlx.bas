100 poke 56,50:clr:dim IN$,I,J,A,B,A$,B$,A(7),N$
110 C4=48:C6=16:C7=7:Z2=2:Z4=254:Z5=255:Z6=256:Z7=127
120 FA=peek(45)+Z6*peek(46):BS=peek(55)+Z6*peek(56):H$="0123456789ABCDEF"
130 R$=chr$(13):L$="cmd":S$=" ":D$=chr$(20):Z$=chr$(0):T$=""
140 SD=54272:for I=SD to SD+23:poke I,0:next:poke SD+24,15:poke 788,52
150 print"load"chr$(142)chr$(8):poke 53280,15:poke 53281,15
160 print T$"   totototototototo  "spc(28)"  wait MLX II   "spc(28)"            "
170 print"   COMPUTE!'S MACHINE LANGUAGE EDITOR"
180 print"stopSTARTING ADDRESSpoke";:gosub300:SA=AD:gosub1040:if F then180
190 print"stop  ENDING ADDRESSpoke";:gosub300:EA=AD:gosub1030:if F then190
200 input"stopCLEAR WORKSPACE [Y/N]poke";A$:if left$(A$,1)<>"Y"then220
210 print"WORKING...";:forI=BS to BS+EA-SA+7:poke I,0:next:print"DONE"
220 printtab(10)"stop MLX COMMAND MENU poke":print T$"EwaitNTER DATA"
230 print T$"DwaitISPLAY DATA":print T$"LwaitOAD DATA"
240 print T$"SwaitAVE FILE":print T$"QwaitUITstop"
250 get A$:if A$=N$ then250
260 A=0:for I=1 to 5:if A$=mid$("EDLSQ",I,1)then A=I:I=5
270 next:on A goto420,610,690,700,280:gosub1060:goto250
280 print" QUIT ":input"pokeARE YOU SURE [Y/N]";A$:if left$(A$,1)<>"Y"then220
290 poke SD+24,0:end
300 IN$=N$:AD=0:inputIN$:iflen(IN$)<>4thenreturn
310 B$=IN$:gosub320:AD=A:B$=mid$(IN$,3):gosub320:AD=AD*256+A:return
320 A=0:for J=1 to 2:A$=mid$(B$,J,1):B=asc(A$)-C4+(A$>"@")*C7:A=A*C6+B
330 if B<0 or B>15 then AD=0:A=-1:J=2
340 next:return
350 B=int(A/C6):print mid$(H$,B+1,1);:B=A-B*C6:print mid$(H$,B+1,1);:return
360 A=int(AD/Z6):gosub350:A=AD-A*Z6:gosub350:print":";
370 CK=int(AD/Z6):CK=AD-Z4*CK+Z5*(CK>Z7):goto390
380 CK=CK*Z2+Z5*(CK>Z7)+A
390 CK=CK+Z5*(CK>Z5):return
400 print"STARTING ATpoke";:gosub300:if IN$<>N$ then gosub1030:if F then400
410 return
420 print" ENTER DATA ":gosub400:if IN$=N$ then220
430 open3,3:print
440 poke198,0:gosub360:if F then print IN$:print"on";
450 for I=0 to 24 step 3:B$=S$:for J=1 to 2:if F then B$=mid$(IN$,I+J,1)
460 print""B$L$;:if I<24then print"wait";
470 get A$:if A$=N$ then470
480 if(A$>"/"andA$<":")or(A$>"@"andA$<"G")then540
490 if A$=R$ and((I=0)and(J=1)or F)then print B$;:J=2:next:I=24:goto550
500 if A$="" then print B$:J=2:next:I=24:next:F=0:goto440
510 if(A$="")andF thenprint B$L$;:goto540
520 if A$<>L$ and A$<>D$ or((I=0)and(J=1))then gosub1060:goto470
530 A$=L$+S$+L$:print B$L$;:J=2-J:if J then print L$;:I=I-3
540 print A$;:next J:print S$;
550 next I:print:print"on";:input#3,IN$:if IN$=N$ then close3:goto220
560 for I=1 to 25 step3:B$=mid$(IN$,I):gosub320:if I<25 then gosub380:A(I/3)=A
570 next:if A<>CK then gosub1060:print"stop ERROR: REENTER LINE poke":F=1:goto440
580 gosub1080:B=BS+AD-SA:for I=0 to 7:poke B+I,A(I):next
590 AD=AD+8:if AD>EA then close3:print"** END OF ENTRY **stop":goto700
600 F=0:goto440
610 print"load DISPLAY DATA ":gosub400:if IN$=N$ then220
620 print"PRESS: SPACEwait TO PAUSE, RETURNwait TO BREAKpoke"
630 gosub360:B=BS+AD-SA:forI=Bto B+7:A=peek(I):gosub350:gosub380:print S$;
640 next:print"";:A=CK:gosub350:print
650 F=1:AD=AD+8:if AD>EA thenprint"** END OF DATA **":goto220
660 get A$:if A$=R$ then gosub1080:goto220
670 if A$=S$ then F=F+1:gosub1080
680 onFgoto630,660,630
690 print" LOAD DATA ":OP=1:goto710
700 print" SAVE FILE ":OP=0
710 IN$=N$:input"FILENAMEpoke";IN$:if IN$=N$ then220
720 F=0:print"stopTwaitAPE OR DwaitISK: poke";
730 get A$:if A$="T"then print"T":goto880
740 if A$<>"D"then730
750 print"D":open15,8,15,"I0:":B=EA-SA:IN$="0:"+IN$:if OP then810
760 open 1,8,8,IN$+",P,W":gosub860:if A then220
770 AH=int(SA/256):AL=SA-(AH*256):print#1,chr$(AL);chr$(AH);
780 for I=0 to B:print#1,chr$(peek(BS+I));:if ST then800
790 next:close1:close15:goto940
800 gosub1060:print"stopERROR DURING SAVE:poke":gosub860:goto220
810 open 1,8,8,IN$+",P,R":gosub860:if A then220
820 get#1,A$,B$:AD=asc(A$+Z$)+256*asc(B$+Z$):if AD<>SA then F=1:goto850
830 for I=0 to B:get#1,A$:poke BS+I,asc(A$+Z$):if ST and(I<>B)then F=2:AD=I:I=B
840 next:if ST<>64 then F=3
850 close1:close15:on abs(F>0)+1 goto960,970
860 input#15,A,A$:if A then close1:close15:gosub1060:print"ERROR: "A$
870 return
880 poke183,peek(FA+2):poke187,peek(FA+3):poke188,peek(FA+4):ifOP=0then920
890 sys 63466:if(peek(783)and1)then gosub1060:print" FILE NOT FOUND ":goto690
900 AD=peek(829)+256*peek(830):if AD<>SA then F=1:goto970
910 A=peek(831)+256*peek(832)-1:F=F-2*(A<EA)-3*(A>EA):AD=A-AD:goto930
920 A=SA:B=EA+1:gosub1010:poke780,3:sys 63338
930 A=BS:B=BS+(EA-SA)+1:gosub1010:on OP goto950:sys 63591
940 gosub1080:print"** SAVE COMPLETED **":goto220
950 poke147,0:sys 63562:if ST<>64 then970
960 gosub1080:print"** LOAD COMPLETED **":goto220
970 gosub1060:print"stopERROR DURING LOAD:poke":on F gosub980,990,1000:goto220
980 print"INCORRECT STARTING ADDRESS (";:gosub360:print")":return
990 print"LOAD ENDED AT ";:AD=SA+AD:gosub360:print D$:return
1000 print"TRUNCATED AT ENDING ADDRESS":return
1010 AH=int(A/256):AL=A-(AH*256):poke193,AL:poke194,AH
1020 AH=int(B/256):AL=B-(AH*256):poke174,AL:poke175,AH:return
1030 if AD<SA or AD>EA then1050
1040 if(AD>511 and AD<40960)or(AD>49151 and AD<53248)then gosub1080:F=0:return
1050 gosub1060:print" INVALID ADDRESS stop":F=1:return
1060 poke SD+5,31:poke SD+6,208:poke SD,240:poke SD+1,4:poke SD+4,33
1070 for S=1 to 100:next:goto1090
1080 poke SD+5,8:poke SD+6,240:poke SD,0:poke SD+1,90:poke SD+4,17
1090 for S=1 to 100:next:poke SD+4,0:poke SD,0:poke SD+1,0:return
