windowchrout
Prints characters that are output via $FFD2 into a textwindow

Jan-2022 V0.1
Wilfried Elmenreich
License: The Unlicense

Routine is almost twice as fast as the KERNAL routine

Note that control character keypresses in direct mode are not handled via FFD2, therefore pressing for example CLR/HOME will leave the window.

The page of the textscreen (stored in $288 / 648) is used to determine the output screen, but if you change the screen, the routine _enable_chrout2window needs to be called again.

limitations: no reverse mode, no backspace, no insert
