REM *********** LD *************
rmake e /DRECOMP /DC52
call clp522r ild
t:
cd \sigma
del p:\zipsig\fmk\LDR.zip
zip p:\zipsig\fmk\ldR ld.exe ild.exe s:*.chs s:s*.txt
s:
