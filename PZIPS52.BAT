REM *********** LD *************
rmake e /DRECOMP /DC52 /DCPOR  /DEXT
call clp522 ild  /DCPOR
t:
cd \sigma
del p:\zipsig\fmk\PORLD.zip
zip p:\zipsig\fmk\PORld ld.exe ild.exe c:*.chs c:s*.txt c:r*.txt c:dok*.txt
c:
