REM *********** LD *************
rmake e /DRECOMP /DC52 /DEXT
call clp522 ild
t:
cd \sigma
del p:\zipsig\fmk_0\LD.zip
zip p:\zipsig\fmk_0\ld ld.exe ild.exe c:*.chs c:s*.txt c:*.rtm
c:
