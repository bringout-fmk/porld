REM *********** LD *************
rmake e0 /DRECOMP /dC50
call clp2 ild
t:
cd \sigma
del p:\zipsig\fmk\LD.zip
zip p:\zipsig\fmk\ld  ld.exe ild.exe  s:*.chs s:s*.txt
s:
