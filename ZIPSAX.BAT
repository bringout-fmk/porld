REM *********** LD *************
rmake e /DRECOMP /DCAX
call clpax2 ild
t:
cd \sigma
del p:\zipsig\fmk\LDAX.zip
zip p:\zipsig\fmk\ldAX  ld.exe ild.exe  c:*.chs c:s*.txt c:*.rtm
c:
