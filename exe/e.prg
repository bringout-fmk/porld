#include "porld.ch"

EXTERNAL RIGHT,LEFT,FIELDPOS

#ifdef LIB

function Main(cKorisn,cSifra,p3,p4,p5,p6,p7)
	MainLD(cKorisn,cSifra,p3,p4,p5,p6,p7)
return

#endif


function MainPorLD(cKorisn,cSifra,p3,p4,p5,p6,p7)

local oPorLD

oPorLD:=TPorModNew()
cModul:="PORLD"

PUBLIC goModul

goModul:=oPorLD
oPorLD:init(NIL, cModul, D_POR_VERZIJA, D_POR_PERIOD , cKorisn, cSifra, p3,p4,p5,p6,p7)

oPorLD:run()

return 


