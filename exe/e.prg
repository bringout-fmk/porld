/* 
 * This file is part of the bring.out FMK, a free and open source 
 * accounting software suite,
 * Copyright (c) 1996-2011 by bring.out doo Sarajevo.
 * It is licensed to you under the Common Public Attribution License
 * version 1.0, the full text of which (including FMK specific Exhibits)
 * is available in the file LICENSE_CPAL_bring.out_FMK.md located at the 
 * root directory of this source code archive.
 * By using this software, you agree to be bound by its terms.
 */


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


