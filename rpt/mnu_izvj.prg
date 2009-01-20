#include "porld.ch"

// ----------------------------------------------------
// osnovna funkcija za poziv izvjestaja - menij
// ----------------------------------------------------
function MnuIzvj()

private opc:={}
private opcexe:={}
private Izbor:=1

AADD(opc,"1. zahtjevi                                  ")
AADD(opcexe,{|| MnuIzvZ()})
AADD(opc,"2. kartice ")
AADD(opcexe,{|| MnuIzvK()})
AADD(opc,"3. rekapitulacije ")
AADD(opcexe,{|| MnuIzvR()})
AADD(opc,"4. specifikacije ")
AADD(opcexe,{|| MnuIzvS()})
AADD(opc,"5. izvjestaj o izvrsenoj isplati")
AADD(opcexe,{|| PregPl("2")})
AADD(opc,"6. pregled primanja za period ")
AADD(opcexe,{|| PregPrimPer()})
AADD(opc,"7. pregled odredjenog primanja ")
AADD(opcexe,{|| PregPrim()})
AADD(opc,"8. platni spisak ")
AADD(opcexe,{|| PlatSp()})

Menu_SC("izvj")

return



// ----------------------------------------
// izvjestaji zahtjevi
// ----------------------------------------
function MnuIzvZ()
private opc:={}
private opcexe:={}
private Izbor:=1

AADD(opc,"1. zahtjev za doznacivanje novcanih sredstava v.1")
AADD(opcexe,{|| PregPl("3")})
AADD(opc,"2. zahtjev za doznacivanje novcanih sredstava v.2")
AADD(opcexe,{|| PregPl()})

Menu_SC("zaht")

return

// ----------------------------------------
// izvjestaji kartice
// ----------------------------------------
function MnuIzvK()
private opc:={}
private opcexe:={}
private Izbor:=1

AADD(opc,"1. kartice plate                      ")
AADD(opcexe,{|| KartPl()})
AADD(opc,"2. kartica plate za period (za m4)")
AADD(opcexe,{|| UKartPl()})

Menu_SC("kart")
return

// -----------------------------------------
// menij - izvjestaji specifikacije
// -----------------------------------------
function MnuIzvS()
private opc:={}
private opcexe:={}
private Izbor:=1

AADD(opc,"1. specif.novcanica potrebnih za isplatu naknada ")
AADD(opcexe,{|| SpecNovcanica()})
AADD(opc,"2. specifikacija primanja po mjesecima")
AADD(opcexe,{|| SpecifPoMjes()})

Menu_SC("spec")
return


// ----------------------------------------
// menij specifikacije
// ----------------------------------------
function MnuIzvR()
private opc:={}
private opcexe:={}
private Izbor:=1

AADD(opc,"1. rekapitulacija                         ")
AADD(opcexe,{|| Rekap(.f.)})
AADD(opc,"2. rekapitulacija za sve rj")
AADD(opcexe,{|| Rekap(.t.)})
AADD(opc,"3. rekapitulacija neto primanja")
AADD(opcexe,{|| RekNeto()})

Menu_SC("rekap")
return



