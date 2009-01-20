#include "porld.ch"

#define  RADNIK  radn->(padr(  trim(naz)+" ("+trim(imerod)+") "+ime,35))


function PregPrimPer()
local nC1:=20

cIdRadn:=space(6)
cIdRj:=gRj
cGodina:=gGodina
cObracun:=gObracun

O_RJ
O_RADN

 IF Pitanje(,"Izvjestaj se pravi za isplacene(D) ili neisplacene(N) radnike?","D")=="D"
   lIsplaceni:=.t.
   O_LD
 ELSE
   lIsplaceni:=.f.
   select (F_LDNO)  ; usex (KUMPATH+"LDNO") alias LD; set order to 1
 ENDIF

private cTip:="  "
cDod:="N"
cKolona:=space(20)
Box(,6,75)
cMjesecOd:=cMjesecDo:=gMjesec
@ m_x+1,m_y+2 SAY "Radna jedinica (prazno-sve): "  GET cIdRJ
@ m_x+2,m_y+2 SAY "Mjesec od: "  GET  cmjesecOd  pict "99"
@ m_x+2,col()+2 SAY "do" GET cMjesecDO  pict "99"
if lViseObr
  @ m_x+2,col()+2 SAY "Obracun:" GET cObracun WHEN HelpObr(.t.,cObracun) VALID ValObr(.t.,cObracun)
endif
@ m_x+3,m_y+2 SAY "Godina: "  GET  cGodina  pict "9999"
@ m_x+4,m_y+2 SAY "Tip primanja: "  GET  cTip
@ m_x+5,m_y+2 SAY "Prikaz dodatnu kolonu: "  GET  cDod pict "@!" valid cdod $ "DN"
read; clvbox(); ESC_BCR
if cDod=="D"
 @ m_x+6,m_y+2 SAY "Naziv kolone:" GET cKolona
 read
endif
fRacunaj:=.f.
if left(cKolona,1)="="
  fRacunaj:=.t.
  ckolona:=strtran(cKolona,"=","")
else
  ckolona:="radn->"+ckolona
endif
BoxC()

if lViseObr
  O_TIPPRN
else
  O_TIPPR
endif

select tippr
hseek ctip
EOF CRET

//"LDi4","str(godina)+idradn+str(mjesec)",KUMPATH+"LD")
select ld

if lViseObr .and. !EMPTY(cObracun)
  set filter to obr==cObracun
endif

set order to tag (TagVO("4"))
hseek str(cGodina,4)

EOF CRET

nStrana:=0
m:="----- ------ ---------------------------------- ------- ----------- -----------"
if cdod=="D"
 if type(ckolona) $ "UUIUE"
     Msg("Nepostojeca kolona")
     closeret
 endif
endif
bZagl:={|| ZPregPrimPer() }

select rj; hseek ld->idrj; select ld

START PRINT CRET
P_10CPI

Eval(bZagl)

nRbr:=0
nT1:=nT2:=nT3:=nT4:=0
nC1:=10

do while !eof() .and.  cgodina==godina
  if prow()>62; FF; Eval(bZagl); endif


  cIdRadn:=idradn
  select radn; hseek cidradn; select ld

  wi&cTip:=0
  ws&cTip:=0

  if fracunaj
      nKolona:=0
  endif
  do while  !eof() .and. cgodina==godina .and. idradn==cidradn
    Scatter()
    if !empty(cidrj) .and. _idrj<>cidrj
       skip; loop
    endif
    if cmjesecod>_mjesec .or. cmjesecdo<_mjesec
       skip; loop
    endif
    wi&cTip+=_i&cTip
    if ! ( lViseObr .and. EMPTY(cObracun) .and. _obr<>"1" )
      ws&cTip+=_s&cTip
    endif
    if fRacunaj
       nKolona+=&cKolona
    endif
    skip
  enddo

  if wi&cTip<>0 .or. ws&cTip<>0
     ? str(++nRbr,4)+".",cidradn, RADNIK
     nC1:=pcol()+1
     if tippr->fiksan=="P"
         @ prow(),pcol()+1 SAY ws&cTip  pict "999.99"
     else
         @ prow(),pcol()+1 SAY ws&cTip  pict gpics
     endif
     @ prow(),pcol()+1 SAY wi&cTip  pict gpici
     nT1+=ws&cTip; nT2+=wi&cTip
     if cdod=="D"
       if fracunaj
         @ prow(),pcol()+1 SAY nKolona pict gpici
       else
         @ prow(),pcol()+1 SAY &ckolona
       endif
     endif

  endif

 select ld
enddo

if prow()>60; FF; Eval(bZagl); endif
? m
? " UKUPNO:"
@ prow(),nC1 SAY  nT1 pict gpics
@ prow(),pcol()+1 SAY  nT2 pict gpici
? m
FF
END PRINT
CLOSERET
return


function ZPregPrimPer()

P_12CPI
? UPPER(TRIM(gTS))+":",gnFirma
?
? "Pregled primanja"+IF(lIsplaceni,""," -neisplaceni radnici-")+" za period od",cmjesecod,"do",cmjesecdo,"mjesec",cGodina
?
if empty(cidrj)
 ? "Pregled za sve RJ ukupno:"
else
 ? "RJ:",cidrj,rj->naz
endif
?? space(4),"Str.",str(++nStrana,3)
?
? "Pregled za tip primanja:",ctip,tippr->naz

? m
? " Rbr  Sifra           Naziv radnika               "+iif(tippr->fiksan=="P"," %  ","Sati")+"      Iznos"
? m


return


function SpecNovcanica()
local aLeg:={}
local aPom:={,,}

gnLMarg:=0; gTabela:=1; gOstr:="D"; cOdvLin:="D"; cVarSpec:="1"

cIdRj:=gRj; cmjesec:=gMjesec; cGodina:=gGodina; cObracun:=gObracun

nAp1  := 100; nAp2  :=  50; nAp3  :=  20; nAp4  :=  10; nAp5  :=   5
nAp6  :=   1; nAp7  := 0.5; nAp8  := 0.2; nAp9  := 0.1; nAp10 :=   0
nAp11 :=   0; nAp12 :=   0
cAp1:=cAp2:=cAp3:=cAp4:=cAp5:=cAp6:=cAp7:=cAp8:=cAp9:="D"
cAp10:=cAp11:=cAp12:="N"

O_KBENEF
O_VPOSLA
O_RJ
O_RADN

 IF Pitanje(,"Izvjestaj se pravi za isplacene(D) ili neisplacene(N) radnike?","D")=="D"
   lIsplaceni:=.t.
   O_LD
 ELSE
   lIsplaceni:=.f.
   select (F_LDNO)  ; usex (KUMPATH+"LDNO") alias LD; set order to 1
 ENDIF

O_PARAMS
Private cSection:="4",cHistory:=" ",aHistory:={}
RPar("t4",@gOstr); RPar("t5",@cOdvLin); RPar("t6",@gTabela)
RPar("u0",@cAp1) ; RPar("u1",@cAp2) ; RPar("u2",@cAp3)
RPar("u3",@cAp4) ; RPar("u4",@cAp5) ; RPar("u5",@cAp6)
RPar("u6",@cAp7) ; RPar("u7",@cAp8) ; RPar("u8",@cAp9)
RPar("u9",@cAp10); RPar("v0",@cAp11); RPar("v1",@cAp12)

RPar("v2",@nAp1) ; RPar("v3",@nAp2) ; RPar("v4",@nAp3)
RPar("v5",@nAp4) ; RPar("v6",@nAp5) ; RPar("v7",@nAp6)
RPar("v8",@nAp7) ; RPar("v9",@nAp8) ; RPar("z0",@nAp9)
RPar("z1",@nAp10); RPar("z2",@nAp11); RPar("z3",@nAp12)

Box(,19,75)
@ m_x+ 1,m_y+ 2 SAY "Radna jedinica (prazno-sve): "  GET cIdRJ
@ m_x+ 2,m_y+ 2 SAY "Mjesec: "  GET  cmjesec  pict "99"
if lViseObr
  @ m_x+ 2,col()+2 SAY "Obracun: " GET cObracun WHEN HelpObr(.t.,cObracun) VALID ValObr(.t.,cObracun)
endif
@ m_x+ 2,col()+2 SAY "Godina: "  GET  cGodina  pict "9999"
@ m_x+ 3,m_y+ 2 SAY "Varijanta (1-samo ukupno,2-po radnicima)"  GET cVarSpec VALID cVarSpec $ "12"
@ m_x+ 4,m_y+ 2 SAY "Nacin crtanja tabele     (0/1/2)   "  GET gTabela VALID gTabela>=0.and.gTabela<=2 pict "9"
@ m_x+ 5,m_y+ 2 SAY "Ukljuceno ostranicavanje (D/N) ?   "  GET gOstr   VALID gOstr$"DN"    pict "@!"
@ m_x+ 6,m_y+ 2 SAY "Odvajati podatke linijom (D/N) ?   "  GET cOdvLin VALID cOdvLin$"DN"  pict "@!"
@ m_x+ 8,m_y+ 2 SAY "Iznos apoena:" GET nAp1 PICT "9999.99"
@ m_x+ 8,m_y+32 SAY ", aktivan (D/N)" GET cAp1 VALID cAp1$"DN" PICT "@!"
@ m_x+ 9,m_y+ 2 SAY "Iznos apoena:" GET nAp2 PICT "9999.99"
@ m_x+ 9,m_y+32 SAY ", aktivan (D/N)" GET cAp2 VALID cAp2$"DN" PICT "@!"
@ m_x+10,m_y+ 2 SAY "Iznos apoena:" GET nAp3 PICT "9999.99"
@ m_x+10,m_y+32 SAY ", aktivan (D/N)" GET cAp3 VALID cAp3$"DN" PICT "@!"
@ m_x+11,m_y+ 2 SAY "Iznos apoena:" GET nAp4 PICT "9999.99"
@ m_x+11,m_y+32 SAY ", aktivan (D/N)" GET cAp4 VALID cAp4$"DN" PICT "@!"
@ m_x+12,m_y+ 2 SAY "Iznos apoena:" GET nAp5 PICT "9999.99"
@ m_x+12,m_y+32 SAY ", aktivan (D/N)" GET cAp5 VALID cAp5$"DN" PICT "@!"
@ m_x+13,m_y+ 2 SAY "Iznos apoena:" GET nAp6 PICT "9999.99"
@ m_x+13,m_y+32 SAY ", aktivan (D/N)" GET cAp6 VALID cAp6$"DN" PICT "@!"
@ m_x+14,m_y+ 2 SAY "Iznos apoena:" GET nAp7 PICT "9999.99"
@ m_x+14,m_y+32 SAY ", aktivan (D/N)" GET cAp7 VALID cAp7$"DN" PICT "@!"
@ m_x+15,m_y+ 2 SAY "Iznos apoena:" GET nAp8 PICT "9999.99"
@ m_x+15,m_y+32 SAY ", aktivan (D/N)" GET cAp8 VALID cAp8$"DN" PICT "@!"
@ m_x+16,m_y+ 2 SAY "Iznos apoena:" GET nAp9 PICT "9999.99"
@ m_x+16,m_y+32 SAY ", aktivan (D/N)" GET cAp9 VALID cAp9$"DN" PICT "@!"
@ m_x+17,m_y+ 2 SAY "Iznos apoena:" GET nAp10 PICT "9999.99"
@ m_x+17,m_y+32 SAY ", aktivan (D/N)" GET cAp10 VALID cAp10$"DN" PICT "@!"
@ m_x+18,m_y+ 2 SAY "Iznos apoena:" GET nAp11 PICT "9999.99"
@ m_x+18,m_y+32 SAY ", aktivan (D/N)" GET cAp11 VALID cAp11$"DN" PICT "@!"
@ m_x+19,m_y+ 2 SAY "Iznos apoena:" GET nAp12 PICT "9999.99"
@ m_x+19,m_y+32 SAY ", aktivan (D/N)" GET cAp12 VALID cAp12$"DN" PICT "@!"
read; clvbox(); ESC_BCR
BoxC()

WPar("t4",gOstr); WPar("t5",cOdvLin); WPar("t6",gTabela)
WPar("u0",cAp1) ; WPar("u1",cAp2) ; WPar("u2",cAp3)
WPar("u3",cAp4) ; WPar("u4",cAp5) ; WPar("u5",cAp6)
WPar("u6",cAp7) ; WPar("u7",cAp8) ; WPar("u8",cAp9)
WPar("u9",cAp10); WPar("v0",cAp11); WPar("v1",cAp12)

WPar("v2",nAp1) ; WPar("v3",nAp2) ; WPar("v4",nAp3)
WPar("v5",nAp4) ; WPar("v6",nAp5) ; WPar("v7",nAp6)
WPar("v8",nAp7) ; WPar("v9",nAp8) ; WPar("z0",nAp9)
WPar("z1",nAp10); WPar("z2",nAp11); WPar("z3",nAp12)
SELECT PARAMS; USE

if lViseObr
  O_TIPPRN
else
  O_TIPPR
endif

select ld


Box(,2,30)
  nSlog:=0; nUkupno:=RECCOUNT2()
  cSort1:="IDRADN"
  cFilt := IF(EMPTY(cIdRj),".t.","IDRJ==cIdRj")+".and."+;
           IF(EMPTY(cMjesec),".t.","MJESEC==cMjesec")+".and."+;
           IF(EMPTY(cGodina),".t.","GODINA==cGodina")
  if lViseObr .and. !EMPTY(cObracun)
    cFilt += (".and. OBR=="+cm2str(cObracun))
  endif
  INDEX ON &cSort1 TO "TMPLD" FOR &cFilt EVAL(TekRec2()) EVERY 1
BoxC()

EOF CRET
GO TOP
aKol:={}

IF cVarSpec=="2"

  AADD( aKol , { "SIFRA"                , {|| cIdRadn}, .f., "C", 13, 0, 1, 1} )

  AADD( aKol , { "PREZIME I IME RADNIKA", {|| cNaziv }, .f., "C", 27, 0, 1, 2} )

  aApoeni:={}; aApSort:={}; aNovc:={}; nKol:=2

  FOR i:=1 TO 12
    cPom:="cAp"+ALLTRIM(STR(i))
    nPom:="nAp"+ALLTRIM(STR(i))
    IF &cPom=="D" .and. ASCAN(aApoeni,&nPom)<=0
      AADD(aApoeni,&nPom)
      AADD(aNovc,0)
      AADD(aApSort,{&nPom,LEN(aApoeni)})
      bBlok:="{|| "+"aNovc["+ALLTRIM(STR(LEN(aApoeni)))+"] }"
      AADD( aKol, { "Apoen "+ALLTRIM(STR(&nPom)), &bBlok., .t., "N", 11, 0, 1, ++nKol} )
    ENDIF
  NEXT

  ASORT( aApSort ,,, { |x,y| x[1] > y[1] } )


  START PRINT CRET

  PRIVATE cIdRadn:="", cNaziv:=""

  ?? space(gnLMarg); ?? "LD: Izvjestaj na dan",date()
  ? space(gnLMarg); IspisFirme("")
  ?
  if empty(cidrj)
   ? "Pregled za sve RJ ukupno:"
  else
   ? "RJ:", cidrj+" - "+Ocitaj(F_RJ,cIdRj,"naz")
  endif
  ?? "  Mjesec:",IF(EMPTY(cMjesec),"SVI",str(cmjesec,2))+IspisObr()
  ?? "    Godina:", IF(EMPTY(cGodina),"SVE",str(cGodina,5))
  ?

  StampaTabele(aKol,{|| FSvaki5()},,gTabela,,;
       ,"Specifikacija novcanica "+IF(lIsplaceni,"potrebnih za isplatu plata","preostalih od neisplacenih plata"),;
                               {|| FFor5()},IF(gOstr=="D",,-1),,cOdvLin=="D",,,)

  ?
  FF
  END PRINT

ELSE    // cVarSpec=="1"

  aApoeni:={}; aNovc:={}

  FOR i:=1 TO 12
    cPom:="cAp"+ALLTRIM(STR(i))
    nPom:="nAp"+ALLTRIM(STR(i))
    IF &cPom=="D" .and. ASCAN(aApoeni,&nPom)<=0
      AADD(aApoeni,&nPom)
      AADD(aNovc,0)
    ENDIF
  NEXT

  DO WHILE !EOF()
    cIdRadn:=IDRADN
    nPom := 0
    DO WHILE !EOF() .and. cIdRadn==IDRADN
      nPom+=uiznos
      SKIP 1
    ENDDO

    FOR i:=1 TO LEN(aApoeni)
      IF STR(nPom,12,2) >= STR(aApoeni[i],12,2)
        nPom2 := INT(round(nPom,2)/round(aApoeni[i],2))
        aNovc[i] += nPom2
        nPom := nPom - nPom2 * aApoeni[i]
      ENDIF
    NEXT
  ENDDO

  nUkupno:=0
  START PRINT CRET
  ?? space(gnLMarg); ?? "LD: Izvjestaj na dan",date()
  ? space(gnLMarg); IspisFirme("")
  ?
  if empty(cidrj)
   ? "Pregled za sve RJ ukupno:"
  else
   ? "RJ:", cidrj+" - "+Ocitaj(F_RJ,cIdRj,"naz")
  endif
  ?? "  Mjesec:",IF(EMPTY(cMjesec),"SVI",str(cmjesec,2))+IspisObr()
  ?? "    Godina:", IF(EMPTY(cGodina),"SVE",str(cGodina,5))
  ?
  ? "------------------------------"
  ? "   SPECIFIKACIJA NOVCANICA"
  IF lIsplaceni
  ? "  POTREBNIH ZA ISPLATU PLATA"
  ELSE
  ? "PREOSTALIH OD NEISPLACEN.PLATA"
  ENDIF
  ? "------------------------------"
  ?

  m := REPL("-",10)+" "+REPL("-",6)+" "+REPL("-",12)
  ? m
  ? PADC("APOEN",10), PADC("BROJ",6), PADC("IZNOS",12)
  ? m
  FOR i:=1 TO LEN(aApoeni)
    ? PADC(ALLTRIM(STR(aApoeni[i])),10), PADC(ALLTRIM(STR(aNovc[i])),6), STR(aApoeni[i]*aNovc[i],12,2)
    nUkupno += ( aApoeni[i] * aNovc[i] )
  NEXT
  ? m
  ? PADR("UKUPNO:",18)+STR(nUkupno,12,2)
  ? m
  ?
  FF
  END PRINT

ENDIF

CLOSERET
return



static function FFor5()
 LOCAL nPom:=0,i:=0
 cIdRadn:=IDRADN
 cNaziv:=Ocitaj(F_RADN,cIdRadn,"TRIM(NAZ)+' '+TRIM(IME)")
 nPom := 0
 DO WHILE !EOF() .and. cIdRadn==IDRADN
   nPom+=uiznos
   SKIP 1
 ENDDO
 SKIP -1

 FOR i:=1 TO LEN(aApSort)
   IF STR(nPom,12,2) >= STR(aApSort[i,1],12,2)
     aNovc[aApSort[i,2]] := INT(round(nPom,2)/round(aApSort[i,1],2))
     nPom := nPom - aNovc[aApSort[i,2]] * aApSort[i,1]
   ELSE
     aNovc[aApSort[i,2]]:=0
   ENDIF
 NEXT
RETURN .t.



static function FSvaki5()
RETURN


// -----------------------------------
// IZvjestaj o OBracunatim DOPrinosima
// -----------------------------------
function IzObDop()
 cIdRj    := gRj
 cGodina  := gGodina
 cObracun := gObracun
 cMjesecOd:=cMjesecDo:=gMjesec
 cObracun:=" "
 cDopr   :="3X;"
 cNazDopr:="ZDRAVSTVENO OSIGURANJE"
 cPoOps:="S"

 O_PAROBR
 O_RJ
 O_OPS
 O_RADN
 O_LD
 O_POR
 O_DOPR

 O_PARAMS
 Private cSection:="5",cHistory:=" ",aHistory:={}

 cMjesecOd := STR(cMjesecOd,2)
 cMjesecDo := STR(cMjesecDo,2)
 cGodina   := STR(cGodina  ,4)

 RPar("p1",@cMjesecOd)
 RPar("p2",@cMjesecDo)
 RPar("p3",@cGodina  )
 RPar("p4",@cIdRj    )
 RPar("p5",@cDopr    )
 RPar("p6",@cNazDopr )
 RPar("p7",@cPoOps )

 cMjesecOd := VAL(cMjesecOd)
 cMjesecDo := VAL(cMjesecDo)
 cGodina   := VAL(cGodina  )
 cDopr     := PADR(cDopr,40)
 cNazDopr  := PADR(cNazDopr,40)

 Box("#Uslovi za izvjestaj o obracunatim doprinosima",8,75)
  @ m_x+2,m_y+2   SAY "Radna jedinica (prazno-sve): "   GET cIdRJ
  @ m_x+3,m_y+2   SAY "Mjesec od: "                     GET cMjesecOd PICT "99"
  @ m_x+3,col()+2 SAY "do"                              GET cMjesecDo PICT "99"
  @ m_x+4,m_y+2   SAY "Godina: "                        GET cGodina   PICT "9999"
  @ m_x+5,m_y+2   SAY "Doprinosi (npr. '3X;')"          GET cDopr PICT "@!"
  @ m_x+6,m_y+2   SAY "Obracunati doprinosi za (naziv)" GET cNazDopr PICT "@!"
  @ m_x+7,m_y+2   SAY "Po kantonu (S-stanovanja,R-rada)" GET cPoOps VALID cPoOps$"SR" PICT "@!"
  READ; ESC_BCR
 BoxC()

 cMjesecOd := STR(cMjesecOd,2)
 cMjesecDo := STR(cMjesecDo,2)
 cGodina   := STR(cGodina  ,4)
 cDopr     := TRIM(cDopr)
 cNazDopr  := TRIM(cNazDopr)

 WPar("p1",cMjesecOd)
 WPar("p2",cMjesecDo)
 WPar("p3",cGodina  )
 WPar("p4",cIdRj    )
 WPar("p5",cDopr    )
 WPar("p6",cNazDopr )
 WPar("p7",cPoOps )
 SELECT PARAMS; USE

 cMjesecOd := VAL(cMjesecOd)
 cMjesecDo := VAL(cMjesecDo)
 cGodina   := VAL(cGodina  )

 SELECT RADN
 IF cPoOps=="R"
   SET RELATION TO idopsrad INTO ops
 ELSE
   SET RELATION TO idopsst INTO ops
 ENDIF
 SELECT LD
 SET RELATION TO idradn INTO radn

 cSort := "OPS->idkan+SortPre2()+str(mjesec)"
 cFilt := "godina==cGodina .and. mjesec>=cMjesecOd .and. mjesec<=cMjesecDo"
 IF !EMPTY(cIdRj)
   cFilt += " .and. idrj=cIdRJ"
 ENDIF

 INDEX ON &cSort TO "TMPLD" FOR &cFilt

 GO TOP
 IF EOF(); MsgBeep("Nema podataka!"); CLOSERET; ENDIF

 START PRINT CRET
  gOstr:="D"; gTabela:=1
  cKanton:=cRadnik:=""; lSubTot7:=.f.; cSubTot7:=""

  aKol:={ { "PREZIME (IME RODITELJA) IME"  , {|| cRadnik   },.f., "C",32, 0, 1, 1} }

  nKol:=1
  FOR i:=cMjesecOd TO cMjesecDo
    cPom:="xneto"+ALLTRIM(STR(i))
    &cPom:=0
    AADD( aKol , { NazMjeseca(i), {|| &cPom. },.t., "N", 9, 2, 1, ++nKol} )
    cPom:="xdopr"+ALLTRIM(STR(i))
    &cPom:=0
    AADD( aKol , { "NETO/DOPR"  , {|| &cPom. },.t., "N", 9, 2, 2,   nKol} )
  NEXT

  xnetoUk:=xdoprUk:=0
  AADD( aKol , { "UKUPNO"     , {|| xnetoUk },.t., "N",10, 2, 1, ++nKol} )
  AADD( aKol , { "NETO/DOPR"  , {|| xdoprUk },.t., "N",10, 2, 2,   nKol} )

  P_10CPI
  ?? gnFirma
  ?
  ? "Mjesec: od", STR(cMjesecOd,2)+".", "do", str(cMjesecDo,2)+"."
  ?? "    Godina:",str(cGodina,4)
  ? "Obuhvacene radne jedinice: "; ?? IF(!EMPTY(cIdRJ),"'"+cIdRj+"'","SVE")
  ? "Obuhvaceni doprinosi (sifre):", "'" + cDopr + "'"
  ?

  SELECT LD

  StampaTabele(aKol,{|| FSvaki7()},,gTabela,,;
       ,"IZVJESTAJ O OBRACUNATIM DOPRINOSIMA ZA "+cNazDopr,;
                               {|| FFor7()},IF(gOstr=="D",,-1),,,{|| SubTot7()},,)
  FF

 END PRINT
CLOSERET
return



static function FFor7()
 IF OPS->idkan <> cKanton .and. LEN(cKanton)>0
   lSubTot7:=.t.
   cSubTot7:=cKanton
 ENDIF
 cKanton:=OPS->idkan
 xNetoUk:=xDoprUk:=0
 cRadnik := RADN->(padr(  trim(naz)+" ("+trim(imerod)+") "+ime,32))
 cIdRadn := IDRADN
 FOR i:=cMjesecOd TO cMjesecDo
   cPom:="xneto"+ALLTRIM(STR(i)); &cPom:=0
   cPom:="xdopr"+ALLTRIM(STR(i)); &cPom:=0
 NEXT
 DO WHILE !EOF() .and. OPS->idkan==cKanton .and. IDRADN==cIdRadn
   nTekMjes:=mjesec
   _uneto:=0
   DO WHILE !EOF() .and. OPS->idkan==cKanton .and. IDRADN==cIdRadn .and. mjesec==nTekMjes
     _uneto += uneto
     SKIP 1
   ENDDO
   SKIP -1
   // neto
   cPom    := "xneto"+ALLTRIM(STR(mjesec))
   &cPom   := _uneto
   xnetoUk += _uneto
   // doprinos
   PoDoIzSez(godina,mjesec)
   nDopr   := IzracDopr(cDopr)
   cPom    := "xdopr"+ALLTRIM(STR(mjesec))
   &cPom   := nDopr
   xdoprUk += nDopr
   SKIP 1
 ENDDO
 SKIP -1
RETURN .t.


static function FSvaki7()
return


static function SubTot7()
 LOCAL aVrati:={.f.,""}
  IF lSubTot7 .or. EOF()
    aVrati := { .t. , "UKUPNO KANTON '"+IF(EOF(),cKanton,cSubTot7)+"'" }
    lSubTot7:=.f.
  ENDIF
return aVrati


function IzracDopr(cDopr)
 LOCAL nArr:=SELECT(), nDopr:=0, nPom:=0, nPom2:=0, nPom0:=0, nBO:=0
  ParObr(mjesec,IF(lViseObr,cObracun,),cIdRj)
  nBo:=round2(parobr->k3/100*MAX(_UNeto,PAROBR->prosld*gPDLimit/100),gZaok2)
  SELECT DOPR; GO TOP
  DO WHILE !EOF()  // doprinosi
   IF !(id $ cDopr); SKIP 1; LOOP; ENDIF
   PozicOps(DOPR->poopst)   // ? mozda ovo rusi koncepciju zbog sorta na LD-u
   IF !ImaUOp("DOPR",DOPR->id)
     SKIP 1; LOOP
   ENDIF
   // if right(id,1)<>"X"
   //   SKIP 1; LOOP
   // endif
   nPom:=max(dlimit,round(iznos/100*nBO,gZaok2))
   if round(iznos,4)=0 .and. dlimit>0  // fuell boss
     nPom:=1*dlimit   // kartica plate
   endif
   nDopr+=nPom
   SKIP 1
  ENDDO // doprinosi
  SELECT (nArr)
return (nDopr)


function SortPre2()
return (BHSORT(RADN->(naz+ime+imerod))+idradn)



