#include "porld.ch"



// ----------------------------------------
// vraca bruto osnovu
// nIzn - ugovoreni neto iznos
// cTipRada - vrsta/tip rada
// nLOdb - iznos licnog odbitka
// nSKoef - koeficijent kod samostalnih poslodavaca
// ----------------------------------------
function bruto_osn( nIzn, cTipRada, nLOdb, nSKoef )
local nBrt := 0

if nLOdb = nil
	nLOdb := 0
endif

if nSKoef = nil
	nSKoef := 0
endif

// stari obracun
if gVarObracun <> "2"
	nBrt := ROUND2( nIzn * ( parobr->k3 / 100 ), gZaok2 )
	return nBrt
endif

do case
	// nesamostalni rad
	case EMPTY(cTipRada)
		nBrt := ROUND2( nIzn * parobr->k5 ,gZaok2 )
	
	// nesamostalni rad, isti neto
	case cTipRada == "I"
		nOsn := nIzn
		
		// ako je iznos manji od minimalca, uzmi minimalac
		if ( nIzn < parobr->minld )
			nOsn := parobr->minld
		endif
		
		// ako je ugovoreni iznos manji od odbitka
		if (nIzn < nLOdb ) .or. radn->opor == "N" 
			nBrt := ROUND2( nOsn * parobr->k6, gZaok2 )
		else
			nBrt := ROUND2( ( (nOsn - nLOdb) / 0.9 + nLOdb ) ;
				/ 0.69  ,gZaok2)
		endif

	// samostalni poslodavci
	case cTipRada == "S"
		nBrt := ROUND2( nIzn * nSKoef ,gZaok2 )
	// nerezidenti
	case cTipRada == "N"
		nBrt := ROUND2( nIzn * parobr->k5 , gZaok2 )

endcase

return nBrt


// ----------------------------------------
// ispisuje bruto obracun
// ----------------------------------------
function bruto_isp( nNeto, cTipRada, nLOdb, nSKoef )
local cPrn := ""
local nBrt := 0

if nLOdb = nil
	nLOdb := 0
endif

if nSKoef = nil
	nSKoef := 0
endif

// izracunaj bruto
nBrt := bruto_osn( nNeto, cTipRada, nLOdb, nSKoef )

if gVarObracun <> "2"
	// stari obracun
	cPrn := ALLTRIM(STR(nNeto)) + " * " + ;
		ALLTRIM(STR( parobr->k3 / 100 )) + " = " + ;
		ALLTRIM(STR( nBrt ))
	return cPrn
endif

do case
	// nesamostalni rad
	case EMPTY(cTipRada)
		cPrn := ALLTRIM(STR(nNeto)) + " * " + ;
			ALLTRIM(STR(parobr->k5)) + " = "
		cPrn += ALLTRIM(STR(nBrt))
	// nesamostalni rad - isti neto
	case cTipRada == "I"
		
		nOsn := nNeto
		
		if ( nNeto < parobr->minld ) 
			nOsn := parobr->minld
		endif

		cPrn := ALLTRIM(STR(nOsn)) + " - " + ALLTRIM(STR(nLOdb)) + ;
			" / 0.9 + " + ALLTRIM(STR(nLOdb)) + " / 0.69 = "
		if ( nNeto < nLOdb ) .or. radn->opor == "N" 
			cPrn := ALLTRIM(STR(nOsn)) + " * " + ;
				ALLTRIM(STR(parobr->k6)) + " = "

		endif

		cPrn += ALLTRIM(STR(nBrt))

	// samostalni poslodavci
	case cTipRada == "S"
		cPrn := ALLTRIM(STR(nNeto)) + " * " + ;
			ALLTRIM(STR(nSKoef)) + " ="
	
		cPrn += ALLTRIM(STR(nBrt))
	// nerezidenti
	case cTipRada == "N"
		cPrn := ALLTRIM(STR(nNeto)) + " * " + ;
			ALLTRIM(STR(parobr->k5)) + " ="

		cPrn += ALLTRIM(STR(nBrt))
endcase

return cPrn





function VisePuta()

cMjesec  := gMjesec
cGodina  := gGodina
cObracun := gObracun

private cKBenef:=" ",cVPosla:=" "

Box(,2,50)
 @ m_x+1, m_y+2 SAY "Mjesec: "  GET  cMjesec  pict "99"
 @ m_x+2, m_y+2 SAY "Godina: "  GET  cGodina  pict "9999"
 read; ESC_BCR
BoxC()

// CREATE_INDEX("LDi2","str(godina)+str(mjesec)+idradn","LD")
// ----------- removao ovu liniju 21.11.2000. MS ------------

O_LD
set order to tag "2"

seek str(cgodina,4)+str(cmjesec,2)
start print cret
? "Radnici obradjeni vise puta za isti mjesec -",cgodina,"/",cmjesec
?
? "RADNIK RJ     neto        sati"
? "------ -- ------------- ----------"
do while !eof() .and. str(cgodina,4)+str(cmjesec,2)==str(godina)+str(mjesec)
  cIdRadn:=idradn
  nProlaz:=0
  do while !eof() .and. str(godina)+str(mjesec)==str(godina)+str(mjesec) .and. idradn==cidradn
     ++nProlaz
     skip
  enddo
  if nProlaz>1
     seek str(cgodina,4)+str(cmjesec,2)+cidradn
     do while !eof() .and. str(godina)+str(mjesec)==str(cgodina,4)+str(cmjesec,2) .and. idradn==cidradn
        ? idradn,idrj,uneto,usati
        skip
     enddo
  endif

enddo
end print
closeret

function ClVBox()
 LOCAL i:=0
 FOR i:=1 TO gnHelpObr
   BoxC()
 NEXT
 gnHelpObr:=0
return



function OKumul(nArea,cStaza,cIme,nIndexa,cDefault)
local cPath,cScreen

if cDefault==NIL
  cDefault:="0"
endif

select (nArea)

if used()
	return
endif

if gKesiraj $ "CD"
  cPath:=strtran(cStaza,LEFT(cStaza,3),gKesiraj+":\")

  DirMak2(cPath)  // napravi odrediçni direktorij

  if cDefault!="0"
    if !file( cPath+cIme+".DBF") .or. Pitanje(,"Osvjeziti podatke za "+cIme, cDefault )=="D"
     save screen to cScr
     cls
     ? "Molim sacekajte prenos podataka na vas racunar "
     ? "radi brzeg pregleda podataka"
     ?
     ? "Ovaj racunar NE KORISTITE za unos novih podataka !"
     ?
     close all
     Copysve(cIme+"*.DB?",cStaza,cPath)
     Copysve(cIme+"*.CDX",cStaza,cPath)
     ?
     ? "pritisni nesto za nastavak ..."
     inkey(10)
     restore screen from cScr
   endif
  endif

else
  cPath:=cStaza
endif
cPath:=cPath+cIme
use  (cPath)
return nil


function LDPoljaINI()
 if !FILE(KUMPATH + "LD.DBF")
 	public cLdPolja := 40
	return 
 endif
 O_LD
 if ld->(fieldpos("S40"))<>0
   public cLDPolja:=40
 elseif ld->(fieldpos("S30"))<>0
   public cLDPolja:=30
 else
   public cLDPolja:=14
 endif
 if ld->(fieldpos("OBR"))<>0
   public lViseObr:=.t.
 else
   public lViseObr:=.f.
 endif
 use
return


function HelpObr(lIzv,cObracun)
  IF lIzv==NIL; lIzv:=.f.; ENDIF
  IF gnHelpObr=0
    Box(,3+IF(lIzv,1,0),40)
    @ m_x+0, m_y+2 SAY PADC(" POMOC: ",36,"Í")
    IF lIzv
      @ m_x+2, m_y+2 SAY "Ukucajte broj obracuna (1/2/.../9)"
      @ m_x+3, m_y+2 SAY "ili prazno ako zelite sve obracune"
    ELSE
      @ m_x+2, m_y+2 SAY "Ukucajte broj obracuna (1/2/.../9)"
    ENDIF
    ++gnHelpObr
  ENDIF
return .t.


function ValObr(lIzv,cObracun)
  LOCAL lVrati:=.t.
  IF lIzv==NIL; lIzv:=.f.; ENDIF
  IF lIzv
    lVrati := ( cObracun $ " 123456789" )
  ELSE
    lVrati := ( cObracun $ "123456789" )
  ENDIF
  IF gnHelpObr>0 .and. lVrati
    BoxC()
    --gnHelpObr
  ENDIF
return lVrati



function TestViseObr()

  IF !FILE(KUMPATH+'LD.DBF')
    lViseObr:=.f.
    lVOBrisiCDX := .f.
    RETURN
  ELSE
    select (F_LD); use (KUMPATH+"LD")
  ENDIF
  IF FIELDPOS("OBR")<>0
    lViseObr:=.t.
  ELSE
    lViseObr:=.f.
  ENDIF
  IF lViseObr .and. ! ( "OBR" $ UPPER(INDEXKEY(3)) )
    lVOBrisiCDX := .t.
    IF Pitanje(,"Polje obr=' ' u LD.DBF zamijeniti sa '1' ? (D/N)","N") == "D"
      GO TOP
      DO WHILE !EOF()
        IF EMPTY(obr)
          Scatter(); _obr:="1"; Gather()
        ENDIF
        SKIP 1
      ENDDO
    ENDIF
  ELSE
    lVOBrisiCDX := .f.
  ENDIF
  USE
RETURN



function V_FSpec()
private cKom:="q "+PRIVPATH+gFSpec
if Pitanje(,"Zelite li izvrsiti ispravku fajla obrasca specifikacije ?","N")=="D"
 if !empty(gFSpec)
   Box(,25,80)
   run &ckom
   BoxC()
 endif
endif
return .t.



function V_FRjes(cVarijanta)
private cKom:="q "+PRIVPATH
if cVarijanta>"4"
 cKom+="dokaz"
else
 cKom+="rjes"
endif
if cvarijanta=="5"
 cKom+="1"
elseif cvarijanta=="6"
 cKom+="2"
else
 cKom+=cVarijanta
endif
cKom+=".txt"

if Pitanje(,"Zelite li izvrsiti ispravku fajla obrasca rjesenja ?","N")=="D"
 if !empty(gFSpec)
   Box(,25,80)
   run &ckom
   BoxC()
 endif
endif
return .t.


