#include "porld.ch"


FUNCTION PRNKod_ON(cKod)
 LOCAL i:=0
  FOR i:=1 TO LEN(cKod)
    DO CASE
      CASE SUBSTR(cKod,i,1)=="U"
         gPU_ON()
      CASE SUBSTR(cKod,i,1)=="I"
         gPI_ON()
      CASE SUBSTR(cKod,i,1)=="B"
         gPB_ON()
    ENDCASE
  NEXT
RETURN (NIL)


FUNCTION PRNKod_OFF(cKod)
 LOCAL i:=0
  FOR i:=1 TO LEN(cKod)
    DO CASE
      CASE SUBSTR(cKod,i,1)=="U"
         gPU_OFF()
      CASE SUBSTR(cKod,i,1)=="I"
         gPI_OFF()
      CASE SUBSTR(cKod,i,1)=="B"
         gPB_OFF()
    ENDCASE
  NEXT
RETURN (NIL)

function TekRec2()
 nSlog++
 @ m_x+1, m_y+2 SAY PADC(ALLTRIM(STR(nSlog))+"/"+ALLTRIM(STR(nUkupno)),20)
 @ m_x+2, m_y+2 SAY "Obuhvaceno: "+STR(cmxKeysIncluded())
return (nil)

function NazMjeseca(nMjesec)
 LOCAL aVrati:={"Januar","Februar","Mart","April","Maj","Juni","Juli",;
                "Avgust","Septembar","Oktobar","Novembar","Decembar","UKUPNO"}
return if( nMjesec>0.and.nMjesec<14 , aVrati[nMjesec] , "" )


function SpecifPoMjes()
gnLMarg:=0; gTabela:=1; gOstr:="N"

cIdRj:=gRj
cGodina:=gGodina
cIdRadn:=SPACE(6)
cSvaPrim:="S"
qqOstPrim:=""
cSamoAktivna:="D"

 IF Pitanje(,"Izvjestaj se pravi za isplacene(D) ili neisplacene(N) radnike?","D")=="D"
   lIsplaceni:=.t.
   O_LD
 ELSE
   lIsplaceni:=.f.
   select (F_LDNO)  ; usex (KUMPATH+"LDNO") alias LD; set order to 1
 ENDIF

PraviMTEMP()

// copy structure extended to struct
// USE
// create MTEMP from struct
CLOSE ALL

O_RJ
O_STRSPR
O_OPS
O_RADN

 IF lIsplaceni
   O_LD
 ELSE
   select (F_LDNO)  ; usex (KUMPATH+"LDNO") alias LD; set order to 1
 ENDIF


// ----------------------------
O_PARAMS
Private cSection:="6",cHistory:=" ",aHistory:={}
RPar("p1",@cIdRadn)
RPar("p2",@cSvaPrim)
RPar("p3",@qqOstPrim)
RPar("p4",@cSamoAktivna)
qqOstPrim:=PADR(qqOstPrim,100)
// ----------------------------

cPrikKolUk:="D"

Box(,7,77)

@ m_x+1,m_y+2 SAY "Radna jedinica (prazno sve): "  GET cIdRJ
@ m_x+2,m_y+2 SAY "Godina: "  GET  cGodina  pict "9999"
@ m_x+3,m_y+2 SAY "Radnik (prazno-svi radnici): "  GET  cIdRadn  valid empty(cIdRadn) .or. P_Radn(@cIdRadn)
@ m_x+7,m_y+2 SAY "Prikazati kolonu 'ukupno' ? (D/N)" GET cPrikKolUk PICT "@!" VALID cPrikKolUk$"DN"

read; ESC_BCR

BoxC()

// ----------------------------
qqOstPrim:=TRIM(qqOstPrim)
WPar("p1",cIdRadn)
WPar("p2",cSvaPrim)
WPar("p3",qqOstPrim)
WPar("p4",cSamoAktivna)
SELECT PARAMS; USE
// ----------------------------

SELECT 0
usex (PRIVPATH+"MTEMP")

// USEX MTEMP ALIAS MTEMP NEW

O_TIPPR

SELECT LD

PRIVATE cFilt1 := "GODINA=="+cm2str(cGodina)+;
		  IF(EMPTY(cIdRJ),"",".and.IDRJ=="+cm2str(cIdRJ))+;
		  IF(EMPTY(cIdRadn),"",".and.IDRADN=="+cm2str(cIdRadn))

SET FILTER TO &cFilt1
SET ORDER TO TAG "2"
GO TOP

DO WHILE !EOF()
  cMjesec:=mjesec
  DO WHILE !EOF() .and. cMjesec==mjesec
    SELECT MTEMP
    IF MTEMP->mjesec!=cMjesec
      APPEND BLANK
      REPLACE mjesec WITH cMjesec
    ENDIF
    FOR i:=1 TO cLDPolja
      cSTP := PADL(ALLTRIM(STR(i)),2,"0")
      IF cSvaPrim!="S" .and. !(cSTP $ qqOstPrim)
        SELECT TIPPR; HSEEK cSTP; SELECT MTEMP
        IF cSvaPrim=="N" .and. TIPPR->uneto=="N" .or.;
           cSvaPrim=="V" .and. TIPPR->uneto=="D" .or.;
           cSvaPrim=="0"
          LOOP
        ENDIF
      ENDIF
      cNPPI := "I"+cSTP
      cNPPS := "S"+cSTP
      nFPosI := FIELDPOS(cNPPI)
      nFPosS := FIELDPOS(cNPPS)
      IF nFPosI>0
        FIELDPUT( nFPosI , FIELDGET(nFPosI) + LD->(FIELDGET(nFPosI)) )
        if ! ( lViseObr .and. LD->obr<>"1" ) // samo sati iz 1.obracuna
          FIELDPUT( nFPosS , FIELDGET(nFPosS) + LD->(FIELDGET(nFPosS)) )
        endif
      ELSE
        EXIT
      ENDIF
    NEXT
    SELECT LD
    SKIP 1
  ENDDO
ENDDO


nSum:={}
aKol:={}

nKol:=1
nRed:=0
nKorekcija:=0

nPicISUk := IF(cPrikKolUk=="D",9,10)  // ako nema kolone ukupno mo§e i 10
nPicSDec := Decimala( gPicS )
nPicIDec := Decimala( gPicI )

NUK := IF(cPrikKolUk=="D",13,12)   // ukupno kolona za iznose

FOR i:=1 TO cLDPolja

  cSTP := PADL(ALLTRIM(STR(i)),2,"0")

  cNPPI := "I"+cSTP
  cNPPS := "S"+cSTP

  SELECT TIPPR; HSEEK cSTP; cAktivno := aktivan
  SELECT LD

  IF FIELDPOS(cNPPI) > 0

    IF ( cSamoAktivna=="N" .or. UPPER(cAktivno)=="D" ) .and.;
       ( cSvaPrim=="S" .or. cSTP $ qqOstPrim .or.;
         cSvaPrim=="N" .and. TIPPR->uneto=="D" .or.;
         cSvaPrim=="V" .and. TIPPR->uneto=="N" )

      cNPrim := "{|| '"+cSTP + "-" +;
                TIPPR->naz+"'}"

      AADD(aKol, { IF((i-nKorekcija)==1,"TIP PRIMANJA","") , &cNPrim. , .f., "C", 25, 0, 2*(i-nKorekcija)-1, 1 } )

      FOR j:=1 TO NUK

        cPomMI := "nSum["+ALLTRIM(STR(i-nKorekcija))+","+ALLTRIM(STR(j))+",1]"
        cPomMS := "nSum["+ALLTRIM(STR(i-nKorekcija))+","+ALLTRIM(STR(j))+",2]"

        AADD(aKol, { IF(i-nKorekcija==1,NazMjeseca(j),""), {|| &cPomMI.}, .f., "N", nPicISUk+IF(j>12,1,0), nPicIDec, 2*(i-nKorekcija)-1, j+1} )
        AADD(aKol, { IF(i-nKorekcija==1,"IZNOS/SATI","") , {|| &cPomMS.}, .f., "N", nPicISUk+IF(j>12,1,0), nPicSDec, 2*(i-nKorekcija)  , j+1} )

      NEXT

    ELSE

      nKorekcija+=1

    ENDIF

  ELSE
    EXIT
  ENDIF

NEXT

// dodati sumu svega (red "UKUPNO")
// --------------------------------
AADD(aKol, { "", {|| REPL("=",25)}, .f., "C", 25, 0, 2*(i-nKorekcija)-1, 1 } )

AADD(aKol, { "", {|| "U K U P N O"    }, .f., "C", 25, 0, 2*(i-nKorekcija), 1 } )
FOR j:=1 TO NUK
  cPomMI := "nSum["+ALLTRIM(STR(i-nKorekcija))+","+ALLTRIM(STR(j))+",1]"
  cPomMS := "nSum["+ALLTRIM(STR(i-nKorekcija))+","+ALLTRIM(STR(j))+",2]"

  AADD(aKol, { "", {|| &cPomMI.}, .f., "N", nPicISUk+IF(j>12,1,0), nPicIDec, 2*(i-nKorekcija)  , j+1} )
  AADD(aKol, { "", {|| &cPomMS.}, .f., "N", nPicISUk+IF(j>12,1,0), nPicSDec, 2*(i-nKorekcija)+1, j+1} )
NEXT
// --------------------------------

Altd()

nSumLen:=i-1-nKorekcija+1
nSum:=ARRAY(nSumLen,NUK,2)
FOR k:=1 TO nSumLen
  FOR j:=1 TO NUK
    FOR l:=1 TO 2
       nSum[k,j,l] := 0
    NEXT
  NEXT
NEXT

SELECT MTEMP
GO TOP

START PRINT CRET

P_12CPI

?? space(gnLMarg); ?? "LD: Izvjestaj na dan",date()
? space(gnLMarg); IspisFirme("")
? space(gnLMarg); ?? "RJ: "; B_ON; ?? IF( EMPTY(cIdRJ) , "SVE" , cIdRJ ); B_OFF
?? "  GODINA: "; B_ON; ?? cGodina; B_OFF
? "RADNIK: "
IF EMPTY(cIdRadn)
 ?? "SVI"
ELSE
 SELECT (F_RADN); HSEEK cIdRadn
 SELECT (F_STRSPR); HSEEK RADN->idstrspr
 SELECT (F_OPS); HSEEK RADN->idopsst; cOStan:=naz
 HSEEK RADN->idopsrad
 SELECT (F_RADN)
 B_ON; ?? cIdRadn+"-"+trim(naz)+' ('+trim(imerod)+') '+ime; B_OFF
 ? "Br.knjiz: "; B_ON; ?? brknjiz; B_OFF
 ?? "  Mat.br: "; B_ON; ?? matbr; B_OFF
 ?? "  R.mjesto: "; B_ON; ?? rmjesto; B_OFF

 ? "Min.rad: "; B_ON; ?? kminrad; B_OFF
 ?? "  Str.spr: "; B_ON; ?? STRSPR->naz; B_OFF
 ?? "  Opst.stan: "; B_ON; ?? cOStan; B_OFF

 ? "Opst.rada: "; B_ON; ?? OPS->naz; B_OFF
 ?? "  Dat.zasn.rad.odnosa: "; B_ON; ?? datod; B_OFF
 ?? "  Pol: "; B_ON; ?? pol; B_OFF
 SELECT MTEMP
ENDIF

StampaTabele(aKol,{|| FSvaki3()},,gTabela,,;
     ,"Specifikacija primanja po mjesecima"+IF(lIsplaceni,"","-neisplaceni"),;
                             {|| FFor3()},IF(gOstr=="D",,-1),,,,,)

FF
END PRINT

CLOSERET


FUNCTION FFor3()
 LOCAL nArr:=SELECT()
 DO WHILE !EOF()
   nKorekcija:=0
   FOR i:=1 TO cLDPolja
     cSTP := PADL(ALLTRIM(STR(i)),2,"0")
     cNPPI := "I"+cSTP
     cNPPS := "S"+cSTP
     SELECT TIPPR; HSEEK cSTP; cAktivno:=aktivan
     SELECT (nArr)
     nFPosI := FIELDPOS(cNPPI)
     nFPosS := FIELDPOS(cNPPS)
     IF nFPosI>0
       IF ( cSamoAktivna=="N" .or. UPPER(cAktivno)=="D" ) .and.;
          ( cSvaPrim=="S" .or. cSTP $ qqOstPrim .or.;
            cSvaPrim=="N" .and. TIPPR->uneto=="D" .or.;
            cSvaPrim=="V" .and. TIPPR->uneto=="N" )
         nSum[i-nKorekcija,mjesec,1] := FIELDGET(nFPosI)
              nSum[nSumLen,mjesec,1] += FIELDGET(nFPosI)
         nSum[i-nKorekcija,mjesec,2] := FIELDGET(nFPosS)
              nSum[nSumLen,mjesec,2] += FIELDGET(nFPosS)

         IF NUK>12
           // kolona 13.mjeseca tj."ukupno" iznos
           nSum[i-nKorekcija,NUK,1] += FIELDGET(nFPosI)
           // red ukupno kolone 13.mjeseca tj."sveukupno" iznos
                nSum[nSumLen,NUK,1] += FIELDGET(nFPosI)
           // kolona 13.mjeseca tj."ukupno" sati
           nSum[i-nKorekcija,NUK,2] += FIELDGET(nFPosS)
           // red ukupno kolone 13.mjeseca tj."sveukupno" sati
                nSum[nSumLen,NUK,2] += FIELDGET(nFPosS)
         ENDIF
       ELSE
         nKorekcija+=1
       ENDIF
     ELSE
       EXIT
     ENDIF
   NEXT
   SKIP 1
 ENDDO
// SKIP -1
RETURN .t.


PROCEDURE FSvaki3()
RETURN



FUNCTION Izrezi(cPoc,nIza,cOstObav)
  LOCAL cVrati:="", nPoz:=0
  DO WHILE (nPoz:=AT(cPoc,cOstObav)) > 0
    cVrati := cVrati + SUBSTR(cOstObav,nPoz+LEN(cPoc),nIza) + ";"
    cOstObav:=STUFF(cOstObav,nPoz,LEN(cPoc)+nIza,"")
    cOstObav:=STRTRAN(cOstObav,";;",";")
  ENDDO
RETURN cVrati


FUNCTION FormNum1(nIznos,nDuz,pici)
 LOCAL cVrati
 cVrati:=TRANSFORM(nIznos,pici)
 cVrati:=STRTRAN(cVrati,".",":")
 cVrati:=STRTRAN(cVrati,",",".")
 cVrati:=STRTRAN(cVrati,":",",")
 cVrati:=ALLTRIM(cVrati)
 cVrati:=IF(LEN(cVrati)>nDuz,REPL("*",nDuz),PADL(cVrati,nDuz))
RETURN cVrati


FUNCTION FormNum2(nIznos,nDuz,pici)
 return alltrim(formnum1(nIznos,nDuz,pici))


PROCEDURE PraviMTEMP()
 LOCAL i:=0
  IF ferase(PRIVPATH+"MTEMP.DBF")==-1
    MsgBeep("Ne mogu izbrisati MTEMP.DBF!")
    ShowFError()
  ENDIF
  aDbf:=LD->(DBSTRUCT())

  // ovdje cemo sva numericka polja prosiriti za 4 mjesta
  // (izuzeci su polja GODINA i MJESEC)
  // ----------------------------------------------------
  FOR i:=1 TO LEN(aDbf)
    IF aDbf[i,2]=="N" .and. !( UPPER(TRIM(aDbf[i,1])) $ "GODINA#MJESEC" )
      aDbf[i,3] += 4
    ENDIF
  NEXT

  DBCREATE2 (PRIVPATH+"MTEMP", aDbf)
RETURN



// ----------------------------
// Porezi i Doprinosi Iz Sezone
// ---------------------------------------------------------------------
// Ova procedura ispituje da li je za izraŸunavanje poreza i doprinosa
// u izvjeçtaju potrebno koristiti çifrarnike iz sezone. Ako se ustanovi
// da ovi çifrarnici postoje u sezoni 'MMGGGG' podrazumijeva se da njih
// treba koristiti za izvjeçtaj. U tom sluŸaju zatvaraju se postoje†i
// çifrarnici POR i DOPR iz radnog podruŸja, a umjesto njih otvaraju se
// sezonski.
// ---------------------------------------------------------------------
// cG - izvjeçtajna godina, cM - izvjeçtajni mjesec
// ---------------------------------------------------------------------
// Ukoliko izvjeçtaj koristi baze POR i/ili DOPR, one moraju biti
// otvorene prije pokretanje ove procedure.
// Ovu proceduru najbolje je pozivati odmah nakon upita za izvjeçtajnu
// godinu i mjesec (prije toga nema svrhe), a prije glavne izvjeçtajne
// petlje.
// ---------------------------------------------------------------------
PROCEDURE PoDoIzSez(cG,cM)
 LOCAL nArr:=SELECT(), cPath, aSez, i, cPom, lPor, lDopr, cPorDir, cDoprDir
  IF cG==NIL .or. cM==NIL; RETURN; ENDIF
  IF VALTYPE(cG)=="N"; cG:=STR(cG,4,0)                 ; ENDIF
  IF VALTYPE(cM)=="N"; cM:=PADL(ALLTRIM(STR(cM)),2,"0"); ENDIF

  cPath   := SIFPATH

  aSez := ASezona2(cPath,cG)

  IF LEN(aSez)<1; RETURN; ENDIF

  lPor    := lDopr    := .f.
  cPorDir := cDoprDir := ""
  FOR i:=1 TO LEN(aSez)
    cPom := TRIM(aSez[i,1])
    IF LEFT(cPom,2) >= cM
      IF FILE(cpath+cPom+"\POR.DBF")
        lPor     := .t.
        cPorDir  := cPom
      ENDIF
      IF FILE(cpath+cPom+"\DOPR.DBF")
        lDopr    := .t.
        cDoprDir := cPom
      ENDIF
    ELSE
      EXIT
    ENDIF
  NEXT

  IF lPor
    SELECT (F_POR); USE
    USE (cPath+cPorDir+"\POR")   ; SET ORDER TO TAG "ID"
  ENDIF

  IF lDopr
    SELECT (F_DOPR); USE
    USE (cPath+cDoprDir+"\DOPR") ; SET ORDER TO TAG "ID"
  ENDIF

  SELECT (nArr)
RETURN


*************************************************************
FUNCTION Razrijedi (cStr)
*
*   Razrijedi (cStr) --> cStrRazr
*      Ubaci u string, izmedju slova, SPACE()
*
*************************************************************
LOCAL cRazrStr, nLenM1, nCnt
cStr := ALLTRIM (cStr)
nLenM1 := LEN (cStr) - 1
cRazrStr := ""
FOR nCnt := 1 TO nLenM1
  cRazrStr += SUBSTR (cStr, nCnt, 1) + " "
NEXT
cRazrStr += RIGHT (cStr, 1)
RETURN (cRazrStr)



function ASezona2(cPath,cG,cFajl)
 LOCAL aSez, i, cPom
  IF cFajl==NIL; cFajl:=""; ENDIF
  aSez := DIRECTORY(cPath+"*.","DV")
  FOR i:=LEN(aSez) TO 1 STEP -1
    IF aSez[i,1]=="." .or. aSez[i,1]==".."
      ADEL(aSez,i)
      ASIZE(aSez,LEN(aSez)-1)
    ENDIF
  NEXT
  FOR i:=LEN(aSez) TO 1 STEP -1
    cPom := TRIM(aSez[i,1])
    IF LEN(cPom)<>6 .or. RIGHT(cPom,4)<>cG .or.;
       !EMPTY(cFajl) .and. !FILE(cPath+cPom+"\"+cFajl)
      ADEL(aSez,i)
      ASIZE(aSez,LEN(aSez)-1)
    ENDIF
  NEXT
  ASORT( aSez ,,, { |x,y| x[1] > y[1] } )
RETURN aSez


function Cijelih(cPic)
 LOCAL nPom := ATTOKEN( ALLTRIM(cPic) , "." , 2 ) - 2
RETURN IF( nPom<1 , LEN(ALLTRIM(cPic)) , nPom )

function Decimala(cPic)
 LOCAL nPom := ATTOKEN( ALLTRIM(cPic) , "." , 2 )
RETURN IF( nPom<1 , 0 ,  LEN( SUBSTR( ALLTRIM(cPic) , nPom ) )  )

