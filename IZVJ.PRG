#include "e.ch"
#include "std2.ch"
#include "inkey.ch"

#define  RADNIK  radn->(padr(  trim(naz)+" ("+trim(imerod)+") "+ime,35))

local izb:=1

#ifndef CPOR
private opc[22], h[22]

opc[ 1]:="1. pregled plata                               "
opc[ 2]:="2. pregled odredjenog primanja"
opc[ 3]:="3. platni spisak"
opc[ 4]:="4. platni spisak tekuci racun"
opc[ 5]:="5. platni spisak stedna knj  "
opc[ 6]:="6. kartice plate"
opc[ 7]:="7. rekapitulacija"
opc[ 8]:="8. rekapitulacija za sve rj"
opc[ 9]:="9. kartica plate za period (za m4)"
opc[10]:="A. rekapitulacija po koeficijentima"
opc[11]:="-----------------------------------"
opc[12]:="C. pregled primanja za period"
opc[13]:="D. specifikacija uz isplatu plata"
opc[14]:="E. specifikacija po opstinama i RJ"
opc[15]:="F. specifikacija po rasponima primanja"
opc[16]:="G. specifikacija primanja po mjesecima"
opc[17]:="H. rekapitulacija neto primanja"
opc[18]:="I. specif.novcanica potrebnih za isplatu plata"
opc[19]:="J. specif.prosjecnog neta po strucnoj spremi"
opc[20]:="K. lista radnika sa netom po opst.stanovanja"
opc[21]:="L. pregled obracunatih doprinosa"
opc[22]:="M. specifikacija primanja po RJ"

#else
private opc[15]

opc[ 1]:="1. zahtjev za doznacivanje novcanih sredstava v.I  "
opc[ 2]:="2. izvjestaj o izvrsenoj isplati"
opc[ 3]:="3. rekapitulacija neto primanja"
opc[ 4]:="4. specif.novcanica potrebnih za isplatu plata"
opc[ 5]:="6. kartice plate"
opc[ 6]:="7. rekapitulacija"
opc[ 7]:="8. rekapitulacija za sve rj"
opc[ 8]:="9. kartica plate za period"
opc[ 9]:="A. pregled primanja za period"
opc[10]:="B. ---------------------------------------------"
opc[11]:="C. ---------------------------------------------"
opc[12]:="D. specifikacija primanja po mjesecima"
opc[13]:="E. pregled odredjenog primanja"
opc[14]:="I. platni spisak"
opc[15]:="J. zahtjev za doznacivanje novcanih sredstava v.II"
#endif


do while .t.

  h[1]:=""
  h[2]:=""
  h[3]:=""
  h[4]:=""
  h[5]:=""
  h[6]:=""

  Izb:=Menu("izvj",opc,Izb,.f.)


#ifndef CPOR
     do case
        case Izb==0
           exit
        case Izb==1
           PregPl()
        case Izb==2
           PregPrim()
        case Izb==3
           PlatSp()
        case Izb==4
           PlatSpTr("1")   // 1 - za tekuci racun
        case Izb==5
           PlatSpTr("2")   // 2  - za stednu knjizicu
        case Izb==6
           KartPl()
        case Izb==7
           Rekap(.f.)
        case Izb==8
           Rekap(.t.)
           // sve rj
        case Izb==9
           UKartPl()
        case Izb==10
           RekapBod()
        case Izb==11
          ObrM4()
        case Izb==12
          PregPrimPer()
        case Izb==13
          Specif()
        case Izb==14
          Specif2()
        case Izb==15
          SpecifRasp()
        case Izb==16
          SpecifPoMjes()
        case Izb==17
          RekNeto()
        case Izb==18
          SpecNovcanica()
        case Izb==19
          Specif3()
        case Izb==20
          SpRadOpSt()
        case Izb==21
          IzObDop()    // izvjestaj o obracunatim doprinosima
        case Izb==22
          SpecPrimRJ()
     endcase
enddo
#else
     do case           // POR-LD
        case Izb==0
           exit
        case Izb==1
           PregPl("3")
        case Izb==15
           PregPl()
        case Izb==2
           PregPl("2")
        case Izb==13
           PregPrim()
        case Izb==14
           PlatSp()
        case Izb==5
           KartPl()
        case Izb==6
           Rekap(.f.)
        case Izb==7
           Rekap(.t.)
           // sve rj
        case Izb==8
           UKartPl()
        case Izb==9
          PregPrimPer()
//      case Izb==10
//        Specif()
//      case Izb==11
//        Specif2()
        case Izb==12
          SpecifPoMjes()
        case Izb==3
          RekNeto()
        case Izb==4
          SpecNovcanica()
     endcase
enddo
#endif


closeret

#ifndef CPOR
********************************
function PregPl()
* LD
********************************
local nC1:=20

cIdRadn:=space(_LR_)
cIdRj:=gRj; cmjesec:=gMjesec
cGodina:=gGodina
cObracun:=gObracun
cVarSort:="2"

O_KBENEF
O_VPOSLA
O_RJ
O_RADN
O_LD

 O_PARAMS
 Private cSection:="4",cHistory:=" ",aHistory:={}
 RPar("VS",@cVarSort)

private cKBenef:=" ",cVPosla:="  "

cIdMinuli:="17"
cKontrola:="N"
Box(,9,75)
@ m_x+1,m_y+2 SAY "Radna jedinica (prazno-sve): "  GET cIdRJ
@ m_x+2,m_y+2 SAY "Mjesec: "  GET  cmjesec  pict "99"
IF lViseObr
  @ m_x+2,col()+2 SAY "Obracun:" GET cObracun WHEN HelpObr(.t.,cObracun) VALID ValObr(.t.,cObracun)
ENDIF
@ m_x+3,m_y+2 SAY "Godina: "  GET  cGodina  pict "9999"
@ m_x+4,m_y+2 SAY "Koeficijent benef.radnog staza (prazno-svi): "  GET  cKBenef valid empty(cKBenef) .or. P_KBenef(@cKBenef)
@ m_x+5,m_y+2 SAY "Vrsta posla (prazno-svi): "  GET  cVPosla
@ m_x+7,m_y+2 SAY "Sifra primanja minuli: "  GET  cIdMinuli pict "@!"
@ m_x+8,m_y+2 SAY "Sortirati po(1-sifri,2-prezime+ime)"  GET cVarSort VALID cVarSort$"12"  pict "9"
@ m_x+9,m_y+2 SAY "Kontrolisati (neto)+(prim.van neta)-(odbici)=(za isplatu) ? (D/N)" GET cKontrola VALID cKontrola$"DN" PICT "@!"
read; clvbox(); ESC_BCR
BoxC()

 WPar("VS",cVarSort)
 SELECT PARAMS; USE

if lViseObr
  O_TIPPRN
else
  O_TIPPR
endif

if !empty(ckbenef)
 select kbenef
 hseek  ckbenef
endif
if !empty(cVPosla)
 select vposla
 hseek  cvposla
endif

select ld
//CREATE_INDEX("LDi1","str(godina)+idrj+str(mjesec)+idradn","LD")
//CREATE_INDEX("LDi2","str(godina)+str(mjesec)+idradn","LD")
if empty(cidrj)
  cidrj:=""
  IF cVarSort=="1"
    set order to tag (TagVO("2"))
    hseek str(cGodina,4)+str(cmjesec,2)+if(lViseObr.and.!EMPTY(cObracun),cObracun,"")
  ELSE
    Box(,2,30)
     nSlog:=0; nUkupno:=RECCOUNT2()
     cSort1:="SortPrez(IDRADN)"
     cFilt := IF(EMPTY(cMjesec),".t.","MJESEC==cMjesec")+".and."+;
              IF(EMPTY(cGodina),".t.","GODINA==cGodina")
     IF lViseObr .and. !EMPTY(cObracun)
       cFilt += ".and.OBR=cObracun"
     ENDIF
     INDEX ON &cSort1 TO "TMPLD" FOR &cFilt EVAL(TekRec2()) EVERY 1
    BoxC()
    GO TOP
  ENDIF
else
  IF cVarSort=="1"
    set order to tag (TagVO("1"))
    hseek str(cGodina,4)+cidrj+str(cmjesec,2)+if(lViseObr.and.!EMPTY(cObracun),cObracun,"")
  ELSE
    Box(,2,30)
     nSlog:=0; nUkupno:=RECCOUNT2()
     cSort1:="SortPrez(IDRADN)"
     cFilt := "IDRJ==cIdRj.and."+;
              IF(EMPTY(cMjesec),".t.","MJESEC==cMjesec")+".and."+;
              IF(EMPTY(cGodina),".t.","GODINA==cGodina")
     IF lViseObr .and. !EMPTY(cObracun)
       cFilt += ".and.OBR=cObracun"
     ENDIF
     INDEX ON &cSort1 TO "TMPLD" FOR &cFilt EVAL(TekRec2()) EVERY 1
    BoxC()
    GO TOP
  ENDIF
endif


EOF CRET

nStrana:=0
IF gVarPP=="2"
  m:="----- ------ ---------------------------------- ------- ----------- ----------- ----------- ----------- ----------- -----------"
ELSE
  m:="----- ------ ---------------------------------- ------- ----------- ----------- -----------"
ENDIF
bZagl:={|| ZPregPl() }

select rj; hseek ld->idrj; select ld

START PRINT CRET
12CPI

Eval(bZagl)

nRbr:=0
nT2a:=nT2b:=0
nT1:=nT2:=nT3:=nT3b:=nT4:=0
nVanP:=0  // van neta plus
nVanM:=0  // van neta minus
do while !eof() .and.  cgodina==godina .and. idrj=cidrj .and. cmjesec=mjesec .and.;
         !( lViseObr .and. !EMPTY(cObracun) .and. obr<>cObracun )
 IF lViseObr .and. EMPTY(cObracun)
   ScatterS(godina,mjesec,idrj,idradn)
 ELSE
   Scatter()
 ENDIF
 select radn; hseek _idradn
 select vposla; hseek _idvposla
 select kbenef; hseek vposla->idkbenef
 select ld
// if !empty(cvposla) .and. cvposla<>left(_idvposla,1)
 if !empty(cvposla) .and. cvposla<>left(_idvposla,2)
   skip; loop
 endif
 if !empty(ckbenef) .and. ckbenef<>kbenef->id
   skip; loop
 endif

 nVanP:=0
 nVanM:=0
 nMinuli:=0
 for i:=1 to cLDPolja
  cPom:=padl(alltrim(str(i)),2,"0")
  select tippr; seek cPom; select ld

  if tippr->(found()) .and. tippr->aktivan=="D"
   nIznos:=_i&cpom
   if tippr->uneto=="N" .and. nIznos<>0
       if nIznos>0
         nVanP+=nIznos
       else
         nVanM+=nIznos
       endif
   elseif tippr->uneto=="D" .and. nIznos<>0
       if cPom==cIdMinuli
           nMinuli:=nIznos
       endif
   endif
  endif
 next

 if prow()>62+gPStranica; FF; Eval(bZagl); endif
 ? str(++nRbr,4)+".",idradn, RADNIK
 nC1:=pcol()+1
 @ prow(),pcol()+1 SAY _usati  pict gpics
 IF gVarPP=="2"
   @ prow(),pcol()+1 SAY _uneto-nMinuli  pict gpici
   @ prow(),pcol()+1 SAY nMinuli  pict gpici
 ENDIF
 @ prow(),pcol()+1 SAY _uneto  pict gpici
 IF gVarPP=="2"
   @ prow(),pcol()+1 SAY nVanP   pict gpici
   @ prow(),pcol()+1 SAY nVanM   pict gpici
 ELSE
   @ prow(),pcol()+1 SAY nVanP+nVanM   pict gpici
 ENDIF
 @ prow(),pcol()+1 SAY _uiznos pict gpici

 if cKontrola=="D" .and. _uiznos<>_uneto+nVanP+nVanM
   @ prow(),pcol()+1 SAY "ERR"
 endif

  nT1+=_usati
  nT2a+=_uneto-nMinuli
  nT2b+=nMinuli
  nT2+=_uneto; nT3+=nVanP; nT3b+=nVanM; nT4+=_uiznos
 skip
enddo

if prow()>60+gpStranica; FF; Eval(bZagl); endif
? m
? " UKUPNO:"
@ prow(),nC1 SAY  nT1 pict gpics
IF gVarPP="2"
  @ prow(),pcol()+1 SAY  nT2a pict gpici
  @ prow(),pcol()+1 SAY  nT2b pict gpici
ENDIF
@ prow(),pcol()+1 SAY  nT2 pict gpici
IF gVarPP="2"
  @ prow(),pcol()+1 SAY  nT3 pict gpici
  @ prow(),pcol()+1 SAY  nT3b pict gpici
ELSE
  @ prow(),pcol()+1 SAY  nT3+nT3b pict gpici
ENDIF
@ prow(),pcol()+1 SAY  nT4 pict gpici
? m
FF
END PRINT
CLOSERET


***********************
function ZPregPl()
* LD
***********************
COND
? UPPER(gTS)+":",gnFirma
?
if empty(cidrj)
 ? "Pregled za sve RJ ukupno:"
else
 ? "RJ:",cidrj,rj->naz
endif
?? "  Mjesec:",str(cmjesec,2)+IspisObr()
?? "    Godina:",str(cGodina,5)
devpos(prow(),74)
?? "Str.",str(++nStrana,3)
if !empty(cvposla)
  ? "Vrsta posla:",cvposla,"-",vposla->naz
endif
if !empty(cKBenef)
  ? "Stopa beneficiranog r.st:",ckbenef,"-",kbenef->naz,":",kbenef->iznos
endif
? m
IF gVarPP=="2"
  ? " Rbr * Sifra*         Naziv radnika            *  Sati *   Redovan *  Minuli   *   Neto    *       VAN NETA       * ZA ISPLATU*"
  ? "     *      *                                  *       *     rad   *   rad     *           * Primanja  * Obustave *           *"
ELSE
  ? " Rbr * Sifra*         Naziv radnika            *  Sati *   Neto    *  Odbici   * ZA ISPLATU*"
  ? "     *      *                                  *       *           *           *           *"
ENDIF
? m
return
#else

********************************
function PregPl                               // "1" - pregl.obracunatih/isplacenih plata VAR.2
* POR-LD                                      // "2" - pregl.isplacenih/obracunatih i neisplacenih(obrisan obracun) plata
********************************              // "3" - pregl.obracunatih/isplacenih plata VAR.1
PARAMETERS cVarijanta
local nC1:=20, nKorPrava:=0, nUkPoDo:=0, nUkDozn:=0

if cVarijanta==NIL; cVarijanta:="1"; ENDIF

cIdRadn:=space(_LR_)
cIdRj:=gRj; cmjesec:=gMjesec
cGodina:=gGodina
IF cVarijanta$"13"
  O_RADKR
  O_RJES
ENDIF
O_PAROBR
O_POR
O_DOPR
O_KBENEF
O_VPOSLA
O_RJ
O_TIPPR
O_RADN
O_LD

private cKBenef:=" ",cVPosla:="  "

cIdMinuli:="17"; cOdvLin:="D"
Box(,7,50)
@ m_x+1,m_y+2 SAY "Radna jedinica (prazno-sve): "  GET cIdRJ
@ m_x+2,m_y+2 SAY "Mjesec: "  GET  cmjesec  pict "99"
@ m_x+3,m_y+2 SAY "Godina: "  GET  cGodina  pict "9999"
IF cVarijanta=="3"
  @ m_x+4,m_y+2 SAY "Odvajati podatke linijom (D/N) ?   "  GET cOdvLin VALID cOdvLin$"DN"  pict "@!"
ENDIF
read; ESC_BCR
BoxC()

SELECT PAROBR
HSEEK STR(cMjesec,2)

if !empty(ckbenef)
 select kbenef
 hseek  ckbenef
endif
if !empty(cVPosla)
 select vposla
 hseek  cvposla
endif

SELECT LD
//CREATE_INDEX("LDi1","str(godina)+idrj+str(mjesec)+idradn","LD")
//CREATE_INDEX("LDi2","str(godina)+str(mjesec)+idradn","LD")

Box(,2,30)
  nSlog:=0; nUkupno:=RECCOUNT2()
  IF cVarijanta=="3"
   cSort1:="SortVar(STR(godina,4)+STR(mjesec,2)+idradn+idkred)+SortPrez(idradn)"
  ELSEIF cVarijanta=="2"
   cSort1:="SortPrez(idradn)"
  ELSE
   cSort1:="SortVar(STR(godina,4)+STR(mjesec,2)+idradn+idkred)"
  ENDIF
  cFilt := IF(EMPTY(cIdRj),".t.","IDRJ==cIdRj")+".and."+;
           IF(EMPTY(cMjesec),".t.","MJESEC==cMjesec")+".and."+;
           IF(EMPTY(cGodina),".t.","GODINA==cGodina")
  INDEX ON &cSort1 TO "TMPLD" FOR &cFilt EVAL(TekRec2()) EVERY 1
  GO TOP
BoxC()

EOF CRET

nStrana:=0
// m:="----- ------ ---------------------------------- ------- ----------- ----------- ----------- ----------- -----------"
m:="----- ------ ---------------------------------- ------- ----------- ----------- -----------"
bZagl:={|| ZPregPl() }

select rj; hseek ld->idrj; select ld

START PRINT CRET
12CPI
PRIVATE nKrug:=1

IF cVarijanta=="3"

 Eval(bZagl)
 nTPor:=nTDopr:=nT1:=nT2:=0
 RekNeto("3")


   IF cMjesec==1
     cGodina2:=cGodina-1; cMjesec2:=12
   ELSE
     cGodina2:=cGodina; cMjesec2:=cMjesec-1
   ENDIF
   SELECT PAROBR
   nParRec:=RECNO()
   HSEEK STR(cMjesec2,2)
   SELECT LD
   PushWA()
   USE
   select (F_LDNO); usex (KUMPATH+"LDNO") alias LD
   if EMPTY(cIdRJ) // sve radne jedinice
     set order to 2
     seek str(cGodina2,4)+str(cmjesec2,2)
   else
     set order to 1
     seek str(cGodina2,4)+cidrj+str(cmjesec2,2)
   endif
   npUNeto:=0
   do while !eof() .and.  cgodina2==godina .and. cmjesec2=mjesec .and. ( EMPTY(cIdRJ) .or. cidrj==idrj )
     npUneto+=LD->uneto
     skip 1
   enddo  // LD
   npBo:=round2(parobr->k3/100*npUneto,gZaok2)
   SELECT POR; GO TOP
   npPom:=npPor:=npPorOps:=0
   do while !eof()
     npPom:=max(dlimit,iznos/100*npUneto)
     npPor+=npPom
     skip 1
   enddo
   SELECT DOPR; GO TOP
   npPom:=npDopr:=0
   do while !eof()
     npPom:=max(dlimit,iznos/100*npBO)
     if right(id,1)=="X"
      npDopr+=npPom
     endif
     skip 1
   enddo
   SELECT LD
   USE
   SELECT PAROBR
   GO (nParRec)
   O_LD
   PopWA()

 10CPI
 ? "--------------------------------- ------------ ------------ ------------"
 ? "        N A Z I V                  OBRACUNATO   PREPLACENO    POTREBNO  "
 ? "--------------------------------- ------------ ------------ ------------"
 ? "UKUPAN IZNOS NETO NAKNADA PLATA :" + STR(ROUND2(nT1+nT2,gZaok2),12,2)+" "+;
                                         SPACE(12)+" "+;
                                         STR(ROUND2(nT1+nT2,gZaok2),12,2)
 ? "UKUPAN IZNOS POREZA             :" + STR(ROUND2(nTPor,gZaok2),12,2)+" "+;
                                         STR(ROUND2(npPor,gZaok2),12,2)+" "+;
                                         STR(ROUND2(nTPor-npPor,gZaok2),12,2)
 ? "UKUPAN IZNOS DOPRINOSA          :" + STR(ROUND2(nTDopr,gZaok2),12,2)+" "+;
                                         STR(ROUND2(npDopr,gZaok2),12,2)+" "+;
                                         STR(ROUND2(nTDopr-npDopr,gZaok2),12,2)
 ? "UKUPAN IZNOS BRUTO NAKNADA PLATA:" + STR(ROUND2(nT1+nT2+nTPor+nTDopr,gZaok2),12,2)+" "+;
                                         STR(ROUND2(npPor+npDopr,gZaok2),12,2)+" "+;
                                         STR(ROUND2(nT1+nT2+nTPor-npPor+nTDopr-npDopr,gZaok2),12,2)

ELSE

DO WHILE cVarijanta=="2".and.nKrug<3 .or. cVarijanta=="1".and.nKrug<2

IF nKrug==2
  m+=REPL("-",20)
  SELECT LD
  USE
  select (F_LDNO)  ; usex (KUMPATH+"LDNO") alias LD

  IF cVarijanta=="3"
   cSort2:="SortVar(STR(godina)+STR(mjesec)+idradn+idkred)+SortPrez(idradn)"
  ELSEIF cVarijanta=="2"
   cSort2:="SortPrez(idradn)"
  ELSE
   cSort2:="SortVar(STR(godina)+STR(mjesec)+idradn+idkred)"
  ENDIF
  INDEX ON &cSort2 TO "TMPLDNO" FOR &cFilt
  GO TOP
ENDIF

nRbr:=0
nT1:=nT2:=nTPor:=nTDopr:=0
n01:=0  // van neta plus
n02:=0  // van neta minus
cVrLica:="X"

IF !EOF(); EVAL(bZagl); ENDIF

do while !eof() .and.  cgodina==godina .and. idrj=cidrj .and. cmjesec=mjesec

 IF cVarijanta=="1"
  cSortPom:=SortVar(STR(godina)+STR(mjesec)+idradn+idkred)
  IF cVrLica!=cSortPom
    IF cSortPom=="1"
    ? "Spisak zena-majki:"
    ? "------------------"
    ELSE
    ? "Spisak lica iz tacke V Odluke:"
    ? "------------------------------"
    ENDIF
    nRbr2:=0
    cVrLica:=cSortPom
  ENDIF
 ENDIF

 Scatter()
 select radn; hseek _idradn
 select vposla; hseek _idvposla
 select kbenef; hseek vposla->idkbenef
 select ld
// if !empty(cvposla) .and. cvposla<>left(_idvposla,1)
 if !empty(cvposla) .and. cvposla<>left(_idvposla,2)
   skip; loop
 endif
 if !empty(ckbenef) .and. ckbenef<>kbenef->id
   skip; loop
 endif

 n01:=0
 n02:=0
 for i:=1 to cLDPolja
  cPom:=padl(alltrim(str(i)),2,"0")
  select tippr; seek cPom; select ld

  if tippr->(found()) .and. tippr->aktivan=="D"
   nIznos:=_i&cpom
   if cpom=="01"
      n01+=nIznos
   else
      n02+=nIznos
   endif
  endif
 next

 if prow()>57+gPStranica; FF; Eval(bZagl); endif  // bilo 62
 ++nRbr
 if cVarijanta=="1"
   ? str(++nRbr2,4)+".",idradn, RADNIK
 else
   ? str(nRbr,4)+".",idradn, RADNIK
 endif
 nC1:=pcol()+1
 @ prow(),pcol()+1 SAY n01  pict gpici
 @ prow(),pcol()+1 SAY n02  pict gpici
 @ prow(),pcol()+1 SAY n01+n02  pict gpici
 IF nKrug==2
   @ prow(),pcol()+1 SAY razlog
 ENDIF

 // izracunajmo i poreze i doprinose

 *****************************************
 *****************************************
 nUNeto:=n01+n02
 nBo:=round2(parobr->k3/100*nUNeto,gZaok2)

 SELECT POR; GO TOP
 nPom:=nPor:=nPorOps:=0
 do while !eof()
//   nPom:=round2(max(dlimit,iznos/100*nUNeto),gZaok2)
   nPom:=max(dlimit,iznos/100*nUNeto)
   nPor+=nPom
   skip 1
 enddo

 SELECT DOPR; GO TOP
 nPom:=nDopr:=0
 do while !eof()
//   nPom:=round2(max(dlimit,iznos/100*nBO),gZaok2)
   nPom:=max(dlimit,iznos/100*nBO)
   if right(id,1)=="X"
    nDopr+=nPom
   endif
   skip 1
 enddo

// @ prow(),pcol()+1 SAY nPor+nDopr  pict gpici
// @ prow(),pcol()+1 SAY n01+n02+nPor+nDopr  pict gpici

 nTPor  += nPor
 nTDopr += nDopr
 *****************************************
 *****************************************

 SELECT LD

 nT1+=n01
 nT2+=n02
 ++nKorPrava
 skip 1
enddo  // LD

if prow()>55+gpStranica; FF; Eval(bZagl); endif  //bilo 60

IF nT1+nT2 > 0
 ? m
 ? " UKUPNO:"
 @ prow(),nC1 SAY  nT1 pict gpici
 @ prow(),pcol()+1 SAY  nT2 pict gpici
 @ prow(),pcol()+1 SAY  nT1+nT2 pict gpici
 ? m
ENDIF

IF cVarijanta=="2"
 ? "UKUPAN IZNOS POREZA I DOPRINOSA "+IF(nKrug==1,"","NE")+"ISPLACENIH RADNIKA :"+IF(nKrug==1,"  ","")+STR(Round2(nTPor+nTDopr,gZaok2),12,2)
 ? "UKUPAN IZNOS BRUTO NAKNADA PLATA "+IF(nKrug==1,"","NE")+"ISPLACENIH RADNIKA:"+IF(nKrug==1,"  ","")+STR(Round2(nT1+nT2+nTPor+nTDopr,gZaok2),12,2)
 nUkPoDo += (nTPor+nTDopr)
 nUkDozn += (nT1+nT2+nTPor+nTDopr)
ELSE
 ? "UKUPAN IZNOS POREZA I DOPRINOSA :"+STR(Round2(nTPor+nTDopr,gZaok2),12,2)
 ? "UKUPAN IZNOS BRUTO NAKNADA PLATA:"+STR(Round2(nT1+nT2+nTPor+nTDopr,gZaok2),12,2)
ENDIF

++nKrug
ENDDO  // zbog dva spiska ( 1-LD i 2-LDNO )

ENDIF   // cVarijanta!="3"

IF cVarijanta=="2"
 ?
 ? "----------------------------------------------------------------"
 ? "BROJ KORISNIKA PRAVA:"+STR(nKorPrava,4)
 ? "UKUPAN IZNOS UPLAèENIH POREZA I DOPRINOSA:"+STR(Round2(nUkPoDo,gZaok2),12,2)
 ? "UKUPAN IZNOS DOZNA¨ENIH SREDSTAVA        :"+STR(Round2(nUkDozn,gZaok2),12,2)
 ? "----------------------------------------------------------------"
ENDIF

FF
END PRINT
CLOSERET


***********************
function ZPregPl()
* POR- LD
***********************
IF nKrug==2
  ?
  ?
ENDIF

IF cVarijanta=="1" .or. nKrug<2
  12CPI
  B_ON
  ? UPPER(gTS)+":",gnFirma
  B_OFF
  ?
  if empty(cidrj)
    ? "Pregled za sve RJ ukupno:"
  else
    ? "RJ:",cidrj,rj->naz
  endif
  ?? "  Mjesec:",str(cmjesec,2),"   Godina:",str(cGodina,5)
  devpos(prow(),74)
  ?? "Str.",str(++nStrana,3)
  if !empty(cvposla)
    ? "Vrsta posla:",cvposla,"-",vposla->naz
  endif
  if !empty(cKBenef)
    ? "Stopa beneficiranog r.st:",ckbenef,"-",kbenef->naz,":",kbenef->iznos
  endif
  ?
ENDIF

10CPI
IF cVarijanta$"13"
 ? "                 ZAHTJEV ZA DOZNACIVANJE NOVCANIH SREDSTAVA"
 12CPI
ELSEIF nKrug<2
 ? "                       IZVJESTAJ O IZVRSENOJ ISPLATI"
 12CPI
ELSEIF nKrug>=2
 ? "                         LISTA NEISPLACENIH RADNIKA"
 COND
ENDIF
?
IF cVarijanta!="3"
 ? m
 // ? " Rbr *   Sifra     *         Naziv radnika            * za tekuci * za ostale *  ukupno   * porezi i  *   ukupno  *"
 // ? "     *             *                                  *  mjesec   *  mjesece  *   neto    * doprinosi *   bruto   *"
 ? " Rbr *   Sifra     *         Naziv radnika            * za tekuci * za ostale *  ukupno   *"+IF(nKrug==2,"      razlog       *","")
 ? "     *             *                                  *  mjesec   *  mjesece  *   neto    *"+IF(nKrug==2,"  neisplacivanja   *","")
 ? m
ENDIF
return

#endif
********************************
********************************
function RekNeto(cVarijanta)
 LOCAL aLeg:={}, aPom:={,,}, cVarSort:="2"
 IF cVarijanta==NIL; cVarijanta:="1"; ENDIF

 IF cVarijanta=="3"
  gnLMarg:=0; gTabela:=0; gOstr:="D"; cPKU:="N"; cPKPN:="N"
 ELSE
  gnLMarg:=0; gTabela:=0; gOstr:="D"; cOdvLin:="D"; cPKU:="N"; cPKPN:="N"
 ENDIF

IF cVarijanta!="3"

 cIdRj:=gRj; cmjesec:=gMjesec
 cGodina:=gGodina; cPrimR1:=cPrimR2:=cPrimR3:=SPACE(60)
 cObracun:=gObracun

 #ifdef CPOR
  cPrimR1:=PADR("01;02;03;04;05;06;07;",60)
  cPrimR2:=PADR("08;09;10;11;12;13;14;",60)
 #else
  cPrimR1:=PADR("01;",60)
 #endif

 O_RADKR
 O_KBENEF
 O_VPOSLA
 O_RJ
 O_RADN

 #ifdef CPOR
  IF Pitanje(,"Izvjestaj se pravi za isplacene(D) ili neisplacene(N) radnike?","D")=="D"
    lIsplaceni:=.t.
    O_LD
  ELSE
    lIsplaceni:=.f.
    select (F_LDNO)  ; usex (KUMPATH+"LDNO") alias LD; set order to 1
  ENDIF
 #else
  O_LD
 #endif

 O_PARAMS
 Private cSection:="4",cHistory:=" ",aHistory:={}
 #ifndef CPOR
  RPar("t1",@cPrimR1)
  RPar("t2",@cPrimR2)
  RPar("t3",@cPrimR3)
 #endif
 RPar("t4",@gOstr)
 RPar("t5",@cOdvLin)
 RPar("t6",@gTabela)
 RPar("t7",@cPKU)
 RPar("t8",@cPKPN)
 RPar("VS",@cVarSort)


 Box(,12,75)
 @ m_x+ 1,m_y+2 SAY "Radna jedinica (prazno-sve): "  GET cIdRJ
 @ m_x+ 2,m_y+2 SAY "Mjesec: "  GET  cmjesec  pict "99"
 if lViseObr
   @ m_x+ 2,col()+2 SAY "Obracun: "  GET  cObracun WHEN HelpObr(.t.,cObracun) VALID ValObr(.t.,cObracun)
 endif
 @ m_x+ 3,m_y+2 SAY "Godina: "  GET  cGodina  pict "9999"
 #ifndef CPOR
  @ m_x+ 4,m_y+2 SAY "Navedi 1.red primanja: (npr.01;02;)"  GET cPrimR1 pict "@S40"
  @ m_x+ 5,m_y+2 SAY "Navedi 2.red primanja: (npr.01;02;)"  GET cPrimR2 pict "@S40"
  @ m_x+ 6,m_y+2 SAY "Navedi 3.red primanja: (npr.01;02;)"  GET cPrimR3 pict "@S40"
 #endif
 @ m_x+ 7,m_y+2 SAY "Nacin crtanja tabele     (0/1/2)   "  GET gTabela VALID gTabela>=0.and.gTabela<=2 pict "9"
 @ m_x+ 8,m_y+2 SAY "Ukljuceno ostranicavanje (D/N) ?   "  GET gOstr   VALID gOstr$"DN"    pict "@!"
 @ m_x+ 9,m_y+2 SAY "Odvajati podatke linijom (D/N) ?   "  GET cOdvLin VALID cOdvLin$"DN"  pict "@!"
 @ m_x+10,m_y+2 SAY "Prikazati kolonu 'UKUPNO'(D/N) ?   "  GET cPKU    VALID cPKU$"DN"  pict "@!"
 @ m_x+11,m_y+2 SAY "Prikazati kolone 'Uk.neto','Uk.sati' i 'Uk.neto/Uk.sati' (D/N) ?"  GET cPKPN  VALID cPKPN$"DN"  pict "@!"
 @ m_x+12,m_y+2 SAY "Sortirati po(1-sifri,2-prezime+ime)"  GET cVarSort VALID cVarSort$"12"  pict "9"
 read; clvbox(); ESC_BCR
 BoxC()

 #ifndef CPOR
  WPar("t1",cPrimR1)
  WPar("t2",cPrimR2)
  WPar("t3",cPrimR3)
 #endif
 WPar("t4",gOstr)
 WPar("t5",cOdvLin)
 WPar("t6",gTabela)
 WPar("t7",cPKU)
 WPar("t8",cPKPN)
 WPar("VS",cVarSort)
 SELECT PARAMS; USE

ENDIF

if lViseObr
 O_TIPPRN
else
 O_TIPPR
endif

SELECT LD

IF cVarijanta!="3"

 Box(,2,30)
  nSlog:=0; nUkupno:=RECCOUNT2()
  IF cVarSort=="1"
    cSort1:="IDRADN"
  ELSE
    cSort1:="SortPrez(IDRADN)"
  ENDIF
  cFilt := IF(EMPTY(cIdRj),".t.","IDRJ==cIdRj")+".and."+;
           IF(EMPTY(cMjesec),".t.","MJESEC==cMjesec")+".and."+;
           IF(EMPTY(cGodina),".t.","GODINA==cGodina")
  if lViseObr .and. !EMPTY(cObracun)
    cFilt += ".and. OBR==cObracun"
  endif
  INDEX ON &cSort1 TO "TMPLD" FOR &cFilt EVAL(TekRec2()) EVERY 1
 BoxC()

 EOF CRET
 GO TOP
// ELSE
//  SET ORDER TO TAG (TagVO("1"))
ENDIF

#ifdef CPOR                          // automatsko biranje tipova primanja
 aPrim:={}                           // za POR-LD
 DO WHILE !EOF()
   FOR i:=1 TO 20
     cPom:="LD->I"+RIGHT("00"+LTRIM(STR(i)),2)
     IF TYPE(cPom) $ "UUIUE"; EXIT; ENDIF
     IF &cPom<>0
       cPrim:=RIGHT(cPom,2)
       IF ASCAN(aPrim,cPrim)==0
         AADD(aPrim,cPrim)
       ENDIF
     ENDIF
   NEXT
   SKIP 1
 ENDDO
 ASORT(aPrim)
 cPrimR1:=cPrimR2:=cPrimR3:=""
 FOR i:=1 TO LEN(aPrim)
   IF i<5
     cPrimR1:=cPrimR1+aPrim[i]+";"
   ELSEIF i<9
     cPrimR2:=cPrimR2+aPrim[i]+";"
   ELSE
     cPrimR3:=cPrimR3+aPrim[i]+";"
   ENDIF
 NEXT
 GO TOP
#endif

aKol:={}; nKol:=0

#ifdef CPOR
  AADD( aKol , { "R.BR." , {|| STR(nRBr,4)+"."}, .f., "C", 5, 0, 1, ++nKol} )
  AADD( aKol , { "SIFRA/JMB" , {|| cIdRadn}, .f., "C", 13, 0, 1, ++nKol} )
#else
  AADD( aKol , { "SIFRA"     , {|| cIdRadn}, .f., "C",  6, 0, 1, ++nKol} )
#endif

AADD( aKol , { "PREZIME I IME RADNIKA", {|| cNaziv }, .f., "C", 27, 0, 1, ++nKol} )

lEPR1:=EMPTY(cPrimR1); lEPR2:=EMPTY(cPrimR2); lEPR3:=EMPTY(cPrimR3)

DO WHILE !( EMPTY(cPrimR1) .and. EMPTY(cPrimR2) .and. EMPTY(cPrimR3) )

  ++nKol; aPom:={"","",""}

  IF !lEPR1
    nPoz1:=AT(";",cPrimR1)
    IF nPoz1>0
      cSifPr:=LEFT(cPrimR1,nPoz1-1)
      cPrimR1:=SUBSTR(cPrimR1,nPoz1+1)
      cVarPr:="SI"+ALLTRIM(STR(VAL(cSifPr)))
      &cVarPr:=0
      cBlk:="{|| "+cVarPr+"}"
    ELSEIF EMPTY(cPrimR1)
      cSifPr:=""
      cBlk:="{|| 0}"
    ELSE
      cSifPr:=TRIM(cPrimR1)
      cPrimR1:=""
      cVarPr:="SI"+ALLTRIM(STR(VAL(cSifPr)))
      &cVarPr:=0
      cBlk:="{|| "+cVarPr+"}"
    ENDIF
#ifdef CPOR
    IF !EMPTY(cSifPr)
      aRez:=GodMj(cGodina,cMjesec,-val(cSifPr)+1)
      cTpnaz := IF(cSifPr='14','<=','') + str(arez[2],2)+"/"+str(arez[1],4)
    ELSE
      cTPNaz:=""
    ENDIF
    AADD( aKol , { cTPNaz , &cBlk. , .t. , "N-" ,  9 , 2 , 1 , nKol } )
#else
    AADD( aKol , { cSifPr , &cBlk. , .t. , "N-" ,  9 , 2 , 1 , nKol } )
#endif
    aPom[1]:=cSifPr
  ENDIF

  IF !lEPR2
    nPoz2:=AT(";",cPrimR2)
    IF nPoz2>0
      cSifPr:=LEFT(cPrimR2,nPoz2-1)
      cPrimR2:=SUBSTR(cPrimR2,nPoz2+1)
      cVarPr:="SI"+ALLTRIM(STR(VAL(cSifPr)))
      &cVarPr:=0
      cBlk:="{|| "+cVarPr+"}"
    ELSEIF EMPTY(cPrimR2)
      cSifPr:=""
      cBlk:="{|| 0}"
    ELSE
      cSifPr:=TRIM(cPrimR2)
      cPrimR2:=""
      cVarPr:="SI"+ALLTRIM(STR(VAL(cSifPr)))
      &cVarPr:=0
      cBlk:="{|| "+cVarPr+"}"
    ENDIF
#ifdef CPOR
    IF !EMPTY(cSifPr)
      aRez:=GodMj(cGodina,cMjesec,-val(cSifPr)+1)
      cTpnaz := IF(cSifPr='14','<=','') + str(arez[2],2)+"/"+str(arez[1],4)
    ELSE
      cTPNaz:=""
    ENDIF
    AADD( aKol , { cTPNaz , &cBlk. , .t. , "N-" ,  9 , 2 , 2 , nKol } )
#else
    AADD( aKol , { cSifPr , &cBlk. , .t. , "N-" ,  9 , 2 , 2 , nKol } )
#endif
    aPom[2]:=cSifPr
  ENDIF

  IF !lEPR3
    nPoz3:=AT(";",cPrimR3)
    IF nPoz3>0
      cSifPr:=LEFT(cPrimR3,nPoz3-1)
      cPrimR3:=SUBSTR(cPrimR3,nPoz3+1)
      cVarPr:="SI"+ALLTRIM(STR(VAL(cSifPr)))
      &cVarPr:=0
      cBlk:="{|| "+cVarPr+"}"
    ELSEIF EMPTY(cPrimR3)
      cSifPr:=""
      cBlk:="{|| 0}"
    ELSE
      cSifPr:=TRIM(cPrimR3)
      cPrimR3:=""
      cVarPr:="SI"+ALLTRIM(STR(VAL(cSifPr)))
      &cVarPr:=0
      cBlk:="{|| "+cVarPr+"}"
    ENDIF
#ifdef CPOR
    IF !EMPTY(cSifPr)
      aRez:=GodMj(cGodina,cMjesec,-val(cSifPr)+1)
      cTpnaz := IF(cSifPr='14','<=','') + str(arez[2],2)+"/"+str(arez[1],4)
    ELSE
      cTPNaz:=""
    ENDIF
    AADD( aKol , { cTPNaz , &cBlk. , .t. , "N-" ,  9 , 2 , 3 , nKol } )
#else
    AADD( aKol , { cSifPr , &cBlk. , .t. , "N-" ,  9 , 2 , 3 , nKol } )
#endif
    aPom[3]:=cSifPr
  ENDIF

  AADD(aLeg,aPom)

ENDDO

siu:=0
IF cVarijanta=="3"
  IF LEN(aPrim)>1
    AADD( aKol , { "UKUPNO" , {|| siu } , .t. , "N-" ,  9 , 2 , IF(!lEPR3,3,IF(!lEPR2,2,1)) , ++nKol } )
  ENDIF
ELSEIF cPKU=="D"
    AADD( aKol , { "UKUPNO" , {|| siu } , .t. , "N-" ,  9 , 2 , IF(!lEPR3,3,IF(!lEPR2,2,1)) , ++nKol } )
ENDIF

IF cPKPN=="D"
    ssn:=0; sin:=0
    AADD( aKol , { "UK.NETO"  , {|| sin } , .t. , "N-" ,  9 , 2 , IF(!lEPR3,3,IF(!lEPR2,2,1)) , ++nKol } )
    AADD( aKol , { "UK.SATI"  , {|| ssn } , .t. , "N-" ,  9 , 2 , IF(!lEPR3,3,IF(!lEPR2,2,1)) , ++nKol } )
    AADD( aKol , { "NETO/SATI", {|| IF(ssn==0,0,sin/ssn) } , .f. , "N-" ,  9 , 2 , IF(!lEPR3,3,IF(!lEPR2,2,1)) , ++nKol } )
ENDIF

#ifdef CPOR
  IF cVarijanta!="3"
    AADD( aKol , { "P O T P I S" , {|| "" } , .f. , "C" , 15 , 0 , 1 , ++nKol } )
  ENDIF
#endif

FOR i:=1 TO 100
  IF FIELDPOS("I"+RIGHT("00"+ALLTRIM(STR(i)),2))==0; nPoljaPr:=i-1; EXIT; ENDIF
  nPoljaPr:=i
NEXT

PRIVATE cIdRadn:="", cNaziv:=""

IF cVarijanta!="3"
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
ENDIF

#ifdef CPOR
PRIVATE nRBr:=0
IF cVarijanta=="3"
 StampaTabele(aKol,{|| FSvaki4()},,gTabela,{|| !EOF().and.SortVar(STR(godina)+STR(mjesec)+idradn+idkred)=="1"},;
      ,"Spisak zena-majki",;
                              {|| FFor43()},IF(gOstr=="D",,-1),,cOdvLin=="D",,,)
 IF !EOF()
  nRBr:=0
  StampaTabele(aKol,{|| FSvaki4()},,gTabela,{|| !EOF().and.SortVar(STR(godina)+STR(mjesec)+idradn+idkred)=="2"},;
       ,"Spisak lica iz tacke V Odluke",;
                               {|| FFor43()},IF(gOstr=="D",,-1),,cOdvLin=="D",,,)
 ENDIF
ELSE
 StampaTabele(aKol,{|| FSvaki4()},,gTabela,,;
      ,"Rekapitulacija "+IF(lIsplaceni,"","neisplacenih ")+"neto primanja",;
                              {|| FFor4()},IF(gOstr=="D",,-1),,cOdvLin=="D",,,)
ENDIF
#else
StampaTabele(aKol,{|| FSvaki4()},,gTabela,,;
     ,"Rekapitulacija neto primanja",;
                             {|| FFor4()},IF(gOstr=="D",,-1),,cOdvLin=="D",,,)
#endif

?

#ifndef CPOR
? "LEGENDA:"
? "U kolonama tabele nalaze se sljedece sifre tipova primanja:"
FOR j:=1 TO 3
  FOR i:=1 TO LEN(aLeg)
    IF !EMPTY(aLeg[i,j])
      ? aLeg[i,j],"-",Ocitaj(SELECT("TIPPR"),aLeg[i,j],"naz")
    ENDIF
  NEXT
NEXT
#endif

IF cVarijanta!="3"
 FF
 END PRINT
 CLOSERET
ELSE
 RETURN
ENDIF





FUNCTION FFor4()
 cIdRadn:=IDRADN
 cNaziv:=Ocitaj(F_RADN,cIdRadn,"TRIM(NAZ)+' '+TRIM(IME)")
 FOR i:=1 TO nPoljaPr
   cPom77:="SI"+ALLTRIM(STR(i))
   IF TYPE(cPom77)=="N"; &cPom77:=0; ENDIF
 NEXT
 siu:=0; ssn:=0; sin:=0
 DO WHILE !EOF() .and. IDRADN==cIdRadn
   FOR i:=1 TO nPoljaPr
     cPom77:="SI"+ALLTRIM(STR(i))
     cPom77I:="I"+RIGHT("00"+ALLTRIM(STR(i)),2)
     IF TYPE(cPom77)=="N"
       &cPom77 := &cPom77 + &cPom77I
       siu := siu + &cPom77I
     ENDIF
   NEXT
   IF cPKPN=="D"
     if !(lViseObr .and. obr<>"1")
       ssn += usati
     endif
     sin += uneto
   ENDIF
   SKIP 1
 ENDDO
 SKIP -1
 #ifdef CPOR
 ++nRbr
 #endif
RETURN .t.



FUNCTION FFor43()
 LOCAL nUNeto,nBo,nPom,nPor,nPorOps,nDopr
 cIdRadn:=IDRADN
 SELECT RADN; HSEEK cidradn
 SELECT VPOSLA; HSEEK LD->idvposla
 SELECT KBENEF; HSEEK vposla->idkbenef
 SELECT LD
 if !empty(cvposla) .and. cvposla<>left(idvposla,2) .or.;
    !empty(ckbenef) .and. ckbenef<>kbenef->id
   return .f.
 endif
 cNaziv:=Ocitaj(F_RADN,cIdRadn,"TRIM(NAZ)+' '+TRIM(IME)")
 FOR i:=1 TO nPoljaPr
   cPom77:="SI"+ALLTRIM(STR(i))
   IF TYPE(cPom77)=="N"; &cPom77:=0; ENDIF
 NEXT
 siu:=0; ssn:=0; sin:=0
 DO WHILE !EOF() .and. IDRADN==cIdRadn
   FOR i:=1 TO nPoljaPr
     cPom77:="SI"+ALLTRIM(STR(i))
     cPom77I:="I"+RIGHT("00"+ALLTRIM(STR(i)),2)
     IF TYPE(cPom77)=="N"
       &cPom77 := &cPom77 + &cPom77I
       siu := siu + &cPom77I
     ENDIF
   NEXT
   IF cPKPN=="D"
     ssn += usati
     sin += uneto
   ENDIF
   SKIP 1
 ENDDO

 nUNeto:=siu
 nBo:=round2(parobr->k3/100*nUNeto,gZaok2)
 SELECT POR; GO TOP
 nPom:=nPor:=nPorOps:=0
 do while !eof()
   nPom:=max(dlimit,iznos/100*nUNeto)
   nPor+=nPom
   skip 1
 enddo

 SELECT DOPR; GO TOP
 nPom:=nDopr:=0
 do while !eof()
   nPom:=max(dlimit,iznos/100*nBO)
   if right(id,1)=="X"
    nDopr+=nPom
   endif
   skip 1
 enddo

 nTPor  += nPor
 nTDopr += nDopr
 nT1    += siu

 SELECT LD

 SKIP -1
 ++nRBr
RETURN IF(siu<>0,.t.,.f.)



PROCEDURE FSvaki4()
RETURN




********************************
********************************
function PregPrim()
local nC1:=20

cIdRadn:=space(_LR_)
cIdRj:=gRj; cmjesec:=gMjesec
cGodina:=gGodina
cObracun:=gObracun
cVarSort:="2"
lKredit:=.f.
cSifKred:=""

nRbr:=0
O_RJ
O_RADN

#ifdef CPOR
 IF Pitanje(,"Izvjestaj se pravi za isplacene(D) ili neisplacene(N) radnike?","D")=="D"
   lIsplaceni:=.t.
   O_LD
 ELSE
   lIsplaceni:=.f.
   select (F_LDNO)  ; usex (KUMPATH+"LDNO") alias LD; set order to 1
 ENDIF
#else
 O_LD
#endif

private cTip:="  "
cDod:="N"
cKolona:=space(20)

 O_PARAMS
 Private cSection:="4",cHistory:=" ",aHistory:={}
 RPar("VS",@cVarSort)

Box(,7,45)
@ m_x+1,m_y+2 SAY "Radna jedinica (prazno sve): "  GET cIdRJ
@ m_x+2,m_y+2 SAY "Mjesec: "  GET  cmjesec  pict "99"
if lViseObr
  @ m_x+2,col()+2 SAY "Obracun: " GET cObracun WHEN HelpObr(.t.,cObracun) VALID ValObr(.t.,cObracun)
endif
@ m_x+3,m_y+2 SAY "Godina: "  GET  cGodina  pict "9999"
@ m_x+4,m_y+2 SAY "Tip primanja: "  GET  cTip
@ m_x+5,m_y+2 SAY "Prikaz dodatnu kolonu: "  GET  cDod pict "@!" valid cdod $ "DN"
@ m_x+6,m_y+2 SAY "Sortirati po(1-sifri,2-prezime+ime)"  GET cVarSort VALID cVarSort$"12"  pict "9"
read; clvbox(); ESC_BCR
if cDod=="D"
 @ m_x+7,m_y+2 SAY "Naziv kolone:" GET cKolona
 read
endif
ckolona:="radn->"+ckolona
BoxC()

 WPar("VS",cVarSort)
 SELECT PARAMS; USE

if lViseObr
 O_TIPPRN
else
 O_TIPPR
endif

select tippr
hseek ctip
EOF CRET

IF "SUMKREDITA" $ formula
  // radi se o kreditu, upitajmo da li je potreban prikaz samo za
  // jednog kreditora
  // ------------------------------------------------------------
  lKredit:=.t.
  O_KRED
  cSifKred:=SPACE(LEN(id))
  Box(,6,75)
   @ m_x+2, m_y+2 SAY "Izabrani tip primanja je kredit ili se tretira na isti nacin kao i kredit."
   @ m_x+3, m_y+2 SAY "Ako zelite mozete dobiti spisak samo za jednog kreditora."
   @ m_x+5, m_y+2 SAY "Kreditor (prazno-svi zajedno)" GET cSifKred  valid EMPTY(cSifKred).or.P_Kred(@cSifKred) PICT "@!"
   READ
  BoxC()
ENDIF

IF !EMPTY(cSifKred)
  O_RADKR
  SET ORDER TO TAG "1"
ENDIF

select ld

if lViseObr
  cObracun:=TRIM(cObracun)
else
  cObracun:=""
endif

if empty(cidrj)
  cidrj:=""
  IF cVarSort=="1"
    set order to tag (TagVO("2"))
    hseek str(cGodina,4)+str(cmjesec,2)+cObracun
  ELSE
    Box(,2,30)
     nSlog:=0; nUkupno:=RECCOUNT2()
     cSort1:="SortPrez(IDRADN)"
     cFilt := IF(EMPTY(cMjesec),".t.","MJESEC==cMjesec")+".and."+;
              IF(EMPTY(cGodina),".t.","GODINA==cGodina")
     if lViseObr
       cFilt += ".and. OBR=cObracun"
     endif
     INDEX ON &cSort1 TO "TMPLD" FOR &cFilt EVAL(TekRec2()) EVERY 1
    BoxC()
    GO TOP
  ENDIF
else
  IF cVarSort=="1"
    set order to tag (TagVO("1"))
    hseek str(cGodina,4)+cidrj+str(cmjesec,2)+cObracun
  ELSE
    Box(,2,30)
     nSlog:=0; nUkupno:=RECCOUNT2()
     cSort1:="SortPrez(IDRADN)"
     cFilt := "IDRJ==cIdRj.and."+;
              IF(EMPTY(cMjesec),".t.","MJESEC==cMjesec")+".and."+;
              IF(EMPTY(cGodina),".t.","GODINA==cGodina")
     if lViseObr
       cFilt += ".and. OBR=cObracun"
     endif
     INDEX ON &cSort1 TO "TMPLD" FOR &cFilt EVAL(TekRec2()) EVERY 1
    BoxC()
    GO TOP
  ENDIF
endif

EOF CRET

nStrana:=0
m:="----- "+replicate("-",_LR_)+" ---------------------------------- "+IF(lKredit.and.!EMPTY(cSifKred),REPL("-",LEN(RADKR->naosnovu)+1),"-------")+" ----------- -----------"
if cdod=="D"
 if type(ckolona) $ "UUIUE"
     Msg("Nepostojeca kolona")
     closeret
 endif
endif
bZagl:={|| ZPregPrim() }

select rj; hseek ld->idrj; select ld

START PRINT CRET
10CPI

Eval(bZagl)

nRbr:=0
nT1:=nT2:=nT3:=nT4:=0
nC1:=10

do while !eof() .and.  cgodina==godina .and. idrj=cidrj .and. cmjesec=mjesec .and.;
         !( lViseObr .and. !EMPTY(cObracun) .and. obr<>cObracun )

 if lViseObr .and. EMPTY(cObracun)
   ScatterS(godina,mjesec,idrj,idradn)
 else
   Scatter()
 endif

 IF lKredit .and. !EMPTY(cSifKred)
   // provjerimo da li otplacuje zadanom kreditoru
   // --------------------------------------------
   SELECT RADKR
   SEEK str(cgodina,4)+str(cmjesec,2)+LD->idradn+cSifKred
   lImaJos:=.f.
   DO WHILE !EOF() .and. str(cgodina,4)+str(cmjesec,2)+LD->idradn+cSifKred == str(godina,4)+str(mjesec,2)+idradn+idkred
     IF placeno>0
       lImaJos:=.t.
       EXIT
     ENDIF
     SKIP 1
   ENDDO
   IF !lImaJos
     SELECT LD; SKIP 1; LOOP
   ELSE
     SELECT LD
   ENDIF
 ENDIF

 select radn; hseek _idradn; select ld

 DO WHILE .t.
   if prow()>62+gPStranica; FF; Eval(bZagl); endif

   if _i&cTip<>0 .or. _s&cTip<>0
     ? str(++nRbr,4)+".",idradn, RADNIK
     nC1:=pcol()+1
     if lKredit .and. !EMPTY(cSifKred)
       @ prow(),pcol()+1 SAY RADKR->naosnovu
     elseif tippr->fiksan=="P"
       @ prow(),pcol()+1 SAY _s&cTip  pict "999.99"
     else
       @ prow(),pcol()+1 SAY _s&cTip  pict gpics
     endif
     IF lKredit .and. !EMPTY(cSifKred)
       @ prow(),pcol()+1 SAY -RADKR->placeno  pict gpici
       nT2 += (-RADKR->placeno)
     ELSE
       @ prow(),pcol()+1 SAY _i&cTip  pict gpici
       nT1+=_s&cTip; nT2+=_i&cTip
     ENDIF
     if cdod=="D"
       @ prow(),pcol()+1 SAY &ckolona
     endif
   endif
   IF lKredit .and. !EMPTY(cSifKred)
     lImaJos:=.f.
     SELECT RADKR; SKIP 1
     DO WHILE !EOF() .and. str(cgodina,4)+str(cmjesec,2)+LD->idradn+cSifKred == str(godina,4)+str(mjesec,2)+idradn+idkred
       IF placeno>0
         lImaJos:=.t.
         EXIT
       ENDIF
       SKIP 1
     ENDDO
     SELECT LD
     IF !lImaJos
       EXIT
     ENDIF
   ELSE
     EXIT
   ENDIF
 ENDDO

 skip 1

enddo

if prow()>60+gPStranica; FF; Eval(bZagl); endif
? m
? " UKUPNO:"
IF lKredit .and. !EMPTY(cSifKred)
  @ prow(),nC1 SAY  SPACE(LEN(RADKR->naosnovu))
ELSE
  @ prow(),nC1 SAY  nT1 pict gpics
ENDIF
@ prow(),pcol()+1 SAY  nT2 pict gpici
? m
FF
END PRINT
CLOSERET


********************
********************
function ZPregPrim()

12CPI
? UPPER(gTS)+":",gnFirma
?
if empty(cidrj)
 ? "Pregled za sve RJ ukupno:"
else
 ? "RJ:",cidrj,rj->naz
endif

?? "  Mjesec:",str(cmjesec,2)+IspisObr()
?? "    Godina:",str(cGodina,5)
devpos(prow(),74)
?? "Str.",str(++nStrana,3)
?
#ifdef CPOR
? "Pregled "+IF(lIsplaceni,"isplacenih iznosa","neisplacenih iznosa")+" za tip primanja:",ctip,tippr->naz
#else
? "Pregled za tip primanja:",ctip,tippr->naz
IF lKredit
  ? "KREDITOR: "
  IF !EMPTY(cSifKred)
    ?? cSifKred+"-"+Ocitaj(F_KRED,cSifKred,"naz")
  ELSE
    ?? "SVI POSTOJECI"
  ENDIF
ENDIF
#endif
?
? m
IF lKredit .and. !EMPTY(cSifKred)
  ? " Rbr  "+padc("Sifra ",_LR_)+"          Naziv radnika               "+PADC("Na osnovu",LEN(RADKR->naosnovu))+"      Iznos"
ELSE
  ? " Rbr  "+padc("Sifra ",_LR_)+"          Naziv radnika               "+iif(tippr->fiksan=="P"," %  ","Sati")+"      Iznos"
ENDIF
? m
return

********************************
********************************
function PlatSp()
local nC1:=20, cVarSort:="2"

cIdRadn:=space(_LR_)
cIdRj:=gRj; cmjesec:=gMjesec
cGodina:=gGodina
cObracun:=gObracun

O_RJ
O_RADN
#ifdef CPOR
 IF Pitanje(,"Izvjestaj se pravi za isplacene(D) ili neisplacene(N) radnike?","D")=="D"
   lIsplaceni:=.t.
   O_LD
 ELSE
   lIsplaceni:=.f.
   select (F_LDNO)  ; usex (KUMPATH+"LDNO") alias LD; set order to 1
 ENDIF
#else
 O_LD
#endif

cProred:="N"
cPrikIzn:="D"
nProcenat:=100
nZkk:=gzaok
cDrugiDio:="D"

 O_PARAMS
 Private cSection:="4",cHistory:=" ",aHistory:={}
 RPar("VS",@cVarSort)

Box(,9,50)
@ m_x+1,m_y+2 SAY "Radna jedinica (prazno-sve): "  GET cIdRJ
@ m_x+2,m_y+2 SAY "Mjesec: "  GET  cmjesec  pict "99"
if lViseObr
  @ m_x+2,col()+2 SAY "Obracun: "  GET  cObracun WHEN HelpObr(.t.,cObracun) VALID ValObr(.t.,cObracun)
endif
@ m_x+3,m_y+2 SAY "Godina: "  GET  cGodina  pict "9999"
@ m_x+4,m_y+2 SAY "Prored:"   GET  cProred  pict "@!"  valid cProred $ "DN"
@ m_x+5,m_y+2 SAY "Prikaz iznosa:" GET cPrikIzn pict "@!" valid cPrikizn$"DN"
@ m_x+6,m_y+2 SAY "Prikaz u procentu %:" GET nprocenat pict "999.99"
@ m_x+7,m_y+2 SAY "Sortirati po(1-sifri,2-prezime+ime)"  GET cVarSort VALID cVarSort$"12"  pict "9"
read; clvbox(); ESC_BCR
if nprocenat<>100
  @ m_x+8,m_y+2 SAY "zaokruzenje" GET nZkk pict "99"
  @ m_x+9,m_y+2 SAY "Prikazati i drugi spisak (za "+LTRIM(STR(100-nProcenat,6,2))+"%-tni dio)" GET cDrugiDio VALID cDrugiDio$"DN" PICT "@!"
  read
else
  cDrugiDio:="N"
endif
BoxC()

 WPar("VS",cVarSort)
 SELECT PARAMS; USE

select ld
//CREATE_INDEX("LDi1","str(godina)+idrj+str(mjesec)+idradn","LD")
//CREATE_INDEX("LDi2","str(godina)+str(mjesec)+idradn","LD")

if lViseObr
  cObracun:=TRIM(cObracun)
else
  cObracun:=""
endif

if empty(cidrj)
  cidrj:=""
  IF cVarSort=="1"
    set order to tag (TagVO("2"))
    hseek str(cGodina,4)+str(cmjesec,2)+cObracun
  ELSE
    Box(,2,30)
     nSlog:=0; nUkupno:=RECCOUNT2()
     cSort1:="SortPrez(IDRADN)"
     cFilt := IF(EMPTY(cMjesec),".t.","MJESEC==cMjesec")+".and."+;
              IF(EMPTY(cGodina),".t.","GODINA==cGodina")
     if lViseObr
       cFilt+=".and. obr=cObracun"
     endif
     INDEX ON &cSort1 TO "TMPLD" FOR &cFilt EVAL(TekRec2()) EVERY 1
    BoxC()
    GO TOP
  ENDIF
else
  IF cVarSort=="1"
    set order to tag (TagVO("1"))
    hseek str(cGodina,4)+cidrj+str(cmjesec,2)+cObracun
  ELSE
    Box(,2,30)
     nSlog:=0; nUkupno:=RECCOUNT2()
     cSort1:="SortPrez(IDRADN)"
     cFilt := "IDRJ==cIdRj.and."+;
              IF(EMPTY(cMjesec),".t.","MJESEC==cMjesec")+".and."+;
              IF(EMPTY(cGodina),".t.","GODINA==cGodina")
     if lViseObr
       cFilt+=".and. obr=cObracun"
     endif
     INDEX ON &cSort1 TO "TMPLD" FOR &cFilt EVAL(TekRec2()) EVERY 1
    BoxC()
    GO TOP
  ENDIF
endif


EOF CRET

nStrana:=0

m:="----- "+replicate("-",_LR_)+" ----------------------------------- ----------- -------------------------"
bZagl:={|| ZPlatSp() }

select rj; hseek ld->idrj; select ld

START PRINT CRET


nPocRec:=RECNO()

FOR nDio:=1 TO IF(cDrugiDio=="D",2,1)

IF nDio==2; GO (nPocRec); ENDIF

Eval(bZagl)

nT1:=nT2:=nT3:=nT4:=0
nRbr:=0
do while !eof() .and.  cgodina==godina .and. idrj=cidrj .and. cmjesec=mjesec .and.;
         !( lViseObr .and. !EMPTY(cObracun) .and. obr<>cObracun )
 IF lViseObr .and. EMPTY(cObracun)
   ScatterS(godina,mjesec,idrj,idradn)
 else
   Scatter()
 endif
 select radn; hseek _idradn; select ld
 if !(empty(radn->isplata) .or. radn->isplata="BL")
    skip
    loop
 endif
 if prow()>62+gpStranica; FF; Eval(bZagl); endif
 ? str(++nRbr,4)+".",idradn, RADNIK
 nC1:=pcol()+1
 if cprikizn=="D"
   if nProcenat<>100
    IF nDio==1
      @ prow(),pcol()+1 SAY round(_uiznos*nprocenat/100,nzkk) pict gpici
    ELSE
      @ prow(),pcol()+1 SAY ROUND(_uiznos,nzkk)-round(_uiznos*nprocenat/100,nzkk) pict gpici
    ENDIF
   else
    @ prow(),pcol()+1 SAY _uiznos pict gpici
   endif
 else
  @ prow(),pcol()+1 SAY space(len(gpici))
 endif
 @ prow(),pcol()+4 SAY replicate("_",22)
 if cProred=="D"
   ?
 endif
 nT1+=_usati; nT2+=_uneto; nT3+=_uodbici
 IF  NPROCENAT<>100
   IF nDio==1
     nT4 += round(_uiznos*nprocenat/100,nzkk)
   ELSE
     nT4 += ( round(_uiznos,nzkk) - round(_uiznos*nprocenat/100,nzkk) )
   ENDIF
 ELSE
   nT4+=_uiznos
 ENDIF
 skip
enddo

if prow()>60+gpStranica; FF; Eval(bZagl); endif
? m
? " UKUPNO:"
if cprikizn=="D"
  @ prow(),nC1 SAY  nT4 pict gpici
endif
? m
FF

NEXT

END PRINT

CLOSERET


*****************
*****************
function ZPlatSp()

12CPI
? UPPER(gTS)+":",gnFirma
?
if empty(cidrj)
 ? "Pregled za sve RJ ukupno:"
else
 ? "RJ:",cidrj,rj->naz
endif

?? "  Mjesec:",str(cmjesec,2)+IspisObr()
?? "    Godina:",str(cGodina,5)
devpos(prow(),74)
?? "Str.",str(++nStrana,3)
?
#ifdef CPOR
? "SPISAK "+IF(lIsplaceni,"ISPLACENIH","NEISPLACENIH")+" RADNIKA"
#endif
if nprocenat<>100
 ?
 ? "Procenat za isplatu:"
 if nDio==1
  @ prow(),pcol()+1 SAY nprocenat pict "999.99%"
 else
  @ prow(),pcol()+1 SAY 100-nprocenat pict "999.99%"
 endif

 ?
endif
? m
#ifdef CPOR
? "Rbr     Sifra                Naziv radnika               "+iif(cprikizn=="D","ZA ISPLATU","          ")+"         Potpis"
#else
? "Rbr   Sifra           Naziv radnika               "+iif(cprikizn=="D","ZA ISPLATU","          ")+"         Potpis"
#endif
? m
return


********************************
* plat spisak tekuci racun
* "1" - tekuciracun
* "2" - stedna knjizica
********************************
function PlatSpTR(cVarijanta)
local nC1:=20

cIdRadn:=space(_LR_)
cIdRj:=gRj; cmjesec:=gMjesec
cGodina:=gGodina
cObracun:=gObracun
cVarSort:="2"

O_KRED
O_RJ
O_RADN
O_LD

cProred:="N"
cPrikIzn:="D"
nProcenat:=100
nZkk:=gzaok
private cIsplata:=""
if cVarijanta=="1"
   cIsplata:="TR"
else
   cIsplata:="SK"
endif

cIDBanka:=space(_LR_)
cDrugiDio:="D"

 O_PARAMS
 Private cSection:="4",cHistory:=" ",aHistory:={}
 RPar("VS",@cVarSort)

Box(,10,50)
@ m_x+1,m_y+2 SAY "Radna jedinica (prazno-sve): "  GET cIdRJ
@ m_x+2,m_y+2 SAY "Mjesec: "  GET  cmjesec  pict "99"
if lViseObr
  @ m_x+2,col()+2 SAY "Obracun: "  GET  cObracun WHEN HelpObr(.t.,cObracun) VALID ValObr(.t.,cObracun)
endif
@ m_x+3,m_y+2 SAY "Godina: "  GET  cGodina  pict "9999"
@ m_x+4,m_y+2 SAY "Prored:"   GET  cProred  pict "@!"  valid cProred $ "DN"
@ m_x+5,m_y+2 SAY "Prikaz iznosa:" GET cPrikIzn pict "@!" valid cPrikizn$"DN"
@ m_x+6,m_y+2 SAY "Prikaz u procentu %:" GET nprocenat pict "999.99"
@ m_x+7,m_y+2 SAY "Banka        :" GET cIdBanka  valid P_Kred(@cidbanka)
@ m_x+8,m_y+2 SAY "Sortirati po(1-sifri,2-prezime+ime)"  GET cVarSort VALID cVarSort$"12"  pict "9"
read; clvbox(); ESC_BCR
if nprocenat<>100
  @ m_x+9,m_y+2 SAY "zaokruzenje" GET nZkk pict "99"
  @ m_x+10,m_y+2 SAY "Prikazati i drugi spisak (za "+LTRIM(STR(100-nProcenat,6,2))+"%-tni dio)" GET cDrugiDio VALID cDrugiDio$"DN" PICT "@!"
  read
else
  cDrugiDio:="N"
endif
BoxC()

 WPar("VS",cVarSort)
 SELECT PARAMS; USE

select ld
//CREATE_INDEX("LDi1","str(godina)+idrj+str(mjesec)+idradn","LD")
//CREATE_INDEX("LDi2","str(godina)+str(mjesec)+idradn","LD")

if lViseObr
  cObracun:=TRIM(cObracun)
else
  cObracun:=""
endif

if empty(cidrj)
  cidrj:=""
  IF cVarSort=="1"
    set order to tag (TagVO("2"))
    hseek str(cGodina,4)+str(cmjesec,2)+cObracun
  ELSE
    Box(,2,30)
     nSlog:=0; nUkupno:=RECCOUNT2()
     cSort1:="SortPrez(IDRADN)"
     cFilt := IF(EMPTY(cMjesec),".t.","MJESEC==cMjesec")+".and."+;
              IF(EMPTY(cGodina),".t.","GODINA==cGodina")
     if lViseObr
       cFilt+=".and. obr=cObracun"
     endif
     INDEX ON &cSort1 TO "TMPLD" FOR &cFilt EVAL(TekRec2()) EVERY 1
    BoxC()
    GO TOP
  ENDIF
else
  IF cVarSort=="1"
    set order to tag (TagVO("1"))
    hseek str(cGodina,4)+cidrj+str(cmjesec,2)+cObracun
  ELSE
    Box(,2,30)
     nSlog:=0; nUkupno:=RECCOUNT2()
     cSort1:="SortPrez(IDRADN)"
     cFilt := "IDRJ==cIdRj.and."+;
              IF(EMPTY(cMjesec),".t.","MJESEC==cMjesec")+".and."+;
              IF(EMPTY(cGodina),".t.","GODINA==cGodina")
     if lViseObr
       cFilt+=".and. obr=cObracun"
     endif
     INDEX ON &cSort1 TO "TMPLD" FOR &cFilt EVAL(TekRec2()) EVERY 1
    BoxC()
    GO TOP
  ENDIF
endif

EOF CRET

nStrana:=0
m:="----- ------ ----------------------------------- ----------- -------------------------"
bZagl:={|| ZPlatSpTR() }

select rj; hseek ld->idrj; select ld

START PRINT CRET

nPocRec:=RECNO()

FOR nDio:=1 TO IF(cDrugiDio=="D",2,1)

IF nDio==2; GO (nPocRec); ENDIF

Eval(bZagl)

nT1:=nT2:=nT3:=nT4:=0
nRbr:=0
do while !eof() .and.  cgodina==godina .and. idrj=cidrj .and. cmjesec=mjesec .and.;
         !( lViseObr .and. !EMPTY(cObracun) .and. obr<>cObracun )
 if lViseObr .and. EMPTY(cObracun)
   ScatterS(godina,mjesec,idrj,idradn)
 else
   Scatter()
 endif
 select radn; hseek _idradn; select ld
 if radn->isplata<>cIsplata .or. radn->idbanka<>cIdBanka // samo za tekuce racune
    skip
    loop
 endif
 if prow()>62+gPStranica; FF; Eval(bZagl); endif
 ? str(++nRbr,4)+".",idradn, RADNIK
 nC1:=pcol()+1
 if cprikizn=="D"
   if nProcenat<>100
    IF nDio==1
      @ prow(),pcol()+1 SAY round(_uiznos*nprocenat/100,nzkk) pict gpici
    ELSE
      @ prow(),pcol()+1 SAY ROUND(_uiznos,nzkk)-round(_uiznos*nprocenat/100,nzkk) pict gpici
    ENDIF
   else
    @ prow(),pcol()+1 SAY _uiznos pict gpici
   endif
 else
  @ prow(),pcol()+1 SAY space(len(gpici))
 endif
 if cisplata=="TR"
  @ prow(),pcol()+4 SAY padl(radn->brtekr,22)
 else
   @ prow(),pcol()+4 SAY padl(radn->brknjiz,22)
 endif
 if cProred=="D"
   ?
 endif
 nT1+=_usati; nT2+=_uneto; nT3+=_uodbici
 IF  NPROCENAT<>100
   IF nDio==1
     nT4 += round(_uiznos*nprocenat/100,nzkk)
   ELSE
     nT4 += ( round(_uiznos,nzkk) - round(_uiznos*nprocenat/100,nzkk) )
   ENDIF
 ELSE
   nT4+=_uiznos
 ENDIF
 skip
enddo

if prow()>60+gPStranica; FF; Eval(bZagl); endif
? m
? " UKUPNO:"
if cprikizn=="D"
  @ prow(),nC1 SAY  nT4 pict gpici
endif
? m
FF

NEXT



END PRINT
CLOSERET


*****************
*****************
function ZPlatSpTR()

12CPI
select kred; hseek radn->idbanka; select ld
?
? "Poslovna BANKA: ", cIDBanka, "-", kred->naz
?
? UPPER(gTS)+":",gnFirma
?
if empty(cidrj)
 ? "Pregled za sve RJ ukupno:"
else
 ? "RJ:",cidrj,rj->naz
endif

?? "  Mjesec:",str(cmjesec,2)+IspisObr()
?? "    Godina:",str(cGodina,5)
devpos(prow(),74)
?? "Str.",str(++nStrana,3)
?
if nprocenat<>100
 ?
 ? "Procenat za isplatu:"

 if nDio==1
  @ prow(),pcol()+1 SAY nprocenat pict "999.99%"
 else
  @ prow(),pcol()+1 SAY 100-nprocenat pict "999.99%"
 endif

 ?
endif
?
? m
? "Rbr   Sifra           Naziv radnika               "+iif(cprikizn=="D","ZA ISPLATU","          ")+iif(cIsplata=="TR","         Broj T.Rac","        Broj St.knj")
? m
return


********************************
********************************
function KartPl(cIdRj,cMjesec,cGodina,cIdRadn,cObrac)
local i,aNeta:={}

lSkrivena:=.f.
cLMSK:=""
l2kolone:=.f.
IF IzFMKIni("LD","SkrivenaKartica","N",KUMPATH)=="D".and.;
   Pitanje(,"Stampati u formi skrivene kartice? (D/N)","D")=="D"
  lSkrivena:=.t.
  cLMSK:=SPACE(VAL(IzFMKINI("SkrivenaKartica","LMarginaKolona","10",KUMPATH)))
  nIZRSK:=VAL(IzFMKINI("SkrivenaKartica","IspodZaglavljaRedova","4",KUMPATH))
  nPZRSK:=VAL(IzFMKINI("SkrivenaKartica","PrijeZaglavljaRedova","2",KUMPATH))
  nKRSK:=VAL(IzFMKINI("SkrivenaKartica","KarticaRedova","52",KUMPATH))
  nDMSK:=VAL(IzFMKINI("SkrivenaKartica","DMarginaRedova","5",KUMPATH))
  l2kolone:=(IzFMKINI("SkrivenaKartica","Dvokolonski","D",KUMPATH)=="D")
ENDIF

#ifdef CPOR
cVarSort:="2"
#else
cVarSort:="1"
#endif

if pcount()<4
 cIdRadn  := space(_LR_)
 cIdRj    := gRj
 cmjesec  := gMjesec
 cGodina  := gGodina
 cObracun := gObracun

 O_PAROBR
 O_RJ
 O_RADN
 O_VPOSLA
 O_RADKR
 O_KRED
#ifdef CPOR
 IF Pitanje(,"Izvjestaj se pravi za isplacene(D) ili neisplacene(N) radnike?","D")=="D"
   lIsplaceni:=.t.
   O_LD
 ELSE
   lIsplaceni:=.f.
   select (F_LDNO)  ; usex (KUMPATH+"LDNO") alias LD; set order to 1
 ENDIF
#else
 O_LD
#endif
else
#ifdef CPOR
 lIsplaceni:=.t.
#endif
 cObracun:=cObrac
endif

private nC1:=20+LEN(cLMSK)

cVarijanta:=" "
c2K1L:="D"

if pcount()<4

 O_PARAMS
 Private cSection:="4",cHistory:=" ",aHistory:={}
 RPar("VS",@cVarSort)
 RPar("2K",@c2K1L)

 cIdRadn:=space(_LR_)

 Box(,7,75)
   @ m_x+1,m_y+2 SAY "Radna jedinica (prazno-sve rj): "  GET cIdRJ valid empty(cidrj) .or. P_RJ(@cidrj)
   @ m_x+2,m_y+2 SAY "Mjesec: "  GET  cmjesec  pict "99"
   IF lViseObr
     @ m_x+2,col()+2 SAY "Obracun: "  GET  cObracun WHEN HelpObr(.t.,cObracun) VALID ValObr(.t.,cObracun)
   ENDIF
   @ m_x+3,m_y+2 SAY "Godina: "  GET  cGodina  pict "9999"
   @ m_x+4,m_y+2 SAY "Radnik (prazno-svi radnici): "  GET  cIdRadn  valid empty(cIdRadn) .or. P_Radn(@cIdRadn)
   IF !lSkrivena
    @ m_x+5,m_y+2 SAY "Varijanta ( /5): "  GET  cVarijanta valid cVarijanta $ " 5"
   ENDIF
   @ m_x+6,m_y+2 SAY "Ako su svi radnici, sortirati po (1-sifri,2-prezime+ime)"  GET cVarSort VALID cVarSort$"12"  pict "9"
   IF !lSkrivena
    @ m_x+7,m_y+2 SAY "Dvije kartice na jedan list ? (D/N)"  GET c2K1L VALID c2K1L$"DN"  pict "@!"
   ENDIF
   read; clvbox(); ESC_BCR
 BoxC()

 WPar("VS",cVarSort)
 WPar("2K",c2K1L)
 SELECT PARAMS; USE

 if lViseObr
   O_TIPPRN
 else
   O_TIPPR
 endif

endif

PoDoIzSez(cGodina,cMjesec)

if cVarijanta=="5"
  O_LDSM
endif

SELECT LD

cIdRadn:=trim(cidradn)

IF EMPTY(cIdRadn) .and. cVarSort=="2"
  IF EMPTY(cIdRj)
    IF lViseObr .and. !EMPTY(cObracun)
      INDEX ON str(godina)+str(mjesec)+obr+SortPrez(idradn)+idrj TO "TMPLD"
      seek str(cGodina,4)+str(cmjesec,2)+cObracun+cIdRadn
    ELSE
      INDEX ON str(godina)+str(mjesec)+SortPrez(idradn)+idrj TO "TMPLD"
      seek str(cGodina,4)+str(cmjesec,2)+cIdRadn
    ENDIF
    cIdrj:=""
  ELSE
    IF lViseObr .and. !EMPTY(cObracun)
      INDEX ON str(godina)+idrj+str(mjesec)+obr+SortPrez(idradn) TO "TMPLD"
      seek str(cGodina,4)+cidrj+str(cmjesec,2)+cObracun+cIdRadn
    ELSE
      INDEX ON str(godina)+idrj+str(mjesec)+SortPrez(idradn) TO "TMPLD"
      seek str(cGodina,4)+cidrj+str(cmjesec,2)+cIdRadn
    ENDIF
  ENDIF
ELSE
  if empty(cidrj)
    set order to tag (TagVO("2"))
    seek str(cGodina,4)+str(cmjesec,2)+;
         IF(lViseObr.and.!EMPTY(cObracun),cObracun,"")+cIdRadn
    cIdrj:=""
  else
    IF PCOUNT()<4
      SET ORDER TO TAG (TagVO("1"))
    ENDIF
    seek str(cGodina,4)+cidrj+str(cmjesec,2)+;
         IF(lViseObr.and.!EMPTY(cObracun),cObracun,"")+cIdRadn
  endif
ENDIF

EOF CRET

nStrana:=0

select vposla; hseek ld->idvposla
select rj; hseek ld->idrj; select ld

if pcount()>=4
  START PRINT RET
else
  START PRINT CRET
endif

12CPI

IF lSkrivena
  gRPL_Gusto()
ENDIF

ParObr(cmjesec,IF(lViseObr,cObracun,),cIdRj)

bZagl:={|| ZaglKar()}

nRbrKart:=0

nT1:=nT2:=nT3:=nT4:=0
do while !eof() .and. cgodina==godina .and. idrj=cidrj .and. cmjesec=mjesec .and.;
         idradn=cIdRadn .and.;
         !( lViseObr .and. !EMPTY(cObracun) .and. obr<>cObracun )

 aNeta:={}

 m:=cLMSK+"----------------------- --------  ----------------   ------------------"

 IF lViseObr .and. EMPTY(cObracun)
   ScatterS(Godina,Mjesec,IdRJ,IdRadn)
 ELSE
   Scatter()
 ENDIF

#ifdef CPOR
 select kred; hseek _idkred
#endif
 select radn; hseek _idradn
 select vposla; hseek _idvposla
 select rj; hseek _idrj; select ld

 AADD(aNeta,{vposla->idkbenef,_UNeto})

 //-- Prikaz samo odredjenih doprinosa na kartici plate
 //-- U fmk.ini /kumpath se definisu koji dopr. da se prikazuju
 //-- Po defaultu stoji prazno - svi doprinosi

 cPrikDopr := IzFmkIni("LD","DoprNaKartPl","D",KUMPATH)
 lPrikSveDopr := .f.

 IF cPrikDopr == "D"; lPrikSveDopr := .t.; ENDIF

 if gPrBruto<>"X"

         Eval(bZagl)
         if gTipObr=="2" .and. parobr->k1<>0
           ?? "        Bod-sat:"; @ prow(),pcol()+1 SAY parobr->vrbod/parobr->k1*brbod pict "99999.99999"
         endif
         IF l2kolone
           COND2
           // aRCPos  := { PROW() , PCOL() }
           cDefDev := SET(_SET_PRINTFILE)
           SvratiUFajl()
           // SETPRC(0,0)
         ENDIF
         ? m
         ? cLMSK+" Vrsta                  Opis         sati/iznos             ukupno"
         ? m
         cUneto:="D"
         for i:=1 to cLDPolja
          cPom:=padl(alltrim(str(i)),2,"0")
          select tippr; seek cPom
          if tippr->uneto=="N" .and. cUneto=="D"
            cUneto:="N"
            ? m
            ? cLMSK+"UKUPNO NETO:"; @ prow(),nC1+8  SAY  _USati  pict gpics; ?? " sati"
            @ prow(),60+LEN(cLMSK) SAY _UNeto pict gpici; ?? "",gValuta
            ? m
          endif

          if tippr->(found()) .and. tippr->aktivan=="D"
           if _i&cpom<>0 .or. _s&cPom<>0

            // uvodi se djoker # : Primjer: Naziv tipa primanja
            // je: REDOVAN RAD BOD #RADN->N1 -> naci RADN->N1
            // i ispisati REDOVAN RAD BOD 12.0
            nDJ:=at("#",tippr->naz)
#ifdef CPOR
            aRez:=GodMj(_godina,_mjesec,-val(tippr->id)+1)
            cTpnaz:=padr("Za "+;
                         iif(tippr->id='14','<= ','')+;
                         str(arez[2],2)+"/"+str(arez[1],4),;
                         len(tippr->naz))
#else
            cTPNaz:=tippr->naz
#endif
            if nDJ<>0
               cDJ:=trim(substr(tippr->naz,nDJ+1))
               if type(cDJ)="C"
                   cTPNaz:=left(tippr->naz,nDJ-1)+&cDJ
               elseif type(cPom)="N"
                   cTPNAZ:=left(tippr->naz,nDJ-1)+alltrim(str(&cDJ))
               endif
            endif
            ? cLMSK+tippr->id+"-"+padr(cTPNAZ,len(tippr->naz)),tippr->opis
            nC1:=pcol()
            if tippr->fiksan $ "DN"
               @ prow(),pcol()+8 SAY _s&cPom  pict gpics; ?? " s"
               @ prow(),60+LEN(cLMSK) say _i&cPom        pict gpici
            elseif tippr->fiksan=="P"
               @ prow(),pcol()+8 SAY _s&cPom  pict "999.99%"
               @ prow(),60+LEN(cLMSK) say _i&cPom        pict gpici
            elseif tippr->fiksan=="B"
               @ prow(),pcol()+8 SAY _s&cPom  pict "999999"; ?? " b"
               @ prow(),60+LEN(cLMSK) say _i&cPom        pict gpici
            elseif tippr->fiksan=="C"
               @ prow(),60+LEN(cLMSK) say _i&cPom        pict gpici
            endif
            if "SUMKREDITA" $ tippr->formula .and. gReKrKP=="1"
              IF l2kolone
                COND2
              ELSE
                COND
              ENDIF
              ? m
#ifdef CPOR
              ? cLMSK+"  ","Od toga pojedinacna rjesenja"
#else
              ? cLMSK+"  ","Od toga pojedinacni krediti:"
#endif
              select radkr; set order to 1
              seek str(_godina,4)+str(_mjesec,2)+_idradn
              do while !eof() .and. _godina==godina .and. _mjesec=mjesec .and. idradn==_idradn
               select kred; hseek radkr->idkred; select radkr
               ? cLMSK+"  ",idkred,left(kred->naz,22),naosnovu
               @ prow(),58+LEN(cLMSK) SAY iznos pict "("+gpici+")"
               skip
              enddo
              ? m
              IF l2kolone
                COND2
              ELSE
                12CPI
              ENDIF
              select ld
            elseif "SUMKREDITA" $ tippr->formula
              //? m
              //? "  ","Od toga pojedinacni krediti:"
              select radkr; set order to 1
              seek str(_godina,4)+str(_mjesec,2)+_idradn
              ukredita:=0
              IF l2kolone
                COND2
              ELSE
                COND
              ENDIF
              ? m2:=cLMSK+"   ------------------------------------------------  --------- --------- -------"
              ?     cLMSK+"        Kreditor      /              na osnovu         Ukupno    Ostalo   Rata"
              ? m2
              do while !eof() .and. _godina==godina .and. _mjesec=mjesec .and. idradn==_idradn
               select kred; hseek radkr->idkred; select radkr
               aIznosi:=OKreditu(idradn,idkred,naosnovu)
               ? cLMSK+" ",idkred,left(kred->naz,22),PADR(naosnovu,20)
               @ prow(),pcol()+1 SAY aIznosi[1] pict "999999.99" // ukupno
               @ prow(),pcol()+1 SAY aIznosi[1]-aIznosi[2] pict "999999.99"// ukupno-placeno
               @ prow(),pcol()+1 SAY iznos pict "9999.99"
               ukredita+=iznos
               skip
              enddo
              IF l2kolone
                COND2
              ELSE
                12CPI
              ENDIF

              select ld
            endif
           endif
          endif
         next
         ?
         ? m
         ?  cLMSK+"UKUPNO ZA ISPLATU";  @ prow(),60+LEN(cLMSK) SAY _UIznos pict gpici; ?? "",gValuta
         ? m

         if cVarijanta=="5"
           select ldsm
           hseek "1"+str(_godina,4)+str(_mjesec,2)+_idradn+_idrj
           ?
           ? cLMSK+"Od toga 1. dio:" ; @ prow(),60+LEN(cLMSK) SAY UIznos pict gpici
           ? m
           hseek "2"+str(_godina,4)+str(_mjesec,2)+_idradn+_idrj
           ? cLMSK+"Od toga 2. dio:" ; @ prow(),60+LEN(cLMSK) SAY UIznos pict gpici
           ? m
           select ld
         else

         endif

         if lSkrivena
         elseif prow()>31 .and. gPotp=="N"
             FF
         else
           if gPrBruto<>"D" .and. gPotp=="N"
             ?
             ?
             ?
             ?
           endif
         endif

         if gPrBruto=="D"  // prikaz bruto iznosa
          select (F_POR)
          if !used(); O_POR; endif
          select (F_DOPR)
          if !used(); O_DOPR; endif
          select (F_KBENEF)
          if !used(); O_KBENEF; endif

          m:=cLMSK+"----------------------- -------- ------------- -------------"
          nBO:=0
          nBo:=round2(parobr->k3/100*_UNeto,gZaok2)
          IF lSkrivena
           ? m
          ELSE
           ?
           ?
          ENDIF
          select por; go top
          nPom:=nPor:=0
          nC1:=30+LEN(cLMSK); nPorOl:=0
          do while !eof()
            PozicOps(POR->poopst)
            IF !ImaUOp("POR",POR->id)
              SKIP 1; LOOP
            ENDIF
            ? cLMSK+id,"-",naz
            @ prow(),pcol()+1 SAY iznos pict "99.99%"
            nC1:=pcol()+1
            @ prow(),pcol()+1 SAY _UNeto pict gpici
            @ prow(),pcol()+1 SAY nPom:=max(dlimit,round(iznos/100*_UNeto,gZaok2)) pict gpici
            nPor+=nPom
            skip
          enddo
          if radn->porol<>0  .and. gDaPorOl=="D" .and. !Obr2_9()  // poreska olaksica
            if alltrim(cVarPorOl)=="2"
              nPorOl:=RADN->porol
            elseif alltrim(cVarPorol)=="1"
              nPorOl:=round(parobr->prosld*radn->porol/100,gZaok)
            else
              nPorOl:= &("_I"+cVarPorol)
            endif
            ? cLMSK+"PORESKA OLAKSICA"
            if nPorOl>nPor // poreska olaksica ne moze biti veca od poreza
              nPorOl:=nPor
            endif
            if cVarPorOl=="2"
              @ prow(),pcol()+1 SAY ""
            else
              @ prow(),pcol()+1 SAY radn->PorOl pict "99.99%"
            endif
            @ prow(),nC1 SAY parobr->prosld pict gpici
            @ prow(),pcol()+1 SAY nPorOl    pict gpici
          endif
           if radn->porol<>0 .and. gDaPorOl=="D" .and. !Obr2_9()
             ? m
             ? cLMSK+"Ukupno Porez"
             @ prow(),nC1 SAY space(len(gpici))
             @ prow(),pcol()+1 SAY nPor-nPorOl pict gpici
             ? m
           endif
           if !lSkrivena .and. prow()>55+gPStranica; FF; endif
           ?
           ?
           ?
           m:=cLMSK+"----------------------- -------- ------------- -------------"
           select dopr; go top
           nPom:=nDopr:=0
           nC1:=20+LEN(cLMSK)
           do while !eof()
            PozicOps(DOPR->poopst)
            IF !ImaUOp("DOPR",DOPR->id) .or. !lPrikSveDopr .and. !DOPR->ID $ cPrikDopr
              SKIP 1; LOOP
            ENDIF
            if right(id,1)=="X"
             ? m
            endif
            ? cLMSK+id,"-",naz

            @ prow(),pcol()+1 SAY iznos pict "99.99%"

            if empty(idkbenef) // doprinos udara na neto
              @ prow(),pcol()+1 SAY nBO pict gpici
              nC1:=pcol()+1
              @ prow(),pcol()+1 SAY nPom:=max(dlimit,round(iznos/100*nBO,gZaok2)) pict gpici
            else
              nPom0:=ASCAN(aNeta,{|x| x[1]==idkbenef})
              if nPom0<>0
                nPom2:=parobr->k3/100*aNeta[nPom0,2]
              else
                nPom2:=0
              endif
              if round(nPom2,gZaok2)<>0
                @ prow(),pcol()+1 SAY nPom2 pict gpici
                nC1:=pcol()+1
                nPom:=max(dlimit,round(iznos/100*nPom2,gZaok2))
                //if round(iznos,4)=0 .and. dlimit>0  // fuell boss
                //  nPom:=nLjudi*dlimit
                //endif
                @ prow(),pcol()+1 SAY nPom pict gpici

              endif
             endif

             if right(id,1)=="X"
               ? m
               ?
             nDopr+=nPom
             endif

             if !lSkrivena .and. prow()>57+gPStranica; FF; endif
             skip
           enddo // doprinosi

           m:=cLMSK+"--------------------------"

           //if prow()>31
           if gPotp<>"D"
            if pcount()==0; FF; endif
           endif
           //else
           //   ?
           //   ?
           //   ?
           //   ?
           //endif

         endif // gPrBruto

         if l2kolone
           SET PRINTER TO (cDefDev) ADDITIVE
           // SETPRC(aRCPos[1],aRCPos[2])
           altd()
           IF PROW()+2+nDMSK>nKRSK*(2-(nRBrKart%2))
             aTekst:=U2Kolone(PROW()+2+nDMSK-nKRSK*(2-(nRBrKart%2)))
             FOR i:=1 TO LEN(aTekst)
               IF i==1
                 ?? aTekst[i]
               ELSE
                 ? aTekst[i]
               ENDIF
             NEXT
             SETPRC(nKRSK*(2-(nRBrKart%2))-2-nDMSK,PCOL())
           ELSE
             PRINTFILE(PRIVPATH+"xoutf.txt")
           ENDIF
         endif

         if lSkrivena
           if prow()<nKRSK+5
             nPom:=nKRSK-PROW()
             FOR i:=1 TO nPom; ?; NEXT
           else
             FF
           endif
         elseif gPotp=="D"
           ?
           ? cLMSK+space(5),"   Obracunao:  ",space(30),"    Potpis:"
           ? cLMSK+space(5),"_______________",space(30),"_______________"
           if pcount()==0 .and.  prow()>31
             FF
           else
            ?
            ?
            ?
            ?
           endif
         endif
 else
 ************************ opcina zenica ...............
 ************************ opcina zenica ...............
 ************************ opcina zenica ...............
 ************************ opcina zenica ...............
 ************************ opcina zenica ...............
 ************************ opcina zenica ...............
 ************************ opcina zenica ...............
 ************************ opcina zenica ...............
         Eval(bZagl)
         if gTipObr=="2" .and. parobr->k1<>0
           ?? "        Bod-sat:"; @ prow(),pcol()+1 SAY parobr->vrbod/parobr->k1*brbod pict "99999.999"
         endif
         IF l2kolone
           COND2
           // aRCPos := { PROW() , PCOL() }
           cDefDev := SET(_SET_PRINTFILE)
           SvratiUFajl()
           // SETPRC(0,0)
         ENDIF
         ? m
         ? cLMSK+" Vrsta                  Opis         sati/iznos             ukupno"
         ? m
         private nC1:=30+LEN(cLMSK)
         for i:=1 to cLDPolja
          cPom:=padl(alltrim(str(i)),2,"0")
          select tippr; seek cPom
          if tippr->uneto=="D"
            PrikPrimanje()
          endif
         next
         ? m
         ? cLMSK+"UKUPNO NETO:"; @ prow(),nC1+8  SAY  _USati  pict gpics; ?? " sati"
         @ prow(),60+LEN(cLMSK) SAY _UNeto pict gpici; ?? "",gValuta
         ? m

         nBruto:=_UNETO
         nPorDopr:=0
         select (F_POR)
         if !used(); O_POR; endif
         select (F_DOPR)
         if !used(); O_DOPR; endif
         select (F_KBENEF)
         if !used(); O_KBENEF; endif
         nBO:=0
         nBo:=round2(parobr->k3/100*_UNeto,gZaok2)
         select por; go top
         nPom:=nPor:=0
         nC1:=30+LEN(cLMSK); nPorOl:=0
         do while !eof()
            PozicOps(POR->poopst)
            IF !ImaUOp("POR",POR->id)
              SKIP 1; LOOP
            ENDIF
            IF !lSkrivena
              ? cLMSK+id,"-",naz
              @ prow(),pcol()+1 SAY iznos pict "99.99%"
              nC1:=pcol()+1
              //@ prow(),pcol()+1 SAY _UNeto pict gpici
              @ prow(),39+LEN(cLMSK)  SAY nPom:=max(dlimit,round(iznos/100*_UNeto,gZaok2)) pict gpici
            ELSE
              nPom:=max(dlimit,round(iznos/100*_UNeto,gZaok2))
            ENDIF
            nPor+=nPom
            skip
         enddo
         nBruto+=nPor
         nPorDopr+=nPor
         if radn->porol<>0 .and. gDaPorOl=="D" .and. !Obr2_9() // poreska olaksica
            if alltrim(cVarPorOl)=="2"
              nPorOl:=RADN->porol
            elseif alltrim(cVarPorol)=="1"
              nPorOl:=round(parobr->prosld*radn->porol/100,gZaok)
            else
              nPorOl:= &("_I"+cVarPorol)
            endif
            IF !lSkrivena
              ? cLMSK+"PORESKA OLAKSICA"
            ENDIF
            if nPorOl>nPor // poreska olaksica ne moze biti veca od poreza
              nPorOl:=nPor
            endif
            IF !lSkrivena
              if cVarPorOl=="2"
                @ prow(),pcol()+1 SAY ""
              else
                @ prow(),pcol()+1 SAY radn->PorOl pict "99.99%"
              endif
              //@ prow(),nC1 SAY parobr->prosld pict gpici
              @ prow(),39+LEN(cLMSK) SAY nPorOl    pict gpici
            ENDIF
            nBruto-=nPorol
            nPorDopr-=nPorOl
         endif
         IF !lSkrivena
           if radn->porol<>0 .and. gDaPorOl=="D" .and. !Obr2_9()
             ? m
             ? cLMSK+"Ukupno Porez"
               //@ prow(),nC1 SAY space(len(gpici))
               @ prow(),39+LEN(cLMSK) SAY nPor-nPorOl pict gpici
              ? m
           endif
         ENDIF
         select dopr; go top
         nPom:=nDopr:=0
         nC1:=20+LEN(cLMSK)
         do while !eof()  // DOPRINOSI
          PozicOps(DOPR->poopst)
          IF !ImaUOp("DOPR",DOPR->id)
            SKIP 1; LOOP
          ENDIF
          if right(id,1)<>"X"
            SKIP; LOOP
          endif
          IF !lSkrivena
            ? cLMSK+id,"-",naz
            @ prow(),pcol()+1 SAY iznos pict "99.99%"
          ENDIF
          if empty(idkbenef) // doprinos udara na neto
            //@ prow(),pcol()+1 SAY nBO pict gpici
            nC1:=pcol()+1
            nPom:=max(dlimit,round(iznos/100*nBO,gZaok2))
            if round(iznos,4)=0 .and. dlimit>0  // fuell boss
              nPom:=1*dlimit   // kartica plate
            endif
            IF !lSkrivena
              @ prow(),39+LEN(cLMSK) SAY nPom pict gpici
            ENDIF
            nDopr+=nPom
            nBruto+=nPom
            nPorDopr+=nPom
          else
            nPom0:=ASCAN(aNeta,{|x| x[1]==idkbenef})
            if nPom0<>0
              nPom2:=parobr->k3/100*aNeta[nPom0,2]
            else
              nPom2:=0
            endif
            if round(nPom2,gZaok2)<>0
              //@ prow(),pcol()+1 SAY nPom2 pict gpici
              nC1:=pcol()+1
              IF !lSkrivena
                @ prow(),39+LEN(cLMSK) SAY nPom:=max(dlimit,round(iznos/100*nPom2,gZaok2)) pict gpici
              ENDIF
              nDopr+=nPom
              nBruto+=nPom
              nPorDopr+=nPom
            endif
          endif

          skip
         enddo // doprinosi
         IF lSkrivena
           ? cLMSK+"UKUPNO POREZ:"+TRANSFORM(nPor-nPorOl,gPicI)+;
                   ", DOPRINOSI:"+TRANSFORM(nDopr,gPicI)+;
                   ", POR.+DOPR.:"+TRANSFORM(nPorDopr,gPicI)
         ELSE
           ? m
           ? cLMSK+"UKUPNO POREZ+DOPRINOSI"
           @ prow(),39+LEN(cLMSK) SAY nPorDopr pict gpici
         ENDIF
         ? m
         ? cLMSK+"BRUTO IZNOS"
         @ prow(),60+LEN(cLMSK) SAY nBruto pict gpici
         ? m

         SELECT LD
         IF !lSkrivena
          ?
          ? m
         ENDIF
         ? cLMSK+"- OBUSTAVE :"
         ? m
         private fImaNak:=.f.,nObustave:=0
         for i:=1 to cLDPolja
          cPom:=padl(alltrim(str(i)),2,"0")
          select tippr; seek cPom
          if tippr->uneto=="N" .and. _i&cPom<0
             PrikPrimanje()
             nObustave+=abs(_i&cPom)
          elseif tippr->uneto=="N" .and. _i&cPom>0
             fimaNak:=.t.
          endif
         next
         if nObustave>0
           ? m
           ? cLMSK+"UKUPNO OBUSTAVE:"
           @ prow(),60+LEN(cLMSK) SAY nObustave pict gpici
           ? m
         endif

         if fImaNak
          if !(nObustave>0)
            ? m
          endif
          ? cLMSK+"+ NAKNADE (primanja van neta):"
          ? m
         endif
         for i:=1 to cLDPolja
          cPom:=padl(alltrim(str(i)),2,"0")
          select tippr; seek cPom
          if tippr->uneto=="N" .and. _i&cPom>0
             PrikPrimanje()
          endif
         next
         IF !lSkrivena
           ?
         ENDIF
         ? m
         ?  cLMSK+"UKUPNO ZA ISPLATU :";  @ prow(),60+LEN(cLMSK) SAY _UIznos pict gpici; ?? "",gValuta
         ? m

         if cVarijanta=="5"
           select ldsm
           hseek "1"+str(_godina,4)+str(_mjesec,2)+_idradn+_idrj
           ?
           ? cLMSK+"Od toga 1. dio:" ; @ prow(),60+LEN(cLMSK) SAY UIznos pict gpici
           ? m
           hseek "2"+str(_godina,4)+str(_mjesec,2)+_idradn+_idrj
           ? cLMSK+"Od toga 2. dio:" ; @ prow(),60+LEN(cLMSK) SAY UIznos pict gpici
           ? m
           select ld
         endif

         // FF                                                           //
                                                                         //
         if gPotp=="D" .and. !lSkrivena                                  //
           ?                                                             //
           ? cLMSK+space(5),"   Obracunao:  ",space(30),"    Potpis:"          //
           ? cLMSK+space(5),"_______________",space(30),"_______________"      //
         endif

         if l2kolone
           SET PRINTER TO (cDefDev) ADDITIVE
           // SETPRC(aRCPos[1],aRCPos[2])
           altd()
           IF PROW()+2+nDMSK>nKRSK*(2-(nRBrKart%2))
             aTekst:=U2Kolone(PROW()+2+nDMSK-nKRSK*(2-(nRBrKart%2)))
             FOR i:=1 TO LEN(aTekst)
               IF i==1
                 ?? aTekst[i]
               ELSE
                 ? aTekst[i]
               ENDIF
             NEXT
             SETPRC(nKRSK*(2-(nRBrKart%2))-2-nDMSK,PCOL())
           ELSE
             PRINTFILE(PRIVPATH+"xoutf.txt")
           ENDIF
         endif

         if lSkrivena
           if prow()<nKRSK+5
             nPom:=nKRSK-PROW()
             FOR i:=1 TO nPom; ?; NEXT
           else
             FF
           endif
         elseif pcount()==0 .and. prow()>31                            //
           FF                                                          //
         else                                                          //
           ?                                                           //
           ?                                                           //
           ?                                                           //
           ?                                                           //
         endif                                                         //

 ************************ opcina zenica ...............
 endif


 nT1+=_usati; nT2+=_uneto; nT3+=_uodbici; nT4+=_uiznos

 select ld
 skip

enddo

IF lSkrivena
  gRPL_Normal()
  IF !pcount()>=4 .and. (nRBrKart%2)==1; FF; ENDIF
ENDIF

if pcount()>=4  // predji na drugu stranu
 FF
endif

END PRINT

if pcount()<4
 closeret
else // pcount >= "4"
 set order to tag (TagVO("1"))
 return
endif

**********************
**********************
function ZaglKar()

++nRBrKart

IF !lSkrivena .and. c2K1L=="D" .and. (nRBrKart%2)==0
  DO WHILE prow()<33; ?; ENDDO
ENDIF

#ifdef CPOR
? "SLUZBA:",gNFirma
? IF(lIsplaceni,"","NEISPLACENI ")+"OBRACUN NAKNADE ZA PORODILJSKO ODSUSTVO ZA "+str(mjesec,2)+"/"+str(godina,4)
? "RJ:",idrj,rj->naz
? idradn,"-",RADNIK,"  Mat.br:",radn->matbr
? "Reg.br.PIO:",radn->rbrpio
? "Poslodavac:",_idkred, trim(kred->naz)
if KRED->(FIELDPOS("ADRESA"))<>0
  ?? ",", trim(kred->adresa)
endif
#else
IF lSkrivena

  FOR i:=1 TO nPZRSK; ?; NEXT
  12CPI
  ?? cLMSK+"OBRACUN PLATE ZA "+str(mjesec,2)+IspisObr()+"/"+str(godina,4)," ZA "+UPPER(TRIM(gTS)),gNFirma

  ? cLMSK+idradn,"-",RADNIK,"  Mat.br:",radn->matbr
  ? cLMSK+"Radno mjesto:",radn->rmjesto,"  STR.SPR:",IDSTRSPR
  ? cLMSK+"Vrsta posla :",idvposla,vposla->naz,"         Radi od:",radn->datod
  FOR i:=1 TO nIZRSK
    ?
  NEXT
  ? cLMSK+IF(gBodK=="1","Broj bodova :","Koeficijent :"),transform(brbod,"99999.99"),space(24)
  if gMinR=="B"
    ?? "Minuli rad:",transform(kminrad,"9999.99")
  else
    ?? "K.Min.rada:",transform(kminrad,"99.99%")
  endif
  ? cLMSK+IF(gBodK=="1","Vrijednost boda:","Vr.koeficijenta:"), transform(parobr->vrbod,"99999.99999")
ELSE
  ? "OBRACUN PLATE ZA "+str(mjesec,2)+IspisObr()+"/"+str(godina,4)," ZA "+UPPER(TRIM(gTS)),gNFirma

  ? "RJ:",idrj,rj->naz
  ? idradn,"-",RADNIK,"  Mat.br:",radn->matbr
  ? "Radno mjesto:",radn->rmjesto,"  STR.SPR:",IDSTRSPR
  ? "Vrsta posla :",idvposla,vposla->naz,"         Radi od:",radn->datod
  ? IF(gBodK=="1","Broj bodova :","Koeficijent :"),transform(brbod,"99999.99"),space(24)
  if gMinR=="B"
    ?? "Minuli rad:",transform(kminrad,"9999.99")
  else
    ?? "K.Min.rada:",transform(kminrad,"99.99%")
  endif
  ? IF(gBodK=="1","Vrijednost boda:","Vr.koeficijenta:"), transform(parobr->vrbod,"99999.99999")
ENDIF
#endif
return


*********************************
* prikaz primanja, opcina zenica
*********************************
function PrikPrimanje()
local nIznos:=0
IF "U" $ TYPE("cLMSK"); cLMSK:=""; ENDIF
IF "U" $ TYPE("l2kolone"); l2kolone:=.f.; ENDIF
if tippr->(found()) .and. tippr->aktivan=="D"
 if _i&cpom<>0 .or. _s&cPom<>0
  ? cLMSK+tippr->id+"-"+tippr->naz,tippr->opis
  nC1:=pcol()
  if tippr->uneto=="N"
     nIznos:=abs(_i&cPom)
  else
     nIznos:=_i&cPom
  endif
  if tippr->fiksan $ "DN"
     @ prow(),pcol()+8 SAY _s&cPom  pict gpics; ?? " s"
     @ prow(),60+LEN(cLMSK) say niznos        pict gpici
  elseif tippr->fiksan=="P"
     @ prow(),pcol()+8 SAY _s&cPom  pict "999.99%"
     @ prow(),60+LEN(cLMSK) say niznos        pict gpici
  elseif tippr->fiksan=="B"
     @ prow(),pcol()+8 SAY abs(_s&cPom)  pict "999999"; ?? " b"
     @ prow(),60+LEN(cLMSK) say niznos        pict gpici
  elseif tippr->fiksan=="C"
    if !("SUMKREDITA" $ tippr->formula)
      @ prow(),60+LEN(cLMSK) say niznos        pict gpici
    endif
  endif
  if "SUMKREDITA" $ tippr->formula
    //? m
    //? "  ","Od toga pojedinacni krediti:"
    select radkr; set order to 1
    seek str(_godina,4)+str(_mjesec,2)+_idradn
    ukredita:=0
    IF l2kolone
      COND2
    ELSE
      COND
    ENDIF
? m2:=cLMSK+" ------------------------------------------- --------- --------- -------"
    ? cLMSK+"    Kreditor   /             na osnovu         Ukupno    Ostalo   Rata"
    ? m2
    do while !eof() .and. _godina==godina .and. _mjesec=mjesec .and. idradn==_idradn
     select kred; hseek radkr->idkred; select radkr
     aIznosi:=OKreditu(idradn,idkred,naosnovu)
     ? cLMSK,idkred,left(kred->naz,15),PADR(naosnovu,20)
     @ prow(),pcol()+1 SAY aIznosi[1] pict "999999.99" // ukupno
     @ prow(),pcol()+1 SAY aIznosi[1]-aIznosi[2] pict "999999.99"// ukupno-placeno
     @ prow(),pcol()+1 SAY iznos pict "9999.99"
     ukredita+=iznos
     skip
    enddo
    if round(ukredita-niznos,2)<>0
     set device to screen
     Beep(2)
     Msg("Za radnika "+_idradn+" iznos sume kredita ne odgovara stanju baze kredita !",6)
     set device to printer
    endif

     //? m
    IF l2kolone
      COND2
    ELSE
      12CPI
    ENDIF
    select ld
  endif
  if "_K" == RIGHT( ALLTRIM(tippr->opis) , 2 )
    nKumPrim:=KumPrim(_IdRadn,cPom)

    if substr(alltrim(tippr->opis), 2,1)=="1"
      nKumPrim := nkumprim + radn->n1
    elseif  substr(alltrim(tippr->opis), 2,1)=="2"
      nKumPrim := nkumprim + radn->n2
    elseif  substr(alltrim(tippr->opis), 2,1)=="3"
      nKumPrim := nkumprim + radn->n3
    endif

    IF tippr->uneto=="N"; nKumPrim:=ABS(nKumPrim); ENDIF
    ? m2:=cLMSK+"   ----------------------------- ----------------------------"
        ? cLMSK+"    SUMA IZ PRETHODNIH OBRA¨UNA   UKUPNO (SA OVIM OBRA¨UNOM)"
        ? m2
        ? cLMSK+"   "+PADC(STR(nKumPrim-nIznos),29)+" "+PADC(STR(nKumPrim),28)
        ? m2
  endif
 endif
endif


FUNCTION KumPrim(cIdRadn,cIdPrim)
 LOCAL j:=0, nVrati:=0, nOdGod:=0, nDoGod:=0
 cPom77:=cIdPrim
  IF cIdRadn==NIL; cIdRadn:=""; ENDIF
    SELECT LD
    PushWA()
    SET ORDER TO TAG (TagVO("4"))
    GO BOTTOM; nDoGod:=godina
    GO TOP; nOdGod:=godina
    FOR j:=nOdGod TO nDoGod
      GO TOP
      seek STR(j,4)+cIdRadn
      DO WHILE godina==j .and. cIdRadn==IdRadn
        nVrati += i&cPom77
        SKIP 1
      ENDDO
    NEXT
    SELECT LD
    PopWA()
RETURN nVrati



********************************
function Rekap(fSvi)
*
*
********************************
local nC1:=20, i
local cTPNaz, cUmPD:="N", nKrug:=1

#ifdef CPOR
 cUmPD:="D"
#endif

cIdRj:=gRj; cmjesec:=gMjesec; cGodina:=gGodina
cObracun:=gObracun
cMjesecDo:=cMjesec

if fSvi==NIL
 fSvi:=.f.
endif

O_POR
O_DOPR
O_PAROBR
O_RJ
O_RADN
O_STRSPR
O_KBENEF
O_VPOSLA
O_OPS
O_RADKR
O_KRED

#ifdef CPOR
 IF Pitanje(,"Izvjestaj se pravi za isplacene(D) ili neisplacene(N) radnike?","D")=="D"
   lIsplaceni:=.t.
   O_LD
 ELSE
   lIsplaceni:=.f.
   select (F_LDNO)  ; usex (KUMPATH+"LDNO") alias LD; set order to 1
 ENDIF
#else
 O_LD
#endif

cIdRadn:=space(_LR_); cStrSpr:=space(3); cOpsSt:=space(4); cOpsRad :=space(4)

if fSvi
// za sve radne jedinice
qqRJ:=SPACE(60)
Box(,10,75)

DO WHILE .t.
 #ifdef CPOR
 if lIsplaceni
   @ m_x+2,m_y+2 SAY "Umanjiti poreze i doprinose za preplaceni iznos? (D/N)"  GET cUmPD VALID cUmPD $ "DN" PICT "@!"
 endif
 #endif
 @ m_x+3,m_y+2 SAY "Radne jedinice: "  GET  qqRJ PICT "@!S25"
 @ m_x+4,m_y+2 SAY "Za mjesece od:"  GET  cmjesec  pict "99" VALID {|| cMjesecDo:=cMjesec,.t.}
 @ m_x+4,col()+2 SAY "do:"  GET  cMjesecDo  pict "99" VALID cMjesecDo>=cMjesec
 if lViseObr
   @ m_x+4,col()+2 SAY "Obracun: " GET cObracun WHEN HelpObr(.t.,cObracun) VALID ValObr(.t.,cObracun)
 endif
 @ m_x+5,m_y+2 SAY "Godina: "  GET  cGodina  pict "9999"
 @ m_x+7,m_y+2 SAY "Strucna Sprema: "  GET  cStrSpr pict "@!" valid empty(cStrSpr) .or. P_StrSpr(@cStrSpr)
 @ m_x+8,m_y+2 SAY "Opstina stanovanja: "  GET  cOpsSt pict "@!" valid empty(cOpsSt) .or. P_Ops(@cOpsSt)
 @ m_x+9,m_y+2 SAY "Opstina rada:       "  GET  cOpsRad  pict "@!" valid empty(cOpsRad) .or. P_Ops(@cOpsRad)

 read; clvbox(); ESC_BCR
 aUsl1:=Parsiraj(qqRJ,"IDRJ")
 aUsl2:=Parsiraj(qqRJ,"ID")
 if aUsl1<>NIL.and.aUsl2<>NIL; exit; endif
ENDDO

BoxC()

if lViseObr
  O_TIPPRN
else
  O_TIPPR
endif

SELECT LD

if lViseObr
  cObracun:=TRIM(cObracun)
else
  cObracun:=""
endif

// CREATE_INDEX("LDi2","str(godina)+str(mjesec)+idradn","LD")
set order to tag (TagVO("2"))

PRIVATE cFilt1:=""
cFilt1 := ".t." + IF(EMPTY(cStrSpr),"",".and.IDSTRSPR=="+cm2str(cStrSpr))+;
		  IF(EMPTY(qqRJ),"",".and."+aUsl1)

IF cMjesec!=cMjesecDo
  cFilt1 := cFilt1 + ".and.mjesec>="+cm2str(cMjesec)+;
                     ".and.mjesec<="+cm2str(cMjesecDo)+;
                     ".and.godina="+cm2str(cGodina)
ENDIF

if lViseObr
  cFilt1 += ".and. OBR="+cm2str(cObracun)
endif

cFilt1 := STRTRAN(cFilt1,".t..and.","")

IF cFilt1==".t."
  SET FILTER TO
ELSE
  SET FILTER TO &cFilt1
ENDIF

IF cMjesec==cMjesecDo
  seek str(cGodina,4)+str(cmjesec,2)+cObracun
  EOF CRET
ELSE
  GO TOP
ENDIF

else
//****** samo jedna radna jedinica
Box(,8,75)
 #ifdef CPOR
 if lIsplaceni
   @ m_x+1,m_y+2 SAY "Umanjiti poreze i doprinose za preplaceni iznos? (D/N)"  GET cUmPD VALID cUmPD $ "DN" PICT "@!"
 endif
 #endif
 @ m_x+2,m_y+2 SAY "Radna jedinica: "  GET cIdRJ
 @ m_x+3,m_y+2 SAY "Za mjesece od:"  GET  cmjesec  pict "99" VALID {|| cMjesecDo:=cMjesec,.t.}
 @ m_x+3,col()+2 SAY "do:"  GET  cMjesecDo  pict "99" VALID cMjesecDo>=cMjesec
 if lViseObr
   @ m_x+3,col()+2 SAY "Obracun: " GET cObracun WHEN HelpObr(.t.,cObracun) VALID ValObr(.t.,cObracun)
 endif
 @ m_x+4,m_y+2 SAY "Godina: "  GET  cGodina  pict "9999"
 @ m_x+6,m_y+2 SAY "Strucna Sprema: "  GET  cStrSpr pict "@!" valid empty(cStrSpr) .or. P_StrSpr(@cStrSpr)
 @ m_x+7,m_y+2 SAY "Opstina stanovanja: "  GET  cOpsSt pict "@!" valid empty(cOpsSt) .or. P_Ops(@cOpsSt)
 @ m_x+8,m_y+2 SAY "Opstina rada:       "  GET  cOpsRad  pict "@!" valid empty(cOpsRad) .or. P_Ops(@cOpsRad)
 read; clvbox(); ESC_BCR
BoxC()

if lViseObr
  O_TIPPRN
else
  O_TIPPR
endif

SELECT LD

if lViseObr
  cObracun:=TRIM(cObracun)
else
  cObracun:=""
endif

set order to tag (TagVO("1"))

PRIVATE cFilt1:=""
cFilt1 := ".t." + IF(EMPTY(cStrSpr),"",".and.IDSTRSPR=="+cm2str(cStrSpr))

IF cMjesec!=cMjesecDo
  cFilt1 := cFilt1 + ".and.mjesec>="+cm2str(cMjesec)+;
                     ".and.mjesec<="+cm2str(cMjesecDo)+;
                     ".and.godina="+cm2str(cGodina)
ENDIF

if lViseObr
  cFilt1 += ".and. OBR="+cm2str(cObracun)
endif

cFilt1 := STRTRAN(cFilt1,".t..and.","")

IF cFilt1==".t."
  SET FILTER TO
ELSE
  SET FILTER TO &cFilt1
ENDIF

// IF cMjesec==cMjesecDo
  seek str(cGodina,4)+cidrj+str(cmjesec,2)+cObracun
  EOF CRET
// ELSE
//   GO TOP
// ENDIF

endif
//********************* fsvi

PoDoIzSez(cGodina,cMjesecDo)

nStrana:=0
m:="------------------------  ----------------              -------------------"



aDbf:={   {"ID"    ,"C", 1,0},;
          {"IDOPS" ,"C", 4,0},;
          {"IZNOS" ,"N",25,4},;
          {"IZNOS2","N",25,4},;
          {"LJUDI" ,"N", 10,0};
      }

#ifdef CPOR
  AADD(aDbf, {"PIZNOS" ,"N",25,4})
  AADD(aDbf, {"PIZNOS2","N",25,4})
  AADD(aDbf, {"PLJUDI" ,"N", 10,0})
#endif

//id- 1 opsstan
//id- 2 opsrad
DBCREATE2(PRIVPATH+"opsld",aDbf)


select(F_OPSLD) ; usex (PRIVPATH+"opsld")
INDEX ON ID+IDOPS tag "1"
index ON  BRISANO TAG "BRISAN"
use


// napraviti   godina, mjesec, idrj, cid , iznos1, iznos2
aDbf:={    {"GODINA"     ,  "C" , 4, 0 } ,;
           {"MJESEC"     ,  "C" , 2, 0 } ,;
           {"ID"         ,  "C" , 30, 0} ,;
           {"opis"       ,  "C" , 20, 0} ,;
           {"opis2"      ,  "C" , 35, 0} ,;
           {"iznos1"     ,  "N" , 25, 4} ,;
           {"iznos2"     ,  "N" , 25, 4} ;
        }
#ifdef CPOR
  AADD( aDbf , {"idpartner"  ,  "C" , 10, 0} )
#else
  AADD( aDbf , {"idpartner"  ,  "C" ,  6, 0} )
#endif

DBCREATE2(KUMPATH+"REKLD",aDbf)

select (F_REKLD); usex (KUMPATH+"rekld")
index ON  BRISANO+"10" TAG "BRISAN"
index on  godina+mjesec+id  tag "1"
set order to tag "1"
use

O_REKLD; O_OPSLD
select ld

START PRINT CRET
10CPI

ParObr(cmjesec,IF(lViseObr,cObracun,),IF(!fSvi,cIdRj,))      // samo pozicionira bazu PAROBR na odgovarajuÜi zapis

private aRekap[cLDPolja,2]

for i:=1 to cLDPolja
  aRekap[i,1]:=0
  aRekap[i,2]:=0
next

nT1:=nT2:=nT3:=nT4:=0
nUNeto:=0
nUIznos:=nUSati:=nUNeto:=nUOdbici:=nUOdbiciP:=nUOdbiciM:=0
nLjudi:=0

private aNeta:={}

select ld

IF cMjesec!=cMjesecDo
 if fSvi
   private bUslov:={|| cgodina==godina .and. mjesec>=cmjesec .and. mjesec<=cMjesecDo .and. IF(lViseObr,obr=cObracun,.t.) }
 else
   private bUslov:={|| cgodina==godina .and. cidrj==idrj .and. mjesec>=cmjesec .and. mjesec<=cMjesecDo .and. IF(lViseObr,obr=cObracun,.t.) }
 endif
ELSE
 if fSvi
   private bUslov:={|| cgodina==godina .and. cmjesec=mjesec .and. IF(lViseObr,obr=cObracun,.t.) }
 else
   private bUslov:={|| cgodina==godina .and. cidrj==idrj .and. cmjesec=mjesec .and. IF(lViseObr,obr=cObracun,.t.) }
 endif
ENDIF

nPorOl:=nUPorOl:=0

aNetoMj:={}

do while !eof() .and. eval(bUSlov)           // vrti se u bazi LD.DBF *******

 if lViseObr .and. EMPTY(cObracun)
   ScatterS(godina,mjesec,idrj,idradn)
 else
   Scatter()
 endif

 select radn; hseek _idradn
 select vposla; hseek _idvposla

 if (!empty(copsst) .and. copsst<>radn->idopsst)  .or.;
    (!empty(copsrad) .and. copsrad<>radn->idopsrad)
   select ld
   skip 1; loop
 endif

 select por; go top
 nPor:=nPorOl:=0
 do while !eof()  // datoteka por
   PozicOps(POR->poopst)
   IF !ImaUOp("POR",POR->id)
     SKIP 1; LOOP
   ENDIF
   nPor+=round2(max(dlimit,iznos/100*_UNeto),gZaok2)
   skip
 enddo
 if radn->porol<>0 .and. gDaPorOl=="D" .and. !Obr2_9() // poreska olaksica
   if alltrim(cVarPorOl)=="2"
     nPorOl:=RADN->porol
   elseif alltrim(cVarPorol)=="1"
     nPorOl:=round(parobr->prosld*radn->porol/100,gZaok)
   else
     nPorOl:= &("_I"+cVarPorol)
   endif
   if nPorOl>nPor // poreska olaksica ne moze biti veca od poreza
     nPorOl:=nPor
   endif
   nUPorOl+=nPorOl
 endif

 //**** nafiluj datoteku OPSLD *********************
 select ops; seek radn->idopsst
 select opsld
 seek "1"+radn->idopsst
 if found()
   replace iznos with iznos+_uneto, iznos2 with iznos2+nPorOl, ljudi with ljudi+1
 else
   append blank
   replace id with "1", idops with radn->idopsst, iznos with _uneto,;
                        iznos2 with iznos2+nPorOl, ljudi with 1
 endif
 seek "3"+ops->idkan
 if found()
   replace iznos with iznos+_uneto, iznos2 with iznos2+nPorOl, ljudi with ljudi+1
 else
   append blank
   replace id with "3", idops with ops->idkan, iznos with _uneto,;
                        iznos2 with iznos2+nPorOl, ljudi with 1
 endif
 seek "5"+ops->idn0
 if found()
   replace iznos with iznos+_uneto, iznos2 with iznos2+nPorOl, ljudi with ljudi+1
 else
   append blank
   replace id with "5", idops with ops->idn0, iznos with _uneto,;
                        iznos2 with iznos2+nPorOl, ljudi with 1
 endif
 select ops; seek radn->idopsrad
 select opsld
 seek "2"+radn->idopsrad
 if found()
   replace iznos with iznos+_uneto, iznos2 with iznos2+nPorOl , ljudi with ljudi+1
 else
   append blank
   replace id with "2", idops with radn->idopsrad, iznos with _uneto,;
                        iznos2 with iznos2+nPorOl, ljudi with 1
 endif
 seek "4"+ops->idkan
 if found()
   replace iznos with iznos+_uneto, iznos2 with iznos2+nPorOl, ljudi with ljudi+1
 else
   append blank
   replace id with "4", idops with ops->idkan, iznos with _uneto,;
                        iznos2 with iznos2+nPorOl, ljudi with 1
 endif
 seek "6"+ops->idn0
 if found()
   replace iznos with iznos+_uneto, iznos2 with iznos2+nPorOl, ljudi with ljudi+1
 else
   append blank
   replace id with "6", idops with ops->idn0, iznos with _uneto,;
                        iznos2 with iznos2+nPorOl, ljudi with 1
 endif
 select ld
 //*************************



 nPom:=ASCAN(aNeta,{|x| x[1]==vposla->idkbenef})
 if nPom==0
    AADD(aNeta,{vposla->idkbenef,_UNeto})
 else
    aNeta[nPom,2]+=_UNeto
 endif

 for i:=1 to cLDPolja
  cPom:=padl(alltrim(str(i)),2,"0")
  select tippr; seek cPom; select ld
  #ifdef CPOR
    n777:=i+cMjesecDO-_mjesec
    aRekap[IF(n777>cLDPolja,cLDPolja,n777),1]+=_s&cPom  // sati
  #else
    aRekap[i,1]+=_s&cPom  // sati
  #endif
  nIznos:=_i&cPom
  #ifdef CPOR
    n777:=i+cMjesecDO-_mjesec
    aRekap[IF(n777>cLDPolja,cLDPolja,n777),2]+=nIznos  // iznos
  #else
    aRekap[i,2]+=nIznos  // iznos
  #endif
  if tippr->uneto=="N" .and. nIznos<>0
    if nIznos>0
      nUOdbiciP+=nIznos
    else
      nUOdbiciM+=nIznos
    endif
  endif
 next

 ++nLjudi
 nUSati+=_USati   // ukupno sati
 nUNeto+=_UNeto  // ukupno neto iznos
 nUIznos+=_UIznos  // ukupno iznos
 nUOdbici+=_UOdbici  // ukupno odbici

 IF cMjesec<>cMjesecDo
   nPom:=ASCAN(aNetoMj,{|x| x[1]==mjesec})
   IF nPom>0
     aNetoMj[nPom,2] += _uneto
     aNetoMj[nPom,3] += _usati
   ELSE
     nTObl:=SELECT()
     nTRec := PAROBR->(RECNO())
     ParObr(mjesec,IF(lViseObr,cObracun,),IF(!fSvi,cIdRj,))      // samo pozicionira bazu PAROBR na odgovarajuÜi zapis
     AADD(aNetoMj,{mjesec,_uneto,_usati,PAROBR->k3,PAROBR->k1})
     SELECT PAROBR; GO (nTRec)
     SELECT (nTObl)
   ENDIF
 ENDIF

 IF RADN->isplata=="TR"  // isplata na tekuci racun
   Rekapld( "IS_"+RADN->idbanka , cgodina , cmjesecDo ,;
            _UIznos , 0 , RADN->idbanka , RADN->brtekr , RADNIK , .t. )
 ENDIF

 select ld
 skip

enddo                                        // vrti se u bazi LD.DBF *******

if nLjudi==0
  nLjudi:=9999999
endif
B_ON
?? "LD: Rekapitulacija primanja"
B_OFF
#ifdef CPOR
 ?? IF(lIsplaceni,"","-neisplaceni radnici-")
#endif
if !empty(cstrspr)
 ?? " za radnike strucne spreme ",cStrSpr
endif
if !empty(cOpsSt)
 ? "Opstina stanovanja:",cOpsSt
endif
if !empty(cOpsRad)
 ? "Opstina rada:",cOpsRad
endif

if fSvi
 select rj
 ? "Obuhvacene radne jedinice: "
 IF !EMPTY(qqRJ)
  SET FILTER TO &aUsl2
  GO TOP
  DO WHILE !EOF()
   ?? id+" - "+naz
   ? SPACE(27)
   SKIP 1
  ENDDO
 ELSE
  ?? "SVE"
  ?
 ENDIF
 B_ON
 IF cMjesec==cMjesecDo
   ? "Firma:",gNFirma,"  Mjesec:",str(cmjesec,2)+IspisObr()
   ?? "    Godina:", str(cGodina,4)
   B_OFF
   ? IF(gBodK=="1","Vrijednost boda:","Vr.koeficijenta:"), transform(parobr->vrbod,"99999.99999")
 ELSE
   ? "Firma:",gNFirma,"  Za mjesece od:",str(cmjesec,2),"do",str(cmjesecDo,2)+IspisObr()
   ?? "    Godina:", str(cGodina,4)
   B_OFF
   // ? IF(gBodK=="1","Vrijednost boda:","Vr.koeficijenta:"), transform(parobr->vrbod,"99999.99999")
 ENDIF
 ?

else
// ************** ne fSvi
 select rj; hseek cIdrj; select ld
 ?
 B_ON
 IF cMjesec==cMjesecDo
   ? "RJ:",cidrj,rj->naz,"  Mjesec:",str(cmjesec,2)+IspisObr()
   ?? "    Godina:", str(cGodina,4)
   B_OFF
   #ifndef CPOR
     ? IF(gBodK=="1","Vrijednost boda:","Vr.koeficijenta:"), transform(parobr->vrbod,"99999.99999")
   #endif
 ELSE
   ? "RJ:",cidrj,rj->naz,"  Za mjesece od:",str(cmjesec,2),"do",str(cmjesecDo,2)+IspisObr()
   ?? "    Godina:", str(cGodina,4)
   B_OFF
 ENDIF
 ?
endif // fsvi

? m
cUNeto:="D"
for i:=1 to cLDPolja

  If prow()>55+gPStranica
    FF
  endif

  //********************* 90 - ke
  cPom:=padl(alltrim(str(i)),2,"0")
  _s&cPom:=aRekap[i,1]   // nafiluj ove varijable radi prora~una dodatnih stavki
  _i&cPom:=aRekap[i,2]
  //**********************

  cPom:=padl(alltrim(str(i)),2,"0")
  select tippr; seek cPom
  if tippr->uneto=="N" .and. cUneto=="D"
    cUneto:="N"
    ? m
    ? "UKUPNO NETO:"; @ prow(),nC1+8  SAY  nUSati  pict gpics; ?? " sati"
    @ prow(),60 SAY nUNeto pict gpici; ?? "",gValuta
    // ****** radi 90 - ke
    _UNeto:=nUNeto
    _USati:=nUSati
    //***********
    ? m
  endif


  if tippr->(found()) .and. tippr->aktivan=="D" .and. (aRekap[i,2]<>0 .or. aRekap[i,1]<>0)
#ifdef CPOR
            aRez:=GodMj(_godina,_mjesec,-val(tippr->id)+1)
            cTpnaz:=padr("Za "+;
                         iif(tippr->id='14','<= ','')+;
                         str(arez[2],2)+"/"+str(arez[1],4),;
                         len(tippr->naz))
#else
            cTPNaz:=tippr->naz
#endif
   ? tippr->id+"-"+cTPNaz
   nC1:=pcol()
   if tippr->fiksan $ "DN"
     @ prow(),pcol()+8 SAY aRekap[i,1]  pict gpics; ?? " s"
     @ prow(),60 say aRekap[i,2]      pict gpici
   elseif tippr->fiksan=="P"
     @ prow(),pcol()+8 SAY aRekap[i,1]/nLjudi pict "999.99%"
     @ prow(),60 say aRekap[i,2]        pict gpici
   elseif tippr->fiksan=="C"
     @ prow(),60 say aRekap[i,2]        pict gpici
   elseif tippr->fiksan=="B"
       @ prow(),pcol()+8 SAY aRekap[i,1] pict "999999"; ?? " b"
       @ prow(),60 say aRekap[i,2]      pict gpici
   endif
   IF cMjesec==cMjesecDo
     Rekapld("PRIM"+tippr->id,cgodina,cmjesec,aRekap[i,2],aRekap[i,1])
   ELSE
     Rekapld("PRIM"+tippr->id,cgodina,cMjesecDo,aRekap[i,2],aRekap[i,1])
   ENDIF

#ifndef CPOR  // za porodilje to bi bio veliki spisak
    if "SUMKREDITA" $ tippr->formula
      if gReKrOs=="X"
        ? m
        ? "  ","Od toga pojedinacni krediti:"
        SELECT RADKR; SET ORDER TO TAG "3"
        SET FILTER TO STR(cGodina,4)+STR(cMjesec,2)<=STR(godina,4)+STR(mjesec,2) .and.;
                      STR(cGodina,4)+STR(cMjesecDo,2)>=STR(godina,4)+STR(mjesec,2)
        GO TOP
        DO WHILE !EOF()
          cIdKred:=IDKRED
          SELECT KRED; HSEEK cIdKred; SELECT RADKR
          nUkKred := 0
          DO WHILE !EOF() .and. IDKRED==cIdKred
            cNaOsnovu:=NAOSNOVU; cIdRadnKR:=IDRADN
            SELECT RADN; HSEEK cIdRadnKR; SELECT RADKR
            cOpis2   := RADNIK
            nUkKrRad := 0
            DO WHILE !EOF() .and. IDKRED==cIdKred .and. cNaOsnovu==NAOSNOVU .and. cIdRadnKR==IDRADN
              mj:=mjesec
              if fSvi
               select ld; set order to tag (TagVO("2")); hseek  str(cGodina,4)+str(mj,2)+if(lViseObr.and.!EMPTY(cObracun),cObracun,"")+radkr->idradn
               //"LDi2","str(godina)+str(mjesec)+idradn"
              else
                select ld; hseek  str(cGodina,4)+cidrj+str(mj,2)+if(lViseObr.and.!EMPTY(cObracun),cObracun,"")+radkr->idradn
              endif // fsvi
              select radkr
              if ld->(found())
                nUkKred  += iznos
                nUkKrRad += iznos
              endif
              SKIP 1
            ENDDO
            if nUkKrRad<>0
              Rekapld("KRED"+cidkred+cnaosnovu,cgodina,cmjesecDo,nUkKrRad,0,cidkred,cnaosnovu,cOpis2,.t.)
            endif
          ENDDO
          IF nUkKred<>0    // ispisati kreditora
            if prow()>55+gPStranica
              FF
            endif
            ? "  ",cidkred,left(kred->naz,22)
            @ prow(),58 SAY nUkKred  pict "("+gpici+")"
          ENDIF
        ENDDO
      else
        ? m
        ? "  ","Od toga pojedinacni krediti:"
        cOpis2:=""
        select radkr; set order to 3  ; go top
        //"RADKRi3","idkred+naosnovu+idradn+str(godina)+str(mjesec)","RADKR")
        do while !eof()
         select kred; hseek radkr->idkred; select radkr
         private cidkred:=idkred, cNaOsnovu:=naosnovu
         select radn; hseek radkr->idradn; select radkr
         cOpis2:= RADNIK
         seek cidkred+cnaosnovu
         private nUkKred:=0
         do while !eof() .and. idkred==cidkred .and. ( cnaosnovu==naosnovu .or. gReKrOs=="N" )
          if fSvi
           select ld; set order to tag (TagVO("2")); hseek  str(cGodina,4)+str(cmjesec,2)+if(lViseObr.and.!EMPTY(cObracun),cObracun,"")+radkr->idradn
           //"LDi2","str(godina)+str(mjesec)+idradn"
          else
            select ld; hseek  str(cGodina,4)+cidrj+str(cmjesec,2)+if(lViseObr.and.!EMPTY(cObracun),cObracun,"")+radkr->idradn
          endif // fsvi
          select radkr
          if ld->(found()) .and. godina==cgodina .and. mjesec=cmjesec
            nUkKred+=iznos
          endif
          IF cMjesecDo>cMjesec
            FOR mj:=cMjesec+1 TO cMjesecDo
              if fSvi
               select ld; set order to tag (TagVO("2")); hseek  str(cGodina,4)+str(mj,2)+if(lViseObr.and.!EMPTY(cObracun),cObracun,"")+radkr->idradn
               //"LDi2","str(godina)+str(mjesec)+idradn"
              else
                select ld; hseek  str(cGodina,4)+cidrj+str(mj,2)+if(lViseObr.and.!EMPTY(cObracun),cObracun,"")+radkr->idradn
              endif // fsvi
              select radkr
              if ld->(found()) .and. godina==cgodina .and. mjesec=mj
                nUkKred+=iznos
              endif
            NEXT
          ENDIF
          skip
         enddo
         if nukkred<>0
          If prow()>55+gPStranica
            FF
          endif
          ? "  ",cidkred,left(kred->naz,22),IF(gReKrOs=="N","",cnaosnovu)
          @ prow(),58 SAY nUkKred  pict "("+gpici+")"
          IF cMjesec==cMjesecDo
            Rekapld("KRED"+cidkred+cnaosnovu,cgodina,cmjesec,nukkred,0,cidkred,cnaosnovu, cOpis2)
          ELSE
            Rekapld("KRED"+cidkred+cnaosnovu,cgodina,cMjesecDo,nukkred,0,cidkred,cnaosnovu, cOpis2)
          ENDIF
         endif
        enddo
        select ld
      endif
    endif
#endif  //CPOR

  endif   // tippr aktivan

next  // cldpolja


? m
?  "UKUPNO ZA ISPLATU";  @ prow(),60 SAY nUIznos pict gpici; ?? "",gValuta
? m
?

// proizvoljni redovi pocinju sa "9"
?
select tippr; seek "9"
do while !eof() .and. left(id,1)="9"
  If prow()>55+gPStranica
    FF
  endif
  ? tippr->id+"-"+tippr->naz
  cPom:=tippr->formula
  @ prow(),60 say round2(&cPom,gZaok2)      pict gpici
  IF cMjesec==cMjesecDo
    Rekapld("PRIM"+tippr->id,cgodina,cmjesec,round2(&cpom,gZaok2),0)
  ELSE
    Rekapld("PRIM"+tippr->id,cgodina,cMjesecDo,round2(&cpom,gZaok2),0)
  ENDIF
  skip
enddo
? m


IF cMjesec==cMjesecDo     // za viÁe mjeseci nema prikaza poreza i doprinosa
?
nBO:=0
? "Koef. Bruto osnove (KBO):",transform(parobr->k3,"999.99999%")
?? space(3),"BRUTO OSNOVA = NETO*KBO ="
@ prow(),pcol()+1 SAY nBo:=round2(parobr->k3/100*nUNeto,gZaok2)  pict gpici
?

#ifdef CPOR
 IF cUmPD=="D"
   IF cMjesec==1
     cGodina2:=cGodina-1; cMjesec2:=12
   ELSE
     cGodina2:=cGodina; cMjesec2:=cMjesec-1
   ENDIF
   SELECT PAROBR
   nParRec:=RECNO()
   HSEEK STR(cMjesec2,2)+cObracun
   SELECT LD
   PushWA()
   USE
   select (F_LDNO); usex (KUMPATH+"LDNO") alias LD

   PRIVATE cFilt1:=""
   cFilt1 := ".t." + IF(EMPTY(cStrSpr),"",".and.IDSTRSPR=="+cm2str(cStrSpr))+;
                     IF(!fSvi.or.EMPTY(qqRJ),"",".and."+aUsl1)
   cFilt1 := STRTRAN(cFilt1,".t..and.","")
   IF cFilt1==".t."
     SET FILTER TO
   ELSE
     SET FILTER TO &cFilt1
   ENDIF

   if fSvi // sve radne jedinice
     set order to 2
     seek str(cGodina2,4)+str(cmjesec2,2)
   else
     set order to 1
     seek str(cGodina2,4)+cidrj+str(cmjesec2,2)
   endif

   nT1:=nT2:=nTPor:=nTDopr:=0
   n01:=0  // van neta plus
   n02:=0  // van neta minus
   do while !eof() .and.  cgodina2==godina .and. cmjesec2=mjesec .and. ( fSvi .or. cidrj==idrj )
    Scatter()
    select radn; hseek _idradn
    select vposla; hseek _idvposla
    select kbenef; hseek vposla->idkbenef
    select ld
    if (!empty(copsst) .and. copsst<>radn->idopsst)  .or.;
       (!empty(copsrad) .and. copsrad<>radn->idopsrad)
      skip 1; loop
    endif


    // neophodno zbog "po opstinama"
    ********************************
    select por; go top
    nPor:=nPorOl:=nUPorOl2:=0
    do while !eof()  // datoteka por
      PozicOps(POR->poopst)
      IF !ImaUOp("POR",POR->id)
        SKIP 1; LOOP
      ENDIF
      nPor+=round2(max(dlimit,iznos/100*_UNeto),gZaok2)
      skip
    enddo
    if radn->porol<>0 .and. gDaPorOl=="D" .and. !Obr2_9() // poreska olaksica
      if alltrim(cVarPorOl)=="2"
        nPorOl:=RADN->porol
      elseif alltrim(cVarPorol)=="1"
        nPorOl:=round(parobr->prosld*radn->porol/100,gZaok)
      else
        nPorOl:= &("_I"+cVarPorol)
      endif
      if nPorOl>nPor // poreska olaksica ne moze biti veca od poreza
        nPorOl:=nPor
      endif
      nUPorOl2+=nPorOl
    endif

    //**** nafiluj datoteku OPSLD *********************
    select ops; seek radn->idopsst
    select opsld
    seek "1"+radn->idopsst
    if found()
      replace piznos with piznos+_uneto, piznos2 with piznos2+nPorOl, pljudi WITH pljudi+1
    else
      append blank
      replace id with "1", idops   with radn->idopsst, piznos with _uneto,;
                           piznos2 with piznos2+nPorOl, pljudi WITH 1
    endif
    seek "3"+ops->idkan  // kanton stanovanja
    if found()
      replace piznos with piznos+_uneto, piznos2 with piznos2+nPorOl, pljudi WITH pljudi+1
    else
      append blank
      replace id with "3", idops   with ops->idkan, piznos with _uneto,;
                           piznos2 with piznos2+nPorOl, pljudi WITH 1
    endif
    seek "5"+ops->idn0  // entitet stanovanja
    if found()
      replace piznos with piznos+_uneto, piznos2 with piznos2+nPorOl, pljudi WITH pljudi+1
    else
      append blank
      replace id with "5", idops   with ops->idn0, piznos with _uneto,;
                           piznos2 with piznos2+nPorOl, pljudi WITH 1
    endif


    select ops; seek radn->idopsst
    select opsld
    seek "2"+radn->idopsrad
    if found()
      replace piznos with piznos+_uneto, piznos2 with piznos2+nPorOl, pljudi WITH pljudi+1
    else
      append blank
      replace id with "2", idops   with radn->idopsrad, piznos with _uneto,;
                           piznos2 with piznos2+nPorOl, pljudi WITH 1
    endif
    seek "4"+ops->idkan  // kanton rada
    if found()
      replace piznos with piznos+_uneto, piznos2 with piznos2+nPorOl, pljudi WITH pljudi+1
    else
      append blank
      replace id with "4", idops   with ops->idkan, piznos with _uneto,;
                           piznos2 with piznos2+nPorOl, pljudi WITH 1
    endif
    seek "6"+ops->idn0  // entitet rada
    if found()
      replace piznos with piznos+_uneto, piznos2 with piznos2+nPorOl, pljudi WITH pljudi+1
    else
      append blank
      replace id with "6", idops   with ops->idn0, piznos with _uneto,;
                           piznos2 with piznos2+nPorOl, pljudi WITH 1
    endif
    ********************************

    select ld
    n01:=0; n02:=0
    for i:=1 to cLDPolja
     cPom:=padl(alltrim(str(i)),2,"0")
     select tippr; seek cPom; select ld

     if tippr->(found()) .and. tippr->aktivan=="D"
      nIznos:=_i&cpom
      if cpom=="01"
         n01+=nIznos
      else
         n02+=nIznos
      endif
     endif
    next
    nT1+=n01
    nT2+=n02
    skip 1
   enddo  // LD
   nUNeto2:=nT1+nT2
   nBo2:=round2(parobr->k3/100*nUNeto2,gZaok2)
   nPK3:=PAROBR->K3
   USE
   SELECT PAROBR
   GO (nParRec)
   O_LD
   PopWA()
 ENDIF
#endif



select por; go top
nPom:=nPor:=nPor2:=nPorOps:=nPorOps2:=0
nC1:=20

m:="----------------------- -------- ----------- -----------"
if cUmPD=="D"
  m+=" ----------- -----------"
endif

if cUmPD=="D"
  12CPI
  ? "----------------------- -------- ----------- ----------- ----------- -----------"
  ? "                                 Obracunska     Porez    Preplaceni     Porez   "
  ? "     Naziv poreza          %      osnovica   po obracunu    porez     za uplatu "
  ? "          (1)             (2)        (3)     (4)=(2)*(3)     (5)     (6)=(4)-(5)"
  ? "----------------------- -------- ----------- ----------- ----------- -----------"
endif

do while !eof()

  If prow()>55+gPStranica
    FF
  endif

   ? id,"-",naz
   @ prow(),pcol()+1 SAY iznos pict "99.99%"
   nC1:=pcol()+1

   if !empty(poopst)
     if poopst=="1"
       ?? " (po opst.stan)"
     elseif poopst=="2"
       ?? " (po opst.stan)"
     elseif poopst=="3"
       ?? " (po kant.stan)"
     elseif poopst=="4"
       ?? " (po kant.rada)"
     elseif poopst=="5"
       ?? " (po ent. stan)"
     elseif poopst=="6"
       ?? " (po ent. rada)"
       ?? " (po opst.rada)"
     endif
     nOOP:=0      // ukupna Osnovica za Obraüun Poreza za po opÁtinama
     nPOLjudi:=0  // ukup.ljudi za po opÁtinama
     nPorOps:=0
     nPorOps2:=0
     select opsld
     seek por->poopst
     ? strtran(m,"-","=")
     do while !eof() .and. id==por->poopst   //idopsst
         select ops; hseek opsld->idops; select opsld
         IF !ImaUOp("POR",POR->id)
           SKIP 1; LOOP
         ENDIF
         ? idops,ops->naz
         @ prow(),nc1 SAY iznos picture gpici
         @ prow(),pcol()+1 SAY nPom:=round2(max(por->dlimit,por->iznos/100*iznos),gZaok2) pict gpici
         if cUmPD=="D"
           // ______  PORLD ______________
           @ prow(),pcol()+1 SAY nPom2:=round2(max(por->dlimit,por->iznos/100*piznos),gZaok2) pict gpici
           @ prow(),pcol()+1 SAY nPom-nPom2 pict gpici
           Rekapld("POR"+por->id+idops,cgodina,cmjesec,nPom-nPom2,0,idops,NLjudi())
           nPorOps2+=nPom2
         else
           Rekapld("POR"+por->id+idops,cgodina,cmjesec,nPom,iznos,idops,NLjudi())
         endif
         nOOP += iznos
         nPOLjudi += ljudi
         nPorOps+=nPom
         skip
         if prow()>62+gPStranica; FF; endif
     enddo
     select por
     ? m
     nPor+=nPorOps
     nPor2+=nPorOps2
   endif // poopst
   if !empty(poopst)
     ? m
     ? "Ukupno:"
//     @ prow(),nc1 SAY nUNeto pict gpici
     @ prow(),nc1 SAY nOOP pict gpici
     @ prow(),pcol()+1 SAY nPorOps   pict gpici
     if cUmPD=="D"
       @ prow(),pcol()+1 SAY nPorOps2   pict gpici
       @ prow(),pcol()+1 SAY nPorOps-nPorOps2   pict gpici
       Rekapld("POR"+por->id,cgodina,cmjesec,nPorOps-nPorOps2,0,,NLjudi())
     else
//       Rekapld("POR"+por->id,cgodina,cmjesec,nPorOps,nUNeto,,NLjudi())
       Rekapld("POR"+por->id,cgodina,cmjesec,nPorOps,nOOP,,"("+ALLTRIM(STR(nPOLjudi))+")")
     endif
     ? m
   else
     @ prow(),nc1 SAY nUNeto pict gpici
     @ prow(),pcol()+1 SAY nPom:=round2(max(dlimit,iznos/100*nUNeto),gZaok2) pict gpici
     if cUmPD=="D"
       @ prow(),pcol()+1 SAY nPom2:=round2(max(dlimit,iznos/100*nUNeto2),gZaok2) pict gpici
       @ prow(),pcol()+1 SAY nPom-nPom2 pict gpici
       Rekapld("POR"+por->id,cgodina,cmjesec,nPom-nPom2,0)
       nPor2+=nPom2
     else
       Rekapld("POR"+por->id,cgodina,cmjesec,nPom,nUNeto,,"("+ALLTRIM(STR(nLjudi))+")")
     endif
     nPor+=nPom
   endif


  skip
enddo
if round2(nUPorOl,2)<>0 .and. gDaPorOl=="D" .and. !Obr2_9()
   ? "PORESKE OLAKSICE"
   select por; go top
   nPOlOps:=0
   if !empty(poopst)
      if poopst=="1"
       ?? " (po opst.stan)"
      else
       ?? " (po opst.rada)"
      endif
      nPOlOps:=0
      select opsld
      seek por->poopst
      do while !eof() .and. id==por->poopst
         If prow()>55+gPStranica
           FF
         endif
         select ops; hseek opsld->idops; select opsld
         IF !ImaUOp("POR",POR->id)
           SKIP 1; LOOP
         ENDIF
         ? idops, ops->naz
         @ prow(), nc1 SAY parobr->prosld picture gpici
         @ prow(), pcol()+1 SAY round2(iznos2,gZaok2)    picture gpici
         Rekapld("POROL"+por->id+opsld->idops,cgodina,cmjesec,round2(iznos2,gZaok2),0,opsld->idops,NLjudi())
         skip
         if prow()>62+gPStranica; FF; endif
      enddo
      select por
      ? m
      ? "UKUPNO POR.OL"
   endif // poopst
   @ prow(),nC1 SAY parobr->prosld  pict gpici
   @ prow(),pcol()+1 SAY round2(nUPorOl,gZaok2)    pict gpici
   Rekapld("POROL"+por->id,cgodina,cmjesec,round2(nUPorOl,gZaok2),0,,"("+ALLTRIM(STR(nLjudi))+")")
   if !empty(poopst); ? m; endif

endif
? m
? "Ukupno Porez"
@ prow(),nC1 SAY space(len(gpici))
@ prow(),pcol()+1 SAY nPor-nUPorOl pict gpici
if cUmPD=="D"
  @ prow(),pcol()+1 SAY nPor2              pict gpici
  @ prow(),pcol()+1 SAY nPor-nUPorOl-nPor2 pict gpici
endif
? m
?
?
?
if prow()>55+gpStranica; FF; endif


m:="----------------------- -------- ----------- -----------"
if cUmPD=="D"
  m+=" ----------- -----------"
endif
select dopr; go top
nPom:=nDopr:=0
nPom2:=nDopr2:=0
nC1:=20

if cUmPD=="D"
  ? "----------------------- -------- ----------- ----------- ----------- -----------"
  ? "                                 Obracunska   Doprinos   Preplaceni   Doprinos  "
  ? "    Naziv doprinosa        %      osnovica   po obracunu  doprinos    za uplatu "
  ? "          (1)             (2)        (3)     (4)=(2)*(3)     (5)     (6)=(4)-(5)"
  ? "----------------------- -------- ----------- ----------- ----------- -----------"
endif

do while !eof()
  if prow()>55+gpStranica; FF; endif

  if right(id,1)=="X"
   ? m
  endif
  ? id,"-",naz

  @ prow(),pcol()+1 SAY iznos pict "99.99%"
  nC1:=pcol()+1

  if empty(idkbenef) // doprinos udara na neto

    altd()
    if !empty(poopst)
      if poopst=="1"
        ?? " (po opst.stan)"
      elseif poopst=="2"
        ?? " (po opst.rada)"
      elseif poopst=="3"
        ?? " (po kant.stan)"
      elseif poopst=="4"
        ?? " (po kant.rada)"
      elseif poopst=="5"
        ?? " (po ent. stan)"
      elseif poopst=="6"
        ?? " (po ent. rada)"
      endif
      ? strtran(m,"-","=")
      nOOD:=0          // ukup.osnovica za obraüun doprinosa za po opÁtinama
      nPOLjudi:=0      // ukup.ljudi za po opÁtinama
      nDoprOps:=0
      nDoprOps2:=0
      select opsld
      seek dopr->poopst
      altd()
      do while !eof() .and. id==dopr->poopst
        altd()
        select ops; hseek opsld->idops; select opsld
        IF !ImaUOp("DOPR",DOPR->id)
          SKIP 1; LOOP
        ENDIF
        ? idops,ops->naz
        nBOOps:=round2(iznos*parobr->k3/100,gZaok2)
        @ prow(),nc1 SAY nBOOps picture gpici
        nPom:=round2(max(dopr->dlimit,dopr->iznos/100*nBOOps),gZaok2)
        if cUmPD=="D"
          nBOOps2:=round2(piznos*nPK3/100,gZaok2)
          nPom2:=round2(max(dopr->dlimit,dopr->iznos/100*nBOOps2),gZaok2)
        endif
        if round(dopr->iznos,4)=0 .and. dopr->dlimit>0
          nPom:=dopr->dlimit*opsld->ljudi
          if cUmPD=="D"
            nPom2:=dopr->dlimit*opsld->pljudi
          endif
        endif
        @ prow(),pcol()+1 SAY  nPom picture gpici
        if cUmPD=="D"
          @ prow(),pcol()+1 SAY  nPom2 picture gpici
          @ prow(),pcol()+1 SAY  nPom-nPom2 picture gpici
          Rekapld("DOPR"+dopr->id+idops,cgodina,cmjesec,nPom-nPom2,0,idops,NLjudi())
          nDoprOps2+=nPom2
          nDoprOps+=nPom
        else
          Rekapld("DOPR"+dopr->id+opsld->idops,cgodina,cmjesec,npom,nBOOps,idops,NLjudi())
          nDoprOps+=nPom
        endif
        nOOD += nBOOps
        nPOLjudi += ljudi
        skip
        if prow()>62+gPStranica; FF; endif
      enddo // opsld
      select dopr
      ? m
      ? "UKUPNO ",DOPR->ID
//      @ prow(),nC1 SAY nBO pict gpici
      @ prow(),nC1 SAY nOOD pict gpici
      @ prow(),pcol()+1 SAY nDoprOps pict gpici
      if cUmPD=="D"
        @ prow(),pcol()+1 SAY nDoprOps2 pict gpici
        @ prow(),pcol()+1 SAY nDoprOps-nDoprOps2 pict gpici
        Rekapld("DOPR"+dopr->id,cgodina,cmjesec,nDoprOps-nDoprOps2,0,,NLjudi())
        nPom2:=nDoprOps2
      else
        if nDoprOps>0
//          Rekapld("DOPR"+dopr->id,cgodina,cmjesec,nDoprOps,nBO,,NLjudi())
          Rekapld("DOPR"+dopr->id,cgodina,cmjesec,nDoprOps,nOOD,,"("+ALLTRIM(STR(nPOLjudi))+")")
        endif
      endif
      ? m
      nPom:=nDoprOps
    else
      // doprinosi nisu po opstinama
      altd()
      @ prow(),nC1 SAY nBO pict gpici
      nPom:=round2(max(dlimit,iznos/100*nBO),gZaok2)
      if cUmPD=="D"
        nPom2:=round2(max(dlimit,iznos/100*nBO2),gZaok2)
      endif
      if round(iznos,4)=0 .and. dlimit>0
          nPom:=dlimit*nljudi      // nije po opstinama
          if cUmPD=="D"
            nPom2:=dlimit*nljudi      // nije po opstinama ?!?nLjudi
          endif
      endif
      @ prow(),pcol()+1 SAY nPom pict gpici
      if cUmPD=="D"
        @ prow(),pcol()+1 SAY nPom2 pict gpici
        @ prow(),pcol()+1 SAY nPom-nPom2 pict gpici
        Rekapld("DOPR"+dopr->id,cgodina,cmjesec,nPom-nPom2,0)
      else
        Rekapld("DOPR"+dopr->id,cgodina,cmjesec,nPom,nBO,,"("+ALLTRIM(STR(nLjudi))+")")
      endif
    endif // poopst
  else
  //**************** po stopama beneficiranog radnog staza ?? nije testirano
    nPom0:=ASCAN(aNeta,{|x| x[1]==idkbenef})
    if nPom0<>0
      nPom2:=parobr->k3/100*aNeta[nPom0,2]
    else
      nPom2:=0
    endif
    if round2(nPom2,gZaok2)<>0
      @ prow(),pcol()+1 SAY nPom2 pict gpici
      nC1:=pcol()+1
      @ prow(),pcol()+1 SAY nPom:=round2(max(dlimit,iznos/100*nPom2),gZaok2) pict gpici
    endif
  endif  // ****************  nije testirano

  if right(id,1)=="X"
    ? m
    ?
    nDopr+=nPom
    if cUmPD=="D"
      nDopr2+=nPom2
    endif
  endif

  skip
  if prow()>56+gPStranica; FF; endif
enddo
? m
? "Ukupno Doprinosi"
@ prow(),nc1 SAY space(len(gpici))
@ prow(),pcol()+1 SAY nDopr  pict gpici
if cUmPD=="D"
  @ prow(),pcol()+1 SAY nDopr2  pict gpici
  @ prow(),pcol()+1 SAY nDopr-nDopr2  pict gpici
endif
? m
IF cUmPD=="D"
  10CPI
ENDIF
?
?


m:="---------------------------------"

if prow()>49+gPStranica; FF; endif
? m
? "     NETO PRIMANJA:"
@ prow(),pcol()+1 SAY nUNeto pict gpici
?? "(za isplatu:"
@ prow(),pcol()+1 SAY nUNeto+nUOdbiciM pict gpici
?? ",Obustave:"
@ prow(),pcol()+1 SAY -nUOdbiciM pict gpici
?? ")"

? " PRIMANJA VAN NETA:"
@ prow(),pcol()+1 SAY nUOdbiciP pict gpici  // dodatna primanja van neta
? "            POREZI:"
IF cUmPD=="D"
  @ prow(),pcol()+1 SAY nPor-nUPorOl-nPor2    pict gpici
ELSE
  @ prow(),pcol()+1 SAY nPor-nUPorOl    pict gpici
ENDIF
? "         DOPRINOSI:"
IF cUmPD=="D"
  @ prow(),pcol()+1 SAY nDopr-nDopr2    pict gpici
ELSE
  @ prow(),pcol()+1 SAY nDopr    pict gpici
ENDIF
? m
IF cUmPD=="D"
  ? " POTREBNA SREDSTVA:"
  @ prow(),pcol()+1 SAY nUNeto+nUOdbiciP+(nPor-nUPorOl)+nDopr-nPor2-nDopr2    pict gpici
ELSE
  ? " POTREBNA SREDSTVA:"
  @ prow(),pcol()+1 SAY nUNeto+nUOdbiciP+(nPor-nUPorOl)+nDopr    pict gpici
ENDIF
? m

?
? "Izvrsena obrada na ",str(nLjudi,5),"radnika"
?
if nUSati==0; nUSati:=999999; endif
? "Prosjecni neto/satu je",alltrim(transform(nUNeto,gpici)),"/",alltrim(str(nUSati)),"=",;
   alltrim(transform(nUNeto/nUsati,gpici)),"*",alltrim(transform(parobr->k1,"999")),"=",;
   alltrim(transform(nUneto/nUsati*parobr->k1,gpici))

ELSE // cMjesec==cMjesecDo // za viÁe mjeseci nema prikaza poreza i doprinosa
  // ali se moße dobiti bruto osnova i prosjeüni neto po satu
  // --------------------------------------------------------
  ASORT(aNetoMj,,,{|x,y| x[1]<y[1]})
  ?
  ?     "MJESEC≥  UK.NETO  ≥UK.SATI≥KOEF.BRUTO≥FOND SATI≥BRUTO OSNOV≥PROSJ.NETO "
  ?     " (A)  ≥    (B)    ≥  (C)  ≥   (D)    ≥   (E)   ≥(B)*(D)/100≥(E)*(B)/(C)"
  ? ms:="ƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒƒƒ"
  nT1:=nT2:=nT3:=nT4:=nT5:=0
  FOR i:=1 TO LEN(aNetoMj)
    ? STR(aNetoMj[i,1],4,0) +". ≥"+;
      TRANS(aNetoMj[i,2],gPicI) +"≥"+;
      STR(aNetoMj[i,3],7) +"≥"+;
      TRANS(aNetoMj[i,4],"999.99999%") +"≥"+;
      STR(aNetoMj[i,5],9) +"≥"+;
      TRANS(ROUND2(aNetoMj[i,2]*aNetoMj[i,4]/100,gZaok2),gPicI) +"≥"+;
      TRANS(aNetoMj[i,5]*aNetoMj[i,2]/aNetoMj[i,3],gPicI)
      nT1 += aNetoMj[i,2]
      nT2 += aNetoMj[i,3]
      nT3 += aNetoMj[i,5]
      nT4 += ROUND2(aNetoMj[i,2]*aNetoMj[i,4]/100,gZaok2)
      nT5 += aNetoMj[i,5]*aNetoMj[i,2]/aNetoMj[i,3]
  NEXT
  nT5 := nT5/LEN(aNetoMj)
  // nT5 := nT3*nT1/nT2
  ? ms
  ?     "UKUPNO≥"+;
      TRANS(nT1,gPicI) +"≥"+;
      STR(nT2,7) +"≥"+;
      "          "+"≥"+;
      STR(nT3,9) +"≥"+;
      TRANS(nT4,gPicI) +"≥"+;
      TRANS(nT5,gPicI)

ENDIF

?
10CPI
if prow()<62+gPStranica
 nPom:=62+gPStranica-prow()
 for i:=1 to nPom
   ?
 next
endif
?  PADC("     Obradio:                                 Direktor:    ",80)
?
?  PADC("_____________________                    __________________",80)
?
FF
END PRINT
#ifdef CAX
select opsld; use
select rekld; use
select ld
#endif
CLOSERET


****************************************************************
function Rekapld(cId,ngodina,nmjesec,nizn1,nizn2,cidpartner,copis,copis2,lObavDodaj)
*
****************************************************************

if lObavDodaj==NIL; lObavDodaj:=.f.; ENDIF

if cidpartner=NIL
  cidpartner=""
endif

if copis=NIL
  copis=""
endif
if copis2=NIL
  copis2=""
endif

pushwa()
select rekld
if lObavDodaj
  append blank
else
  seek str(ngodina,4)+str(nmjesec,2)+cid+" "
  if !found()
       append blank
  endif
endif
replace godina with str(ngodina,4),  mjesec with str(nmjesec,2),;
        id    with  cid,;
        iznos1 with nizn1, iznos2 with nizn2,;
        idpartner with cidpartner,;
        opis with copis ,;
        opis2 with cOpis2

popwa()
return
********************************
********************************
function UKartPl()
local nC1:=20,i


 cIdRadn:=space(_LR_)
 cIdRj:=gRj; cmjesec:=gMjesec; cmjesec2:=gmjesec
 cGodina:=gGodina
 cObracun:=gObracun
 cRazdvoji := "N"

#ifdef CPOR
 IF Pitanje(,"Izvjestaj se pravi za isplacene(D) ili neisplacene(N) radnike?","D")=="D"
   lIsplaceni:=.t.
   O_LD
 ELSE
   lIsplaceni:=.f.
   select (F_LDNO)  ; usex (KUMPATH+"LDNO") alias LD; set order to 1
 ENDIF
#else
 O_LD
#endif

 copy structure extended to struct
 use
 SELECT 100             // ovo malo siri strukturu
 USE struct             // naime, polja sa satima su premala za
 GO TOP                 // rekapitulairanje vise mjeseci
 While ! Eof()
   IF LEN (Trim (Field_Name))==3 .and. Left (Field_Name, 1)="S"
     REPLACE Field_Len WITH Field_Len + 3
   EndIF
   SKIP
 EndDO
 select Struct; USE
 ferase(PRIVPATH+"_LD.CDX")
 cPom:=PRIVPATH+"_LD"
 create (cPom) from struct
 use (cPom)
 index on idradn+idrj tag "1"
 close all
 *
 O_PAROBR
 O_RJ
 O_RADN
 O_VPOSLA
 O_RADKR
 O_KRED
 O__LD
#ifdef C50
 set index to (PRIVPATH+"_LDi1")
#else
 set order to 1
#endif

#ifdef CPOR
 IF lIsplaceni
   O_LD
 ELSE
   select (F_LDNO)  ; usex (KUMPATH+"LDNO") alias LD; set order to 1
 ENDIF
#else
 O_LD
#endif


 cIdRadn:=space(_LR_)

 Box(,5,75)
   @ m_x+1,m_y+2 SAY "Radna jedinica (prazno-sve rj): "  GET cIdRJ valid empty(cidrj) .or. P_RJ(@cidrj)
   @ m_x+2,m_y+2 SAY "od mjeseca: "  GET  cmjesec  pict "99"
   @ m_x+2,col()+2 SAY "do"  GET  cmjesec2  pict "99"
   if lViseObr
     @ m_x+2,col()+2 SAY "Obracun:" GET cObracun WHEN HelpObr(.t.,cObracun) VALID ValObr(.t.,cObracun)
   endif
   @ m_x+3,m_y+2 SAY "Godina: "  GET  cGodina  pict "9999"
   @ m_x+4,m_y+2 SAY "Radnik (prazno-svi radnici):" GET cIdRadn  valid empty(cIdRadn) .or. P_Radn(@cIdRadn)
   @ m_x+5,m_y+2 SAY "Razdvojiti za radnika po RJ:" GET cRazdvoji pict "@!";
                     when Empty (cIdRj) valid cRazdvoji $ "DN"
   read; clvbox(); ESC_BCR
  BoxC()

if lViseObr
  O_TIPPRN
else
  O_TIPPR
endif

SELECT LD

if lViseObr .and. !EMPTY(cObracun)
  SET FILTER TO obr=cObracun
endif

cIdRadn:=trim(cidradn)
if empty(cidrj)
  set order to tag (TagVO("4"))
  seek str(cGodina,4)+cIdRadn
  cIdrj:=""
else
  set order to tag (TagVO("3"))
  seek str(cGodina,4)+cidrj+cIdRadn
endif
EOF CRET

nStrana:=0
#ifdef CPOR  //POR-LD
IF cRazdvoji=="N"
  bZagl:={|| ;
             qqout("OBRACUN "+IF(lIsplaceni,"","NEISPLACENIH NAKNADA ")+"ZA PERIOD"+str(cmjesec,2)+"-"+str(cmjesec2,2)+"/"+str(godina,4)," ZA "+UPPER(TRIM(gTS))+" ",gNFirma),;
             qout("RJ:",idrj,rj->naz),;
             qout(idradn,"-",RADNIK,"Mat.br:",radn->matbr),;
             qout("Rbr.PIO:",radn->rbrpio);
         }
Else
  bZagl:={|| ;
             qqout("OBRACUN "+IF(lIsplaceni,"","NEISPLACENIH NAKNADA ")+"ZA PERIOD"+str(cmjesec,2)+"-"+str(cmjesec2,2)+"/"+str(godina,4)," ZA "+UPPER(TRIM(gTS))+" ",gNFirma),;
             qout(idradn,"-",RADNIK,"Mat.br:",radn->matbr),;
             qout("Rbr.PIO:",radn->rbrpio);
         }
EndIF

#else   // LD
IF cRazdvoji=="N"
  bZagl:={|| ;
             qqout("OBRACUN"+IF(lViseObr,IF(EMPTY(cObracun)," ' '(SVI)"," '"+cObracun+"'"),"")+" PLATE ZA PERIOD"+str(cmjesec,2)+"-"+str(cmjesec2,2)+"/"+str(godina,4)," ZA "+UPPER(TRIM(gTS))+" ",gNFirma),;
             qout("RJ:",idrj,rj->naz),;
             qout(idradn,"-",RADNIK,"Mat.br:",radn->matbr," STR.SPR:",IDSTRSPR),;
             qout("Broj knjizice:",RADN->brknjiz),;
             qout("Vrsta posla:",idvposla,vposla->naz,"        U radnom odnosu od ",radn->datod);
         }
Else
  bZagl:={|| ;
             qqout("OBRACUN"+IF(lViseObr,IF(EMPTY(cObracun)," ' '(SVI)"," '"+cObracun+"'"),"")+" PLATE ZA PERIOD"+str(cmjesec,2)+"-"+str(cmjesec2,2)+"/"+str(godina,4)," ZA "+UPPER(TRIM(gTS))+" ",gNFirma),;
             qout(idradn,"-",RADNIK,"Mat.br:",radn->matbr," STR.SPR:",IDSTRSPR),;
             qout("Broj knjizice:",RADN->brknjiz),;
             qout("Vrsta posla:",idvposla,vposla->naz,"        U radnom odnosu od ",radn->datod);
         }
EndIF
#endif

select vposla; hseek ld->idvposla
select rj; hseek ld->idrj; select ld

if pcount()==4
  START PRINT RET
else
  START PRINT CRET
endif

//ParObr(cmjesec)
select ld
nT1:=nT2:=nT3:=nT4:=0
do while !eof() .and.  cgodina==godina .and. idrj=cidrj .and. idradn=cIdRadn

xIdRadn:=idradn
IF cRazdvoji=="N"
  Scatter("w")
  for i:=1 to cLDPolja
    cPom:=padl(alltrim(str(i)),2,"0")
    ws&cPom:=0
    wi&cPom:=0
    wUNeto:=wUSati:=wUIznos:=0
  next
EndIF

IF cRazdvoji=="N"
  select radn; hseek xidradn
  select vposla; hseek ld->idvposla
  select rj; hseek ld->idrj; select ld
  Eval(bZagl)
EndIF
do while !eof() .and.  cgodina==godina .and. idrj=cidrj .and. idradn==xIdRadn

 m:="----------------------- --------  ----------------   ------------------"

 select radn; hseek xidradn; select ld

 if (mjesec<cmjesec .or. mjesec>cmjesec2)
   skip; loop
 endif
 Scatter()
 ***
 IF cRazdvoji=="D"
   SELECT _LD
   HSEEK xIdRadn+LD->IdRj
   IF ! Found()
     Append Blank
   EndIF
   Scatter ("w")
   For i:=1 To cLDpolja
     cPom:=padl(alltrim(str(i)),2,"0")
     ** select tippr; seek cPom
     ws&cPom+=_s&cPom
     wi&cPom+=_i&cPom
   Next
   wUIznos+=_UIznos
   wUSati+=_USati
   wUNeto+=_UNeto
   wIdRj := _IdRj
   wIdRadn := xIdRadn
   Gather("w")
   SELECT LD
   SKIP; LOOP
 EndIF
 ***
 cUneto:="D"
 for i:=1 to cLDPolja
  cPom:=padl(alltrim(str(i)),2,"0")
  select tippr; seek cPom
  ws&cPom+=_s&cPom
  wi&cPom+=_i&cPom
 next
 select ld
 wUIznos+=_UIznos
 wUSati+=_USati
 wUNeto+=_UNeto
 skip
enddo

 IF cRazdvoji=="N"
   ? m
   ? " Vrsta                  Opis         sati/iznos             ukupno"
   ? m
   cUneto:="D"
   for i:=1 to cLDPolja
    cPom:=padl(alltrim(str(i)),2,"0")
    select tippr; seek cPom
    if tippr->uneto=="N" .and. cUneto=="D"
      cUneto:="N"
      ? m
      ? "UKUPNO NETO:"; @ prow(),nC1+8  SAY  wUSati  pict gpics; ?? " sati"
      @ prow(),60 SAY wUNeto pict gpici; ?? "",gValuta
      ? m
    endif

    if tippr->(found()) .and. tippr->aktivan=="D"
     if wi&cpom<>0 .or. ws&cPom<>0
      ? tippr->id+"-"+tippr->naz,tippr->opis
      nC1:=pcol()
      if tippr->fiksan $ "DN"
         @ prow(),pcol()+8 SAY ws&cPom  pict gpics; ?? " s"
         @ prow(),60 say wi&cPom        pict gpici
      elseif tippr->fiksan=="P"
         @ prow(),pcol()+8 SAY ws&cPom  pict "999.99%"
         @ prow(),60 say wi&cPom        pict gpici
      elseif tippr->fiksan=="B"
         @ prow(),pcol()+8 SAY ws&cPom  pict "999999"; ?? " b"
         @ prow(),60 say wi&cPom        pict gpici
      elseif tippr->fiksan=="C"
         @ prow(),60 say wi&cPom        pict gpici
      endif
     endif
    endif
   next
   ? m
   ?  "UKUPNO ZA ISPLATU";  @ prow(),60 SAY wUIznos pict gpici; ?? "",gValuta
   ? m
   if prow()>31
       FF
   else
       ?
       ?
       ?
       ?
   endif
 Else
   SELECT _LD
   GO TOP
   select radn; hseek _LD->idradn
   select vposla; hseek _LD->idvposla
   SELECT _LD
   Eval(bZagl)
   ?
   While ! Eof()
     select rj; hseek _ld->idrj; select _ld
     qout("RJ:",idrj,rj->naz)
     ? m
     ? " Vrsta                  Opis         sati/iznos             ukupno"
     ? m
     *
     Scatter("w")
     cUneto:="D"
     for i:=1 to cLDPolja
       cPom:=padl(alltrim(str(i)),2,"0")
       select tippr; seek cPom
       if tippr->uneto=="N" .and. cUneto=="D"
         cUneto:="N"
         ? m
         ? "UKUPNO NETO:"; @ prow(),nC1+8  SAY  wUSati  pict gpics; ?? " sati"
         @ prow(),60 SAY wUNeto pict gpici; ?? "",gValuta
         ? m
       endif

       if tippr->(found()) .and. tippr->aktivan=="D"
        if wi&cpom<>0 .or. ws&cPom<>0
         ? tippr->id+"-"+tippr->naz,tippr->opis
         nC1:=pcol()
         if tippr->fiksan $ "DN"
            @ prow(),pcol()+8 SAY ws&cPom  pict gpics; ?? " s"
            @ prow(),60 say wi&cPom        pict gpici
         elseif tippr->fiksan=="P"
            @ prow(),pcol()+8 SAY ws&cPom  pict "999.99%"
            @ prow(),60 say wi&cPom        pict gpici
         elseif tippr->fiksan=="B"
            @ prow(),pcol()+8 SAY ws&cPom  pict "999999"; ?? " b"
            @ prow(),60 say wi&cPom        pict gpici
         elseif tippr->fiksan=="C"
            @ prow(),60 say wi&cPom        pict gpici
         endif
        endif
       endif
     next
     ? m
     ?  "UKUPNO ZA ISPLATU U RJ", _LD->IdRj
     @ prow(),60 SAY wUIznos pict gpici; ?? "",gValuta
     ? m
     if prow()>60+gPstranica
         FF
     else
         ?
         ?
     endif
     SELECT _LD
     SKIP
   EndDO
 EndIF
 select ld

enddo

 FF
 END PRINT
closeret


********************************
* rekapitulacija primanja radnika
* nedovrseno
********************************
function RekapRad()
local nC1:=20,i

 cIdRadn:=space(_LR_)
 cIdRj:=gRj; cmjesec:=gMjesec; cmjesec2:=gmjesec
 cGodina:=gGodina
 cObracun:=gObracun

 O_PAROBR
 O_RJ
 O_RADN
 O_VPOSLA
 O_RADKR
 O_KRED
 O_LD

 cIdRadn:=space(_LR_)

 Box(,4,75)
   @ m_x+1,m_y+2 SAY "Radna jedinica (prazno-sve rj): "  GET cIdRJ valid empty(cidrj) .or. P_RJ(@cidrj)
   @ m_x+2,m_y+2 SAY "od mjeseca: "  GET  cmjesec  pict "99"
   @ m_x+2,col()+2 SAY "do"  GET  cmjesec2  pict "99"
   if lViseObr
     @ m_x+2,col()+2 SAY "Obracun:" GET cObracun WHEN HelpObr(.t.,cObracun) VALID ValObr(.t.,cObracun)
   endif
   @ m_x+3,m_y+2 SAY "Godina: "  GET  cGodina  pict "9999"
   @ m_x+4,m_y+2 SAY "Radnik (prazno-svi radnici): "  GET  cIdRadn  valid empty(cIdRadn) .or. P_Radn(@cIdRadn)
   read; clvbox(); ESC_BCR
 BoxC()

if lViseObr
  O_TIPPRN
else
  O_TIPPR
endif

SELECT LD

if lViseObr .and. !EMPTY(cObracun)
  SET FILTER TO obr=cObracun
endif

cIdRadn:=trim(cidradn)
if empty(cidrj)
  set order to tag (TagVO("4"))
  seek str(cGodina,4)+cIdRadn
  cIdrj:=""
else
  set order to tag (TagVO("3"))
  seek str(cGodina,4)+cidrj+cIdRadn
endif
EOF CRET

nStrana:=0
bZagl:={|| ;
           qqout("PREGLED PRIMANJA ZA PERIOD "+str(cmjesec,2)+"-"+str(cmjesec2,2)+IspisObr()+"/"+str(godina,4)," ZA "+UPPER(TRIM(gTS))+" ",gNFirma),;
           qout("RJ:",idrj,rj->naz),;
           qout(idradn,"-",RADNIK,"Mat.br:",radn->matbr," STR.SPR:",IDSTRSPR),;
           qout("Vrsta posla:",idvposla,vposla->naz,"        U radnom odnosu od ",radn->datod);
       }

select vposla; hseek ld->idvposla
select rj; hseek ld->idrj; select ld

if pcount()==4
  START PRINT RET
else
  START PRINT CRET
endif

//ParObr(cmjesec)
select ld
nT1:=nT2:=nT3:=nT4:=0
do while !eof() .and.  cgodina==godina .and. idrj=cidrj .and. idradn=cIdRadn



Scatter("w")
xIdRadn:=idradn
for i:=1 to cLDPolja
   cPom:=padl(alltrim(str(i)),2,"0")
   ws&cPom:=0
   wi&cPom:=0
   wUNeto:=wUSati:=wUIznos:=0
next

select radn; hseek xidradn
select vposla; hseek ld->idvposla
select rj; hseek ld->idrj; select ld
Eval(bZagl)

//nNeto:=0
//nBruto:=bruto
//nBolovanje:=0
? " Mjesec      Sati    NETO          BRUTO         Doprinosi         Stopa               Iznos            "
? "                                                                  dopr.PIO         naknade bolovanje     "
do while !eof() .and.  cgodina==godina .and. idrj=cidrj .and. idradn==xIdRadn

 m:="----------------------- --------  ----------------   ------------------"

 select radn; hseek xidradn; select ld

// jedan mjesec mjesec
 Scatter()
 cUneto:="D"
 for i:=1 to cLDPolja
  cPom:=padl(alltrim(str(i)),2,"0")
  select tippr; seek cPom
  ws&cPom+=_s&cPom
  wi&cPom+=_i&cPom
 next
 select ld
 wUIznos+=_UIznos
 wUSati+=_USati
 wUNeto+=_UNeto
 skip
? str(mjesec,2)
@ prow(),pcol()+1 SAY _USati pict gpici
@ prow(),pcol()+1 SAY _UNeto  pict gpici
enddo
? "---"
? str(mjesec,2)
@ prow(),pcol()+1 SAY  wUSati  pict gpici
@ prow(),pcol()+1 SAY  wUNeto  pict gpici
?

 select ld

enddo

 FF
END PRINT

CLOSERET


***********************
function SpecifRasp()
*
***********************

gnLMarg:=0; gTabela:=1; gOstr:="D"

cIdRj:=gRj; cmjesec:=gMjesec
cGodina:=gGodina
cObracun:=gObracun

O_RJ
O_RADN
O_LD
private cFormula:=PADR("UNETO",40)
private cNaziv:=PADR("UKUPNO NETO",20)
cDod:="N"

nDo1 := 85; nDo2 := 150; nDo3 := 200; nDo4 := 250; nDo5  := 300
nDo6 := 0 ; nDo7 := 0  ; nDo8 := 0  ; nDo9 := 0  ; nDo10 := 0
nDo11:= 0 ; nDo12:= 0  ; nDo13:= 0  ; nDo14:= 0  ; nDo15 := 0
nDo16:= 0 ; nDo17:= 0  ; nDo18:= 0  ; nDo19:= 0  ; nDo20 := 0

O_PARAMS
Private cSection:="4",cHistory:=" ",aHistory:={}

RPar("p1",@cNaziv)
RPar("p2",@cFormula)
RPar("p3",@nDo1)
RPar("p4",@nDo2)
RPar("p5",@nDo3)
RPar("p6",@nDo4)
RPar("p7",@nDo5)
RPar("p8",@nDo6)
RPar("p9",@nDo7)
RPar("r0",@nDo8)
RPar("r1",@nDo9)
RPar("r2",@nDo10)
RPar("r3",@nDo11)
RPar("r4",@nDo12)
RPar("r5",@nDo13)
RPar("r6",@nDo14)
RPar("r7",@nDo15)
RPar("r8",@nDo16)
RPar("r9",@nDo17)
RPar("s0",@nDo18)
RPar("s1",@nDo19)
RPar("s2",@nDo20)

Box(,19,77)

@ m_x+1,m_y+2 SAY "Radna jedinica (prazno sve): "  GET cIdRJ
@ m_x+2,m_y+2 SAY "Mjesec: "  GET  cmjesec  pict "99"
if lViseObr
  @ m_x+2,col()+2 SAY "Obracun:" GET cObracun WHEN HelpObr(.t.,cObracun) VALID ValObr(.t.,cObracun)
endif
@ m_x+3,m_y+2 SAY "Godina: "  GET  cGodina  pict "9999"

@ m_x+5,m_y+2 SAY "Naziv raspona primanja: "  GET cNaziv
@ m_x+6,m_y+2 SAY "Formula primanja      : "  GET cFormula PICT "@S20"

@ m_x+ 8,m_y+2 SAY "             (0 - raspon se ne prikazuje)"
@ m_x+ 9,m_y+2 SAY " 1. raspon do " GET nDo1 pict "99999"
@ m_x+10,m_y+2 SAY " 2. raspon do " GET nDo2 pict "99999"
@ m_x+11,m_y+2 SAY " 3. raspon do " GET nDo3 pict "99999"
@ m_x+12,m_y+2 SAY " 4. raspon do " GET nDo4 pict "99999"
@ m_x+13,m_y+2 SAY " 5. raspon do " GET nDo5 pict "99999"
@ m_x+14,m_y+2 SAY " 6. raspon do " GET nDo6 pict "99999"
@ m_x+15,m_y+2 SAY " 7. raspon do " GET nDo7 pict "99999"
@ m_x+16,m_y+2 SAY " 8. raspon do " GET nDo8 pict "99999"
@ m_x+17,m_y+2 SAY " 9. raspon do " GET nDo9 pict "99999"
@ m_x+18,m_y+2 SAY "10. raspon do " GET nDo10 pict "99999"

@ m_x+ 9,m_y+25 SAY "11. raspon do " GET nDo11 pict "99999"
@ m_x+10,m_y+25 SAY "12. raspon do " GET nDo12 pict "99999"
@ m_x+11,m_y+25 SAY "13. raspon do " GET nDo13 pict "99999"
@ m_x+12,m_y+25 SAY "14. raspon do " GET nDo14 pict "99999"
@ m_x+13,m_y+25 SAY "15. raspon do " GET nDo15 pict "99999"
@ m_x+14,m_y+25 SAY "16. raspon do " GET nDo16 pict "99999"
@ m_x+15,m_y+25 SAY "17. raspon do " GET nDo17 pict "99999"
@ m_x+16,m_y+25 SAY "18. raspon do " GET nDo18 pict "99999"
@ m_x+17,m_y+25 SAY "19. raspon do " GET nDo19 pict "99999"
@ m_x+18,m_y+25 SAY "20. raspon do " GET nDo20 pict "99999"

read; clvbox(); ESC_BCR

BoxC()

WPar("p1",cNaziv)
WPar("p2",cFormula)
WPar("p3",nDo1)
WPar("p4",nDo2)
WPar("p5",nDo3)
WPar("p6",nDo4)
WPar("p7",nDo5)
WPar("p8",nDo6)
WPar("p9",nDo7)
WPar("r0",nDo8)
WPar("r1",nDo9)
WPar("r2",nDo10)
WPar("r3",nDo11)
WPar("r4",nDo12)
WPar("r5",nDo13)
WPar("r6",nDo14)
WPar("r7",nDo15)
WPar("r8",nDo16)
WPar("r9",nDo17)
WPar("s0",nDo18)
WPar("s1",nDo19)
WPar("s2",nDo20)

select params; use

if lViseObr
  O_TIPPRN
else
  O_TIPPR
endif

aRasponi:={ nDo1 , nDo2 , nDo3, nDo4 , nDo5 , nDo6 , nDo7 , nDo8 , nDo9 ,;
            nDo10 , nDo11 , nDo12 , nDo13 , nDo14 , nDo15 , nDo16 , nDo17 ,;
            nDo18 , nDo19 , nDo20 }

ASORT(aRasponi)

nLast:=0
// nKol:=0
nRed:=0

// aKol:={ { "" , {|| "BR.RADNIKA"  }, .f., "C", 10, 0, 1, ++nKol } }
aKol:={}

aUslRasp:={}
nSumRasp:={}

FOR i:=1 TO LEN(aRasponi)
 IF aRasponi[i]>0
   ++nRed
//   ++nKol

   AADD( nSumRasp , 0 )

//   cPomM:="nSumRasp["+ALLTRIM(STR(nKol-1))+"]"
//   AADD( aKol , { ALLTRIM(cNaziv) , {|| STR(&cPomM.,11)  }, .f., "C", 20, 0, 1, nKol } )
//   AADD( aKol , { "OD "+STR(nLast,5)+" DO "+STR(aRasponi[i],5) , {|| "#"  }, .f., "C", 20, 0, 2, nKol } )
   cPomM:="nSumRasp["+ALLTRIM(STR(nRed))+"]"

   cPom77 := "{|| 'OD "+STR(nLast,5)+" DO "+STR(aRasponi[i],5)+"' }"
   IF nRed==1
    AADD( aKol , { ALLTRIM(cNaziv) , &cPom77. , .f., "C", 40, 0, nRed , 1 } )
    AADD( aKol , { "BROJ RADNIKA"  , {|| &cPomM.   }, .f., "N", 12, 0, nRed , 2 } )
   ELSE
    AADD( aKol , { "" , &cPom77. , .f., "C", 40, 0, nRed , 1 } )
    AADD( aKol , { "" , {|| &cPomM.   }, .f., "N", 12, 0, nRed , 2 } )
   ENDIF

   AADD(aUslRasp,{nLast,aRasponi[i]})
   nLast:=aRasponi[i]
 ENDIF
NEXT

IF LEN(aKol)<2; CLOSERET; ENDIF
ASORT(aKol,,,{|x,y| 100*x[8]+x[7]<100*y[8]+y[7]})

SELECT LD

SET ORDER TO TAG (TagVO("1"))

PRIVATE cFilt1:=""
cFilt1 := "GODINA=="+cm2str(cGodina)+".and.MJESEC=="+cm2str(cMjesec)+;
		  IF(EMPTY(cIdRJ),"",".and.IDRJ=="+cm2str(cIdRJ))
cFilt1 := STRTRAN(cFilt1,".t..and.","")

IF lViseObr .and. !EMPTY(cObracun)
  cFilt1 += (".and. OBR=="+cm2str(cObracun))
ENDIF

IF cFilt1==".t."
  SET FILTER TO
ELSE
  SET FILTER TO &cFilt1
ENDIF

GO TOP

START PRINT CRET

PRIVATE cIdPartner:="", cNPartnera:="", nUkRoba:=0, nUkIznos:=0

?? space(gnLMarg); ?? "LD: Izvjestaj na dan",date()
? space(gnLMarg); IspisFirme("")
? space(gnLMarg)
if empty(cidrj)
 ?? "Pregled za sve RJ ukupno:"
else
 ?? "RJ:", cidrj+" - "+Ocitaj(F_RJ,cIdRj,"naz")
endif
?? "  Mjesec:",str(cmjesec,2)+IspisObr()
?? "    Godina:", str(cGodina,5)

StampaTabele(aKol,{|| FSvaki2()},,gTabela,,;
     ,"Specifikacija po rasponima primanja",;
                             {|| FFor2()},IF(gOstr=="D",,-1),,,,,)

FF
END PRINT

CLOSERET


FUNCTION FFor2()
 DO WHILE !EOF()
   nPrim:=&(cFormula)
   FOR i:=1 TO LEN(aUslRasp)
//     ? aUslRasp[i,1], nPrim, aUslRasp[i,2]
     IF nPrim>aUslRasp[i,1] .and. nPrim<=aUslRasp[i,2]
       ++nSumRasp[i]
     ENDIF
   NEXT
//   ?
   SKIP 1
 ENDDO
 SKIP -1
RETURN .t.


PROCEDURE FSvaki2()
RETURN


// FUNCTION TekRec()
//  nSlog++
//  @ m_x+1, m_y+2 SAY PADC(ALLTRIM(STR(nSlog))+"/"+ALLTRIM(STR(nUkupno)),20)
//  @ m_x+2, m_y+2 SAY "Obuhvaceno: "+STR(cmxKeysIncluded())
// RETURN (NIL)


**********************
**********************
function IspisFirme(cidrj)
local nOArr:=select()

?? "Firma: "
B_ON; ?? gNFirma; B_OFF
if !empty(cidrj)
  select rj; hseek cidrj; select(nOArr)
  ?? "  RJ",rj->naz
endif
return



FUNCTION SortPrez(cId)
 LOCAL cVrati:="", nArr:=SELECT()
 SELECT RADN
 HSEEK cId
 cVrati:=BHSORT(naz+ime+imerod)+id
 SELECT (nArr)
RETURN cVrati



FUNCTION SortVar(cId)
 LOCAL cVrati:="", nArr:=SELECT()
 SELECT RADKR
 SEEK cId
 SELECT RJES
 SEEK RADKR->naosnovu+RADKR->idradn
 cVrati:=varijanta
 SELECT (nArr)
RETURN cVrati



FUNCTION BHSORT(cInput)
 IF gKodnaS=="7"
   cInput:=STRTRAN(cInput,"[","S"+CHR(255))
   cInput:=STRTRAN(cInput,"\","D"+CHR(255))
   cInput:=STRTRAN(cInput,"^","C"+CHR(254))
   cInput:=STRTRAN(cInput,"]","C"+CHR(255))
   cInput:=STRTRAN(cInput,"@","Z"+CHR(255))
   cInput:=STRTRAN(cInput,"{","s"+CHR(255))
   cInput:=STRTRAN(cInput,"|","d"+CHR(255))
   cInput:=STRTRAN(cInput,"~","c"+CHR(254))
   cInput:=STRTRAN(cInput,"}","c"+CHR(255))
   cInput:=STRTRAN(cInput,"`","z"+CHR(255))
 ELSE  // "8"
   cInput:=STRTRAN(cInput,"Ê","S"+CHR(255))
   cInput:=STRTRAN(cInput,"—","D"+CHR(255))
   cInput:=STRTRAN(cInput,"¨","C"+CHR(254))
   cInput:=STRTRAN(cInput,"è","C"+CHR(255))
   cInput:=STRTRAN(cInput,"¶","Z"+CHR(255))
   cInput:=STRTRAN(cInput,"Á","s"+CHR(255))
   cInput:=STRTRAN(cInput,"–","d"+CHR(255))
   cInput:=STRTRAN(cInput,"ü","c"+CHR(254))
   cInput:=STRTRAN(cInput,"Ü","c"+CHR(255))
   cInput:=STRTRAN(cInput,"ß","z"+CHR(255))
 ENDIF
RETURN PADR(cInput,100)


FUNCTION NLjudi()
RETURN "("+ALLTRIM(STR(opsld->ljudi))+")"


FUNC ImaUOp(cPD,cSif)
 LOCAL lVrati:=.t.
  IF OPS->(FIELDPOS("DNE"))<>0
    IF UPPER(cPD)="P"   // porez
      lVrati := ! ( cSif $ OPS->pne )
    ELSE                // doprinos
      lVrati := ! ( cSif $ OPS->dne )
    ENDIF
  ENDIF
RETURN lVrati


PROC PozicOps(cSR)
 LOCAL nArr:=SELECT(), cO:=""
  IF cSR=="1"      // stanovanja
    cO:=RADN->idopsst
  ELSEIF cSR=="2"  // rada
    cO:=RADN->idopsrad
  *ELSEIF cSR=="3"  // kanton stanovanja
  *  PushWa(); select ops; set order to tag "KAN"; seek rand->idopsst; cO:=ops->IDKAN;  PopWa()
  *ELSEIF cSR=="4"  // kanton rada
  *  PushWa(); select ops; set order to tag "KAN"
  *  seek rand->idopsrad; cO:=ops->IDKAN;  PopWa()
  *ELSEIF cSR=="5"  // entitet stanovanja
  *  PushWa(); select ops; set order to tag "IDN0"; seek rand->idopsst; cO:=ops->idn0;  PopWa()
  *ELSEIF cSR=="6"  // entitet rada
  *  PushWa(); select ops; set order to tag "IDN0"; seek rand->idopsrad; cO:=ops->idn0;  PopWa()
  ELSE             // " "
    cO:=CHR(255)
  ENDIF
  SELECT (F_OPS)
  IF !USED()
    O_OPS
  ENDIF
  SEEK cO
  SELECT (nArr)
RETURN


PROCEDURE ScatterS(cG,cM,cJ,cR,cPrefix)
 private cP7:=cPrefix
  IF cPrefix==NIL
    Scatter()
  ELSE
    Scatter(cPrefix)
  ENDIF
  SKIP 1
  DO WHILE !EOF() .and. mjesec=cM .and. godina=cG .and. idradn=cR .and.;
           idrj=cJ
    IF cPrefix==NIL
      for i:=1 to cLDPolja
        cPom    := padl(alltrim(str(i)),2,"0")
        _i&cPom += i&cPom
      next
      _uneto   += uneto
      _uodbici += uodbici
      _uiznos  += uiznos
    ELSE
      for i:=1 to cLDPolja
        cPom    := padl(alltrim(str(i)),2,"0")
        &cP7.i&cPom += i&cPom
      next
      &cP7.uneto   += uneto
      &cP7.uodbici += uodbici
      &cP7.uiznos  += uiznos
    ENDIF
    SKIP 1
  ENDDO
  SKIP -1
RETURN


FUNC IspisObr()
 LOCAL cVrati:=""
 if lViseObr .and. !EMPTY(cObracun)
   cVrati:="/"+cObracun
 endif
RETURN cVrati


FUNC Obr2_9()
RETURN lViseObr .and. !EMPTY(cObracun) .and. cObracun<>"1"


FUNC TagVO(cT,cI)
  IF cI==NIL; cI:=""; ENDIF
  IF lViseObr .and. cT $ "12"
    IF cI=="I" .or. EMPTY(cObracun)
      cT := cT + "U"
    ENDIF
  ENDIF
RETURN cT



PROC SvratiUFajl()
  FERASE(PRIVPATH+"xoutf.txt")
  SET PRINTER TO (PRIVPATH+"xoutf.txt")
RETURN


FUNC U2Kolone(nViska)
 LOCAL cImeF, nURed
 IF "U" $ TYPE("cLMSK"); cLMSK:=""; ENDIF
 nSirKol:=80+LEN(cLMSK)
 cImeF:=PRIVPATH+"xoutf.txt"
 nURed:=BrLinFajla(cImeF)
 aR    := DioFajlaUNiz(cImeF,1,nURed-nViska,nURed)
 aRPom := DioFajlaUNiz(cImeF,nURed-nViska+1,nViska,nURed)
 aR[1] = PADR(aR[1],nSirKol) + aR[1]
 aR[2] = PADR(aR[2],nSirKol) + aR[2]
 aR[3] = PADR(aR[3],nSirKol) + aR[3]
 aR[4] = PADR(aR[4],nSirKol) + aR[4]
 FOR i:=1 TO LEN(aRPom)
   aR[i+4] = PADR(aR[i+4],nSirKol) + aRPom[i]
 NEXT
RETURN aR

