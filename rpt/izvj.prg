#include "porld.ch"

#define  RADNIK  radn->(padr(  trim(naz)+" ("+trim(imerod)+") "+ime,35))

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
      P_COND2
    ELSE
      P_COND
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
      P_COND2
    ELSE
      P_12CPI
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

function PregPl()
// "1" - pregl.obracunatih/isplacenih plata VAR.2
// POR-LD
// "2" - pregl.isplacenih/obracunatih i neisplacenih(obrisan obracun) plata     // "3" - pregl.obracunatih/isplacenih plata VAR.1
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
P_12CPI
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
   nNetoOsnova:=0
   do while !eof() .and.  cgodina2==godina .and. cmjesec2=mjesec .and. ( EMPTY(cIdRJ) .or. cidrj==idrj )
     npUneto+=LD->uneto
     nNetoOsnova+=MAX(LD->uneto,PAROBR->prosld*gPDLimit/100)
     skip 1
   enddo  // LD
   npBo:=round2(parobr->k3/100*nNetoOsnova,gZaok2)
   SELECT POR; GO TOP
   npPom:=npPor:=npPorOps:=0
   do while !eof()
     npPom:=max(dlimit,iznos/100*nNetoOsnova)
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

 P_10CPI
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

 nUNeto:=n01+n02
 //nBo:=round2(parobr->k3/100*MAX(nUneto,PAROBR->prosld*gPDLimit/100),gZaok2)
 
 // nova funkcija za bruto izracunavanje !
 nBO := bruto_osn( nUNeto )

 SELECT POR; GO TOP
 nPom:=nPor:=nPorOps:=0
 do while !eof()
//   nPom:=round2(max(dlimit,iznos/100*nUNeto),gZaok2)
   nPom:=max(dlimit,iznos/100*MAX(nUneto,PAROBR->prosld*gPDLimit/100))
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
return

function ZPregPl()
IF nKrug==2
  ?
  ?
ENDIF

IF cVarijanta=="1" .or. nKrug<2
  P_12CPI
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

P_10CPI
IF cVarijanta$"13"
 ? "                 ZAHTJEV ZA DOZNACIVANJE NOVCANIH SREDSTAVA"
 P_12CPI
ELSEIF nKrug<2
 ? "                       IZVJESTAJ O IZVRSENOJ ISPLATI"
 P_12CPI
ELSEIF nKrug>=2
 ? "                         LISTA NEISPLACENIH RADNIKA"
 P_COND
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

 cPrimR1:=PADR("01;02;03;04;05;06;07;",60)
 cPrimR2:=PADR("08;09;10;11;12;13;14;",60)

 O_RADKR
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
 @ m_x+ 7,m_y+2 SAY "Nacin crtanja tabele     (0/1/2)   "  GET gTabela VALID gTabela>=0.and.gTabela<=2 pict "9"
 @ m_x+ 8,m_y+2 SAY "Ukljuceno ostranicavanje (D/N) ?   "  GET gOstr   VALID gOstr$"DN"    pict "@!"
 @ m_x+ 9,m_y+2 SAY "Odvajati podatke linijom (D/N) ?   "  GET cOdvLin VALID cOdvLin$"DN"  pict "@!"
 @ m_x+10,m_y+2 SAY "Prikazati kolonu 'UKUPNO'(D/N) ?   "  GET cPKU    VALID cPKU$"DN"  pict "@!"
 @ m_x+11,m_y+2 SAY "Prikazati kolone 'Uk.neto','Uk.sati' i 'Uk.neto/Uk.sati' (D/N) ?"  GET cPKPN  VALID cPKPN$"DN"  pict "@!"
 @ m_x+12,m_y+2 SAY "Sortirati po(1-sifri,2-prezime+ime)"  GET cVarSort VALID cVarSort$"12"  pict "9"
 read; clvbox(); ESC_BCR
 BoxC()

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

 // automatsko biranje tipova primanja
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

aKol:={}; nKol:=0

AADD( aKol , { "R.BR." , {|| STR(nRBr,4)+"."}, .f., "C", 5, 0, 1, ++nKol} )
AADD( aKol , { "SIFRA/JMB" , {|| cIdRadn}, .f., "C", 13, 0, 1, ++nKol} )
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
    IF !EMPTY(cSifPr)
      aRez:=GodMj(cGodina,cMjesec,-val(cSifPr)+1)
      cTpnaz := IF(cSifPr='14','<=','') + str(arez[2],2)+"/"+str(arez[1],4)
    ELSE
      cTPNaz:=""
    ENDIF
    AADD( aKol , { cTPNaz , &cBlk. , .t. , "N-" ,  9 , 2 , 1 , nKol } )
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
    IF !EMPTY(cSifPr)
      aRez:=GodMj(cGodina,cMjesec,-val(cSifPr)+1)
      cTpnaz := IF(cSifPr='14','<=','') + str(arez[2],2)+"/"+str(arez[1],4)
    ELSE
      cTPNaz:=""
    ENDIF
    AADD( aKol , { cTPNaz , &cBlk. , .t. , "N-" ,  9 , 2 , 2 , nKol } )
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
    IF !EMPTY(cSifPr)
      aRez:=GodMj(cGodina,cMjesec,-val(cSifPr)+1)
      cTpnaz := IF(cSifPr='14','<=','') + str(arez[2],2)+"/"+str(arez[1],4)
    ELSE
      cTPNaz:=""
    ENDIF
    AADD( aKol , { cTPNaz , &cBlk. , .t. , "N-" ,  9 , 2 , 3 , nKol } )
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

  IF cVarijanta!="3"
    AADD( aKol , { "P O T P I S" , {|| "" } , .f. , "C" , 15 , 0 , 1 , ++nKol } )
  ENDIF

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

?

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
 ++nRbr
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
 nBo := bruto_osn(nUNeto)
 SELECT POR; GO TOP
 nPom:=nPor:=nPorOps:=0
 do while !eof()
   nPom:=max(dlimit,iznos/100*MAX(nUNeto,PAROBR->prosld*gPDLimit/100))
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
P_10CPI

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

P_12CPI
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
? "Pregled "+IF(lIsplaceni,"isplacenih iznosa","neisplacenih iznosa")+" za tip primanja:",ctip,tippr->naz
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
 IF Pitanje(,"Izvjestaj se pravi za isplacene(D) ili neisplacene(N) radnike?","D")=="D"
   lIsplaceni:=.t.
   O_LD
 ELSE
   lIsplaceni:=.f.
   select (F_LDNO)  ; usex (KUMPATH+"LDNO") alias LD; set order to 1
 ENDIF

cProred:="N"
cPrikIzn:="D"
nProcenat:=100
nZkk:=gzaok
cDrugiDio:="D"
cNaslov:=""       // ISPLATA PLATA
cNaslovTO:=""     // ISPLATA TOPLOG OBROKA
nIznosTO:=0

 O_PARAMS
 Private cSection:="4",cHistory:=" ",aHistory:={}
 RPar("VS",@cVarSort)
 RPar("ni",@cNaslov)
 RPar("to",@cNaslovTO)

cNaslov   := PADR( cNaslov   , 90 )
cNaslovTO := PADR( cNaslovTO , 90 )

Box(,12,60)
@ m_x+1,m_y+2 SAY "Radna jedinica (prazno-sve): "  GET cIdRJ
@ m_x+2,m_y+2 SAY "Mjesec: "  GET  cmjesec  pict "99"
if lViseObr
  @ m_x+ 2,col()+2 SAY "Obracun: "  GET  cObracun WHEN HelpObr(.t.,cObracun) VALID ValObr(.t.,cObracun)
endif
@ m_x+ 3,m_y+2 SAY "Godina: "  GET  cGodina  pict "9999"
@ m_x+ 4,m_y+2 SAY "Prored:"   GET  cProred  pict "@!"  valid cProred $ "DN"
@ m_x+ 5,m_y+2 SAY "Prikaz iznosa:" GET cPrikIzn pict "@!" valid cPrikizn$"DN"
@ m_x+ 6,m_y+2 SAY "Prikaz u procentu %:" GET nprocenat pict "999.99"
@ m_x+ 7,m_y+2 SAY "Sortirati po(1-sifri,2-prezime+ime)"  GET cVarSort VALID cVarSort$"12"  pict "9"
@ m_x+ 8,m_y+2 SAY "Naslov izvjestaja"  GET cNaslov pict "@S30"
@ m_x+ 9,m_y+2 SAY "Naslov za topl.obrok"  GET cNaslovTO pict "@S30"
@ m_x+10,m_y+2 SAY "Iznos (samo za topli obrok)"  GET nIznosTO pict gPicI
read; clvbox(); ESC_BCR
if nprocenat<>100
  @ m_x+11,m_y+2 SAY "zaokruzenje" GET nZkk pict "99"
  @ m_x+12,m_y+2 SAY "Prikazati i drugi spisak (za "+LTRIM(STR(100-nProcenat,6,2))+"%-tni dio)" GET cDrugiDio VALID cDrugiDio$"DN" PICT "@!"
  read
else
  cDrugiDio:="N"
endif
BoxC()

 WPar("VS",cVarSort)
 cNaslov:=ALLTRIM(cNaslov)
 WPar("ni",cNaslov)
 cNaslovTO:=ALLTRIM(cNaslovTO)
 WPar("to",cNaslovTO)
 SELECT PARAMS; USE

IF nIznosTO<>0
  cNaslov:=cNaslovTO
  qqImaTO:=IzFMKIni("LD","UslovImaTopliObrok",'UPPER(RADN->K2)=="D"',KUMPATH)
ENDIF
IF !EMPTY(cNaslov)
  cNaslov += (" za mjesec:"+STR(cMjesec,2)+". godine:"+STR(cGodina,4)+".")
ENDIF

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
 if nIznosTO=0      // isplata plate
   if !(empty(radn->isplata) .or. radn->isplata="BL")
      skip
      loop
   endif
 else               // isplata toplog obroka
   if !(&qqImaTO)
      skip
      loop
   endif
 endif
 if prow()>62+gpStranica; FF; Eval(bZagl); endif
 ? str(++nRbr,4)+".",idradn, RADNIK
 nC1:=pcol()+1
 IF nIznosTO<>0
   _uiznos:=nIznosTO
 ENDIF
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

P_12CPI
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
? "SPISAK "+IF(lIsplaceni,"ISPLACENIH","NEISPLACENIH")+" RADNIKA"
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
? "Rbr     Sifra                Naziv radnika               "+iif(cprikizn=="D","ZA ISPLATU","          ")+"         Potpis"
? m

return

// -----------------------------------------------
// kartica plate
// -----------------------------------------------
function KartPl(cIdRj,cMjesec,cGodina,cIdRadn,cObrac)
local i,aNeta:={}
nRRSati:=0
lSkrivena:=.f.
lRadniSati:=.f.
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

cVarSort:="2"

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
 	IF Pitanje(,"Izvjestaj se pravi za isplacene(D) ili neisplacene(N) radnike?","D")=="D"
   		lIsplaceni:=.t.
   		O_LD
 	ELSE
   		lIsplaceni:=.f.
   		select (F_LDNO)  ; usex (KUMPATH+"LDNO") alias LD; set order to 1
 	ENDIF
else
	lIsplaceni:=.t.
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
   		read
		clvbox()
		ESC_BCR
 		BoxC()

 		WPar("VS",cVarSort)
 		WPar("2K",c2K1L)
 		SELECT PARAMS
		USE

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

select vposla
hseek ld->idvposla
select rj
hseek ld->idrj
select ld

if pcount()>=4
	START PRINT RET
else
  	START PRINT CRET
endif

P_12CPI

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

 select kred; hseek _idkred
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
           P_COND2
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
            ? cLMSK+"UKUPNO NETO:"
            @ prow(),nC1+8  SAY  _USati  pict gpics
            ?? " sati"
            @ prow(),60+LEN(cLMSK) SAY _UNeto pict gpici
            ?? "",gValuta
            ? m
          endif

          if tippr->(found()) .and. tippr->aktivan=="D"
           if _i&cpom<>0 .or. _s&cPom<>0

            // uvodi se djoker # : Primjer: Naziv tipa primanja
            // je: REDOVAN RAD BOD #RADN->N1 -> naci RADN->N1
            // i ispisati REDOVAN RAD BOD 12.0
            nDJ:=at("#",tippr->naz)
            aRez:=GodMj(_godina,_mjesec,-val(tippr->id)+1)
            cTpnaz:=padr("Za "+;
                         iif(tippr->id='14','<= ','')+;
                         str(arez[2],2)+"/"+str(arez[1],4),;
                         len(tippr->naz))
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
               if tippr->id=="01" .and. lRadniSati
                  nRRSati:=_s&cPom
                  @ prow(),60+LEN(cLMSK) SAY _i&cPom * parobr->k3/100 pict gpici
                  @ prow()+1,0 SAY "Odbici od bruta: "
                  @ prow(), pcol()+48 SAY "-" + ALLTRIM(STR((_i&cPom * (parobr->k3)/100)-_i&cPom))

               else
                  @ prow(),60+LEN(cLMSK) say _i&cPom        pict gpici
               endif
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
                P_COND2
              ELSE
                P_COND
              ENDIF
              ? m
              ? cLMSK+"  ","Od toga pojedinacna rjesenja"
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
                P_COND2
              ELSE
                P_12CPI
              ENDIF
              select ld
            elseif "SUMKREDITA" $ tippr->formula
              //? m
              //? "  ","Od toga pojedinacni krediti:"
              select radkr; set order to 1
              seek str(_godina,4)+str(_mjesec,2)+_idradn
              ukredita:=0
              IF l2kolone
                P_COND2
              ELSE
                P_COND
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
                P_COND2
              ELSE
                P_12CPI
              ENDIF

              select ld
            endif
           endif
          endif
         next
         ?
         ? m
         ?  cLMSK+"UKUPNO ZA ISPLATU"
         @ prow(),60+LEN(cLMSK) SAY _UIznos pict gpici
         ?? "",gValuta
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

         if lRadniSati
            ? "NAPOMENA: Ostaje da se plati iz preraspodjele radnog vremena "
            ?? ALLTRIM(STR((ld->radsat)-nRRSati))  + " sati."
            ?
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
          nBO:=bruto_osn( _UNeto )

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
            @ prow(),pcol()+1 SAY MAX(_UNeto,PAROBR->prosld*gPDLimit/100) pict gpici
            @ prow(),pcol()+1 SAY nPom:=max(dlimit,round(iznos/100*MAX(_UNeto,PAROBR->prosld*gPDLimit/100),gZaok2)) pict gpici
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
           
	   select ld
	   ?
           ? bruto_isp(_UNeto)
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
         Eval(bZagl)
         if gTipObr=="2" .and. parobr->k1<>0
           ?? "        Bod-sat:"; @ prow(),pcol()+1 SAY parobr->vrbod/parobr->k1*brbod pict "99999.999"
         endif
         IF l2kolone
           P_COND2
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
         //nBo:=round2(parobr->k3/100*MAX(_UNeto,PAROBR->prosld*gPDLimit/100),gZaok2)
         nBo := bruto_osn( _UNeto )

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
              @ prow(),39+LEN(cLMSK)  SAY nPom:=max(dlimit,round(iznos/100*MAX(_UNeto,PAROBR->prosld*gPDLimit/100),gZaok2)) pict gpici
            ELSE
              nPom:=max(dlimit,round(iznos/100*MAX(_UNeto,PAROBR->prosld*gPDLimit/100),gZaok2))
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
         ?
         
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

return



function ZaglKar()

++nRBrKart

IF !lSkrivena .and. c2K1L=="D" .and. (nRBrKart%2)==0
  DO WHILE prow()<33; ?; ENDDO
ENDIF

? "SLUZBA:",gNFirma
? IF(lIsplaceni,"","NEISPLACENI ")+"OBRACUN NAKNADE ZA PORODILJSKO ODSUSTVO ZA "+str(mjesec,2)+"/"+str(godina,4)
? "RJ:",idrj,rj->naz
? idradn,"-",RADNIK,"  Mat.br:",radn->matbr
? "Reg.br.PIO:",radn->rbrpio
? "Poslodavac:",_idkred, trim(kred->naz)
if KRED->(FIELDPOS("ADRESA"))<>0
  ?? ",", trim(kred->adresa)
endif
return



// ----------------------------------------
// rekapitulacija primanja
// ----------------------------------------
function Rekap(fSvi)
local nC1:=20, i
local cTPNaz
local cUmPD:="N"
local nKrug:=1

fPorNaRekap:=IzFmkIni("LD","PoreziNaRekapitulaciji","N",KUMPATH)=="D"

cUmPD:="D"
cIdRj:=gRj
cmjesec:=gMjesec
cGodina:=gGodina
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

IF Pitanje(,"Izvjestaj se pravi za isplacene(D) ili neisplacene(N) radnike?","D")=="D"
	lIsplaceni:=.t.
   	O_LD
ELSE
	lIsplaceni:=.f.
   	select (F_LDNO)  ; usex (KUMPATH+"LDNO") alias LD; set order to 1
ENDIF

cIdRadn:=space(_LR_)
cStrSpr:=space(3)
cOpsSt:=space(4)
cOpsRad :=space(4)

if fSvi
	// za sve radne jedinice
	qqRJ:=SPACE(60)
	Box(,10,75)

	DO WHILE .t.
 		if lIsplaceni
   			@ m_x+2,m_y+2 SAY "Umanjiti poreze i doprinose za preplaceni iznos? (D/N)"  GET cUmPD VALID cUmPD $ "DN" PICT "@!"
 		endif
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

 		read
		clvbox()
		ESC_BCR
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
	Box(,8,75)
 	if lIsplaceni
   		@ m_x+1,m_y+2 SAY "Umanjiti poreze i doprinose za preplaceni iznos? (D/N)"  GET cUmPD VALID cUmPD $ "DN" PICT "@!"
 	endif
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
 	read
	clvbox()
	ESC_BCR
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
endif

PoDoIzSez(cGodina,cMjesecDo)

nStrana:=0

if !fPorNaRekap
	m:="------------------------  ----------------               -------------------"
else
  	m:="------------------------  ---------------  ---------------  -------------"
endif

aDbf:={   {"ID"    ,"C", 1,0},;
          {"IDOPS" ,"C", 4,0},;
          {"IZNOS" ,"N",25,4},;
          {"IZNOS2","N",25,4},;
          {"LJUDI" ,"N", 10,0};
      }

AADD(aDbf, {"PIZNOS" ,"N",25,4})
AADD(aDbf, {"PIZNOS2","N",25,4})
AADD(aDbf, {"PLJUDI" ,"N", 10,0})

//id- 1 opsstan
//id- 2 opsrad
DBCREATE2(PRIVPATH+"opsld",aDbf)

select(F_OPSLD)
usex (PRIVPATH+"opsld")
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
AADD( aDbf , {"idpartner"  ,  "C" , 10, 0} )

DBCREATE2(KUMPATH+"REKLD",aDbf)

select (F_REKLD)
usex (KUMPATH+"rekld")
index ON  BRISANO+"10" TAG "BRISAN"
index on  godina+mjesec+id  tag "1"
set order to tag "1"
use

O_REKLD
O_OPSLD
select ld

START PRINT CRET
P_10CPI

IF IzFMKIni("LD","RekapitulacijaGustoPoVisini","N",KUMPATH)=="D"
	lGusto:=.t.
  	gRPL_Gusto()
  	nDSGusto:=VAL(IzFMKIni("RekapGustoPoVisini","DodatnihRedovaNaStranici","11",KUMPATH))
  	gPStranica+=nDSGusto
ELSE
  	lGusto:=.f.
  	nDSGusto:=0
ENDIF

// samo pozicionira bazu PAROBR na odgovarajuci zapis
ParObr(cmjesec,IF(lViseObr,cObracun,),IF(!fSvi,cIdRj,))     

private aRekap[cLDPolja,2]

for i:=1 to cLDPolja
	aRekap[i,1]:=0
  	aRekap[i,2]:=0
next

nT1:=nT2:=nT3:=nT4:=0
nUNeto:=0
nUNetoOsnova := 0
nZaBrutoOsnova := 0
nUIznos:=nUSati:=nUOdbici:=nUOdbiciP:=nUOdbiciM:=0
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

nPorOl:=0
nUPorOl:=0

aNetoMj:={}
aUkTr:={}

do while !eof() .and. eval(bUSlov)           
	// provrti kroz bazu ld-a radnike
	
	if lViseObr .and. EMPTY(cObracun)
   		ScatterS(godina,mjesec,idrj,idradn)
 	else
   		Scatter()
 	endif

 	select radn
	hseek _idradn
 	select vposla
	hseek _idvposla

 	if (!empty(copsst) .and. copsst<>radn->idopsst)  .or.;
    		(!empty(copsrad) .and. copsrad<>radn->idopsrad)
   		select ld
   		skip 1
		loop
 	endif

 	_ouneto:=MAX(_uneto,PAROBR->prosld*gPDLimit/100)
 	
	select por
	go top
 
 	nPor:=0
	nPorOl:=0
 
 	// prodji kroz poreze
 	do while !eof()  
   		PozicOps(POR->poopst)
   		IF !ImaUOp("POR",POR->id)
     			SKIP 1
			LOOP
   		ENDIF
   		nPor+=round2(max(dlimit,iznos/100*_oUNeto),gZaok2)
   		skip
 	enddo
 	
	if radn->porol<>0 .and. gDaPorOl=="D" .and. !Obr2_9() 
		// poreska olaksica
   		if alltrim(cVarPorOl)=="2"
     			nPorOl:=RADN->porol
   		elseif alltrim(cVarPorol)=="1"
     			nPorOl:=round(parobr->prosld*radn->porol/100,gZaok)
  		else
     			nPorOl:= &("_I"+cVarPorol)
   		endif
   		if nPorOl>nPor 
			// poreska olaksica ne moze biti veca od poreza
     			nPorOl:=nPor
   		endif
   		nUPorOl+=nPorOl
 	endif

 	// napuni datoteku OPSLD
 	select ops
	seek radn->idopsst
 	select opsld
 	seek "1"+radn->idopsst
 	if found()
   		replace iznos with iznos+_ouneto, iznos2 with iznos2+nPorOl, ljudi with ljudi+1
 	else
   		append blank
   		replace id with "1", idops with radn->idopsst, ;
			iznos with _ouneto,;
                        iznos2 with iznos2+nPorOl, ljudi with 1
 	endif
 	seek "3"+ops->idkan
 	if found()
   		replace iznos with iznos+_ouneto, ;
			iznos2 with iznos2+nPorOl, ;
			ljudi with ljudi+1
 	else
   		append blank
   		replace id with "3", idops with ops->idkan, ;
		iznos with _ouneto,;
                iznos2 with iznos2+nPorOl, ljudi with 1
 	endif
 	seek "5"+ops->idn0
 	if found()
   		replace iznos with iznos+_ouneto, ;
		iznos2 with iznos2+nPorOl, ljudi with ljudi+1
 	else
   		append blank
   		replace id with "5", idops with ops->idn0, ;
			iznos with _ouneto,;
                        iznos2 with iznos2+nPorOl, ljudi with 1
 	endif
 	select ops
	seek radn->idopsrad
 	select opsld
 	seek "2"+radn->idopsrad
 	if found()
   		replace iznos with iznos+_ouneto, ;
		iznos2 with iznos2+nPorOl , ljudi with ljudi+1
 	else
   		append blank
   		replace id with "2", idops with radn->idopsrad, ;
			iznos with _ouneto,;
                        iznos2 with iznos2+nPorOl, ljudi with 1
 	endif
 	seek "4"+ops->idkan
 	if found()
   		replace iznos with iznos+_ouneto, ;
		iznos2 with iznos2+nPorOl, ljudi with ljudi+1
 	else
   		append blank
   		replace id with "4", idops with ops->idkan, ;
			iznos with _ouneto,;
                        iznos2 with iznos2+nPorOl, ljudi with 1
 	endif
 	seek "6"+ops->idn0
 	if found()
   		replace iznos with iznos+_ouneto, ;
		iznos2 with iznos2+nPorOl, ljudi with ljudi+1
 	else
   		append blank
   		replace id with "6", idops with ops->idn0, iznos with _ouneto,;
                        iznos2 with iznos2+nPorOl, ljudi with 1
 	endif
 	
	select ld

 	nPom:=ASCAN(aNeta,{|x| x[1]==vposla->idkbenef})
 	if nPom==0
    		AADD(aNeta,{vposla->idkbenef,_oUNeto})
	else
    		aNeta[nPom,2]+=_oUNeto
 	endif

 	for i:=1 to cLDPolja
  		cPom:=padl(alltrim(str(i)),2,"0")
  		select tippr; seek cPom; select ld
    		n777:=i+cMjesecDO-_mjesec
    		aRekap[IF(n777>cLDPolja,cLDPolja,n777),1]+=_s&cPom  
		// sati
  		nIznos:=_i&cPom
    		n777:=i+cMjesecDO-_mjesec
    		aRekap[IF(n777>cLDPolja,cLDPolja,n777),2]+=nIznos  // iznos
  		if tippr->uneto=="N" .and. nIznos<>0
    			if nIznos>0
      				nUOdbiciP+=nIznos
    			else
      				nUOdbiciM+=nIznos
    			endif
  		endif
 	next
	
	++nLjudi
 	nUSati+=_USati   
	// ukupno sati
 	nUNeto+=_UNeto  // ukupno neto iznos
 	nUNetoOsnova += _oUNeto  // ukupno neto osnova za obracun por.i dopr.
	// izracunaj bruto osnovicu za obracun varijanta 2
	if ( _oUNeto < parobr->minld )
		nZaBrutoOsnova += parobr->minld
	else
		nZaBrutoOsnova += _oUNeto
	endif

 	cTR := IF( RADN->isplata$"TR#SK", RADN->idbanka,;
                                   SPACE(LEN(RADN->idbanka)) )

 	IF LEN(aUkTR)>0 .and. ( nPomTR := ASCAN( aUkTr , {|x| x[1]==cTR} ) ) > 0
   		aUkTR[nPomTR,2] += _uiznos
 	ELSE
   		AADD( aUkTR , { cTR , _uiznos } )
 	ENDIF

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
     			ParObr(mjesec,IF(lViseObr,cObracun,),IF(!fSvi,cIdRj,))
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
enddo


if nLjudi==0
  nLjudi:=9999999
endif
B_ON
?? "LD: Rekapitulacija primanja"
B_OFF
?? IF(lIsplaceni,"","-neisplaceni radnici-")
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
 select por
 go top
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
 select rj
 hseek cIdrj

 select por
 go top

 select ld

 ?
 B_ON
 IF cMjesec==cMjesecDo
   ? "RJ:",cidrj,rj->naz,"  Mjesec:",str(cmjesec,2)+IspisObr()
   ?? "    Godina:", str(cGodina,4)
   B_OFF
 ELSE
   ? "RJ:",cidrj,rj->naz,"  Za mjesece od:",str(cmjesec,2),"do",str(cmjesecDo,2)+IspisObr()
   ?? "    Godina:", str(cGodina,4)
   B_OFF
 ENDIF
 ?
endif // fsvi
? SPACE(60) + "Porez:" + STR(por->iznos) + "%"
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
    if !fPorNaRekap
       ? "UKUPNO NETO:"; @ prow(),nC1+8  SAY  nUSati  pict gpics; ?? " sati"
       @ prow(),60 SAY nUNeto pict gpici; ?? "",gValuta
    else
       ? "UKUPNO NETO:"; @ prow(),nC1+5  SAY  nUSati  pict gpics; ?? " sati"
       @ prow(),42 SAY nUNeto pict gpici; ?? "",gValuta
       @ prow(),60 SAY nUNeto*(por->iznos/100) pict gpici; ?? "",gValuta
    endif
    // ****** radi 90 - ke
    _UNeto:=nUNeto
    _USati:=nUSati
    //***********
    ? m
  endif


  if tippr->(found()) .and. tippr->aktivan=="D" .and. (aRekap[i,2]<>0 .or. aRekap[i,1]<>0)
            aRez:=GodMj(_godina,_mjesec,-val(tippr->id)+1)
            cTpnaz:=padr("Za "+;
                         iif(tippr->id='14','<= ','')+;
                         str(arez[2],2)+"/"+str(arez[1],4),;
                         len(tippr->naz))
  ? tippr->id+"-"+cTPNaz
  nC1:=pcol()
  if !fPorNaRekap
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
  else
   if tippr->fiksan $ "DN"
     @ prow(),pcol()+5 SAY aRekap[i,1]  pict gpics; ?? " s"
     @ prow(),42 say aRekap[i,2]      pict gpici
     if tippr->uneto=="D"
        @ prow(),60 say aRekap[i,2]*(por->iznos/100)      pict gpici
     endif
   elseif tippr->fiksan=="P"
     @ prow(),pcol()+4 SAY aRekap[i,1]/nLjudi pict "999.99%"
     @ prow(),42 say aRekap[i,2]        pict gpici
     if tippr->uneto=="D"
        @ prow(),60 say aRekap[i,2]*(por->iznos/100)      pict gpici
     endif
   elseif tippr->fiksan=="C"
     @ prow(),42 say aRekap[i,2]        pict gpici
     if tippr->uneto=="D"
        @ prow(),60 say aRekap[i,2]*(por->iznos/100)      pict gpici
     endif
   elseif tippr->fiksan=="B"
     @ prow(),pcol()+4 SAY aRekap[i,1] pict "999999"; ?? " b"
     @ prow(),42 say aRekap[i,2]      pict gpici
     if tippr->uneto=="D"
        @ prow(),60 say aRekap[i,2]*(por->iznos/100)      pict gpici
     endif
   endif
  endif
   IF cMjesec==cMjesecDo
     Rekapld("PRIM"+tippr->id,cgodina,cmjesec,aRekap[i,2],aRekap[i,1])
   ELSE
     Rekapld("PRIM"+tippr->id,cgodina,cMjesecDo,aRekap[i,2],aRekap[i,1])
   ENDIF

  endif   // tippr aktivan

next  // cldpolja

IF IzFMKIni("LD","Rekap_ZaIsplatuRasclanitiPoTekRacunima","N",KUMPATH)=="D" .and. LEN(aUkTR)>1
  ? m
  ? "ZA ISPLATU:"
  ? "-----------"
  nMArr:=SELECT()
  SELECT KRED
  ASORT(aUkTr,,,{|x,y| x[1]<y[1]})
  FOR i:=1 TO LEN(aUkTR)
    IF EMPTY(aUkTR[i,1])
      ? PADR("B L A G A J N A",LEN(aUkTR[i,1]+KRED->naz)+1)
    ELSE
      HSEEK aUkTR[i,1]
      ? aUkTR[i,1], KRED->naz
    ENDIF
    @ prow(),60 SAY aUkTR[i,2] pict gpici; ?? "",gValuta
  NEXT
  SELECT (nMArr)
ENDIF

? m
if !fPorNaRekap
   ?  "UKUPNO ZA ISPLATU";  @ prow(),60 SAY nUIznos pict gpici; ?? "",gValuta
else
   ?  "UKUPNO ZA ISPLATU";  @ prow(),42 SAY nUIznos pict gpici; ?? "",gValuta
endif

? m
IF !lGusto
  ?
ENDIF

// proizvoljni redovi pocinju sa "9"
?
select tippr; seek "9"
do while !eof() .and. left(id,1)="9"
  If prow()>55+gPStranica
    FF
  endif
  ? tippr->id+"-"+tippr->naz
  cPom:=tippr->formula
  if !fPorNaRekap
     @ prow(),60 say round2(&cPom,gZaok2)      pict gpici
  else
     @ prow(),42 say round2(&cPom,gZaok2)      pict gpici
  endif
  IF cMjesec==cMjesecDo
    Rekapld("PRIM"+tippr->id,cgodina,cmjesec,round2(&cpom,gZaok2),0)
  ELSE
    Rekapld("PRIM"+tippr->id,cgodina,cMjesecDo,round2(&cpom,gZaok2),0)
  ENDIF
  skip
  IF eof() .or. !left(id,1)="9"
    ? m
  ENDIF
enddo


IF cMjesec==cMjesecDo     // za viÁe mjeseci nema prikaza poreza i doprinosa
IF !lGusto
  ?
ENDIF

nBO := 0

if gVarObracun == "2"
	// ovdje uzmi osnovicu namjenjenu ovoj varijanti
	nBO := bruto_osn( nZaBrutoOsnova )
	? bruto_isp(nZaBrutoOsnova)
else
	nBO := bruto_osn( nUNetoOsnova )
	? bruto_isp(nUNetoOsnova)
endif

@ prow(),pcol()+1 SAY nBo  pict gpici

?

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
      nPor+=round2(max(dlimit,iznos/100*MAX(_UNeto,PAROBR->prosld*gPDLimit/100)),gZaok2)
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
    _uneto:=MAX(_uneto,PAROBR->prosld*gPDLimit/100)
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
   nBo2:=bruto_osn(nUNeto2)
   // gPDLimit?!
   nPK3:=PAROBR->K3
   USE
   SELECT PAROBR
   GO (nParRec)
   O_LD
   PopWA()
 ENDIF


select por; go top
nPom:=nPor:=nPor2:=nPorOps:=nPorOps2:=0
nC1:=20

m:="----------------------- -------- ----------- -----------"
if cUmPD=="D"
  m+=" ----------- -----------"
endif

if cUmPD=="D"
  P_12CPI
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
IF !lGusto
 ?
 ?
ENDIF
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
        nBOOps:=bruto_osn(iznos)
        @ prow(),nc1 SAY nBOOps picture gpici
        nPom:=round2(max(dopr->dlimit,dopr->iznos/100*nBOOps),gZaok2)
        if cUmPD=="D"
          nBOOps2:=bruto_osn(piznos)
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
    IF !lGusto
      ?
    ENDIF
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
  P_10CPI
ENDIF
?
?


m:="---------------------------------"
altd()
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
P_10CPI
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
IF lGusto
  gRPL_Normal()
  gPStranica-=nDSGusto
ENDIF
END PRINT
CLOSERET


function Rekapld(cId,ngodina,nmjesec,nizn1,nizn2,cidpartner,copis,copis2,lObavDodaj)

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

 IF Pitanje(,"Izvjestaj se pravi za isplacene(D) ili neisplacene(N) radnike?","D")=="D"
   lIsplaceni:=.t.
   O_LD
 ELSE
   lIsplaceni:=.f.
   select (F_LDNO)  ; usex (KUMPATH+"LDNO") alias LD; set order to 1
 ENDIF

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

 IF lIsplaceni
   O_LD
 ELSE
   select (F_LDNO)  ; usex (KUMPATH+"LDNO") alias LD; set order to 1
 ENDIF

 cIdRadn:=space(_LR_)

 cSatiVO:="S"

 Box(,6,77)
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
   if lViseObr .and. EMPTY(cObracun)
     @ m_x+6,m_y+2 SAY "Prikaz sati (S-sabrati sve obracune , 1-obracun 1 , 2-obracun 2, ... )" GET cSatiVO VALID cSatiVO$"S123456789" PICT "@!"
     read; ESC_BCR
   endif
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
     IF !lViseObr .or. cSatiVO=="S" .or. cSatiVO==_obr
       ws&cPom+=_s&cPom
     ENDIF
     wi&cPom+=_i&cPom
   Next
   wUIznos+=_UIznos
   IF !lViseObr .or. cSatiVO=="S" .or. cSatiVO==_obr
     wUSati+=_USati
   ENDIF
   wUNeto+=_UNeto
   wIdRj := _IdRj
   wIdRadn := xIdRadn
   Gather("w")
   SELECT LD
   SKIP; LOOP
 EndIF
 cUneto:="D"
 for i:=1 to cLDPolja
  cPom:=padl(alltrim(str(i)),2,"0")
  select tippr; seek cPom
  IF !lViseObr .or. cSatiVO=="S" .or. cSatiVO==_obr
    ws&cPom+=_s&cPom
  ENDIF
  wi&cPom+=_i&cPom
 next
 select ld
 wUIznos+=_UIznos
 IF !lViseObr .or. cSatiVO=="S" .or. cSatiVO==_obr
   wUSati+=_USati
 ENDIF
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
return



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

