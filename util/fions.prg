#include "porld.ch"


function  UkRadnik()
local i,nArr

nArr:=select()

private cpom:=""

for i:=1 to cLDPolja
 cPom:=padl(alltrim(str(i)),2,"0")
 select tippr; seek cPom
 if tippr->(found()) .and. tippr->aktivan=="D"
   if tippr->ufs=="D"
     _USati+=_s&cPom
   endif
   _UIznos+=_i&cPom
   if tippr->uneto=="D"
      _Uneto+=_i&cPom
   else
      _UOdbici+=_i&cPom
   endif
 endif
next

select(nArr)
return (nil)


// --------------------------------------------------
// generisanje obracuna
// --------------------------------------------------
function mnu_gen()
private opc := {}
private opcexe := {}
private izbor:=1

AADD(opc, "1. generacija naknada za mjesec  ")
AADD(opcexe, {|| GenNak() })

menu_sc("genn")

return 


// ***************************
function GenNak()
local i,nArrm,nLjudi

Box(,4,60)
  @ m_x+1,m_y+2 SAY "Ova opcija vrsi generaciju naknada za mjesec"
  @ m_x+4,m_y+2 SAY "               <ESC> Izlaz"
  inkey(0)
BoxC()
if lastkey()==K_ESC
   closeret
endif

cIdRj:=gRj
cMjesec:=gMjesec
cGodina:=gGodina

O_RADN
O_PAROBR
O_TIPPR
O_RJES
O_LDNO
O_RADKR
//"1","str(godina)+str(mjesec)+idradn+idkred+naosnovu",KUMPATH+"RADKR")
O_LD
//"1","str(godina)+idrj+str(mjesec)+idradn"

cIdRadn:=space(_LR_)
cStrSpr:=space(3)
Box(,3,50)

@ m_x+1,m_y+2 SAY "Radna jedinica: "; ?? cIdRJ
@ m_x+2,m_y+2 SAY "Mjesec: "  GET  cMjesec  pict "99"
@ m_x+3,m_y+2 SAY "Godina: "  GET  cGodina  pict "9999"

read
ESC_BCR

BoxC()

SELECT PAROBR
SEEK STR(cMjesec)
IF FOUND()
  IF RIGHT(naz,1)=="*"
    IF SUBSTR(naz,4,2)==RIGHT(STR(cGodina),2)
      MsgBeep("Obracun za ovaj mjesec je zakljucen. Ispravke nisu dozvoljene!")
    ELSE
      MsgBeep("Obracun za ovaj mjesec nece biti uradjen dok ne definisete#"+;
              "parametre obracuna !")
    ENDIF
    CLOSERET
  ENDIF
ELSE
  MsgBeep("Obracun za ovaj mjesec nece biti uradjen dok ne definisete#"+;
          "parametre obracuna !")
  CLOSERET
ENDIF

select ld
seek str(cGodina,4)+cidrj+str(cMjesec,2)
if found()
  MsgBeep("Vec postoje obradjeni radnici ????!")
  if pitanje(,"Nastaviti ?","N")=="N"
      closeret
  endif
endif

select radkr; go top
EOF CRET

private cpom:="", lRekalk:=.t.

nLjudi:=0
Box(,1,12)
nTRKrRec:=0

do while !eof()

//  IF RADKR->idradn$"1502974195002#2803977198618"
//    AltD()
//  ENDIF

 if  .NOT. ( (pmjesec=0 .and. pgodina=0 .and. (godina<cGodina .or. (godina==cgodina .and. mjesec<=cMjesec)) ) .or. ;
              (pmjesec=cmjesec .and. pgodina=cgodina) ;
           )

      skip; loop
 endif
 select rjes; seek radkr->(naosnovu+idradn)
 if !empty(PrekDatPoc) .and. PrekDatPoc <= DMG(1,radkr->mjesec,radkr->godina)
    // ubilje§en je prekid rjeçenja
    select radkr
    skip; loop  // preskoŸi
 endif
 select radkr

 nTRKrRec:=recno()
 select rjes
 seek radkr->(NaOsnovu+IdRadn)

 select ld
 seek str(cGodina,4)+cidrj+str(cMjesec,2)+radkr->idradn

 Scatter()
 if !found()
   append blank
   _idradn:=radkr->idradn
   _idkred:=radkr->idkred
   _idrj:=cIdrj
   _mjesec:=cmjesec
   _godina:=cgodina
   gather()
   // pogledaj postoji li obracun medju obrisanim
   select ldno
   seek STR(cGodina,4)+cIdRj+STR(cMjesec,2)+radkr->idradn
   if found()    // ako postoji, ukini ga
     delete
   endif
   select ld
 endif  // ako nema sloga, dodaj ga

 ParObr(_mjesec)
 select radn; hseek _idradn; select ld

 for i:=1 to cLDPolja
  cPom:=padl(alltrim(str(i)),2,"0")
  select tippr; seek cPom; select ld
  if tippr->(found()) .and. tippr->aktivan=="D"
     //.and. "PAROBR" $ upper(tippr->formula)

    _UIznos:=_UIznos-_i&cPom
    if tippr->uneto=="D"           //  izbij ovu stavku
       _Uneto:=_UNeto-_i&cPom      //    ..
    else                           //    ..
       _UOdbici:=_UOdbici-_i&cPom  //    .
    endif                          //    ..

    Izracunaj(@_i&cPom)            //  preracunaj ovu stavku

    cPom:=padl(alltrim(str(i)),2,"0")  // MS 23.03.01.

    _UIznos+=_i&cPom               //  dodaj je nakon preracuna
    if tippr->uneto=="D"           //
       _Uneto+=_i&cPom             //
    else                           //
       _UOdbici+=_i&cPom           //
    endif

  endif

 next

 // test verzija
_usati:=0
for i:=1 to cLDPolja
  cPom:=padl(alltrim(str(i)),2,"0")
  select tippr; seek cPom
  if tippr->(found()) .and. tippr->aktivan=="D"
    if tippr->ufs=="D"
      _USati+=_s&cPom
    endif
  endif
next

select ld
Gather()  // LD

select radkr
go nTRKrRec; skip

@ m_x+1,m_y+2 SAY ++nljudi pict "99999"
enddo
Beep(1); inkey(1)
BoxC()
lRekalk:=.f.
return (nil)



function REkalkA()
local i,nArrm,nLjudi

Box(,4,60)
  @ m_x+1,m_y+2 SAY "Ova opcija vrsi preracunavanja onih stavki  primanja koja"
  @ m_x+2,m_y+2 SAY "u svojoj formuli proracuna sadrze paramtre obracuna."
  @ m_x+4,m_y+2 SAY "               <ESC> Izlaz"
  inkey(0)
BoxC()
if lastkey()==K_ESC
   closeret
endif

cIdRj    := gRj
cMjesec  := gMjesec
cGodina  := gGodina
cObracun := gObracun

O_RADN
O_PAROBR
O_LD

cIdRadn:=space(_LR_)
cStrSpr:=space(3)
Box(,3+IF(lViseObr,1,0),50)
@ m_x+1,m_y+2 SAY "Radna jedinica: "  GET cIdRJ
@ m_x+2,m_y+2 SAY "Mjesec: "  GET  cMjesec  pict "99"
@ m_x+3,m_y+2 SAY "Godina: "  GET  cGodina  pict "9999"
IF lViseObr
  @ m_x+4,m_y+2 SAY "Obracun:"  GET  cObracun WHEN HelpObr(.f.,cObracun) VALID ValObr(.f.,cObracun)
ENDIF
read; clvbox(); ESC_BCR
BoxC()

if lViseObr
  O_TIPPRN
else
  O_TIPPR
endif

SELECT LD
seek str(cGodina,4)+cidrj+str(cMjesec,2)+IF(lViseObr,cObracun,"")
EOF CRET

private cpom:="", lRekalk:=.t.

nLjudi:=0
Box(,1,12)
do while !eof() .and.  cgodina==godina .and. cidrj==idrj .and.;
         cmjesec=mjesec .and. IF(lViseObr,cObracun==obr,.t.)

 Scatter()
 ParObr(_mjesec,IF(lViseObr,_obr,),cIdRj)  // podesi parametre obra~una za ovaj mjesec

 select radn; hseek _idradn
 select ld

 for i:=1 to cLDPolja
  cPom:=padl(alltrim(str(i)),2,"0")
  select tippr; seek cPom; select ld
  if tippr->(found()) .and. tippr->aktivan=="D" .and. "PAROBR" $ upper(tippr->formula)

    _UIznos:=_UIznos-_i&cPom
    if tippr->uneto=="D"           //  izbij ovu stavku
       _Uneto:=_UNeto-_i&cPom      //    ..
    else                           //    ..
       _UOdbici:=_UOdbici-_i&cPom  //    .
    endif                          //    ..

    Izracunaj(@_i&cPom)            //  preracunaj ovu stavku

    // cPom je privatna, varijabla koja je Ÿesto koriçtena i to gotovo
    // uvijek kao privatna varijabla. Jednostavno, sada †u rijeçiti problem
    // ponovnim dodjeljivanjem vrijednosti, a za ovaj problem inaŸe smatram
    // da bi trebalo uvesti konvenciju davanja naziva ovakvim varijablama
    // --------------------------------------------------------------------
    cPom:=padl(alltrim(str(i)),2,"0") // MS 23.03.01.

    _UIznos+=_i&cPom               //  dodaj je nakon preracuna
    if tippr->uneto=="D"           //
       _Uneto+=_i&cPom             //
    else                           //
       _UOdbici+=_i&cPom           //
    endif

  endif

 next

 // test verzija
_usati:=0
for i:=1 to cLDPolja
   cPom:=padl(alltrim(str(i)),2,"0")
   select tippr; seek cPom
   if tippr->(found()) .and. tippr->aktivan=="D"
     if tippr->ufs=="D"
       _USati+=_s&cPom
     endif
   endif
next
select ld


 Gather()
 @ m_x+1,m_y+2 SAY ++nljudi pict "99999"
 skip
enddo
Beep(1); inkey(1)
BoxC()
lRekalk:=.f.
closeret


function REkalkPrim()
local i,nArrm,nLjudi

Box(,4,60)
  @ m_x+1,m_y+2 SAY "Ova opcija vrsi preracunavanje iznosa odredjenog primanja"
  @ m_x+4,m_y+2 SAY "               <ESC> Izlaz"
  inkey(0)
BoxC()
if lastkey()==K_ESC
   closeret
endif

cIdRj    := gRj
cMjesec  := gMjesec
cGodina  := gGodina
cObracun := gObracun

O_RADN
O_PAROBR
O_TIPPR
O_TIPPR2
O_LD

cIdRadn:=space(_LR_)
cStrSpr:=space(3)
nProcPrim:=0
cTipPP:="  "
cDN:="N"
Box(,7,50)
@ m_x+1,m_y+2 SAY "Radna jedinica: "  GET cIdRJ
@ m_x+2,m_y+2 SAY "Mjesec: "  GET  cMjesec  pict "99"
@ m_x+3,m_y+2 SAY "Godina: "  GET  cGodina  pict "9999"
IF lViseObr
  @ m_x+4,m_y+2 SAY "Obracun:"  GET  cObracun WHEN HelpObr(.f.,cObracun) VALID ValObr(.f.,cObracun)
ENDIF
@ m_x+5,m_y+2 SAY "Sifra tipa primanja " GET  cTipPP valid if(lViseObr.and.cObracun<>"1",P_Tippr2(@cTipPP),P_Tippr(@cTipPP)) .and. !empty(cTipPP)
@ m_x+6,m_y+2 SAY "Procenat za koji se vrsi promjena " GET  nProcPrim pict "999999.999"
@ m_x+7,m_y+2 SAY "Sigurno zelite nastaviti   (D/N) ?" GET  cDN pict "@!" valid cDN $"DN"
read; clvbox(); ESC_BCR
BoxC()

SELECT (F_TIPPR) ; USE
SELECT (F_TIPPR2); USE
if lViseObr
  O_TIPPRN
else
  O_TIPPR
endif

SELECT LD

seek str(cGodina,4)+cidrj+str(cMjesec,2)+IF(lViseObr,cObracun,"")
EOF CRET

private cpom:=""
nLjudi:=0
Box(,1,12)

nStariIznos:=nNoviIznos:=0

do while !eof() .and.  cgodina==godina .and. cidrj==idrj .and.;
         cmjesec=mjesec .and. IF(lViseObr,cObracun==obr,.t.)

 Scatter()
 ParObr(_mjesec,IF(lViseObr,_obr,),cIdRj)  // podesi parametre obra~una za ovaj mjesec

 select radn; hseek _idradn
 select ld

 for i:=1 to cLDPolja
  cPom:=padl(alltrim(str(i)),2,"0")
  if cPom==cTipPP .and. _i&cPom<>0  // to je to primanje
    select tippr; seek cPom; select ld

    nStariIznos := _i&cPom

    _UIznos:=_UIznos-nStariIznos
    if tippr->uneto=="D"                    //  izbij ovu stavku
       _Uneto:=_UNeto-nStariIznos           //    ..
    else                                    //    ..
       _UOdbici:=_UOdbici-nStariIznos       //    .
    endif                                   //    ..

    nNoviIznos := _i&cPom := round(nStariIznos*(1+nProcPrim/100),gZaok)
    ///*******Izracunaj(@_i&cPom)            //  preracunaj ovu stavku

    if tippr->fiksan=="P"
      // preraŸunaj i procenat
      _s&cPom :=  ROUND( _s&cPom * nNoviIznos / nStariIznos , 2)
      if _s&cPom=0
        MsgBeep("Istopio se postotak kod radnika:'"+_idradn+"' !")
      endif
      // ponovo izracunaj iznos radi zaokru§enja
      Izracunaj(@_i&cPom)

      cPom:=padl(alltrim(str(i)),2,"0")  // MS 23.03.01.

      nNoviIznos := _i&cPom
    endif

    _UIznos+=nNoviIznos            //  dodaj je nakon preracuna
    if tippr->uneto=="D"           //
       _Uneto+=nNoviIznos          //
    else                           //
       _UOdbici+=nNoviIznos        //
    endif

  endif

 next

 // test verzija
_usati:=0
for i:=1 to cLDPolja
   cPom:=padl(alltrim(str(i)),2,"0")
   select tippr; seek cPom
   if tippr->(found()) .and. tippr->aktivan=="D"
     if tippr->ufs=="D"
       _USati+=_s&cPom
     endif
   endif
next
select ld


 Gather()
 @ m_x+1,m_y+2 SAY ++nljudi pict "99999"
 skip
enddo
Beep(1); inkey(1)
BoxC()
closeret


function REkalkPr2()
local i,nArrm,nLjudi

Box(,4,60)
  @ m_x+1,m_y+2 SAY "Ova opcija vrsi preracunavanje odredjenog primanja"
  @ m_x+4,m_y+2 SAY "               <ESC> Izlaz"
  inkey(0)
BoxC()
if lastkey()==K_ESC
   closeret
endif

cIdRj    := gRj
cMjesec  := gMjesec
cGodina  := gGodina
cObracun := gObracun

O_RADN
O_PAROBR
O_TIPPR
O_TIPPR2
O_LD

cIdRadn:=space(_LR_)
cStrSpr:=space(3)
nProcPrim:=0
cTipPP:="  "
cDN:="N"
Box(,7,50)
@ m_x+1,m_y+2 SAY "Radna jedinica: "  GET cIdRJ
@ m_x+2,m_y+2 SAY "Mjesec: "  GET  cMjesec  pict "99"
@ m_x+3,m_y+2 SAY "Godina: "  GET  cGodina  pict "9999"
IF lViseObr
  @ m_x+4,m_y+2 SAY "Obracun:"  GET  cObracun WHEN HelpObr(.f.,cObracun) VALID ValObr(.f.,cObracun)
ENDIF
@ m_x+5,m_y+2 SAY "Sifra tipa primanja " GET  cTipPP valid if(lViseObr.and.cObracun<>"1",P_Tippr2(@cTipPP),P_Tippr(@cTipPP)) .and. !empty(cTipPP)
@ m_x+7,m_y+2 SAY "Sigurno zelite nastaviti   (D/N) ?" GET  cDN pict "@!" valid cDN $"DN"
read; clvbox(); ESC_BCR
BoxC()

SELECT (F_TIPPR) ; USE
SELECT (F_TIPPR2); USE
if lViseObr
  O_TIPPRN
else
  O_TIPPR
endif
SELECT LD

seek str(cGodina,4)+cidrj+str(cMjesec,2)+IF(lViseObr,cObracun,"")
EOF CRET

private cpom:=""
nLjudi:=0
Box(,1,12)
do while !eof() .and.  cgodina==godina .and. cidrj==idrj .and.;
         cmjesec=mjesec .and. IF(lViseObr,cObracun==obr,.t.)

 Scatter()
 ParObr(_mjesec,IF(lViseObr,_obr,),cIdRj)  // podesi parametre obra~una za ovaj mjesec

 select radn; hseek _idradn
 select ld

 for i:=1 to cLDPolja
  cPom:=padl(alltrim(str(i)),2,"0")
  if cPom==cTipPP  // to je to primanje
    select tippr; seek cPom; select ld
    _UIznos:=_UIznos-_i&cPom
    if tippr->uneto=="D"           //  izbij ovu stavku
       _Uneto:=_UNeto-_i&cPom      //    ..
    else                           //    ..
       _UOdbici:=_UOdbici-_i&cPom  //    .
    endif                          //    ..


    //_i&cPom:=round(_i&cPom*(1+nProcPrim/100),gZaok)
    Izracunaj(@_i&cPom)

    cPom:=padl(alltrim(str(i)),2,"0")  // MS 23.03.01.

    ///*******Izracunaj(@_i&cPom)            //  preracunaj ovu stavku


    _UIznos+=_i&cPom               //  dodaj je nakon preracuna
    if tippr->uneto=="D"           //
       _Uneto+=_i&cPom             //
    else                           //
       _UOdbici+=_i&cPom           //
    endif

  endif

 next

 // test verzija
_usati:=0
for i:=1 to cLDPolja
   cPom:=padl(alltrim(str(i)),2,"0")
   select tippr; seek cPom
   if tippr->(found()) .and. tippr->aktivan=="D"
     if tippr->ufs=="D"
       _USati+=_s&cPom
     endif
   endif
next
select ld


 Gather()
 @ m_x+1,m_y+2 SAY ++nljudi pict "99999"
 skip
enddo
Beep(1); inkey(1)
BoxC()
closeret

function RekalkS()
local i,nArrm,nLjudi


Box(,4,60)
  @ m_x+1,m_y+2 SAY "Ova opcija vrsi preracunavanja:                        "
  @ m_x+2,m_y+2 SAY "NETO SATI, NETO IZNOS, UKUPNO ZA ISPLATU, UKUPNO ODBICI"
  @ m_x+4,m_y+2 SAY "               <ESC> Izlaz"
  inkey(0)
BoxC()
if lastkey()==K_ESC
   closeret
endif

cMjesec  := gMjesec
cGodina  := gGodina
cObracun := gObracun

O_RADN
O_PAROBR
O_LD

cIdRadn:=space(_LR_)
cStrSpr:=space(3)

Box(,3+IF(lViseObr,1,0),50)
 @ m_x+2,m_y+2 SAY "Mjesec: "  GET  cMjesec  pict "99"
 @ m_x+3,m_y+2 SAY "Godina: "  GET  cGodina  pict "9999"
 IF lViseObr
   @ m_x+4,m_y+2 SAY "Obracun:"  GET  cObracun WHEN HelpObr(.f.,cObracun) VALID ValObr(.f.,cObracun)
 ENDIF
 read; clvbox(); ESC_BCR
BoxC()

if lViseObr
  O_TIPPRN
else
  O_TIPPR
endif

SELECT LD
set order to tag "2"
seek str(cGodina,4)+str(cMjesec,2)+IF(lViseObr,cObracun,"")

EOF CRET

private cpom:=""
nLjudi:=0
Box(,1,12)
do while !eof() .and.  cgodina==godina .and.  cmjesec=mjesec .and.;
         IF(lViseObr,cObracun==obr,.t.)

 Scatter()
 ParObr(_mjesec,IF(lViseObr,_obr,),_idrj)  // podesi parametre obra~una za ovaj mjesec

 select radn; hseek _idradn
 select ld


 _USati:=0
 _UNeto:=0;_UOdbici:=0
 UkRadnik()  // filuje _USati,_UNeto,_UOdbici
 _UIznos:=_UNeto+_UOdbici

 Gather()
 @ m_x+1,m_y+2 SAY ++nljudi pict "99999"
 skip
enddo
Beep(1); inkey(1)
BoxC()
closeret


// -------------------------------------
// parametri obracuna
// -------------------------------------
function ParObr(nMjesec,cObr,cIDRJ)
 LOCAL nNaz, nRec1:=0, nRec2:=0, nRec3:=0
 IF cObr==NIL; cObr:=""; ENDIF
 IF cIDRJ==NIL; cIDRJ:=""; ENDIF
 nArr := SELECT()
 SELECT PAROBR
 SEEK STR(nMjesec,2)+cObr
 IF !FOUND() .or. EOF()
   SKIP -1
 ENDIF
 IF IzFMKINI("LD","VrBodaPoRJ","N",KUMPATH)=="D"
   nRec1:=RECNO()
   DO WHILE !EOF() .and. id==STR(nMjesec,2)
     IF lViseObr .and. cObr<>obr
       SKIP 1; LOOP
     ENDIF
     IF IDRJ==cIdRj
       nRec3:=RECNO()
       EXIT
     ENDIF
     IF EMPTY(IDRJ)
       nRec2:=RECNO()
     ENDIF
     SKIP 1
   ENDDO
   IF nRec3<>0
     GO (nRec3)
   ELSEIF nRec2<>0
     GO (nRec2)
   ELSE
     GO (nRec1)
   ENDIF
 ENDIF
 SELECT (nArr)
RETURN



****************************
* fprikaz = .T. prikazi
* fprikaz = .F.
****************************
function Izracunaj(ixx,fPrikaz)
private cFormula

if pcount()==1; fPrikaz:=.t.; endif
cFormula:=trim(tippr->formula)
if tippr->fiksan<>"D" // ako je fiksan iznos ni{ta ne izra~unavaj
 if empty(cFormula)
   ixx:=0
 else
   ixx:=&cFormula
 endif
ixx:=round(ixx,gZaok)
endif
//if fprikaz
// @ row(),m_y+50 SAY ixx pict "999999.99"
//endif
return .t.



***************************************************************************
function Prosj3(cTip,cTip2)
* if cTip== "1"  -> prosjek neta/ satu
* if ctip== "2"  -> prosjek ukupnog primanja/satu
* if cTip== "3"  -> prosjek neta
* if cTip== "4"  -> prosjek ukupnog primanja
* if cTip== "5"  -> prosjek ukupnog primanja/ukupno sati
* if cTip== "6"  -> prosjek ukupnih "raznih" primanja/satu
* if cTip== "7"  -> prosjek ukupnih "raznih" primanja/ukupno sati
* if cTip== "8"  -> prosjek ukupnih "raznih" primanja
*
* if ctip2=="1" -> striktno predhodna 3 mjeseca
* if ctip2=="2" -> vracam se mjesec unazad u kome nije bilo godisnjeg
*************************************************************************
local nMj1:=nMj2:=nMj3:=0,nDijeli:=0, cmj1:=cmj2:=cmj3:="",npomak:=0,i:=0
local nss1:=0,nss2:=0,nss3:=0,nSumsat:=0
local nsp1:=0, nsp2:=0, nsp3:=0

PushWA()

//CREATE_INDEX("LDi1","str(godina)+idrj+str(mjesec)+idradn","LD")
//CREATE_INDEX("LDi2","str(godina)+str(mjesec)+idradn","LD")
set order to tag (TagVO("2","I"))

i:=0
if ctip2=="2"
 do while .t.
  ++i
  if _Mjesec-i<1
   seek str(_Godina-1,4)+str(12+_Mjesec-i,2)+_idradn
   cmj1:=str(12+_mjesec-i,2)+"."+str(_godina-1,4)
  else
   seek str(_Godina,4)+str(_mjesec-i,2)+_idradn
   cmj1:=str(_mjesec-i,2)+"."+str(_godina,4)
  endif
  if &gFUGod<>0
    nPomak++
  else
    exit
  endif
  if i>12  // nema podataka
    exit
  endif
 enddo
endif

if _mjesec-1-npomak<1
  seek str(_Godina-1,4)+str(12+_Mjesec-1-npomak,2)+_idradn
  cmj1:=str(12+_mjesec-1-npomak,2)+"."+str(_godina-1,4)
else
  seek str(_Godina,4)+str(_Mjesec-1-npomak,2)+_idradn
  cmj1:=str(_mjesec-1-npomak,2)+"."+str(_godina,4)
endif
if found()
   if lViseObr
     ScatterS(godina,mjesec,idrj,idradn,"w")
   else
     wuneto := uneto
     wusati := usati
   endif
   if cTip $ "13"
     nMj1:= wUNeto
   elseif cTip $ "678"
     nMj1:=URPrim()
   else
     nMj1:=UPrim()
   endif
   if cTip $ "126"
    nss1:=wUSati
    nsp1:=nMj1
    if wusati<>0
      nMj1:=nMj1/wUSati
    else
      nMj1:=0
    endif
   elseif ctip $ "5"
      nss1:=USati()
   elseif ctip $ "7"
      nss1:=URSati()
   endif
   if nMj1<>0; ++ndijeli; endif
endif
if _mjesec-2-npomak<1
  seek str(_Godina-1,4)+str(12+_Mjesec-2-npomak,2)+_idradn
  cmj2:=str(12+_mjesec-2-npomak,2)+"."+str(_godina-1,4)
else
  seek str(_Godina,4)+str(_Mjesec-2-npomak,2)+_idradn
  cmj2:=str(_mjesec-2-npomak,2)+"."+str(_godina,4)
endif
if found()
   if lViseObr
     ScatterS(godina,mjesec,idrj,idradn,"w")
   else
     wuneto := uneto
     wusati := usati
   endif
   if cTip $ "13"
     nMj2:= wUNeto
   elseif cTip $ "678"
     nMj2:=URPrim()
   else
     nMj2:=UPrim()
   endif
   if cTip $ "126"
    nss2:=wUSati
    nsp2:=nMj2
    if wusati<>0
      nMj2:=nMj2/wUSati
    else
      nMj2:=0
    endif
   elseif ctip $ "5"
      nss2:=USati()
   elseif ctip $ "7"
      nss2:=URSati()
   endif
   if nMj2<>0; ++ndijeli; endif
endif
if _mjesec-3-npomak<1
  seek str(_Godina-1,4)+str(12+_Mjesec-3-npomak,2)+_idradn
  cmj3:=str(12+_mjesec-3-npomak,2)+"."+str(_godina-1,4)
else
  seek str(_Godina,4)+str(_Mjesec-3-npomak,2)+_idradn
  cmj3:=str(_mjesec-3-npomak,2)+"."+str(_godina,4)
endif
if found()
   if lViseObr
     ScatterS(godina,mjesec,idrj,idradn,"w")
   else
     wuneto := uneto
     wusati := usati
   endif
   if cTip $ "13"
     nMj3:= wUNeto
   elseif cTip $ "678"
     nMj3:=URPrim()
   else
     nMj3:=UPrim()
   endif
   if cTip $ "126"
    nss3:=wUSati
    nsp3:=nMj3
    if wusati<>0
      nMj3:=nMj3/wUSati
    else
      nMj3:=0
    endif
   elseif ctip $ "5"
      nss3:=USati()
   elseif ctip $ "7"
      nss3:=URSati()
   endif
   if nMj3<>0; ++ndijeli; endif
endif

if nDijeli==0; nDijeli:=99999999; endif
nSumsat:=IF(nss1+nss2+nss3<>0,nss1+nss2+nss3,99999999)

Box("#"+IF(cTip$"57","UKUPNA PRIMANJA","Prosjek")+" ZA MJESECE UNAZAD:",6,60)
 @ m_x+2,m_y+2 SAY cmj1; @ row(),col()+2 SAY nMj1 pict "999999.999"
 IF cTip$"126"; ?? "  primanja/sati:"; ?? nsp1,"/",nss1; ENDIF
 IF cTip$"57"; ?? "  sati:"; ?? nss1; ENDIF
 @ m_x+3,m_y+2 SAY cmj2; @ row(),col()+2 SAY nMj2 pict "999999.999"
 IF cTip$"126"; ?? "  primanja/sati:"; ?? nsp2,"/",nss2; ENDIF
 IF cTip$"57"; ?? "  sati:"; ?? nss2; ENDIF
 @ m_x+4,m_y+2 SAY cmj3; @ row(),col()+2 SAY nMj3 pict "999999.999"
 IF cTip$"126"; ?? "  primanja/sati:"; ?? nsp3,"/",nss3; ENDIF
 IF cTip$"57"; ?? "  sati:"; ?? nss3; ENDIF
 @ m_x+6,m_y+2 SAY "Prosjek"; @ row(),col()+2 SAY (nMj3+nMj2+nMj1)/IF(cTip$"57",nSumsat,nDijeli) pict "999999.999"
 inkey(0)
BoxC()

PopWa()

return  (nMj3+nMj2+nMj1)/IF(cTip$"57",nSumsat,ndijeli)

********************
*ukupna primanja
********************
function UPrim()
IF lViseObr
  c719:=UbaciPrefix(gFUPrim,"w")
ELSE
  c719:=gFUPrim
ENDIF
return &c719

********************
*ukupna primanja
********************
function USati()
IF lViseObr
  c719:=UbaciPrefix(gFUSati,"w")
ELSE
  c719:=gFUSati
ENDIF
return &c719

********************
*ukupna razna primanja
********************
function URPrim()
IF lViseObr
  c719:=UbaciPrefix(gFURaz,"w")
ELSE
  c719:=gFURaz
ENDIF
return &c719

********************
*ukupna razna primanja
********************
function URSati()
IF lViseObr
  c719:=UbaciPrefix(gFURSati,"w")
ELSE
  c719:=gFURSati
ENDIF
return &c719

**********************************************
function Prosj1(cTip,cTip2,cF0)
* if cTip== "1"  -> prosjek neta/ satu
* if ctip== "2"  -> prosjek ukupnog primanja/satu
* if cTip=="3"  -> prosjek neta
* if cTip=="4"  -> prosjek ukupnog primanja
* if cTip=="5"  -> prosjek ukupnog primanja/ukupno sati
* if cTip== "6"  -> prosjek ukupnih "raznih" primanja/satu
* if cTip== "7"  -> prosjek ukupnih "raznih" primanja/ukupno sati
* if cTip== "8"  -> prosjek ukupnih "raznih" primanja

* if cTip2=="1"  -> prosli mjesec i  primanje <> 0
* if ctip2=="2"  -> predhodni mjesec za koji je UNeto==UPrim() i primanje <> 0
* if ctip2=="3"  -> predhodni mjesec za koji je UNeto==URPrim() i primanje <> 0
*
* cF0 = "_i18"  - ne uzimaj mjesec ako je _i18<>0
*************************************************
local nMj1:=0,i:=0
private cFormula
PushWA()

if cF0=NIL
   cFormula:="0"
else
   cFormula:=cF0
endif

//CREATE_INDEX("LDi1","str(godina)+idrj+str(mjesec)+idradn","LD")
//CREATE_INDEX("LDi2","str(godina)+str(mjesec)+idradn","LD")
set order to tag (TagVO("2","I"))

i:=0
do while .t.
 ++i
 if _Mjesec-i<1
   seek str(_Godina-1,4)+str(12+_Mjesec-i,2)+_idradn
   cmj1:=str(12+_mjesec-i,2)+"."+str(_godina-1,4)
 else
   seek str(_Godina,4)+str(_Mjesec-i,2)+_idradn
   cmj1:=str(_mjesec-i,2)+"."+str(_godina,4)
 endif

 if found()
   if lViseObr
     ScatterS(godina,mjesec,idrj,idradn,"w")
   else
     wuneto := uneto
     wusati := usati
   endif
   if cTip $ "13"
     nMj1:= wUNeto
   elseif cTip $ "678"
     nMj1:=URPrim()
   else
     nMj1:=UPrim()
   endif
   if cTip $ "126"
    if wusati<>0
      nMj1:=nMj1/wUSati
    else
      nMj1:=0
    endif
   elseif cTip $ "5"
    if USati()<>0
      nMj1:=nMj1/USati()
    else
      nMj1:=0
    endif
   elseif cTip $ "7"
    if URSati()<>0
      nMj1:=nMj1/URSati()
    else
      nMj1:=0
    endif
   endif
 else
   MsgBeep("Prosjek je uzet iz sifrarnika radnika - OSN.BOL. !")
   SELECT RADN; SET ORDER TO TAG "1"; GO TOP
   HSEEK _IdRadn
   nMj1 := osnbol
   SELECT LD
   exit
 endif

 if nMj1==0; loop; endif

 if &cFormula<>0
    loop
 endif

 if cTip2=="1"  // gleda se prosli mjesec
   exit
 elseif cTip2=="3"
   if round(wUNeto,2)==round(URPrim(),2)
     exit
   endif
 else
   if round(wUNeto,2)==round(UPrim(),2)
     exit
   endif
 endif

enddo

Box(,4,50)
 @ m_x+1,m_y+2 SAY "PRIMANJE ZA PROSLI MJESEC:"
 @ m_x+2,m_y+2 SAY  cmj1; @ row(),col()+2 SAY nMj1 pict "999999.999"
 @ m_x+4,m_y+2 SAY "Prosjek"; @ row(),col()+2 SAY nMj1 pict "999999.999"
 inkey(0)
BoxC()

PopWa()

return  nMj1


*********************
*
*********************
function Predhodni(i,cVar,cObr)
 local cKljuc:=""
 if cObr==NIL; cObr:="1"; ENDIF
 private cpom:=""

 IF "U" $ TYPE("lRekalk"); lRekalk:=.f.; ENDIF

 IF lRekalk .and. !TPImaPO(SUBSTR(cVar,3))  // pri rekalkulaciji ne racunaj
   return 0                                 // predhodni ukoliko u formuli
 ENDIF                                      // nema parametara obracuna

 PushWa()

 //CREATE_INDEX("LDi1","str(godina)+idrj+str(mjesec)+idradn","LD")
 //CREATE_INDEX("LDi2","str(godina)+str(mjesec)+idradn","LD")
 set order to tag (TagVO("2","I"))

 if _Mjesec-i<1
    hseek str(_Godina-1,4)+str(12+_Mjesec-1,2)+_idradn
 else
    hseek str(_Godina,4)+str(_Mjesec-i,2)+_idradn
 endif

 cPom:=cVar
 cField=substr(cPom,2)

 if lViseObr
   &cPom := 0
   cKljuc := STR(godina,4)+STR(mjesec,2)+idradn
   IF !EMPTY(cObr)
     do while !eof() .and. STR(godina,4)+STR(mjesec,2)+idradn == cKljuc
       IF obr==cObr
         &cPom += &cField
       ENDIF
       skip 1
     enddo
   ELSE
     do while !eof() .and. STR(godina,4)+STR(mjesec,2)+idradn == cKljuc
       &cPom += &cField
       skip 1
     enddo
   ENDIF
 else
   &cPom:=&cField
 endif

 PopWa()
return 0


************************************
function PrimSM(cOznaka,cTipPr)
*
* cOznaka - oznaka primanja u smecu
* cTipPr  - "01, "02" , ...
*           "NE" - neto
* izlaz = primanje iz smeca
************************************
local nRez:=0

private cTipa:=""
//"LDSMi1","Obr+str(godina)+str(mjesec)+idradn+idrj",PRIVPATH+"LDSM")

private cpom:=""

PushWa()

select (F_LDSM)
if !used()
  O_LDSM
endif

seek cOznaka+str(_godina)+str(_mjesec)+_idradn+_idrj
if cTippr=="NE"
    nRez:=UNETO
else
    cTipa:="I"+cTipPr
    nRez :=&cTipa
endif

PopWa()
return nRez

**************************
**************************
function Fill(xValue,xIzn)
 if type(xIzn)<>"UI" .and. type(xIzn)<>"UE"
   xVAlue:=&xIzn
   ShowGets()
 endif
return 0

**************************
function FillR(xValue,xIzn)
* fora za bolovanje
* Filr("OSNBOL",PROSJ1("2","2","I18"))
**************************
 PushWa()
 select radn
 replace &xVAlue with xIzn
 PopWa()
return xIzn

**************************
function GETR(cPrompt,xValue)
**************************
local nRezult

private Getlist:={}

 PushWa()
 select radn

 nRezult:=&xValue
 Box(,2,60)
    @ m_x+1,m_y+2 SAY cPrompt GET nRezult
    read
 BoxC()
 if lastkey()==K_ESC
    return &xValue
 endif
 replace &xValue with nRezult
 PopWa()
return nRezult

********************
********************
function Brisi()
local nTrec
local cIdRadn, cMjesec, cIdRj, fnovi

O_RADKR
set order to tag "PGM"
//"PGM","idradn+str(pgodina)+str(pmjesec)",KUMPATH+"RADKR")

O_RADN
do while .t.
O_LD

cIdRadn:=space(_LR_)
cIdRj:=gRj; cMjesec:=gMjesec
cGodina:=gGodina
cObracun := gObracun
Box(,4,60)

@ m_x+1,m_y+2 SAY "Radna jedinica: "; QQOUTC(cIdRJ,"N/W")
@ m_x+2,m_y+2 SAY "Mjesec: "; QQOUTC(str(cMjesec,2),"N/W")
if lViseObr
  @ m_x+2,col()+2 SAY "Obracun: "; QQOUTC(cObracun,"N/W")
endif
@ m_x+3,m_y+2 SAY "Godina: "; QQOUTC(str(cGodina,4),"N/W")
@ m_x+4,m_y+2 SAY "Radnik" GET cidradn valid {|| cidradn $ "XXXXXX" .or. P_Radn(@cIdRadn),setpos(m_x+2,m_y+20),qqout(trim(radn->naz)+" ("+trim(radn->imerod)+") "+radn->ime),.t.}
read; ESC_BCR
BoxC()

if cidradn<>"XXXXXX"
seek str(cGodina,4)+cIdRj+str(cMjesec,2)+if(lViseObr,cObracun,"")+cIdRadn
if found()
  if Pitanje(,"Sigurno zelite izbrisati ovaj zapis D/N","N")=="D"
    delete
    // izbrisi sve oznake da je naknada isplacena u radkr
    select radkr
    seek cidradn+str(cgodina,4)+str(cmjesec,2)
    do while !eof() .and. cidradn+str(cgodina,4)+str(cmjesec,2)==idradn+str(pgodina,4)+str(pmjesec,2)
       skip; nTrec:=recno(); skip -1
       replace pgodina with 0, pmjesec with 0, placeno with 0
       go nTrec
    enddo
    select ld
  endif
else
  Msg("Podatak ne postoji...",4)
endif
else
  select ld; set order to 0
  if flock()
   go top
   postotak(1,reccount(),"Ukloni 0 zapise")
   do while !eof()
     nPom:=0
     Scatter()
     for i:=1 to cLDPolja
        cPom:=padl(alltrim(str(i)),2,"0")
        nPom+=abs(_i&cPom) + abs(_s&cPom)  // ako su sve nule
     next
     if round(npom,5)=0
        dbdelete2()
     endif
     Postotak(2,recno())
     skip
   enddo
   Postotak(0)
  else
    msgbeep("Neko vec koristi datoteku LD")
  endif
endif

select ld; use
enddo
closeret

********************
********************
function BrisiMj()
local cMjesec,cIdRj,fnovi

O_LDNO
O_RADKR
select radkr
set order to tag "PGM"
//"PGM","idradn+str(pgodina)+str(pmjesec)",KUMPATH+"RADKR")

O_RADN
do while .t.
O_LD

cIdRadn:=space(_LR_)
cIdRj:=gRj; cMjesec:=gMjesec
cGodina:=gGodina
cObracun := gObracun
Box(,4,60)

@ m_x+1,m_y+2 SAY "Radna jedinica: " GET cIdRJ
@ m_x+2,m_y+2 SAY "Mjesec: "  GET cMjesec pict "99"
if lViseObr
  @ m_x+2,col()+2 SAY "Obracun: " GET cObracun WHEN HelpObr(.f.,cObracun) VALID ValObr(.f.,cObracun)
endif
@ m_x+3,m_y+2 SAY "Godina: "  GET cGodina pict "9999"
read; clvbox(); ESC_BCR
BoxC()

O_PAROBR
SEEK STR(cMjesec)
IF FOUND()
  IF RIGHT(naz,1)=="*"
    IF SUBSTR(naz,4,2)==RIGHT(STR(cGodina),2)
      MsgBeep("Obracun za ovaj mjesec je zakljucen. Ispravke nisu dozvoljene!")
    ELSE
      MsgBeep("Obracun za ovaj mjesec ne moze biti ni brisan ni uradjen dok ne#"+;
              "definisete parametre obracuna (opcija D. u osnovnom meniju) !")
    ENDIF
    CLOSERET
  ENDIF
ELSE
  MsgBeep("Obracun za ovaj mjesec ne moze biti ni brisan ni uradjen dok ne#"+;
          "definisete parametre obracuna (opcija D. u osnovnom meniju) !")
  CLOSERET
ENDIF

if pitanje(,"Sigurno zelite izbrisati sve podatke za RJ za ovaj mjesec !?","N")=="N"
 closeret
endif

Msgo("Sacekajte, brisem podatke....")

SELECT LD
seek str(cGodina,4)+cIdRj+str(cMjesec,2)+if(lViseObr,cObracun,"")

do while str(cGodina,4)+cIdRj+str(cMjesec,2)== str(Godina,4)+IdRj+str(Mjesec,2) .and.;
         if(lViseObr,cObracun==obr,.t.)
   skip; nRec:=recno(); skip -1
   cIdRadn:=idradn
   delete
    // izbrisi sve oznake da je naknada isplacena u radkr
    select radkr
    seek cidradn+str(cgodina,4)+str(cmjesec,2)
    do while !eof() .and. cidradn+str(cgodina,4)+str(cmjesec,2)==idradn+str(pgodina,4)+str(pmjesec,2)
       skip; nTrec:=recno(); skip -1
       replace pgodina with 0, pmjesec with 0, placeno with 0
       go nTrec
    enddo
    select ld

   go nRec
enddo

SELECT LDNO
seek str(cGodina,4)+cIdRj+str(cMjesec,2)
do while str(cGodina,4)+cIdRj+str(cMjesec,2)== str(Godina,4)+IdRj+str(Mjesec,2)
	skip
	nRec:=recno()
	skip -1
    	cIdRadn:=idradn
    	delete
    	go nRec
enddo

MsgC()

exit
enddo
closeret

**********************
**********************
function FillBrBod()
if radn->brbod<>_brbod
  if Pitanje(,"Staviti u sifrarnik radnika ovu vrijednost D/N?","N")=="D"
     select radn
     replace brbod with _brbod
     select ld
  endif
endif
return .t.

****************************
****************************
function FillKMinRad()
if radn->kminrad<>_kminrad
  if Pitanje(,"Staviti u sifrarnik radnika ovu vrijednost D/N?","N")=="D"
     select radn
     replace kminrad with _kminrad
     select ld
  endif
endif
return .t.

****************************
****************************
function FillVPosla()
if radn->idvposla<>_idvposla
  if Pitanje(,"Staviti u sifrarnik radnika ovu vrijednost D/N?","N")=="D"
     select radn
     replace idvposla with _idvposla
     select ld
  endif
endif
return .t.


******************************
* izracun bruto iznosa
******************************
function Bruto(nbruto,ndopr)

nBruto:=_UNETO
nPorDopr:=0
select (F_POR)
if !used(); O_POR; endif
select (F_DOPR)
if !used(); O_DOPR; endif
select (F_KBENEF)
if !used(); O_KBENEF; endif
nBO:=0
nBo:=parobr->k3/100*MAX(_UNeto,PAROBR->prosld*gPDLimit/100)
select por; go top
nPom:=nPor:=0
nC1:=30; nPorOl:=0
do while !eof()
   nPom:=max(dlimit,round(iznos/100*MAX(_UNeto,PAROBR->prosld*gPDLimit/100),gZaok))
   nPor+=nPom
   skip
enddo
nBruto+=nPor
nPorDopr+=nPor
if radn->porol<>0  // poreska olaksica
   nPorOl:=parobr->prosld*radn->porol/100
   if nPorOl>nPor // poreska olaksica ne moze biti veca od poreza
     nPorOl:=nPor
   endif
   nBruto-=nPorol
   nPorDopr-=nPorOl
endif
if radn->porol<>0
  //? m
  //? "Ukupno Porez"
    //@ prow(),nC1 SAY space(len(gpici))
    //@ prow(),39 SAY nPor-nPorOl pict gpici
   //? m
endif
select dopr; go top
nPom:=nDopr:=0
nC1:=20
do while !eof()  // DOPRINOSI
 if right(id,1)<>"X"
   SKIP; LOOP
 endif
 //? id,"-",naz
 //@ prow(),pcol()+1 SAY iznos pict "99.99%"
 if empty(idkbenef) // doprinos udara na neto
   //@ prow(),pcol()+1 SAY nBO pict gpici
   //nC1:=pcol()+1
   nPom:=max(dlimit,round(iznos/100*nBO,gZaok))
   nBruto+=nPom
   nPorDopr+=nPom
 else
   nPom0:=ASCAN(aNeta,{|x| x[1]==idkbenef})
   if nPom0<>0
     nPom2:=parobr->k3/100*aNeta[nPom0,2]
   else
     nPom2:=0
   endif
   if round(nPom2,gZaok)<>0
     //@ prow(),pcol()+1 SAY nPom2 pict gpici
     //nC1:=pcol()+1
     nPom:=max(dlimit,round(iznos/100*nPom2,gZaok))
     nBruto+=nPom
     nPorDopr+=nPom
   endif
 endif

 skip
enddo // doprinosi
//? m
//? "UKUPNO POREZ+DOPRINOSI"
//@ prow(),39 SAY nPorDopr pict gpici
//? m
//? "BRUTO IZNOS"
//@ prow(),60 SAY nBruto pict gpici
//? m
return (nil)


***********************************************
// Provjerava ima li u formuli tipa
// primanja cTP parametara obracuna ("PAROBR")
***********************************************
FUNCTION TPImaPO(cTP)
  LOCAL lVrati:=.f., nObl:=SELECT()
  SELECT TIPPR; PushWA()
  SEEK cTP
  IF ID==cTP .and. "PAROBR" $ UPPER(TIPPR->formula); lVrati:=.t.; ENDIF
  PopWA(); SELECT (nObl)
RETURN lVrati


PROCEDURE PromSif()
 cSifr:="1"            // 1-radnici, 2-firme
 Box(,4,70)
  @ m_x+2, m_y+2 SAY "Iz kojeg sifrarnika je sifra koju zelite promijeniti?"
  @ m_x+3, m_y+2 SAY "(1-radnici,2-firme).................................." GET cSifr VALID cSifr$"12"
  READ
 BoxC()
 IF LASTKEY()==K_ESC; CLOSERET; ENDIF

 DO CASE

   CASE cSifr=="1"
      #ifdef CPOR
       cIdS:=cIdN:=SPACE(13)
      #else
       cIdS:=cIdN:=SPACE(6)
      #endif
      Box(,4,60)
       @ m_x+0, m_y+2 SAY "PROMJENA SIFRE RADNIKA"
       @ m_x+2, m_y+2 SAY "Stara sifra:" GET cIdS
       @ m_x+3, m_y+2 SAY "Nova sifra :" GET cIdN
       READ
      BoxC()
      IF LASTKEY()==K_ESC .or. Pitanje(,"Jeste li sigurni da zelite promijeniti ovu sifru? (D/N)","N")=="N"
        CLOSERET
      ENDIF

      O_RADN
      SEEK cIdN
      IF FOUND()
        IF Pitanje(,"Nova sifra vec postoji u sifrarniku! Zelite li nastaviti?","N")=="N"
          CLOSERET
        ENDIF
      ENDIF
      SEEK cIdS
      IF FOUND()
        Scatter(); _id:=cIdN; Gather()
      ENDIF

      O_RADKR
      SET ORDER TO TAG "2"
      SEEK cIdS
      DO WHILE !EOF() .and. cIdS==IDRADN
        SKIP 1; nRec:=RECNO(); SKIP -1
        Scatter(); _idradn:=cIdN; Gather()
        GO (nRec)
      ENDDO

      O_LD
      GO TOP
      DO WHILE !EOF()
        IF cIdS==IDRADN
          Scatter(); _idradn:=cIdN; Gather()
        ENDIF
        SKIP 1
      ENDDO

       O_RJES; GO TOP
       DO WHILE !EOF()
         IF cIdS==IDRADN
           Scatter(); _idradn:=cIdN; Gather()
         ENDIF
         SKIP 1
       ENDDO

       O_LDNO; GO TOP
       DO WHILE !EOF()
         IF cIdS==IDRADN
           Scatter(); _idradn:=cIdN; Gather()
         ENDIF
         SKIP 1
       ENDDO

   CASE cSifr=="2"
      cIdS:=cIdN:=SPACE(10)
      Box(,4,60)
       @ m_x+0, m_y+2 SAY "PROMJENA SIFRE FIRME"
       @ m_x+2, m_y+2 SAY "Stara sifra:" GET cIdS
       @ m_x+3, m_y+2 SAY "Nova sifra :" GET cIdN
       READ
      BoxC()
      IF LASTKEY()==K_ESC .or. Pitanje(,"Jeste li sigurni da zelite promijeniti ovu sifru? (D/N)","N")=="N"
        CLOSERET
      ENDIF

      O_KRED
      SEEK cIdN
      IF FOUND()
        IF Pitanje(,"Nova sifra vec postoji u sifrarniku! Zelite li nastaviti?","N")=="N"
          CLOSERET
        ENDIF
      ENDIF
      SEEK cIdS
      IF FOUND()
        Scatter(); _id:=cIdN; Gather()
      ENDIF

      O_RADKR
      SET ORDER TO TAG "3"
      SEEK cIdS
      DO WHILE !EOF() .and. cIdS==IDKRED
        SKIP 1; nRec:=RECNO(); SKIP -1
        Scatter(); _idkred:=cIdN; Gather()
        GO (nRec)
      ENDDO

       O_LD
       GO TOP
       DO WHILE !EOF()
         IF cIdS==IDKRED
           Scatter(); _idkred:=cIdN; Gather()
         ENDIF
         SKIP 1
       ENDDO

       O_LDNO; GO TOP
       DO WHILE !EOF()
         IF cIdS==IDKRED
           Scatter(); _idkred:=cIdN; Gather()
         ENDIF
         SKIP 1
       ENDDO

 ENDCASE

CLOSERET



function ZakljuciObr()
  LOCAL cMjesec:=gMjesec, cGodina:=gGodina
  MsgBeep("Zakljucenje obracuna se vrsi kada je sigurno da je rad na#"+;
          "obracunu zavrsen.#"+;
          "Kada zakljucite obracun program vam nece dozvoliti#"+;
          "nikakve dodatne promjene, vec cete imati na raspolaganju#"+;
          "samo izvjestajne opcije.")

    Box(,4,70)
    @ m_x+0, m_y+2 SAY "ZAKLJUCENJE OBRACUNA ZA:"
    @ m_x+2, m_y+2 SAY "Mjesec:" GET cMjesec PICT "99"
    @ m_x+3, m_y+2 SAY "Godina:" GET cGodina PICT "9999"
    READ
    BoxC()
    IF LASTKEY()!=K_ESC .and. Pitanje(,"Da li ste sigurni da zelite zakljuciti ovaj obracun? (D/N)","N")=="D"
      O_PAROBR
      SEEK STR(cMjesec,2)
      IF FOUND() .and. SUBSTR(naz,4,2)==RIGHT(STR(cGodina),2)
        Scatter()
         _naz:=RIGHT("00"+LTRIM(STR(cMjesec,2)),2)+"."+RIGHT(STR(cGodina),2)+"*****"
        Gather()
      ELSE
        MsgBeep("Ne postoje parametri obracuna za trazeni mjesec i godinu!")
      ENDIF
    ENDIF
CLOSERET




function DefinisiObr()
  LOCAL cMjesec:=gMjesec, cGodina:=gGodina, lNovi:=.f., lBrisi:=.f.
  MsgBeep("Definisanje parametara obracuna se vrsi kada se zeli zapoceti#"+;
          "sa radom na obracunu.#"+;
          "Za ispravnost obracuna neophodno je tacno definisati ove#"+;
          "parametre.#"+;
          "Ukoliko postoje par.obr.za isti mjesec prosle godine, oni ce#"+;
          "biti zamijenjeni novim parametrima.")

    Box(,4,70)
    @ m_x+0, m_y+2 SAY "DEFINISANJE OBRACUNA ZA:"
    @ m_x+2, m_y+2 SAY "Mjesec:" GET cMjesec PICT "99"
    @ m_x+3, m_y+2 SAY "Godina:" GET cGodina PICT "9999"
    READ
    BoxC()
    IF LASTKEY()!=K_ESC .and. Pitanje(,"Da li sigurno zelite definisati parametre obracuna za ovaj mjesec? (D/N)","D")=="D"
      O_PAROBR
      SEEK STR(cMjesec,2)
      nRecBris:=RECNO()
      IF FOUND()
        IF SUBSTR(naz,4,2)==RIGHT(STR(cGodina),2)
          IF RIGHT(naz,1)=="*"
            MsgBeep("Obracun za trazeni mjesec i godinu je vec zakljucen!")
            CLOSERET
          ENDIF
        ELSE
          lBrisi:=.t.; lNovi:=.t.
          IF cMjesec==1
            SEEK "12"
          ELSE
            SKIP -1
          ENDIF
        ENDIF
      ELSE
        lNovi:=.t.
        IF cMjesec==1
          SEEK "12"
        ELSE
          SKIP -1
        ENDIF
      ENDIF
      Scatter()
      _id:=cMjesec
      _naz:=RIGHT("00"+LTRIM(STR(cMjesec,2)),2)+"."+RIGHT(STR(cGodina),2)
      Box(,5,70)
      @ m_x+1, m_y+2 SAY "ID(mjesec):" GET _id PICT "99"
      @ m_x+2, m_y+2 SAY "Opis      :" GET _naz
      @ m_x+3, m_y+2 SAY "Bruto Osn.:" GET _k3
      @ m_x+4, m_y+2 SAY "Prosj.LD  :" GET _Prosld
      @ m_x+5, m_y+2 SAY "Koef4     :" GET _k4
      READ
      BoxC()
      IF LASTKEY()!=K_ESC
        _id:=STR(_id,2)
        IF lNovi
          APPEND BLANK
        ENDIF
        Gather()
        IF lBrisi
          GO (nRecBris)
          DELETE
        ENDIF
      ENDIF
    ENDIF
CLOSERET



function UbaciPrefix(cU,cP)

  cU := PADR(UPPER( cU ),250)

  cU := STRTRAN( cU , "I0"      , cP+"I0"      )
  cU := STRTRAN( cU , "I1"      , cP+"I1"      )
  cU := STRTRAN( cU , "I2"      , cP+"I2"      )
  cU := STRTRAN( cU , "I3"      , cP+"I3"      )
  cU := STRTRAN( cU , "I4"      , cP+"I4"      )

  cU := STRTRAN( cU , "S0"      , cP+"S0"      )
  cU := STRTRAN( cU , "S1"      , cP+"S1"      )
  cU := STRTRAN( cU , "S2"      , cP+"S2"      )
  cU := STRTRAN( cU , "S3"      , cP+"S3"      )
  cU := STRTRAN( cU , "S4"      , cP+"S4"      )

  cU := STRTRAN( cU , "USATI"   , cP+"USATI"   )
  cU := STRTRAN( cU , "UNETO"   , cP+"UNETO"   )
  cU := STRTRAN( cU , "UODBICI" , cP+"UODBICI" )
  cU := STRTRAN( cU , "UIZNOS"  , cP+"UIZNOS"  )

return TRIM(cU)

// ------------------------------------------
// * cOznaka - oznaka obracuna
// * cTipPr  - "01, "02" , ...
// *           "NE" - neto
// * izlaz = iznos cTipPr (primanja ili neta)
// ------------------------------------------
function PrimLD(cOznaka,cTipPr)
local nRez:=0, nArr:=SELECT()
private cTipa:=""
private cpom:=""

select (F_LD)
if !used()
  O_LD
endif

PushWA()

SET ORDER TO TAG "1"
//  CREATE_INDEX("1","str(godina)+idrj+str(mjesec)+obr+idradn",KUMPATH+"LD")

seek str(_godina,4)+_idrj+str(_mjesec,2)+cOznaka+_idradn

if cTippr=="NE"
    nRez:=UNETO
else
    cTipa:="I"+cTipPr
    nRez :=&cTipa
endif

PopWa()

SELECT (nArr)
return nRez




