#include "e.ch"
#include "std2.ch"
#include "inkey.ch"
#include "dbedit.ch"

#define  RADNIK  radn->(padr(  trim(naz)+" ("+trim(imerod)+") "+ime,35))


local izbor:=1

#ifdef CPOR
private opc[5]
Opc[1]:="1. novo rjesenje                                      "
Opc[2]:="2. pregled/ispravka rjesenja"
Opc[3]:="3. lista rjesenja za jedno preduzece"
Opc[4]:="4. rjesenje o prestanku prava"
Opc[5]:="5. lista rjesenja za radnike kojima je prestalo pravo"
#else
private opc[4]
Opc[1]:="1. novi kredit                        "
Opc[2]:="2. pregled/ispravka kredita"
Opc[3]:="3. lista kredita za jednog kreditora"
Opc[4]:="4. brisanje kredita"
#endif


do while .t.

 h[1]:=""
 h[2]:=""

Izbor:=menu("kred",opc,Izbor,.f.)

   do case
     case Izbor==0
       EXIT
     case izbor == 1
        cSecur:=SecurR(KLevel,"NKRED")
        cSecur2:=SecurR(KLevel,"SGLEDAJ")
        if ImaSlovo("X",cSecur)  .or. ImaSlovo("D",cSecur2)
          MsgBeep("Opcija nedostupna !")
        else
          NKred()
        endif
     case izbor == 2
        cSecur:=SecurR(KLevel,"EDKRED")
        cSecur2:=SecurR(KLevel,"SGLEDAJ")
        if ImaSlovo("X",cSecur)  .or. ImaSlovo("D",cSecur2)
           MsgBeep("Opcija nedostupna !")
        else
         EdKred()
        endif
     case izbor == 3
        cSecur:=SecurR(KLevel,"IZVJKRED")
        if ImaSlovo("X",cSecur)
           MsgBeep("Opcija nedostupna !")
        else
         LKred()
        endif
     #ifdef CPOR
     case izbor == 4
        RjesPrest()
     case izbor == 5
        ListPrekRjes()
     #else
       case izbor == 4
        BrisiKred()
     #endif
   endcase

enddo

return NIL

************************
function NKred()
*
*
************************
local i,cIdRadn:=space(_LR_)

private nMjesec:=gmjesec
private nGodina:=gGodina

private cIdKred:=space(_LK_)


private nIznKred:=nRata:=nRata2:=0


#IFDEF CPOR
private cOsnov:=space(10)
#ELSE
private cOsnov:=space(20)
#ENDIF

do while .t.

#ifdef CPOR
 O_PAROBR
 O_RJES
#endif
O_RJ
O_KRED
O_STRSPR
O_OPS
O_RADN
O_RADKR


#ifdef CPOR
set order to tag "4"
go bottom
// format polja na osnovu 1999000001
//                        GGGGBBBBBB  G(4) B(6)
nZadBroj:=val(right(NaOsnovu,6))

Box(,20,77)


  dDatum:=date()
  dDatrodj:=ctod("")
  dDatPodn:=ctod("")
  dDatZapos:=ctod("")
  dDatPPRa:=ctod("")
  dDatKPra:=ctod("")
  nRata:=150
  nBroj:=12
  nGodina:=year(dDatum)
  nZadBroj++
  nProspl:=200
  cDokazi:=""
  dDat1:=ctod("")
  cVarijanta:="1"
  cImeDjete:=space(15)
  cPol:=" "
  cPrezDjete:=""
  nPIMjesec:=1
  nMaxRata:=0

  do while .t.
  @ m_x+1,m_y+2 SAY "Radnik   :" GET cIdRadn  valid {|| P_Radn(@cIdRadn),setpos(m_x+1,m_y+26),qqout(trim(radn->naz)+" ("+trim(radn->imerod)+") "+radn->ime),.t.}
  @ m_x+2,m_y+2 SAY "Preduzece:" GET cIdKred pict "@!" valid P_Kred(@cIdKred,2,26)

  @ m_x+3,m_y+2 SAY "Datum rodjenja djeteta   :" GET dDatrodj valid !empty(dDatRodj)
  @ m_x+4,m_y+2 SAY "Datum podnosenja zahtjeva:" GET dDatpodn valid dDatPodn>=dDAtRodj
  @ m_x+5,m_y+2 SAY "Datum pocetka prava      :" GET dDatPPra ;
     when {|| dDatPPra:=iif(empty(dDatPPra),dDatRodj,dDatPPra),.t.} ;
     valid dDatPPra>=dDAtRodj
* @ m_x+6,m_y+2 SAY "Datum zavrsetka prava    :" GET dDatKPra ;
*    when {|| dDatKPra:=iif(empty(dDatKPra),DatAdd(dDatPPra,0,1),dDatKPra),.t.} ;
*    valid dDatKPra>=dDAtPPra
  @ m_x+6,m_y+2 SAY "Datum zavrsetka prava    :" GET dDatKPra ;
     when {|| dDatKPra:=iif(empty(dDatKPra),AddMonth(dDatPPra,12),dDatKPra),.t.} ;
     valid dDatKPra>=dDAtPPra
  @ m_x+7,m_y+2 SAY "Datum rjesenja" GET dDatum valid dDatum>=dDatPodn
  @ m_x+7,col()+2 SAY "Broj rjesenja:" GET nzadbroj pict "999999"
  @ m_x+7,col()+2 SAY "/" GET nGodina pict "9999"
  @ m_x+8,m_y+2 SAY "Posljednji izradjeni mjesec prije odlaska na odsustvo (99-nebitno)" GET nPIMjesec PICT "99" VALID nPIMjesec>0 .and. nPIMjesec<13 .or. nPIMjesec==99 WHEN {|| nPIMjesec:=IF(MONTH(dDatRodj)==1,12,MONTH(dDatRodj)-1), .t.}
  read;ESC_BCR

  IF nPIMjesec<>99
    nArrea:=SELECT(); SELECT PAROBR; HSEEK STR(nPIMjesec,2)
    IF FOUND()
      nMaxRata:=PAROBR->PROSLD
      Box(,3,75)
      @ m_x+0,m_y+2 SAY "MAKSIMALNI IZNOS NAKNADE:"
      @ m_x+2,m_y+2 SAY "Prosjecna plata na nivou kantona za "+LTRIM(STR(nPIMjesec,2))+". mjesec iznosila je: "+LTRIM(STR(nMaxRata))
      @ m_x+4,m_y+55 SAY "<Enter> za nastavak"
      INKEY(30)
      BoxC()
    ELSE
      MsgBeep("Maks.iznos naknade = 0 jer ne postoje parametri obracuna za ovaj mjesec!")
    ENDIF
    SELECT (nArrea)
  ENDIF

  cOsnov:=str(nGodina,4)+padl(alltrim(str(nzadBroj)),6,"0")
  nMjesec:=month(dDatPPra)  // !! datum podnosenja se uzima za naknadu!!!!
  nGodina:=year (dDatPPra)

  @ m_x+ 9,m_y+2 SAY "Datum zaposlenja:" GET dDatZapos valid !empty(dDatZapos)
  @ m_x+10,m_y+2 SAY  "Prosjecna plata :" GET nProsPl pict "99"+gPicI
  @ m_x+10,col()+2 SAY  "Mjesecna naknada:" GET nRata pict "99"+gPicI ;
     when {|| nRata:=ROUND(IF(nProsPl*0.8>nMaxRata.and.nMaxRata>=gMinRata,nMaxRata,;
                     IF(nProsPl*0.8<gMinRata,gMinRata,nProsPl*0.8)),1),.t.}

  if empty(cPrezDjete)
   cPrezdjete:=radn->naz
  endif
  @ m_x+12,m_y+2 SAY "DIJETE,Prezime:" GET cPrezDjete pict "@!"
  @ m_x+12,col()+2 SAY "Ime:" GET cImeDjete pict "@!"
  @ m_x+12,col()+2 SAY "Pol (M/Z)" GET cPol pict "@!" valid cpol$"MZ"
  // ako nije pun mjesec sam program razlomi na 13-naest
  read;ESC_BCR


  @ m_x+13,m_y+2 SAY  "Varijanta 1/2:" GET cVarijanta pict "@!" valid cvarijanta $"12"
  read
  if empty(cDokazi)
     cDokazi:=memoread(PRIVPATH+"dokaz"+cvarijanta+".txt")
  endif

  UsTipke()
  setcolor(Invert)
  @ m_x+20,m_y+2 SAY "<c-W> za kraj unosa dokaza"
  cDokazi:=MemoEdit(cDokazi,m_x+14,m_y+1,m_x+19,m_y+77)
  BosTipke()
  setcolor(Normal)

   cDN:="N"
   @ m_x+20,m_y+50 SAY "Ispravno ? " get cDN pict "@!" valid cDN $"DN"
   read
   if cDN=="D"; exit; endif
   if lastkey()==K_ESC; Boxc(); closeret; endif
  enddo

  cDokazi:=OdsjPlk(cDokazi)
  cDokazi:=Memotran(cDokazi,chr(13),chr(13))
  select rjes; seek cosnov+cidradn
  select radkr
  set order to tag "1"
  seek  str(ngodina,4)+str(nmjesec,2)+cidradn
  fUpozori:=.f.
  if found()
     fUpozori:=.t.
  endif
  if !fupozori
   set order to tag "2"
   seek  cidradn
   if found()  .and. abs(year(rjes->datum)-nGodina)<2
      // ako se naknada pojavljuje u intervalu manjem od dvije godine
      fUpozori:=.t.
   endif
  endif
  if fupozori
      MsgBeep("Vec postoji definisana naknada:##Rjesenje br:"+NaOsnovu+;
              " od "+dtoc(rjes->datum)+"##-Preduzece:"+IdKred)
      if pitanje(,"Nastaviti sa unosom  ?","N")=="N"
         BoxC()
         closeret
      endif

      //!! ako je naknada vec isplacivana NE MOZE se ispravljati !
      seek cidradn+cidkred+cosnov
      do while !eof() .and. idradn=cidradn .and. cidkred=idkred .and. cosnov=naosnovu
         if placeno<>0 .and. pmjesec<>0 .and. pgodina<>0
            msgbeep("Radniku su po ovom osnovu vec vrsene isplate !")
            BoxC()
            closeret
         endif
         skip
      enddo

  endif
  nMjeseci:=0
  ndana:=0
  Datrazmak(dDatKPra,dDatPPra, @nmjeseci, @ndana)
  //nIznKred:=nRata* ( nmjeseci + ndana/DanaUmjesecu(dDatKPra) )
  nIznKred:=nRata*nmjeseci + round(nRata*ndana/30,1)

BoxC()
#else
Box(,10,70)

  @ m_x+1,m_y+2 SAY "Mjesec:" GET nMjesec pict "99"
  @ m_x+1,col()+2 SAY "Godina:" GET nGodina pict "9999"
  @ m_x+2,m_y+2 SAY "Radnik  :" GET cIdRadn  valid {|| P_Radn(@cIdRadn),setpos(m_x+2,m_y+20),qqout(trim(radn->naz)+" ("+trim(radn->imerod)+") "+radn->ime),.t.}
  @ m_x+3,m_y+2 SAY "Kreditor:" GET cIdKred pict "@!" valid P_Kred(@cIdKred,3,21)
  @ m_x+4,m_y+2 SAY "Kredit po osnovu:" GET cOsnov pict "@!"

  @ m_x+5,m_y+2 SAY "Ukupan iznos kredita:" GET nIznKred pict "99"+gPicI
  IF IzFMKIni("LD","ZaNoviKreditUnositiBrojRata","N",KUMPATH)=="D"
    lBrojRata:=.t.
    @ m_x+7,m_y+2 SAY "Broj rata   :" GET nRata2 pict "9999" valid nRata2>0
  ELSE
    lBrojRata:=.f.
    @ m_x+7,m_y+2 SAY "Rata kredita:" GET nRata pict gpici valid nRata>0
  ENDIF
  read;ESC_BCR

BoxC()
IF lBrojRata
  nRata := ROUND(nIznKred/nRata2,2)
  IF nRata*nRata2-nIznKred<0
    nRata += 0.01
  ENDIF
ENDIF
#endif

select radkr; set order to 2
//"2","idradn+idkred+naosnovu+str(godina)+str(mjesec)"

seek cidradn+cIdkred+cosnov
private nRec:=0
if found()
  if Pitanje(,"Stavke vec postoje. Zamijeniti novim podacima ?","D")=="N"
    MsgBeep("Rate nisu formirane! Unesite novu osnovu kredita za zadanog kreditora!")
    closeret
  else
    #ifdef CPOR
     select rjes
     seek cosnov+cidradn
     if found(); delete; endif
    #endif

    select radkr
    do while !eof() .and. cidradn==idradn .and. cidkred==idkred .and. cosnov==naosnovu
      skip; nRec:=recno(); skip -1
      delete
      go nRec
    enddo
  endif
endif



private nOstalo:=nIznKred
nTekMj:=nMjesec
nTekGodina:=nGodina

altd()
i:=0
nTekMj:=nMjesec-1
do while .t.

  if nTeKMj+1>12
    nTekMj:=1
    ++nTekGodina
  else
   nTekMj++
  endif

#ifdef CPOR
  if i=0 // prva naknada
    if day(dDatPPra)>1
//      nIRata:=round(nRata*(DanaUmjesecu(dDatPPra)-day(dDatPPra))/DanaUMjesecu(dDatPPRa),1)
//      NISAM URACUNAO +1 !!!! ernad
        nIRata:=round(nRata*(DanaUmjesecu(dDatPPra)-day(dDatPPra)+1)/DanaUMjesecu(dDatPPRa),1)
    else
      nIRata:=nRata
    endif
  else
    nIRata:=nRata
  endif
#else
  nIRata:=nRata
#endif

  altd()
  if nIRata>0 .and. (nOstalo-nIRata<0)  // rata je pozitivna
    nIRata:=nOstalo
  endif
  if nIRata<0 .and. (nOstalo-nIRata>0)  // rata je negativna
    nIRata:=nOstalo
  endif

  if round(nIRata,2)<>0
   append blank
   replace idradn with cidradn, mjesec with nTekMj, Godina with nTekGodina,;
          idkred with cidkred, iznos with nIRata, naosnovu with cOsnov
#ifdef CPOR
   select rjes
   seek cosnov+cidradn
   if !found(); append blank; endif
    replace naosnovu with cosnov, idradn with cidradn,;
            datum with dDatum, datrodj with dDatRodj, ;
            datpodn with dDatPodn, datZapos with dDatZapos,;
            DatPPra with dDatPPra, DatKPra with dDatKPra,;
            Dokazi with cDokazi, varijanta with cVarijanta,;
            prezdjete with cprezdjete, imedjete with cimedjete, pol with cpol
   select radkr
#endif
   ++i
  endif

  nOstalo:=nOstalo-nIRata
  if round(nOstalo,2)==0
    exit
  endif

enddo

#ifdef CPOR
private cDn:="N"
Box(,5,60)
set confirm off
  @ m_x+1,m_y+2 SAY "Za radnika "+cidradn+" definisane naknade na "+str(i,3)+" dijelova"
  @ m_x+3,m_y+2 SAY "Prikazati pregled naknada:" GET cDN pict "@!"
  read
BoxC()
set confirm on


StRjes(cidradn,cidkred,cosnov)

#else
private cDn:="N"
Box(,5,60)
set confirm off
  @ m_x+1,m_y+2 SAY "Za radnika "+cidradn+" kredit je formiran na "+str(i,3)+" rata"
  @ m_x+3,m_y+2 SAY "Prikazati pregled kamata:" GET cDN pict "@!"
  read
BoxC()
set confirm on
#endif

close all

if cDn=="D"
  EdKred(cidradn,cidkred,cosnov)
endif

enddo

closeret

*******************
function EdKred
*
*******************
parameters cidradn,cidkred,cnaosnovu

if pcount()==0
  cIdRadn:=space(_LR_)
  cIdKRed:=space(_LK_)
#ifdef CPOR
  cNaOsnovu:=space(10)
#else
  cNaOsnovu:=space(20)
#endif
endif

O_RJ
O_OPS
O_RADN
O_KRED


#ifdef CPOR
  O_RJES
#endif
O_RADKR; set order to 2

Box(,19,77)

ImeKol:={}
AADD(ImeKol,{ "Mjesec",          {|| mjesec}                          })
AADD(ImeKol,{ "Godina",          {|| godina}                          })
AADD(ImeKol,{ "Iznos",           {|| iznos}                           })
AADD(ImeKol,{ "Otplaceno",       {|| placeno}                         })
#ifdef CPOR
 AADD(ImeKol,{ "PMjesec",          {|| Pmjesec}                          })
 AADD(ImeKol,{ "PGodina",          {|| Pgodina}                          })
#else
 AADD(ImeKol,{ "NaOsnovu",       {|| naosnovu}                         })
#endif
Kol:={}
for i:=1 to len(ImeKol); AADD(Kol,i); next

set cursor on

#ifdef CPOR
@ m_x+1,m_y+2 SAY "PRIMANJA - pregled, ispravka"
@ m_x+2,m_y+2 SAY "Radnik:   " GET cIdRadn  valid {|| P_Radn(@cIdRadn),setpos(m_x+2,m_y+27),qqout(trim(radn->naz)+" ("+trim(radn->imerod)+") "+radn->ime),P_Krediti(cIdRadn,@cIdKred,@cNaOsnovu),.t.}
@ m_x+3,m_y+2 SAY "Preduzece:" GET cIdKred  valid P_Kred(@cIdKred,3,26) pict "@!"
read
nZadBroj:=val(right(cNaOsnovu,6))
nGodina:=val(left(cNaOsnovu,4))

@ m_x+4,m_y+2 SAY "Broj rjesenja:" GET nzadbroj pict "999999"
@ m_x+4,col()+2 SAY " godina " GET nGodina pict "9999"
#else
@ m_x+1,m_y+2 SAY "KREDIT - pregled, ispravka"
@ m_x+2,m_y+2 SAY "Radnik:   " GET cIdRadn  valid {|| P_Radn(@cIdRadn),setpos(m_x+2,m_y+20),qqout(trim(radn->naz)+" ("+trim(radn->imerod)+") "+radn->ime),P_Krediti(cIdRadn,@cIdKred,@cNaOsnovu),.t.}
@ m_x+3,m_y+2 SAY "Kreditor: " GET cIdKred  valid P_Kred(@cIdKred,3,21) pict "@!"
@ m_x+4,m_y+2 SAY "Na osnovu:" GET cNaOsnovu pict "@!"
#endif

if pcount()==0
 read; ESC_BCR
else
 GetList:={}
endif

#ifdef CPOR
  cNaOsnovu:=str(nGodina,4)+padl(alltrim(str(nzadBroj)),6,"0")
  select rjes; hseek cnaosnovu+cidradn
  if !empty(PrekBroj)
    MsgBeep("Izdato je rjesenje o prekidu prava broj:"+PrekBroj+;
           "#Datum prekida prava:"+dtoc(PrekDatPoc)+" !!")
  endif
  select radkr
  @ m_x+20,m_y+1 SAY "<F10> ispravka opisnih podatakaÍÍ<Enter> ispravka rateÍÍ<c-P> stampa rjesenja"
#else
  cNaOsnovu := PADR( cNaOsnovu , LEN(RADKR->naosnovu) )
#endif


BrowseKey(m_x+6,m_y+1,m_x+19,m_y+77,ImeKol,{|Ch| EddKred(Ch)},"idradn+idkred+naosnovu=cidradn+cidkred+cnaosnovu",cidradn+cidkred+cnaosnovu,2,,)

BoxC()

closeret

*****************
*****************
function EddKred(Ch)
local cDn:="N",nRet:=DE_CONT,nRec:=RECNO()
do case
  case Ch==K_ENTER
    #ifdef CPOR
     if sigmaSif("SIGMAXXX")
    #endif
       scatter()
       Box(,6,70)
         @ m_x+1,m_y+2 SAY "Rucna prepravka rate !"
         @ m_x+3,m_y+2 SAY "Iznos  " GET _iznos pict gpici
         @ m_x+4,m_y+2 SAY "Placeno" GET _placeno pict gpici

    #ifdef CPOR
         @ m_x+6,m_y+2 SAY "Izvrsena isplata: Godina" GET _pgodina pict "9999"
         @ m_x+6,col()+2 SAY "Mjesec" GET _pmjesec pict "99"

    #else
          // ernad 13.02.2001
          cNaOsnovu2:=cNaOsnovu
          @ m_x+6,m_y+2 SAY "Na osnovu" GET cNaOsnovu2
          read

          if cNaOsnovu2<>naosnovu .and. Pitanje(,"Zelite li promijeniti osnov kredita ? (D/N)","N")=="D"
           SEEK cidradn+cidkred+cnaosnovu
           DO WHILE !EOF() .and. idradn+idkred+naosnovu==cidradn+cidkred+cnaosnovu
             SKIP 1; nRecK:=RECNO(); SKIP -1
             Scatter("w"); wNaOsnovu:=cNaOsnovu2; Gather("w")
             GO (nRecK)
           ENDDO
           _naosnovu:=cNaOsnovu:=cNaOsnovu2
          endif

    #endif

         read
       BoxC()
       GO (nRec)
       gather()
       nRet:=DE_REFRESH
    #ifdef CPOR
     endif
    #endif
  case Ch==K_CTRL_N
    #ifdef CPOR
     if sigmaSif("SIGMAXXX")
       scatter()
       Box(,5,50)
         ++_mjesec
         @ m_x+1,m_y+2 SAY "Rucno dodavanje rate !"
         @ m_x+2,m_y+2 SAY "Mjesec  " GET _mjesec pict "99"
         @ m_x+3,m_y+2 SAY "Godina  " GET _godina pict "9999"
         @ m_x+4,m_y+2 SAY "Iznos  " GET _iznos pict gpici
         @ m_x+5,m_y+2 SAY "Placeno" GET _placeno pict gpici
         read
       BoxC()
       if lastkey()<>K_ESC
         flock()
         append ncnl
         gather()
       endif
     endif
    #endif
    nRet:=DE_REFRESH
  case Ch==K_CTRL_T
    #ifdef CPOR
    if sigmaSif("SIGMAXXX")
       if !(pgodina=0 .and. pmjesec=0)
           MsgBeep("Ova rata je vec obracunata radniku !!")
       elseif  Pitanje(,"Zelite li izbrisati ratu ??","N")=="D"
          delete
       endif
    endif
    #endif
    nRet:=DE_REFRESH
  case Ch==K_CTRL_P
     PushWa()
         StRjes(radkr->idradn,radkr->idkred,radkr->naosnovu)
     PopWA()
     nRet:=DE_REFRESH
  case Ch==K_F10
     PushWa()
      SELECT RJES
      Box(,13,77)
      Scatter()
      cIdKred2:=cIdKred
      @ m_x+2,  m_y+2 SAY "Preduzece:"                 GET cIdKred2   pict "@!" valid P_Kred(@cIdKred2,2,26)
      @ m_x+3,  m_y+2 SAY "Datum rodjenja djeteta   :" GET _Datrodj   valid !empty(_DatRodj)
      @ m_x+4,  m_y+2 SAY "DIJETE,Prezime:"            GET _PrezDjete pict "@!"
      @ m_x+4,col()+2 SAY "Ime:"                       GET _ImeDjete  pict "@!"
      @ m_x+4,col()+2 SAY "Pol (M/Z)"                  GET _Pol       pict "@!" valid _pol$"MZ"
      @ m_x+5,  m_y+2 SAY "Varijanta 1/2:"             GET _Varijanta pict "@!" valid _varijanta $"12"
      READ
      IF LASTKEY()!=K_ESC
        UsTipke()
        setcolor(Invert)
        @ m_x+13,m_y+2 SAY "<c-W> za kraj unosa dokaza"
        _Dokazi:=STRTRAN(_dokazi,CHR(13),CHR(141)+CHR(10))
        _Dokazi:=STRTRAN(_dokazi,CHR(141)+CHR(10),CHR(13)+CHR(10))
        _Dokazi:=MemoEdit(_Dokazi,m_x+7,m_y+1,m_x+12,m_y+77,,,81)
        BosTipke()
        setcolor(Normal)
        _Dokazi:=OdsjPlk(_Dokazi)
        _Dokazi:=Memotran(_Dokazi,chr(13),chr(13))
        Gather()
        SELECT RADKR
        SEEK cidradn+cidkred+cnaosnovu
        DO WHILE idradn+idkred+naosnovu==cidradn+cidkred+cnaosnovu
          SKIP 1; nRecK:=RECNO(); SKIP -1
          Scatter(); _idkred:=cIdKred2; Gather()
          GO (nRecK)
        ENDDO
        cIdKred:=cIdKred2
      ENDIF
      BoxC()
      @ m_x+3,m_y+2 SAY "Preduzece: "+cIdKred
      P_Kred(@cIdKred,3,26)
     PopWA()
     nRet:=DE_REFRESH
endcase
return nRet

#ifdef CPOR
function RjesPrest()

local i,cIdRadn:=space(_LR_)

private nMjesec:=gmjesec
private nGodina:=gGodina
private cIdKred:=space(_LK_)
private cOsnov:=space(10)


O_KRED
O_RADN
O_OPS
O_RJES
O_RADKR
Box(,20,77)

cRazlPrek:=""
dPrekDatRj:=date()
dPrekDatPoc:=ctod("")

select rjes; set order to tag "PREKBROJ"; go bottom
nZadBrojP := IF( !EMPTY(PREKBROJ) , val(right(PREKBROJ,6)) , 0 )
++nZadBrojP
  nGodinaP:=year(date())

cNaosnovu:=space(10)
do while .t.
  @ m_x+1,m_y+2 SAY "Radnik:   " GET cIdRadn  valid {|| P_Radn(@cIdRadn),setpos(m_x+1,m_y+27),qqout(trim(radn->naz)+" ("+trim(radn->imerod)+") "+radn->ime),P_Krediti(cIdRadn,@cIdKred,@cNaOsnovu),.t.}
  @ m_x+2,m_y+2 SAY "Preduzece:" GET cIdKred  valid P_Kred(@cIdKred,2,26) pict "@!"
  read; ESC_BCR
  nZadBroj:=val(right(cNaOsnovu,6))
  nGodina:=val(left(cNaOsnovu,4))

  @ m_x+4,m_y+2 SAY "Broj rjesenja:" GET nzadbroj pict "999999"
  @ m_x+4,col()+2 SAY " godina " GET nGodina pict "9999"
  read; ESC_BCR
  cOsnov:=str(nGodina,4)+padl(alltrim(str(nzadBroj)),6,"0")

  @ m_x+6,m_y+2 SAY "***** PREKID : ************"
  @ m_x+8,m_y+2 SAY "Broj rjesenja:" GET nzadbrojP pict "999999"
  @ m_x+8,col()+2 SAY " godina " GET nGodinaP pict "9999"
  @ m_x+10,M_Y+2 say "Datum rjesenja o prekidu        :" GET dPrekDatRj valid !empty(dPrekDatRj)
  @ m_x+11,M_Y+2 say "Datum prestanka prava na naknadu:" GET dPrekDatPoc
  read

  read
  cPrekBroj:=str(nGodinap,4)+padl(alltrim(str(nzadBrojp)),6,"0")

  UsTipke()
  setcolor(Invert)
  @ m_x+20,m_y+2 SAY "<c-W> za kraj unosa razloga prekida"
  cRazlPrek:=MemoEdit(cRazlPrek,m_x+13,m_y+1,m_x+19,m_y+77)
  BosTipke()
  setcolor(Normal)
  cDN:="N"
  @ m_x+20,m_y+50 SAY "Ispravno ? " get cDN pict "@!" valid cDN $"DN"
  read; ESC_BCR
  if cdn=="D"; exit; endif
enddo

BoxC()

select rjes; set order to tag "NAOSNOVU"
seek cosnov+cidradn
replace PrekBroj with cPrekBroj, PrekDatRj with dPrekDatRj, PrekDatPoc with dPrekDatPoc,;
        RazlPrek with cRazlprek
// markiramo da je rjesenje prekinuto !!!!



closeret
#endif


*******************
function SumKredita()
*
*******************
local fused:=.t.
PushWa()
select (F_RADKR)
if !used()
 fused:=.f.
 O_RADKR
endif
seek str(_godina,4)+str(_mjesec,2)+_idradn
nIznos:=0
do while !eof() .and. _godina==godina .and. _mjesec=mjesec .and. idradn=_idradn
  niznos+=iznos
  replace Placeno with Iznos
  skip
enddo
if !fused
select radkr; use
endif
PopWa()
return nIznos

*******************
function Okreditu(_idradn,cidkred,cnaosnovu)
*
* izbaci matricu vezano za kredit
*
*******************
local nUkupno, nPlaceno, nNTXORd
local fused:=.t.
PushWa()
select (F_RADKR)
if !used()
 fused:=.f.
 O_RADKR
 set order to 2
 //"RADKRi2","idradn+idkred+naosnovu itd..."
else
 nNTXORD:=indexord()
 set order to 2
endif
seek _idradn+cidkred+cnaosnovu
nUkupno:=0
nPlaceno:=0
do while !eof() .and. idradn=_idradn .and. idkred=cidkred .and. naosnovu==cnaosnovu
  nUkupno+=iznos
  nPlaceno+=placeno
  skip
enddo
if !fused
select radkr; use
else
#ifdef C52
 ordsetfocus(nNTXOrd)
#else
 set order to nNTXORd
#endif
endif
PopWa()
return {nUkupno,nPlaceno}

**********************
* lista kredita
**********************
function LKred()
private fSvi  // izlistaj sva preduzeca

private nR:=nIzn:=nIznP:=0
private nUkIzn:=nUkIznP:=nUkIRR:=0
private nCol1:=10

#ifdef CPOR
 O_RJES
#endif
O_KRED
O_RADN
O_RADKR
private m:="----- "+replicate("-",_LR_)+" ------------------------------- "+replicate("-",39)

cIdKred:=space(_LK_)

#ifdef  CPOR
 cNaOsnovu:=padr(".",10)
#else
 cNaOsnovu:=padr(".",20)
#endif
cGodina:=gGodina; cMjesec:=gmjesec
private cRateDN:="D", cAktivni:="D"
Box(,12,60)
#ifdef CPOR
 @ m_x+1,m_y+2 SAY "Preduzece ('.' svi) : " GET cIdKred  valid {|| cidkred='.' .or. P_Kred(@cIdKred)} pict "@!"
 @ m_x+2,m_y+2 SAY "Na osnovu ('.' po svim osnovama):" GET cNaOsnovu pict "@!"
 @ m_x+3,m_y+2 SAY "Prikazati mjesecne naknade D/N/J/R/T:"
 @ m_x+4,m_y+2 SAY "D - prikazati sve naknade"
 @ m_x+5,m_y+2 SAY "N - prikazati samo broj naknada i ukupan iznos"
 @ m_x+6,m_y+2 SAY "J - samo jedna naknada"
 @ m_x+7,m_y+2 SAY "R - partija,br.naknada,iznos,naknada,ostalo"
 @ m_x+8,m_y+2 SAY "T - trenutno stanje" GET cRateDN pict "@!" valid cRateDN $ "DNJRT"
 @ m_x+9,m_y+2 SAY "Prikazi samo aktivne-neisplacene naknade D/N" GET cAktivni ;
                   pict "@!" valid cAktivni$"DN"
 read; ESC_BCR
 if cRateDN $ "JR"
   @ m_x+11,m_y+2 SAY "Prikazati naknadu od godina/mjesec:" GET cGodina pict "9999"
   @ m_x+11,col()+1 SAY "/" GET cMjesec pict "99"
   read; ESC_BCR
 endif
#else
 @ m_x+1,m_y+2 SAY "Kreditor ('.' svi): " GET cIdKred  valid {|| cidkred='.' .or. P_Kred(@cIdKred)} pict "@!"
 @ m_x+2,m_y+2 SAY "Na osnovu ('.' po svim osnovama):" GET cNaOsnovu pict "@!"
 @ m_x+3,m_y+2 SAY "Prikazati rate kredita D/N/J/R/T:"
 @ m_x+4,m_y+2 SAY "D - prikazati sve rate"
 @ m_x+5,m_y+2 SAY "N - prikazati samo broj rata i ukupan iznos"
 @ m_x+6,m_y+2 SAY "J - samo jedna rata"
 @ m_x+7,m_y+2 SAY "R - partija,br.rata,iznos,rata,ostalo"
 @ m_x+8,m_y+2 SAY "T - trenutno stanje" GET cRateDN pict "@!" valid cRateDN $ "DNJRT"
 @ m_x+9,m_y+2 SAY "Prikazi samo aktivne-neotplacene kredite D/N" GET cAktivni ;
                   pict "@!" valid cAktivni$"DN"
 read; ESC_BCR
 if cRateDN $ "JR"
   @ m_x+11,m_y+2 SAY "Prikazati ratu od godina/mjesec:" GET cGodina pict "9999"
   @ m_x+11,col()+1 SAY "/" GET cMjesec pict "99"
   read; ESC_BCR
 endif
#endif
BoxC()
if trim(cNaosnovu)=="."
   cNaOsnovu:=""
endif

select radkr; set order to 3
//"RADKRi3","idkred+naosnovu+idradn"

seek cidkred+cnaosnovu
nRbr:=0

if cRateDN=="R"; m+=REPL("-",16); endif

if cidkred='.'
  fSvi:=.t.
  go top
else
  IF !FOUND()
    MsgBeep("Nema podataka!")
    CLOSERET
  ENDIF
  fSvi:=.f.
endif

START PRINT CRET

ZaglKred()
do while !eof()  // vrti ako je fsvi=.t.

cIdKred:=IdKred
select kred; hseek cidkred; select radkr
if fsvi
 ?
 ? strtran(m,"-","*")
 ? gTS+":",cidkred,kred->naz
 ? strtran(m,"-","*")
endif
cOsn:=""
nCol1:=20
do while !eof() .and. idkred=cidkred .and. naosnovu=cnaosnovu
   private cOsn:=naosnovu
   cIdRadn:=idradn
   #ifdef CPOR
     SELECT RJES
     SEEK cOsn+cIdRadn
     IF !EMPTY(PrekDatPoc) .and. PrekDatPoc <= DMG(1,RADKR->mjesec,RADKR->godina)
       SELECT RADKR
       SKIP 1; LOOP
     ENDIF
     SELECT RADKR
   #endif
   *
   nIzn:=nIznP:=0
   IF cAktivni=="D"
     nTekRec := RECNO()
     RKgod := RADKR->Godina
     RKmjes := RADKR->Mjesec
     do While !Eof() .and. idkred=cidkred .and. cosn==naosnovu .and. idradn==cidradn
       #ifdef CPOR
        nKoef:=1
        IF !EMPTY(RJES->PrekDatPoc)
          dPrviuM:=DMG(1,RADKR->mjesec,RADKR->godina)
          if RJES->PrekDatPoc <= dPrviUM
            // ubilje§en je prekid rjeçenja
            skip 1; loop  // preskoŸi
          elseif month(RJES->PrekDatPoc)==month(dPRviUm) .and. year(RJES->PrekdatPoc)==year(dPrviUM)
            if month(rjes->datkpra)=month(rjes->prekdatpoc) .and.;
                year(rjes->datkpra)= year(rjes->prekdatpoc)
                // mjesec zavrsetka prava = mjesecu prekida prava
               nKoef:=(day(RJES->PrekDatPoc)-1)/day(rjes->datkpra)
            else
               nKoef:=(day(RJES->PrekDatPoc)-1)/30
            endif
          endif
        ENDIF
        nIzn += RADKR->Iznos*nKoef
        nIznP += RADKR->Placeno
        RKgod := RADKR->Godina; RKmjes := RADKR->Mjesec
       #else
        nIzn += RADKR->Iznos
        nIznP += RADKR->Placeno
        RKgod := RADKR->Godina; RKmjes := RADKR->Mjesec
       #endif
       SKIP 1
     EndDO
     IF nIzn>nIznP .or. ;
        (nIzn==nIznP .and. RKgod==cGodina .and. RKmjes>=cMjesec)
       GO nTekRec
     Else
       LOOP
     EndIF
   ENDIF

   if cNaOsnovu=="" .and. cOsn<>naosnovu
     ?
     ? m
     ? "KREDIT PO OSNOVI:",naosnovu
     ? m
   endif

   select radn; hseek cidradn
   select radkr
   if prow()>60; FF; ZaglKred(); endif
   ?
   ? str(++nRbr,4)+".",cidradn,RADNIK

   if cRateDN == "D"
     ?? " Osnov:",cOsn,replicate("_",11)
   endif
   nR:=nIzn:=nIznP:=0
   nCol1:=64
   nIRR:=0
   do while !eof() .and. idkred=cidkred .and. cosn==naosnovu .and. idradn==cidradn

     nKoef:=1
     #ifdef CPOR
      IF !EMPTY(RJES->PrekDatPoc)
        dPrviuM:=DMG(1,RADKR->mjesec,RADKR->godina)
        if RJES->PrekDatPoc <= dPrviUM
          // ubilje§en je prekid rjeçenja
          SKIP 1; LOOP
        elseif month(RJES->PrekDatPoc)==month(dPRviUm) .and. year(RJES->PrekdatPoc)==year(dPrviUM)
            if month(rjes->datkpra)=month(rjes->prekdatpoc) .and.;
                year(rjes->datkpra)= year(rjes->prekdatpoc)
                // mjesec zavrsetka prava = mjesecu prekida prava
               nKoef:=(day(RJES->PrekDatPoc)-1)/day(rjes->datkpra)
            else
               nKoef:=(day(RJES->PrekDatPoc)-1)/30
            endif
        endif
      ENDIF
     #endif

     if cRateDN<>"J" .or. (godina==cgodina .and. mjesec==cmjesec)
      ++nR; nIzn+=iznos*nKoef; nIznP+=placeno
      if iznos*nKoef==0 .and. cRateDN=="R"; --nR; endif  // mozda i za sve var. ?!
      IF cMjesec==mjesec .and. cGodina==godina; nIRR:=iznos*nKoef; ENDIF
     endif

     if cRateDN=="D"
      ? space(47),str(mjesec)+"/"+str(godina)
      nCol1:=pcol()+1
      @ prow(),pcol()+1 SAY iznos*nKoef pict gpici
     elseif cRateDN=="J"
       if godina==cgodina .and. mjesec==cmjesec
           ?? "",str(mjesec)+"/"+str(godina)
           nCol1:=pcol()+1
           @ prow(),pcol()+1 SAY iznos*nKoef pict gpici
           @ prow(),pcol()+1 SAY "___________"
       endif
     endif
     skip 1
   enddo
   if cRateDN=="N"
           @ prow(),pcol()+1 SAY nR pict "9999"
           nCol1:=pcol()+1
           @ prow(),pcol()+1 SAY nIzn pict gpici
           @ prow(),pcol()+1 SAY "___________"
   endif
   if cRateDN=="T"
           @ prow(),pcol()+1 SAY ""
           nCol1:=pcol()+1
           @ prow(),pcol()+1 SAY nIzn pict gpici
           @ prow(),pcol()+1 SAY nIznP pict gpici
           @ prow(),pcol()+1 SAY nIzn-nIznP pict gpici
   endif
   if cRateDN=="R"
           @ prow(),pcol()+1 SAY cOsn
           @ prow(),pcol()+1 SAY nR pict "9999"
           nCol1:=pcol()+1
           @ prow(),pcol()+1 SAY nIzn pict gpici
           @ prow(),pcol()+1 SAY nIRR pict gpici
           @ prow(),pcol()+1 SAY nIzn-nIznP pict gpici
   endif

   nUkIzn+=nIzn
   nUkIznP+=nIznP
   nUkIRR+=nIRR
enddo
if prow()>62; FF; ZaglKred(); endif
? m
? "UKUPNO:"
@ prow(),nCol1 SAY nUkIzn pict gpici
if cratedn=="T"
   @ prow(),pcol()+1 SAY nUkIznP  pict gpici
   @ prow(),pcol()+1 SAY nUkizn-nUkIznP  pict gpici
endif
if cratedn=="R"
   @ prow(),pcol()+1 SAY nUkIRR          pict gpici
   @ prow(),pcol()+1 SAY nUkizn-nUkIznP  pict gpici
endif
? m

if !fsvi; exit; endif
enddo  // eof()

FF
END PRINT

CLOSERET

*******************
*******************
function ZaglKred()

10CPI
IF cRateDN=="R"
 ? "LD, izvjestaj na dan:",date()
 ? "FIRMA   :",gNFirma
 ?
#ifdef CPOR
if !fsvi
 ? gTS+":",cidkred,kred->naz
endif
 ? "Ziro-r. :",kred->ziro
 ?
 ? PADC("DOJAVA RJESENJA ZA MJESEC : "+STR(cMjesec)+". GODINE: "+STR(cGodina)+".",78)
#else
if !fsvi
 ? "Kreditor:",cidkred,kred->naz
endif
 ? "Ziro-r. :",kred->ziro
 ?
 ? PADC("DOJAVA KREDITA ZA MJESEC : "+STR(cMjesec)+". GODINE: "+STR(cGodina)+".",78)
#endif
ELSE
#ifdef CPOR
 ? "LD: SPISAK RJESENJA, izvjestaj na dan:",date()
if !fsvi
 ? gTS+":",cidkred,kred->naz
endif
#else
 ? "LD: SPISAK KREDITA, izvjestaj na dan:",date()
if !fsvi
 ? "Kreditor:",cidkred,kred->naz
endif
#endif
 if !(cNaOsnovu=="")
  ?? "   na osnovu:",cnaosnovu
 endif
ENDIF
IF cRateDN=="R"
  COND
ELSE
  12CPI
ENDIF
?
? m
if cRateDN=="N"
  ? " Rbr *"+padc("Sifra ",_LR_)+"*    Radnik                         Br.Rata    Iznos      Potpis"
elseif cratedn=="T"
  ? " Rbr *"+padc("Sifra ",_LR_)+"*    Radnik                           Ukupno       Placeno       Ostalo"
elseif cratedn=="R"
  ? " Red.*"+padc(" ",_LR_)+"*                                  Partija kr.   Broj     Iznos                   Ostatak"
  ? " br. *"+padc("Sifra ",_LR_)+"*    Radnik                        (na osnovu)   rata     kredita      Rata         duga "
else
  ? " Rbr *"+padc("Sifra ",_LR_)+"*    Radnik                        Mjesec/godina/Rata"
endif
? m
return

************************************************
function P_Krediti
parameters cIdRadn,cIdkred,cNaOsnovu
*
* Ponudi postojece kredite, i napuni cidkred, cnaosnovu
********************************************************
local i
private ImeKol

PushWa()
select radkr; set order to 2
//"2","idradn+idkred+naosnovu+str(godina)+str(mjesec)",KUMPATH+"RADKR")
set scope to (cIdRadn)

seek cIdRadn

private Imekol:={}
AADD(ImeKol, {"Kreditor",      {|| IdKred   } } )
AADD(ImeKol, {"Osnov",         {|| NaOsnovu } } )
AADD(ImeKol, {"Mjesec",        {|| mjesec   } } )
AADD(ImeKol, {"Godina",        {|| godina   } } )
AADD(ImeKol, {"Iznos",         {|| Iznos    } } )


Kol:={}; for i:=1 to len(ImeKol); AADD(Kol,i); next
Box(,18,60)
ObjDbedit("PKred",18,60,{|| EdP_Krediti()},"Postojece stavke za "+cidradn,"", , , , )
Boxc()

set scope to

PopwA()
return


*************************
function EdP_Krediti()
*************************
if Ch==K_ENTER
  cIdKred:=radkr->idkred
  cNaOsnovu:=radkr->NaOsnovu
  return DE_ABORT
endif
return DE_CONT



#ifdef CPOR

*******************
function SumKred2(npomak, fSvestarije)
*
* npomak = 1 (predhodni mjesec)
* fsvestarije = pokupi sve starije od zadatog
*******************
local fused:=.t., fUsedr:=.t., nKoef:=1, dPrviUM

if fsvestarije==NIL
  fSveStarije:=.f.
endif

PushWa()
select (F_RJES)
if !used()
 fusedr:=.f.
 O_RJES
endif

select (F_RADKR)
if !used()
 fused:=.f.
 O_RADKR
endif

if _Mjesec-npomak<1
   seek str(_Godina-1,4)+str(12+_Mjesec-npomak,2)+_idradn
   cmj1:=str(_godina-1,4)+str(12+_mjesec-npomak,2)
else
   seek str(_Godina,4)+str(_mjesec-npomak,2)+_idradn
   cmj1:=str(_godina,4)+str(_mjesec-npomak,2)
endif

nIznos:=0
do while !eof() .and. str(godina,4)+str(mjesec,2)=cmj1 .and. idradn=_idradn

  select rjes; seek radkr->(naosnovu+idradn)
  if !empty(PrekDatPoc)
    dPrviuM:=DMG(1,radkr->mjesec,radkr->godina)
    if PrekDatPoc <= dPrviUM
      // ubilje§en je prekid rjeçenja
      select radkr
      skip; loop  // preskoŸi
    elseif month(PrekDatPoc)==month(dPRviUm) .and. year(PrekdatPoc)==year(dPrviUM)
            if month(rjes->datkpra)=month(rjes->prekdatpoc) .and.;
                year(rjes->datkpra)= year(rjes->prekdatpoc)
                // mjesec zavrsetka prava = mjesecu prekida prava
               nKoef:=(day(RJES->PrekDatPoc)-1)/day(rjes->datkpra)
            else
               nKoef:=(day(RJES->PrekDatPoc)-1)/30
            endif
    endif
  endif
  select radkr

  if (pgodina=0 .and. pmjesec=0) .or.;
     (pgodina=_godina .and. pmjesec=_mjesec)  //isplaceno u tekucem mjesecu/godini
     niznos+=round(iznos*nkoef,1)
     replace Placeno with round(Iznos*nkoef,1),;
             pgodina with _godina, pmjesec with _mjesec  // oznaci da je placeno
  endif
  skip
enddo
if fsvestarije
set order to tag "2"
seek _idradn
do while !eof() .and. idradn=_idradn
 if str(godina,4)+str(mjesec,2)<cmj1

  select rjes; seek radkr->(naosnovu+idradn)
  nKoef:=1
  if !empty(PrekDatPoc)
    dPrviuM:=DMG(1,radkr->mjesec,radkr->godina)
    if PrekDatPoc <= dPrviUM
      // ubilje§en je prekid rjeçenja
      select radkr
      skip; loop  // preskoŸi
    elseif month(PrekDatPoc)==month(dPRviUm) .and. year(PrekdatPoc)==year(dPrviUM)
            if month(rjes->datkpra)=month(rjes->prekdatpoc) .and.;
                year(rjes->datkpra)= year(rjes->prekdatpoc)
                // mjesec zavrsetka prava = mjesecu prekida prava
               nKoef:=(day(RJES->PrekDatPoc)-1)/day(rjes->datkpra)
            else
               nKoef:=(day(RJES->PrekDatPoc)-1)/30
            endif
    endif
  endif
  select radkr

  if (pgodina=0 .and. pmjesec=0) .or.;
     (pgodina=_godina .and. pmjesec=_mjesec)  //isplaceno u tekucem mjesecu/godini
     niznos+=round(iznos*nkoef,1)
     replace Placeno with round(iznos*nkoef,1),;
             pgodina with _godina, pmjesec with _mjesec  // oznaci da je placeno
  endif

 endif
 skip
enddo
set order to tag "1"
endif


if !fusedr
select rjes; use
endif
if !fused
select radkr; use
endif
PopWa()
return nIznos
#endif



**********************
function StRjes(cidradn,cidkred,cosnov)
*
* stampa rjesenja
**********************
local cPom
PRIVATE nMjRata:=0 //Koristi ga StRjes()!!!!


PRIVATE cPeriod:=""


select rjes
seek cosnov+cidradn
cVarijanta:=Varijanta
if empty(cVarijanta)
  cVarijanta:="1"
endif
nstr:=1

private  fRtf:=.f.

if gTxtRTf=="T"
 if pitanje(,"Stapmati u TXT formatu","D")=="D"
   fRtf:=.f.
 else
   frtf:=.t.
 endif
else
 if pitanje(,"Stampati u RTF formatu","D")=="D"
   fRtf:=.t.
 endif
endif

set century on

select radkr; seek cidradn+cidkred+cosnov
nIznos:=-9999
dDatOd:=ctod("")
dDatDo:=ctod("")
dZStampan:=ctod("")  // zadnji odstampan datum
do while idradn+idkred+naosnovu==cidradn+cidkred+cosnov
  if niznos=-9999 // prva stavka
    nIznos:=iznos
    dDatOd:=rjes->datPPra
    dDatDo:=min(DatZUmj(dDatOd),rjes->datKPra)
  else // niznos<>0
    if niznos<>iznos
         cPeriod+="- od "+dtoc(dDatOd)+".godine do "+dtoc(dDatDo)+".godine"
         dZStampan:=dDatOd
         nMjeseci:=0; nDana:=0
         DatRazmak(dDatDo,dDatOd,@nmjeseci,@nDana)
         if nmjeseci>0  // obuhvata se vise mjeseci
            cPeriod+=" u mjesecnom iznosu od po "
         else
           cPeriod+=" u iznosu od "
         endif
         if frtf
           cPeriod+="{\b "+ToRtfStr(alltrim(str(niznos,9,2))+" KM")+"}\line "+;
            ToRtfStr("   (slovima:"+slovima(niznos,"KM")+ ")")+"\line "
         else
           cPeriod+=gPB_ON+alltrim(str(niznos,9,2))+" KM"+gPB_Off+chr(13)+;
            "   (slovima:"+slovima(niznos,"KM")+ ")"+chr(13)
         endif
         dDatOd:=ddatdo+1 // prvi u narednom
         dDatDo:=DatZUmj(dDatOd)
         if dDatDo>=rjes->datkpra  // zavrsava se sa datumom kraja prava
             dDatDo:=rjes->datkpra
         endif
         nIznos:=iznos
    else
        cPom:="01."+padl(alltrim(str(mjesec,2)),2,"0")+"."+str(godina,4)
        dDatDo:=DatZUmj(ctod(cPom))
        if dDatDo>=rjes->datkpra  // zavrsava se sa datumom kraja prava
             dDatDo:=rjes->datkpra
        endif
    endif


  endif // niznos==0


  skip
enddo

if  dZStampan<dDatOd
 cPeriod+="- od "+dtoc(dDatOd)+".godine do "+dtoc(dDatDo)+".godine"
 nMjeseci:=0; nDana:=0
 DatRazmak(dDatDo,dDatOd,@nmjeseci,@nDana)
 if nmjeseci>0  // obuhvata se vise mjeseci
 cPeriod+=" u mjesecnom iznosu od po "
 else
   cPeriod+=" u iznosu od "
 endif
 if frtf
   cPeriod+="{\b "+ToRtfStr(alltrim(str(niznos,9,2))+" KM")+"}\line "+;
    ToRtfStr("   (slovima:"+slovima(niznos,"KM")+ ")")+"\line "
 else
   cPeriod+=gPB_ON+alltrim(str(niznos,9,2))+" KM"+gPB_Off+chr(13)+;
    "   (slovima:"+slovima(niznos,"KM")+ ")"+chr(13)
 endif
endif

select radkr
seek cidradn+cidkred+cosnov  // idi ponovo na prvi slog radkr


if frtf

 cOut:=PRIVPATH+"rjes.rtf"
 if (nH:=fcreate(cOut))==-1
   Beep(4)
   Msg("Fajl "+cOut+" se vec koristi !",6)
   fclose(nH)
   return
 endif
 fclose(nH)

 set printer to (cOut)
 set printer on
 set console off

 Setpxlat()
 WWInit0()
 WWFontTbl()
 WWStyleTbl()
 WWInit1()
 WWSetMarg(20,30,15,35)

 WWSetPage("A4","P")
 ?? "\f2 "  //arial
 //?? "\f2\pard \qc \li0\ri0\nowidctlpar\faauto\rin0\lin0\itap0 " // centered
 // font 2 justified poravnanje
 ?? "\footery1583 {\footer \pard\plain \qc \li0\ri0\nowidctlpar\faauto\rin0\lin0\itap0 "+;
    "\f2\fs20\lang1033\langfe1033\cgrid\langnp1033\langfenp1033 {\i - Strana: }{\field{\*\fldinst {\i PAGE }}"+;
    "{\fldrslt {\i\lang1024\langfe1024\noproof 2}}}{\i  -}{\i \par }}"

else
START PRINT CRET
endif


cFajl:=PRIVPATH+"rjes"+cVarijanta+".txt"

if frtf
 nLijevaMarg:=0
else
 nLijevaMarg:=4
endif

nLin:=BrLinFajla(cFajl)
nPocetak:=0; nPreskociRedova:=0
FOR i:=1 TO nLin
  aPom:=SljedLin(cFajl,nPocetak)
  nPocetak:=aPom[2]
  cLin:=space(nlijevaMarg)+aPom[1]
  if frtf .and. right(cLin,1)<>"#"
     cLin+=" " // dodaj razmaknicu
  endif
  IF nPreskociRedova>0
    --nPreskociRedova
    LOOP
  ENDIF
  IF i>1; ?; ENDIF
  DO WHILE .t.
    nPom:=AT("#",cLin)
    IF nPom>0
      cPom:=SUBSTR(cLin,nPom,4)
      aPom:=UzmiVRjes( SUBSTR(cPom,2,2) )
      if frtf
        setpxlat()
        ?? ToRtfStr(LEFT(cLin,nPom-1))
      else
        ?? LEFT(cLin,nPom-1)
      endif
      cLin:=SUBSTR(cLin,nPom+4)
      IF !frtf .and. !EMPTY(aPom[1])
        PrnKod_ON(aPom[1])
      ENDIF
      IF aPom[1]=="K"
         if frtf
           setpxlat()
           ?? aPom[2]
         else
           cPom:=&(aPom[2])
         endif
      ELSEif aPom[1]="C"
        if frtf
         setpxlat()
         ?? aPom[2]
        else
         ?? aPom[2]
        endif
      ELSEif aPom[1]="M"
        apom[2]:=strtran(aPom[2],chr(13),chr(13)+chr(10))
        for i:=1 to  mlcount(apom[2])
           if frtf
             setpxlat()
           endif
           if i>1
             ?? space(nlijevamarg)
           endif
           if frtf
              cpom:=trim(strtran(memoline(aPom[2],150,i),chr(13),""))
              if left(alltrim(cpom),1)=="-"
                 ?? "\line "
              endif
              ?? cpom
           else
              ?? strtran(memoline(aPom[2],92,i),chr(13),"")
           endif
           if frtf; ?? " "; endif
           ?

           if prow()>59
             if !frtf
              FF
              ?? space(nlijevamarg)+padc("---  "+str(++nstr,2)+" ---",79)
              ?
              ?
             endif
           endif
        next

      ELSEif aPom[1]="F"
          //prelazak na novu stranu
           if prow()>59
             if !frtf
               FF
               ?
               ?? padc("---  "+str(++nstr,2)+" ---",79)
               ?
               ?
             endif
           endif
      else
        cPom:=&(aPom[2])
        if frtf .and. valtype(cPom)=="C"
            setpxlat()
            ?? ToRtfStr(cPom)
        else
          ?? cPom
        endif
      ENDIF
      IF !frtf .and. !EMPTY(aPom[1])
        PrnKod_OFF(aPom[1])
      ENDIF
    ELSE
      if frtf
        setpxlat()
        ?? ToRtfStr(cLin)
      else
        ?? cLin
      endif
      EXIT
    ENDIF
  ENDDO
NEXT

if frtf
 WWEnd()

 set printer to
 set printer off
 set console on
 konvtable()
else
 FF
 END PRINT
endif
set century off

////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////
if fRtf
 cKom:="start "+PRIVPATH+"rjes.rtf"
 run &ckom
endif

return

***************************
FUNCTION UzmiVRjes(cVar)
***************************
local nPom, nGod, nMj
LOCAL cVrati:={"","''"}
DO CASE
  CASE cVar=="FF"
      cVrati := { "F" , "" }
      // nova strana
  CASE cVar=="RJ"
      select rj; hseek gRj; select radkr
      cVrati := { "" , "rj->naz" }
  CASE cVar=="JM"
      select rj; hseek gRj; select radkr
      cVrati := { "" , "radn->id" }
  CASE cVar=="IM"
      cVrati := { "" , "trim(radn->naz)+' ('+trim(radn->imerod)+') '+trim(radn->ime)" }
  CASE cVar=="D1"
      cVrati := { "" , "rjes->datum" }
  CASE cVar=="D2"
      cVrati := { "" , "rjes->datpodn" }
  CASE cVar=="D3"
      cVrati := { "" , "rjes->datrodj" }
  CASE cVar=="D4"
      cVrati := { "" , "rjes->datppra" }
  CASE cVar=="D5"
      cVrati := { "" , "rjes->datkpra" }
  CASE cVar=="D6"
      cVrati := { "" , "rjes->datzapos" }

  CASE cVar=="BR"
       cVrati := { "C" , &gBrRjes }

  CASE cVar=="PE" // period
      cVrati := { "M" , cPeriod }
  CASE cVar=="UL"
      cVrati := { "" , "trim(radn->ulica)" }
  CASE cVar=="MS" // mjesto stanovanja
      select ops; hseek radn->idopsst; select radkr
      cVrati := { "" , "trim(ops->naz)" }
  CASE cVar=="MR" // mjesto stanovanja
      select ops; hseek radn->idopsrad; select radkr
      cVrati := { "" , "trim(ops->naz)" }
  CASE cVar=="PB" // prebivaliste/boraviste
      cVrati := { "" , IF(UPPER(radn->k2)=="P","'prebivaliste'","'boraviste'") }
  CASE cVar=="PR" // preduzece
      select kred; hseek radkr->idkred; select radkr
      cVrati := { "" , "trim(kred->naz)" }
  CASE cVar=="TZ" // trajanje zaposlenja
      nPom:=rjes->(datpodn-datzapos)
      nGod:=int(nPom/365)
      nMj:= int(nPom%365 / 30)
      cPom:=alltrim(str(ngod,2))+".god i "+alltrim(str(nmj,2))+".mjes."
      // C -constanta
      if frtf
         cVrati := { "C" , ToRtfStr(cPom) }
      else
         cVrati := { "C" , cPom }
      endif

  CASE cVar=="DO" // trajanje zaposlenja
     if frtf
      cVrati := { "M" , ToRtfStr(rjes->dokazi) }
     else
      cVrati := { "M" , rjes->dokazi }
     endif

  CASE cVar=="SP"  // space
      if frtf
         cVrati := { "K", " " }
      endif
  CASE cVar=="DP"  // dos space
      if !frtf
         cVrati := { "C", space(5) }
      endif
  CASE cVar=="_T"
      if frtf
         cVrati := { "K", "\tab " }
      endif
  CASE cVar=="_P"
      if frtf
         cVrati := { "K", "\par " }
      endif
  CASE cVar=="&L"
      if frtf
         cVrati := { "K", "\ql " }
      endif
  CASE cVar=="&J"
      if frtf
         cVrati := { "K", "\qj " }
      endif
  CASE cVar=="&R"
      if frtf
         cVrati := { "K", "\qr " }
      endif
  CASE cVar=="&C"
      if frtf
         cVrati := { "K", "\qc " }
      endif
  CASE cVar=="_L"
      if frtf
         cVrati := { "K", "\line " }
      endif
  CASE cVar=="_1"
      if frtf
         cVrati := { "K", "\fs20 " }
      endif
  CASE cVar=="_2"
      if frtf
         cVrati := { "K", "\fs24 " }
      endif
  CASE cVar=="_4"
      if frtf
         cVrati := { "K", "\fs48 " }
      endif
  CASE cVar=="_8"
      if frtf
         cVrati := { "K", "\fs16 " }
      endif
  CASE cVar=="_9"
      if frtf
         cVrati := { "K", "\fs18 " }
      endif
  CASE cVar=="B1"
      if frtf
         cVrati := { "K", "{\b " }
      else
         cVrati := { "K", "gPB_ON()" }
      endif
  CASE cVar=="B0"
      if frtf
         cVrati := { "K", "}" }
      else
        cVrati := { "K", "gPB_OFF()" }
      endif
  CASE cVar=="10"
     if frtf
     else
      cVrati := { "K", "gP10CPI()" }
     endif
  CASE cVar=="12"
     if frtf
     else
      cVrati := { "K", "gP12CPI()" }
     endif
  CASE cVar=="U1"
     if frtf
     else
      cVrati := { "K", "gPU_ON()" }
     endif
  CASE cVar=="U0"
     if frtf
     else
      cVrati := { "K", "gPU_OFF()" }
     endif
  CASE cVar=="I1"
     if frtf
     else
      cVrati := { "K", "gPI_ON()" }
     endif
  CASE cVar=="I0"
     if frtf
     else
      cVrati := { "K", "gPI_OFF()" }
     endif
ENDCASE
RETURN cVrati


***********************
function Bez0(cStr)
*
* "00005" -> "5"
***********************
local nSjeci,i, n0:=0

nsjeci:=1
for i:=1 to len(cStr)
    if substr(cStr,i,1) $ " 0"
         nSjeci:=i+1
    else
         exit
    endif
next
return substr(cStr,nSjeci)



******************************
******************************
function Slovima(nIzn,cDINDEM)
local npom; cRez:=""
fI:=.f.

if nIzn<0
  nIzn:=-nIzn
  cRez:="negativno:"
endif

if (nPom:=int(nIzn/10**9))>=1
   if nPom==1
     cRez+="milijarda"
   else
     Stotice(nPom,@cRez,.f.,.t.,cDinDEM)
      if right(cRez,1) $ "eiou"
        cRez+="milijarde"
      else
        cRez+="milijardi"
     endif
   endif
   nIzn:=nIzn-nPom*10**9
   fi:=.t.
endif
if (nPom:=int(nIzn/10**6))>=1
   //if fi; cRez+="i"; endif
   if fi; cRez+=""; endif
   fi:=.t.
   if nPom==1
     cRez+="milion"
   else
     Stotice(nPom,@cRez,.f.,.f.,cDINDEM)
     cRez+="miliona"
   endif
   nIzn:=nIzn-nPom*10**6
   f6:=.t.
endif
if (nPom:=int(nIzn/10**3))>=1
   //if fi; cRez+="i"; endif
   if fi; cRez+=""; endif
   fi:=.t.
   if nPom==1
     cRez+="hiljadu"
   else
     Stotice(nPom,@cRez,.f.,.t.,cDINDEM)
     if right(cRez,1) $ "eiou"
       cRez+="hiljade"
     else
       cRez+="hiljada"
     endif
   endif
   nIzn:=nIzn-nPom*10**3
endif
//if fi .and. nIzn>=1; cRez+="i"; endif
if fi .and. nIzn>=1; cRez+=""; endif
Stotice(nIzn,@cRez,.t.,.t.,cDINDEM)

*******************************************
*
****************************************
function Stotice(nIzn,cRez,fdecimale,fmnozina,cDINDEM)
local fDec,fSto:=.f.,i

   if (nPom:=int(nIzn/100))>=1
      aSl:={ "stotinu", "dvijestotine", "tristotine", "~etiristotine",;
             "petstotina","{eststotina","sedamstotina","osamstotina","devetstotina"}
      if gKodnaS=="8"
        for i:=1 to len(aSL)
          aSL[i]:=KSTo852(aSl[i])
        next
      endif
      cRez+=aSl[nPom]
      nIzn:=nIzn-nPom*100
      fSto:=.t.
   endif

   fDec:=.f.
   do while .t.
     if fdec
        cRez+=alltrim(str(nizn,2))
     else
      if int(nIzn)>10 .and. int(nIzn)<20
        aSl:={ "jedanaest", "dvanaest", "trinaest", "~etrnaest",;
               "petnaest","{esnaest","sedamnaest","osamnaest","devetnaest"}

        if gKodnaS=="8"
          for i:=1 to len(aSL)
            aSL[i]:=KSTo852(aSl[i])
          next
        endif
        cRez+=aSl[int(nIzn)-10]
        nIzn:=nIzn-int(nIzn)
      endif
      if (nPom:=int(nIzn/10))>=1
        aSl:={ "deset", "dvadeset", "trideset", "~etrdeset",;
               "pedeset","{ezdeset","sedamdeset","osamdeset","devedeset"}
        if gKodnaS=="8"
          for i:=1 to len(aSL)
            aSL[i]:=KSTo852(aSl[i])
          next
        endif
        cRez+=aSl[nPom]
        nIzn:=nIzn-nPom*10
      endif
      if (nPom:=int(nIzn))>=1
         aSl:={ "jedan", "dva", "tri", "~etiri",;
                "pet","{est","sedam","osam","devet"}
         if gKodnaS=="8"
          for i:=1 to len(aSL)
            aSL[i]:=KSTo852(aSl[i])
          next
         endif
        if fmnozina
             aSl[1]:="jedna"
             aSl[2]:="dvije"
        endif
        cRez+=aSl[nPom]
        nIzn:=nIzn-nPom
      endif
      if !fDecimale; exit; endif

     endif // fdec
     if fdec; cRez+="/100 "+cDINDEM; exit; endif
     fDec:=.t.
     fMnozina:=.f.
     nizn:=round(nIzn*100,0)
     if nizn>0
       if !empty(cRez)
           cRez+=" i "
       endif
     else
       if empty(cRez)
          cRez:="nula DEM"
       else
          cRez+=" "+cDINDEM
       endif
       exit
     endif
   enddo


return cRez

****************************************
function  GodMj(ngodina,nmjesec,npomak)
*
* ngodina=1999
* nmjesec=4
* npomak = -6
* vratiti -> {1998,10}
*
*****************************************
local nPGodina, nPMjesec
local nVgodina:=0

if npomak<0  // vrati se unazad
   npomak:=abs(npomak)
   nVGodina:=int(npomak/12)
   nPomak:=npomak%12
   if nMjesec-npomak<1
      nPGodina:=nGodina-1
      nPMjesec:=12+nMjesec-npomak
   else
      nPGodina:=nGodina
      nPMjesec:=nmjesec-npomak
   endif
   nPGodina:=nPGodina-nVGodina
else
   nVGodina:=int(npomak/12)
   nPomak:=npomak%12
   if nMjesec+npomak>12
      nPGodina:=nGodina+1
      nPMjesec:=nMjesec+npomak-12
   else
      nPGodina:=nGodina
      nPMjesec:=nmjesec+npomak
   endif
   nPGodina:=nPGodina+nVGodina
endif
return {nPGodina,nPMjesec}

***************************************
function DatADD(dDat,nmjeseci,nGodina)
local aRez, cPom:=""

aRez:=Godmj(year(dDat),month(dDat),nmjeseci+12*ngodina)
altd()
cPom:=str(aRez[1],4)
cpom+=padl( alltrim(str(aRez[2]  ,2)),2,"0")
cPom+=padl( alltrim(str(day(dDat),2)), 2,"0")
return stod(cPom)


***************************************
function DatRazmak(dDatDo,dDatOd,nmjeseci,ndana)
* datumski razmak izrazen u: mjeseci, dana
*
* poziv: DatRazmak("15.07.99","05.06.98", @nmjeseci, @ndana)
************************************************************
local i
local aRez, cPom:=""
local fZadnjiDan:=.f.
nmjeseci:=0
ndana:=0
dNextMj:=dDatOd
i:=0
altd()


if day(dDatOd)=lastdayom(dDatOd)
  altd()
  fZadnjiDan:=.t.
endif

if month(dDatDo)==month(dDatOd) .and. day(dDatDo)=day(dDatOd)
  //isti mjesec, isti dan
  nMjeseci:=(year(dDatDo)-year(dDatOd))*12
  nDana:=0
  return
endif

do while .t.  // predvidjen je razmak do 36 mjeseci

   if month(dnextmj)=month(dDatDO) .and. year(dnextmj)=year(dDatDo)
       // uletili smo u isti mjesec
       ndana:=day(dDatDo)-day(dnextmj)
       if ndana<0  // moramo se vratiti mjesec unazad
          altd()
          //dNextMj:=DatAdd(dNextMj,-1,0)
          dNextMj:=Addmonth(dNextMj,-1)
          --nmjeseci
          //ndana:=nDana+DanaUmjesecu(dNextMj)-day(dnextmj)
          if nmjeseci=0  //samo dva krnjava mjeseca
             altd()
             //ndana:=30+nDana+30-min(day(dnextmj),30)
             nDana:=(day(eom(dDatOd))-day(dDatOd)+1)+day(dDatDo)-1
          else
             //??????????????????
             //??????? ndana:=nDana+30-min(day(dnextmj),30)
             ndana:=(day(eom(dNextMj))-day(dDatOd)+1)+day(dDatDo)-1
          endif

       elseif ndana>=0
           // ???
           //++ndana   NEISPRAVNO - centar za soc rad
       endif
       exit
   endif

   altd()
   //dNextMj:=DatAdd(dNextMj,1,0)
   dNextMj:=AddMonth(dNextMj,1)
   if fzadnjiDan  // zadnji dan u mjesecu
       dNextMj:=eom(dNextMj)
   endif

   nmjeseci++
   ++i
   if i>200
      msgbeep("jel to neko lud ovdje ?")
      exit
   endif
enddo

********************************
function DanaUmjesecu(dDat)
*
* koliko dana u mjesecu
********************************
local nDatZM
*local ndana, dPoc

*dPoc:=dDat
*ndana:=day(dDat)
*do while .t.
*   dDat++
*   if month(dPoc)=month(dDat)
*      nDana:=day(dDat)
*   else
*      exit  // uletio sam usljedeci mjesec
*   endif
*
*enddo

nDatZM:=eom(dDat)
*return nDana
return day(nDatZM)

****************************
function DatZUmj(dDat)
* datum zadnjeg u mjesecu
****************************
local ndana, dPoc

dPoc:=dDat
ndana:=day(dDat)
do while .t.
   dDat++
   if month(dPoc)=month(dDat)
      nDana:=day(dDat)
   else
      exit  // uletio sam usljedeci mjesec
   endif

enddo
return dDat-1  // vrati se unazad



**************************************
function DMG(nday,nmjesec,ngodina)
*
*  dmg(1,5,1999) -> datumska varijabla 01.05.99
*
**************************************
local cpom
cpom:=padl(alltrim(str(nday,2)),2,"0")
cpom+="."+padl(alltrim(str(nmjesec,2)),2,"0")
cpom+="."+padl(alltrim(str(ngodina,4)),4,"0")
return ctod(cpom)

****************************************
* odsjeca Prazne Linije na Kraju stringa
*****************************************
function OdsjPLK(cTxt)
local i
for i:=len(cTxt) to 1 step -1
  if !(substr(cTxt,i,1) $ Chr(13)+Chr(10)+" ")
       exit
  endif
next
return left(cTxt,i)


//               1         2
// varijanta (roditelj,staratelj) se upisuje samo u RJES.DBF (polje VARIJANTA)
//
//  P      B
// preb./borav. se upisuje iskljucivo u polje K2 baze RADN.DBF
//

#ifdef CPOR

PROCEDURE ListPrekRjes()
 PRIVATE dOd:=CTOD("01.07.99"), dDo:=DATE()
  gnLMarg:=0; gTabela:=0; gOstr:="D"; cOdvLin:="D"; cPKU:="N"; cVarSort:="1"
  O_RADN
  O_RJES

  O_PARAMS
  Private cSection:="4",cHistory:=" ",aHistory:={}
  RPar("VS",@cVarSort)

  Box(,6,75)
   @ m_x+2, m_y+2 SAY "Rjesenja za radnike kojima je prestalo pravo u periodu:"
   @ m_x+3, m_y+2 SAY "od:" GET dOd
   @ m_x+4, m_y+2 SAY "do:" GET dDo
   @ m_x+5, m_y+2 SAY "Sortirati listu po ( 1-br.rjesenja , 2-prezime+ime )" GET cVarSort PICT "9" VALID cVarSort$"12"
   READ; ESC_BCR
  BoxC()

  WPar("VS",cVarSort)
  SELECT PARAMS; USE

  SELECT RJES
  IF cVarSort=="2"    // prezime+ime
    Box(,2,30)
     nSlog:=0; nUkupno:=RECCOUNT2()
     cSort1:="SortPrez(IDRADN)"
     cFilt := "prekdatpoc>="+cm2str(dOd)+" .and. prekdatpoc<="+cm2str(dDo)
     INDEX ON &cSort1 TO "TMPRJES" FOR &cFilt EVAL(TekRec2()) EVERY 1
    BoxC()
    GO TOP
  ENDIF

  aKol:={}; nKol:=0
  AADD( aKol , { "R.BR." , {|| STR(nRBr,4)+"."}, .f., "C", 5, 0, 1, ++nKol} )
  AADD( aKol , { "BR.RJESENJA"  , {|| naosnovu}, .f., "C", 13, 0, 1, ++nKol} )
  AADD( aKol , { "SIFRA/JMB"    , {|| idradn  }, .f., "C", 13, 0, 1, ++nKol} )
  AADD( aKol , { "PREZIME I IME", {|| cNazRadn}, .f., "C", 30, 0, 1, ++nKol} )
  AADD( aKol , { "DAT.POCETKA"  , {|| DTOC(datppra) }, .f., "C", 11, 0, 1, ++nKol} )
  AADD( aKol , { "PRAVA"        , {|| "#"     }, .f., "C", 11, 0, 2,   nKol} )
  AADD( aKol , { "DAT.PREKIDA"  , {|| DTOC(prekdatpoc)}, .f., "C", 11, 0, 1, ++nKol} )
  AADD( aKol , { "PRAVA"        , {|| "#"     }, .f., "C", 11, 0, 2,   nKol} )

  START PRINT CRET

  ?? space(gnLMarg); ?? "LD: Izvjestaj na dan",date()
  ? space(gnLMarg); IspisFirme("")
  ?

  PRIVATE nRBr:=0, cNazRadn:=""
  GO TOP
  StampaTabele(aKol,,,gTabela,,;
                     ,"Spisak rjesenja radnika kojima je prestalo pravo za period od "+DTOC(dOd)+" do "+DTOC(dDo),;
                     IF(cVarSort=="2",{|| FFor6a()},{|| FFor6()}),IF(gOstr=="D",,-1),,cOdvLin=="D",,,)

  // FF
  END PRINT
CLOSERET

FUNCTION FFor6()
 LOCAL lVrati:=.f.
 IF prekdatpoc>=dOd .and. prekdatpoc<=dDo
   cNazRadn:=Ocitaj(F_RADN,idradn,"TRIM(naz)+' '+TRIM(ime)")
   ++nRBr
   lVrati:=.t.
 ENDIF
RETURN lVrati

FUNCTION FFor6a()
  cNazRadn:=Ocitaj(F_RADN,idradn,"TRIM(naz)+' '+TRIM(ime)")
  ++nRBr
RETURN .t.

#endif


PROC BrisiKred()
 cIdRadn   := SPACE(_LR_)
 cIdKRed   := SPACE(_LK_)
 cNaOsnovu := SPACE(20)
 cBrisi:="N"

 O_RJ
 O_OPS
 O_RADN
 O_KRED

 O_RADKR; SET ORDER TO 2

 Box("#BRISANJE NEOTPLACENIH RATA KREDITA",9,77)
  @ m_x+2,m_y+2 SAY "Radnik:   " GET cIdRadn  valid {|| P_Radn(@cIdRadn),setpos(m_x+2,m_y+20),qqout(trim(radn->naz)+" ("+trim(radn->imerod)+") "+radn->ime),P_Krediti(cIdRadn,@cIdKred,@cNaOsnovu),.t.}
  @ m_x+3,m_y+2 SAY "Kreditor: " GET cIdKred  valid P_Kred(@cIdKred,3,21) pict "@!"
  @ m_x+4,m_y+2 SAY "Na osnovu:" GET cNaOsnovu pict "@!"
  @ m_x+6, m_y+2, m_x+8, m_y+76 BOX "         " COLOR "GR+/R"
  @ m_x+7,m_y+8 SAY "Jeste li 100% sigurni da zelite izbrisati ovaj kredit ? (D/N)" COLOR "GR+/R"
  @ row(), col()+1 GET cBrisi VALID cBrisi$"DN" PICT "@!" COLOR "N/W"
  READ; ESC_BCR
 BoxC()

 IF cBrisi=="D"
   SELECT RADKR
   SET ORDER TO TAG "2" // idradn+idkred+naosnovu+str(godina)+str(mjesec)
   SEEK cIdRadn+cIdKred+cNaOsnovu
   nStavki:=0
   DO WHILE !EOF() .and. idradn+idkred+naosnovu==cIdRadn+cIdKred+cNaOsnovu
     SKIP 1; nRec:=RECNO(); SKIP -1
     IF placeno=0
       ++nStavki
       DELETE
     ENDIF
     GO (nRec)
   ENDDO
   IF nStavki>0
     MsgBeep("Sve neotplacene rate (ukupno "+ALLTRIM(STR(nStavki))+") kredita izbrisane!")
   ELSE
     MsgBeep("Nista nije izbrisano. Za izabrani kredit ne postoje neotplacene rate!")
   ENDIF
 ENDIF

CLOSERET

