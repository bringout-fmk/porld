#include "c:\re\ld\e.ch"
#include "std2.ch"
#include "inkey.ch"
#include "dbedit.ch"

local cIdRadn,cMjesec,cIdRj

private fnovi

O_PAROBR
O_RADN
O_VPOSLA
O_STRSPR
O_KBENEF
O_OPS
O_RJ
O_RADKR
O_KRED

#ifdef CPOR
  O_LDNO
#endif

cIdRadn:=space(_LR_)

do while .t.

O_LD

//cIdRadn:=space(6)   vrati ovo !!
cIdRj    := gRj
cMjesec  := gMjesec
cGodina  := gGodina
cObracun := gObracun

Box(,21,77)

@ m_x+1,m_y+2 SAY "Radna jedinica: "; QQOUTC(cIdRJ,"GR+/N")

if gunmjesec=="D"
 @ m_x+1,col()+2 SAY "Mjesec: "  GET cMjesec pict "99"
else
 @ m_x+1,col()+2 SAY "Mjesec: "; QQOUTC(str(cMjesec,2),"GR+/N")
endif

if lViseObr
 if gunmjesec=="D"
  @ m_x+1,col()+2 SAY "Obracun: " GET cObracun WHEN HelpObr(.f.,cObracun) VALID ValObr(.f.,cObracun)
 else
  @ m_x+1,col()+2 SAY "Obracun: "; QQOUTC(cObracun,"GR+/N")
 endif
endif

@ m_x+1,col()+2 SAY "Godina: "; QQOUTC(str(cGodina,4),"GR+/N")
@ m_x+2,m_y+2 SAY "Radnik" GET cidradn valid {|| P_Radn(@cIdRadn),setpos(m_x+2,m_y+26),qqout(trim(radn->naz)+" ("+trim(radn->imerod)+") "+radn->ime),.t.}
read; clvbox(); ESC_BCR

#ifdef CPOR
SELECT PAROBR
SEEK STR(cMjesec)
IF FOUND()
  IF RIGHT(naz,1)=="*"
    IF SUBSTR(naz,4,2)==RIGHT(STR(cGodina),2)
      MsgBeep("Obracun za ovaj mjesec je zakljucen. Ispravke nisu dozvoljene!")
    ELSE
      MsgBeep("Obracun za ovaj mjesec nece biti uradjen dok ne definisete#"+;
              "parametre obracuna (opcija D. u osnovnom meniju) !")
    ENDIF
    BoxC(); CLOSERET
  ENDIF
ELSE
  MsgBeep("Obracun za ovaj mjesec nece biti uradjen dok ne definisete#"+;
          "parametre obracuna (opcija D. u osnovnom meniju) !")
  BoxC(); CLOSERET
ENDIF
#endif

if lViseObr
  O_TIPPRN
else
  O_TIPPR
endif

SELECT LD
seek str(cGodina,4)+cIdRj+str(cMjesec,2)+IF(lViseObr,cObracun,"")+cIdRadn
if found()
  fNovi:=.f.
  scatter()
else
#ifdef CPOR
  Beep(1)
  if Pitanje(,"Za radnika ne postoje podaci. Nastaviti ?","N")=="N"
     Boxc()
     closeret
  endif
#endif
  fNovi:=.t.
  append blank
  scatter()
  _Godina:=cGodina
  _idrj:=cIdRj; _idradn:=cIdRadn; _mjesec:=cMjesec
  IF lViseObr; _obr := cObracun; ENDIF
endif

if fnovi
  #ifndef CPOR
    _brbod    := radn->brbod
    _kminrad  := radn->kminrad
    _idvposla := radn->idvposla
  #endif
  _idstrspr:=radn->idstrspr
endif

ParObr(cMjesec,IF(lViseObr,cObracun,),cIdRj)  // podesi parametre obra~una za ovaj mjesec

#ifndef CPOR
if gtipObr=="1"
 @ m_x+3,m_y+2   SAY IF(gBodK=="1","Broj bodova","Koeficijent") GET  _brbod pict "99999.99" valid FillBrBod()
else
 @ m_x+3,m_y+2   SAY "Plan.osnov ld" GET  _brbod pict "99999.99" valid FillBrBod()
endif
@ m_x+3,col()+2 SAY IF(gBodK=="1","Vrijednost boda","Vr.koeficijenta"); @ row(),col()+1 SAY parobr->vrbod  pict "99999.99999"
if gMinR=="B"
 @ m_x+3,col()+2 SAY "Minuli rad (bod)" GET _kminrad pict "9999.99" valid FillKMinRad()
else
 @ m_x+3,col()+2 SAY "Koef.minulog rada" GET _kminrad pict "99.99%" valid FillKMinRad()
endif
@ m_x+4,m_y+2 SAY "Vrsta posla koji radnik obavlja" GET _IdVPosla valid (empty(_idvposla) .or. P_VPosla(@_IdVPosla,4,43)) .and. FillVPosla()
#endif

if gSihtarica=="D"

O_PARAMS
altd()

private cZadnjiRadnik:=cIdRadn
Private cSection:="S",cHistory:=" ",aHistory:={}
RPar("zr",@cZadnjiRAdnik)

O_RADSIHT
select radsiht
seek str(_godina,4)+str(cmjesec,2)+cZadnjiRadnik+cIdRj

if found() // ovaj je radnik fakat radjen

 seek str(_godina,4)+str(cmjesec,2)+cidradn+cIdRj
 if !found()
  // ako je ovaj radnik vec radjen ne pitaj nista za preuzimanje
  if pitanje(,'Zelite li preuzeti sihtaricu od radnika '+cZadnjiRadnik+' D/N','D')=='D'
     select radsiht
     seek str(_godina,4)+str(cmjesec,2)+cZadnjiRadnik+cIdRj
     private nTSrec:=0
     do while !eof() .and. (str(godina,4)+str(mjesec,2)+idradn+IdRj)==;
                           (str(_godina,4)+str(cmjesec,2)+cZadnjiRadnik+cIdRj)
         skip; nTSrec:=recno(); skip -1
         Scatter('w')
         wIdRadn:=cidradn  // sve je isto osim sifre radnika
         append blank
         Gather('w')
         go nTSrec
    enddo

  endif // pitanje
 endif
endif


Unossiht()


select params
Private cSection:="S",cHistory:=" ",aHistory:={}

altd()

select radsiht
seek str(_godina,4)+str(cmjesec,2)+cIdRadn+cIdRj
if found()  // nesto je bilo u sihtarici
 select params
 cZadnjiRadnik:=cIdRadn
 WPar("zr",cZadnjiRAdnik)
endif
select params; use

select RadSiht; use

endif // gSihtarica=="D"

altd()

public cFormula:=""

#include "c:\re\ld\e3.ch"
if cLDPolja==40
  Unos2()
endif

_USati:=0
_UNeto:=0;_UOdbici:=0
UkRadnik()  // filuje _USati,_UNeto,_UOdbici
_UIznos:=_UNeto+_UOdbici

@ m_x+19,m_y+2 SAY "Ukupno sati:"; @ row(),col()+1 SAY _USati  pict gpics
@ m_x+20,m_y+2 SAY "NETO iznos:"; @ row(),col()+1 SAY _UNeto   pict gpici
@ m_x+20,col()+2 SAY "Odbici:";   @ row(),col()+1 SAY _UOdbici pict gpici
@ m_x+20,col()+2 SAY "UKUPNO ZA ISPLATU:"; @ row(),col()+1 SAY _UIznos  pict gpici
@ m_x+22,m_y+10 SAY "Pritisni <ENTER> za snimanje, <ESC> napustanje"
inkey(0)

BoxC()



if lastkey()<>K_ESC
  if _UIznos<0
    Beep(2)
    Msg("Radnik ne moze imati platu u negativnom iznosu!?")
  endif

  ///////////////////////////////////////
  nPom:=0
  for i:=1 to cLDPolja
     cPom:=padl(alltrim(str(i)),2,"0")
     nPom+=abs(_i&cPom) + abs(_s&cPom)  // ako su sve nule
  next
  if nPom<>0
     gather()
#ifdef CPOR
     // provjerimo postoji li uradjeni obracun medju obrisanim
     select ldno
      seek str(cGodina,4)+cIdRj+str(cMjesec,2)+cIdRadn
      if found()    // ako postoji, ukinucemo ga
        delete
      endif
     select ld
#endif
  else
     if fnovi
        delete
     endif
  endif
  if gListic=="D"
    IF lViseObr
      KartPl(cIdRj,cMjesec,cGodina,cIdRadn,cObracun)
    ELSE
      KartPl(cIdRj,cMjesec,cGodina,cIdRadn)
    ENDIF
  endif
else //K_ESC

  if fnovi  // ako je novi zapis  .and. ESCAPE
     delete
  endif
  closeret
endif

select ld; use  // svaki put zatvoriti tabelu ld
Beep(1)

enddo // do while .t.





*****************************************************************
#ifdef CPOR
PROCEDURE PriBris()

O_EditPrBr()

ImeKol:={ ;
          { "R.broj" , {|| rbr     }, "rbr"    } ,;
          { "Radnik" , {|| idradn  }, "Idradn" } ,;
          { "RJ"     , {|| idrj    }, "idrj"   } ,;
          { "Godina" , {|| godina  }, "godina" } ,;
          { "Mjesec" , {|| mjesec  }, "mjesec" } ,;
          { "Razlog" , {|| razlog  }, "razlog" } ;
        }

Kol:={}; for i:=1 to LEN(ImeKol); AADD(Kol,i); next

Box(,20,77)

@ m_x+19,m_y+2 SAY " <c-N>  Nove Stavke      ³ <ENT> Ispravi stavku   ³ <c-T> Brisi Stavku"
@ m_x+20,m_y+2 SAY " <c-F9> Brisi pripremu   ³ <c-P> Stampa pripreme  ³ <a-A> Azuriranje  "

ObjDbedit("PrBr",20,77,{|| EdPrBr()},"","Priprema za brisanje obracuna...", , , , ,2)

BoxC()

CLOSERET

*****************************************************************


*****************************************************************

function EdPrBr()
local nTr2

SELECT PRIPNO

if (Ch==K_CTRL_T .or. Ch==K_ENTER) .and. (recno()<=0 .or. !EMPTY(brisano))
  return DE_CONT
endif

do case

  case Ch==K_CTRL_T
     if Pitanje(,"Zelite izbrisati ovu stavku ?","D")=="D"
       delete
       return DE_REFRESH
     endif
     return DE_CONT

  case Ch==K_ENTER
    Box("ist",20,75,.f.,"Priprema za brisanje obracuna - ispravka stavke")
    Scatter()
    nRbr:=VAL(_Rbr)
    if EditPrBr(.f.)==0
      BoxC()
      return DE_CONT
    else
      Gather()
      BoxC()
      return DE_REFRESH
    endif

  case Ch==K_CTRL_N  // nove stavke
     nPrvi:=0
     GO BOTTOM
     Box("nspb",20,77,.f.,"Priprema za brisanje obracuna - nova stavka")
     do while .t.
        Scatter()
        _IdRadn:=SPACE(_LR_)
        _razlog:=SPACE(20)
        nRbr:=VAL(_Rbr)+1
        @ m_x+1,m_y+1 CLEAR to m_x+19,m_y+76
        if EditPrBr(.t.)==0
          exit
        endif
        inkey(10)
        select PRIPNO
        APPEND BLANK
        Gather()
     enddo

     BoxC()
     return DE_REFRESH

   case Ch=K_CTRL_F9
     if Pitanje(,"Zelite li izbrisati pripremu !!????","N")=="D"
          zap
     endif
     return DE_REFRESH

   case Ch==K_CTRL_P
     StPrBr()
     return DE_REFRESH

   case Ch==K_ALT_A
     IF Pitanje(,"Sigurno zelite azurirati pripremu za brisanje obracuna?","N")=="D"
       AzurPrBr()
       O_EditPrBr()
       return DE_REFRESH
     ENDIF

endcase

return DE_CONT

*****************************************************************


*****************************************************************

FUNCTION EditPrBr
 PARAMETERS fNovi

  IF fnovi .and. nRbr==1
    _idrj  :=gRJ
    _mjesec:=gMjesec
    _godina:=gGodina
  ENDIF

  SET CURSOR ON

  @ m_x+ 2, m_y+ 2 SAY "Redni broj:" GET nRbr PICT "9999"
  @ m_x+ 4, m_y+ 2 SAY "Godina    :" GET _godina PICT "9999"
  @ m_x+ 6, m_y+ 2 SAY "Mjesec    :" GET _mjesec PICT "99"
  @ m_x+ 8, m_y+ 2 SAY "Radnik    :" GET _idradn VALID {|| P_Radn(@_IdRadn),setpos(m_x+8,m_y+27),qqout(trim(radn->naz)+" ("+trim(radn->imerod)+") "+radn->ime),.t.}
  @ m_x+10, m_y+ 2 SAY "R.jedinica:" GET _idrj VALID P_RJ(@_idrj)
  @ m_x+12, m_y+ 2 SAY "Razlog    :" GET _razlog
  READ
  ESC_RETURN 0

  _Rbr:=STR(nRbr,4)
RETURN 1



PROCEDURE O_EditPrBr()
  O_PAROBR
  O_RJ
  O_RADN
  O_LD
  O_LDNO
  O_PRIPNO
RETURN



PROCEDURE StPrBr()
  SELECT PRIPNO; nTRec:=RECNO(); GO TOP
  m:=REPL("-",4)+" "+REPL("-",4)+" "+REPL("-",2)+" "+REPL("-",13)+" "+REPL("-",40)+" "+REPL("-",2)+" "+REPL("-",20)
  START PRINT RET
  12CPI
  ? "PREGLED RADNIKA CIJI SE OBRACUN PONISTAVA"
  ? "-----------------------------------------"
  ?
  ? m
  ? "RBr. God. Mj .ID.Radnika.. ..........Ime,ime oca,prezime........... RJ ......Razlog........"
  ? m

  DO WHILE !EOF()
    SELECT RADN; SEEK PRIPNO->idradn; SELECT PRIPNO
    ?  PRIPNO->rbr, PRIPNO->godina, PRIPNO->mjesec, PRIPNO->idradn,;
       PADR(TRIM(RADN->ime)+" ("+TRIM(RADN->imerod)+") "+TRIM(RADN->naz),40),;
       PRIPNO->idrj, PRIPNO->razlog
    SKIP 1
  ENDDO

  END PRINT
  SELECT PRIPNO; GO (nTRec)
RETURN



PROCEDURE AzurPrBr()
 local nTrec, cIdRadn, cMjesec, cIdRj, fnovi

 O_RADKR
 set order to tag "PGM"
 // "PGM","idradn+str(pgodina)+str(pmjesec)",KUMPATH+"RADKR")

 SELECT PRIPNO; GO TOP

 DO WHILE !EOF()

   SELECT PAROBR
   SEEK STR(PRIPNO->mjesec,2)
   IF FOUND()
     IF SUBSTR(naz,4,2)!=RIGHT(STR(PRIPNO->godina),2)
       MsgBeep("Stavka br."+ALLTRIM(PRIPNO->rbr)+" nije azurirana: ne postoje param.obracuna!")
       SELECT PRIPNO; SKIP 1; LOOP
     ELSEIF RIGHT(naz,1)=="*"
       MsgBeep("Stavka br."+ALLTRIM(PRIPNO->rbr)+" nije azurirana: obracun je vec zakljucen!")
       SELECT PRIPNO; SKIP 1; LOOP
     ENDIF
   ELSE
     MsgBeep("Stavka br."+ALLTRIM(PRIPNO->rbr)+" nije azurirana: ne postoje param.obracuna!")
     SELECT PRIPNO; SKIP 1; LOOP
   ENDIF

   SELECT LD

   cIdRadn := PRIPNO->idradn; cIdRj   := PRIPNO->idrj
   cMjesec := PRIPNO->mjesec; cGodina := PRIPNO->godina

   SEEK STR(cGodina,4)+cIdRj+STR(cMjesec,2)+cIdRadn

   IF FOUND()
     // ubaci obracun u LDNO.DBF
     Scatter()
     SELECT LDNO
      SEEK STR(cGodina,4)+cIdRj+STR(cMjesec,2)+cIdRadn
      IF FOUND(); DELETE; ENDIF
      APPEND BLANK
      _razlog := PRIPNO->razlog
      Gather()
     // izbrisi obracun u LD.DBF
     SELECT LD
      DELETE
     // izbrisi sve oznake da je naknada isplacena u RADKR.DBF
     SELECT RADKR
      SEEK cidradn+str(cgodina,4)+str(cmjesec,2)
      do while !eof() .and. cidradn+str(cgodina,4)+str(cmjesec,2)==idradn+str(pgodina,4)+str(pmjesec,2)
        skip 1; nTrec:=recno(); skip -1
        Scatter()
         _pgodina := 0
         _pmjesec := 0
         _placeno := 0
        Gather()
        go nTrec
      enddo
     // izbrisi zapis u pripremi
     SELECT PRIPNO
      SKIP 1; nTRec:=RECNO(); SKIP -1; DELETE; GO (nTRec)
   ELSE
     Msg("Stavka br."+ALLTRIM(PRIPNO->rbr)+" ne postoji u obracunu, nemoguce azuriranje!",4)
     SELECT PRIPNO
     SKIP 1
   ENDIF

 ENDDO

CLOSERET

#endif


PROC QQOUTC(cT,cC)
 @ row(), col() SAY cT COLOR cC
RETURN


