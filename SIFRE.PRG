#include "std2.ch"
#include "e.ch"
#include "set.ch"
#include "inkey.ch"
#include "dbedit.ch"

local izb:=1
#ifdef CPOR
private opc[14], h[14]
#else
private opc[16], h[16]
O_SIFK; O_SIFV
O_BANKE
#endif


if gSihtarica=="D"
  O_TPRSIHT
  O_NORSIHT
endif

gMeniSif:=.t.
opc[1]:="1. radnici                            "
opc[2]:="2. parametri obracuna"
opc[3]:="3. tipovi primanja"

if lViseObr
 opc[4]:="4. tipovi primanja za obracun 2"
else
 opc[4]:="---------------"
endif

opc[5]:="5. radne jedinice"
opc[6]:="6. opstine"
opc[7]:="7. porezi"
opc[8]:="8. doprinosi"
#ifdef CPOR
opc[9]:= "9. --------------"
opc[10]:="A. --------------"
opc[11]:="B. --------------"
opc[12]:="C. preduzeca"
#else
opc[9]:="9. vrste posla"
opc[10]:="A. koef.benef.rst"
opc[11]:="B. strucne spreme"
opc[12]:="C. kreditori"
if gSihtarica=="D"
 opc[13]:="D. tipovi primanja u sihtarici"
 opc[14]:="E. norme radova u sihtarici   "
else
 opc[13]:="-"
 opc[14]:="-"
endif
opc[15]:="F. banke"
opc[16]:="G. sifk"


#endif

O_RADN
O_PAROBR
O_TIPPR
O_RJ
O_POR
O_DOPR
O_STRSPR
O_KBENEF
O_VPOSLA
O_OPS
O_KRED

if lViseObr
  O_TIPPR2
endif

do while .t.

  h:=ARRAY(LEN(opc)); AFILL(h,"")

  Izb:=Menu("fsif",opc,Izb,.f.)
     do case
        case Izb==0
           exit
        case Izb==1
           P_Radn()
        case Izb==2
           P_ParObr()
        case Izb==3
#ifdef CPOR
           MsgBeep("Mijenjanje ovog sifrarnika nije uobicajena opcija,#"+;
                   "ali ukoliko imate potrebu za promjenom obracuna#"+;
                   "najbolje je da prvo kontaktirate SIGMA-COM servis!")

           if sigmaSif("SIGMAXXX")
                      P_TipPr()
           endif
#else
           P_TipPr()
#endif
        case Izb==4
           if lViseObr
             P_TipPr2()
           endif

        case Izb==5
           P_RJ()
        case Izb==6
           P_Ops()
        case Izb==7
           P_Por()
           SELECT (F_POR); USE
           O_POR
        case Izb==8
           P_Dopr()
           SELECT (F_DOPR); USE
           O_DOPR
        case Izb==9
           P_VPosla()
        case Izb==10
           P_KBenef()
        case Izb==11
           P_StrSpr()
        case Izb==12
           P_Kred()
        case Izb==13
          if gSihtarica=="D"
            P_TprSiht()
          endif
        case Izb==14
          if gSihtarica=="D"
             P_NorSiht()
          endif

        case Izb==15
           P_Banke()
        case Izb==16
           P_Sifk()
     endcase
enddo

gMeniSif:=.f.
closeret

***************************
***************************
function P_Radn(cId,dx,dy)
local i
private imekol,kol

#ifndef CPOR
ImeKol:={ { padr("Id",6), {|| id}, "id", {|| .t.}, {|| vpsifra(wid)} },;
          { padr("Prezime",20),{|| naz}, "naz" } ,;
          { padr("Ime roditelja",15),{|| imerod}, "imerod" } ,;
          { padr("Ime",15),{|| ime}, "ime" } ,;
          { padr(IF(gBodK=="1","Br.bodova","Koeficij."),10),{|| brbod}, "brbod" }, ;
          { padr("MinR%",5),{|| kminrad}, "kminrad" }, ;
          { padr("StrSpr",6),{|| padc(Idstrspr,6)}, "idstrspr", {||.t.}, {|| P_StrSpr(@wIdStrSpr)} } ,;
          { padr("V.Posla",6),{|| padc(IdVPosla,6)}, "IdVPosla", {||.t.}, {|| empty(widvposla) .or. P_VPosla(@wIdVPosla)} }, ;
          { padr("Ops.Stan",8),{|| padc(IdOpsSt,8)}, "IdOpsSt", {||.t.}, {|| P_Ops(@wIdOpsSt)} }, ;
          { padr("Ops.Rada",8),{|| padc(IdOpsRad,8)}, "IdOpsRad", {||.t.}, {|| P_Ops(@wIdOpsRad)} }, ;
          { padr("Maticni Br.",13),{|| padc(matbr,13)}, "MatBr", {||.t.}, {|| .t.} }, ;
          { padr("Dat.Od",8),{|| datod}, "datod", {||.t.}, {|| .t.} }, ;
          { padr("POL",3),{|| padc(pol,3)}, "POL", {||.t.}, {|| wPol $ "MZ"} }, ;
          { padr("K1",2),{|| padc(k1,2)}, "K1", {||.t.}, {|| .t.} }, ;
          { padr("K2",2),{|| padc(k2,2)}, "K2", {||.t.}, {|| .t.} }, ;
          { padr("K3",2),{|| padc(k3,2)}, "K3", {||.t.}, {|| .t.} }, ;
          { padr("K4",2),{|| padc(k4,2)}, "K4", {||.t.}, {|| .t.} }, ;
          { padr("PorOl",6),{|| porol}, "POROL", {||.t.}, {|| .t.} }, ;
          { padr("Radno mjesto",30),{|| rmjesto}, "RMJESTO", {||.t.}, {|| .t.} },;
          { padr("Br. Knjizice ",12),{|| padc(brknjiz,12)}, "brknjiz", {||.t.}, {|| .t.} }, ;
          { padr("Br. Tekuceg rac.",20) ,{|| padc(brtekr,20)}, "brtekr", {||.t.}, {|| .t.} }, ;
          { padr("Isplata",7),{|| padc(isplata,7)}, "isplata", {||.t.}, {|| wIsplata $ "  #TR#SK#BL" .or. MsgIspl()} }, ;
          { padr("Banka",6),{|| padc(idbanka,6)}, "idbanka", {||.t.}, {|| EMPTY(WIDBANKA) .OR. P_Kred(@widbanka)} } ,;
          { padr("OSN.Bol",11),{|| osnbol}, "osnbol" } ;
         }

#else   // POR-LD
ImeKol:={;
          { padr("JMBG",13), {|| id}, "id", {|| .t.}, {|| vpsifra(wid)} },;
          { padr("Prezime",20),{|| naz}, "naz" } ,;
          { padr("Ime roditelja",15),{|| imerod}, "imerod" } ,;
          { padr("Ime",15),{|| ime}, "ime" } ,;
          { padr("P)reb/B)orav",35),{|| k2}, "k2", {||.t.}, {|| wk2:=UPPER(wk2),wk2$"PB"} } ,;
          { padr("Ulica i broj",35),{|| ulica}, "ulica" } ,;
          { padr("Ops.Stan",8),{|| padc(IdOpsSt,8)}, "IdOpsSt", {||.t.}, {|| P_Ops(@wIdOpsSt)} }, ;
          { padr("Ops.Rada",8),{|| padc(IdOpsRad,8)}, "IdOpsRad", {||.t.}, {|| P_Ops(@wIdOpsRad)} }, ;
          { padr("Rbr. PIO",10),{|| padc(RbrPIO,10)}, "RBRPIO", {||.t.}, {|| .t.} }, ;
          { padr("POL",3), {|| padc(pol,3)}, "POL", {|| wPol:=iif(empty(wPol),'Z',wPol),.t.}, {||  wPol$"MZ"} } ;
         }
//          { padr("POL",3), {|| padc(pol,3)}, "POL", {|| wPol:=iif(empty(wPol),'Z',wPol),.t.}, {|| IIF( !wPol$'MZ',msgbeep('Kucaj Z-zensko, M-musko')), wPol$"MZ"} } ;

// { padr("Id",6), {|| id}, "id", {|| .t.}, {|| vpsifra(wid)} },;
// { padr("JMBG",13),{|| padc(matbr,13)}, "MatBr", {||.t.}, {|| .t.} }, ;

#endif
if radn->(fieldpos("N1"))<>0
  AADD (ImeKol,{ padc("N1",12 ), {|| n1}, "n1" })
  AADD (ImeKol,{ padc("N2",12 ), {|| n2}, "n2" })
  AADD (ImeKol,{ padc("N3",12 ), {|| n3}, "n3" })
endif

Kol:={}
for i:=1 to len(ImeKol)
  AADD(Kol,I)
NEXT

if gMinR=="B"
    ImeKol[6]:={ padr("MinR",7),{|| transform(kminrad,"9999.99")}, "kminrad" }
endif

if radn->(fieldpos("S1"))<>0
  AADD (ImeKol,{ padc("s1",10 ), {|| s1}, "S1" })
  AADD(Kol,24)
  AADD (ImeKol,{ padc("s2",10 ), {|| s2}, "S2" })
  AADD(Kol,25)
  AADD (ImeKol,{ padc("s3",10 ), {|| s3}, "S3" })
  AADD(Kol,26)
  AADD (ImeKol,{ padc("s4",10 ), {|| s4}, "S4" })
  AADD(Kol,27)
endif


return PostojiSifra(F_RADN,1,12,72,"Lista radnika",@cId,dx,dy,{|Ch| RadBl(Ch)},,,,,{"ID"})


*******************
*******************
function RadBl(Ch)

LOCAL cMjesec:=gmjesec

if Ch==K_ALT_M
  Box(,4,60)
   @ m_x+1,m_y+2 SAY "Postavljenje koef. minulog rada:"
   @ m_x+2,m_y+2 SAY "Pazite da ovu opciju ne izvrsite vise puta za isti mjesec !"
   @ m_x+4,m_y+2 SAY "Mjesec:" GET cmjesec pict "99"
   read
  BoxC()
  if lastkey()==K_ESC;  RETURN DE_CONT; endif

   MsgO("Prolazim kroz tabelu radnika..")
   select radn
   go top
   do while !eof()
    if month(datOd)==cmjesec
     if pol=="M"
       replace kminrad with kminrad+gMRM
     elseif pol=="Z"
       replace kminrad with kminrad+gMRZ
     endif
    endif
    if kminrad>20   // ogranicenje minulog rada
      replace kminrad with 20
    endif
    skip
   enddo
  MsgC()
  go top
  return DE_REFRESH
elseif Ch==K_CTRL_T
 if ImaURadKr(RADN->id,"2")
   Beep(1)
   Msg("Stavka radnika se ne moze brisati jer se vec nalazi u obracunu!")
   return 7
 endif
elseif Ch==K_F2
 if ImaURadKr(RADN->id,"2")
   return 99
 endif
endif

RETURN DE_CONT


*****************
*****************
function MsgIspl()

Box(,3,50)
 @ m_x+1,m_y+2 SAY "Vazece sifre su: TR - tekuci racun   "
 @ m_x+2,m_y+2 SAY "                 SK - stedna knjizica"
 @ m_x+3,m_y+2 SAY "                 BL - blagajna"
 inkey(0)
BoxC()
return .f.

*********************************
*********************************
function P_ParObr(cId,dx,dy)
private imekol, kol:={}

#ifdef CPOR
ImeKol:={  { padr("ID",8), {|| id}, "id", {|| wid:=val(wid), .t. }, {|| wid:=str(wid,2), .t.}  },;
           { padr("Opis",10), {|| naz}, "naz" } ,;
           { padr("Bruto Osn",6), {|| k3} , "k3"  }  ,;
           { padr("Prosj.LD",12), {|| Prosld} , "PROSLD"  },  ;
           { padr("Koef4",6), {|| k4} , "k4"  }  ;
         }

Kol:={1,2,3,4,5}
#else
ImeKol:={  { padr("ID",8), {|| id}, "id", {|| wid:=val(wid), .t. }, {|| wid:=str(wid,2), .t.}  },;
           { padr("Opis",10), {|| naz}, "naz" } ,;
           { padr(IF(gBodK=="1","Vrijednost boda","Vr.koeficijenta"),15), {|| vrbod}, "vrbod" } ,;
           { padr("Br.Sati",5), {|| k1} , "k1"  }  ,;
           { padr("Koef2",5), {|| k2} , "k2"  }  ,;
           { padr("Bruto osn.",6), {|| k3} , "k3"  }  ,;
           { padr("Koef4",6), {|| k4} , "k4"  }  ,;
           { padr("Prosj.LD",12), {|| Prosld} , "PROSLD"  }  ;
         }
if lViseObr
  ASIZE(ImeKol,LEN(ImeKol)+1)
  AINS(ImeKol,2)
  ImeKol[2] := { padr("Obracun",10) , {|| obr} , "obr" }
endif
if IzFMKINI("LD","VrBodaPoRJ","N",KUMPATH)=="D"
  ASIZE(ImeKol,LEN(ImeKol)+1)
  AINS(ImeKol,3-IF(lViseObr,0,1))
  ImeKol[3-IF(lViseObr,0,1)] := { "RJ" , {|| IDRJ} , "IDRJ" }
endif
for i:=1 to len(ImeKol); aadd(kol,i); next
#endif
return PostojiSifra(F_PAROBR,1,10,70,"Parametri obracuna",@cId,dx,dy)



***************************************
***************************************
function P_TipPr(cId,dx,dy)

private imekol,kol

ImeKol:={ { padr("Id",2), {|| id}, "id", {|| .t.}, {|| vpsifra(wid)} },;
          { padr("Naziv",20), {||  naz}, "naz" }                       ,;
          {      "Aktivan"  , {||  padc(aktivan,7) }, "aktivan" }      ,;
          {      "Fiksan"  , {||  padc(fiksan,7) }, "fiksan" }         ,;
          { padr("U fond s.",10), {||  padc(ufs,10) }, "ufs" } ,;
          { padr("U neto", 6), {||  padc(uneto,6 ) }, "uneto" } ,;
          { padr("Formula",200), {|| formula}, "formula"  }, ;
          { padr("Opis",8), {|| opis}, "opis"  } ;
       }
Kol:={1,2,3,4,5,6,7,8}
return PostojiSifra(F_TIPPR,1,10,55,"Tipovi primanja",@cId,dx,dy)

***************************************
***************************************
function P_TipPr2(cId,dx,dy)

private imekol,kol

ImeKol:={ { padr("Id",2), {|| id}, "id", {|| .t.}, {|| vpsifra(wid)} },;
          { padr("Naziv",20), {||  naz}, "naz" }                       ,;
          {      "Aktivan"  , {||  padc(aktivan,7) }, "aktivan" }      ,;
          {      "Fiksan"  , {||  padc(fiksan,7) }, "fiksan" }         ,;
          { padr("U fond s.",10), {||  padc(ufs,10) }, "ufs" } ,;
          { padr("U neto", 6), {||  padc(uneto,6 ) }, "uneto" } ,;
          { padr("Formula",200), {|| formula}, "formula"  }, ;
          { padr("Opis",8), {|| opis}, "opis"  } ;
       }
Kol:={1,2,3,4,5,6,7,8}
return PostojiSifra(F_TIPPR2,1,10,55,"Tipovi primanja za obracun 2",@cId,dx,dy)

***************************************
***************************************
function P_RJ(cId,dx,dy)

private imekol,kol

ImeKol:={ { padr("Id",2), {|| id}, "id", {|| .t.}, {|| vpsifra(wid)} },;
          { padr("Naziv",35), {||  naz}, "naz" }                       ;
       }
Kol:={1,2}
return PostojiSifra(F_RJ,1,10,55,"Lista radnih jedinica",@cId,dx,dy)

***************************************
***************************************
function P_Ops(cId,dx,dy)
local i:=0
private imekol,kol:={}

ImeKol:={ { padr("Id",2), {|| id}, "id", {|| .t.}, {|| vpsifra(wid)} },;
          { padr("IDJ",3), {||  idj}, "idj" }                       ,;
          { padr("Kan",3), {||  idKan}, "idKan" }                       ,;
          { padr("N0",3), {||  idN0}, "IdN0" }                       ,;
          { padr("Naziv",20), {||  naz}, "naz" }                       ;
       }

Kol:={}
if OPS->(fieldpos("PNE"))<>0
  aadd( ImeKol, { padr("Bez poreza:",20), {||  pne}, "pne" } )
endif
if OPS->(fieldpos("DNE"))<>0
  aadd( ImeKol, { padr("Bez doprinosa:",20), {||  dne}, "dne" } )
endif

for i:=1 to len(ImeKol); aadd(kol,i); next
return PostojiSifra(F_OPS,1,10,65,"Lista opcina",@cId,dx,dy)

***************************************
***************************************
function P_Kred(cId,dx,dy)

private imekol,kol

#ifdef CPOR
ImeKol:={ { padr("Rbr PIO",10), {|| id}, "id", {|| .t.}, {|| vpsifra(wid)} },;
          { padr("Naziv",30), {||  naz}, "naz" }                       ,;
          { padr("Racun",16),{||  ziro}, "ziro" }                     ;
       }
Kol:={1,2,3}
return PostojiSifra(F_KRED,1,10,55,"Lista preduzeca",@cId,dx,dy, {|Ch| KrBlok(Ch)},,,,,{"id"})
#else
ImeKol:={ { padr("Id",6), {|| id}, "id", {|| .t.}, {|| vpsifra(wid)} },;
          { padr("Naziv",30), {||  naz}, "naz" }                       ,;
          { padr("Racun",20)  ,{||  ziro}, "ziro" }                     ,;
          { padr("Partija",20),{||  zirod}, "zirod" }                 ;
       }
// Dorade 2001
Kol:={1,2,3,4}
return PostojiSifra(F_KRED,1,10,55,"Lista kreditora",@cId,dx,dy)
#endif

************************
************************
function KrBlok(Ch)
if Ch==K_CTRL_T
 if ImaURadKr(KRED->id,"3")
   Beep(1)
   Msg("Firma se ne moze brisati jer je vec koristena u obracunu!")
   return 7
 endif
elseif Ch==K_F2
 if ImaURadKr(KRED->id,"3")
   return 99
 endif
endif

return DE_CONT

***************************************
FUNCTION ImaURadKr(cKljuc,cTag)
  LOCAL lVrati:=.f., lUsed:=.t., nArr:=SELECT()
  SELECT (F_RADKR)
  IF !USED()
    lUsed:=.f.
    O_RADKR
  ELSE
    PushWA()
  ENDIF
  SET ORDER TO TAG (cTag)
  seek cKljuc
  lVrati:=found()
  IF !lUsed
    USE
  ELSE
    PopWA()
  ENDIF
  select (nArr)
RETURN lVrati


***************************************
FUNCTION ImaUObrac(cKljuc,cTag)
  LOCAL lVrati:=.f., lUsed:=.t., nArr:=SELECT()
  SELECT (F_LD)
  IF !USED()
    lUsed:=.f.
    O_LD
  ELSE
    PushWA()
  ENDIF
  SET ORDER TO TAG (cTag)
  seek cKljuc
  lVrati:=found()
  IF !lUsed
    USE
  ELSE
    PopWA()
  ENDIF
  IF !lVrati  // ako nema u LD, provjerimo ima li u 1.dijelu obracuna (smece)
    SELECT (F_LDSM)
    IF !USED()
      lUsed:=.f.
      O_LDSM
    ELSE
      PushWA()
    ENDIF
    SET ORDER TO TAG (cTag)
    seek cKljuc
    lVrati:=found()
    IF !lUsed
      USE
    ELSE
      PopWA()
    ENDIF
  ENDIF
  select (nArr)
RETURN lVrati


***************************************
***************************************
function P_POR(cId,dx,dy)

private imekol,kol

ImeKol:={ { padr("Id",2), {|| id}, "id", {|| .t.}, {|| vpsifra(wid)} },;
          { padr("Naziv",20), {||  naz}, "naz" }                       ,;
          { padr("Iznos",20), {||  iznos}, "iznos" }                       ,;
          { padr("Donji limit",12), {||  dlimit}, "dlimit" }           ,;
          { padr("PoOpst",6), {||  poopst}, "poopst" }           ;
       }
Kol:={1,2,3,4,5}
PushWa()

select (F_SIFK)
if !used()
  O_SIFK; O_SIFV
endif

select sifk; set order to tag "ID"; seek "POR"
do while !eof() .and. ID="POR"

 AADD (ImeKol, {  IzSifKNaz("POR",SIFK->Oznaka) })
 AADD (ImeKol[Len(ImeKol)], &( "{|| ToStr(IzSifk('POR','" + sifk->oznaka + "')) }" ) )
 AADD (ImeKol[Len(ImeKol)], "SIFK->"+SIFK->Oznaka )
 if sifk->edkolona > 0
   for ii:=4 to 9
    AADD( ImeKol[Len(ImeKol)], NIL  )
   next
   AADD( ImeKol[Len(ImeKol)], sifk->edkolona  )
 else
   for ii:=4 to 10
    AADD( ImeKol[Len(ImeKol)], NIL  )
   next
 endif

 // postavi picture za brojeve
 if sifk->Tip="N"
   if decimal > 0
     ImeKol [Len(ImeKol),7] := replicate("9", sifk->duzina - sifk->decimal-1 )+"."+replicate("9",sifk->decimal)
   else
     ImeKol [Len(ImeKol),7] := replicate("9", sifk->duzina )
   endif
 endif

 AADD  (Kol, iif( sifk->UBrowsu='1',++i, 0) )

 skip
enddo
PopWa()
return PostojiSifra(F_POR,1,10,75,"Lista poreza na platu.....<F5> arhiviranje poreza, <F6> pregled",@cId,dx,dy,{|Ch| PorBl(Ch)})

***************************************
***************************************
function P_DOPR(cId,dx,dy)

private imekol,kol

ImeKol:={ { padr("Id",2), {|| id}, "id", {|| .t.}, {|| vpsifra(wid)} },;
          { padr("Naziv",20), {||  naz}, "naz" }                      , ;
          { padr("Iznos",20), {||  iznos}, "iznos" }                   ,;
          { padr("KBenef",5), {|| padc(idkbenef,5)}, "idkbenef", {|| .t.}, {|| empty(widkbenef) .or. P_KBenef(@widkbenef) } },;
          { padr("Donji limit",12), {||  dlimit}, "dlimit" }       ,;
          { padr("PoOpst",6), {||  poopst}, "poopst" }           ;
       }
Kol:={1,2,3,4,5,6}
PushWa()

select (F_SIFK)
if !used()
  O_SIFK; O_SIFV
endif

select sifk; set order to tag "ID"; seek "DOPR"
do while !eof() .and. ID="DOPR"

 AADD (ImeKol, {  IzSifKNaz("DOPR",SIFK->Oznaka) })
 AADD (ImeKol[Len(ImeKol)], &( "{|| ToStr(IzSifk('DOPR','" + sifk->oznaka + "')) }" ) )
 AADD (ImeKol[Len(ImeKol)], "SIFK->"+SIFK->Oznaka )
 if sifk->edkolona > 0
   for ii:=4 to 9
    AADD( ImeKol[Len(ImeKol)], NIL  )
   next
   AADD( ImeKol[Len(ImeKol)], sifk->edkolona  )
 else
   for ii:=4 to 10
    AADD( ImeKol[Len(ImeKol)], NIL  )
   next
 endif

 // postavi picture za brojeve
 if sifk->Tip="N"
   if decimal > 0
     ImeKol [Len(ImeKol),7] := replicate("9", sifk->duzina - sifk->decimal-1 )+"."+replicate("9",sifk->decimal)
   else
     ImeKol [Len(ImeKol),7] := replicate("9", sifk->duzina )
   endif
 endif

 AADD  (Kol, iif( sifk->UBrowsu='1',++i, 0) )

 skip
enddo
PopWa()
return PostojiSifra(F_DOPR,1,10,75,"Lista doprinosa na platu......<F5> arhiviranje doprinosa, <F6> pregled",@cId,dx,dy,{|Ch| DoprBl(Ch)})

***************************************
***************************************
function P_KBenef(cId,dx,dy)

private imekol,kol

ImeKol:={ { padr("Id",3), {|| padc(id,3)}, "id", {|| .t.}, {|| vpsifra(wid)} },;
          { padr("Naziv",8), {||  naz}, "naz" }                      , ;
          { padr("Iznos",5), {||  iznos}, "iznos" }                       ;
       }
Kol:={1,2,3}
return PostojiSifra(F_KBENEF,1,10,55,"Lista koef.beneficiranog radnog staza",@cId,dx,dy)

***************************************
***************************************
function P_StrSpr(cId,dx,dy)

private imekol,kol

ImeKol:={ { padr("Id",3), {|| id}, "id", {|| .t.}, {|| vpsifra(wid)} },;
          { padr("Naziv",20), {||  naz}, "naz" }                    , ;
          { padr("naz2",6), {|| naz2}, "naz2" }                     ;
       }
Kol:={1,2,3}
return PostojiSifra(F_STRSPR,1,10,55,"Lista: strucne spreme",@cId,dx,dy)

***************************************
***************************************
function P_VPosla(cId,dx,dy)

private imekol,kol

ImeKol:={ { padr("Id",2), {|| id}, "id", {|| .t.}, {|| vpsifra(wid)} },;
          { padr("Naziv",20), {||  naz}, "naz" }                    , ;
          { padr("KBenef",5), {|| padc(idkbenef,5)}, "idkbenef", {|| .t.}, {|| P_KBenef(@widkbenef) }  }  ;
       }
Kol:={ 1,2,3}
return PostojiSifra(F_VPOSLA,1,10,55,"Lista: Vrste posla",@cId,dx,dy)


***************************************
***************************************
function P_NorSiht(cId,dx,dy)

private imekol,kol

ImeKol:={ { padr("Id",4), {|| id}, "id", {|| .t.}, {|| vpsifra(wid)} },;
          { padr("Naziv",20), {||  naz}, "naz" }                    , ;
          { padr("JMJ",3), {|| padc(jmj,3)}, "jmj"  } , ;
          { padr("Iznos",8), {|| Iznos}, "Iznos"  }  ;
       }
Kol:={1,2,3,4}
return PostojiSifra(F_NORSIHT,1,10,55,"Lista: Norme u sihtarici",@cId,dx,dy)


***************************************
***************************************
function P_TPRSiht(cId,dx,dy)

private imekol,kol

ImeKol:={ { padr("Id",4), {|| id}, "id", {|| .t.}, {|| vpsifra(wid)} },;
          { padr("Naziv",30), {||  naz}, "naz" }                    , ;
          { padC("K1",3), {|| padc(K1,3)}, "k1"  }  ;
       }
Kol:={1,2,3}
return PostojiSifra(F_TPRSIHT,1,10,55,"Lista: Tipovi primanja u sihtarici",@cId,dx,dy)


PROCEDURE TotBrisRadn()
 LOCAL cSigurno:="N", nRec
 PRIVATE cIdRadn:=SPACE(6)
 IF !SigmaSif("SIGMATB ")
   RETURN
 ENDIF
 O_RADN         // id, "1"
 O_RADKR        // idradn, "2"
 O_LD           // idradn, "RADN"
 O_LDSM         // idradn, "RADN"
 Box(,7,75)
 @ m_x+ 0, m_y+ 5 SAY "TOTALNO BRISANJE RADNIKA IZ EVIDENCIJE"
 @ m_x+ 8, m_y+20 SAY "<F5> - trazenje radnika pomocu sifrarnika"
 SET KEY K_F5 TO TRUSif()
 DO WHILE .t.
    BoxCLS()
    IF cSigurno=="D"
      cIdRadn:=SPACE(6)
      cSigurno:="N"
    ENDIF
    @ m_x+2, m_y+2 SAY "Radnik" GET cIdRadn PICT "@!"
    @ m_x+6, m_y+2 SAY "Sigurno ga zelite obrisati (D/N) ?" GET cSigurno WHEN PrTotBR(cIdRadn) VALID cSigurno$"DN" PICT "@!"
    READ
    IF LASTKEY()==K_ESC; EXIT; ENDIF
    IF cSigurno!="D"; LOOP; ENDIF
    // brisem ga iz sifrarnika radnika
    // -------------------------------
      SELECT (F_RADN); SET ORDER TO TAG "1"; GO TOP
      SEEK cIdRadn
      DO WHILE !EOF() .and. id==cIdRadn
        SKIP 1; nRec:=RECNO(); SKIP -1; DELETE; GO (nRec)
      ENDDO
    // brisem ga iz baze kredita
    // -------------------------
      SELECT (F_RADKR); SET ORDER TO TAG "2"; GO TOP
      SEEK cIdRadn
      DO WHILE !EOF() .and. idradn==cIdRadn
        SKIP 1; nRec:=RECNO(); SKIP -1; DELETE; GO (nRec)
      ENDDO
    // brisem ga iz baze obracuna
    // --------------------------
      SELECT (F_LD); SET ORDER TO TAG "RADN"; GO TOP
      SEEK cIdRadn
      DO WHILE !EOF() .and. idradn==cIdRadn
        SKIP 1; nRec:=RECNO(); SKIP -1; DELETE; GO (nRec)
      ENDDO
    // brisem ga iz baze obracuna u smecu
    // ----------------------------------
      SELECT (F_LDSM); SET ORDER TO TAG "RADN"; GO TOP
      SEEK cIdRadn
      DO WHILE !EOF() .and. idradn==cIdRadn
        SKIP 1; nRec:=RECNO(); SKIP -1; DELETE; GO (nRec)
      ENDDO
 ENDDO
 SET KEY K_F5 TO
 BoxC()
CLOSERET


FUNCTION PrTotBr(cIdRadn)
 LOCAL cBI:="W+/G"

  SELECT (F_RADN) ; SET ORDER TO TAG "1"   ; GO TOP; SEEK cIdRadn

  SELECT (F_RADKR); SET ORDER TO TAG "2"   ; GO TOP; SEEK cIdRadn
  cKljuc:=STR(godina,4)+STR(mjesec,2)
  DO WHILE !EOF() .and. idradn==cIdRadn
    IF cKljuc < STR(godina,4)+STR(mjesec,2)
      cKljuc:=STR(godina,4)+STR(mjesec,2)
    ENDIF
    SKIP 1
  ENDDO
  SKIP -1

  SELECT (F_LD)   ; SET ORDER TO TAG "RADN"; GO TOP; SEEK cIdRadn
  cKljuc:=STR(godina,4)+STR(mjesec,2)
  DO WHILE !EOF() .and. idradn==cIdRadn
    IF cKljuc < STR(godina,4)+STR(mjesec,2)
      cKljuc:=STR(godina,4)+STR(mjesec,2)
    ENDIF
    SKIP 1
  ENDDO
  SKIP -1

  SELECT (F_LDSM) ; SET ORDER TO TAG "RADN"; GO TOP; SEEK cIdRadn
  cKljuc:=STR(godina,4)+STR(mjesec,2)
  DO WHILE !EOF() .and. idradn==cIdRadn
    IF cKljuc < STR(godina,4)+STR(mjesec,2)
      cKljuc:=STR(godina,4)+STR(mjesec,2)
    ENDIF
    SKIP 1
  ENDDO
  SKIP -1

  @ m_x+3, m_y+1 CLEAR TO m_x+5, m_y+75

  @ m_x+3, m_y+ 2 SAY "PREZIME I IME:"
  @ m_x+3, m_y+17 SAY IF(RADN->id==cIdRadn,RADN->(TRIM(naz)+" ("+TRIM(imerod)+") "+TRIM(ime)),"nema podatka") COLOR cBI
  @ m_x+4, m_y+ 2 SAY "POSLJEDNJI OBRACUN:"
  @ m_x+4, m_y+22 SAY IF(LD->idradn==cIdRadn,STR(LD->mjesec,2)+"/"+STR(LD->godina,4),"nema podatka") COLOR cBI
  @ m_x+4, m_y+35 SAY "RJ:"
  @ m_x+4, m_y+39 SAY IF(LD->idradn==cIdRadn,LD->idrj,"nema podatka") COLOR cBI
  @ m_x+5, m_y+ 2 SAY "POSLJEDNJA RATA KREDITA:"
  @ m_x+5, m_y+27 SAY IF(RADKR->idradn==cIdRadn,STR(RADKR->mjesec,2)+"/"+STR(RADKR->godina,4),"nema podatka") COLOR cBI

RETURN IF(RADN->id==cIdRadn.or.LD->idradn==cIdRadn.or.;
          LDSM->idradn==cIdRadn.or.RADKR->idradn==cIdRadn,.t.,.f.)

PROCEDURE TRUSif()
  IF READVAR()=="CIDRADN"
    P_Radn(@cIdRadn)
    KEYBOARD CHR(K_ENTER)+CHR(K_UP)
  ENDIF
RETURN

***************************************
***************************************
function P_Banke(cId,dx,dy)

private imekol,kol

ImeKol:={ { padr("Id",2), {|| id}, "id", {|| .t.}, {|| vpsifra(wid)} },;
          { "Naziv", {||  naz}, "naz" }                      ,;
          { "Mjesto", {|| mjesto}, "mjesto" }                ;
       }
Kol:={1,2,3}
return PostojiSifra(F_BANKE,1,10,55,"Lista banaka",@cId,dx,dy)


FUNC PorBl(Ch)
  LOCAL nVrati:=DE_CONT, nRec:=RECNO()
  DO CASE
    CASE Ch==K_F5

      // pitati za posljednji mjesec
      // ---------------------------
      cMj  := gMjesec
      cGod := gGodina
      private GetList:={}
      Box("#PROMJENA POREZA U TOKU GODINE",4,60)
        @ m_x+2, m_y+2 SAY "Posljednji mjesec po starim porezima:" GET cMj VALID cMj>0 .and. cMj<13
        @ m_x+3, m_y+2 SAY "Godina: "+STR(cGod)
        READ
        IF LASTKEY()==K_ESC; BoxC(); RETURN nVrati; ENDIF
      BoxC()

      // formiraj imena direktorija
      // --------------------------
      cPodDir := PADL(ALLTRIM(STR(cMj)),2,"0")+STR(cGod,4)
      cPath:=SIFPATH
      aIme := { "POR.DBF" , "POR.CDX" }

      // zatvaram POR.DBF
      // ----------------
      SELECT POR
      USE

      // napraviti direktorij i iskopirati POR.* u njega
      // -------------------------------------------------
      DIRMAKE(cPath+cPodDir)
      lKopirano:=.f.
      FOR i:=1 TO LEN(aIme)
        IF FILE(cpath+cPodDir+"\"+aIme[i])
          MsgBeep("Fajl "+aIme[i]+" vec postoji u "+cpath+cPodDir+" !"+;
                  "#Ukoliko ga sada zamijenite necete ga moci vratiti!")
          IF Pitanje(,"Zelite li ga zamijeniti?","N")=="N"
            LOOP
          ENDIF
        ENDIF
        lKopirano:=.t.
        FILECOPY( cPath+aIme[i] , cpath+cPodDir+"\"+aIme[i] )
      NEXT

      // otvaram POR.DBF
      // ---------------
      O_POR
      GO (nRec)

      // poruka: mozete definisati nove poreze
      // -------------------------------------
      IF lKopirano
        MsgBeep("Stari porezi su smjesteni u podrucje "+cPodDir+"#Nakon ovoga "+;
                "mozete definisati nove poreze.")
      ENDIF

    CASE Ch==K_F6
      // meni sezona
      cPath   := SIFPATH
      cGodina := gGodina
      private GetList:={}
      Box(,3,30)
        @ m_x+2, m_y+2 SAY "Godina:" GET cGodina PICT "9999"
        READ
      BoxC()
      IF LASTKEY()==K_ESC; RETURN nVrati; ENDIF
      cGodina:=STR(cGodina,4,0)
      aSez := ASezona2(cPath,cGodina,"POR.DBF")
      IF EMPTY(aSez)
        MsgBeep("Ne postoje sezone promjena poreza u "+cGodina+". godini!")
        RETURN nVrati
      ELSE
        // meni sezona - aSez
        // ------------------
        FOR i:=1 TO LEN(aSez)
          aSez[i] := PADR( aSez[i,1]+" - "+NazMjeseca(VAL(LEFT(aSez[i,1],2))) , 73)
        NEXT
        h:=ARRAY(LEN(aSez)); AFILL(h,"")
        Box("#SEZONE PRED PROMJENU POREZA U "+cGodina+".GODINI: อออออ <Enter>-izbor ",MIN(LEN(aSez),16)+3,77)
         @ m_x+1, m_y+2 SAY PADC("M J E S E C",75)
         @ m_x+2, m_y+2 SAY REPL("ฤ",75)
         nPom := 1
         @ row()-1, col()-6 SAY ""
         nPom := Menu("SPME",aSez,nPom,.f.,,,{m_x+2,m_y+1})
         IF nPom>0
           Menu("SPME",aSez,0,.f.)
         ENDIF
        BoxC()
        IF nPom>0
          cPorDir := LEFT( aSez[nPom] , 6 )
        ELSE
          RETURN nVrati
        ENDIF
      ENDIF

      // otvaranje sezonske baze
      SELECT (F_POR); USE
      USE (cPath+cPorDir+"\POR") ; SET ORDER TO TAG "ID"
      GO TOP
      @ m_x+11, m_y+2 SAY "Porezi koji su vazili zakljucno sa (MMGGGG):"+cPorDir
      KEYBOARD CHR(K_CTRL_PGUP)
      nVrati:=DE_REFRESH

  ENDCASE
RETURN nVrati


FUNC DoprBl(Ch)
  LOCAL nVrati:=DE_CONT, nRec:=RECNO()
  DO CASE
    CASE Ch==K_F5

      // pitati za posljednji mjesec
      // ---------------------------
      cMj  := gMjesec
      cGod := gGodina
      private GetList:={}
      Box("#PROMJENA DOPRINOSA U TOKU GODINE",4,60)
        @ m_x+2, m_y+2 SAY "Posljednji mjesec po starim doprinosima:" GET cMj VALID cMj>0 .and. cMj<13
        @ m_x+3, m_y+2 SAY "Godina: "+STR(cGod)
        READ
        IF LASTKEY()==K_ESC; BoxC(); RETURN nVrati; ENDIF
      BoxC()

      // formiraj imena direktorija
      // --------------------------
      cPodDir := PADL(ALLTRIM(STR(cMj)),2,"0")+STR(cGod,4)
      cPath:=SIFPATH
      aIme := { "DOPR.DBF" , "DOPR.CDX" }

      // zatvaram DOPR.DBF
      // -----------------
      SELECT DOPR
      USE

      // napraviti direktorij i iskopirati DOPR.* u njega
      // -------------------------------------------------
      DIRMAKE(cPath+cPodDir)
      lKopirano:=.f.
      FOR i:=1 TO LEN(aIme)
        IF FILE(cpath+cPodDir+"\"+aIme[i])
          MsgBeep("Fajl "+aIme[i]+" vec postoji u "+cpath+cPodDir+" !"+;
                  "#Ukoliko ga sada zamijenite necete ga moci vratiti!")
          IF Pitanje(,"Zelite li ga zamijeniti?","N")=="N"
            LOOP
          ENDIF
        ENDIF
        lKopirano:=.t.
        FILECOPY( cPath+aIme[i] , cpath+cPodDir+"\"+aIme[i] )
      NEXT

      // otvaram DOPR.DBF
      // ----------------
      O_DOPR
      GO (nRec)

      // poruka: mozete definisati nove doprinose
      // ----------------------------------------
      IF lKopirano
        MsgBeep("Stari doprinosi su smjesteni u podrucje "+cPodDir+"#Nakon ovoga "+;
                "mozete definisati nove doprinose.")
      ENDIF

    CASE Ch==K_F6
      // meni sezona
      cPath   := SIFPATH
      cGodina := gGodina
      private GetList:={}
      Box(,3,30)
        @ m_x+2, m_y+2 SAY "Godina:" GET cGodina PICT "9999"
        READ
      BoxC()
      IF LASTKEY()==K_ESC; RETURN nVrati; ENDIF
      cGodina:=STR(cGodina,4,0)
      aSez := ASezona2(cPath,cGodina,"DOPR.DBF")
      IF EMPTY(aSez)
        MsgBeep("Ne postoje sezone promjena doprinosa u "+cGodina+". godini!")
        RETURN nVrati
      ELSE
        // meni sezona - aSez
        // ------------------
        FOR i:=1 TO LEN(aSez)
          aSez[i] := PADR( aSez[i,1]+" - "+NazMjeseca(VAL(LEFT(aSez[i,1],2))) , 73)
        NEXT
        h:=ARRAY(LEN(aSez)); AFILL(h,"")
        Box("#SEZONE PRED PROMJENU DOPRINOSA U "+cGodina+".GODINI: อออออ <Enter>-izbor ",MIN(LEN(aSez),16)+3,77)
         @ m_x+1, m_y+2 SAY PADC("M J E S E C",75)
         @ m_x+2, m_y+2 SAY REPL("ฤ",75)
         nPom := 1
         @ row()-1, col()-6 SAY ""
         nPom := Menu("SDME",aSez,nPom,.f.,,,{m_x+2,m_y+1})
         IF nPom>0
           Menu("SDME",aSez,0,.f.)
         ENDIF
        BoxC()
        IF nPom>0
          cDoprDir := LEFT( aSez[nPom] , 6 )
        ELSE
          RETURN nVrati
        ENDIF
      ENDIF

      // otvaranje sezonske baze
      SELECT (F_DOPR); USE
      USE (cPath+cDoprDir+"\DOPR") ; SET ORDER TO TAG "ID"
      GO TOP
      @ m_x+11, m_y+2 SAY "Doprinosi koji su vazili zakljucno sa (MMGGGG):"+cDoprDir
      KEYBOARD CHR(K_CTRL_PGUP)
      nVrati:=DE_REFRESH

  ENDCASE
RETURN nVrati

