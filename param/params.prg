#include "porld.ch"


// ---------------------------------------
// postavka parametara firme
// ---------------------------------------
function SetFirma()
  private GetList:={}
  IF cIspravka=="D"
    Box(, 9,77)
      @ m_x+ 2,m_y+2 SAY "Radna jedinica" GET gRJ valid P_Rj(@gRj) pict "@!"
      @ m_x+ 4,m_y+2 SAY "Mjesec        " GET gMjesec pict "99"
      @ m_x+ 6,m_y+2 SAY "Godina        " GET gGodina pict "9999"
      IF lViseObr
        @ m_x+ 7,m_y+2 SAY "Obracun       " GET gObracun WHEN HelpObr(.f.,gObracun) VALID ValObr(.f.,gObracun)
      ENDIF
      @ m_x+ 8,m_y+2 SAY "Naziv firme:" GET gNFirma
      @ m_x+ 9,m_y+2 SAY "TIP SUBJ.:" GET gTS
      read
      clvbox()
    BoxC()
    IF lastkey()<>K_ESC
      Wpar("fn",gNFirma)
      Wpar("ts",gTS)
      Wpar("go",gGodina)
      Wpar("mj",gMjesec)
      Wpar("ob",gObracun)
      Wpar("rj",gRJ)
    ENDIF
    cIspravka:="N"
    RETURN .T.
  ELSEIF cIspravka=="N"
    RETURN .T.
  ELSE
    RETURN .F.
  ENDIF
RETURN (NIL)


FUNCTION SetForma()
  private GetList:={}
  IF cIspravka=="D"
    Box(,11,77)
      @ m_x+ 2,m_y+2 SAY "Zaokruzenje primanja" GET gZaok pict "99"
      @ m_x+ 4,m_y+2 SAY "Zaokruzenje poreza i doprinosa" GET gZaok2 pict "99"
      @ m_x+ 6,m_y+2 SAY "Valuta " GET gValuta pict "XXX"
      @ m_x+ 8,m_y+2 SAY "Prikaz iznosa" GET gPicI
      @ m_x+10,m_y+2 SAY "Prikaz sati" GET gPicS
      read
    BoxC()
    IF lastkey()<>K_ESC
      Wpar("pi",gPicI)
      Wpar("ps",gPicS)
      Wpar("va",gValuta)
      Wpar("z2",gZaok2)
      Wpar("zo",gZaok)
    ENDIF
    cIspravka:="N"
    RETURN .T.
  ELSEIF cIspravka=="N"
    RETURN .T.
  ELSE
    RETURN .F.
  ENDIF
RETURN (NIL)



FUNCTION SetFormule()
  private GetList:={}
  IF cIspravka=="D"
    Box(,17,77)
      gFURaz:=PADR(gFURaz,100)
      gFUPrim:=PADR(gFUPrim,100)
      gFUSati:=PADR(gFUSati,100)
      gFURSati:=PADR(gFURSati,100)
      @ m_x+ 2,m_y+2 SAY "Formula za ukupna primanja:" GET gFUPrim  pict "@!S30"
      @ m_x+ 4,m_y+2 SAY "Formula za ukupno sati    :" GET gFUSati  pict "@!S30"
      @ m_x+ 6,m_y+2 SAY "Formula za godisnji:" GET gFUGod pict "@!S30"
      @ m_x+ 8,m_y+2 SAY "Formula za uk.prim.-razno :" GET gFURaz pict "@!S30"
      @ m_x+10,m_y+2 SAY "Formula za uk.sati -razno :" GET gFURSati pict "@!S30"
      @ m_x+12,m_y+2 SAY "God. promjena koef.min.rada - ZENE:" GET gMRZ   pict "9999.99"
      @ m_x+14,m_y+2 SAY "God. promjena koef.min.rada - MUSK:" GET gMRM   pict "9999.99"
      @ m_x+16,m_y+2 SAY "% prosjecne plate kao donji limit neta za obracun poreza i doprinosa" GET gPDLimit pict "999.99"
      read
    BoxC()
    IF lastkey()<>K_ESC
      Wpar("gd",gFUGod)
      WPar("m1", @gMRM)
      WPar("m2", @gMRZ)
      WPar("dl", @gPDLimit)
      Wpar("uH",@gFURSati)
      Wpar("uS",@gFUSati)
      Wpar("up",gFUPrim)
      Wpar("ur",gFURaz)
    ENDIF
    cIspravka:="N"
    RETURN .T.
  ELSEIF cIspravka=="N"
    RETURN .T.
  ELSE
    RETURN .F.
  ENDIF
RETURN (NIL)



FUNCTION SetObracun()
  private GetList:={}
  cVarPorol:=padr(cVarPorol,2)
  IF cIspravka=="D"
    Box(,16,77)
      @ m_x+ 2,m_y+2 SAY "Tip obracuna " GET gTipObr
      @ m_x+ 4,m_y+2 SAY "Mogucnost unosa mjeseca pri obradi D/N:" GET gUnMjesec  pict "@!" valid glistic $ "DN"
      @ m_x+ 6,m_y+2 SAY "Koristiti set formula (sifrarnik Tipovi primanja):" GET gSetForm pict "9" valid V_setform()
      @ m_x+ 8,m_y+2 SAY "Minuli rad  %/B:" GET gMinR  valid gMinR $ "%B"   pict "@!"
      @ m_x+10,m_y+2 SAY "Pri obracunu napraviti poreske olaksice D/N:" GET gDaPorOl  valid gDaPorOl $ "DN"   pict "@!"
      @ m_x+11,m_y+2 SAY "Ako se prave por.ol.pri obracunu, koja varijanta se koristi:"
      @ m_x+12,m_y+2 SAY " '1' - POROL = RADN->porol*PAROBR->prosld/100 ÄÄ¿  "
      @ m_x+13,m_y+2 SAY " '2' - POROL = RADN->porol, '29' - LD->I29    ÄÄÁÄ>" GET cVarPorOl WHEN gDaPorOl=="D"   PICT "99"

      @ m_x+15,m_y+2 SAY "Grupe poslova u specif.uz platu (1-automatski/2-korisnik definise):" GET gVarSpec  valid gVarSpec $ "12" pict "9"
      @ m_x+16,m_y+2 SAY "Obrada sihtarice ?" GET gSihtarica valid gSihtarica $ "DN" pict "@!"
      read
    BoxC()
    IF lastkey()<>K_ESC
      WPar("fo", gSetForm)
      WPar("mr", @gMinR)   // min rad %, Bodovi
      WPar("p9", @gDaPorOl) // praviti poresku olaksicu D/N
      Wpar("to",gTipObr)
      Wpar("vo",cVarPorOl)
      WPar("um",gUNMjesec)
      Wpar("vs",gVarSpec)
      Wpar("Si",gSihtarica)
    ENDIF
    cIspravka:="N"
    RETURN .T.
  ELSEIF cIspravka=="N"
    RETURN .T.
  ELSE
    RETURN .F.
  ENDIF
RETURN (NIL)



FUNCTION SetPrikaz()
  private GetList:={}
  IF cIspravka=="D"
    Box(,17,77)
      @ m_x+ 2,m_y+2 SAY "Krediti-rekap.po 'na osnovu' (D/N/X)?" GET gReKrOs VALID gReKrOs $ "DNX" PICT "@!"
      @ m_x+ 4,m_y+2 SAY "Na kraju obrade odstampati listic D/N:" GET gListic  pict "@!" valid glistic $ "DN"
      @ m_x+ 6,m_y+2 SAY "Prikaz bruto iznosa na kartici radnika (D/N/X) " GET gPrBruto pict "@!" valid gPrBruto $ "DNX"
      @ m_x+ 8,m_y+2 SAY "Potpis na kartici radnika D/N:" GET gPotp  valid gPotp $ "DN"   pict "@!"
      @ m_x+10,m_y+2 SAY "Varijanta kartice plate za kredite (1/2) ?" GET gReKrKP VALID gReKrKP$"12"
      @ m_x+12,m_y+2 SAY "Opis osnovnih podataka za obracun (1-bodovi/2-koeficijenti) ?" GET gBodK VALID gBodK$"12"
      @ m_x+14,m_y+2 SAY "Pregled plata: varijanta izvjestaja (1/2)" GET gVarPP VALID gVarPP$"12"
      //@ m_x+14,m_y+2 SAY "Prikaz tabele 0/1/2" GET gTabela VALID gTabela$"012"
      read
    BoxC()
    IF lastkey()<>K_ESC
      Wpar("bk",gBodK)
      Wpar("kp",gReKrKP)
      Wpar("pp",gVarPP)
      Wpar("li",gListic)
      WPar("pb", gPrBruto)   // set formula
      WPar("po", gPotp)   // potp4is na listicu
      Wpar("rk",gReKrOs)
      //Wpar("tB",gTabela)
    ENDIF
    cIspravka:="N"
    RETURN .T.
  ELSEIF cIspravka=="N"
    RETURN .T.
  ELSE
    RETURN .F.
  ENDIF
RETURN (NIL)



FUNCTION SetRazno()
  private GetList:={}
  IF cIspravka=="D"
    Box(, 4,77)
      @ m_x+ 2,m_y+2 SAY "Fajl obrasca specifikacije" GET gFSpec VALID V_FSpec()
      read
    BoxC()
    IF lastkey()<>K_ESC
      WPar("os", @gFSpec)   // fajl-obrazac specifikacije
    ENDIF
    cIspravka:="N"
    RETURN .T.
  ELSEIF cIspravka=="N"
    RETURN .T.
  ELSE
    RETURN .F.
  ENDIF
RETURN (NIL)


FUNCTION SetPorLD()
  private GetList:={}
  IF cIspravka=="D"
    gBrRjes:=padr(gBrRjes,60)
    Box(, 7,77)
      cEr:=" "
      @ m_x+ 1,m_y+2 SAY "Formula za broj rjesenja " GET gBrRjes pict "@!S40"
      cEr:="0"
      @ m_x+ 4,m_y+2 SAY "Fajl rjesenja.txt 0/1/2/5/6" GET cER valid {|| cEr="0" .or. V_FRjes(cEr) }
      @ m_x+ 6,m_y+2 SAY "Minimalni iznos mjesecne naknade:" GET gMinRata  pict "99999.99"
      @ m_x+ 7,m_y+2 SAY "Ispis TXT/RTF tekuci   (T/R)    :" GET gTxtRtf  pict "@!" valid gTxtRTf $ "TR"
      read
    BoxC()
    gBrRjes:=trim(gBrRjes)
    IF lastkey()<>K_ESC
      WPar("#1", gBrRjes)   // fajl-obrazac specifikacije
      WPar("m3", @gMinRata)
      WPar("#2", @gTxtRtf)
    ENDIF
    cIspravka:="N"
    RETURN .T.
  ELSEIF cIspravka=="N"
    RETURN .T.
  ELSE
    RETURN .F.
  ENDIF
RETURN (NIL)



function v_setform()
local cscsr, nArr:=SELECT()
if file(SIFPATH+"TIPPR.DB"+gSetForm) .and. pitanje(,"Sifrarnik tipova primanja uzeti iz arhive br. "+gSetForm+" ?","N")=="D"
 save screen to cscr
 select (F_TIPPR)
 use

 cls
 ? filecopy  (SIFPATH+"TIPPR.DB"+gSetForm  ,SIFPATH+"TIPPR.DBF"  )
 ? filecopy  (SIFPATH+"TIPPR.CD"+gSetForm,SIFPATH+"TIPPR.CDX")
 inkey(20)
 restore screen from cscr
 select (F_TIPPR)
 if !used(); O_TIPPR; endif
 P_Tippr()
 select params
elseif  pitanje(,"Tekuci sifrarnik tipova primanja staviti u arhivu br. "+gSetForm+" ?","N")=="D"
 save screen to cscr
 select (F_TIPPR)
 use
 cls
 ? filecopy  (SIFPATH+"TIPPR.DBF",SIFPATH+"TIPPR.DB"+gSetForm)
 ? filecopy  (SIFPATH+"TIPPR.CDX",SIFPATH+"TIPPR.CD"+gSetForm)
 inkey(20)
 restore screen from cscr
endif
select (nArr)
return .t.



