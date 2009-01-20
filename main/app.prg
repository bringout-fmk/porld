#include "porld.ch"


function TPorModNew()
local oObj

#ifdef CLIP

#else
	oObj:=TPorLDMod():new()
#endif

oObj:self:=oObj
return oObj


#ifdef CPP
/*! \class TLDMod
 *  \brief LD aplikacijski modul
 */

class TPorLDMod: public TAppMod
{
	public:
	*void dummy();
	*void setGVars();
	*void mMenu();
	*void mMenuStandard();
	*void sRegg();
	*void initdb();
	*void srv();
	*void chk_db();
#endif

#ifndef CPP
#include "class(y).ch"
CREATE CLASS TPorLDMod INHERIT TAppMod
	EXPORTED:
	method dummy 
	method setGVars
	method mMenu
	method mMenuStandard
	method sRegg
	method initdb
	method srv
	method chk_db
END CLASS
#endif


/*! \fn TLDMod::dummy()
 *  \brief dummy
 */

*void TLDMod::dummy()
*{
method dummy()
return
*}


*void TLDMod::initdb()
*{
method initdb()

::oDatabase:=TDBPorNew()

return nil
*}


/*! \fn *void TLDMod::chk_db()
 *  \brief provjera tabela
 */
*void TLDMod::chk_db()
*{
method chk_db()
local cModStru:=""
// provjeri postojanje specificnih polja LD.DBF
O_RADN
select radn
if radn->(FieldPOS("HIREDFROM")) == 0
	// obavjesti za modifikaciju
	cModStru += "DP.CHS, "
endif

if !EMPTY(cModStru)
	MsgBeep("Upozorenje!##Odraditi modifikacije struktura:#" + cModStru)
endif

return


/*! \fn *void TLDMod::mMenu()
 *  \brief Osnovni meni LD modula
 */
*void TLDMod::mMenu()
*{
method mMenu()

private Izbor
private lPodBugom

CheckROnly(KUMPATH + "\LD.DBF")

SETKEY(K_SH_F1,{|| Calc()})
Izbor:=1

O_LD
select ld

TrebaRegistrovati(10)

::chk_db()

use

#ifdef PROBA
	KEYBOARD "213"
#endif

@ 1,2 SAY padc(gTS+": "+gNFirma,50,"*")
@ 4,5 SAY ""

ParObracun()

::mMenuStandard()

::quit()

return nil
*}


*void TLDMod::mMenuStandard()
*{
method mMenuStandard
private opc:={}
private opcexe:={}

AADD(opc,   Lokal("1. porodilje-rjesenja                            "))
AADD(opcexe, {|| mnu_rjes()} )
AADD(opc,   Lokal("2. generacija obracuna"))
AADD(opcexe, {|| Rekalk()})
AADD(opc,   Lokal("3. unos/pregled podataka nakon generacije"))
AADD(opcexe, {|| Unos()})
AADD(opc,   Lokal("4. izvjestaji"))
AADD(opcexe, {|| MnuIzvj()})
AADD(opc,   Lokal("5. unos neisplacenih naknada "))
AADD(opcexe, {|| PriBris()})
AADD(opc,   Lokal("6. brisanje obracun za jedan mjesec svi radnici ! "))
AADD(opcexe, {|| BrisiMj()})
AADD(opc,   Lokal("7. radnici obradjeni vise puta za jedan mjesec "))
AADD(opcexe, {|| VisePuta()})
AADD(opc,   Lokal("8. promjena sifre radnika/preduzeca "))
AADD(opcexe, {|| PromSif()})
AADD(opc,   Lokal("9. definisanje parametara obracuna "))
AADD(opcexe, {|| DefinisiObr()})

AADD(opc,"------------------------------------")
AADD(opcexe, nil)
AADD(opc,   Lokal("7. sifrarnici"))
AADD(opcexe, {|| mnu_sifre()})
AADD(opc,   Lokal("9. administriranje baze podataka")) 
AADD(opcexe, {|| MnuAdmin()})
AADD(opc,"------------------------------------")
AADD(opcexe, nil)
AADD(opc,"------------------------------------")
AADD(opcexe, nil)
AADD(opc,   Lokal("X. parametri     "))
AADD(opcexe, {|| mnu_param()})

private Izbor:=1

say_fmk_ver()

Menu_SC("gld",.t.,lPodBugom)

return



*void TLDMod::sRegg()
*{
method sRegg()
sreg("PORLD.EXE","PORLD")
return
*}

*void TLDMod::srv()
*{
method srv()
? "Pokrecem PORLD aplikacijski server"
if (MPar37("/KONVERT", goModul))
	if LEFT(self:cP5,3)=="/S="
		cKonvSez:=SUBSTR(self:cP5,4)
		? "Radim sezonu: " + cKonvSez
		if cKonvSez<>"RADP"
			// prebaci se u sezonu cKonvSez
			goModul:oDataBase:cSezonDir:=SLASH+cKonvSez
 			goModul:oDataBase:setDirKum(trim(goModul:oDataBase:cDirKum)+SLASH+cKonvSez)
 			goModul:oDataBase:setDirSif(trim(goModul:oDataBase:cDirSif)+SLASH+cKonvSez)
 			goModul:oDataBase:setDirPriv(trim(goModul:oDataBase:cDirPriv)+SLASH+cKonvSez)
		endif
	endif
	goModul:oDataBase:KonvZN()
	goModul:quit(.f.)
endif
return
*}


/*! \fn *void TLDMod::setGVars()
 *  \brief opste funkcije LD modula
 */
*void TLDMod::setGVars()
*{
method setGVars()
O_PARAMS

//::super:setGVars()

SetFmkSGVars()

//SetLDSpecifVars()

public cSection:="1"
public cHistory:=" "
public aHistory:={}
public cFormula:=""

public gRJ:="00"
public gnHelpObr:=0
public gMjesec:=1
public gObracun:=" "
public gGodina:=2008
public gZaok:=2
public gZaok2:=2
public gValuta:="KM "
public gPicI:="99999999.99"
public gPicS:="999999"
public gTipObr:="1"
public gVarSpec:="1"
public cVarPorOl:="1"
public gSihtarica:="N"
public gFUPrim:=padr("UNETO+I24+I25",50)
public gFURaz :=padr("",60)
public gFUSati:=padr("USATI",50)
public gFURSati:=padr("",50)
public gFUGod:=padr("I06",40)
public gNFirma:=space(20)  
// naziv firme
public gListic:="N"
public gTS:="Preduzece"
public gUNMjesec:="N"
public gMRM:=0
public gMRZ:=0
public gPDLimit:=0
public gSetForm:="1"
public gPrBruto:="N"
public gMinR:="%"
public gPotp:="D"
public gBodK:="1"
public gDaPorol:="D" 
// pri obracunu uzeti u obzir poreske olaksice
public gFSpec:=PADR("SPEC.TXT",12), gReKrOs:="X", gReKrKP:="1", gVarPP:="1"
public gPotpRpt:="N"
public gPotp1:=PADR("PADL('Potpis:',70)",150)
public gPotp2:=PADR("PADL('_________________',70)",150)
public lViseObr:=.f.
public lVOBrisiCDX:=.f.
public cLdPolja:=40
public cZabrana:="Opcija nedostupna za ovaj nivo !!!"
public gZastitaObracuna:=IzFmkIni("LD","ZastitaObr","N",KUMPATH)
public gMinRata:=150
public gBrRjes:='"02-01/8-124-"+Bez0(RIGHT(radkr->naosnovu,6))'
public gTxtRTf:="T"

RPar("m3",@gMinRata)
Rpar("#1",@gBrRjes)
RPar("#2",@gTxtRtf)

RPar("bk",@gBodK)      
// opisno: 1-"bodovi" ili 2-"koeficijenti"
Rpar("fn",@gNFirma)
Rpar("ts",@gTS)
RPar("fo",@gSetForm)   
// set formula
Rpar("gd",@gFUGod)
Rpar("go",@gGodina)
Rpar("kp",@gReKrKP)
Rpar("pp",@gVarPP)
Rpar("li",@gListic)
RPar("m1",@gMRM)
RPar("m2",@gMRZ)
RPar("dl",@gPDLimit)
Rpar("mj",@gMjesec)
Rpar("ob",@gObracun)
RPar("mr",@gMinR)      // min rad %, Bodovi
RPar("os",@gFSpec)     // fajl-obrazac specifikacije
RPar("p9",@gDaPorOl)   // praviti poresku olaksicu D/N
RPar("pb",@gPrBruto)   // set formula
Rpar("pi",@gPicI)
RPar("po",@gPotp)      // potpis na listicu
Rpar("ps",@gPicS)
Rpar("rj",@gRj)
Rpar("rk",@gReKrOs)
Rpar("to",@gTipObr)
Rpar("vo",@cVarPorOl)
Rpar("uH",@gFURSati)
Rpar("uS",@gFUSati)
RPar("um",@gUNMjesec)
Rpar("up",@gFUPrim)
Rpar("ur",@gFURaz)
Rpar("va",@gValuta)
Rpar("vs",@gVarSpec)
Rpar("Si",@gSihtarica)
Rpar("z2",@gZaok2)
Rpar("zo",@gZaok)

select (F_PARAMS)
use

LDPoljaINI()

//definisano u SC_CLIB-u
gGlBaza:="LD.DBF"

public lPodBugom:=.f.
IF IzFMKINI("ZASTITA","PodBugom","N",KUMPATH)=="D"
  lPodBugom:=.t.
  gaKeys := { { K_ALT_O , {|| OtkljucajBug()} } }
ELSE
  lPodBugom:=.f.
ENDIF

return


function RadnikJeProizvodni()
private cPom
cPom:=IzFmkIni("ProizvodniRadnik","Formula",'"P"$RADN->K4',KUMPATH)
return (&cPom)


