/* 
 * This file is part of the bring.out FMK, a free and open source 
 * accounting software suite,
 * Copyright (c) 1996-2011 by bring.out doo Sarajevo.
 * It is licensed to you under the Common Public Attribution License
 * version 1.0, the full text of which (including FMK specific Exhibits)
 * is available in the file LICENSE_CPAL_bring.out_FMK.md located at the 
 * root directory of this source code archive.
 * By using this software, you agree to be bound by its terms.
 */


#include "ld.ch"


function TDbPorNew()
local oObj
oObj:=TDbPorLD():new()
oObj:self:=oObj
oObj:cName:="PORLD"
oObj:lAdmin:=.f.
return oObj


#ifdef CPP
class TDbPorLD: public TDB 
{
     public:
     	TObject self;
	string cName;
	*void dummy();
	*void skloniSezonu(string cSezona, bool finverse, bool fda, bool fnulirati, bool fRS);
	*void install(string cKorisn,string cSifra,variant p3,variant p4,variant p5,variant p6,variant p7);
	*void setgaDBFs();
	*void obaza(int i);
	*void ostalef();
	*void kreiraj(int nArea);
}
#endif

#ifndef CPP
#include "class(y).ch"
CREATE CLASS TDbPorLD INHERIT TDB

	EXPORTED:
	var    self
	var    cName
	method skloniSezonu	
	method install	
	method setgaDBFs	
	method ostalef	
	method obaza	
	method kreiraj	
	method konvZn

END CLASS
#endif


/*! \fn *void TDbPorLD::dummy()
 */
*void TDbPorLD::dummy()
*{
method dummy
return
*}

/*! \fn *void TDbPorLD::skloniSezonu(string cSezona, bool finverse,bool fda,bool fnulirati,bool fRS)
 *  \brief formiraj sezonsku bazu podataka
 *  \param cSezona - 
 *  \param fInverse - .t. iz sezone u radno, .f. iz radnog u sezonu
 *  \param fda - ne znam
 *  \param fnulirati - nulirati tabele
 *  \param fRS - ne znam
 */

*void TDbPorLD::skloniSezonu(string cSezona, bool finverse,bool fda,bool fnulirati,bool fRS)
*{

method skloniSezonu(cSezona,finverse,fda,fnulirati, fRS)

local cScr

if fDa==nil
	fDa:=.f.
endif

if fInverse==nil
	fInverse:=.f.
endif

if fNulirati==nil
	fNulirati:=.f.
endif

if fRs==nil
	// mrezna radna stanica , sezona je otvorena
  	fRs:=.f.
endif

if fRs // radna stanica
	if File(PRIVPATH+cSezona+SLASH+"_RADKR.DBF")
      	// nema se sta raditi ......., pripr.dbf u sezoni postoji !
      		return
	endif
  	aFilesK:={}
  	aFilesS:={}
  	aFilesP:={}
endif

save screen to cScr

if KLevel<>"0"
	MsgBeep("Nemate pravo na koristenje ove opcije")
endif

cls

if fRS
   // mrezna radna stanica
   ? "Formiranje DBF-ova u privatnom direktoriju, RS ...."
endif

?

if fInverse
	? "Prenos iz  sezonskih direktorija u radne podatke"
else
	? "Prenos radnih podataka u sezonske direktorije"
endif

?
// privatni
fNul:=.f.

Skloni(PRIVPATH,"PARAMS.DBF",cSezona,finverse,fda,fnul)
Skloni(PRIVPATH,"_RADN.DBF",cSezona,finverse,fda,fnul)
Skloni(PRIVPATH,"_RADKR.DBF",cSezona,finverse,fda,fnul)
Skloni(PRIVPATH,"_OPSLD.DBF",cSezona,finverse,fda,fnul)
Skloni(PRIVPATH,"_KRED.DBF",cSezona,finverse,fda,fnul)
Skloni(PRIVPATH,"_PRIPNO.DBF",cSezona,finverse,fda,fnul)
Skloni(PRIVPATH,"_LD.DBF",cSezona,finverse,fda,fnul)
Skloni(PRIVPATH,"GPARAMS.DBF",cSezona,finverse,fda,fnul)
Skloni(PRIVPATH,"LDT22.DBF",cSezona,finverse,fda,fnul)
Skloni(PRIVPATH,"OPSLD.DBF",cSezona,finverse,fda,fnul)
Skloni(PRIVPATH,"REKNI.DBF",cSezona,finverse,fda,fnul)
Skloni(PRIVPATH,"FMK.INI",cSezona,finverse,fda,fnul)

if fNulirati
	fNul:=.t.
else
	fNul:=.f.
endif  

Skloni(PRIVPATH,"LDSM.DBF",cSezona,finverse,fda,fnul)

if fRs
	// mrezna radna stanica!!! , baci samo privatne direktorije
 	?
 	?
 	?
 	Beep(4)
 	? "pritisni nesto za nastavak.."
 	restore screen from cScr
 	return
endif

fNul:=.f.

Skloni(KUMPATH,"RADN.DBF",cSezona,finverse,fda,fnul)
Skloni(KUMPATH,"RADKR.DBF",cSezona,finverse,fda,fnul)
Skloni(KUMPATH,"RJ.DBF",cSezona,finverse,fda,fnul)
Skloni(KUMPATH,"LD.DBF",cSezona,finverse,fda,fnul)
Skloni(KUMPATH,"RJES.DBF",cSezona,finverse,fda,fnul)
Skloni(KUMPATH,"KPARAMS.DBF",cSezona,finverse,fda,fnul)
Skloni(KUMPATH,"NORSIHT.DBF",cSezona,finverse,fda,fnul)
Skloni(KUMPATH,"FMK.INI",cSezona,finverse,fda,fnul)
Skloni(KUMPATH,"RADSIHT.DBF",cSezona,finverse,fda,fnul)
Skloni(KUMPATH,"REKLD.DBF",cSezona,finverse,fda,fnul)
Skloni(KUMPATH,"TPRSIHT.DBF",cSezona,finverse,fda,fnul)

fNul:=.f.

Skloni(SIFPATH,"PAROBR.DBF",cSezona,finverse,fda,fnul)
Skloni(SIFPATH,"KRED.DBF",cSezona,finverse,fda,fnul)
Skloni(SIFPATH,"OPS.DBF",cSezona,finverse,fda,fnul)
Skloni(SIFPATH,"POR.DBF",cSezona,finverse,fda,fnul)
Skloni(SIFPATH,"DOPR.DBF",cSezona,finverse,fda,fnul)
Skloni(SIFPATH,"STRSPR.DBF",cSezona,finverse,fda,fnul)
Skloni(SIFPATH,"KBENEF.DBF",cSezona,finverse,fda,fnul)
Skloni(SIFPATH,"VPOSLA.DBF",cSezona,finverse,fda,fnul)
Skloni(SIFPATH,"TIPPR.DBF",cSezona,finverse,fda,fnul)
//if lViseObr
Skloni(SIFPATH,"TIPPR2.DBF",cSezona,finverse,fda,fnul)
//endif
Skloni(SIFPATH,"SIFK.DBF",cSezona,finverse,fda,fnul)
Skloni(SIFPATH,"SIFV.DBF",cSezona,finverse,fda,fnul)
Skloni(SIFPATH,"FMK.INI",cSezona,finverse,fda,fnul)

//sifrarnici
?
?
?
Beep(4)
? "pritisni nesto za nastavak.."

restore screen from cScr
return
*}



/*! \fn *void TDbPorLD::setgaDbfs()
 *  \brief Setuje matricu gaDbfs 
 */
*void TDbPorLD::setgaDbfs()
*{
method setgaDBFs()

public gaDbfs := {;
{ F__LD    ,"_LD"     , P_PRIVPATH    },;
{ F__RADKR ,"_RADKR"  , P_PRIVPATH    },;
{ F__RADN  ,"_RADN"   , P_PRIVPATH    },;
{ F_LDSM   ,"LDSM"    , P_PRIVPATH    },;
{ F_OPSLD  ,"OPSLD"   , P_PRIVPATH    },;
{ F_LD     ,"LD"      , P_KUMPATH     },;
{ F_RADKR  ,"RADKR"   , P_KUMPATH     },;
{ F_RADN   ,"RADN"    , P_KUMPATH     },;
{ F_RJES   ,"RJES"    , P_KUMPATH     },;
{ F_RJ     ,"RJ"      , P_KUMPATH     },;
{ F_POR    ,"POR"     , P_SIFPATH     },;
{ F_DOPR   ,"DOPR"    , P_SIFPATH     },;
{ F_PAROBR ,"PAROBR"  , P_SIFPATH     },;
{ F_TIPPR  ,"TIPPR"   , P_SIFPATH     },;
{ F_TIPPR2 ,"TIPPR2"  , P_SIFPATH     },;
{ F_KRED   ,"KRED"    , P_SIFPATH     },;
{ F_STRSPR ,"STRSPR"  , P_SIFPATH     },;
{ F_KBENEF ,"KBENEF"  , P_SIFPATH     },;
{ F_VPOSLA ,"VPOSLA"  , P_SIFPATH     },;
{ F_BANKE  ,"BANKE"   , P_SIFPATH     };
}

return


/*! \fn *void TDbPorLD::install(string cKorisn,string cSifra,variant p3,variant p4,variant p5,variant p6,variant p7)
 *  \brief osnovni meni za instalacijske procedure
 */

*void TDbPorLD::install(string cKorisn,string cSifra,variant p3,variant p4,variant p5,variant p6,variant p7)
*{

method install(cKorisn,cSifra,p3,p4,p5,p6,p7)
	ISC_START(goModul,.f.)
return
*}

/*! \fn *void TDbLD::kreiraj(int nArea)
 *  \brief kreirane baze podataka LD-a
 */
 
*void TDbPorLD::kreiraj(int nArea)
*{
method kreiraj(nArea)

if (nArea==nil)
	nArea:=-1
endif

if (nArea<>-1)
	CreSystemDb(nArea)
endif

CreFMKSvi()

PRIVATE lViseObr:=.f., lVOBrisiCDX := .f.

TestViseObr()

// ---------------------------------------
// RADN.DBF
// ---------------------------------------

aDbf:={}
AADD(aDBf,{ 'ID'                  , 'C' ,  13 ,  0 })
AADD(aDBf,{ 'NAZ'                 , 'C' ,  20 ,  0 })
AADD(aDBf,{ 'IMEROD'              , 'C' ,  15 ,  0 })
AADD(aDBf,{ 'IME'                 , 'C' ,  15 ,  0 })
AADD(aDBf,{ 'IDSTRSPR'            , 'C' ,   3 ,  0 })
AADD(aDBf,{ 'IDOPSST'             , 'C' ,   4 ,  0 })
AADD(aDBf,{ 'IDOPSRAD'            , 'C' ,   4 ,  0 })

AADD(aDBf,{ 'TIPRADA'             , 'C' ,   1 ,  0 })
AADD(aDBf,{ 'KLO'                 , 'N' ,   5 ,  2 })

AADD(aDBf,{ 'POL'                 , 'C' ,   1 ,  0 })
AADD(aDBf,{ 'MATBR'               , 'C' ,  13 ,  0 })
AADD(aDBf,{ 'DATOD'               , 'D' ,   8 ,  0 })
AADD(aDBf,{ 'brknjiz'             , 'C' ,  12,   0 })
AADD(aDBf,{ 'brtekr'              , 'C' ,  20,   0 })
AADD(aDBf,{ 'Isplata'             , 'C' ,   2,   0 })
AADD(aDBf,{ 'IdBanka'             , 'C' ,   6,   0 })
AADD(aDBf,{ 'K1'                  , 'C' ,   1 ,  0 })
AADD(aDBf,{ 'K2'                  , 'C' ,   1 ,  0 })
AADD(aDBf,{ 'K3'                  , 'C' ,   2 ,  0 })
AADD(aDBf,{ 'K4'                  , 'C' ,   2 ,  0 })
AADD(aDBf,{ 'POL'                 , 'C' ,   1 ,  0 })
AADD(aDBf,{ 'RMJESTO'             , 'C' ,  30 ,  0 })
AADD(aDBf,{ 'POROL'               , 'N' ,   5 ,  2 })
AADD(aDBf,{ 'ULICA'              , 'C' ,  35 ,  0 })
AADD(aDBf,{ 'RBRPIO'             , 'C' ,  14 ,  0 })

if !file(KUMPATH+"RADN.dbf")
   DBCREATE2(KUMPATH+'RADN.DBF',aDbf)
endif

if !file(PRIVPATH+"_RADN.dbf")
   DBCREATE2(PRIVPATH+'_RADN.DBF',aDbf)
endif

CREATE_INDEX("1","id",KUMPATH+"RADN")
CREATE_INDEX("2","naz",KUMPATH+"RADN")


// -------------------------------------------
// RADKR.DBF
// -------------------------------------------
aDbf:={}
AADD(aDBf,{ 'IDRadn'              , 'C' ,  13 ,  0 })
AADD(aDBf,{ 'Mjesec'              , 'N' ,   2 ,  0 })
AADD(aDBf,{ 'Godina'              , 'N' ,   4 ,  0 })
AADD(aDBf,{ 'IdKred'              , 'C' ,  10 ,  0 })
AADD(aDBf,{ 'Iznos'               , 'N' ,  12 ,  2 })
AADD(aDBf,{ 'Placeno'             , 'N' ,  12 ,  2 })
AADD(aDBf,{ 'PMjesec'             , 'N' ,   2 ,  0 })
AADD(aDBf,{ 'PGodina'             , 'N' ,   4 ,  0 })
AADD(aDBf,{ 'NaOsnovu'            , 'C' ,  10 ,  0 })

if !file(KUMPATH+"RADKR.dbf")
   DBCREATE2(KUMPATH+'RADKR.DBF',aDbf)
endif
if !file(PRIVPATH+"_RADKR.dbf")
   DBCREATE2(PRIVPATH+'_RADKR.DBF',aDbf)
endif


CREATE_INDEX("1","str(godina)+str(mjesec)+idradn+idkred+naosnovu",KUMPATH+"RADKR")
CREATE_INDEX("2","idradn+idkred+naosnovu+str(godina)+str(mjesec)",KUMPATH+"RADKR")
CREATE_INDEX("3","idkred+naosnovu+idradn+str(godina)+str(mjesec)",KUMPATH+"RADKR")

// radi POR-LD
CREATE_INDEX("4","naosnovu",KUMPATH+"RADKR")
CREATE_INDEX("PGM","idradn+str(pgodina)+str(pmjesec)",KUMPATH+"RADKR")
CREATE_INDEX("PGM2","str(pgodina)+str(pmjesec)",KUMPATH+"RADKR")


// ----------------------------------------------------
// RJES.DBF
// ----------------------------------------------------

aDbf:={}
AADD(aDBf,{ 'NaOsnovu'             , 'C' ,  10 ,  0 })
// varijanta -1 standardno
// varijanta -2 starateljstvo
AADD(aDBf,{ 'IDRADN'               , 'C' ,  13 ,  0 })
AADD(aDBf,{ 'Varijanta'            , 'C' ,   1 ,  0 })
AADD(aDBf,{ 'Datum'                , 'D' ,   8 ,  0 })
AADD(aDBf,{ 'DatPodn'              , 'D' ,   8 ,  0 })
AADD(aDBf,{ 'DatPPra'              , 'D' ,   8 ,  0 })
// datum podnoçenja zahtjeva
AADD(aDBf,{ 'DatKPra'              , 'D' ,   8 ,  0 })
AADD(aDBf,{ 'DatRodj'              , 'D' ,   8 ,  0 })
AADD(aDBf,{ 'DatZapos'             , 'D' ,   8 ,  0 })
AADD(aDBf,{ 'PREZDJETE'            , 'C' ,  20 ,  0 })
AADD(aDBf,{ 'IMEDJETE'             , 'C' ,  15 ,  0 })
AADD(aDBf,{ 'POL'                  , 'C' ,   1 ,  0 })
// prosjecna plata
AADD(aDBf,{ 'dokazi'               , 'M' ,  10 ,  0 })
AADD(aDBf,{ 'PREKBroj'             , 'C' ,  10 ,  0 })
AADD(aDBf,{ 'PREKDatRj'            , 'D' ,   8 ,  0 })  // datum rjesenja prekida
AADD(aDBf,{ 'PREKDatPoc'           , 'D' ,   8 ,  0 })  // datum pocetka prestanka prava
AADD(aDBf,{ 'RazlPrek'             , 'M' ,  10 ,  0 })
if !file(KUMPATH+"RJES.DBF")
   DBCREATE2(KUMPATH+'RJES.DBF',aDbf)
endif
CREATE_INDEX("NAOSNOVU","NAOSNOVU+IDRADN",KUMPATH+"RJES")
CREATE_INDEX("PREKBROJ","PREKBROJ+IDRADN",KUMPATH+"RJES")


if !file(KUMPATH+"REKLD.DBF")
  aDbf:={  {"GODINA"     ,  "C" ,  4, 0} ,;
           {"MJESEC"     ,  "C" ,  2, 0} ,;
           {"ID"         ,  "C" , 30, 0} ,;
           {"opis"       ,  "C" , 20, 0} ,;
           {"iznos1"     ,  "N" , 18, 4} ,;
           {"iznos2"     ,  "N" , 18, 4} ;
          }
  AADD( aDbf , {"idpartner"  ,  "C" , 10, 0} )
  DBCREATE2(KUMPATH+"REKLD.DBF",aDbf)
endif

CREATE_INDEX("1","godina+mjesec+id",KUMPATH+"REKLD")


if !file(PRIVPATH+"OPSLD.DBF")
  aDbf:={   {"ID"    , "C" ,  1, 0},;
            {"IDOPS" , "C" ,  4, 0},;
            {"IZNOS" , "N" , 18, 4},;
            {"IZNOS2", "N" , 18, 4},;
            {"LJUDI" , "N" ,  4, 0};
          }
  DBCREATE2(PRIVPATH+"OPSLD.DBF",aDbf)
endif

CREATE_INDEX("1","id+idops",PRIVPATH+"OPSLD")

// -------------------------------------------
// PAROBR.DBF
// -------------------------------------------
if !file(SIFPATH+"PAROBR.DBF")
   aDBf:={}
   AADD(aDBf,{ 'ID'                  , 'C' ,   2 ,  0 })  // mjesec
   AADD(aDBf,{ 'NAZ'                 , 'C' ,  10 ,  0 })
   AADD(aDBf,{ 'IDRJ'                , 'C' ,   2 ,  0 })
   AADD(aDBf,{ 'VrBod'               , 'N' ,  15 ,  5 })
   AADD(aDBf,{ 'K1'                  , 'N' ,  11 ,  6 })
   AADD(aDBf,{ 'K2'                  , 'N' ,  11 ,  6 })
   AADD(aDBf,{ 'K3'                  , 'N' ,   9 ,  5 })
   AADD(aDBf,{ 'K4'                  , 'N' ,   6 ,  3 })
   AADD(aDBf,{ 'K5'                  , 'N' ,  11 ,  6 })
   AADD(aDBf,{ 'K6'                  , 'N' ,  11 ,  6 })
   AADD(aDBf,{ 'PROSLD'              , 'N' ,  12 ,  2 })
   AADD(aDBf,{ 'MINLD'               , 'N' ,  12 ,  2 })
   DBCREATE2(SIFPATH+'PAROBR.DBF',aDbf)
endif

IF lVOBrisiCDX
  DelSve("PAROBR.CDX",trim(cDirSif))
ENDIF
IF lViseObr
  CREATE_INDEX("ID","id+obr",SIFPATH+"PAROBR")
ELSE
  CREATE_INDEX("ID","id",SIFPATH+"PAROBR")
ENDIF


// ---------------------------------------------
// tipovi primanja
// ---------------------------------------------
aDBf:={}
AADD(aDBf,{ 'ID'                  , 'C' ,   2 ,  0 })
AADD(aDBf,{ 'NAZ'                 , 'C' ,  20 ,  0 })
AADD(aDBf,{ 'Aktivan'             , 'C' ,   1 ,  0 })
AADD(aDBf,{ 'Fiksan'              , 'C' ,   1 ,  0 })
AADD(aDBf,{ 'UFS'                 , 'C' ,   1 ,  0 })  // u fond sati
AADD(aDBf,{ 'UNeto'               , 'C' ,   1 ,  0 })
AADD(aDBf,{ 'Koef1'               , 'N' ,   5 ,  2 })
AADD(aDBf,{ 'Formula'             , 'C' , 200 ,  0 })
AADD(aDBf,{ 'OPIS'                , 'C' ,   8 ,  0 })

if !file(SIFPATH+"TIPPR.DBF")
   DBCREATE2(SIFPATH+'TIPPR.DBF',aDbf)
endif
CREATE_INDEX("ID","id",SIFPATH+"TIPPR")

if !file(SIFPATH+"TIPPR2.DBF")
   DBCREATE2(SIFPATH+'TIPPR2.DBF',aDbf)
endif
CREATE_INDEX("ID","id",SIFPATH+"TIPPR2")

// -----------------------------------------------
// RJ.DBF
// -----------------------------------------------
if !file(KUMPATH+"RJ.DBF")
   aDBf:={}
   AADD(aDBf,{ 'ID'                  , 'C' ,   2 ,  0 })
   AADD(aDBf,{ 'NAZ'                 , 'C' ,  35 ,  0 })
   DBCREATE2(KUMPATH+'RJ.DBF',aDbf)
endif
CREATE_INDEX("ID","id",KUMPATH+"RJ")

// -----------------------------------------------
// kreditori 
// -----------------------------------------------
aDBf:={}
AADD(aDBf,{ 'ID'                  , 'C' ,  10 ,  0 })
AADD(aDBf,{ 'NAZ'                 , 'C' ,  30 ,  0 })
AADD(aDBf,{ 'ZIRO'                , 'C' ,  20 ,  0 })
AADD(aDBf,{ 'ZIROD'               , 'C' ,  20 ,  0 })
AADD(aDBf,{ 'TELEFON'             , 'C' ,  20 ,  0 })
AADD(aDBf,{ 'ADRESA'              , 'C' ,  30 ,  0 })
if !file(SIFPATH+"KRED.DBF")
   DBCREATE2(SIFPATH+'KRED.DBF',aDbf)
endif
if !file(PRIVPATH+"_KRED.DBF")
   DBCREATE2(PRIVPATH+'_KRED.DBF',aDbf)
endif
CREATE_INDEX("ID","id",SIFPATH+"KRED")
CREATE_INDEX("NAZ","naz",SIFPATH+"KRED")

// ----------------------------------------------------
// OPS.DBF
// ----------------------------------------------------
if !file(SIFPATH+"OPS.DBF")
   aDBf:={}
   AADD(aDBf,{ 'ID'                  , 'C' ,   4 ,  0 })
   AADD(aDBf,{ 'IDJ'                 , 'C' ,   3 ,  0 })
   AADD(aDBf,{ 'IDKAN'               , 'C' ,   2 ,  0 })
   AADD(aDBf,{ 'IDN0'                , 'C' ,   1 ,  0 })
   AADD(aDBf,{ 'NAZ'                 , 'C' ,  20 ,  0 })
   DBCREATE2(SIFPATH+'OPS.DBF',aDbf)
endif

CREATE_INDEX("ID","id",SIFPATH+"OPS")
CREATE_INDEX("IDJ","idj",SIFPATH+"OPS")
CREATE_INDEX("IDKAN","idkan",SIFPATH+"OPS")
CREATE_INDEX("IDN0","idN0",SIFPATH+"OPS")
CREATE_INDEX("NAZ","naz",SIFPATH+"OPS")

// ----------------------------------------------
// POR.DBF
// ----------------------------------------------
if !file(SIFPATH+"POR.DBF")
   aDBf:={}
   AADD(aDBf,{ 'ID'                  , 'C' ,   2 ,  0 })
   AADD(aDBf,{ 'NAZ'                 , 'C' ,  20 ,  0 })
   AADD(aDBf,{ 'IZNOS'               , 'N' ,   5 ,  2 })
   AADD(aDBf,{ 'DLIMIT'              , 'N' ,  12 ,  2 })
   AADD(aDBf,{ 'POOPST'              , 'C' ,   1 ,  0 })
   AADD(aDBf,{ 'PORTIP'              , 'C' ,   1 ,  0 })
   DBCREATE2(SIFPATH+'POR.DBF',aDbf)
endif

CREATE_INDEX("ID","id",SIFPATH+"POR")

// ------------------------------------------
// DOPR.DBF
// ------------------------------------------
if !file(SIFPATH+"DOPR.DBF")
   aDBf:={}
   AADD(aDBf,{ 'ID'                  , 'C' ,   2 ,  0 })
   AADD(aDBf,{ 'NAZ'                 , 'C' ,  20 ,  0 })
   AADD(aDBf,{ 'IZNOS'               , 'N' ,   5 ,  2 })
   AADD(aDBf,{ 'IdKBenef'            , 'C' ,   1 ,  0 })
   AADD(aDBf,{ 'DLIMIT'              , 'N' ,  12 ,  2 })
   AADD(aDBf,{ 'POOPST'              , 'C' ,   1 ,  0 })
   DBCREATE2(SIFPATH+'DOPR.DBF',aDbf)
endif

CREATE_INDEX("ID","id",SIFPATH+"DOPR")

// ---------------------------------------------
// LD.DBF
// ---------------------------------------------
aDBf:={}
AADD(aDBf,{ 'Godina'              , 'N' ,   4 ,  0 })
AADD(aDBf,{ 'IDRJ'                , 'C' ,   2 ,  0 })
AADD(aDBf,{ 'IDRADN'              , 'C' ,   13 ,  0 })
AADD(aDBf,{ 'Mjesec'              , 'N' ,   2 ,  0 })
AADD(aDBf,{ 'BRBOD'               , 'N' ,  11 ,  2 })
AADD(aDBf,{ 'IdStrSpr'            , 'C' ,   3 ,  0 })
AADD(aDBf,{ 'IdVPosla'            , 'C' ,   2 ,  0 })
AADD(aDBf,{ 'KMinRad'             , 'N' ,   5 ,  2 })
AADD(aDBf,{ 'S01'                 , 'N' ,   6 ,  2 })
AADD(aDBf,{ 'I01'                 , 'N' ,  12 ,  2 })
AADD(aDBf,{ 'S02'                 , 'N' ,   6 ,  2 })
AADD(aDBf,{ 'I02'                 , 'N' ,  12 ,  2 })
AADD(aDBf,{ 'S03'                 , 'N' ,   6 ,  2 })
AADD(aDBf,{ 'I03'                 , 'N' ,  12 ,  2 })
AADD(aDBf,{ 'S04'                 , 'N' ,   6 ,  2 })
AADD(aDBf,{ 'I04'                 , 'N' ,  12 ,  2 })
AADD(aDBf,{ 'S05'                 , 'N' ,   6 ,  2 })
AADD(aDBf,{ 'I05'                 , 'N' ,  12 ,  2 })
AADD(aDBf,{ 'S06'                 , 'N' ,   6 ,  2 })
AADD(aDBf,{ 'I06'                 , 'N' ,  12 ,  2 })
AADD(aDBf,{ 'S07'                 , 'N' ,   6 ,  2 })
AADD(aDBf,{ 'I07'                 , 'N' ,  12 ,  2 })
AADD(aDBf,{ 'S08'                 , 'N' ,   6 ,  2 })
AADD(aDBf,{ 'I08'                 , 'N' ,  12 ,  2 })
AADD(aDBf,{ 'S09'                 , 'N' ,   6 ,  2 })
AADD(aDBf,{ 'I09'                 , 'N' ,  12 ,  2 })
AADD(aDBf,{ 'S10'                 , 'N' ,   6 ,  2 })
AADD(aDBf,{ 'I10'                 , 'N' ,  12 ,  2 })
AADD(aDBf,{ 'S11'                 , 'N' ,   6 ,  2 })
AADD(aDBf,{ 'I11'                 , 'N' ,  12 ,  2 })
AADD(aDBf,{ 'S12'                 , 'N' ,   6 ,  2 })
AADD(aDBf,{ 'I12'                 , 'N' ,  12 ,  2 })
AADD(aDBf,{ 'S13'                 , 'N' ,   6 ,  2 })
AADD(aDBf,{ 'I13'                 , 'N' ,  12 ,  2 })
AADD(aDBf,{ 'S14'                 , 'N' ,   6 ,  2 })
AADD(aDBf,{ 'I14'                 , 'N' ,  12 ,  2 })
AADD(aDBf,{ 'IDKRED'              , 'C' ,  10 ,  0 })
AADD(aDBf,{ 'USATI'               , 'N' ,   8 ,  1 })
AADD(aDBf,{ 'UNETO'               , 'N' ,  13 ,  2 })
AADD(aDBf,{ 'UODBICI'             , 'N' ,  13 ,  2 })
AADD(aDBf,{ 'UIZNOS'              , 'N' ,  13 ,  2 })
AADD(aDBf,{ 'UNETO2'              , 'N' ,  13 ,  2 })

if !file(KUMPATH+'LD.DBF')
 DBCREATE2(KUMPATH+'LD.DBF',aDbf)
endif

IF lVOBrisiCDX
  DelSve("LD.CDX",trim(cDirRad))
ENDIF

IF lViseObr
  // polje OBR koristimo u indeksima
  CREATE_INDEX("1","str(godina)+idrj+str(mjesec)+obr+idradn",KUMPATH+"LD")
  CREATE_INDEX("2","str(godina)+str(mjesec)+obr+idradn+idrj",KUMPATH+"LD")
  CREATE_INDEX("3","str(godina)+idrj+idradn",KUMPATH+"LD")
  CREATE_INDEX("4","str(godina)+idradn+str(mjesec)+obr",KUMPATH+"LD")
  CREATE_INDEX("1U","str(godina)+idrj+str(mjesec)+idradn",KUMPATH+"LD")
  CREATE_INDEX("2U","str(godina)+str(mjesec)+idradn+idrj",KUMPATH+"LD")
ELSE
  // standardno: ne postoji polje OBR
  CREATE_INDEX("1","str(godina)+idrj+str(mjesec)+idradn",KUMPATH+"LD")
  CREATE_INDEX("2","str(godina)+str(mjesec)+idradn+idrj",KUMPATH+"LD")
  CREATE_INDEX("3","str(godina)+idrj+idradn",KUMPATH+"LD")
  CREATE_INDEX("4","str(godina)+idradn+str(mjesec)",KUMPATH+"LD")
ENDIF

CREATE_INDEX("RADN","idradn",KUMPATH+"LD")

if !file(KUMPATH+"LDNO.DBF")
    AADD(aDBf, { "RAZLOG","C",20,0 } )      // razlog neisplacivanja
    DBCREATE2(KUMPATH+"LDNO.DBF",aDbf)
    ASIZE(aDbf,LEN(aDbf)-1)
endif
CREATE_INDEX("1","str(godina)+idrj+str(mjesec)+idradn",KUMPATH+"LDNO")
CREATE_INDEX("2","str(godina)+str(mjesec)+idradn+idrj",KUMPATH+"LDNO")
CREATE_INDEX("3","str(godina)+idrj+idradn",KUMPATH+"LDNO")
CREATE_INDEX("4","str(godina)+idradn+str(mjesec)",KUMPATH+"LDNO")

if !file(PRIVPATH+"LDSM.DBF")
   AADD(aDBf, { "Obr","C",1,0 } )      // obracun
   DBCREATE2(PRIVPATH+"LDSM.DBF",aDbf)
endif


CREATE_INDEX("1","Obr+str(godina)+str(mjesec)+idradn+idrj",PRIVPATH+"LDSM")
CREATE_INDEX("RADN","idradn",PRIVPATH+"LDSM")

if !file(PRIVPATH+"_LD.DBF")
   DBCREATE2(PRIVPATH+"_LD.DBF",aDbf)
endif


if !file(SIFPATH+"STRSPR.DBF")
    aDbf:={ {"id","C",3,0} ,;
            {"naz","C",20,0} ,;
            {"naz2","C",6,0} ;
                }
     DBCREATE2(SIFPATH+"STRSPR.DBF",aDbf)
endif


CREATE_INDEX("ID","id",SIFPATH+"strspr")

if !file(SIFPATH+"KBENEF.DBF")
   aDbf:={ {"id","C",1,0} ,;
           {"naz","C",8,0} ,;
           {"iznos","N",5,2} ;
         }
   DBCREATE2(SIFPATH+"KBENEF",aDbf)
endif
CREATE_INDEX("ID","id",SIFPATH+"KBENEF")


if !file(SIFPATH+"VPOSLA.DBF")  // vrste posla
   aDbf:={  {"id","C",2,0}   ,;
            {"naz","C",20,0} ,;
            {"idkbenef","C",1,0} ;
         }
   DBCREATE2(SIFPATH+"VPOSLA",aDbf)
endif
CREATE_INDEX("ID","id",SIFPATH+"VPOSLA")

if !file(PRIVPATH+"PRIPNO.DBF")
   aDbf:={ { "MJESEC"    , "N" ,  2 ,  0 } ,;
           { "GODINA"    , "N" ,  4 ,  0 } ,;
           { "IDRADN"    , "C" , 13 ,  0 } ,;
           { "IDRJ"      , "C" ,  2 ,  0 } ,;
           { "RBR"       , "C" ,  4 ,  0 } ,;
           { "RAZLOG"    , "C" , 20 ,  0 } ;
         }
    DBCREATE2(PRIVPATH+"PRIPNO.DBF",aDbf)
endif

// ---------------------------------------------
// BANKE.DBF
// ---------------------------------------------
if !file(SIFPATH+"BANKE.DBF")
        aDbf:={}
        AADD(aDBf,{ 'ID'                  , 'C' ,   3 ,  0 })
        AADD(aDBf,{ 'NAZ'                 , 'C' ,  45 ,  0 })
        AADD(aDBf,{ 'Mjesto'              , 'C' ,  20 ,  0 })
        DBCREATE2(SIFPATH+'BANKE.DBF',aDbf)
endif

CREATE_INDEX("ID","id", SIFPATH+"BANKE")
CREATE_INDEX("NAZ","naz", SIFPATH+"BANKE")


return



/*! \fn *void TDbLD::obaza(int i)
 *  \brief otvara odgovarajucu tabelu
 *  
 *      
 */

*void TDbLD::obaza(int i)
*{

method obaza(i)
local lIdIDalje
local cDbfName

lIdiDalje:=.f.

//altd()

if i==F__LD .or. i==F__RADN .or. i==F__RADKR .or. i==F_LDSM .or. i==F_OPSLD 
	lIdiDalje:=.t.
endif

if i==F_LD .or. i=F_RADN .or. i==F_RADKR .or. i==F_RJ .or. i==F_RJES  
	lIdiDalje:=.t.
endif

if i==F_POR .or. i==F_DOPR .or. i==F_PAROBR .or. i==F_TIPPR .or. i==F_TIPPR2 .or. i==F_KRED .or. i==F_STRSPR .or. i==F_KBENEF .or. i==F_VPOSLA .or. i==F_BANKE
	lIdiDalje := .t.
endif

if (gSecurity=="D" .and. (i==175 .or. i==176 .or. i==177 .or. i==178 .or. I==179))
	lIdiDalje := .t.
endif

if lIdiDalje
	cDbfName:=DBFName(i,.t.)
	if gAppSrv 
		? "OPEN: " + cDbfName + ".DBF"
		if !File(cDbfName + ".DBF")
			? "Fajl " + cDbfName + ".dbf ne postoji!!!"
			use
			return
		endif
	endif
	
	select (i)
	usex (cDbfName)
else
	use
	return
endif


return

/*! \fn *void TDbLD::ostalef()
 *  \brief Ostalef funkcije (bivsi install modul)
 *  \note  sifra: SIGMAXXX
*/

*void TDbLD::ostalef()
*{
method ostalef()

return
*}

/*! \fn *void TDbLD::konvZn()
 *  \brief koverzija 7->8 baze podataka LD
 */
 
*void TDbLD::konvZn()
*{
method konvZn() 
local cIz:="7"
local cU:="8"
local aPriv:={}
local aKum:={}
local aSif:={}
local GetList:={}
local cSif:="D"
local cKum:="D"
local cPriv:="D"

private aKonvZN:={}

if !gAppSrv	
	if !SigmaSif("KZ      ")
		return
	endif

	Box(,8,50)
	@ m_x+2, m_y+2 SAY "Trenutni standard (7/8)        " GET cIz   VALID   cIz$"78B"  PICT "@!"
  	@ m_x+3, m_y+2 SAY "Konvertovati u standard (7/8/A)" GET cU    VALID    cU$"78AB" PICT "@!"
  	@ m_x+5, m_y+2 SAY "Konvertovati sifrarnike (D/N)  " GET cSif  VALID  cSif$"DN"  PICT "@!"
  	@ m_x+6, m_y+2 SAY "Konvertovati radne baze (D/N)  " GET cKum  VALID  cKum$"DN"  PICT "@!"
  	@ m_x+7, m_y+2 SAY "Konvertovati priv.baze  (D/N)  " GET cPriv VALID cPriv$"DN"  PICT "@!"
  	read
  	if LastKey()==K_ESC
		BoxC()
		return
	endif
  	if Pitanje(,"Jeste li sigurni da zelite izvrsiti konverziju (D/N)","N")=="N"
    		BoxC()
		return
  	endif
	BoxC()
else
	?
	cKonvertTo:=IzFmkIni("FMK","KonvertTo","78",EXEPATH)
	
	if cKonvertTo=="78"
		cIz:="7"
		cU:="8"
		? "Trenutni standard: " + cIz
		? "Konvertovati u: " + cU 
	elseif cKonvertTo=="87"
		cIz:="8"
		cU:="7"
		? "Trenutni standard: " + cIz
		? "Konvertovati u: " + cU 
	else // pitaj
		?
		@ 10, 2 SAY "Trenutni standard (7/8)        " GET cIz VALID cIz$"78B" PICT "@!"
		?
		@ 11, 2 SAY "Konvertovati u standard (7/8/A)" GET cU VALID cU$"78AB" PICT "@!"
		read
	endif
	cSif:="D"
	cKum:="D"
	cPriv:="D"
endif

aPriv:= {}
aKum:= {F_LD, F_RADKR, F_RADN, F_RJ, F_RJES}
aSif:={F_PAROBR, F_TIPPR, F_TIPPR, F_STRSPR, F_KBENEF, F_VPOSLA, F_OPS, F_POR, F_DOPR, F_RJ, F_KRED, F_LDSM }

if cSif=="N"
	aSif:={}
endif

if cKum=="N"
	aKum:={}
endif

if cPriv=="N"
	aPriv:={}
endif

private aSifRev:={}
//
if cU=="B" .or. cIz=="B" 
	KZNBaza(aPriv, aKum, aSif, cIz, cU, "B")
else
	KZNBaza(aPriv, aKum, aSif, cIz, cU)
endif

// Odstampaj rezultate zamjene sifara
START PRINT CRET
? "Stanje zamjene sifara: Obracun plata"
?
? "--------------------------------------------------------"
? "RADNICI: "
? "Stara sifra  -  Nova sifra  -  Ime i prezime radnika"
? "--------------------------------------------------------"
O_RADN
for i:=1 to LEN(aKonvZN)
	select radn
	set order to tag "1"
	seek aKonvZN[i, 2]
	
	? aKonvZN[i, 1] + "       -   " + aKonvZN[i, 2] + "     -  " + ALLTRIM(radn->ime) + " " + ALLTRIM(radn->naz) 
next

?

FF
END PRINT

return


