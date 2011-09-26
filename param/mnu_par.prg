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


#include "porld.ch"


function mnu_param()
private opc:={}
private opcexe:={}
private izbor:=1
O_RJ
O_PARAMS

AADD(opc, "1. naziv firme, RJ, mjesec, godina...                           ")
if (ImaPravoPristupa(goModul:oDatabase:cName,"PARAM","SETFIRMA"))
	AADD(opcexe, {|| SetFirma()})
else
	AADD(opcexe, {|| MsgBeep(cZabrana)})
endif

AADD(opc, "2. postavka zaokruzenja, valute, formata prikaza iznosa...      ")
if (ImaPravoPristupa(goModul:oDatabase:cName,"PARAM","SETFORMA"))
	AADD(opcexe, {|| SetForma()})
else
	AADD(opcexe, {|| MsgBeep(cZabrana)})
endif

AADD(opc, "3. postavka nacina obracuna ")
if (ImaPravoPristupa(goModul:oDatabase:cName,"PARAM","SETOBRACUN"))
	AADD(opcexe, {|| SetObracun()})
else
	AADD(opcexe, {|| MsgBeep(cZabrana)})
endif

AADD(opc, "4. postavka formula (uk.prim.,uk.sati,godisnji) i koeficijenata ")
if (ImaPravoPristupa(goModul:oDatabase:cName,"PARAM","SETFORMULE"))
	AADD(opcexe, {|| SetFormule()})
else
	AADD(opcexe, {|| MsgBeep(cZabrana)})
endif

AADD(opc, "5. postavka parametara izgleda dokumenata ")
if (ImaPravoPristupa(goModul:oDatabase:cName,"PARAM","SETPRIKAZ"))
	AADD(opcexe, {|| SetPrikaz()})
else
	AADD(opcexe, {|| MsgBeep(cZabrana)})
endif

AADD(opc, "6. parametri - razno ")
if (ImaPravoPristupa(goModul:oDatabase:cName,"PARAM","SETRAZNO"))
	AADD(opcexe, {|| SetRazno()})
else
	AADD(opcexe, {|| MsgBeep(cZabrana)})
endif

AADD(opc, "7. postavka specificnih parametara modula ")
if (ImaPravoPristupa(goModul:oDatabase:cName,"PARAM","SETSPECIF"))
	AADD(opcexe, {|| SetPorLD()})
else
	AADD(opcexe, {|| MsgBeep(cZabrana)})
endif


Menu_SC("par")

return

