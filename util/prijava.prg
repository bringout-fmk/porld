#include "porld.ch"


// ----------------------------------------
// funkcija za prijavu u obracun
// ----------------------------------------
function ParObracun()
local nX := 1
local nPadL := 20

O_RJ
O_PARAMS

select rj

Box(, 4 +IF(lViseObr, 1, 0), 50)
	
	set cursor on
	
	@ m_x + nX, m_y + 2 SAY PADL( "Radna jedinica", nPadL ) + " " + gRj 
	
	++nX

	@ m_x + nX, m_y + 2 SAY PADL( "Mjesec", nPadL ) GET gMjesec pict "99"
 	
	++nX
	
	@ m_x + nX, m_y + 2 SAY PADL( "Godina", nPadL ) GET gGodina pict "9999"
 	
	++nX

	@ m_x + nX, m_y + 2 SAY PADL( "Varijanta obracuna", nPadL ) GET gVarObracun

	if lViseObr
		
		++nX
   		
		@ m_x + nX, m_y + 2 SAY PADL( "Obracun", nPadL ) GET gObracun ;
			WHEN HelpObr(.f.,gObracun) VALID ValObr(.f.,gObracun)
	
	endif
 	
	read
 	
	clvbox()
	
BoxC()

if (LASTKEY()<>K_ESC)

	select params
 	
	Wpar("rj",@gRJ)
 	Wpar("mj",@gMjesec)
 	Wpar("go",@gGodina)
 	Wpar("ob",@gObracun)
 	Wpar("ov",@gVarObracun)
 	
	select params
	use

endif

return


