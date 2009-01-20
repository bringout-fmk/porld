#ifdef CPOR
  #define _LR_  13
#else
  #define _LR_   6
#endif

#ifdef CPOR
  #define _LK_  10
#else
  #define _LK_   6
#endif

#define F_RADN     1
#define F_PAROBR   2
#define F_TIPPR    3
#define F_LD       4
#define F_POR      6
#define F_DOPR     7
#define F_RJ       8
#define F_STRSPR   9
#define F_VPOSLA   10
#define F_KBENEF   11
#define F_OPS      12
#define F_KRED     13
#define F_RADKR    14
#define F_LDSM    15
#define F__RADN    16
#define F__LD      17
#define F_REKLD    20
#define F__RADKR   21
#define F__KRED    22
#define F_OPSLD    95

#ifdef CPOR
 #define F_PRIPNO  23
 #define F_LDNO    24
 #define F_RJES    25

 #xcommand O_LDNO   => select (F_LDNO)  ; usex (KUMPATH+"LDNO"); set order to 1
 #xcommand O_PRIPNO => select (F_PRIPNO); usex (PRIVPATH+"PRIPNO")
 #xcommand O_RJES => select (F_RJES); usex (KUMPATH+"RJES"); set order to tag "NAOSNOVU"
#endif

#define F_NORSIHT 23
#define F_TPRSIHT 24
#define F_RADSIHT 25
#define F_TIPPR2  26

#define F_BANKE   27

#xcommand O_RADN    => OKumul(F_RADN, KUMPATH,"RADN",1); set order to 1

#xcommand O_TPRSIHT   => OKumul(F_TPRSIHT, KUMPATH,"TPRSIHT",1); set order to tag "ID"
#xcommand O_NORSIHT   => OKumul(F_NORSIHT, KUMPATH,"NORSIHT",1); set order to tag "ID"
#xcommand O_RADSIHT   => OKumul(F_RADSIHT, KUMPATH,"RADSIHT",1); set order to tag "1"


#xcommand O__RADN    => select (F__RADN);  usex (PRIVPATH+"_RADN")
#xcommand O_RADKR    => OKumul(F_RADKR, KUMPATH,"RADKR",1); set order to  1
#xcommand O_RADKRX   => select (F_RADKR);  usex (KUMPATH+"RADKR") ; set order to  0
#xcommand O__RADKR   => select (F__RADKR);    use (PRIVPATH+"_RADKR")
#xcommand O_LD      => OKumul(F_LD, KUMPATH, "LD",1)   ; set order to 1
#xcommand O_LDX      => select (F_LD);    usex (KUMPATH+"LD") ; set order to 1
#xcommand O__LD     => select (F__LD);    usex (PRIVPATH+"_LD")
#xcommand O_LDSM    => select (F_LDSM);   use (PRIVPATH+"LDSM") ; set order to 1
#xcommand O_LDSMX   => select (F_LDSM);   usex (PRIVPATH+"LDSM") ; set order to 0
#xcommand O_OPSLD  => select 95; usex (PRIVPATH+"opsld") ; set order to 1
#xcommand O_REKLD0 => select (F_REKLD); usex (KUMPATH+"rekld")
#xcommand O_REKLD  => select (F_REKLD); usex (KUMPATH+"rekld") ; set order to 1

#xcommand O_RJ   => select (F_RJ); use  (KUMPATH+"RJ") ; set order to tag "ID"
#xcommand O_KBENEF => select (F_KBENEF); use (SIFPATH+"KBENEF")  ;set order to tag "ID"
#xcommand O_POR   => select (F_POR); use  (SIFPATH+"POR")  ; set order to tag "ID"
#xcommand O_DOPR   => select (F_DOPR); use  (SIFPATH+"DOPR") ; set order to tag "ID"
#xcommand O_OPS   => select (F_OPS); use  (SIFPATH+"OPS")  ; set order to tag "ID"
#xcommand O_KRED  => select (F_KRED); use  (SIFPATH+"KRED")  ; set order to tag "ID"
#xcommand O__KRED => select (F__KRED); use  (PRIVPATH+"_KRED") ; set order to tag "ID"
#xcommand O_STRSPR => select (F_STRSPR); use  (SIFPATH+"STRSPR") ; set order to tag "ID"
#xcommand O_VPOSLA => select (F_VPOSLA); use  (SIFPATH+"VPOSLA")  ; set order to tag "ID"
#xcommand O_PAROBR  => select (F_PAROBR);  use (SIFPATH+"PAROBR") ; set order to tag "ID"
#xcommand O_TIPPR   => select (F_TIPPR);   use (SIFPATH+"TIPPR") ; set order to tag "ID"
#xcommand O_TIPPR2  => select (F_TIPPR2);  use (SIFPATH+"TIPPR2") ; set order to tag "ID"

#xcommand O_TIPPRN  => IF cObracun<>"1".and.!EMPTY(cObracun);
                      ;  select (F_TIPPR2)                  ;
                      ;  use (SIFPATH+"TIPPR2") alias TIPPR ;
                      ;  set order to tag "ID"              ;
                      ;ELSE                                 ;
                      ;  select (F_TIPPR)                   ;
                      ;  use (SIFPATH+"TIPPR")              ;
                      ;  set order to tag "ID"              ;
                      ;ENDIF


#xcommand O_BANKE   => select (F_BANKE) ; use  (SIFPATH+"BANKE")  ; set order to tag "ID"


