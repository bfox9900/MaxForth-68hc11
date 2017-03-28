( INDICES.MAX   a faster way to access integer arrays 14FEB93 Fox)
HEX
FORTH DEFINITIONS

CODE I@ ( i adr -- n)
18 C, EC C,
 0 C, 18 C,
 8 C, 18 C,
 8 C, 18 C,
68 C,  1 C,
18 C, 69 C,
 0 C, 18 C,
E3 C,  0 C,
18 C, ED C,
 0 C, CD C,
EE C,  0 C,
EC C,  0 C,
7E C,  FE47 ,
END-CODE

CODE I! ( n i adr --)
18 C, EC C,
 0 C, 18 C,
 8 C, 18 C,
 8 C, 18 C,
68 C,  1 C,
18 C, 69 C,
 0 C, 18 C,
E3 C,  0 C,
18 C, ED C,
 0 C, CD C,
EE C,  0 C,
18 C,  8 C,
18 C,  8 C,
18 C, EC C,
 0 C, ED C,
 0 C,
7E C, FE4A ,
END-CODE

 CODE I!  ( n i adr -- )
      TOP    LDD,  INY, INY,      ( POP address into D )
      TOP 1+ ASL,      (  same as FORTH 2* )
      TOP    ROL,      (  "        "    "  )
      TOP    ADDD,     ( add address to index in D reg )
      TOP    STD,      ( put result back on top)
      TOP    LDX,      ( POP that into X )
      SEC    LDD,
      0 ,X   STD,      ( store D value at address  X+0  )
      POPTWO JMP,      ( return to FORTH )
 END-CODE

 CODE I@  ( i adr -- n )
      TOP    LDD,  INY, INY,      ( pop ADDRESS into D )
      TOP 1+ ASL,      ( ......... same as FORTH 2* )
      TOP    ROL,
      TOP    ADDD,     ( add address to index in D reg )
      TOP    STD,      ( put result back on top)
      TOP    LDX,      ( put that into X )
      0 ,X   LDD,      ( fetch value at address  X+0  )
      PUT    JMP,      ( put D on TOP
 END-CODE

( * character oriented indexing )

( CODE IC!  ( n i adr -- )
(      TOP    LDD,  INY, INY,      ( POP address into D )
(      TOP    ADDD,     ( add address to index in D reg )
(      TOP    STD,      ( put result back on top)
(      TOP    LDX,      ( POP that into X )
(      TOP    LDD,
(      0 ,X   STD,      ( store D value at address  X+0  )
(      POP    JMP,      ( return to FORTH )
( END-CODE



