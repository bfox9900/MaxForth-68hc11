( +CFETCH  to speed-up translation tables )
HEX
( CODE +C@
(   18 C, EC C,  0 C,  18 C, E3 C,  2 C,
(   18 C, ED C,  2 C,  CD C, EE C,  2 C,
(   EC C,  0 C,
(   18 C, ED C,  2 C,
(   7E C, FC97 ,
( END-CODE

 CODE +C@  ( i adr -- n )
      TOP    LDD,      ( pop ADDRESS into D )
      SEC    ADDD,     ( add address to index in D reg )
      SEC    STD,      ( put result back on top)
      SEC    LDX,      ( put that into X )
      0 ,X   LDD,      ( fetch value at address  X+0  )
      SEC    STD,
      POP    JMP,      ( put D on TOP
 END-CODE
DECIMAL

