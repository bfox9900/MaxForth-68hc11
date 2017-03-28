( 0C!.ASM  clear a byte address )

 CODE 0C! ( adr -- )
      186F , 0 C,
      7E C, FC97 ,
 END-CODE

( CODE 0C!
(       TOP 1+ LDX,
(       0 ,X CLR,
(       POP JMP,
( END-CODE

