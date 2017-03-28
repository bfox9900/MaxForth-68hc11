( CARRAY.ASM  a faster way to access integer arrays 14FEB93 Fox)



( * Not for use in ROM based systems see file:  RAM-ARRAY.ASM *)
HEX

( uses 20 bytes of space when headerless )
: CARRAY: ( n -- <name> )
       <BUILDS  HERE , 2+  ALLOT


 ;CODE ( w -- addr )
 ( LDD  0 Y   ) 18 C, EC C, 00 C,
 ( ADDD W     ) D3 C, 00 C,
 ( ADDD # 4   ) C3 C, 00 C, 04 C,
 ( STD  0 Y   ) 18 C, ED C, 00 C,
 ( JMP  NEXT  ) 7E C, FE4A C,
 END-CODE


