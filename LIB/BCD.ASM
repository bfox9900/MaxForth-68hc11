( BCD->S  bcd to integer conversion word   26JAN93
HEX
ASSEMBLER
CODE BCD->S  ( bcd -- n)
       TOP LDAA,
       TOP LDAB,               ( get a copy of bcd in B)
       0F # ANDB,              ( units is in b)
       F0 # ANDA,              ( tens in A )
       04 # ASRA,              ( shift right 4 bits )
       TOP  STAA,              ( push tens onto forth stack)
       TOP 0A # MUL,           ( multiply by 10 )
       TOP ADDD,
       TOP STD,
       NEXT JMP,
END-CODE
