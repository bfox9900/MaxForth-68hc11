( 0!.ASM   to clear an integer variable   )
HEX
CODE 0!
        CD C, EE C,  0 C,
        4F C, 5F C, ED C, 0 C,
        7E C, FC97 ,
END-CODE

( CODE 0!
(      TOP LDX,    ( 6 )
(      CLRA, CLRB, ( 4 )
(      0 ,X STD,   ( 5 )
(       POP JMP,   ( 3 )
( END-CODE )
DECIMAL
