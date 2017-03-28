( test for .CLR. .SET.  test operators. )
( should make very fast test loops. )

FORTH DEFINITIONS
HEX
CODE   BITBRCH
           B003 LDX,  -5 01 0 ,X BRCLR,
        NEXT JMP,
END-CODE

