( 1-!0MAX.MAX  decrements a VARIABLE but never goes below zero )
 
CODE 1-!0MAX
        CD C, EE C, 00 C, EC C, 00 C, 
        27 C, 05 C, 83 C, 00 C, 01 C, 
        ED C, 00 C, 7E C, FC97 ,
END-CODE

( CODE 1-!0MAX  ( adr -- )
(        TOP LDX,
(        0 ,X LDD,
(        .NE.
(        IF, 
(          1 # SUBD,
(          0 ,X  STD,
(        THEN,
(        POP JMP,
( END-CODE
 
