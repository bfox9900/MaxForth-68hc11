( @&+! is to speed up this time critical routine 25mar93 fox)
( pronounced: "fetch and plus store" )

( Meaning   :  given 2 addresses of variables on the stack,)
(              fetch the value of the source and add it to )
(              the value of the destination variable       )

( Example:    X @  Y +!    becomes...   X Y @&+!  20% faster)

HEX
CODE @&+! ( adr adr -- )
       CD C, EE C, 02 C, EC C, 00 C, CD C, EE C, 00 C,
       E3 C, 00 C, ED C, 00 C,
       7E C, FC93 ,
END-CODE

( CODE @&+!  ( adr1 adr2 -- )
(       SEC LDX,
(       0 ,X LDD,
(       TOP LDX,
(       0 ,X ADDD,
(       0 ,X STD,
(       POPTWO JMP,                ( clean off the stack return to FORTH)
( END-CODE
