( HI SPEED bit counter word    31jan94 fox )
HEX
CODE COUNT_8_BITS
18 C, A6 C, 01 C, 84 C, 01 C, 
27 C, 39 C, CD C, EE C, 02 C, 
E6 C, 00 C, 26 C, 11 C, CD C, 
EE C, 04 C, EC C, 00 C, C3 C, 
00 C, 01 C, ED C, 00 C, CD C, 
EE C, 02 C, C6 C, 50 C, E7 C, 
00 C, CD C, EE C, 02 C, E6 C, 
00 C, 27 C, 03 C, 5A C, E7 C, 
00 C, 18 C, EC C, 04 C, C3 C, 
00 C, 02 C, 18 C, ED C, 04 C, 
18 C, EC C, 02 C, C3 C, 00 C, 
01 C, 18 C, ED C, 02 C, 18 C, 
66 C, 01 C, 20 C, C0 C, 18 C, 
08 C, 18 C, 08 C, 18 C, 08 C, 
18 C, 08 C, 7E C, FC97 ,
END-CODE

( CODE COUNT_8_BITS ( count[7] latch[7] data -- )
(        ( -- is  data>0 ? -- )
(    BEGIN,    
(        TOP 1+ LDAA,                             ( data byte -> A)
(        01 # ANDA,                               ( check bit #7  )
(        .NE.                                    
(     WHILE,     
(            ( -- check if latch=0 --)                                      
(            SEC LDX, 
(            0 ,X LDAB,                          ( latch byte -> B )
(            .EQ.
(            IF, 
(                ( -- increment the counter -- )
(                3RD LDX,                          ( counter[] ->X )
(                0 ,X LDD,                         ( [X] ->D)
(                1 # ADDD,                         ( D ++ )
(                0 ,X STD,                         ( D ->counter[] )

                ( -- set the latch --)
(                SEC LDX,
(                t # LDAB,
(                0 ,X STAB,
(            THEN,
         ( -- decrement the latch value to 0 -- )
(         SEC  LDX,                                ( address into X)
(         0 ,X LDAB,                               ( load byte into B)
(         .NE.
(         IF,                                      ( if B<>0 )
(             DECB,                                ( decrement B)
(             0 ,X STAB,                           ( B->latch[])
(        THEN,
         ( -- goto next array location & next bit --)
(         3RD LDD,       ( load array addr.  from FORTH stack into X )
(         2 # ADDD,      ( incr. by 2 {integer array})
(         3RD STD,       ( put the new address on the stack)
         
(         SEC LDD,    ( INCR LOWER byte of array address for LATCH[] )
(         1 # ADDD,
(         SEC STD,

(         TOP 1+ ROR,    ( ROLL data byte right 1 bit ) 
(     REPEAT,
(     INY, INY,                  ( DROP data byte )
(     INY, INY,                  ( DROP latch address )
(     POP JMP,                   ( DROP count adr. & return to  FORTH )
( END-CODE                

