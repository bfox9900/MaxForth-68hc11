( KEY & EMIT interface for cooperative multi tasking  24MAR94 FOX)

HEX
   B02F CONSTANT SCDR          ( serial communication data reg.
   B02E CONSTANT SCSR          ( serial communication status reg.

    16 6 -  USER UKEY                    ( vector address of key routine)
    18 6 -  USER UEMIT
( added these to make the polling a little faster )
CODE SCRX? ( -- c)             ( is serial rx. ready )
        F6 C, SCSR ,           ( ldab scsr           )
        C4 C, 20 C,            ( andb # 20           )
        4F C,                  ( clra                )
        7E C, FE43 ,           ( jmp next            )
END-CODE

CODE SCTX?  ( -- c )           ( is serial tx. empty )
        F6 C, SCSR ,           ( ldab SCSR           )
        C4 C,  80 C,           ( andb # 80           )
        4F C,                  ( clra                )
        7E C, FE43 ,           ( jmp next            )
END-CODE



( these I/O words SWITCH CONTEXT every time they execute! )
HEX
CODE <MKEY>
        BEGIN,
         ' YIELD @   JMP,        ( jump to pfa of YIELD)
          SCSR LDAB,             ( check com status register)
          20 # ANDB,             ( and it with  20 )
         .NE.
        UNTIL,                   ( loop until Not equal to 0)
        SCDR LDAB,              ( WE GOT ONE. load char into ACC. B)
        CLRA,                   ( clear ACC. A, upper byte )
        DEY, DEY,               ( make a space on Forth Stack )
        TOP STD,                ( put the CHAR on the Forth stack )
        NEXT JMP,
(        RTS, )
END-CODE

: MEMIT ( c -- )
        BEGIN
            SCSR C@ 80 AND
            ( YIELD)
        UNTIL
        SCDR C! ;

( These are hi-level FORTH sub-routines. They are easier to write and
( test than assembler I/O routines.  They can be called by a JSR
( instruction!  Therefore they can be used to re-vector UKEY & UEMIT
( so ALL input will execute a YIELD OPERATION on completion


CODE <MEMIT>
       CC C, ' MEMIT CFA ,     ( LDD ' MEMIT CFA
       BD C, ATO4 ,            ( JSR  ATO4 ( assembler to Forth converter)
       39 C,                   ( RTS
 END-CODE

( ** Think about these words carefully before admitting you don't get it **)

( At compile time we FETCH the present sub-routine address of UKEY and)
( lock it in as a literal number in this WORD definition.             )
( Now when we execute this word it will make the value of UKEY the    )
( RS232 port sub-routine address. )

: STD-IO ( SINGLE ) ( -I/O)
       [  UKEY @ ] LITERAL UKEY !
       [ UEMIT @ ] LITERAL UEMIT ! ;

( Here we set UKEY & UEMIT to the value of our new sub-routines )
: MULTI-I/O
       [ ' <MKEY> @  ] LITERAL UKEY !
       [ ' <MEMIT> @ ] LITERAL UEMIT ! ;

DECIMAL




