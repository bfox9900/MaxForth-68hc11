( BITS.ASM  CODE word. B@ attempts to speed up bit counting )

HEX
FORTH DEFINITIONS
                            ( cycles)
CODE B@  ( c bit# -- ? )
       TOP LDX, INY, INY,     ( 14)   ( load bit# into X & correct stack)
       INX,                   (  3)   ( INC x to correct counting)
       TOP LDD,               (  6)   ( load # in D )
       BEGIN,
           DEX,               (  3)    ( dec the count index in X)
       .HI.                   (  3)    ( branch HI instruction )
       WHILE,                 (  0)    ( compiles the jump address)
            LSRD,             (  3)    ( shift left logical )
       REPEAT,                (  3)    ( Jump back to begin )
       CLRA,  01 # ANDB,      (  4)    ( and out all except bit 0 )
       PUT JMP,               (  3)    ( put D reg. on the stack )
END-CODE
                      ( total=  42+ [bit# * 12]  )

