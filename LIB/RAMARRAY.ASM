( Machine code int & byte arrays for use with ROMmed apps.  29JUN93 FOX )

( re-written to improve speed and size May14/96                         )

FORTH DEFINITIONS
( ------------------------------notes-----------------------------------)
( Coded arrays were tested against the FORTH equivalent in Max-Forth    )
( and they index exactly twice the speed of hi-level forth              )
( They depend on the word THERE which in my rommed system points to the )
( next available RAM byte.  "THERE" is compiled into the definition of  )
( a new array as the base address of the array at compile time          )
( The base address is found by getting the value at W [address 0] in    )
( MaxForth then fetching the contents of base+2                         )
( ----------------------------------------------------------------------)


HEX
: RAM-ARRAY: ( n  -- <name> )
       <BUILDS   THERE ,          ( compile base address of the array)
                 2*  ALLOT-RAM    ( grab what you need )

      ;CODE  ( i adr -- adr)
                18 C, EC C, 00 C, 05 C, DE C,
                00 C, E3 C, 02 C, 7E C, FE47 ,
              ( -- source code for above -- )
              ( TOP   LDD,      ( get the index                   )
              (       LSLD,     ( time 2                          )
              ( 00    LDX,      ( get base address from W into X  )
              ( 2 ,X  ADDD,     ( add the value at base+2 with D  )
              ( PUT   JMP,
        END-CODE

( - Run time hi-level FORTH equivalent to:  DOES> @ SWAP 2* + ; )


: RAM-CARRAY: ( n adr -- <name> )
       <BUILDS  THERE ,
                ALLOT-RAM

       ;CODE  ( i adr -- adr)
                18 C, EC C, 00 C, DE C, 00 C,
                E3 C, 02 C, 7E C, FE47 ,
              ( -- source code for above -- )
              ( TOP    LDD,
              ( 00     LDX,     ( Same as DOCON                         )
              ( 2 ,X   ADDD,    ( FETCH  value at W register into D     )
              ( PUT    JMP,
        END-CODE


