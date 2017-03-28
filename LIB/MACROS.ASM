( MACROS an experiment in optimizing MAXForth by removing NEXT 08DEC93 FOX)

(      These words are machine code macros that expand inline into the      )
( same code that FORTH normally calls.  This allows time critical           )
( inner loops to be optimized when they involve heavy use of primitive      )
( FORTH kernel words.   Ironically the greatest performance improvements    )
( will occurr only when the most primitive parts of the kernel are needed   )
( because because simple words like "+" "*" or "DUP" are trivial to execute.)
( They are therefore slowed down siginicantly by the need to execute the    )
( "NEXT" routine after every operation.
(      These macros attempt to allow programming the FORTH machine as inline)
( instructions.  This results in near assembler speed without the need to   )
( to know the CPU very well. It of course can never achieve the performance )
( of an experienced assembly language programmer.
(      This solution of course presents the programmer with the classic     )
( size versus speed trade-off. )
HEX

ASSEMBLER DEFINITIONS

( MACRO for FORTH NEXT that ends a routine )
: >NEXT         7E C,  FE4A , ;

( MACROS to cope with the FORTH virtual machine )

( FORTH stack control )
: LIFT,        1809 , 1809 , ;            ( DEY DEY )
: DROP,        1808 , 1808 , ;            ( INY INY )

( registers D & X are used for everything)
: ->D     ( n -- n )    18EC , ( i ) C, ;
: ->X     ( n -- n )    CDEE , ( i ) C, ;
: POPD    ( n -- )      top ->D  DROP, ;           ( LDD 0,Y  DROP )
: POPX    ( n -- )      top ->X  DROP, ;
: <-D     ( n -- n )    18ED , ( i ) C,  ;              ( STD i,Y)
: <-X     ( n -- n )    CDEF , ( i ) C,  ;              ( STX i,Y)
: PUSHD   ( -- n )      LIFT,  top <-D  ;           ( STD 0,Y)
: PUSHX   ( -- n )      LIFT,  top <-X  ;
: D->[X]  ( -- )        ED00 , ;
: [X]->D  ( -- )        EC00 , ;
: D+!     ( n -- )      18E3 , top C,  DROP, ;  ( add stack in D register)
: X<->D   ( -- )        8F C,  ;

( this should make a nestable loop )
: FOR   ( n -- )
       POPX                            ( pop FORTH stack to X)
       PSHX,                           ( push X to Return stack )
       BEGIN, ;                        ( start a loop )

: LOOP
       PULX,                           ( pull index off stack)
       DEX,                            ( decrement the index )
       PSHX,                           ( put it back for next time)
       .EQ. UNTIL,
       INS, INS,  ;

: DUP  ( n -- n n )
       top ->D
       PUSHD  ( NEXT ) ;

: SWAP ( x y -- y x )
       top ->D
       sec ->X
       sec <-D
       top <-X ;

: ROT  ( x y z -- y z x )
       18 C, EC C, 04 C, CD C, EE C,
       02 C, CD C, EF C, 04 C, CD C,
       EE C, 00 C, CD C, EF C, 02 C, ;

: @    ( adr -- n )
       top ->X
       [X]->D
       top <-D ;

: !    ( n adr -- )
       top ->X
       sec ->D
       D->[X]
       DROP, DROP, ;

: +    ( n n -- n )
       sec ->D
       18E3 , top C,
       sec <-D
       DROP, ;

: -    ( n n -- n )
       sec ->D
       18A3 , top C,
       sec <-D
       DROP, ;

: /    ( n n -- n )
       F8 C, 39 C, FD C, 4E C, FD C,
       7F C, FE C, B6 C, FE C, AC C,
       FB C, 35 C, FD C, 7F C, FE C,
       B6 C, FE C, AC C, FE C, B6 C,
       FE C, 58 C, DE C, 04 C, EE C,
       0C C, 3C C, EE C, 00 C, E6 C,
       00 C, 38 C, E4 C, 02 C, E8 C,
       03 C, 27 C, 02 C, C6 C, FF C,
       17 C,
       sec <-D DROP,  ;

: INLINE:  [COMPILE]  CODE ; IMMEDIATE

: ;INLINE    NEXT JMP,  [COMPILE] END-CODE ;

