( FOR LOOP  for forth assembler this  makes a nestable loop )

ASSEMBLER DEFINITIONS
( registers D & X are used for everything)
: ->D     ( n -- n )    18EC , ( i ) C, ;
: ->X     ( n -- n )    CDEE , ( i ) C, ;
: POPD    ( n -- )      top ->D  DROP, ;           ( LDD 0,Y  DROP )
: POPX    ( n -- )      top ->X  DROP, ;

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



