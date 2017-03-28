( Task switcher with priority, SLEEP & WAKE [MTASK.200]  07DEC93 FOX )

( this is the NEW multitasking switcher that allows the use of the )
( PRIORITY user variable AND it allows INDIVIDUAL wake and sleep)
( *WITHOUT HALTING* the multitasker to do it. )
( It is incorporated into MTASK.200 as machine code )
HEX
CODE PAUSE
               40 #  LDAA,    ( 2)
               TFLG2 STAA,    ( 4)   ( clear real time interupt flag)
               IP    LDX,     ( 5)   ( get forth instruction pointer)
                     PSHX,    ( 4)   ( & push it onto hardware stack)
               W     LDX,     ( 5)
                     PSHX,    ( 5)
               UP    LDX,     ( 5)   ( load forth user pointer into X)
( RPSAVE)      6 ,X  STS,     ( 5)   ( store current R stack pointer)
                          ( =35)     ( * This is the context switch *)
               BEGIN,                ( loop for next awake task )
( TLINK )        2 ,X LDX,     ( 5)  ( load next task address into X)
( TFLAG )        0 ,X LDAA,    ( 4)  ( load sleep flag into A)
                .NE.           ( 3)
               UNTIL,
               UP    STX,     ( 5)   ( & put it into user pointer )
( RPSAVE)      6 ,X  LDS,     ( 5)   ( get new hardware stack [RP] value)
                             ( T0T=57)
               PACTL LDAA,    ( 4)
               FC #  ANDA,    ( 2)   ( clr bits 0 &1 IN REG A)
( PRIORITY+1)  5 ,X  ORAA,    ( 4)   ( OR the priority value into REG A)
               PACTL STAA,    ( 4)   ( put new value into PACTL )

               PULX,          ( 5)
               W     STX,     ( 5)   ( POP previous W )
                     PULX,    ( 5)
               IP    STX,     ( 5)   ( POP previous IP )
                     RTI,    ( 12)
END-CODE                     ( =103)


