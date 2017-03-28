( Multitasking switcher with PRIORITY user variable.  APR93 FOX )
(  It exists as HEX code in MTASK.200)

CODE IRQRTN
       40 #            LDAA,    ( 2)
       TFLG2           STAA,    ( 4)   ( clear real time interupt flag)
       IP              LDX,     ( 5)   ( get forth instruction pointer)
                       PSHX,    ( 4)   ( & push it onto hardware stack)
       W               LDX,
                       PSHX,
       UP              LDX,     ( 5)   ( load forth user pointer into X)
( RPSAVE) 6 ,X         STS,     ( 5)   ( store current R stack pointer)
( TLINK ) 2 ,X         LDX,     ( 5)   ( load next task address into X)
       UP              STX,     ( 5)   ( & put it into user pointer )
( RPSAVE) 6 ,X         LDS,     ( 5)   ( init. hardware stack [RP] )
                              ( =40)
        PATCL          LDAA,    ( 4)
        FC #           ANDA,    ( 2)   ( clr bits 0 &1 at PACTL I/O register)
( PRIORITY) 4 ,X       ORAA,    ( 4)   ( OR the priority value into REG A)
        PATCL          STAA,    ( 4)   ( put new value into PACTL )
                              ( =14)
                       PULX,    ( 5)
       W               STX,     ( 5)   ( POP previous W )
                       PULX,    ( 5)
       IP              STX,     ( 5)   ( POP previous IP )
                       RTI,    ( 12)
END-CODE                      ( =32)


