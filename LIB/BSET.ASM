( BSET BCLR   code words for bit manipulation in memory 20MAR93 Fox)

HEX
FORTH DEFINITIONS

CODE BSET  ( adr c -- )
        TOP 1+ LDAA,
        SEC LDX,
        0 ,X ORAA,
        0 ,X STAA,
        POPTWO JMP,
END-CODE


CODE BCLR ( adr msk  -- )
        TOP 1+ LDAA,
        SEC LDX,
        COMA,
        0 ,X ORAA,
        0 ,X STAA,
        POPTWO JMP,
END-CODE


