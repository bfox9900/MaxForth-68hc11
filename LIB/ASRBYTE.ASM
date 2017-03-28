( ASRBYTE.ASM  various CODE words for bit shifting a byte  FOX )

CODE ASRB ( c #bits -- )       ( #bits is 7 max)
        TOP LDX,
        BEGIN,
                0 # CPX,
        .NE. WHILE,
                SEC 1+ ASR,
                DEX,
        REPEAT,
        POP JMP,
END-CODE




