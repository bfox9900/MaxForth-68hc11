( LOGICAL shift right a byte. CODE word.
CODE LSRB ( c #bits -- )       ( #bits is 7 max)
        TOP LDX,
        BEGIN,
                0 # CPX,
        .NE. WHILE,
                SEC 1+ LSR,
                DEX,
        REPEAT,
        POP JMP,
END-CODE



