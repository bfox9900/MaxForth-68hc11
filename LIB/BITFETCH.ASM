( LOGICAL shift right a byte. CODE word.
CODE B@ ( c #bits -- )       ( #bits is 7 max)
        TOP LDX,
        SEC 1+ LDAB,
        BEGIN,
                0 # CPX,
        .NE. WHILE,
                LSRB,
                DEX,
        REPEAT,
        CLRA, 1 # ANDB,
        SEC 1+ STD,
        POP JMP,
END-CODE



