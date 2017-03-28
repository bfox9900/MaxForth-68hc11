( ASR.ASM  CODE word.  arithmetic shift right operator  FOX )

CODE ASR ( n #bits -- )         ( arithmetic shift right )
        TOP LDX,
        BEGIN,
                0 # CPX,
        .NE. WHILE,
                SEC 1+ ASR, SEC ROR,
                DEX,
        REPEAT,
        POP JMP,
END-CODE
