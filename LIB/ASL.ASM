( ASL.ASM    arithimetic shift right    )

ASSEMBLER
CODE ASL  ( n #bits -- )       ( like FORTH '2*' but for more bits )
        TOP LDX,
        BEGIN,
                0 # CPX,
        .NE. WHILE,
                SEC 1+ ASL,  SEC ROL,
                DEX,
        REPEAT,
        POP JMP,
END-CODE

