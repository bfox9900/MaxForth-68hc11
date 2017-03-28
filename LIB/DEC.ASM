( CODE 1-C!   decrements a byte variable )
HEX
ASSEMBLER
CODE 1-C!  ( var -- )
     TOP LDX,
     1 ,X DEC,
     POP JMP,
END-CODE

