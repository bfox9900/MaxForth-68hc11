( * SMALL C COMPILER SYS. LIBRARY, Modified for MaxFORTH ASM6811*)
( * Nov 1993                           *)
( *  Brian Fox                         *)
( *  London Ontario                    *)
( *  Canada                            *)
( **************************************)

HEX
FORTH DEFINITIONS

ASSEMBLER

(  Enable interrupts )
CODE EN_INT  ( -- )
      CLI,   NEXT JMP,
END-CODE

(  Disable interrupts )
CODE DIS_INT  ( -- )
      SEI, NEXT JMP,
END-CODE

( BSET BCLR   code words for bit manipulation in memory 20MAR93 Fox)

HEX
FORTH DEFINITIONS

CODE BSET  ( adr c -- )
        ASSEMBLER
        TOP 1+ LDAB,
        SEC LDX,
        0 ,X ORAB,
        0 ,X STAB,
        POPTWO JMP,
END-CODE


CODE BCLR ( adr msk  -- )
        ASSEMBLER
        TOP 1+ LDAB,
        SEC LDX,
        COMB,
        0 ,X ANDB,
        0 ,X STAB,
        POPTWO JMP,
END-CODE

( *************************************************)
( ***    START OF LOW LEVEL LIBRARY ROUTINES   ****)
( ***    PARTS BORROWED FROM MOTOROLA, OTHERS  ****)
( ***    RE-WRITTEN BY MATT TAYLOR             ****)
( ***    conversion to FORTH Brian Fox         ****)
( *************************************************)

ASSEMBLER
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
