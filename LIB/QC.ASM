( QC@ QC! operators for byte Q date structure

( constants define offset to access the "QUEUE:" data structure )
HEX
0 CONSTANT head
1 CONSTANT tail
2 CONSTANT bytes
3 CONSTANT mask
4 CONSTANT data

FORTH DEFINITIONS

: QRST   ( q -- )  3 ERASE ;

( Simple Q fetch byte word.  c = -1 if Q is empty)
CODE QC@ ( q -- c)
       TOP  LDX,                ( get base address of Q into X
            CLRA,               ( clear A {ERROR} register
     tail ,X LDAB,               ( load tail ptr. into B
     head ,X CMPB, .EQ.          ( compare B to head pointer
            IF,                 ( if true Q is empty
         ( 0)  FFFF # LDD,      ( set bits in D. {error flag}
            ELSE,
               INCB,            ( increment B to next location
               mask ,X ANDB,     ( roll it over with mask value
               tail ,X STAB,     ( update the tail to new value
                       ABX,     ( X=X+B. X now contains read address
               data ,X LDAB,    ( bring byte into B
            THEN,
       PUT  JMP,                ( Replace 'q' with 'c' & return to FORTH
END-CODE

( NO ERROR CHECKS !! FAST BUT RISKY. )

CODE QC!  ( c queue -- )
(                  clocks
(                  ------
        TOP  LDX,   ( 6      ( get base address of Q into X
     head ,X LDAB,  ( 4      ( load head ptr. into B
             INCB,  ( 2      ( increment B to next location
     mask ,X ANDB,  ( 4      ( roll it over with mask value
     head ,X STAB,  ( 4      ( update the head to new value
             ABX,   ( 3      ( X=[B+X]. X = next write address -4
      SEC 1+ LDAB,  ( 5      ( get LSB off FORTH stack 2nd item
     data ,X STAB,  ( 4      ( byte into Q at addr. in X+data offset
                    ( = 30 = 15uS
     POPTWO JMP,   ( 53     ( clean the stack & goto FORTH
(           RTI,   ( 12     ( alternate ending for ISR use
END-CODE



