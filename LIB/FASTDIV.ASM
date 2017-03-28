( FASTDIV.ASM                        28JAN94 FOX )

( unsigned divsion operator.  6 times faster than MAXFORTH / !!!!!! )
CODE U/ ( u u -- u )
      PSHX,
      SEC LDD,
      TOP LDX,
      IDIV,
      SEC STX,
      PULX,
      POP JMP,
END-CODE

CODE FDIV ( u u -- u )
      PSHX,
      SEC LDD,
      TOP LDX,
      FDIV,
      SEC STX,
      PULX,
      POP JMP,
END-CODE
