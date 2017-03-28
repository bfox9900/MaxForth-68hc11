( PORTC@ word and  [i]PORTC_BIT@  word  )

HEX
CODE PORTC@ ( -- n)
       B003 LDAB,
       CLRA,
       PUSHD JMP,
END-CODE

CODE ]PORTC_BIT@ ( i -- ?)
       B003 LDAB,                             ( get data @ port reg.B )
       TOP LDX,                               ( load bit# into reg.X )
       BEGIN,
       0 # CPX, .NE.
       WHILE,                                  ( while X<>0 )
           LSRB,                               ( logical shift right reg.B)
           DEX,                                ( decrement X )
       REPEAT,
       1 # ANDB,                               ( test bit# 0 in reg.B)
       CLRA,                                   ( clear reg.A)
       PUT JMP,                                ( PUT reg.D onto FORTH stack)
END-CODE

