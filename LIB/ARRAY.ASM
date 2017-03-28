( ARRAY.4TH  rdb 04/03/89  Definer for a one dimen-sional array)
( This implementation runs 2X faster than Forth code only)
HEX

( ARRAY       )
( Compilation: ( w ccc --    )
( At compila-tion, create an array with w 16 bit cells, named by the
( token ccc in the input stream.

( Execution:  ) ( w -- addr   )
( At execution return the address addr of the cell w.

: ARRAY:
 <BUILDS ( w ccc -- )  HERE , 2*  ALLOT

 ;CODE ( w -- addr )
 ( LDD  0 Y   ) 18 C, EC C, 00 C,
 ( LSLD       ) 05 C,
 ( ADDD W     ) D3 C, 00 C,
 ( ADDD # 4   ) C3 C, 00 C, 04 C,
 ( STD  0 Y   ) 18 C, ED C, 00 C,
 ( JMP  NEXT  ) 7E C, FE C, 4A C,
 END-CODE

DECIMAL
