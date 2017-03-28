\ BIGSTRINGS.f provides re-entrant string functions for Win32Forth  BFox

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ Change History
\ Original FigForth: Brian Fox London Ontario Canada    08Oct87
\ Ported to HSForth                                     14Sep88
\ Ported to Win32Forth & sped up string stack           09Oct04
\ Test under VFX to compare speed  PII 350Mhz           30Oct04
\ ( VFX 12 seconds, Win32 Forth 19.8 secs)

\ Tested on SEMPRON 3200 (1.6GHz)  Win32Forth 4.516 seconds 19NOV05


\ Explanation
\ This file extends Forth to create non ANS counted strings.  The original
\ objective was to demostrate string handling in Forth that was as easy as BASIC
\ I believe the objective was met.
\
\ The principal is simple. All string functions that potentially alter a string
\ move their output to a string stack and return the address of that new string.
\ The String stack is created in temporary space above PAD in a location called
\ TOP$.  By creating intermediate output strings each function can be
\ "strung together" until the final result is obtained.  The final results can be
\ stored back to another string or printed.  Printing or storing a string
\ collapses the string stack automatically.

\ Naming Convention:
\       WORDs that end in '$' leave results in TOP$
\       Address of  top$ is left on the parameter stack on completion
\
\ Features Include:
\   Compile time size checking. ( for novice users, can be commented out )
\   MaxLen byte compiled each into string for run time overflow checks.
\   String stack of fixed width for speed
\   TOP$ is the top of a stack of PAD's; for multiple string operations
\   Normal "BASIC" functions are re-entrant (LEFT$ RIGHT$ MID$)
\   $. and $! now collapse the string stack on completion.
\   +$ allows multiple concatenation with run time size checking
\   :="   for easy assignment of string literals.
\   :=""  for string clear routine.
\   $POS   finds position of a character within a string.
\   MAXLEN  returns the maximum size of the string
\   $.R   prints flush right text with leading blanks
\   $.LEFT prints flush left text with trailing blanks
\   TRIM$  removes trailing blanks

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ BIG string primitive operations
: len  ( adr$ - n )   @ ;
: maxlen ( str -- maximum length )  1 cell - len ;
: !len   ( n adr -- adr )  swap over ! ;

: bigcount ( str -- adr n )
        dup len swap cell+ ;

\ string stack lexicon
HEX
        pad value top$                  \ contains address of top of string stack

\        100 constant ss-width          \ string stack width
          constant max$len              \ biggest string we handle arbitrary

: +ssp                                  \ incr. string stack pointer
        top$ dup len + to top$  ;

: clrssp
        pad to top$ ;                 \ reset top of string stack to pad


: ($!) ( $addr1 $addr2 -- )           \ no size checking!! be careful
      >r  dup c@ 1+  r> swap cmove ;


: >top$ ( str -- )                      \ push str to string stack
         +ssp top$ ($!) ;


: $clip ( $addr n --  $addr n )
          max over c@ min ;

: ?stringsize  ( n --  )
                max$len > abort" string too big" ;

decimal
\ string variable changed to compile size byte for error checking
\ this version also uses compile time size checking with an abort.

: big$variable ( #bytes  --  )
       create dup ?stringsize  \ remove if not needed
              dup , 0 , allot
       does> 1+ ;





DECIMAL
: chr$      ( ascii#   --  pad    )    256 *   1+ top$ ! top$ ;
: asc       ( adr$     --  ascii# )    1+ c@ ;


\ TEXT$ is like TEXT (Brodie) but much handier since you can get multiple
\ inputs that simple stack up automatically. ( BL $TEXT  BL $TEXT etc...)
: TEXT$ ( delimit-char -- TOP$)
                 WORD >TOP$ TOP$ ;


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ STRING I/O WORDS:
\  $!  $.  I refer to as string I/O words. This type of word
\ must clear the string stack when complete. Since these operators always
\ DO something with end product string this does not pose much of restriction
\ to the FORTH programmer and abstracts the details allowing the Forth
\ programmer to write complex phrases of string language with less concern
\ about the mechanism at work.

: $! ( $adr1 $adr2 -- )
        ($!) clrssp ;



\ useful if you need error checking
: $move  ( $adr1 , $adr2 -- )
          2dup maxlen swap len < abort" string to big"
          $! ;

\  $. clears string stack after printing.
: $.   ( $addr -- )
                count type clrssp ;

: $.left ( str,n -- )                   \ prints str, n chars wide
                over $.                         \ print the string
                swap c@ - 0 max spaces ;        \ print len(str)-n spaces

: $.r    ( str,n -- )                   \ print right justified
                over len - 0 max spaces $. ;


\ view top$ but do not collapse the string stack
: .top$ ( str -- )
                top$ count type ;


\ these are syntax candy
: :="  ( $addr -- <text> )
                34 word swap $! ;             \ usage:  name :=" Brian Fox"

: :="" ( $addr -- )
               dup maxlen 0 fill ;            \ usage:  name :=""

: $xchg   ( str1,str2 -- )      \ does run time size checking
                dup >top$
                over swap ($!)  ( don't collapse the stack. we still need it)
                top$ swap $move  ;

: +$    ( str1,str2 -- top$ )
                2dup swap ( str1) >top$
                ( str2) count top$ count +  swap cmove
                len swap len + dup ?stringsize
                top$ !len ;

: left$  ( adr$ #char --  top$ )
                swap  >top$  top$ !len  ;

: right$    ( adr$ #char --  top$ )
                +ssp
                0 $clip  >r  count r@ - + r@ top$ c!
                top$ 1+   r>  cmove  top$ ;

: mid$      (  adr$  start  #char  --  top$  )
                +ssp
                0 max  >r  1 $clip
                2dup swap c@ - negate 1+ r> min
                top$ c!  +  top$ 1+ top$ c@ cmove   top$  ;

: str$      ( n -- top$ )
            +ssp  0 <# #s #>   dup top$ c!  top$ 1+ swap cmove  top$  ;


: val       ( adr$ - n )        \ can't accept commas or periods
                count number? not
                if
                   abort" val cannot convert the string to a number"
                then
                drop  ;

: trim$   ( adr$ - top$ )       \ removes trailing blanks, results in Top$
                >top$
                top$ count -trailing swap 1- tuck c! ;

: $compare    ( adr$1 adr$2 -- -n:0:n )
              count rot count compare ;


: $<   ( adr$1 adr$2 -- ? )
                $compare 1 = ;

: $>   ( adr$1 adr$2 -- ? )
                $compare -1 = ;

: $=   ( adr$1 adr$2 -- ? )
                $compare 0= ;

: $pos   ( addr$ char -- position )      \ returns 0 if not found
                over >r
                >r dup count
                r> scan drop swap -
                r> c@
                over <=
                if  drop 0 then ;

: $>Z ( addr$ -- top$ )  \ convert a counted string to a 'zero' string 
        >top$
        top$ count 2dup
        + 0 swap c! ; 

decimal
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ test suite

\ test strings
255 $variable q   q :=" 12345678901234567890123456789012345678901234567890"
255 $variable w   w :=" abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz     "
125 $variable x   x :=" string X"  ( smaller to check for errors)
255 $variable y   y :=" string Y"

: stringmuncher
           clrssp
           w trim$ q +$   q +$  w trim$ +$   y $!  \ create a big string

           y 100 left$  60 right$   2 50 mid$  x $! ;


: $tester
        cr ." testing 1,000,000 iterations..."
        timer-reset
        1000 0
        do
           1000 0
           do
             stringmuncher
           loop
        loop
        cr
        cr ." Results:"
        cr ." X = "  x $.
        cr
        cr ." Y = "  y $.
        cr .elapsed
        cr
        cr ." Testing 'X Y $xchg' size test. Should abort with error"
        cr  x y $xchg ;



 
