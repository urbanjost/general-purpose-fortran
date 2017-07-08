!>
!!##NAME
!!    M_strings - [M_strings]Fortran string module
!!##SYNOPSIS
!!
!!  public entities:
!!
!!    use m_strings, only : split,delim,chomp
!!    use m_strings, only : substitute,change,modif,transliterate,reverse
!!    use M_strings, only : upper,lower
!!    use M_strings, only : adjustc,compact,nospace,indent,crop
!!    use M_strings, only : len_white,lenset,merge_str
!!    use M_strings, only : switch,s2c,c2s
!!    use M_strings, only : noesc,notabs,expand
!!    use M_strings, only : string_to_value,string_to_values,s2v,s2vs,value_to_string,v2s
!!    use M_strings, only : matchw
!!    use M_strings, only : isalnum, isalpha, iscntrl, isdigit, isgraph, islower,
!!                          isprint, ispunct, isspace, isupper, isascii, isblank, isxdigit
!!
!!    TOKENS
!!
!!    split  subroutine parses string using specified delimiter characters and stores tokens into an array
!!    delim  subroutine parses string using specified delimiter characters and store tokens into an array
!!    chomp  function consumes input line as it returns next token in a string using specified delimiters
!!
!!    EDITING
!!
!!    substitute     subroutine non-recursively globally replaces old substring
!!                   with new substring
!!    change         subroutine non-recursively globally replaces old substring
!!                   with new substring with a directive like line editor
!!    modif          subroutine modifies a string with a directive like the XEDIT
!!                   line editor MODIFY command
!!    transliterate  replace characters found in set one with characters from set two
!!    reverse        reverse character order in a string
!!
!!    CASE
!!
!!    upper  function converts string to uppercase
!!    lower  function converts string to miniscule
!!
!!    WHITE SPACE
!!
!!    adjustc  elemental function centers text within the length of the input string
!!    compact  left justify string and replace duplicate whitespace with single characters or nothing
!!    nospace  function replaces whitespace with nothing
!!    indent   find number of leading spaces
!!    crop     function trims leading and trailing spaces
!!
!!    STRING LENGTH
!!
!!    len_white  find location of last non-whitespace character
!!    lenset     return a string of specified length
!!    merge_str  make strings of equal length and then call MERGE(3f) intrinsic
!!
!!    CHARACTER ARRAY VERSUS STRING
!!
!!    switch  switch between a string and an array of single characters
!!    s2c     convert string to array of single characters and add null terminator for passing to C
!!    c2s     convert null-terminated array of single characters to string for converting strings returned from C
!!
!!    NONALPHA
!!
!!    noesc   convert non-printable ASCII8 characters to a space
!!    notabs  convert tabs to spaces while maintaining columns, assuming tabs are set every 8 characters
!!    expand  expand escape sequences in a string
!!
!!    NUMERIC STRINGS
!!
!!    string_to_value   generic subroutine returns numeric value (REAL, DOUBLEPRECISION, INTEGER)  from string
!!    string_to_values  subroutine reads an array of numbers from a string
!!    s2v               function returns DOUBLEPRECISION numeric value from string
!!    s2vs              function returns a DOUBLEPRECISION array of numbers from a string
!!    value_to_string   generic subroutine returns string given numeric value (REAL, DOUBLEPRECISION, INTEGER )
!!    v2s               generic function returns string from numeric value (REAL, DOUBLEPRECISION, INTEGER )
!!
!!    LOGICAL TESTS
!!
!!    matchw  compares given string for match to pattern which may contain wildcard characters
!!
!!    CHARACTER TESTS
!!
!!    o isalnum   returns .true. if character is a letter or digit
!!    o isalpha   returns .true. if character is a letter and .false. otherwise
!!    o iscntrl   returns .true. if character is a delete character or ordinary control character
!!    o isdigit   returns .true. if character is a digit (0,1,...,9) and .false. otherwise
!!    o isgraph   returns .true. if character is a printable character except a space is considered non-printable
!!    o islower   returns .true. if character is a miniscule letter (a-z)
!!    o isprint   returns .true. if character is an ASCII printable character
!!    o ispunct   returns .true. if character is a printable punctuation character
!!    o isspace   returns .true. if character is a null, space, tab, carriage return, new line, vertical tab, or formfeed
!!    o isupper   returns .true. if character is an uppercase letter (A-Z)
!!    o isascii   returns .true. if the character is in the range char(0) to char(127)
!!    o isblank   returns .true. if character is a blank character (space or horizontal tab.
!!    o isxdigit  returns .true. if character is a hexadecimal digit (0-9, a-f, or A-F).
!!
!!##DESCRIPTION
!!
!!    The M_strings module is a collection of Fortran procedures that process
!!    character strings. Routines for parsing, tokenizing, changing case,
!!    substituting new strings for substrings, locating strings with simple
!!    wildcard expressions, removing tabs and line terminators and other
!!    string manipulations are included.
!!
!!    M_strings_oop is a companion module that provides an OOP interface
!!    to the M_strings module.
!!
!!    As newer Fortran features become more widely available a significant
!!    amount of the code (much of which originated as pre-Fortran90 routines)
!!    is subject to updating so new versions of this module are not expected
!!    to be compatible with older versions.
!!
!!    OOPS INTERFACE
!!
!!    If you prefer an Object-oriented interface the M_strings_oop
!!    module (included with the M_strings module source) provides an OOP
!!    interface to the M_strings module; as described in the example
!!    program OBJECT_ORIENTED shown in the example program found below.
!!
!!##EXAMPLES
!!
!!
!! Each of the procedural functions includes an example program in the corresponding man(1) page for the function.
!! The object-oriented interface does not have individual man(1) pages, but is instead demonstrated using the following
!! example program:
!!
!!  program  object_oriented
!!  !
!!  ! This is an example using the object-oriented class/type model defined in M_strings_oop
!!  ! This is essentially the same functionality as the procedures combined with several Fortran intrinsics and overloaded operators
!!  !
!!  use M_strings_oop,only : string, p
!!  implicit none
!!  TYPE(string) :: str1
!!  TYPE(string) :: str2
!!  TYPE(string) :: str3
!!  !==============================================================================
!!    write(*,*)'exercise the M_STRING_OOP module interface'
!!    ! draw a break line in the output
!!    write(*,*)repeat('=',78)
!!    write(*,*)'Call methods of type(STRING)'
!!    ! define TYPE(STRING) with constructor
!!    str2=string('   This  is  a  String!       ')
!!    write(*,*)repeat('=',78)
!!    ! print members of type
!!    write(*,101)'str2%str is ................ ',str2%str
!!    ! same as intrinsic LEN()
!!    write(*,202)'len ........................ ',str2%len()
!!    ! same as intrinsic INDEX()
!!    write(*,202)'len_trim ................... ',str2%len_trim()
!!    ! same as intrinsic INDEX()
!!    write(*,202)'index("is")................. ',str2%index("is")
!!    ! same as intrinsic INDEX()
!!    write(*,202)'index("is",back=.T.) ....... ',str2%index("is",back=.TRUE.)
!!    ! output TYPE(STRING) with %str all uppercase
!!    write(*,101)'upper ...................... ',p(str2%upper())
!!    ! output TYPE(STRING) with %str all miniscule
!!    write(*,101)'lower ...................... ',p(str2%lower())
!!    ! output TYPE(STRING) with %str reversed
!!    write(*,101)'reverse .................... ',p(str2%reverse())
!!    ! same as intrinsic ADJUSTL()
!!    write(*,101)'adjustl .................... ',p(str2%adjustl())
!!    ! same as intrinsic ADJUSTR()
!!    write(*,101)'adjustr .................... ',p(str2%adjustr())
!!    ! center string in current string length
!!    write(*,101)'adjustc .................... ',p(str2%adjustc())
!!    ! center string in string length of NN
!!    write(*,101)'adjustc(49) ................ ',p(str2%adjustc(49))
!!    ! force %str to be NN characters long
!!    write(*,101)'lenset(49) ................. ',p(str2%lenset(49))
!!    ! same as intrinsic TRIM()
!!    write(*,101)'trim ....................... ',p(str2%trim())
!!    ! trim leading and trailing spaces
!!    write(*,101)'crop ....................... ',p(str2%crop())
!!    ! calls M_strings procedure SUBSTITUTE()
!!    write(*,101)'substitute("This","Here") .. ',p(str2%substitute("This","Here"))
!!    ! calls M_strings procedure COMPACT()
!!    write(*,101)'compact .................... ',p(str2%compact())
!!    write(*,101)'compact("") ................ ',p(str2%compact(""))
!!    write(*,101)'compact(":") ............... ',p(str2%compact(":"))
!!    ! calls M_strings procedure TRANSLITERATE()
!!    write(*,101)'transliterate("aei","VWX") . ',p(str2%transliterate("aei","VWX"))
!!    write(*,101)'transliterate("aeiou"," ") . ',p(str2%transliterate("aeiou"," "))
!!    write(*,101)'transliterate("aeiou","") .. ',p(str2%transliterate("aeiou",""))
!!    write(*,101)'transliterate(" aeiou","") . ',p(str2%transliterate(" aeiou",""))
!!    ! calls M_strings procedure SWITCH()
!!    write(*,404)'chars .................... . ',str2%chars()
!!
!!    write(*,*)repeat('=',78)
!!    str2%str='\t\tSome tabs\t   x\bX '
!!    write(*,101)'str2%str ................... ',str2%str
!!    write(*,101)'expand ..................... ',p(str2%expand())
!!    str2=str2%expand()
!!    ! calls M_strings procedure NOTABS()
!!    write(*,101)'notabs ..................... ',p(str2%notabs())
!!    ! calls M_strings procedure NOESC()
!!    write(*,101)'noesc ...................... ',p(str2%noesc())
!!
!!    write(*,*)repeat('=',78)
!!    write(*,*)'Casting to numeric variables'
!!    str3=string('   12.345678901234567e1        ')
!!    write(*,101)'str3%str ................... ',str3%str
!!    ! calls M_strings procedure STRING_TO_VALUE()
!!    write(*,*)'int  ....................... ', str3%int()
!!    ! calls M_strings procedure STRING_TO_VALUE()
!!    write(*,*)'real ....................... ', str3%real()
!!    ! calls M_strings procedure STRING_TO_VALUE()
!!    write(*,*)'dble ....................... ', str3%dble()
!!
!!    write(*,*)repeat('=',78)
!!    write(*,*)'Matching simple globbing patterns'
!!    str3=string('   12.345678901234567e1        ')
!!    str3=string('Four score and seven years ago')
!!    write(*,101)'str3%str ................... ',str3%str
!!    ! calls M_strings procedure MATCHW
!!    write(*,*)'match("Fo*") ............... ', str3%match("Fo*")
!!    ! calls M_strings procedure MATCHW
!!    write(*,*)'match("and") ............... ', str3%match("and")
!!    ! calls M_strings procedure MATCHW
!!    write(*,*)'match("*and*") ............. ', str3%match("*and*")
!!
!!    101 format(1x,a,"[",a,"]")
!!    202 format(1x,a,i0)
!!    303 format(1x,*(l3))
!!    404 format(1x,a,*("[",a1,"]":))
!!
!!    write(*,*)repeat('=',78)
!!    write(*,*)'OVERLOADED OPERATORS (add and subtract,return TYPE(STRING))'
!!    str1%str='123.456'
!!    str2%str='AaBbCcDdEeFfGgHhIiJj AaBbCcDdEeFfGgHhIiJj'
!!    write(*,101)'str1%str ................... ',str1%str
!!    write(*,101)'str2%str ................... ',str2%str
!!    write(*,*)'str1 + str2 ................ ',p(str1 + str2)
!!    ! a string that looks like a numeric value can have a value added
!!    write(*,*)'str1 + 20000 ............... ',p(str1 +20000)
!!    write(*,*)'str1 - 20.0 ................ ',p(str1 -20.0)
!!    write(*,*)'str2 - "Aa" (removes ALL) .. ',p(str2 - 'Aa')
!!
!!    write(*,*)repeat('=',78)
!!    write(*,*)'OVERLOADED OPERATORS (multiply,return TYPE(STRING))'
!!    str1%str='AaBbCcDdEeFfGgHhIiJj'
!!    write(*,101)'str1%str ................... ',str1%str
!!    write(*,*)'str1 * 3 ................... ',p(str1 * 3)
!!
!!    write(*,*)repeat('=',78)
!!    write(*,*)'OVERLOADED OPERATORS (//,return TYPE(STRING))'
!!    str1%str='String one:'
!!    str2%str='String two:'
!!    write(*,101)'str1%str ................... ',str1%str
!!    write(*,101)'str2%str ................... ',str2%str
!!    write(*,*)'str1 // str2 ................ ',p(str1 // str2)
!!    ! numeric values are converted to strings
!!    write(*,*)'str1 // 20000 ............... ',p(str1 // 20000)
!!    write(*,*)'str1 // 20.0 ................ ',p(str1 // 20.0)
!!
!!    write(*,*)repeat('=',78)
!!    write(*,*)'OVERLOADED OPERATORS (logical comparisons,return logical)'
!!    ! NOTE: comparisons are performed on the character variable members
!!    !       of the type(string)
!!    str1%str='abcdefghij'
!!    str2%str='klmnopqrst'
!!    write(*,101)'str1%str ................... ',str1%str
!!    write(*,101)'str2%str ................... ',str2%str
!!    write(*,*)': EQ LT GT LE GE NE'
!!    write(*,*)'compare str1 to str1'
!!    write(*,303)str1.eq.str1  ,str1.lt.str1  ,str1.gt.str1  ,str1.le.str1 &
!!               & ,str1.ge.str1  ,str1.ne.str1
!!    write(*,*)'compare str1 to str2'
!!    write(*,303)str1.eq.str2  ,str1.lt.str2  ,str1.gt.str2  ,str1.le.str2 &
!!               & ,str1.ge.str2  ,str1.ne.str2
!!    write(*,*)'compare str2 to str1'
!!    write(*,303)str2.eq.str1  ,str2.lt.str1  ,str2.gt.str1  ,str2.le.str1 &
!!               & ,str2.ge.str1  ,str2.ne.str1
!!
!!    write(*,*)repeat('=',78)
!!
!!  end program object_oriented
!!
!!
!!  Expected output
!!
!!   exercise the M_STRING_OOP module interface
!!   =============================================================================
!!   Call methods of type(STRING)
!!   =============================================================================
!!   str2%str is ................ [   This  is  a  String!             ]
!!   len ........................ 36
!!   len_trim ................... 23
!!   index("is")................. 6
!!   index("is",back=.T.) ....... 10
!!   upper ...................... [   THIS  IS  A  STRING!             ]
!!   lower ...................... [   this  is  a  string!             ]
!!   reverse .................... [             !gnirtS  a  si  sihT   ]
!!   adjustl .................... [This  is  a  String!                ]
!!   adjustr .................... [                This  is  a  String!]
!!   adjustc .................... [        This  is  a  String!        ]
!!   adjustc(49) ................ [              This  is  a  String!               ]
!!   lenset(49) ................. [   This  is  a  String!                          ]
!!   trim ....................... [   This  is  a  String!]
!!   crop ....................... [This  is  a  String!]
!!   substitute("This","Here") .. [   Here  is  a  String!             ]
!!   compact .................... [This is a String!]
!!   compact("") ................ [ThisisaString!]
!!   compact(":") ............... [This:is:a:String!]
!!   transliterate("aei","VWX") . [   ThXs  Xs  V  StrXng!             ]
!!   transliterate("aeiou"," ") . [   Th s   s     Str ng!             ]
!!   transliterate("aeiou","") .. [   Ths  s    Strng!                 ]
!!   transliterate(" aeiou","") . [ThssStrng!                          ]
!!   chars .................... . [ ][ ][ ][T][h][i][s][ ][ ][i][s][ ][ ][a][ ][ ][S][t][r][i][n][g][!][ ][ ][ ][ ][ ][ ][ ]
!!   =============================================================================
!!   str2%str ................... [\t\tSome tabs\t   x\bX ]
!!   expand ..................... [         Some tabs          xX]
!!   notabs ..................... [                Some tabs          xX]
!!   noesc ...................... [  Some tabs    x X]
!!   =============================================================================
!!   Casting to numeric variables
!!   str3%str ................... [   12.345678901234567e1        ]
!!   int  .......................          123
!!   real .......................    123.456787
!!   dble .......................    123.45678901234567
!!   =============================================================================
!!   Matching simple globbing patterns
!!   str3%str ................... [Four score and seven years ago]
!!   match("Fo*") ...............  T
!!   match("and") ...............  F
!!   match("*and*") .............  T
!!   ==============================================================================
!!   OVERLOADED OPERATORS (add and subtract, return TYPE(STRING))
!!   str1%str ................... [123.456]
!!   str2%str ................... [AaBbCcDdEeFfGgHhIiJj AaBbCcDdEeFfGgHhIiJj]
!!   str1 + str2 ................ 123.456 AaBbCcDdEeFfGgHhIiJj AaBbCcDdEeFfGgHhIiJj
!!   str1 + 20000 ............... 20123.455999999998
!!   str1 - 20.0 ................ -103.456
!!   str2 - "Aa" (removes ALL) .. BbCcDdEeFfGgHhIiJj BbCcDdEeFfGgHhIiJj
!!   =============================================================================
!!   OVERLOADED OPERATORS (multiply, return TYPE(STRING))
!!   str1%str ................... [AaBbCcDdEeFfGgHhIiJj]
!!   str1 * 3 ................... AaBbCcDdEeFfGgHhIiJjAaBbCcDdEeFfGgHhIiJjAaBbCcDdEeFfGgHhIiJj
!!   =============================================================================
!!   OVERLOADED OPERATORS (//, return TYPE(STRING))
!!   str1%str ................... [String one:]
!!   str2%str ................... [String two:]
!!   str1 // str2 ................ String one:String two:
!!   str1 // 20000 ............... String one:20000
!!   str1 // 20.0 ................ String one:20.0
!!   =============================================================================
!!   OVERLOADED OPERATORS (logical comparisons, return logical)
!!   str1%str ................... [abcdefghij]
!!   str2%str ................... [klmnopqrst]
!!   : EQ LT GT LE GE NE
!!   compare str1 to str1
!!   :  T  F  F  T  T  F
!!   compare str1 to str2
!!   :  F  T  F  T  F  T
!!   compare str2 to str1
!!   :  F  F  T  F  T  T
!!   =============================================================================
!===================================================================================================================================
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
MODULE M_strings !
use M_journal, only : journal
implicit none    ! change default for every procedure contained in the module
character(len=*),parameter::ident="@(#)M_strings(3f): Fortran module containing routines that deal with character strings"
!-----------------------------------------------------------------------------------------------------------------------------------
PRIVATE

!----------------------# TOKENS
PUBLIC split           !  subroutine parses a string using specified delimiter characters and store tokens into an array
PUBLIC chomp           !  function consumes input line as it returns next token in a string using specified delimiters
PUBLIC delim           !  subroutine parses a string using specified delimiter characters and store tokens into an array
!----------------------# EDITING
PUBLIC substitute      !  subroutine non-recursively globally replaces old substring with new substring in string
PUBLIC change          !  replaces old substring with new substring in string with a directive like a line editor
PRIVATE strtok         !  gets next token. Used by change(3f)
PUBLIC modif           !  change string using a directive using rules similar to XEDIT line editor MODIFY command
PUBLIC transliterate   !  when characters in set one are found replace them with characters from set two
PUBLIC reverse         !  elemental function reverses character order in a string
!----------------------# CHARACTER ARRAY VERSUS STRING
PUBLIC switch          !  generic switch between a string and an array of single characters (a2s,s2a)
PRIVATE a2s            !  function to copy char array to string
PRIVATE s2a            !  function to copy string(1:Clen(string)) to char array
PUBLIC s2c             !  convert character variable to array of character(len=1) with null terminator for C compatibility
PUBLIC c2s             !  convert null-terminate array of character(len=1) to string for strings returned by C
!----------------------# CASE
PUBLIC upper           !  elemental function converts string to uppercase
PUBLIC lower           !  elemental function converts string to miniscule
!----------------------# WHITE SPACE
PUBLIC adjustc         !  elemental function centers string within the length of the input string
PUBLIC compact         !  left justify string and replace duplicate whitespace with single characters or nothing
PUBLIC nospace         !  function replaces whitespace with nothing
PUBLIC indent          !  count number of leading spaces
PUBLIC crop            !  function trims leading and trailing spaces
!----------------------# STRING LENGTH
PUBLIC lenset          !  return a string as specified length
PUBLIC merge_str       !  make strings of equal length and then call MERGE(3f) intrinsic
PUBLIC len_white       !  find location of last non-whitespace character
!----------------------# CONTENT TESTS
PUBLIC matchw          !  compares given string for match to pattern which may contain wildcard characters
!----------------------# NONALPHA
PUBLIC noesc           !  elemental function converts non-printable ASCII8 characters to a space
PUBLIC notabs          !  convert tabs to spaces in output while maintaining columns, assuming a tab is set every 8 characters
PUBLIC expand          !  expand escape sequences in a string
!----------------------# NUMERIC STRINGS
PUBLIC string_to_value !  generic subroutine returns REAL|DOUBLEPRECISION|INTEGER value from string (a2d,a2r,a2i)
PRIVATE a2d            !  subroutine returns double value from string
PRIVATE a2r            !  subroutine returns real value from string
PRIVATE a2i            !  subroutine returns integer value from string
PUBLIC string_to_values!  subroutine returns values from a string
PUBLIC s2v             !  function returns doubleprecision value from string
PUBLIC s2vs            !  function returns a doubleprecision array of numbers from a string
PUBLIC value_to_string !  generic subroutine returns string given numeric REAL|DOUBLEPRECISION|INTEGER value
PUBLIC v2s             !  generic function returns string given numeric REAL|DOUBLEPRECISION|INTEGER value
PRIVATE d2s            !  function returns strings from doubleprecision value
PRIVATE r2s            !  function returns strings from real value
PRIVATE i2s            !  function returns strings from integer value
PUBLIC v2s_bug         !  generic function returns string given numeric REAL|DOUBLEPRECISION|INTEGER value
PRIVATE trimzeros      !  Delete trailing zeros from numeric decimal string
!----------------------# LOGICAL TESTS
PUBLIC isalnum         !  elemental function returns .true. if CHR is a letter or digit
PUBLIC isalpha         !  elemental function returns .true. if CHR is a letter and .false. otherwise
PUBLIC isascii         !  elemental function returns .true. if the low order byte of c is in the range char(0) to char(127)
PUBLIC isblank         !  elemental function returns .true. if CHR is a blank character (space or horizontal tab.
PUBLIC iscntrl         !  elemental function returns .true. if CHR is a delete character or ordinary control character
PUBLIC isdigit         !  elemental function returns .true. if CHR is a digit (0,1,...,9) and .false. otherwise
PUBLIC isgraph         !  elemental function true if CHR is an ASCII printable character except considers a space non-printable
PUBLIC islower         !  elemental function returns .true. if CHR is a miniscule letter (a-z)
PUBLIC isprint         !  elemental function determines if CHR is an ASCII printable character
PUBLIC ispunct         !  elemental function returns .true. if CHR is a printable punctuation character
PUBLIC isspace         !  elemental function true if CHR is a null, space, tab, carriage return, new line, vertical tab, or formfeed
PUBLIC isupper         !  elemental function returns .true. if CHR is an uppercase letter (A-Z)
PUBLIC isxdigit        !  elemental function returns .true. if CHR is a hexadecimal digit (0-9, a-f, or A-F).
!----------------------#
PUBLIC describe        !  returns a string describing character
!----------------------#
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=*),parameter :: ident1=&
                 &"@(#)M_strings::switch(3f): toggle between string and array of characters; generic{a2s,s2a}"
interface switch
   module procedure a2s, s2a
end interface switch
! note how returned result is "created" by the function
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=*),parameter :: ident2=&
                 &"@(#)M_strings::string_to_value(3f): Generic subroutine converts numeric string to a number (a2d,a2r,a2i)"
interface string_to_value
   module procedure a2d, a2r, a2i
end interface
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=*),parameter :: ident4=&
                 &"@(#)M_strings::v2s(3f): Generic function returns string given REAL|INTEGER|DOUBLEPRECISION value(d2s,r2s,i2s)"
interface v2s
   module procedure d2s, r2s, i2s
end interface
!-----------------------------------------------------------------------------------------------------------------------------------
integer, parameter,public :: IPcmd=32768                        ! length of command
!-----------------------------------------------------------------------------------------------------------------------------------
! ASCII character constants
character, public, parameter :: ascii_nul = char(0)   ! null
character, public, parameter :: ascii_bel = char(7)   ! bell
character, public, parameter :: ascii_bs  = char(8)   ! backspace
character, public, parameter :: ascii_ht  = char(9)   ! horizontal tab
character, public, parameter :: ascii_lf  = char(10)  ! line feed or newline
character, public, parameter :: ascii_ff  = char(12)  ! form feed or newpage
character, public, parameter :: ascii_cr  = char(13)  ! carriage return
character, public, parameter :: ascii_esc = char(27)  ! escape
!-----------------------------------------------------------------------------------------------------------------------------------

CONTAINS
!>
!!##NAME
!!    matchw - [M_strings]compare given string for match to pattern which may contain wildcard characters
!!
!!##SYNOPSIS
!!
!!    logical function matchw(string, pattern )
!!
!!     character(len=*),intent(in) :: string
!!     character(len=*),intent(in) :: pattern
!!
!!##DESCRIPTION
!!
!!    matchw(3f) compares given string for match to pattern which may
!!    contain wildcard characters.
!!
!!    In this version to get a match entire string must be described by pattern.
!!
!!       o  "?" matching any one character
!!       o  "*" matching zero or more characters.
!!       o  Both strings may have trailing spaces which are ignored.
!!
!!##EXAMPLES
!!
!!
!!   Example program
!!
!!    program demo_matchw
!!    use M_strings, only : matchw
!!
!!    ! first match is not all of string so F
!!    write(*,*)matchw('*c*ax ','abcdefgaxaxaxax')
!!    ! true
!!    write(*,*)matchw('*c*ax*','abcdefgaxaxaxax')
!!
!!    write(*,*)merge('MATCH','ERROR',matchw('abcdefgaxaxaxax','*c*ax*'))
!!    write(*,*)merge('MATCH','ERROR',matchw('abcdefgaxaxaxax','*c??f*'))
!!    write(*,*)merge('ERROR','NO   ',matchw('abcdefgaxaxaxax','*a??f'))
!!    write(*,*)merge('ERROR','NO   ',matchw('abcdefgaxaxaxax','*y'))
!!
!!    end program demo_matchw
!!
!!   Expected output
!!
!!     > F
!!     > T
!!     > MATCH
!!     > MATCH
!!     > NO
!!     > NO
!!
!!   More extensive example
!!
!!    program test_matchw
!!    use M_strings, only : matchw
!!    !implicit none
!!    integer np, ns
!!    parameter (np =  19, ns =  6)
!!    character pattern(np)*8, string(ns)*12
!!    character pattern2(np)*8
!!    integer s, p
!!    data pattern /'*','a*a','a*','ab*','*a','a*a','a?d?','a?d*','abra',&
!!    & 'aa','a','ab','*','?','????','?*','*?','***?','****?'/
!!    data pattern2/'*','a**a','a*d?','ab*','*a','a*a','a?d?','a?d*','alda',&
!!    & 'aa','a','ab','*','?','???a','????','**','***a','?????'/
!!    data string / 'abracadabra', 'aldabra', 'alda', 'carta', 'abdc', 'abra'/
!!
!!       write(*,'("TABLE 1",t18, *(a6))') pattern
!!       do s = 1,ns
!!          write(*, '(a, 100L6)') &
!!           & string(s),(matchw(string(s),pattern(p)), p=1,np)
!!       enddo
!!
!!       write(*,'("TABLE 2",t18, *(a6))') pattern2
!!       do s = 1,ns
!!          write(*, '(a, 100L6)') &
!!           & string(s),(matchw(string(s),pattern2(p)), p=1,np)
!!       enddo
!!
!!       stop
!!
!!       do s = 1,ns
!!          do p=1,np
!!          write(*, '(a,a,L7)') &
!!           & string(s),pattern2(p),matchw(string(s),pattern2(p))
!!          enddo
!!       enddo
!!
!!    end program test_matchw
!!
!!   Expected output
!!
!!    TABLE 1     * a*a  a*   ab* *a a*a a?d? a?d* abra aa a ab * ? ???? ?*   *? ***? ****?
!!    abracadabra T T    T    T   T  T   F    F    F    F  F F  T F F    T    F  F    F
!!    aldabra     T T    T    F   T  T   F    T    F    F  F F  T F F    T    F  F    F
!!    alda        T T    T    F   T  T   T    T    F    F  F F  T F T    T    F  F    F
!!    carta       T F    F    F   T  F   F    F    F    F  F F  T F F    T    F  F    F
!!    abdc        T F    T    T   F  F   T    T    F    F  F F  T F T    T    F  F    F
!!    abra        T T    T    T   T  T   F    F    T    F  F F  T F T    T    F  F    F
!!    TABLE 2     * a**a a*d? ab* *a a*a a?d? a?d* alda aa a ab * ? ???a ???? ** ***a ?????
!!    abracadabra T F    F    T   T  T   F    F    F    F  F F  T F F    F    F  F    F
!!    aldabra     T F    F    F   T  T   F    T    F    F  F F  T F F    F    F  F    F
!!    alda        T F    T    F   T  T   T    T    T    F  F F  T F T    T    F  F    F
!!    carta       T F    F    F   T  F   F    F    F    F  F F  T F F    F    F  F    T
!!    abdc        T F    T    T   F  F   T    T    F    F  F F  T F F    T    F  F    F
!!    abra        T F    F    T   T  T   F    F    F    F  F F  T F T    T    F  F    F
!===================================================================================================================================
logical function matchw(string,pattern)
!  "?" matching any one character, and
!  "*" matching zero or more characters.
!  Both strings may have trailing spaces which are ignored.
! Author: Clive Page, cgp@le.ac.uk,  2003 June 24.
!
! Revised: John S. Urban
! Changed so does not report a match if pattern is matched but string is not "used up"
! Still has problems with adjacent wild-character characters
!
character(len=*),parameter::ident="@(#)M_strings::matchw(3f): compare string to pattern which may contain wildcard characters"
character(len=*),intent(in) :: pattern                            ! input: pattern may contain * and ?
character(len=*),intent(in) :: string                             ! input: string to be compared
   integer :: lenp
   integer :: lens
   integer :: n
   integer :: p
   integer :: s
!-----------------------------------------------------------------------========----------------------------------------------------
   lenp = len_trim(pattern)                                       ! find last non-blank character in pattern string
   lens = len_trim(string)                                        ! find last non-blank character in input string
   p = 1
   s = 1
   matchw = .false.
!-----------------------------------------------------------------------========----------------------------------------------------
   do                                                             ! start looping thru string
      if(pattern(p:p) .eq. '?') then                              ! accept any char in string
         p = p + 1
         s = s + 1
      else if(pattern(p:p) .eq. '*') then
         p = p + 1
         if(p .gt. lenp) then                                     ! anything goes in rest of string
            matchw = .true.
            goto 999
         else if(p .eq. lenp) then                                ! just check last char of string
            matchw = pattern(p:p) .eq. string(lens:lens)
            goto 999
         else                                                     ! search string for char at p
            n = index(string(s:), pattern(p:p))
            if(n .eq. 0) goto 999                                 ! no such char, exit false
            s = n + s - 1
         end if
      else if(pattern(p:p) .eq. string(s:s)) then                 ! single char match
         p = p + 1
         s = s + 1
      else                                                        ! non-match
         exit
      end if
      if(p .gt. lenp .or. s .gt. lens ) then                      ! end of pattern/string, exit .true. (usually)
         exit
      end if
   enddo
   if(p .gt. lenp ) then                                          ! end of pattern/string, exit .true.
      if(s.gt.lens)then
         matchw = .true.
      elseif(p.gt.lens+1)then
         matchw = .false.
      else
         matchw = .false.
      endif
   elseif(s .gt. lens) then                                       ! end of pattern/string, exit .true.
         matchw = .false.
   end if
999   continue
end function matchw
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    split - [M_strings]parse string into an array using specified delimiters
!!
!!##SYNOPSIS
!!
!!    subroutine split(input_line,array,delimiters,order,nulls)
!!
!!     character(len=*),intent(in)              :: input_line
!!     character(len=:),allocatable,intent(out) :: array(:)
!!     character(len=*),optional,intent(in)     :: delimiters
!!     character(len=*),optional,intent(in)     :: order
!!     character(len=*),optional,intent(in)     :: nulls
!!
!!##DESCRIPTION
!!     SPLIT(3f) parses a string using specified delimiter characters and
!!     store tokens into an array
!!
!!##OPTIONS
!!
!!    INPUT_LINE  Input string to tokenize
!!
!!    ARRAY       Output array of tokens
!!
!!    DELIMITERS  List of delimiter characters.
!!                The default delimiters are the "whitespace" characters
!!                (space, tab,new line, vertical tab, formfeed, carriage
!!                return, and null). You may specify an alternate set of
!!                delimiter characters.
!!
!!                Multi-character delimiters are not supported (Each
!!                character in the DELIMITERS list is considered to be
!!                a delimiter).
!!
!!                Quoting of delimiter characters is not supported.
!!
!!    ORDER SEQUENTIAL|REVERSE|RIGHT  Order of output array.
!!                By default ARRAY contains the tokens having parsed
!!                the INPUT_LINE from left to right. If ORDER='RIGHT'
!!                or ORDER='REVERSE' the parsing goes from right to left.
!!
!!    NULLS IGNORE|RETURN|IGNOREEND  Treatment of null fields.
!!                By default adjacent delimiters in the input string
!!                do not create an empty string in the output array. if
!!                NULLS='return' adjacent delimiters create an empty element
!!                in the output ARRAY. If NULLS='ignoreend' then only
!!                trailing delimiters at the right of the string are ignored.
!!
!!##EXAMPLES
!!
!!
!!    program demo_split
!!
!!     use m_strings, only: split
!!     character(len=*),parameter     :: &
!!     & line='  aBcdef   ghijklmnop qrstuvwxyz  1:|:2     333|333 a B cc    '
!!     character(len=256),allocatable :: array(:) ! output array of tokens
!!        write(*,*)'INPUT LINE:['//LINE//']'
!!     write(*,'(80("="))')
!!        write(*,*)'typical call:'
!!        CALL split(line,array)
!!        write(*,'(i0," ==> ",a)')(i,trim(array(i)),i=1,size(array))
!!        write(*,*)'SIZE:',SIZE(array)
!!     write(*,'(80("-"))')
!!      write(*,*)'custom list of delimiters (colon and vertical line):'
!!      CALL split(line,array,delimiters=':|',order='sequential',nulls='ignore')
!!      write(*,'(i0," ==> ",a)')(i,trim(array(i)),i=1,size(array))
!!      write(*,*)'SIZE:',SIZE(array)
!!     write(*,'(80("-"))')
!!      write(*,*)&
!!      &'custom list of delimiters, reverse array order and count null fields:'
!!        CALL split(line,array,delimiters=':|',order='reverse',nulls='return')
!!        write(*,'(i0," ==> ",a)')(i,trim(array(i)),i=1,size(array))
!!        write(*,*)'SIZE:',SIZE(array)
!!     write(*,'(80("-"))')
!!        write(*,*)'INPUT LINE:['//LINE//']'
!!        write(*,*)&
!!        &'default delimiters and reverse array order and return null fields:'
!!        CALL split(line,array,delimiters='',order='reverse',nulls='return')
!!        write(*,'(i0," ==> ",a)')(i,trim(array(i)),i=1,size(array))
!!        write(*,*)'SIZE:',SIZE(array)
!!
!!    end program demo_split
!!
!!   Output
!!
!!    > INPUT LINE:[  aBcdef   ghijklmnop qrstuvwxyz  1:|:2     333|333 a B cc    ]
!!    > ===========================================================================
!!    >  typical call:
!!    > 1 ==> aBcdef
!!    > 2 ==> ghijklmnop
!!    > 3 ==> qrstuvwxyz
!!    > 4 ==> 1:|:2
!!    > 5 ==> 333|333
!!    > 6 ==> a
!!    > 7 ==> B
!!    > 8 ==> cc
!!    >  SIZE:           8
!!    > --------------------------------------------------------------------------
!!    >  custom list of delimiters (colon and vertical line):
!!    > 1 ==>   aBcdef   ghijklmnop qrstuvwxyz  1
!!    > 2 ==> 2     333
!!    > 3 ==> 333 a B cc
!!    >  SIZE:           3
!!    > --------------------------------------------------------------------------
!!    >  custom list of delimiters, reverse array order and return null fields:
!!    > 1 ==> 333 a B cc
!!    > 2 ==> 2     333
!!    > 3 ==>
!!    > 4 ==>
!!    > 5 ==>   aBcdef   ghijklmnop qrstuvwxyz  1
!!    >  SIZE:           5
!!    > --------------------------------------------------------------------------
!!    >  INPUT LINE:[  aBcdef   ghijklmnop qrstuvwxyz  1:|:2     333|333 a B cc    ]
!!    >  default delimiters and reverse array order and count null fields:
!!    > 1 ==>
!!    > 2 ==>
!!    > 3 ==>
!!    > 4 ==> cc
!!    > 5 ==> B
!!    > 6 ==> a
!!    > 7 ==> 333|333
!!    > 8 ==>
!!    > 9 ==>
!!    > 10 ==>
!!    > 11 ==>
!!    > 12 ==> 1:|:2
!!    > 13 ==>
!!    > 14 ==> qrstuvwxyz
!!    > 15 ==> ghijklmnop
!!    > 16 ==>
!!    > 17 ==>
!!    > 18 ==> aBcdef
!!    > 19 ==>
!!    > 20 ==>
!!    >  SIZE:          20
!===================================================================================================================================
   subroutine split(input_line,array,delimiters,order,nulls)
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=*),parameter::ident="@(#)M_strings::split(3f): parse string on delimiter characters and store tokens into an array"
!  John S. Urban
!-----------------------------------------------------------------------------------------------------------------------------------
   intrinsic index, min, present, len
!-----------------------------------------------------------------------------------------------------------------------------------
!  given a line of structure " par1 par2 par3 ... parn " store each par(n) into a separate variable in array.
!    o by default  adjacent delimiters in the input string do not create an empty string in the output array
!    o no quoting of delimiters is supported
   character(len=*),intent(in)              :: input_line  ! input string to tokenize
   character(len=*),optional,intent(in)     :: delimiters  ! list of delimiter characters
   character(len=*),optional,intent(in)     :: order       ! order of output array sequential|[reverse|right]
   character(len=*),optional,intent(in)     :: nulls       ! return strings composed of delimiters or not ignore|return|ignoreend
   !x!character(len=:),allocatable,intent(out) :: array(:)    ! output array of tokens
   character(len=*),allocatable,intent(out) :: array(:)    ! output array of tokens
!-----------------------------------------------------------------------------------------------------------------------------------
   integer                       :: n                      ! max number of strings INPUT_LINE could split into if all delimiter
   integer,allocatable           :: ibegin(:)              ! positions in input string where tokens start
   integer,allocatable           :: iterm(:)               ! positions in input string where tokens end
   character(len=:),allocatable  :: dlim                   ! string containing delimiter characters
   character(len=:),allocatable  :: ordr                   ! string containing order keyword
   character(len=:),allocatable  :: nlls                   ! string containing order keyword
   integer                       :: ii,iiii                ! loop parameters used to control print order
   integer                       :: icount                 ! number of tokens found
   integer                       :: ilen                   ! length of input string with trailing spaces trimmed
   integer                       :: i10,i20,i30            ! loop counters
   integer                       :: icol                   ! pointer into input string as it is being parsed
   integer                       :: idlim                  ! number of delimiter characters
   integer                       :: ifound                 ! where next delimiter character is found in remaining input string data
   integer                       :: inotnull               ! count strings not composed of delimiters
   integer                       :: ireturn                ! number of tokens returned
   integer                       :: imax                   ! length of longest token
!-----------------------------------------------------------------------------------------------------------------------------------
   ! decide on value for optional DELIMITERS parameter
   if (present(delimiters)) then                                     ! optional delimiter list was present
      if(delimiters.ne.'')then                                       ! if DELIMITERS was specified and not null use it
         dlim=delimiters
      else                                                           ! DELIMITERS was specified on call as empty string
         dlim=' '//char(9)//char(10)//char(11)//char(12)//char(13)//char(0) ! use default delimiter when not specified
      endif
   else                                                              ! no delimiter value was specified
      dlim=' '//char(9)//char(10)//char(11)//char(12)//char(13)//char(0)    ! use default delimiter when not specified
   endif
   idlim=len(dlim)                                                   ! dlim a lot of blanks on some machines if dlim is a big string
!-----------------------------------------------------------------------------------------------------------------------------------
   if(present(order))then; ordr=lower(adjustl(order)); else; ordr='sequential'; endif ! decide on value for optional ORDER parameter
   if(present(nulls))then; nlls=lower(adjustl(nulls)); else; nlls='ignore'    ; endif ! optional parameter
!-----------------------------------------------------------------------------------------------------------------------------------
   n=len(input_line)+1                        ! max number of strings INPUT_LINE could split into if all delimiter
   allocate(ibegin(n))                        ! allocate enough space to hold starting location of tokens if string all tokens
   allocate(iterm(n))                         ! allocate enough space to hold ending  location of tokens if string all tokens
   ibegin(:)=1
   iterm(:)=1
!-----------------------------------------------------------------------------------------------------------------------------------
   ilen=len(input_line)                                           ! ILEN is the column position of the last non-blank character
   icount=0                                                       ! how many tokens found
   inotnull=0                                                     ! how many tokens found not composed of delimiters
   imax=0                                                         ! length of longest token found
!-----------------------------------------------------------------------------------------------------------------------------------
   select case (ilen)
!-----------------------------------------------------------------------------------------------------------------------------------
   case (:0)                                                      ! command was totally blank
!-----------------------------------------------------------------------------------------------------------------------------------
   case default                                                   ! there is at least one non-delimiter in INPUT_LINE if get here
      icol=1                                                      ! initialize pointer into input line
      INFINITE: do i30=1,ilen,1                                   ! store into each array element
         ibegin(i30)=icol                                         ! assume start new token on the character
         if(index(dlim(1:idlim),input_line(icol:icol)).eq.0)then  ! if current character is not a delimiter
            iterm(i30)=ilen                                       ! initially assume no more tokens
            do i10=1,idlim                                        ! search for next delimiter
               ifound=index(input_line(ibegin(i30):ilen),dlim(i10:i10))
               IF(ifound.gt.0)then
                  iterm(i30)=min(iterm(i30),ifound+ibegin(i30)-2)
               endif
            enddo
            icol=iterm(i30)+2                                     ! next place to look as found end of this token
            inotnull=inotnull+1                                   ! increment count of number of tokens not composed of delimiters
         else                                                     ! character is a delimiter for a null string
            iterm(i30)=icol-1                                     ! record assumed end of string. Will be less than beginning
            icol=icol+1                                           ! advance pointer into input string
         endif
         imax=max(imax,iterm(i30)-ibegin(i30)+1)
         icount=i30                                               ! increment count of number of tokens found
         if(icol.gt.ilen)then                                     ! text left
            exit INFINITE
         endif
      enddo INFINITE
!-----------------------------------------------------------------------------------------------------------------------------------
   end select
!-----------------------------------------------------------------------------------------------------------------------------------
   select case (trim(adjustl(nlls)))
   case ('ignore','','ignoreend')
      ireturn=inotnull
   case default
      ireturn=icount
   end select
   !X!allocate(character(len=imax) :: array(ireturn))                ! allocate the array to return
   allocate(array(ireturn))                                      ! allocate the array to turn
!-----------------------------------------------------------------------------------------------------------------------------------
   select case (trim(adjustl(ordr)))                              ! decide which order to store tokens
   case ('reverse','right') ; ii=ireturn ; iiii=-1                ! last to first
   case default             ; ii=1       ; iiii=1                 ! first to last
   end select
!-----------------------------------------------------------------------------------------------------------------------------------
   do i20=1,icount                                                ! fill the array with the tokens that were found
      if(iterm(i20).lt.ibegin(i20))then
         select case (trim(adjustl(nlls)))
         case ('ignore','','ignoreend')
         case default
            array(ii)=' '
            ii=ii+iiii
         end select
      else
         array(ii)=input_line(ibegin(i20):iterm(i20))
         ii=ii+iiii
      endif
   enddo
!-----------------------------------------------------------------------------------------------------------------------------------
   end subroutine split
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    chomp - [M_strings]Tokenize a string, consuming it one token per call
!!
!!##SYNOPSIS
!!
!!    function chomp(source_string,token[,delimiters])
!!
!!     character(len=*)                     :: source_string
!!     character(len=:),intent(out),token   :: token
!!     character(len=:),intent(in),optional :: delimiters
!!     integer                              :: chomp
!!
!!##DESCRIPTION
!!    The CHOMP(3f) function is used to isolate sequential tokens in a
!!    string, SOURCE_STRING. These tokens are delimited in the string by at
!!    least one of the characters in DELIMITERS. This routine consumes the
!!    source_string one token per call. It returns -1 when complete. The
!!    default delimiter list is "space,tab,carriage return,newline".
!!
!!##OPTIONS
!!     SOURCE_STRING  string to tokenize
!!     DELIMITERS     list of separator characters
!!
!!##RETURNS
!!     TOKEN          returned token
!!     CHOMP          status flag. 0 = success, -1 = no tokens remain
!!
!!##EXAMPLES
!!
!!
!!    program test_chomp
!!
!!       use m_strings, only : chomp
!!       implicit none
!!       character(len=100)            :: inline
!!       character(len=:),allocatable  :: token
!!       character(len=*),parameter    :: delimiters=' ;,'
!!       integer                       :: ios
!!       integer                       :: icount
!!       integer                       :: itoken
!!          icount=0
!!          do        ! read lines from stdin until end-of-file or error
!!             read (unit=*,fmt="(a)",iostat=ios) inline
!!             if(ios.ne.0)stop
!!             icount=icount+1
!!             itoken=0
!!             write(*,*)'INLINE ',trim(inline)
!!             do while ( chomp(inline,token,delimiters).ge. 0)
!!                itoken=itoken+1
!!                print *, itoken,'TOKEN=['//trim(token)//']'
!!             enddo
!!          enddo
!!
!!    end program test_chomp
!!
!!    sample input file
!!
!!     this is a test of chomp; A:B :;,C;;
!!
!!    sample output file
!!
!!     INLINE     this is a test of chomp; A:B :;,C;;
!!               1 TOKEN=[this]
!!               2 TOKEN=[is]
!!               3 TOKEN=[a]
!!               4 TOKEN=[test]
!!               5 TOKEN=[of]
!!               6 TOKEN=[chomp]
!!               7 TOKEN=[A:B]
!!               8 TOKEN=[:]
!!               9 TOKEN=[C]
!===================================================================================================================================
FUNCTION chomp(source_string,token,delimiters)
character(len=*),parameter::ident="@(#)M_strings::chomp(3f): Tokenize a string : JSU- 20151030"
character(len=*)                         :: source_string    ! string to tokenize
character(len=:),allocatable,intent(out) :: token            ! returned token
character(len=*),intent(in),optional     :: delimiters       ! list of separator characters
integer                                  :: chomp            ! returns copy of shifted source_string
   character(len=:),allocatable          :: delimiters_local
   integer                               :: token_start      ! beginning of token found if function result is .true.
   integer                               :: token_end        ! end of token found if function result is .true.
   integer                               :: isource_len
!-----------------------------------------------------------------------------------------------------------------------------------
!  calculate where token_start should start for this pass
   if(present(delimiters))then
      delimiters_local=delimiters
   else                                          ! increment start to previous end + 1
      delimiters_local=char(32)//char(09)//char(10)//char(13) ! space,horizontal tab, newline, carriage return
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   isource_len=len(source_string)                ! length of input string
!-----------------------------------------------------------------------------------------------------------------------------------
   ! find beginning of token
   token_start=1
   do while (token_start .le. isource_len)       ! step thru each character to find next delimiter, if any
      if(index(delimiters_local,source_string(token_start:token_start)) .ne. 0) then
         token_start = token_start + 1
      else
         exit
      endif
   enddo
!-----------------------------------------------------------------------------------------------------------------------------------
   token_end=token_start
   do while (token_end .le. isource_len-1)                         ! step thru each character to find next delimiter, if any
      if(index(delimiters_local,source_string(token_end+1:token_end+1)) .ne. 0) then  ! found a delimiter in next character
         exit
      endif
      token_end = token_end + 1
   enddo
   !write(*,*)'TOKEN_START ',token_start
   !write(*,*)'TOKEN_END   ',token_end
   chomp=isource_len-token_end
   if(chomp.ge.0)then
      token=source_string(token_start:token_end)
      source_string=source_string(token_end+1:)
   else
      token=''
      source_string=''
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
end function chomp
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!      delim - [M_strings]parse a string and store tokens into an array
!!##SYNOPSIS
!!
!!    subroutine delim(line,array,n,icount,ibegin,iterm,ilen,dlim)
!!
!!     character(len=*),intent(in)  :: line
!!     integer,integer(in)          :: n
!!     integer,intent(out)          :: icount
!!     character(len=*)             :: array(n)
!!     integer,intent(out)          :: ibegin(n)
!!     integer,intent(out)          :: iterm(n)
!!     integer,intent(out)          :: ilen
!!     character(len=*)             :: dlim
!!##DESCRIPTION
!!
!!      Given a LINE of structure " par1 par2 par3 ... parn "
!!      store each par(n) into a separate variable in ARRAY (UNLESS
!!      ARRAY(1).eq.'#N#')
!!
!!      Also set ICOUNT to number of elements of array initialized, and
!!      return beginning and ending positions for each element in IBEGIN(N)
!!      and ITERM(N).
!!
!!      Return position of last non-blank character (even if more
!!      than N elements were found) in ILEN
!!
!!      No quoting or escaping of delimiter is allowed, so the delimiter
!!      character can not be placed in a token.
!!
!!      No checking for more than N parameters; If any more they are ignored.
!!
!!##OPTIONS
!!    LINE      input string to parse into tokens
!!    ARRAY(N)  array that receives tokens
!!    N         size of arrays ARRAY, IBEGIN, ITERM
!!    ICOUNT    number of tokens found
!!    IBEGIN(N) starting columns of tokens found
!!    ITERM(N)  ending columns of tokens found
!!    ILEN      position of last non-blank character in input string LINE
!!    DLIM      delimiter characters
!!
!!##EXAMPLES
!!
!!
!!  program demo_delim
!!
!!     use m_strings, only: delim
!!     character(len=80) :: line
!!     character(len=80) :: dlm
!!     integer,parameter :: n=10
!!     character(len=20) :: array(n)=' '
!!     integer           :: ibegin(n),iterm(n)
!!     line=' first  second 10.3 words_of_stuff  '
!!     do i20=1,4
!!        ! change delimiter list and what is calculated or parsed
!!        if(i20.eq.1)dlm=' '
!!        if(i20.eq.2)dlm='o'
!!        if(i20.eq.3)dlm=' aeiou'    ! NOTE SPACE IS FIRST
!!        if(i20.eq.3)ARRAY(1)='#N#'  ! QUIT RETURNING STRING ARRAY
!!        if(i20.eq.4)line='AAAaBBBBBBbIIIIIi  J K L'
!!
!!        ! write out a break line composed of =========== ..
!!        write(*,'(57("="))')
!!        ! show line being parsed
!!        write(*,'(a)')'PARSING=['//trim(line)//'] on '//trim(dlm)
!!        ! call parsing procedure
!!        call delim(line,array,n,icount,ibegin,iterm,ilen,dlm)
!!        write(*,*)'number of tokens found=',icount
!!        write(*,*)'last character in column ',ilen
!!        if(icount.gt.0)then
!!           if(ilen.ne.iterm(icount))then
!!              write(*,*)'ignored from column ',iterm(icount)+1,' to ',ilen
!!           endif
!!           do i10=1,icount
!!              ! check flag to see if ARRAY() was set
!!              if(array(1).ne.'#N#')then
!!                 ! from returned array
!!                 write(*,'(a,a,a)',advance='no')&
!!                 &'[',array(i10)(:iterm(i10)-ibegin(i10)+1),']'
!!              endif
!!           enddo
!!           ! using start and end positions in IBEGIN() and ITERM()
!!           write(*,*)
!!           do i10=1,icount
!!              ! from positions in original line
!!              write(*,'(a,a,a)',advance='no')&
!!              &'[',line(ibegin(i10):iterm(i10)),']'
!!           enddo
!!           write(*,*)
!!        endif
!!     enddo
!!  end program demo_delim
!!
!!  Expected output
!!
!!    ========================================================
!!    PARSING=[ first  second 10.3 words_of_stuff] on
!!     number of tokens found=           4
!!     last character in column           34
!!    [first][second][10.3][words_of_stuff]
!!    [first][second][10.3][words_of_stuff]
!!    ========================================================
!!    PARSING=[ first  second 10.3 words_of_stuff] on o
!!     number of tokens found=           4
!!     last character in column           34
!!    [ first  sec][nd 10.3 w][rds_][f_stuff]
!!    [ first  sec][nd 10.3 w][rds_][f_stuff]
!!    ========================================================
!!    PARSING=[ first  second 10.3 words_of_stuff] on  aeiou
!!     number of tokens found=          10
!!     last character in column           34
!!
!!    [f][rst][s][c][nd][10.3][w][rds_][f_st][ff]
!!    =========================================================
!!    PARSING=[AAAaBBBBBBbIIIIIi  J K L] on  aeiou
!!     number of tokens found=           5
!!     last character in column           24
!!
!!    [AAA][BBBBBBbIIIII][J][K][L]
!===================================================================================================================================
subroutine delim(line,array,n,icount,ibegin,iterm,ilen,dlim)
character(len=*),parameter::ident="@(#)M_strings::delim(3f): parse a string and store tokens into an array"
!
!     given a line of structure " par1 par2 par3 ... parn "
!     store each par(n) into a separate variable in array.
!
!     IF ARRAY(1) == '#N#' do not store into string array  (KLUDGE))
!
!     also count number of elements of array initialized, and
!     return beginning and ending positions for each element.
!     also return position of last non-blank character (even if more
!     than n elements were found).
!
!     no quoting of delimiter is allowed
!     no checking for more than n parameters, if any more they are ignored
!
      character(len=*),intent(in)    :: line
      integer,intent(in)             :: n
      character(len=*)               :: array(n)
      integer,intent(out)            :: icount
      integer,intent(out)            :: ibegin(n)
      integer,intent(out)            :: iterm(n)
      integer,intent(out)            :: ilen
      character(len=*),intent(in)    :: dlim
!-----------------------------------------------------------------------------------------------------------------------------------
      character(len=IPcmd):: line_local
      logical             :: lstore
      integer             :: i10
      integer             :: iarray
      integer             :: icol
      integer             :: idlim
      integer             :: iend
      integer             :: ifound
      integer             :: istart
!-----------------------------------------------------------------------------------------------------------------------------------
      icount=0
      ilen=len_trim(line)
      if(ilen > IPcmd)then
         call journal('*delim* input line too long')
      endif
      line_local=line

      idlim=len(dlim)
      if(idlim > 5)then
         idlim=len_trim(dlim)      ! dlim a lot of blanks on some machines if dlim is a big string
         if(idlim == 0)then
            idlim=1     ! blank string
         endif
      endif

      if(ilen == 0)then                                        ! command was totally blank
         return
      endif
!
!     there is at least one non-blank character in the command
!     ilen is the column position of the last non-blank character
!     find next non-delimiter
      icol=1

      if(array(1) == '#N#')then                                ! special flag to not store into character array
         lstore=.false.
      else
         lstore=.true.
      endif

      do iarray=1,n,1                                          ! store into each array element until done or too many words
         NOINCREMENT: do
            if(index(dlim(1:idlim),line_local(icol:icol)) == 0)then  ! if current character is not a delimiter
               istart=icol                                     ! start new token on the non-delimiter character
               ibegin(iarray)=icol
               iend=ilen-istart+1+1                            ! assume no delimiters so put past end of line
               do i10=1,idlim
                  ifound=index(line_local(istart:ilen),dlim(i10:i10))
                  if(ifound > 0)then
                     iend=min(iend,ifound)
                  endif
               enddo
               if(iend <= 0)then                               ! no remaining delimiters
                 iterm(iarray)=ilen
                 if(lstore)then
                    array(iarray)=line_local(istart:ilen)
                 endif
                 icount=iarray
                 return
               else
                 iend=iend+istart-2
                 iterm(iarray)=iend
                 if(lstore)then
                    array(iarray)=line_local(istart:iend)
                 endif
               endif
               icol=iend+2
               exit NOINCREMENT
            endif
            icol=icol+1
         enddo NOINCREMENT
!        last character in line was a delimiter, so no text left
!        (should not happen where blank=delimiter)
         if(icol > ilen)then
           icount=iarray
           if( (iterm(icount)-ibegin(icount)) < 0)then         ! last token was all delimiters
              icount=icount-1
           endif
           return
         endif
      enddo
      icount=n  ! more than n elements
end subroutine delim
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    substitute - [M_strings]Globally substitute one substring for another in string
!!
!!##SYNOPSIS
!!
!!    subroutine substitute(targetline,old,new,ierr,start,end)
!!
!!     character(len=*)              :: targetline
!!     character(len=*),intent(in)   :: old
!!     character(len=*),intent(in)   :: new
!!     integer,intent(out),optional  :: ierr
!!     integer,intent(in),optional   :: start
!!     integer,intent(in),optional   :: end
!!
!!##DESCRIPTION
!!    Globally substitute one substring for another in string.
!!
!!##OPTIONS
!!     targetline  input line to be changed
!!     old         old substring to replace
!!     new         new substring
!!     ierr        error code. iF ier = -1 bad directive, &gt;= 0 then
!!                 count of changes made
!!     start       start sets the left  margin
!!     end         end sets the right  margin
!!
!!##EXAMPLES
!!
!!   Sample Program:
!!
!!    program test_substitute
!!    use M_strings, only : substitute
!!    implicit none
!!    ! must be long enough to hold changed line
!!    character(len=80) :: targetline
!!
!!    targetline='this is the input string'
!!    write(*,*)'ORIGINAL    : '//trim(targetline)
!!
!!    ! changes the input to 'THis is THe input string'
!!    call substitute(targetline,'th','TH')
!!    write(*,*)'th => TH    : '//trim(targetline)
!!
!!    ! a null old substring means "at beginning of line"
!!    ! changes the input to 'BEFORE:this is the input string'
!!    call substitute(targetline,'','BEFORE:')
!!    write(*,*)'"" => BEFORE: '//trim(targetline)
!!
!!    ! a null new string deletes occurrences of the old substring
!!    ! changes the input to 'ths s the nput strng'
!!    call substitute(targetline,'i','')
!!    write(*,*)'i => ""     : '//trim(targetline)
!!
!!    end program test_substitute
!!
!!   Expected output
!!
!!     ORIGINAL    : this is the input string
!!     th => TH    : THis is THe input string
!!     "" => BEFORE: BEFORE:THis is THe input string
!!     i => ""     : BEFORE:THs s THe nput strng
!===================================================================================================================================
subroutine substitute(targetline,old,new,ierr,start,end)
character(len=*),parameter::ident="@(#)M_strings::substitute(3f): Globally substitute one substring for another in string"
!-----------------------------------------------------------------------------------------------------------------------------------
   character(len=*)              :: targetline         ! input line to be changed
   character(len=*),intent(in)   :: old                ! old substring to replace
   character(len=*),intent(in)   :: new                ! new substring
   character(len=len(targetline)):: dum1               ! scratch string buffers
   integer,intent(out),optional  :: ierr               ! error code. if ierr = -1 bad directive, >=0 then ierr changes made
   integer,intent(in),optional   :: start              ! start sets the left  margin
   integer,intent(in),optional   :: end                ! end sets the right  margin
!-----------------------------------------------------------------------------------------------------------------------------------
   integer                       :: ml, mr, ier1
   integer                       :: maxlengthout       ! MAXIMUM LENGTH ALLOWED FOR NEW STRING
   integer                       :: original_input_length
   integer                       :: len_old, len_new
   integer                       :: ladd
   integer                       :: ir
   integer                       :: ind
   integer                       :: il
   integer                       :: id
   integer                       :: ic
   integer                       :: ichar
!-----------------------------------------------------------------------------------------------------------------------------------
   if (present(start)) then                            ! optional starting column
      ml=start
   else
      ml=1
   endif
   if (present(end)) then                              ! optional ending column
      mr=end
   else
      mr=len(targetline)
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   ier1=0                                              ! initialize error flag/change count
   maxlengthout=len(targetline)                        ! max length of output string
   original_input_length=len_trim(targetline)          ! get non-blank length of input line
   dum1(:)=' '                                         ! initialize string to build output in
   id=mr-ml                                            ! check for window option !! change to optional parameter(s)
!-----------------------------------------------------------------------------------------------------------------------------------
   len_old=len(old)                                    ! length of old substring to be replaced
   len_new=len(new)                                    ! length of new substring to replace old substring
   if(id.le.0)then                                     ! no window so change entire input string
      il=1                                             ! il is left margin of window to change
      ir=maxlengthout                                  ! ir is right margin  of window to change
      dum1(:)=' '                                      ! begin with a blank line
   else                                                ! if window is set
      il=ml                                            ! use left margin
      ir=min0(mr,maxlengthout)                         ! use right margin or rightmost
      dum1=targetline(:il-1)                           ! begin with what's below margin
   endif                                               ! end of window settings
!-----------------------------------------------------------------------------------------------------------------------------------
   if(len_old.eq.0)then                                ! c//new/ means insert new at beginning of line (or left margin)
      ichar=len_new + original_input_length
      if(ichar.gt.maxlengthout)then
         call journal('sc','*substitute* new line will be too long')
         ier1=-1
         if (present(ierr))ierr=ier1
         return
      endif
      if(len_new.gt.0)then
         dum1(il:)=new(:len_new)//targetline(il:original_input_length)
      else
         dum1(il:)=targetline(il:original_input_length)
      endif
      targetline(1:maxlengthout)=dum1(:maxlengthout)
      ier1=1                                           ! made one change. actually, c/// should maybe return 0
      if(present(ierr))ierr=ier1
      return
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   ichar=il                                            ! place to put characters into output string
   ic=il                                               ! place looking at in input string
   loop: do
      ind=index(targetline(ic:),old(:len_old))+ic-1    ! try to find start of old string in remaining part of input in change window
      if(ind.eq.ic-1.or.ind.gt.ir)then                 ! did not find old string or found old string past edit window
         exit loop                                     ! no more changes left to make
      endif
      ier1=ier1+1                                      ! found an old string to change, so increment count of changes
      if(ind.gt.ic)then                                ! if found old string past at current position in input string copy unchanged
         ladd=ind-ic                                   ! find length of character range to copy as-is from input to output
         if(ichar-1+ladd.gt.maxlengthout)then
            ier1=-1
            exit loop
         endif
         dum1(ichar:)=targetline(ic:ind-1)
         ichar=ichar+ladd
      endif
      if(ichar-1+len_new.gt.maxlengthout)then
         ier1=-2
         exit loop
      endif
      if(len_new.ne.0)then
         dum1(ichar:)=new(:len_new)
         ichar=ichar+len_new
      endif
      ic=ind+len_old
   enddo loop
!-----------------------------------------------------------------------------------------------------------------------------------
   select case (ier1)
   case (:-1)
      call journal('sc','*substitute* new line will be too long')
   case (0)                                                ! there were no changes made to the window
   case default
      ladd=original_input_length-ic
      if(ichar+ladd.gt.maxlengthout)then
         call journal('sc','*substitute* new line will be too long')
         ier1=-1
         if(present(ierr))ierr=ier1
         return
      endif
      if(ic.lt.len(targetline))then
         dum1(ichar:)=targetline(ic:max(ic,original_input_length))
      endif
      targetline=dum1(:maxlengthout)
   end select
   if(present(ierr))ierr=ier1
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine substitute
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    change - [M_strings]change old string to new string with a directive like a line editor
!!
!!##SYNOPSIS
!!
!!    subroutine change(target_string,cmd,ierr)
!!
!!     character(len=*),intent(inout) :: target_string
!!     character(len=*),intent(in)    :: cmd
!!     integer                        :: ierr
!!
!!##DESCRIPTION
!!
!!    change an old substring into a new substring in a character variable
!!    like a line editor. Primarily used to create interactive utilities
!!    such as input history editors for interactive line-mode programs. The
!!    output string is assumed long enough to accommodate the change.
!!    a directive resembles a line editor directive of the form
!!
!!       C/old_string/new_string/
!!
!!    where / may be any character which is not included in old_string
!!    or new_string.
!!
!!    a null old_string implies "beginning of string".
!!
!!##OPTIONS
!!    target_string  line to be changed
!!    cmd            contains instructions to change the string
!!    ierr           error code.
!!
!!       o =-1 bad directive
!!       o =0 no changes made
!!       o >0 count of changes made
!!
!!##EXAMPLES
!!
!!
!!    program demo_change
!!
!!     use M_strings, only : change
!!     implicit none
!!     character(len=132) :: line='This is a test string to change'
!!     integer            :: ierr
!!        write(*,*)trim(line)
!!
!!        ! change miniscule a to uppercase A
!!        call change(line,'c/a/A/',ierr)
!!        write(*,*)trim(line)
!!
!!        ! put string at beginning of line
!!        call change(line,'c//prefix: /',ierr)
!!        write(*,*)trim(line)
!!
!!        ! remove blanks
!!        call change(line,'c/ //',ierr)
!!        write(*,*)trim(line)
!!
!!    end program demo_change
!!
!!    Expected output
!!
!!     This is a test string to change
!!     This is A test string to chAnge
!!     prefix: This is A test string to chAnge
!!     prefix:ThisisAteststringtochAnge
!===================================================================================================================================
subroutine change(target_string,cmd,ierr)
! Change a string assumed long enough to accommodate the change, with a directive that resembles a line editor directive of the form
!    C/old_string/new_string/
! where / may be any character which is not included in old_string or new_string.
! a null old_string implies "beginning of string"
!===================================================================================================================================
character(len=*),parameter::ident="@(#)M_strings::change(3f): change a character string like a line editor"
character(len=*),intent(inout)   :: target_string          ! line to be changed
character(len=*),intent(in)      :: cmd                    ! contains the instructions changing the string
character(len=1)                 :: delimiters
integer                          :: ierr                   ! error code. ier=-1 bad directive;=0 no changes made;>0 ier changes made
integer                          :: itoken
integer,parameter                :: id=2                   ! expected location of delimiter
character(len=:),allocatable     :: old,new                ! scratch string buffers
logical                          :: ifok
integer                          :: lmax                   ! length of target string
integer                          :: start_token,end_token
!-----------------------------------------------------------------------------------------------------------------------------------
   lmax=len_trim(cmd)                                                          ! significant length of change directive
   if(lmax.ge.4)then                         ! strtok ignores blank tokens so look for special case where first token is really null
      delimiters=cmd(id:id)                                                    ! find delimiter in expected location
      itoken=0                                                                 ! initialize strtok(3f) procedure

      if(strtok(cmd(id:),itoken,start_token,end_token,delimiters)) then        ! find OLD string
         old=cmd(start_token+id-1:end_token+id-1)
      else
         old=''
      endif

      if(cmd(id:id).eq.cmd(id+1:id+1))then
         new=old
         old=''
      else                                                                     ! normal case
         ifok=strtok(cmd(id:),itoken,start_token,end_token,delimiters)         ! find NEW string
         if(end_token .eq. (len(cmd)-id+1) )end_token=len_trim(cmd(id:))       ! if missing ending delimiter
         new=cmd(start_token+id-1:min(end_token+id-1,lmax))
      endif

      call substitute(target_string,old,new,ierr,1,len_trim(target_string))    ! change old substrings to new substrings
   else                                                                        ! command was two or less characters
      ierr=-1
      call journal('sc','*change* incorrect change directive -too short')
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine change
!>
!!##NAME
!!     strtok - Tokenize a string
!!##SYNOPSIS
!!
!!
!!        function strtok(source_string,itoken,token_start,token_end,delimiters)
!!                 result(strtok_status)
!!
!!        character(len=*),intent(in)  :: source_string    ! string to tokenize
!!        integer,intent(inout)        :: itoken           ! token count since started
!!        logical                      :: strtok_status    ! returned value
!!        integer,intent(out)          :: token_start      ! beginning of token
!!        integer,intent(out)          :: token_end        ! end of token
!!        character(len=*),intent(in)  :: delimiters       ! list of separator characters
!!
!!
!!##DESCRIPTION
!!     The STRTOK(3f) function is used to isolate sequential tokens in a string,
!!     SOURCE_STRING. These tokens are delimited in the string by at least one of
!!     the characters in DELIMITERS. The first time that STRTOK(3f) is called,
!!     ITOKEN should be specified as zero. Subsequent calls, wishing to obtain
!!     further tokens from the same string, should pass back in TOKEN_START and
!!     ITOKEN until the function result returns .false.
!!##EXAMPLES
!!
!!
!!     !===============================================================================
!!     program test_strtok
!!     use m_strings, only : strtok
!!     character(len=264)          :: inline
!!     character(len=*),parameter  :: delimiters=' ;,'
!!     integer                     :: ios
!!     !-------------------------------------------------------------------------------
!!        do                        ! read lines from stdin until end-of-file or error
!!           read (unit=*,fmt="(a)",iostat=ios) inline
!!           if(ios.ne.0)stop
!!           itoken=0 ! must set ITOKEN=0 before looping on strtok(3f) on a new string.
!!           do while ( strtok(inline,itoken,istart,iend,delimiters) )
!!              print *, itoken,'TOKEN=['//(inline(istart:iend))//']',istart,iend
!!           enddo
!!        enddo
!!     end program test_strtok
!!     !===============================================================================
!!
!!     sample input file
!!
!!      this is a test of strtok; A:B :;,C;;
!!
!!     sample output file
!!
!!     1  TOKEN=[this]    2   5
!!     2  TOKEN=[is]      7   8
!!     3  TOKEN=[a]       10  10
!!     4  TOKEN=[test]    12  15
!!     5  TOKEN=[of]      17  18
!!     6  TOKEN=[strtok]  20  25
!!     7  TOKEN=[A:B]     28  30
!!     8  TOKEN=[:]       32  32
!!     9  TOKEN=[C]       35  35
!===================================================================================================================================
FUNCTION strtok(source_string,itoken,token_start,token_end,delimiters) result(strtok_status)
!   DESCRIPTION:
!      The  STRTOK(3f)  function is used to isolate sequential tokens in a string, SOURCE_STRING.
!      These tokens are delimited in the  string by  at  least  one  of the characters in DELIMITERS.
!      The first time that STRTOK(3f) is called, ITOKEN should  be  specified as zero.
!      Subsequent calls, wishing  to  obtain further tokens from the same string, should pass back in TOKEN_START
!      until the function result returns .false.
character(len=*),parameter::ident="@(#)M_strings::strtok(3fp): Tokenize a string : JSU- 20151030"
character(len=*),intent(in)  :: source_string    ! Source string to tokenize.
character(len=*),intent(in)  :: delimiters       ! list of separator characters. May change between calls
integer,intent(inout)        :: itoken           ! token count since started
logical                      :: strtok_status    ! returned value
integer,intent(out)          :: token_start      ! beginning of token found if function result is .true.
integer,intent(out)          :: token_end        ! end of token found if function result is .true.
   integer,save              :: isource_len
!----------------------------------------------------------------------------------------------------------------------------
!  calculate where token_start should start for this pass
   if(itoken.le.0)then                           ! this is assumed to be the first call
      token_start=1
   else                                          ! increment start to previous end + 1
      token_start=token_end+1
   endif
!----------------------------------------------------------------------------------------------------------------------------
   isource_len=len(source_string)                ! length of input string
!----------------------------------------------------------------------------------------------------------------------------
   if(token_start.gt.isource_len)then            ! user input error or at end of string
      token_end=isource_len                      ! assume end of token is end of string until proven otherwise so it is set
      strtok_status=.false.
      return
   endif
!----------------------------------------------------------------------------------------------------------------------------
   ! find beginning of token
   do while (token_start .le. isource_len)       ! step thru each character to find next delimiter, if any
      if(index(delimiters,source_string(token_start:token_start)) .ne. 0) then
         token_start = token_start + 1
      else
         exit
      endif
   enddo
!----------------------------------------------------------------------------------------------------------------------------
   token_end=token_start
   do while (token_end .le. isource_len-1)       ! step thru each character to find next delimiter, if any
      if(index(delimiters,source_string(token_end+1:token_end+1)) .ne. 0) then  ! found a delimiter in next character
         exit
      endif
      token_end = token_end + 1
   enddo
!----------------------------------------------------------------------------------------------------------------------------
   if (token_start .gt. isource_len) then        ! determine if finished
      strtok_status=.false.                      ! flag that input string has been completely processed
   else
      itoken=itoken+1                            ! increment count of tokens found
      strtok_status=.true.                       ! flag more tokens may remain
   endif
!----------------------------------------------------------------------------------------------------------------------------
end function strtok
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    modif - [M_strings]emulate the MODIFY command from the line editor XEDIT
!!
!!##SYNOPSIS
!!
!!
!!    subroutine modif(cline,cmod)
!!
!!     character(len=*) :: cline ! input string to change
!!     character(len=*) :: cmod  ! directive provides directions on changing string
!!
!!
!!##DESCRIPTION
!!
!!   MODIF(3f) Modifies the line currently pointed at using a directive
!!   that acts much like a line editor directive.
!!   Primarily used to create interactive utilities such as input history
!!   editors for interactive line-mode programs.
!!
!!   the modify directives are as follows-
!!
!!    DIRECTIVE  EXPLANATION
!!
!!    ^STRING#   Causes the string of characters between the ^ and the
!!               next # to be inserted before the characters pointed to
!!               by the ^. an ^ or & within the string is treated as a
!!               regular character. If the closing # is not specified,
!!               MODIF(3f) inserts the remainder of the line as if a # was
!!               specified after the last nonblank character.
!!
!!               There are two exceptions. the combination ^# causes a #
!!               to be inserted before the character pointed to by the
!!               ^, and an ^ as the last character of the directives
!!               causes a blank to be inserted.
!!
!!    #          (When not the first # after an ^) causes the character
!!               above it to be deleted.
!!
!!    &          Replaces the character above it with a space.
!!
!!    (SPACE)    A space below a character leaves it unchanged.
!!
!!    Any other character replaces the character above it.
!!
!!##EXAMPLES
!!
!!
!!   Example input/output:
!!
!!    THE INPUT LINE........ 10 THIS STRING  TO BE MORTIFD
!!    THE DIRECTIVES LINE...        ^ IS THE#        D#  ^IE
!!    ALTERED INPUT LINE.... 10 THIS IS THE STRING  TO BE MODIFIED
!===================================================================================================================================
SUBROUTINE MODIF(CLINE,MOD)
character(len=*),parameter::ident="@(#)M_strings::modif(3f): Emulate the MODIFY command from the line editor XEDIT"
!
! MODIF
! =====
! ACTION- MODIFIES THE LINE CURRENTLY POINTED AT. THE INPUT STRING CLINE IS ASSUMED TO BE LONG ENOUGH TO ACCOMMODATE THE CHANGES
!         THE MODIFY  DIRECTIVES ARE AS FOLLOWS-
!
!   DIRECTIVE                       EXPLANATION
!   ---------                       ------------
!   ^STRING#   CAUSES THE STRING OF CHARACTERS BETWEEN THE ^ AND THE
!              NEXT  # TO BE INSERTED BEFORE THE CHARACTERS POINTED TO
!              BY THE ^.  AN ^ OR & WITHIN THE STRING IS TREATED AS A
!              REGULAR  CHARACTER.  IF THE CLOSING # IS NOT SPECIFIED,
!              MODIF(3f) INSERTS THE REMAINDER OFTHELINE AS IF A # WAS
!              SPECIFIED AFTER THE LAST NONBLANK CHARACTER.
!
!              THERE ARE TWO EXCEPTIONS. THE COMBINATION ^# CAUSES A #
!              TO BE INSERTED BEFORE THE CHARACTER POINTED TO  BY  THE
!              ^,  AND AN ^ AS THE LAST CHARACTER OF THE DIRECTIVES
!              CAUSES A BLANK TO BE INSERTED.
!
!   #          (WHEN NOT THE FIRST # AFTER AN ^) CAUSES THE  CHARACTER
!              ABOVE IT TO BE DELETED.
!
!   &          REPLACES THE CHARACTER ABOVE IT WITH A SPACE.
!
!   (SPACE)    A SPACE BELOW A CHARACTER LEAVES IT UNCHANGED.
!
!   ANY OTHER CHARACTER REPLACES THE CHARACTER ABOVE IT.
!
! EXAMPLE-
! THE INPUT LINE........ 10 THIS STRING  TO BE MORTIFD
! THE DIRECTIVES LINE...        ^ IS THE#        D#  ^IE
! ALTERED INPUT LINE.... 10 THIS IS THE STRING  TO BE MODIFIED
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
character(len=*) :: cline                   !STRING TO BE MODIFIED
character(len=*),intent(in) ::  mod         !STRING TO DIRECT MODIFICATION
   character(len=:),allocatable  :: cmod
   character(len=3),parameter :: c='#&^'    !ASSIGN DEFAULT EDIT CHARACTERS
   integer,parameter :: maxscra=255         !LENGTH OF SCRATCH BUFFER
   character(len=maxscra) :: dum2           !SCRATCH CHARACTER BUFFER
   logical linsrt                           !FLAG FOR INSERTING DATA ON LINE
   integer :: i, j, ic, ichar, iend, lmax, lmx1
      CMOD=MOD
      LMAX=MIN0(LEN(CLINE),MAXSCRA)         !DETERMINE MAXIMUM LINE LENGTH
      LMX1=LMAX-1                           !MAX LINE LENGTH -1
      DUM2=' '                              !INITIALIZE NEW LINE
      LINSRT=.FALSE.                        !INITIALIZE INSERT MODE
      IEND=len_trim(CMOD)                   !DETERMINE END OF MODS
      I=0                                   !CHAR COUNTER FOR MOD LINE CMOD
      IC=0                                  !CHAR COUNTER FOR CURRENT LINE CLINE
      ICHAR=0                               !CHAR COUNTER NEW LINE DUM2
11    CONTINUE
      I=I+1                                 !NEXT CHAR IN MOD LINE
      IF(ICHAR.GT.LMX1)GOTO 999             !IF TOO MANY CHARS IN NEW LINE
      IF(LINSRT) THEN                       !IF INSERTING NEW CHARS
         IF(I.GT.IEND) CMOD(I:I)=C(1:1)     !FORCE END OF INSERT MODE
         IF(CMOD(I:I).EQ.C(1:1))THEN        !IF END OF INSERT MODE
            LINSRT=.FALSE.                  !RESET INSERT MODE FLAG
            IF(IC+1.EQ.I)THEN               !NULL INSERT STRING
               ICHAR=ICHAR+1                !INCREMENT COUNTER FOR NEW LINE
               DUM2(ICHAR:ICHAR)=C(1:1)     !INSERT INSERT MODE TERMINATOR
            ENDIF
            DO    J=IC,I                    !LOOP OF NUMBER OF CHARS INSERTED
               ICHAR=ICHAR+1                !INCREMENT COUNTER FOR NEW LINE
               IF(ICHAR.GT.LMAX)GOTO 999    !IF AT BUFFER LIMIT, QUIT
               DUM2(ICHAR:ICHAR)=CLINE(J:J) !APPEND CHARS FROM ORIG LINE
            ENDDO                           !...WHICH ALIGN WITH INSERTED CHARS
            IC=I                            !RESET CHAR COUNT TO END OF INSERT
            GOTO 1                          !CHECK NEW LINE LENGTH AND CYCLE
         ENDIF                              !END OF TERMINATED INSERT LOGIC
         ICHAR=ICHAR+1                      !INCREMENT NEW LINE COUNT
         DUM2(ICHAR:ICHAR)=CMOD(I:I)        !SET NEWLINE CHAR TO INSERTED CHAR
      ELSE                                  !IF NOT INSERTING CHARACTERS
         IC=IC+1                            !INCREMENT ORIGINAL LINE COUNTER
         IF(CMOD(I:I).EQ.C(1:1))GOTO 1      !IF DELETE CHAR. NO COPY AND CYCLE
         IF(CMOD(I:I).EQ.C(3:3))THEN        !IF BEGIN INSERT MODE
            LINSRT=.TRUE.                   !SET INSERT FLAG TRUE
            GOTO 1                          !CHECK LINE LENGTH AND CONTINUE
         ENDIF                              !IF NOT BEGINNING INSERT MODE
         ICHAR=ICHAR+1                      !INCREMENT NEW LINE COUNTER
         IF(CMOD(I:I).EQ.C(2:2))THEN        !IF REPLACE WITH BLANK
            DUM2(ICHAR:ICHAR)=' '           !SET NEWLINE CHAR TO BLANK
            GOTO 1                          !CHECK LINE LENGTH AND CYCLE
         ENDIF                              !IF NOT REPLACE WITH BLANK
         IF(CMOD(I:I).EQ.' ')THEN           !IF BLANK, KEEP ORIGINAL CHARACTER
            DUM2(ICHAR:ICHAR)=CLINE(IC:IC)  !SET NEW CHAR TO ORIGINAL CHAR
         ELSE                               !IF NOT KEEPING OLD CHAR
            DUM2(ICHAR:ICHAR)=CMOD(I:I)     !REPLACE ORIGINAL CHAR WITH NEW
         ENDIF                              !END CHAR KEEP OR REPLACE
      ENDIF                                 !END INSERT OR NO-INSERT
1     CONTINUE
      IF(I.LT.LMAX)GOTO 11                  !CHECK FOR END OF LINE REACHED
                                            !AND CYCLE IF OK
999   CONTINUE
      CLINE=DUM2                            !SET ORIGINAL CHARS TO NEW CHARS
END SUBROUTINE MODIF                        !RETURN
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!      len_white - [M_strings]get length of string trimmed of whitespace.
!!
!!##SYNOPSIS
!!
!!    integer function len_white(string)
!!
!!     character(len=*) :: string
!!
!!##DESCRIPTION
!!
!!      len_white(string) returns the position of the last
!!      character in string that is not a whitespace character.
!!      The Fortran90 intrinsic LEN_TRIM() should be used when
!!      trailing whitespace can be assumed to always be spaces.
!!
!!      This heavily used in the past because ANSI FORTRAN 77
!!      character objects are fixed length and blank padded and
!!      the LEN_TRIM() intrinsic did not exist. It should now
!!      be used only when whitespace characters other than blanks
!!      are likely.
!!
!!##EXAMPLE
!!
!!
!!   Sample Program:
!!
!!    program demo_len_white
!!
!!     use M_strings, only : len_white
!!     character(len=80) ::  s
!!     intrinsic len
!!
!!     s=' ABCDEFG abcdefg '
!!     ilen = len(s)
!!     lastnb = len_white(s)
!!
!!     write(*,*) 'total length of variable is ',ilen
!!     write(*,*) 'trimmed length of variable is ',lastnb
!!     write(*,*) 'trimmed string=[',s(:lastnb),']'
!!
!!    end program demo_len_white
!!
!!##NOTES
!!
!! o len_white
!!
!!      is a resource-intensive routine. Once the end of
!!      the string is found, it is probably best to keep track of it in
!!      order to avoid repeated calls to len_white. Because they
!!      might be more efficient, consider looking for vendor-supplied or
!!      system-optimized equivalents. For example:
!!
!!         o lnblnk - Solaris f77
!!         o len_trim - FORTRAN 90
!!
!! o
!!      Some compilers seem to have trouble passing a string of variable
!!      length properly. To be safe, use something like this:
!!
!!       subroutine message(s)
!!        character(len=*) :: s ! s is of variable length
!!           ilen=len(s)        ! get total length of variable
!!           ! explicitly specify a substring instead of just variable name
!!           lastnb = len_white(s(:ilen))
!!           write(*,*)'error:[',s(:lastnb),']'
!!       end subroutine messages
!===================================================================================================================================
elemental integer function len_white(string)
!  DEPRECATED. Use len_trim(3f),trim(3f) unless you might have trailing nulls (common when interacting with C procedures)"
!  John S. Urban, 1984, 1997-12-31
!  Note that if the string is blank, a length of 0 is returned; which is not a legal string length in Fortran77.
!  this routine used to return one instead of zero.
!   - mod 1:     1994
!                added null (char(0)) because HP and some Suns not padding
!                strings with blank, but with null characters; 1994 JSU
!   - mod 2:     1999
!                update syntax with INTENT(), ENDDO, no RETURN
!                still need instead of LEN_TRIM() because some systems stil pad CHARACTER with NULL
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=*),parameter::ident="@(#)M_strings::len_white(3f): return position of last non-blank/non-null character in string"
character(len=*),intent(in):: string ! input string to determine length of
integer                    :: i10
intrinsic len
   len_white=0
   do i10=len(string),1,-1
      select case(string(i10:i10))
      case(' ')                 ! space(32)
      case(char(0))             ! null(0)
      case(char(9):char(13))    ! tab(9), new line(10), vertical tab(11), formfeed(12), carriage return(13)
      case default
         len_white=i10
         exit
      end select
   enddo
end function len_white
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    crop - [M_strings]trim leading blanks and trailing blanks from a string
!!
!!##SYNOPSIS
!!
!!    function crop(strin) result (strout)
!!
!!     character(len=*),intent(in)  :: strin
!!     character(len=:),allocatable :: strout
!!
!!##DESCRIPTION
!!    trim leading blanks from a string and return position of last
!!    non-blank character in the string.
!!
!!##EXAMPLE
!!
!!
!!    program demo_crop
!!
!!     use m_strings, only: crop
!!     implicit none
!!     character(len=20) ::  untrimmed = '   ABCDEFG abcdefg  '
!!        write(*,*) 'untrimmed string=[',untrimmed,']'
!!        write(*,*) 'cropped string=[',crop(untrimmed),']'
!!
!!    end program demo_crop
!!
!!    Expected output
!!
!!      untrimmed string=[   ABCDEFG abcdefg                      ]
!!      cropped string=[ABCDEFG abcdefg]
!===================================================================================================================================
function crop(strin) result (strout)
use M_journal, only : journal
character(len=*),parameter::ident="@(#)M_strings::crop(3f): trim leading and trailings blanks from string"
character(len=*),intent(in)  :: strin
character(len=:),allocatable :: strout
   strout=trim(adjustl(strin))
end function crop
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    transliterate - [M_strings]replace characters from old set with new set
!!
!!##SYNOPSIS
!!
!!    pure function transliterate(instr,old_set,new_set) result(outstr)
!!
!!     character(len=*),intent(in)  :: instr
!!     character(len=*),intent(in)  :: old_set
!!     character(len=*),intent(in)  :: new_set
!!     character(len=len(instr))    :: outstr
!!
!!##DESCRIPTION
!!    Translate, squeeze, and/or delete characters from the input string.
!!
!!     o Each character in the input string that matches a character in
!!       the old set is replaced.
!!     o If the new_set is the empty set the matched characters are deleted.
!!     o If the new_set is shorter than the old set the last character in the
!!       new set is used to replace the remaining characters in the new set.
!!
!!##EXAMPLES
!!
!!    program demo_transliterate
!!
!!     use M_strings, only : transliterate
!!     implicit none
!!     character(len=80)   :: STRING
!!
!!     STRING='aAbBcCdDeEfFgGhHiIjJkKlLmMnNoOpPqQrRsStTuUvVwWxXyYzZ'
!!     write(*,'(a)') STRING
!!
!!     ! convert a string to uppercase:
!!     write(*,*) TRANSLITERATE(STRING,'abcdefghijklmnopqrstuvwxyz','ABCDEFGHIJKLMNOPQRSTUVWXYZ')
!!
!!     ! change all miniscule letters to a colon (":"):
!!     write(*,*) TRANSLITERATE(STRING,'abcdefghijklmnopqrstuvwxyz',':')
!!
!!     ! delete all miniscule letters
!!     write(*,*) TRANSLITERATE(STRING,'abcdefghijklmnopqrstuvwxyz','')
!!
!!    end program demo_transliterate
!!
!!    Expected output
!!
!!     > aAbBcCdDeEfFgGhHiIjJkKlLmMnNoOpPqQrRsStTuUvVwWxXyYzZ
!!     > AABBCCDDEEFFGGHHIIJJKKLLMMNNOOPPQQRRSSTTUUVVWWXXYYZZ
!!     > :A:B:C:D:E:F:G:H:I:J:K:L:M:N:O:P:Q:R:S:T:U:V:W:X:Y:Z
!!     > ABCDEFGHIJKLMNOPQRSTUVWXYZ
!===================================================================================================================================
PURE FUNCTION transliterate(instr,old_set,new_set) RESULT(outstr)
character(len=*),parameter::ident="@(#)M_strings::transliterate(3f): replace characters from old set with new set"
!-----------------------------------------------------------------------------------------------------------------------------------
CHARACTER(LEN=*),INTENT(IN)  :: instr                             ! input string to change
CHARACTER(LEN=*),intent(in)  :: old_set
CHARACTER(LEN=*),intent(in)  :: new_set
!-----------------------------------------------------------------------------------------------------------------------------------
CHARACTER(LEN=LEN(instr))    :: outstr                            ! output string to generate
!-----------------------------------------------------------------------------------------------------------------------------------
INTEGER                      :: i10                               ! loop counter for stepping thru string
INTEGER                      :: ii,jj
!-----------------------------------------------------------------------------------------------------------------------------------
   jj=LEN(new_set)
   IF(jj.NE.0)THEN
      outstr=instr                                                ! initially assume output string equals input string
      stepthru: DO i10 = 1, LEN(instr)
         ii=iNDEX(old_set,instr(i10:i10))                         ! see if current character is in old_set
         IF (ii.NE.0)THEN
            if(ii.le.jj)then                                      ! use corresponding character in new_set
               outstr(i10:i10) = new_set(ii:ii)
            else
               outstr(i10:i10) = new_set(jj:jj)                   ! new_set not as long as old_set; use last character in new_set
            endif
         ENDIF
      ENDDO stepthru
   else                                                           ! new_set is null string so delete characters in old_set
      outstr=' '
      hopthru: DO i10 = 1, LEN(instr)
         ii=iNDEX(old_set,instr(i10:i10))                         ! see if current character is in old_set
         IF (ii.EQ.0)THEN                                         ! only keep characters not in old_set
            jj=jj+1
            outstr(jj:jj) = instr(i10:i10)
         ENDIF
      ENDDO hopthru
   endif
END FUNCTION transliterate
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!      reverse - [M_strings]Return a string reversed
!!
!!##SYNOPSIS
!!
!!
!!    elemental pure function reverse(str) result (string)
!!
!!     character(*), intent(in) :: str
!!     character(len(str))      :: string
!!
!!##DESCRIPTION
!!      reverse(string) returns a copy of the input string with
!!      all characters reversed from right to left.
!!
!!##EXAMPLE
!!
!!
!!    Sample call
!!
!!       program demo_reverse
!!       use M_strings, only: reverse
!!       implicit none
!!       character(len=:),allocatable  :: s
!!          s='abcdefghijklmnopqrstuvwxyz'
!!          write(*,*) 'original input string is ....',s
!!          write(*,*) 'reversed output string is ...',reverse(s)
!!       end program demo_reverse
!!
!!    Expected output
!!
!!      original input string is ....abcdefghijklmnopqrstuvwxyz
!!      reversed output string is ...zyxwvutsrqponmlkjihgfedcba
!===================================================================================================================================
elemental function reverse(string ) result (rev)
character(len=*),parameter::ident="@(#)M_strings::reverse(3f): Return a string reversed"
character(len=*),intent(in)    :: string   ! string to reverse
character(len=len(string))     :: rev      ! return value (reversed string)
   integer                     :: length
   integer                     :: i
   length = len(string)
   do i = 1,length
      rev(i:i)=string(length-i+1:length-i+1)
   enddo
end function reverse
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!      upper - [M_strings]changes a string to uppercase
!!
!!##SYNOPSIS
!!
!!    elemental pure function upper(str,begin,end) result (string)
!!
!!     character(*), intent(in)    :: str
!!     integer,optional,intent(in) :: begin,end
!!     character(len(str))         :: string  ! output string
!!
!!##DESCRIPTION
!!      upper(string) returns a copy of the input string with all characters
!!      converted in the optionally specified ran to uppercase, assuming
!!      ASCII character sets are being used. If no range is specified the
!!      entire string is converted to uppercase.
!!
!!##OPTIONS
!!    str    string to convert to uppercase
!!    begin  optional starting position in "str" to begin converting to uppercase
!!    end    optional ending position in "str" to stop converting to uppercase
!!
!!##RESULTS
!!    upper  copy of the input string with all characters converted to uppercase
!!           over optionally specified range.
!!
!!##EXAMPLE
!!
!!
!!    Sample call
!!
!!     program demo_upper
!!     use M_strings, only: upper
!!     implicit none
!!     character(len=:),allocatable  :: s
!!        s=' ABCDEFG abcdefg '
!!        write(*,*) 'mixed-case input string is ....',s
!!        write(*,*) 'upper-case output string is ...',upper(s)
!!        write(*,*) 'make first character uppercase  ... ',upper('this is a sentence.',1,1)
!!        write(*,'(1x,a,*(a:,"+"))') 'UPPER(3f) is elemental ==>',upper(["abc","def","ghi"])
!!     end program demo_upper
!!
!!    Expected output
!!
!!     mixed-case input string is .... ABCDEFG abcdefg
!!     upper-case output string is ... ABCDEFG ABCDEFG
!!     make first character uppercase  ... This is a sentence.
!!     UPPER(3f) is elemental ==>ABC+DEF+GHI
!===================================================================================================================================
!===================================================================================================================================
! Timing
!
!    Several different methods have been proposed for changing case.
!    A simple program that copies a large file and converts it to
!    uppercase was timed and compared to a simple copy. This was used
!    to select the default function.
!
! NULL:    83.41user  9.25system 1:37.94elapsed 94%CPU
! upper:  101.44user 10.89system 1:58.36elapsed 94%CPU
! upper2: 105.04user 10.69system 2:04.17elapsed 93%CPU
! upper3: 267.21user 11.69system 4:49.21elapsed 96%CPU
elemental pure function upper(str,begin,end) result (string)
character(len=*),parameter::ident="@(#)M_strings::upper(3f): Changes a string to uppercase"
character(*), intent(In)      :: str                 ! inpout string to convert to all uppercase
integer, intent(in), optional :: begin,end
   character(len(str))        :: string              ! output string that contains no miniscule letters
   integer                    :: i                   ! loop counter
   integer                    :: ibegin,iend
   string = str                                      ! initialize output string to input string

   ibegin = 1
   if (present(begin))then
      ibegin = max(ibegin,begin)
   endif

   iend = len_trim(str)
   if (present(end))then
      iend= min(iend,end)
   endif

   do i = ibegin, iend                               ! step thru each letter in the string in specified range
       select case (str(i:i))
       case ('a':'z')                                ! located miniscule letter
          string(i:i) = char(iachar(str(i:i))-32)    ! change miniscule letter to uppercase
       end select
   end do

end function upper
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!      lower - [M_strings]changes a string to lowercase over specified range
!!
!!##SYNOPSIS
!!
!!
!!    elemental pure function lower(str,begin,end) result (string)
!!
!!     character(*), intent(in) :: str
!!     integer,optional         :: begin, end
!!     character(len(str))      :: string  ! output string
!!
!!##DESCRIPTION
!!      lower(string) returns a copy of the input string with all characters
!!      converted to miniscule over the specified range, assuming ASCII
!!      character sets are being used. If no range is specified the entire
!!      string is converted to miniscule.
!!
!!##OPTIONS
!!    str    string to convert to miniscule
!!    begin  optional starting position in "str" to begin converting to miniscule
!!    end    optional ending position in "str" to stop converting to miniscule
!!
!!##RESULTS
!!    lower  copy of the input string with all characters converted to miniscule
!!           over optionally specified range.
!!
!!##EXAMPLE
!!
!!
!!    Sample call
!!
!!       program demo_lower
!!       use M_strings, only: lower
!!       implicit none
!!       character(len=:),allocatable  :: s
!!          s=' ABCDEFG abcdefg '
!!          write(*,*) 'mixed-case input string is ....',s
!!          write(*,*) 'lower-case output string is ...',lower(s)
!!       end program demo_lower
!!
!!    Expected output
!!
!!       mixed-case input string is .... ABCDEFG abcdefg
!!       lower-case output string is ... abcdefg abcdefg
!===================================================================================================================================
elemental pure function lower(str,begin,end) result (string)
character(len=*),parameter::ident="@(#)M_strings::lower(3f): Changes a string to lowercase over specified range"
character(*), intent(In)     :: str
character(len(str))          :: string
integer,intent(in),optional  :: begin, end
   integer                   :: i
   integer                   :: ibegin, iend
   string = str

   ibegin = 1
   if (present(begin))then
      ibegin = max(ibegin,begin)
   endif

   iend = len_trim(str)
   if (present(end))then
      iend= min(iend,end)
   endif

   do i = ibegin, iend                               ! step thru each letter in the string in specified range
      select case (str(i:i))
      case ('A':'Z')
         string(i:i) = char(iachar(str(i:i))+32)     ! change letter to miniscule
      case default
      end select
   end do

end function lower
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!
!!    switch - [M_strings]generic composition of a2s() and s2a() converts between CHARACTER scalar and array of single characters
!!
!!##SYNOPSIS
!!
!!    pure function switch(array) result (string)
!!
!!     character(len=1),intent(in) :: array(:)
!!     character(len=SIZE(array))  :: string
!!
!!    or
!!
!!    pure function switch(string) result (array)
!!
!!     character(len=1),intent(in) :: array(:)
!!     character(len=SIZE(array))  :: string
!!
!!##DESCRIPTION
!!
!!    SWITCH(3f): generic function that switches CHARACTER string to an array
!!    of single characters or an array of single characters to a CHARACTER
!!    string. Useful in passing strings to C.  New Fortran features may
!!    supersede these routines.
!!
!!
!!##EXAMPLES
!!
!!
!!  Sample program:
!!
!!    program demo_switch
!!    use M_strings, only : switch, isalpha, islower, nospace
!!    character(len=*),parameter :: dashes='-----------------------------------'
!!    character(len=*),parameter :: string='This is a string of letters'
!!    character(len=1024)        :: line
!!
!!    ! First, examples of standard Fortran features
!!    write(*,*)['A','=','=','=','=','='].eq.'='      ! returns array [F,T,T,T,T,T]
!!    write(*,*)all(['=','=','=','=','=','='].eq.'=') ! this would return T
!!    write(*,*)all(['A','=','=','=','=','='].eq.'=') ! this would return F
!!
!!    ! so to test if the string DASHES is all dashes using SWITCH(3f) is
!!    if(all(switch(dashes).eq.'-'))then
!!       write(*,*)'DASHES is all dashes'
!!    endif
!!
!!    ! so to test is a string is all letters
!!    ! isalpha(3f) returns .true. only if character is a letter
!!    write(*,*) all(isalpha(switch(dashes)))  ! false because dashes are not a letter
!!    write(*,*) all(isalpha(switch(string)))  ! false because of spaces
!!    write(*,*) all(isalpha(switch(nospace(string))))  ! true because removed whitespace
!!
!!    ! to see if a string is all uppercase
!!    write(*,*) string                           ! show the string
!!    write(*,'(1x,*("[",a,"]":))') switch(string)   ! converted to character array
!!    write(*,'(*(l3))') islower(switch(string))
!!
!!    line=nospace(string)                        ! we need a string that is all letters
!!    write(*,*)'LINE=',trim(line)
!!    write(*,*) islower(switch(nospace(string))) ! all true except first character
!!    write(*,*) all(islower(switch(nospace(string))))      ! should be false
!!    write(*,*) all(islower(switch(nospace(string(2:)))))  ! should be true
!!
!!    end program demo_switch
!!
!!  Expected output
!!
!!     F T T T T T
!!     T
!!     F
!!     DASHES is all dashes
!!     F
!!     F
!!     T
!!     This is a string of letters
!!     [T][h][i][s][ ][i][s][ ][a][ ][s][t][r][i][n][g][ ][o][f][ ][l][e][t][t][e][r][s]
!!      F  T  T  T  F  T  T  F  T  F  T  T  T  T  T  T  F  T  T  F  T  T  T  T  T  T  T
!!     LINE=Thisisastringofletters
!!     F T T T T T T T T T T T T T T T T T T T T T
!!     F
!!     T
!===================================================================================================================================
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
pure function a2s(array)  result (string)
character(len=*),parameter::ident="@(#)M_strings::a2s(3fp): function to copy char array to string"
character(len=1),intent(in) :: array(:)
character(len=SIZE(array))  :: string
integer                     :: i
! ----------------------------------------------------------------------------------------------------------------------------------
   forall( i = 1:size(array)) string(i:i) = array(i)
! ----------------------------------------------------------------------------------------------------------------------------------
end function a2s
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
pure function s2a(string)  RESULT (array)
character(len=*),parameter::ident="@(#)M_strings::s2a(3fp): function to copy string(1:Clen(string)) to char array"
   character(len=*),intent(in) :: string
   character(len=1)            :: array(len(string))
   integer                     :: i
! ----------------------------------------------------------------------------------------------------------------------------------
   forall(i=1:len(string)) array(i) = string(i:i)
! ----------------------------------------------------------------------------------------------------------------------------------
end function s2a
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!      s2c - [M_strings]convert character variable to array of characters with last element set to null
!!
!!##SYNOPSIS
!!
!!    function s2c(string)
!!
!!     character(len=*),intent=(in)  :: string
!!     character(len=1),allocatable  :: s2c(:)
!!##DESCRIPTION
!!    Given a character variable convert it to an array of single-character
!!    character variables with the last element set to a null character.
!!    This is generally used to pass character variables to C procedures.
!!
!!##EXAMPLES
!!
!!
!!    Sample Program:
!!
!!     program demo_s2c
!!     use M_strings, only : s2c
!!     implicit none
!!     character(len=*),parameter   :: string="single string"
!!     character(len=3),allocatable :: array(:)
!!        write(*,*)'INPUT STRING ',trim(string)
!!        ! put one character into each 3-character element of array
!!        array=s2c(string)
!!        ! write array with ASCII Decimal Equivalent below it except show
!!        ! unprintable characters like NULL as "XXX"
!!        write(*,'(1x,*("[",a3,"]":))')&
!!             & merge('XXX',array,ichar(array(:)(1:1)).lt.32)
!!        write(*,'(1x,*("[",i3,"]":))')&
!!             & ichar(array(:)(1:1))
!!     end program demo_s2c
!!
!!   Expected output:
!!
!!    INPUT STRING single string
!!    [s  ][i  ][n  ][g  ][l  ][e  ][   ][s  ][t  ][r  ][i  ][n  ][g  ][XXX]
!!    [115][105][110][103][108][101][ 32][115][116][114][105][110][103][  0]
!===================================================================================================================================
pure function s2c(string)  RESULT (array)
use,intrinsic :: ISO_C_BINDING, only : C_CHAR
character(len=*),parameter::ident="@(#)M_strings::s2c(3f): copy string(1:Clen(string)) to char array with null terminator"
character(len=*),intent(in)     :: string

! This is changing, but currently the most portable way to pass a CHARACTER variable to C is to convert it to an array of
! character variables with length one and add a null character to the end of the array. The s2c(3f) function helps do this.
   character(kind=C_CHAR,len=1) :: array(len_trim(string)+1)
   integer                      :: i
   do i = 1,size(array)-1
      array(i) = string(i:i)
   enddo
   array(size(array):)=achar(0)
end function s2c
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!      c2s - [M_strings]convert C string pointer to Fortran character string
!!
!!##SYNOPSIS
!!
!!    function c2s(c_string_pointer) result(f_string)
!!
!!     type(c_ptr), intent(in)       :: c_string_pointer
!!     character(len=:), allocatable  :: f_string
!!##DESCRIPTION
!!     Given a C pointer to a character string return a Fortran character string.
!!##OPTIONS
!!##EXAMPLE
!!
!===================================================================================================================================
function c2s(c_string_pointer) result(f_string)
! gets a C string (pointer), and returns the corresponding Fortran string;
! If the C string is null, it returns "NULL", similar to C's "(null)" printed in similar cases:
use, intrinsic :: iso_c_binding, only: c_ptr,c_f_pointer,c_char,c_null_char
character(len=*),parameter::ident="&
&@(#)M_strings::c2s(3f): copy pointer to C char array till a null is encountered to a Fortran string up to 4096 characters"
   integer,parameter                             :: max_length=4096
   type(c_ptr), intent(in)                       :: c_string_pointer
   character(len=:), allocatable                 :: f_string
   character(kind=c_char), dimension(:), pointer :: char_array_pointer => null()
   character(len=max_length)                            :: aux_string
   integer                                       :: i,length=0
   call c_f_pointer(c_string_pointer,char_array_pointer,[max_length])
   if (.not.associated(char_array_pointer)) then
     allocate(character(len=4)::f_string)
     f_string="NULL"
     return
   endif
   aux_string=" "
   do i=1,max_length

     write(*,*)'c2s got here c2 ',i
     write(*,*)'c2s got here c2 ',i,size(char_array_pointer)
     write(*,*)'c2s got here c2 ',i,'cannot use char_array_pointer'
     write(*,*)'c2s got here c2 ',i,trim(switch(char_array_pointer))
     write(*,*)'c2s got here c2 ',i,char_array_pointer(i)

     if (char_array_pointer(i)==c_null_char) then
       length=i-1
       exit
     endif
     write(*,*)'c2s got here d'
     aux_string(i:i)=char_array_pointer(i)
   enddo
   allocate(character(len=length)::f_string)
   f_string=aux_string(1:length)
   write(*,*)'c2s got here f'
end function c2s
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!      indent - [M_strings]count number of leading spaces in a string
!!
!!##SYNOPSIS
!!
!!    function indent(line)
!!
!!     integer                        :: indent
!!     character(len=*),intent(in)    :: line
!!
!!##DESCRIPTION
!!    Count number of leading spaces in a CHARACTER variable.
!!
!!##EXAMPLES
!!
!!    Sample Program:
!!
!!     program demo_indent
!!     !  test filter to count leading spaces in a character variable
!!     !  might want to call notabs(3f) to expand tab characters
!!     use M_strings, only : indent
!!     implicit none
!!     character(len=1024) :: in
!!     integer             :: ios
!!        READFILE: do
!!           read(*,'(A)',iostat=ios)in
!!           if(ios /= 0) exit READFILE
!!           write(*,'(i3,"",a)')indent(in),trim(in)
!!        enddo READFILE
!!     end program demo_indent
!===================================================================================================================================
function indent(line)
implicit none
character(len=*),parameter::ident="@(#)M_strings::indent(3f): find number of leading spaces in a string"
integer                        :: indent
character(len=*),intent(in)    :: line
   integer                     :: i
   indent=0
   NOTSPACE: block
      SCAN: do i=1,len(line)
         if(line(i:i).ne.' ')then
            indent=i-1
            exit NOTSPACE
         endif
      enddo SCAN
      indent=len(line)
   endblock NOTSPACE
end function indent
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    expand - [M_strings]expand escape sequences
!!
!!##SYNOPSIS
!!
!!    function expand(line,escape) result(lineout)
!!
!!     character(len=*)                      :: line
!!     character(len=1),intent(in),optional  :: escape
!!     character(len=:),allocatable          :: lineout
!!##DESCRIPTION
!!
!!     EXPAND() expands sequences used to represent commonly used escape sequences
!!     or control characters.
!!
!!     Escape sequences
!!       \\     escape character
!!       \a     alert (BEL) -- g is an alias for a
!!       \b     backspace
!!       \c     suppress further output
!!       \e     escape
!!       \f     form feed
!!       \n     new line
!!       \r     carriage return
!!       \t     horizontal tab
!!       \v     vertical tab
!!       \oNNN  byte with octal value NNN (3 digits)
!!       \dNNN  byte with decimal value NNN (3 digits)
!!       \xHH   byte with hexadecimal value HH (2 digits) -- h is an alias for x
!!
!!     The default escape character is the backslash, but this may be changed using
!!     the optional parameter ESCAPE.
!!
!!##EXAMPLES
!!
!!
!!    Sample Program:
!!
!!     program demo_expand
!!     !  test filter to expand escape sequences in input lines
!!     use M_strings, only : expand
!!     character(len=1024) :: line
!!     integer             :: ios
!!        READFILE: block
!!           do
!!              read(*,'(A)',iostat=ios)line
!!              if(ios /= 0) exit READFILE
!!              write(*,'(a)')trim(expand(line))
!!           enddo
!!        endblock READFILE
!!     end program demo_expand
!===================================================================================================================================
function expand(line,escape) result(lineout)
implicit none
character(len=*),parameter::ident="@(#)M_strings::expand(3f): return string with escape sequences expanded"
character(len=*)                      :: line
character(len=1),intent(in),optional  :: escape ! escape character. Default is backslash
! expand escape sequences found in input string
! Escape sequences
!    %%     escape character           %a     alert (BEL) -- gi is an alias for a
!    %b     backspace                  %c     suppress further output
!    %e     escape                     %E     escape
!    %f     form feed                  %n     new line
!    %r     carriage return            %t     horizontal tab
!    %v     vertical tab
!    %oNNN  byte with octal value NNN (3 digits)
!    %dNNN  byte with decimal value NNN (3 digits)
!    %xHH   byte with hexadecimal value HH (2 digits) -- h is an alias for x
   character(len=1)             :: esc    ! escape character. Default is %
   character(len=:),allocatable :: lineout
   integer                      :: i
   integer                      :: j
   integer                      :: ilen
   character(len=3)             :: thr
   integer                      :: xxx
   integer                      :: ios
   j=0 ! pointer into output
   i=0 ! pointer into input
   ilen=len_trim(line)
   lineout=''
   if (present(escape))then
      esc=escape
   else
      esc=char(92)
   endif
   EXP: do
      i=i+1
      if(line(i:i).eq.esc)then
         i=i+1
         j=j+1
         if(line(i:i).ne.esc)then
            BACKSLASH: select case(line(i:i))
            case('a','A','g','G');lineout=lineout//char(  7) ! %a     alert (BEL)
            case('b','B');lineout=lineout//char(  8)         ! %b     backspace
            case('c','C');exit EXP                           ! %c     suppress further output
            case('d','D')                                    ! %d     Dnnn decimal value
                      thr=line(i+1:)
                   read(thr,'(i3)',iostat=ios)xxx
                      lineout=lineout//char(xxx)
                   i=i+3
            case('e','E');lineout=lineout//char( 27)         ! %e     escape
            case('f','F');lineout=lineout//char( 12)         ! %f     form feed
            case('n','N');lineout=lineout//char( 10)         ! %n     new line
            case('O','o')
                      thr=line(i+1:)
                   read(thr,'(o3)',iostat=ios)xxx
                      lineout=lineout//char(xxx)
                   i=i+3
            case('r','R');lineout=lineout//char( 13)         ! %r     carriage return
            case('t','T');lineout=lineout//char(  9)         ! %t     horizontal tab
            case('v','V');lineout=lineout//char( 11)         ! %v     vertical tab
            case('x','X','h','H')                            ! %x     xHH  byte with hexadecimal value HH (1 to 2 digits)
                      thr=line(i+1:)
                   read(thr,'(z2)',iostat=ios)xxx
                      lineout=lineout//char(xxx)
                   i=i+2
            end select BACKSLASH
         else
            lineout=lineout//esc                             ! escape character, defaults to backslash
         endif
      else
         j=j+1
         lineout=lineout//line(i:i)
      endif
      if(i.ge.ilen)exit EXP
   enddo EXP
end function expand
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    notabs - [M_strings]expand tab characters
!!##SYNOPSIS
!!
!!    subroutine notabs(INSTR,OUTSTR,ILEN)
!!
!!     character(len=*),intent=(in)  :: INSTR
!!     character(len=*),intent=(out) :: OUTSTR
!!     integer,intent=(out)          :: ILEN
!!
!!##DESCRIPTION
!!     NOTABS() converts tabs in INSTR to spaces in OUTSTR while maintaining
!!     columns. It assumes a tab is set every 8 characters. Lines are
!!     limited to 1024 characters. Trailing spaces, carriage returns,
!!     and line feeds are removed.
!!
!!     Sometimes tabs in files cause problems. For example: Some FORTRAN
!!     compilers hate tabs; some printers; some editors will have problems
!!     with tabs.
!!
!!##OPTIONS
!!     instr     Input line to remove tabs from
!!
!!##RESULTS
!!     outstr    Output string with tabs expanded.
!!     ilen      Significant length of returned string
!!
!!##EXAMPLES
!!
!!   program demo_notabs
!!
!!    !  test filter to remove tabs and trailing white space from input
!!    !  on files up to 1024 characters wide
!!    use M_strings, only : notabs
!!    character(len=1024) :: in,out
!!    integer             :: ios,iout
!!       READFILE: block
!!          do
!!             read(*,'(A)',iostat=ios)in
!!             if(ios /= 0) exit READFILE
!!             call notabs(in,out,iout)
!!             write(*,'(a)')out(:iout)
!!          enddo
!!       endblock READFILE
!!
!!   end program demo_notabs
!===================================================================================================================================
subroutine notabs(INSTR,OUTSTR,ILEN)
character(len=*),parameter::ident="@(#)M_strings::notabs(3f): convert tabs to spaces while maintaining columns, remove CRLF chars"
!  o given input string INSTR return output string OUTSTR with tabs expanded
!  o assuming tabs are set every 8 characters
!  o carriage return and line feed characters are replaced with a space
!  o ILEN holds the position of the last non-blank character in OUTSTR
!
! USES:
!       It is often useful to expand tabs in input files to simplify further processing such as tokenizing an input line.
!       Some FORTRAN compilers hate tabs in input files; some printers; some editors will have problems with tabs.
!       Also, trailing carriage returns and line feed characters are removed, as they are usually a problem created
!       by going to and from MSWIndows.
! AUTHOR:
!       John S. Urban
!
! SEE ALSO:
!       GNU/Unix commands expand(1) and unexpand(1)
!
   CHARACTER(LEN=*),INTENT(IN)   :: instr     ! input line to scan for tab characters
   CHARACTER(LEN=*),INTENT(OUT)  :: outstr    ! tab-expanded version of INSTR produced
   INTEGER,INTENT(OUT)           :: ilen      ! column position of last character put into output string
!===================================================================================================================================
   INTEGER,PARAMETER             :: tabsize=8 ! assume a tab stop is set every 8th column
   INTEGER                       :: ipos      ! position in OUTSTR to put next character of INSTR
   INTEGER                       :: lenin     ! length of input string trimmed of trailing spaces
   INTEGER                       :: lenout    ! number of characters output string can hold
   INTEGER                       :: istep     ! counter that advances thru input string INSTR one character at a time
   CHARACTER(LEN=1)              :: c         ! character in input line being processed
   INTEGER                       :: iade      ! ADE (ASCII Decimal Equivalent) of character being tested
!===================================================================================================================================
   IPOS=1                                     ! where to put next character in output string OUTSTR
   lenin=LEN(instr)                           ! length of character variable INSTR
   lenin=LEN_TRIM(instr(1:lenin))             ! length of INSTR trimmed of trailing spaces
   lenout=LEN(outstr)                         ! number of characters output string OUTSTR can hold
   OUTSTR=" "                                 ! this SHOULD blank-fill string, a buggy machine required a loop to set all characters
!===================================================================================================================================
      SCAN_LINE: DO istep=1,lenin             ! look through input string one character at a time
         c=instr(istep:istep)                 ! get next character
         iade=ICHAR(c)                        ! get ADE of the character
         expand_tabs : SELECT CASE (iade)     ! take different actions depending on which character was found
         CASE(9)                              ! test if character is a tab and move pointer out to appropriate column
            ipos = ipos + (tabsize - (MOD(ipos-1,tabsize)))
         CASE(10,13)                          ! convert carriage-return and new-line to space ,typically to handle DOS-format files
            ipos=ipos+1
         CASE DEFAULT                         ! c is anything else other than a tab,newline,or return  insert it in output string
            IF(ipos > lenout)THEN
               CALL journal("*notabs* output string overflow")
               EXIT
            ELSE
               outstr(ipos:ipos)=c
               ipos=ipos+1
            ENDIF
         END SELECT expand_tabs
      enddo SCAN_LINE
!===================================================================================================================================
      ipos=MIN(ipos,lenout)                   ! tabs or newline or return characters or last character might have gone too far
      ilen=LEN_TRIM(outstr(:ipos))            ! trim trailing spaces
!===================================================================================================================================
END SUBROUTINE notabs
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!       adjustc(3f) - [M_strings]center text
!!
!!##SYNOPSIS
!!
!!   pure function adjustc(string[,length])
!!
!!    character(len=*),intent(in)  :: string
!!    integer,intent(in),optional  :: length
!!    character(len=:),allocatable :: adjustc
!!
!!##DESCRIPTION
!!     Centers input text in a string of the length specified. Returns a
!!     string of length LENGTH if LENGTH is present. Otherwise returns a
!!     string of the length of the input string.
!!##OPTIONS
!!     string  input string to trim and center
!!     length  line length to center text in, optional.
!!##RETURNS
!!     adjustc  centered output string
!!
!!##EXAMPLES
!!
!!
!!   Sample Program:
!!
!!    program demo_adjustc
!!    use M_strings, only : adjustc
!!    !  using length of the input string
!!       write(*,'(a)')       '================================'
!!       write(*,'(a)')adjustc('centered string                 ')
!!       write(*,'(a)')adjustc('                 centered string')
!!       write(*,'(a)')adjustc('  centered string               ')
!!    !  using explicit output string length
!!       write(*,'(a)')repeat('=',50)
!!       write(*,'(a)')adjustc('this is a centered string',50)
!!       write(*,'(a)')repeat('=',50)
!!    end program demo_adjustc
!!
!!   Expected output
!!
!!    ================================
!!            centered string
!!            centered string
!!            centered string
!!    ==================================================
!!                this is a centered string
!!    ==================================================
!===================================================================================================================================
pure function adjustc(string,length)
character(len=*),parameter::ident="@(#)M_strings::adjustc(3f): center text"
!>
!! PROCEDURE   adjustc(3f)
!! DESCRIPTION center text using implicit or explicit length
!!##VERSION     2.0, 20160711
!! AUTHOR      John S. Urban
!===================================================================================================================================
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=*),intent(in)  :: string         ! input string to trim and center
integer,intent(in),optional  :: length         ! line length to center text in
character(len=:),allocatable :: adjustc        ! output string
integer                      :: inlen
integer                      :: ileft          ! left edge of string if it is centered
!-----------------------------------------------------------------------------------------------------------------------------------
   if(present(length))then                     ! optional length
      inlen=length                             ! length will be requested length
      if(inlen.le.0)then                       ! bad input length
         inlen=len(string)                     ! could not use input value, fall back to length of input string
      endif
   else                                        ! output length was not explicitly specified, use input string length
      inlen=len(string)
   endif
   allocate(character(len=inlen):: adjustc)    ! create output at requested length
   adjustc(1:inlen)=' '                        ! initialize output string to all blanks
!-----------------------------------------------------------------------------------------------------------------------------------
   ileft =(inlen-len_trim(adjustl(string)))/2  ! find starting point to start input string to center it
   if(ileft.gt.0)then                          ! if string will fit centered in output
      adjustc(ileft+1:inlen)=adjustl(string)   ! center the input text in the output string
   else                                        ! input string will not fit centered in output string
      adjustc(1:inlen)=adjustl(string)         ! copy as much of input to output as can
   endif
end function adjustc
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    nospace - [M_strings]remove all whitespace from input string
!!
!!##SYNOPSIS
!!
!!    function nospace(str) - remove all whitespace from input string
!!
!!     character(len=*),intent(in)          :: str
!!     character(len=:),allocatable         :: nospace
!!
!!##DESCRIPTION
!!
!!    nospace(3f) removes space, tab, carriage return, new line, vertical
!!    tab, formfeed and null characters (called "whitespace"). The output
!!    is returned trimmed.
!!
!!##EXAMPLES
!!
!!   Sample call
!!
!!     program demo_nospace
!!     use M_strings, only: nospace
!!     implicit none
!!     character(len=:),allocatable  :: s
!!        s='  This     is      a     test  '
!!        write(*,*) 'original input string is ....',s
!!        write(*,*) 'processed output string is ...',nospace(s)
!!        if(nospace(s).eq.'Thisisatest')then
!!           write(*,*)'nospace test passed'
!!        else
!!           write(*,*)'nospace test error'
!!        endif
!!     end program demo_nospace
!!
!!   Expected output
!!
!!     original input string is ....  This     is      a     test
!!     processed output string is ...Thisisatest
!!     nospace test passed
!===================================================================================================================================
function nospace(line)
character(len=*),parameter::ident="@(#)M_strings::nospace(3f): remove all whitespace from input string"
character(len=*),intent(in)    ::  line             ! remove whitespace from this string and return it
character(len=:),allocatable   ::  nospace          ! returned string
   integer                     ::  ipos             ! position to place next output character at
   integer                     ::  i                ! counter to increment from beginning to end of input string
!-----------------------------------------------------------------------------------------------------------------------------------
   allocate(nospace,mold=line)                      ! initially make output line length of input line
   nospace(:len_trim(nospace))=' '
   ipos=0
   do i=1,len_trim(line)                            ! increment from first to last character of the input line
      if ( isspace( line(i:i) ) ) cycle             ! if a blank is encountered skip it
      ipos=ipos+1                                   ! increment count of non-blank characters found
      nospace(ipos:ipos)=line(i:i)                  ! store non-blank character in output
   enddo
   nospace=trim(nospace)                            ! blank out unpacked part of line
end function nospace
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    lenset - [M_strings]return string trimmed or padded to specified length
!!
!!##SYNOPSIS
!!
!!    function lenset(str,length) result(strout)
!!
!!     character(len=*)                     :: str
!!     character(len=length)                :: strout
!!     integer,intent(in)                   :: length
!!
!!##DESCRIPTION
!!    lenset(3f) truncates a string or pads it with spaces to the specified
!!    length.
!!
!!##EXAMPLE
!!
!!
!!    Sample Program:
!!
!!     program demo_lenset
!!      use M_strings, only : lenset
!!      implicit none
!!      character(len=10)            :: string='abcdefghij'
!!      character(len=:),allocatable :: answer
!!         answer=lenset(string,5)
!!         write(*,'("[",a,"]")') answer
!!         answer=lenset(string,20)
!!         write(*,'("[",a,"]")') answer
!!     end program demo_lenset
!!
!!    Expected output:
!!
!!     [abcde]
!!     [abcdefghij          ]
!===================================================================================================================================
function lenset(line,length) result(strout)
character(len=*),parameter::ident="@(#)M_strings::lenset(3f): return string trimmed or padded to specified length"
character(len=*),intent(in)  ::  line
integer,intent(in)           ::  length
character(len=length)        ::  strout
   strout=line
end function lenset
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    merge_str - [M_strings]pads strings to same length and then calls MERGE(3f)
!!
!!##SYNOPSIS
!!
!!    function merge_str(str1,str2,expr) result(strout)
!!
!!     character(len=*),intent(in)     :: str1
!!     character(len=*),intent(in)     :: str2
!!     logical,intent(in)              :: expr
!!     character(len=:),allocatable    :: strout
!!
!!##DESCRIPTION
!!    merge_str(3f) pads the shorter of str1 and str2 to the longest length
!!    of str1 and str2 and then calls MERGE(padded_str1,padded_str2,expr).
!!    It trims trailing spaces off the result and returns the trimmed
!!    string. This makes it easier to call MERGE(3f) with strings, as
!!    MERGE(3f) requires the strings to be the same length.
!!
!!##EXAMPLES
!!
!!   Sample Program:
!!
!!     program demo_merge_str
!!     use M_strings, only : merge_str
!!     implicit none
!!     character(len=:), allocatable :: answer
!!        answer=merge_str('first string', 'second string is longer',10.eq.10)
!!        write(*,'("[",a,"]")') answer
!!        answer=merge_str('first string', 'second string is longer',10.ne.10)
!!        write(*,'("[",a,"]")') answer
!!    end program demo_merge_str
!!
!!    Expected output
!!
!!     [first string]
!!     [second string is longer]
!===================================================================================================================================
function merge_str(str1,str2,expr) result(strout)
! for some reason the MERGE(3f) intrinsic requires the strings it compares to be of equal length
! make an alias for MERGE(3f) that makes the lengths the same before doing the comparison by padding the shorter one with spaces
character(len=*),parameter::ident="@(#)M_strings::merge_str(3f): pads first and second arguments to MERGE(3f) to same length"
character(len=*),intent(in)     :: str1
character(len=*),intent(in)     :: str2
logical,intent(in)              :: expr
character(len=:),allocatable    :: strout
   integer                      :: big
   big=max(len(str1),len(str2))
   strout=trim(merge(lenset(str1,big),lenset(str2,big),expr))
end function merge_str
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    compact - [M_strings]converts contiguous whitespace to a single character (or nothing)
!!
!!##SYNOPSIS
!!
!!    function compact(STR,CHAR) result (OUTSTR)
!!
!!     character(len=*),intent(in)          :: STR
!!     character(len=*),intent(in),optional :: CHAR
!!     character(len=len(str))              :: OUTSTR
!!
!!##DESCRIPTION
!!    COMPACT(3f) converts multiple spaces, tabs and control characters
!!    (called "whitespace") to a single character or nothing. Leading
!!    whitespace is removed.
!!
!!##OPTIONS
!!    STR     input string to reduce or remove whitespace from
!!    CHAR    By default the character that replaces adjacent
!!            whitespace is a space. If the optional CHAR parameter is supplied
!!            it will be used to replace the whitespace. If a null character is
!!            supplied for CHAR whitespace is removed.
!!##RETURNS
!!    OUTSTR  string of same length as input string but with all contigious whitespace
!!            reduced to a single space and leading whitespace removed
!!
!!##EXAMPLES
!!
!!    program demo_compact
!!
!!     use M_strings, only : compact
!!     implicit none
!!     ! produces 'This is a test               '
!!     write(*,*)compact('  This     is      a     test  ')
!!     ! produces 'Thisisatest                  '
!!     write(*,*)compact('  This     is      a     test  ',char='')
!!     ! produces 'This:is:a:test               '
!!     write(*,*)compact('  This     is      a     test  ',char=':')
!!
!!     ! note CHAR is used to replace the whitespace, but if CHAR is
!!     ! in the original string it is just copied
!!     write(*,*)compact('A  AA    A   AAAAA',char='A')
!!     ! produces (original A characters are left as-is) 'AAAAAAAAAAAA'
!!     ! not 'A'
!!
!!    end program demo_compact
!!
!!    Expected output
!!
!!     >This is a test
!!     >Thisisatest
!!     >This:is:a:test
!!     >AAAAAAAAAAAA
!===================================================================================================================================
!elemental pure function compact(str,char) result (outstr)
function compact(str,char) result (outstr)
character(len=*),parameter::ident="@(#)M_strings::compact(3f): Converts white-space to single spaces"
character(len=*),intent(in)          :: str
character(len=*),intent(in),optional :: char
character(len=len(str))              :: outstr
character(len=1)                     :: ch
integer                              :: i
integer                              :: position_in_output
logical                              :: last_was_space
character(len=1)                     :: char_p
logical                              :: nospace
if(present(char))then
   char_p=char
   if(len(char).eq.0)then
      nospace=.true.
   else
      nospace=.false.
   endif
else
   char_p=' '
   nospace=.false.
endif
   outstr=' '
   last_was_space=.false.
   position_in_output=0

   IFSPACE: do i=1,len_trim(str)
     ch=str(i:i)
     select case(ichar(ch))
       case(0:32,127)                                         ! space or tab character or control character
         if(position_in_output.eq.0)then                      ! still at beginning so ignore leading whitespace
            cycle IFSPACE
         elseif(.not.last_was_space) then                     ! if have not already put out a space output one
           if(.not.nospace)then
              position_in_output=position_in_output+1
              outstr(position_in_output:position_in_output)=char_p
           endif
         endif
         last_was_space=.true.
       case(:-1,33:126,128:)                                  ! not a space, quote, or control character so copy it
         position_in_output=position_in_output+1
         outstr(position_in_output:position_in_output)=ch
         last_was_space=.false.
     end select
   end do IFSPACE

end function compact
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!     noesc - [M_strings]convert non-printable characters to a space.
!!
!!##SYNOPSIS
!!
!!    elemental function noesc(INSTR)
!!
!!     character(len=*),intent(in) :: INSTR
!!     character(len=len(instr))   :: noesc
!!
!!##DESCRIPTION
!!      Convert non-printable characters to a space.
!!
!!##EXAMPLES
!!
!!    program demo_noesc
!!
!!     use M_strings, only : noesc
!!     character(len=128) :: ascii
!!     character(len=128) :: cleared
!!     ! fill variable with base ASCII character set
!!     do i=1,128
!!        ascii(i:i)=char(i-1)
!!     enddo
!!     cleared=noesc(ascii)
!!     write(*,*)'characters and their ADE (ASCII Decimal Equivalent)'
!!     call ade(ascii)
!!     write(*,*)'Cleared of non-printable characters'
!!     call ade(cleared)
!!     write(*,*)'Cleared string:'
!!     write(*,*)cleared
!!     contains
!!       subroutine ade(string)
!!       implicit none
!!       ! the string to print
!!       character(len=*),intent(in) :: string
!!       ! number of characters in string to print
!!       integer :: ilen
!!       ! counter used to step thru string
!!       integer :: i
!!          ! get trimmed length of input string
!!          ilen=len_trim(string(:len(string)))
!!
!!          ! replace lower unprintable characters with spaces
!!          write(*,101)(merge(string(i:i),' ',&
!!          & ichar(string(i:i)).ge.32         &
!!          & .and.                            &
!!          & ichar(string(i:i)).le.126)       &
!!          & ,i=1,ilen)
!!
!!          ! print ADE value of character underneath it
!!          write(*,202)     (ichar(string(i:i))/100,    i=1,ilen)
!!          write(*,202)(mod( ichar(string(i:i)),100)/10,i=1,ilen)
!!          write(*,202)(mod((ichar(string(i:i))),10),   i=1,ilen)
!!       ! format for printing string characters
!!       101   format(*(a1:))
!!       ! format for printing ADE values
!!       202   format(*(i1:))
!!       end subroutine ade
!!     end program demo_noesc
!!
!!    Expected output
!!
!!    The string is printed with the ADE value vertically beneath.
!!    The original string has all the ADEs from 000 to 127. After
!!    NOESC(3f) is called on the string all the "non-printable"
!!    characters are replaced with a space (ADE of 032).
!!
!!   characters and their ADE (ASCII Decimal Equivalent)
!!
!!    >                                 !"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmnopqrstuvwxyz{|}~
!!    >00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001111111111111111111111111111
!!    >00000000001111111111222222222233333333334444444444555555555566666666667777777777888888888899999999990000000000111111111122222222
!!    >01234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567
!!
!!   Cleared of non-printable characters
!!
!!    >                                 !"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmnopqrstuvwxyz{|}~
!!    >0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000111111111111111111111111111
!!    >3333333333333333333333333333333333333333444444444455555555556666666666777777777788888888889999999999000000000011111111112222222
!!    >2222222222222222222222222222222223456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456
!!
!!   Cleared string:
!!    >                                  !"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmnopqrstuvwxyz{|}~
!===================================================================================================================================
elemental function noesc(INSTR)
character(len=*),parameter::ident="@(#)M_strings::noesc(3f): convert non-printable characters to a space"
   character(len=*),intent(in) :: INSTR   ! string that might contain nonprintable characters
   character(len=len(instr))   :: noesc
   integer                     :: ic,i10
!-----------------------------------------------------------------------------------------------------------------------------------
   noesc=''                               ! initialize output string
   do i10=1,len_trim(INSTR(1:len(INSTR)))
      ic=ichar(INSTR(i10:i10))
      if(ic.le.31.or.ic.eq.127)then       ! find characters with ADE of 0-31, 127
         noesc(I10:I10)=' '               ! replace non-printable characters with a space
      else
         noesc(I10:I10)=INSTR(i10:i10)    ! copy other characters as-is from input string to output string
      endif
   enddo
end function noesc
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!      string_to_value - [M_strings]subroutine returns real value from string
!!
!!##SYNOPSIS
!!
!!    subroutine string_to_value(chars,valu,ierr)
!!
!!     character(len=*),intent(in) :: chars   ! input string
!!     integer|real|doubleprecision,intent(out)         :: valu
!!     integer,intent(out)         :: ierr
!!
!!##DESCRIPTION
!!       returns a real value from a numeric character string.
!!
!!       works with any g-format input, including integer, real, and
!!       exponential.  If the input string begins with "B", "Z", or "O"
!!       and otherwise represents a positive whole number it is assumed to
!!       be a binary, hexadecimal, or octal value. If the string contains
!!       commas they are removed.
!!
!!       if an error occurs in the READ, IOSTAT is returned in IERR and
!!       value is set to zero.  if no error occurs, IERR=0.
!!##OPTIONS
!!       CHARS  input string to read numeric value from
!!##RETURNS
!!       VALU   numeric value returned. May be INTEGER, REAL, or DOUBLEPRECISION.
!!       IERR   error flag (0 == no error)
!!##EXAMPLE
!!
!!
!!    program demo_string_to_value
!!
!!     use m_strings, only: string_to_value
!!     character(len=80) :: string
!!        string=' -40.5e-2 '
!!        call string_to_value(string,value,ierr)
!!        write(*,*) 'value of string ['//trim(string)//'] is ',value
!!
!!    end program demo_string_to_value
!===================================================================================================================================
subroutine a2r(chars,valu,ierr)
character(len=*),parameter::ident="@(#)M_strings::a2r(3fp): subroutine returns real value from string"
   character(len=*),intent(in) :: chars                      ! input string
   real,intent(out)            :: valu                       ! value read from input string
   integer,intent(out)         :: ierr                       ! error flag (0 == no error)
   doubleprecision             :: valu8
   valu8=0.0d0
   call a2d(chars,valu8,ierr)
   valu=real(valu8)
end subroutine a2r
!----------------------------------------------------------------------------------------------------------------------------------
subroutine a2i(chars,valu,ierr)
character(len=*),parameter::ident="@(#)M_strings::a2i(3fp): subroutine returns integer value from string"
   character(len=*),intent(in) :: chars                      ! input string
   integer,intent(out)         :: valu                       ! value read from input string
   integer,intent(out)         :: ierr                       ! error flag (0 == no error)
   doubleprecision             :: valu8
   valu8=0.0d0
   call a2d(chars,valu8,ierr)
   valu=int(valu8)
end subroutine a2i
!----------------------------------------------------------------------------------------------------------------------------------
subroutine a2d(chars,valu,ierr)
character(len=*),parameter::ident="@(#)M_strings::a2d(3fp): subroutine returns double value from string"
!     1989,2016 John S. Urban.
!
!  o  works with any g-format input, including integer, real, and exponential.
!  o  if an error occurs in the read, iostat is returned in ierr and value is set to zero.  if no error occurs, ierr=0.
!  o  if the string happens to be 'eod' no error message is produced so this string may be used to act as an end-of-data.
!     IERR will still be non-zero in this case.
!----------------------------------------------------------------------------------------------------------------------------------
   character(len=*),intent(in)  :: chars                        ! input string
   character(len=:),allocatable :: local_chars
   doubleprecision,intent(out)  :: valu                         ! value read from input string
   integer,intent(out)          :: ierr                         ! error flag (0 == no error)
!----------------------------------------------------------------------------------------------------------------------------------
   character(len=*),parameter   :: fmt="('(bn,g',i5,'.0)')"     ! format used to build frmt
   character(len=15)            :: frmt                         ! holds format built to read input string
   character(len=256)           :: msg                          ! hold message from I/O errors
   integer                      :: intg
!----------------------------------------------------------------------------------------------------------------------------------
   ierr=0                                                       ! initialize error flag to zero
   local_chars=chars
   if(len(local_chars).eq.0)local_chars=' '
   call substitute(local_chars,',','')                          ! remove any comma characters
   select case(local_chars(1:1))
   case('z','Z','h','H')                                        ! assume hexadecimal
      frmt='(Z'//v2s(len(local_chars))//')'
      read(local_chars(2:),frmt,iostat=ierr,iomsg=msg)intg
      valu=dble(intg)
   case('b','B')                                                ! assume binary (base 2)
      frmt='(B'//v2s(len(local_chars))//')'
      read(local_chars(2:),frmt,iostat=ierr,iomsg=msg)intg
      valu=dble(intg)
   case('O','o')                                                ! assume octal
      frmt='(O'//v2s(len(local_chars))//')'
      read(local_chars(2:),frmt,iostat=ierr,iomsg=msg)intg
      valu=dble(intg)
   case default
      write(frmt,fmt)len(local_chars)                           ! build format of form '(BN,Gn.0)'
      read(local_chars,fmt=frmt,iostat=ierr,iomsg=msg)valu      ! try to read value from string
   end select
   if(ierr.ne.0)then                                            ! if an error occurred ierr will be non-zero.
      valu=0.0                                                  ! set returned value to zero on error
         if(local_chars.ne.'eod')then                                 ! print warning message
            call journal('sc','*a2d* - cannot produce number from string ['//trim(chars)//']')
            call journal('sc','*a2d* - ['//trim(msg)//']')
         endif
      endif
end subroutine a2d
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!      s2v - [M_strings]function returns doubleprecision numeric value from a string
!!
!!##SYNOPSIS
!!
!!    function s2v(string,[ierr])
!!
!!     character(len=*)             :: string
!!     doubleprecision              :: s2v
!!     integer,intent(out),optional :: ierr
!!
!!##DESCRIPTION
!!       This function converts a string to a DOUBLEPRECISION numeric value.
!!       A value of zero (0) is returned on error.
!!
!!       If an error occurs the program is stopped if the optional parameter
!!       IERR is not present. If IERR is non-zero an error occurred.
!!
!!##EXAMPLE
!!
!!
!!    program demo_s2v
!!
!!     use m_strings, only: s2v
!!     implicit none
!!     character(len=8)              :: s=' 10.345 '
!!     integer                       :: i
!!     character(len=14),allocatable :: strings(:)
!!     doubleprecision               :: dv
!!     integer                       :: errnum
!!
!!     ! different strings representing INTEGER, REAL, and DOUBLEPRECISION
!!     strings=[&
!!     &' 10.345       ',&
!!     &'+10           ',&
!!     &'    -3        ',&
!!     &'    -4.94e-2  ',&
!!     &'0.1           ',&
!!     &'12345.678910d0',&
!!     &'              ',& ! Note: will return zero without an error message
!!     &'1 2 1 2 1 . 0 ',& ! Note: spaces will be ignored
!!     &'WHAT?         ']  ! Note: error messages will appear, zero returned
!!
!!     ! a numeric value is returned, so it can be used in numeric expression
!!     write(*,*) '1/2 value of string is ',s2v(s)/2.0d0
!!     write(*,*)
!!     write(*,*)' STRING            VALUE                    ERROR_NUMBER'
!!     do i=1,size(strings)
!!        ! Note: not a good idea to use s2v(3f) in a WRITE(3f) statement,
!!        ! as it does I/O when errors occur, so called on a separate line
!!        dv=s2v(strings(i),errnum)
!!        write(*,*) strings(i)//'=',dv,errnum
!!     enddo
!!     write(*,*)"That's all folks!"
!!
!!     end program demo_s2v
!!
!!    Expected output
!!
!!     >1/2 value of string is    5.1725000000000003
!!     >
!!     > STRING            VALUE                    ERROR_NUMBER
!!     > 10.345       =   10.345000000000001                0
!!     >+10           =   10.000000000000000                0
!!     >    -3        =  -3.0000000000000000                0
!!     >    -4.94e-2  =  -4.9399999999999999E-002           0
!!     >0.1           =  0.10000000000000001                0
!!     >12345.678910d0=   12345.678910000001                0
!!     >              =   0.0000000000000000                0
!!     >1 2 1 2 1 . 0 =   12121.000000000000                0
!!     >*a2d* - cannot produce number from string [WHAT?]
!!     >*a2d* - [Bad value during floating point read]
!!     >WHAT?         =   0.0000000000000000             5010
!!     >That's all folks!
!===================================================================================================================================
!>
!!##PROCEDURE:
!! DESCRIPTION: s2v(3f): function returns doubleprecision number from string;zero if error occurs"
!!##VERSION:     2.0, 20160704
!! AUTHOR:      John S. Urban
!===================================================================================================================================
doubleprecision function s2v(chars,ierr)
!  1989 John S. Urban
character(len=*),parameter::ident="@(#)M_strings::s2v(3f): returns doubleprecision number from string"

character(len=*),intent(in) :: chars
integer,optional            :: ierr
doubleprecision             :: valu
   integer                  :: ierr_local

   ierr_local=0
   call a2d(chars,valu,ierr_local)
   s2v=valu
   if(present(ierr))then ! if error is not returned stop program on error
      ierr=ierr_local
   elseif(ierr_local.ne.0)then
      write(*,*)'*s2v* stopped while reading '//trim(chars)
      stop 1
   endif
end function s2v
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!      value_to_string - [M_strings]return numeric string from a numeric value
!!
!!##SYNOPSIS
!!
!!
!!    subroutine value_to_string(value,chars[,ilen,ierr,fmt])
!!
!!     character(len=*) :: chars  ! minimum of 23 characters required
!!     !--------
!!     ! VALUE may be any <em>one</em> of the following types:
!!     doubleprecision,intent(in)               :: value
!!     real,intent(in)                          :: value
!!     integer,intent(in)                       :: value
!!     !--------
!!     character(len=*),intent(out)             :: chars
!!     integer,intent(out),optional             :: ilen
!!     integer,optional                         :: ierr
!!     character(len=*),intent(in),optional     :: fmt
!!
!!##DESCRIPTION
!!
!!    value_to_string(3f)
!!    that returns a numeric representation in a string given a numeric value of type
!!    REAL, DOUBLEPRECISION, or INTEGER. It creates the strings using internal writes.
!!    It then removes trailing zeros from non-zero values, and left-justifies the string.
!!
!!##OPTIONS
!!       o  VALUE - input value to be converted to a string
!!##RETURNS
!!       o  CHARS - returned string representing input value, must be at least 23 characters long;
!!                  or what is required by optional FMT if longer.
!!       o  ILEN - position of last non-blank character in returned string; optional.
!!       o  IERR - If not zero, error occurred.; optional.
!!       o  FMT - You may specify a specific format that produces a string up to the length of CHARS; optional.
!!
!!##EXAMPLE
!!
!!
!!    Sample program
!!
!!      program demo_value_to_string
!!      use m_strings, only: value_to_string
!!      implicit none
!!      character(len=80) :: string
!!      integer           :: ilen
!!         call value_to_string(3.0/4.0,string,ilen)
!!         write(*,*) 'The value is [',string(:ilen),']'
!!
!!         call value_to_string(3.0/4.0,string,ilen,fmt='')
!!         write(*,*) 'The value is [',string(:ilen),']'
!!
!!         call value_to_string(3.0/4.0,string,ilen,fmt='("THE VALUE IS ",g0)')
!!         write(*,*) 'The value is [',string(:ilen),']'
!!
!!         call value_to_string(1234,string,ilen)
!!         write(*,*) 'The value is [',string(:ilen),']'
!!
!!         call value_to_string(1.0d0/3.0d0,string,ilen)
!!         write(*,*) 'The value is [',string(:ilen),']'
!!
!!      end program demo_value_to_string
!!
!!    Expected output
!!
!!     The value is [0.75]
!!     The value is [      0.7500000000]
!!     The value is [THE VALUE IS .750000000]
!!     The value is [1234]
!!     The value is [0.33333333333333331]
!===================================================================================================================================
subroutine value_to_string(gval,chars,length,err,fmt)
character(len=*),parameter::ident="@(#)M_strings::value_to_string(3fp): subroutine returns a string from a value"
class(*),intent(in)                      :: gval
character(len=*),intent(out)             :: chars
integer,intent(out),optional             :: length
integer,optional                         :: err
integer                                  :: err_local
character(len=*),optional,intent(in)     :: fmt         ! format to write value with
character(len=:),allocatable             :: fmt_local

!  Notice that the value GVAL can be any of several types ( INTEGER,REAL,DOUBLEPRECISION)

   if (present(fmt)) then
      select type(gval)
      type is (integer)
         fmt_local='(i0)'
         if(fmt.ne.'') fmt_local=fmt
         write(chars,fmt_local,iostat=err_local)gval
      type is (real)
         fmt_local='(bz,g23.10e3)'
         if(fmt.ne.'') fmt_local=fmt
         write(chars,fmt_local,iostat=err_local)gval
      type is (doubleprecision)
         fmt_local='(bz,g0)'
         if(fmt.ne.'') fmt_local=fmt
         write(chars,fmt_local,iostat=err_local)gval
      end select
      if(fmt.eq.'') then
         chars=adjustl(chars)
         call trimzeros(chars)
      endif
   else                                                  ! no explicit format option present
      select type(gval)
      type is (integer)
         write(chars,*,iostat=err_local)gval
      type is (real)
         write(chars,*,iostat=err_local)gval
      type is (doubleprecision)
         write(chars,*,iostat=err_local)gval
      end select
      chars=adjustl(chars)
      if(index(chars,'.').ne.0) call trimzeros(chars)
   endif

   if(present(length)) then ; length=len_trim(chars) ; endif

   if(present(err)) then ; err=err_local ; endif

end subroutine value_to_string
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!      v2s - [M_strings]return numeric string from a numeric value
!!##SYNOPSIS
!!
!!       function v2s(value) result(outstr)
!!
!!        integer|real|doubleprecision,intent(in ) :: value
!!        character(len=:),allocatable :: outstr
!!##DESCRIPTION
!!
!!    v2s(3f) returns a representation of a numeric value as a string when
!!    given a numeric value of type REAL, DOUBLEPRECISION, or INTEGER. It
!!    creates the strings using internal WRITE() statements.  trailing zeros
!!    are removed from non-zero values, and the string is left-justified.
!!
!!       o  VALUE - input value to be converted to a string
!!       o  OUTSTR - returned string representing input value,
!!##EXAMPLE
!!
!!
!!   Sample call
!!
!!    program demo_v2s
!!    use m_strings, only: v2s
!!    write(*,*) 'The value of 3.0/4.0 is ['//v2s(3.0/4.0)//']'
!!    write(*,*) 'The value of 1234    is ['//v2s(1234)//']'
!!    write(*,*) 'The value of 0d0     is ['//v2s(0d0)//']'
!!    end program demo_v2s
!!
!!   Expected output
!!
!!     The value of 3.0/4.0 is [0.75]
!!     The value of 1234    is [1234]
!!     The value of 0d0     is [0.0]
!!
!!##FILES AND METADATA
!!
!!       o  References: none
!!       o  Dependencies: value_to_string
!!       o  Legal Restrictions: none
!!       o  QA:ufpp(1) ggodbad(1) test in source file
!!       o  Authors: John S. Urban
!===================================================================================================================================
! very odd compiler problems in many (but not all) programs using this routine; GNU Fortran (GCC) 5.4.0; 20161030
function v2s_bug(gval) result(outstr)
character(len=*),parameter::ident="@(#)M_strings::v2s_bug(3f): function returns string given numeric value"
class(*),intent(in)          :: gval                         ! input value to convert to a string
character(len=:),allocatable :: outstr                       ! output string to generate
character(len=80)            :: string
   select type(gval)
   type is (integer)
      call value_to_string(gval,string)
   type is (real)
      call value_to_string(gval,string)
   type is (doubleprecision)
      call value_to_string(gval,string)
   end select
   outstr=trim(string)
end function v2s_bug
!===================================================================================================================================
function d2s(dvalue) result(outstr)
character(len=*),parameter::ident="@(#)M_strings::d2s(3fp): private function returns string given doubleprecision value"
doubleprecision,intent(in)   :: dvalue                         ! input value to convert to a string
character(len=:),allocatable :: outstr                         ! output string to generate
character(len=80)            :: string
   call value_to_string(dvalue,string)
   outstr=trim(string)
end function d2s
!===================================================================================================================================
function r2s(rvalue) result(outstr)
character(len=*),parameter::ident="@(#)M_strings::r2s(3fp): private function returns string given real value"
real,intent(in )             :: rvalue                         ! input value to convert to a string
character(len=:),allocatable :: outstr                         ! output string to generate
character(len=80)            :: string
   call value_to_string(rvalue,string)
   outstr=trim(string)
end function r2s
!===================================================================================================================================
function i2s(ivalue) result(outstr)
character(len=*),parameter::ident="@(#)M_strings::i2s(3fp): private function returns string given integer value"
integer,intent(in )          :: ivalue                         ! input value to convert to a string
character(len=:),allocatable :: outstr                         ! output string to generate
character(len=80)            :: string
   call value_to_string(ivalue,string)
   outstr=trim(string)
end function i2s
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    trimzeros(3fp) - [M_strings]Delete trailing zeros from numeric decimal string
!!##SYNOPSIS
!!
!!    subroutine trimzeros(str)
!!
!!     character(len=*)  :: str
!!##DESCRIPTION
!!    TRIMZEROS(3f) deletes trailing zeros from a string representing a
!!    number. If the resulting string would end in a decimal point, one
!!    trailing zero is added.
!!##EXAMPLES
!!
!!    Sample program:
!!
!!       program demo_trimzeros
!!       use M_strings, only : trimzeros
!!       character(len=:),allocatable :: string
!!       write(*,*)trimzeros('123.450000000000')
!!       write(*,*)trimzeros('12345')
!!       write(*,*)trimzeros('12345.')
!!       write(*,*)trimzeros('12345.00e3')
!!       end program demo_trimzeros
!===================================================================================================================================
subroutine trimzeros(string)
character(len=*),parameter::ident="@(#)M_strings::trimzeros(3fp): Delete trailing zeros from numeric decimal string"
! if zero needs added at end assumes input string has room
character(len=*)             :: string
character(len=len(string)+2) :: str
character(len=len(string))   :: exp          ! the exponent string if present
integer                      :: ipos         ! where exponent letter appears if present
integer                      :: i, ii
   str=string                                ! working copy of string
   ipos=scan(str,'eEdD')                     ! find end of real number if string uses exponent notation
   if(ipos>0) then                           ! letter was found
      exp=str(ipos:)                         ! keep exponent string so it can be added back as a suffix
      str=str(1:ipos-1)                      ! just the real part, exponent removed will not have trailing zeros removed
   endif
   if(index(str,'.').eq.0)then               ! if no decimal character in original string add one to end of string
      ii=len_trim(str)
      str(ii+1:ii+1)='.'                     ! add decimal to end of string
   endif
   do i=len_trim(str),1,-1                   ! scanning from end find a non-zero character
      select case(str(i:i))
      case('0')                              ! found a trailing zero so keep trimming
         cycle
      case('.')                              ! found a decimal character at end of remaining string
         if(i.le.1)then
            str='0'
         else
            str=str(1:i-1)
         endif
         exit
      case default
         str=str(1:i)                        ! found a non-zero character so trim string and exit
         exit
      end select
   end do
   if(ipos>0)then                            ! if originally had an exponent place it back on
      string=trim(str)//trim(exp)
   else
      string=str
   endif
end subroutine trimzeros
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    describe(3f) - [M_strings]returns a string describing the name of a single character
!!
!!##SYNOPSIS
!!
!!    function describe(ch) result (string)
!!
!!     character(len=1),intent(in)   :: ch
!!     character(len=:),allocatable  :: string
!!
!!##DESCRIPTION
!!    describe(3f) returns a string describing long name of a single character
!!
!!##EXAMPLES
!!
!!   Sample Program:
!!
!!    program demo_describe
!!     use M_strings, only : describe
!!     implicit none
!!     integer :: i
!!        do i=1,128  ! fill variable with base ASCII character set
!!           write(*,*)describe(char(i-1))
!!        enddo
!!    end program demo_describe
!!
!!   Expected output
!!
!!     ctrl-@ or ctrl-? (NUL) null
!!     ctrl-A (SOH) start of heading
!!     ctrl-B (STX) start of text
!!     ctrl-C (ETX) end of text
!!     ctrl-D (EOT) end of transmission
!!     ctrl-E (ENQ) enquiry
!!     ctrl-F (ACK) acknowledge
!!     ctrl-G (BEL) bell
!!     ctrl-H (BS) backspace
!!     ctrl-I (HT) horizontal tabulation
!!     ctrl-J (LF) line feed
!!     ctrl-K (VT) vertical tabulation
!!     ctrl-L (FF) form feed
!!     ctrl-M (CR) carriage return
!!     ctrl-N (SO) shift out
!!     ctrl-O (SI) shift in
!!     ctrl-P (DLE) data link escape
!!     ctrl-Q (DC1) device control 1
!!     ctrl-R (DC2) device control 2
!!     ctrl-S (DC3) device control 3
!!     ctrl-T (DC4) device control 4
!!     ctrl-U (NAK) negative acknowledge
!!     ctrl-V (SYN) synchronous idle
!!     ctrl-W (ETB) end of transmission block
!!     ctrl-X (CAN) cancel
!!     ctrl-Y (EM) end of medium
!!     ctrl-Z (SUB) substitute
!!     ctrl-[ (ESC) escape
!!     ctrl-\ or ctrl-@ (FS) file separator
!!     ctrl-] (GS) group separator
!!     ctrl-^ or ctrl-= (RS) record separator
!!     ctrl-_ (US) unit separator
!!     space
!!     ! exclamation point
!!     " quotation marks
!!     # number sign
!!     $ currency symbol
!!     % percent
!!     & ampersand
!!     ' apostrophe
!!     ( left parenthesis
!!     ) right parenthesis
!!     * asterisk
!!     + plus
!!     , comma
!!     - minus
!!     . period
!!     / slash
!!     0 zero
!!     1 one
!!     2 two
!!     3 three
!!     4 four
!!     5 five
!!     6 six
!!     7 seven
!!     8 eight
!!     9 nine
!!     : colon
!!     ; semicolon
!!     < less than
!!     = equals
!!     > greater than
!!     ? question mark
!!     @ at sign
!!     majuscule A
!!     majuscule B
!!     majuscule C
!!     majuscule D
!!     majuscule E
!!     majuscule F
!!     majuscule G
!!     majuscule H
!!     majuscule I
!!     majuscule J
!!     majuscule K
!!     majuscule L
!!     majuscule M
!!     majuscule N
!!     majuscule O
!!     majuscule P
!!     majuscule Q
!!     majuscule R
!!     majuscule S
!!     majuscule T
!!     majuscule U
!!     majuscule V
!!     majuscule W
!!     majuscule X
!!     majuscule Y
!!     majuscule Z
!!     [ left bracket
!!     \ backslash
!!     ] right bracket
!!     ^ caret
!!     _ underscore
!!     ` grave accent
!!     miniscule a
!!     miniscule b
!!     miniscule c
!!     miniscule d
!!     miniscule e
!!     miniscule f
!!     miniscule g
!!     miniscule h
!!     miniscule i
!!     miniscule j
!!     miniscule k
!!     miniscule l
!!     miniscule m
!!     miniscule n
!!     miniscule o
!!     miniscule p
!!     miniscule q
!!     miniscule r
!!     miniscule s
!!     miniscule t
!!     miniscule u
!!     miniscule v
!!     miniscule w
!!     miniscule x
!!     miniscule y
!!     miniscule z
!!     { left brace
!!     | vertical line
!!     } right brace
!!     ~ tilde
!!     ctrl-? (DEL) delete
!===================================================================================================================================
function describe(ch) result (string)
character(len=*),parameter::ident="@(#)M_strings::describe(3f): return string describing long name of a single character"
character(len=1),intent(in)   :: ch
character(len=:),allocatable  :: string
! LATER: add hex, octal, decimal, key-press description, alternate names
!  ASCII character codes
   select case (ichar(ch))
   case(     0  ); STRING="ctrl-@ or ctrl-? (NUL) null"
   case(     1  ); STRING="ctrl-A (SOH) start of heading"
   case(     2  ); STRING="ctrl-B (STX) start of text"
   case(     3  ); STRING="ctrl-C (ETX) end of text"
   case(     4  ); STRING="ctrl-D (EOT) end of transmission"
   case(     5  ); STRING="ctrl-E (ENQ) enquiry"
   case(     6  ); STRING="ctrl-F (ACK) acknowledge"
   case(     7  ); STRING="ctrl-G (BEL) bell"
   case(     8  ); STRING="ctrl-H (BS) backspace"
   case(     9  ); STRING="ctrl-I (HT) horizontal tabulation"
   case(    10  ); STRING="ctrl-J (LF) line feed"
   case(    11  ); STRING="ctrl-K (VT) vertical tabulation"
   case(    12  ); STRING="ctrl-L (FF) form feed"
   case(    13  ); STRING="ctrl-M (CR) carriage return"
   case(    14  ); STRING="ctrl-N (SO) shift out"
   case(    15  ); STRING="ctrl-O (SI) shift in"
   case(    16  ); STRING="ctrl-P (DLE) data link escape"
   case(    17  ); STRING="ctrl-Q (DC1) device control 1"
   case(    18  ); STRING="ctrl-R (DC2) device control 2"
   case(    19  ); STRING="ctrl-S (DC3) device control 3"
   case(    20  ); STRING="ctrl-T (DC4) device control 4"
   case(    21  ); STRING="ctrl-U (NAK) negative acknowledge"
   case(    22  ); STRING="ctrl-V (SYN) synchronous idle"
   case(    23  ); STRING="ctrl-W (ETB) end of transmission block"
   case(    24  ); STRING="ctrl-X (CAN) cancel"
   case(    25  ); STRING="ctrl-Y (EM) end of medium"
   case(    26  ); STRING="ctrl-Z (SUB) substitute"
   case(    27  ); STRING="ctrl-[ (ESC) escape"
   case(    28  ); STRING="ctrl-\ or ctrl-@ (FS) file separator"
   case(    29  ); STRING="ctrl-] (GS) group separator"
   case(    30  ); STRING="ctrl-^ or ctrl-= (RS) record separator"
   case(    31  ); STRING="ctrl-_ (US) unit separator"
   case(    32  ); STRING="space"
   case(    33  ); STRING="! exclamation point"
   case(    34  ); STRING=""" quotation marks"
   case(    35  ); STRING="# number sign"
   case(    36  ); STRING="$ currency symbol"
   case(    37  ); STRING="% percent"
   case(    38  ); STRING="& ampersand"
   case(    39  ); STRING="' apostrophe"
   case(    40  ); STRING="( left parenthesis"
   case(    41  ); STRING=") right parenthesis"
   case(    42  ); STRING="* asterisk"
   case(    43  ); STRING="+ plus"
   case(    44  ); STRING=", comma"
   case(    45  ); STRING="- minus"
   case(    46  ); STRING=". period"
   case(    47  ); STRING="/ slash"
   case(    48  ); STRING="0 zero"
   case(    49  ); STRING="1 one"
   case(    50  ); STRING="2 two"
   case(    51  ); STRING="3 three"
   case(    52  ); STRING="4 four"
   case(    53  ); STRING="5 five"
   case(    54  ); STRING="6 six"
   case(    55  ); STRING="7 seven"
   case(    56  ); STRING="8 eight"
   case(    57  ); STRING="9 nine"
   case(    58  ); STRING=": colon"
   case(    59  ); STRING="; semicolon"
   case(    60  ); STRING="< less than"
   case(    61  ); STRING="= equals"
   case(    62  ); STRING="> greater than"
   case(    63  ); STRING="? question mark"
   case(    64  ); STRING="@ at sign"
   case(    65  ); STRING="A majuscule A"
   case(    66  ); STRING="B majuscule B"
   case(    67  ); STRING="C majuscule C"
   case(    68  ); STRING="D majuscule D"
   case(    69  ); STRING="E majuscule E"
   case(    70  ); STRING="F majuscule F"
   case(    71  ); STRING="G majuscule G"
   case(    72  ); STRING="H majuscule H"
   case(    73  ); STRING="I majuscule I"
   case(    74  ); STRING="J majuscule J"
   case(    75  ); STRING="K majuscule K"
   case(    76  ); STRING="L majuscule L"
   case(    77  ); STRING="M majuscule M"
   case(    78  ); STRING="N majuscule N"
   case(    79  ); STRING="O majuscule O"
   case(    80  ); STRING="P majuscule P"
   case(    81  ); STRING="Q majuscule Q"
   case(    82  ); STRING="R majuscule R"
   case(    83  ); STRING="S majuscule S"
   case(    84  ); STRING="T majuscule T"
   case(    85  ); STRING="U majuscule U"
   case(    86  ); STRING="V majuscule V"
   case(    87  ); STRING="W majuscule W"
   case(    88  ); STRING="X majuscule X"
   case(    89  ); STRING="Y majuscule Y"
   case(    90  ); STRING="Z majuscule Z"
   case(    91  ); STRING="[ left bracket"
   case(    92  ); STRING="\ backslash"
   case(    93  ); STRING="] right bracket"
   case(    94  ); STRING="^ caret"
   case(    95  ); STRING="_ underscore"
   case(    96  ); STRING="` grave accent"
   case(    97  ); STRING="a miniscule a"
   case(    98  ); STRING="b miniscule b"
   case(    99  ); STRING="c miniscule c"
   case(   100  ); STRING="d miniscule d"
   case(   101  ); STRING="e miniscule e"
   case(   102  ); STRING="f miniscule f"
   case(   103  ); STRING="g miniscule g"
   case(   104  ); STRING="h miniscule h"
   case(   105  ); STRING="i miniscule i"
   case(   106  ); STRING="j miniscule j"
   case(   107  ); STRING="k miniscule k"
   case(   108  ); STRING="l miniscule l"
   case(   109  ); STRING="m miniscule m"
   case(   110  ); STRING="n miniscule n"
   case(   111  ); STRING="o miniscule o"
   case(   112  ); STRING="p miniscule p"
   case(   113  ); STRING="q miniscule q"
   case(   114  ); STRING="r miniscule r"
   case(   115  ); STRING="s miniscule s"
   case(   116  ); STRING="t miniscule t"
   case(   117  ); STRING="u miniscule u"
   case(   118  ); STRING="v miniscule v"
   case(   119  ); STRING="w miniscule w"
   case(   120  ); STRING="x miniscule x"
   case(   121  ); STRING="y miniscule y"
   case(   122  ); STRING="z miniscule z"
   case(   123  ); STRING="{ left brace"
   case(   124  ); STRING="| vertical line"
   case(   125  ); STRING="} right brace"
   case(   126  ); STRING="~ tilde"
   case(   127  ); STRING="ctrl-? (DEL) delete"
   case default
         STRING='UNKNOWN'//v2s(ICHAR(ch))
   end select
end function describe
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!      string_to_values - [M_strings]read a string representing numbers into a numeric array
!!
!!##SYNOPSIS
!!
!!       subroutine string_to_values(line,iread,values,inums,delims,ierr)
!!
!!        character(len=*) :: line
!!        integer          :: iread
!!        real             :: values(*)
!!        integer          :: inums
!!        character(len=*) :: delims
!!        integer          :: ierr
!!
!!##DESCRIPTION
!!    This routine can take a string representing a series of numbers and
!!    convert it to a numeric array and return how many numbers were found.
!!
!!##OPTIONS
!!
!!       LINE     Input string containing numbers
!!       IREAD    maximum number of values to try to read from input string
!!
!!##RESULTS
!!
!!       VALUES   real array to be filled with numbers
!!       INUMS    number of values successfully read (before error occurs
!!                if one does)
!!       DELIMS   delimiter character(s), usually a space. must not be a
!!                null string. If more than one character, a space must
!!                not be the last character or it will be ignored.
!!       IERR     error flag (0=no error, else column number string starts
!!                at that error occurred on).
!!
!!##EXAMPLE
!!
!!    Sample Program:
!!
!!      program demo_string_to_values
!!       use M_strings, only : string_to_values
!!       character(len=80)  :: s=' 10 20e3;3.45 -400.3e-2;1234; 5678 '
!!       integer,parameter  :: isz=10
!!       real               :: array(isz)
!!
!!       call string_to_values(s,10,array,inums,' ;',ierr)
!!       call reportit()
!!
!!       call string_to_values('10;2.3;3.1416',isz,array,inums,' ;',ierr)
!!       call reportit()
!!
!!       contains
!!          subroutine reportit()
!!             write(*,*)'string_to_values:'
!!             write(*,*)'input string.............',trim(s)
!!             write(*,*)'number of values found...',inums
!!             write(*,*)'values...................',(array(ii),ii=1,inums)
!!          end subroutine reportit
!!      end program demo_string_to_values
!!
!!    Expected output
!!
!!     string_to_values:
!!     input string............. 10 20e3;3.45 -400.3e-2;1234; 5678
!!     number of values found...           6
!!     values...................   10.0000000  20000.0000  3.45000005  -4.00299978  1234.00000  5678.00000
!!     string_to_values:
!!     input string............. 10 20e3;3.45 -400.3e-2;1234; 5678
!!     number of values found...           3
!!     values...................   10.0000000  2.29999995  3.14159989
!===================================================================================================================================
subroutine string_to_values(line,iread,values,inums,delims,ierr)
use M_journal, only : journal
implicit none
!----------------------------------------------------------------------------------------------------------------------------------
!   1989,1997-12-31,2014 John S. Urban

!   given a line of structure , string , string , string process each
!   string as a numeric value and store into an array.
!   DELIMS contain the legal delimiters. If a space is an allowed delimiter, it must not appear last in DELIMS.
!   There is no direct checking for more values than can fit in VALUES.
!   Quits if encounters any errors in read.
!----------------------------------------------------------------------------------------------------------------------------------
character(len=*),parameter::ident="@(#)M_strings::string_to_values(3f): reads an array of numbers from a numeric string"
character(len=*),intent(in)  :: line          ! input string
integer,intent(in)           :: iread         ! maximum number of values to try to read into values
real,intent(inout)           :: values(iread) ! real array to be filled with values
integer,intent(out)          :: inums         ! number of values successfully read from string
character(len=*),intent(in)  :: delims        ! allowed delimiters
integer,intent(out)          :: ierr          ! 0 if no error, else column number undecipherable string starts at
!----------------------------------------------------------------------------------------------------------------------------------
   character(len=256)           :: delims_local        ! mutable copy of allowed delimiters
   integer                      :: istart,iend,ilen,icol
   integer                      :: i10,i20,i40
   real                         :: rval
   integer                      :: ier
   integer                      :: delimiters_length
!----------------------------------------------------------------------------------------------------------------------------------
      delims_local=delims                                 ! need a mutable copy of the delimiter list
      if(delims_local.eq.'')then                          ! if delimiter list is null or all spaces make it a space
         delims_local=' '                                 ! delimiter is a single space
         delimiters_length=1                        ! length of delimiter list
      else
         delimiters_length=len_trim(delims)         ! length of variable WITH TRAILING WHITESPACE TRIMMED
      endif
!----------------------------------------------------------------------------------------------------------------------------------
      ierr=0                                        ! initialize error code returned
      inums=0                                       ! initialize count of values successfully returned
!----------------------------------------------------------------------------------------------------------------------------------
      ilen=0                                        ! ilen will be the position of the right-most non-delimiter in the input line
      do i20=len(line),1,-1                         ! loop from end of string to beginning to find right-most non-delimiter
         if(index(delims_local(:delimiters_length),line(i20:i20)).eq.0)then   ! found a non-delimiter
            ilen=i20
            exit
         endif
      enddo
      if(ilen.eq.0)then                             ! command was totally composed of delimiters
         call journal('*string_to_values* blank line passed as a list of numbers')
         return
      endif
!----------------------------------------------------------------------------------------------------------------------------------
!     there is at least one non-delimiter sub-string
!     ilen is the column position of the last non-delimiter character
!     now, starting at beginning of string find next non-delimiter
      icol=1                                                     ! pointer to beginning of unprocessed part of LINE
      LOOP: dO i10=1,iread,1                                     ! each pass should find a value
         if(icol.gt.ilen) EXIT LOOP                              ! everything is done
         INFINITE: do
            if(index(delims_local(:delimiters_length),line(icol:icol)).eq.0)then           ! found non-delimiter
               istart=icol
               iend=0                                            ! FIND END OF SUBSTRING
               do i40=istart,ilen                                ! look at each character starting at left
                  if(index(delims_local(:delimiters_length),line(i40:i40)).ne.0)then       ! determine if character is a delimiter
                     iend=i40                                    ! found a delimiter. record where it was found
                     EXIT                                        ! found end of substring so leave loop
                  endif
               enddo
              if(iend.eq.0)iend=ilen+1                           ! no delimiters found, so this substring goes to end of line
               iend=iend-1                                       ! do not want to pass delimiter to be converted
               rval=0.0
               call string_to_value(line(istart:iend),rval,ier)  ! call procedure to convert string to a numeric value
               if(ier.eq.0)then                                  ! a substring was successfully converted to a numeric value
                  values(i10)=rval                               ! store numeric value in return array
                  inums=inums+1                                  ! increment number of values converted to a numeric value
               else                                              ! an error occurred converting string to value
                  ierr=istart                                    ! return starting position of substring that could not be converted
                  return
               endif
               icol=iend+2                                       ! set to next character to look at
               CYCLE LOOP                                        ! start looking for next value
            else                                                 ! this is a delimiter so keep looking for start of next string
               icol=icol+1                                       ! increment pointer into LINE
               CYCLE INFINITE
            endif
         enddo INFINITE
      enddo LOOP
!     error >>>>> more than iread numbers were in the line.
end subroutine string_to_values
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!      s2vs - [M_strings]given a string representing numbers return a numeric array
!!
!!##SYNOPSIS
!!
!!       function s2vs(line[,delim])
!!
!!        character(len=*) :: line
!!        doubleprecision,allocatable :: s2vs(:)
!!
!!##DESCRIPTION
!!
!!    The function S2VS(3f) takes a string representing a series of numbers
!!    and converts it to a numeric doubleprecision array. The string values
!!    may be delimited by spaces, semi-colons, and commas by default.
!!
!!##OPTIONS
!!       LINE   Input string containing numbers
!!       DELIM  optional list of delimiter characters. If a space is
!!              included, it should appear as the left-most character
!!              in the list. The default is " ;," (spaces, semi-colons,
!!              and commas).
!!##RESULTS
!!       S2VS   doubleprecision array
!!
!!##EXAMPLE
!!
!!
!!     Sample Program:
!!
!!      program demo_s2vs
!!      use M_strings, only : s2vs
!!      character(len=80)           :: s=' 10 20e3;3.45 -400.3e-2;1234; 5678 '
!!      doubleprecision,allocatable :: values(:)
!!      integer,allocatable         :: ivalues(:)
!!
!!      values=s2vs(s)
!!      ivalues=int(s2vs(s))
!!      call reportit()
!!
!!      contains
!!        subroutine reportit()
!!          write(*,*)'S2VS:'
!!          write(*,*)'input string.............',trim(s)
!!          write(*,*)'number of values found...',size(values)
!!          write(*,*)'values...................',(values(ii),ii=1,size(values))
!!          write(*,*)'ivalues..................',(ivalues(ii),ii=1,size(values))
!!        end subroutine reportit
!!      end program demo_s2vs
!!
!!    Expected output
!!
!!     S2VS:
!!     input string............. 10 20e3;3.45 -400.3e-2;1234; 5678
!!     number of values found... 6
!!     values................... 10.000000000000000  20000.000000000000 3.4500000000000002
!!     -4.0030000000000001       1234.0000000000000  5678.0000000000000
!!     ivalues.................. 10  20000  3  -4 1234 5678
!===================================================================================================================================
function s2vs(string,delim) result(darray)
character(len=*),parameter::ident="@(#)M_strings::s2vs(3f): function returns array of values from a string"
character(len=*),intent(in)        :: string                       ! keyword to retrieve value for from dictionary
character(len=*),optional          :: delim                        ! delimiter characters
character(len=:),allocatable       :: delim_local
doubleprecision,allocatable        :: darray(:)                    ! function type

   character(len=132),allocatable  :: carray(:)          ! convert value to an array using split(3f)
   integer                         :: i
   integer                         :: ier
!-----------------------------------------------------------------------------------------------------------------------------------
   if(present(delim))then
      delim_local=delim
   else
      delim_local=' ;,'
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   call split(string,carray,delimiters=delim_local)         ! split string into an array
   allocate(darray(size(carray)))                           ! create the output array
   do i=1,size(carray)
      call string_to_value(carray(i), darray(i), ier)       ! convert the string to a numeric value
   enddo
!-----------------------------------------------------------------------------------------------------------------------------------
end function s2vs
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
elemental function isprint(onechar)
character(len=*),parameter::ident="@(#)M_strings::isprint(3f): indicates if input character is a printable ASCII character"
character,intent(in) :: onechar
logical              :: isprint
   select case (onechar)
      case (' ':'~')   ; isprint=.TRUE.
      case default     ; isprint=.FALSE.
   end select
end function isprint
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
elemental function isgraph(onechar)
character(len=*),parameter::ident="@(#)M_strings::isgraph(3f) :indicates if character is printable ASCII character excluding space"
character,intent(in) :: onechar
logical              :: isgraph
   select case (iachar(onechar))
   case (33:126)
     isgraph=.TRUE.
   case default
     isgraph=.FALSE.
   end select
end function isgraph
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
elemental function isalpha(ch) result(res)
character(len=*),parameter::ident="@(#)M_strings::isalpha(3f): Return .true. if character is a letter and .false. otherwise"
character,intent(in) :: ch
logical              :: res
   select case(ch)
   case('A':'Z','a':'z')
     res=.true.
   case default
     res=.false.
   end select
end function isalpha
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
elemental function isxdigit(ch) result(res)
character(len=*),parameter::ident="@(#)M_strings::isxdigit(3f): returns .true. if c is a hexadecimal digit (0-9,a-f, or A-F)"
character,intent(in) :: ch
logical              :: res
   select case(ch)
   case('A':'F','a':'f','0':'9')
     res=.true.
   case default
     res=.false.
   end select
end function isxdigit
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
elemental function isdigit(ch) result(res)
character(len=*),parameter::ident="@(#)M_strings::isdigit(3f): Returns .true. if ch is a digit (0-9) and .false. otherwise"
character,intent(in) :: ch
logical              :: res
   select case(ch)
   case('0':'9')
     res=.true.
   case default
     res=.false.
   end select
end function isdigit
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
elemental function isblank(ch) result(res)
character(len=*),parameter::ident="@(#)M_strings::isblank(3f): returns .true. if character is a blank (space or horizontal tab)"
character,intent(in) :: ch
logical              :: res
   select case(ch)
   case(' ',char(9))
     res=.true.
   case default
     res=.false.
   end select
end function isblank
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
elemental function isascii(ch) result(res)
character(len=*),parameter::ident="@(#)M_strings::isascii(3f): returns .true. if character is in the range char(0) to char(127)"
character,intent(in) :: ch
logical              :: res
   select case(ichar(ch))
   case(0:127)
     res=.true.
   case default
     res=.false.
   end select
end function isascii
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
elemental function isspace(ch) result(res)
character(len=*),parameter::ident="@(#)M_strings::isspace(3f): true if null,space,tab,return,new line,vertical tab, or formfeed"
character,intent(in) :: ch
logical              :: res
   select case(ch)
   case(' ')                 ! space(32)
     res=.true.
   case(char(0))             ! null(0)
     res=.true.
   case(char(9):char(13))    ! tab(9), new line(10), vertical tab(11), formfeed(12), carriage return(13),
     res=.true.
   case default
     res=.false.
   end select
end function isspace
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
elemental function iscntrl(ch) result(res)
character(len=*),parameter::ident="@(#)M_strings::iscntrl(3f): true if a delete or ordinary control character(0x7F or 0x00-0x1F)"
character,intent(in) :: ch
logical              :: res
   select case(ch)
   case(char(127),char(0):char(31))
     res=.true.
   case default
     res=.false.
   end select
end function iscntrl
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
elemental function ispunct(ch) result(res)
character(len=*),parameter::ident="@(#)M_strings::ispunct(3f): true if a printable punctuation character (isgraph(c)&&"
character,intent(in) :: ch
logical              :: res
   select case(ch)
   case (char(33):char(47), char(58):char(64), char(91):char(96), char(123):char(126))
     res=.true.
!  case(' ','0':'9','A':'Z','a':'z',char(128):)
!    res=.true.
!  case(char(0):char(31),char(127))
!    res=.true.
   case default
     res=.false.
   end select
end function ispunct
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
pure elemental function isupper(ch) result(res)
character(len=*),parameter::ident="@(#)M_strings::isupper(3f): returns true if character is an uppercase letter (A-Z)"
character,intent(in) :: ch
logical              :: res
   select case(ch)
   case('A':'Z')
     res=.true.
   case default
     res=.false.
   end select
end function isupper
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
elemental function islower(ch) result(res)
character(len=*),parameter::ident="@(#)M_strings::islower(3f): returns true if character is a miniscule letter (a-z)"
character,intent(in) :: ch
logical              :: res
   select case(ch)
   case('a':'z')
     res=.true.
   case default
     res=.false.
   end select
end function islower
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    isalnum,isalpha,iscntrl,isdigit,isgraph,islower,
!!    isprint,ispunct,isspace,isupper,isascii,isblank,isxdigit - [M_strings]test membership in subsets of ASCII character set
!!
!!##SYNOPSIS
!!
!!    Where "FUNCNAME" is one of the function names in the group, the functions are defined by
!!
!!     elemental function FUNCNAME(onechar)
!!     character,intent(in) :: onechar
!!     logical              :: FUNC_NAME
!!
!!##DESCRIPTION
!!
!!       These elemental functions test if a character belongs to various subsets of the ASCII character set.
!!
!!       o isalnum:   returns .true. if character is a letter (a-z,A-Z) or digit (0-9)
!!       o isalpha:   returns .true. if character is a letter and .false. otherwise
!!       o isascii:   returns .true. if character is in the range char(0) to char(127)
!!       o isblank:   returns .true. if character is a blank (space or horizontal tab).
!!       o iscntrl:   returns .true. if character is a delete character or ordinary control character (0x7F or 0x00-0x1F).
!!       o isdigit:   returns .true. if character is a digit (0,1,...,9) and .false. otherwise
!!       o isgraph:   returns .true. if character is a printable ASCII character excluding space
!!       o islower:   returns .true. if character is a miniscule letter (a-z)
!!       o isprint:   returns .true. if character is a printable ASCII character
!!       o ispunct:   returns .true. if character is a printable punctuation character (isgraph(c) && !isalnum(c)).
!!       o isspace:   returns .true. if character is a null, space, tab, carriage return, new line, vertical tab, or formfeed
!!       o isupper:   returns .true. if character is an uppercase letter (A-Z)
!!       o isxdigit:  returns .true. if character is a hexadecimal digit (0-9, a-f, or A-F).
!!
!!##EXAMPLES
!!
!!
!!   program demo_isdigit
!!
!!    use m_strings, only : isdigit, isspace, switch
!!    implicit none
!!    character(len=10),allocatable :: string(:)
!!    integer                       :: i
!!       string=[&
!!       & '1 2 3 4 5 ' ,&
!!       & 'letters   ' ,&
!!       & '1234567890' ,&
!!       & 'both 8787 ' ]
!!       ! if string is nothing but digits and whitespace return .true.
!!       do i=1,size(string)
!!          write(*,'(a)',advance='no')'For string['//string(i)//']'
!!          write(*,*) &
!!          all(isdigit(switch(string(i))).or.isspace(switch(string(i))))
!!       enddo
!!
!!   end program demo_isdigit
!!
!!   Expected output:
!!
!!    For string[1 2 3 4 5 ] T
!!    For string[letters   ] F
!!    For string[1234567890] T
!!    For string[both 8787 ] F
!===================================================================================================================================
elemental function isalnum(ch) result(res)
character(len=*),parameter::ident="@(#)M_strings::isalnum(3f): returns true if character is a letter (a-z,A-Z) or digit(0-9)"
character,intent(in)       :: ch
logical                    :: res
   select case(ch)
   case('a':'z','A':'Z','0':'9')
     res=.true.
   case default
     res=.false.
   end select
end function isalnum
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
END MODULE M_strings
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!
!           X
!           X
!  XXXXX   XXXX   XXX XX   XXX    XX XX    XXXXXX
! X     X   X       XX  X    X     XX  X  X    X
!  XXX      X       X        X     X   X  X    X
!     XX    X       X        X     X   X  X    X
! X     X   X  X    X        X     X   X   XXXXX
!  XXXXX     XX   XXXXX    XXXXX  XXX XXX      X
!                                              X
!                                          XXXX
!
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
module M_strings_oop
!
! Define an OOP (Object-Oriented Programming) interface for the M_strings module.
!
! Leveraging the existing procedural functions in module M_strings to do the calculations allows
! this to be a definition of a derived type ( TYPE(STRING) ) and the
! methods it supports and overloading of operators to support the new data type.
!
use M_strings, only : upper, lower                            ! case
use M_strings, only : lenset, adjustc, compact, crop  ! whitespace
use M_strings, only : reverse
use M_strings, only : notabs, noesc, expand
use M_strings, only : substitute, transliterate
use M_strings, only : string_to_value, switch, v2s, s2v
use M_strings, only : switch, split, matchw
implicit none
private
integer,parameter,private :: dp=kind(0.0d0)
public p
!-----------------------------------------------------------------------------------------------------------------------------------
   public string
!-----------------------------------------------------------------------------------------------------------------------------------
!DERIVED TYPE STRING
!
type string
   ! COMPONENTS:
   character(len=:),allocatable :: str
contains
   ! METHODS:
   procedure  ::  adjustc        =>  oop_adjustc
   procedure  ::  adjustl        =>  oop_adjustl
   procedure  ::  adjustr        =>  oop_adjustr
   procedure  ::  compact        =>  oop_compact
   procedure  ::  crop           =>  oop_crop
   procedure  ::  dble           =>  oop_dble
   procedure  ::  expand         =>  oop_expand
   procedure  ::  index          =>  oop_index
   procedure  ::  init           =>  init_string
   procedure  ::  int            =>  oop_int
   procedure  ::  len            =>  oop_len
   procedure  ::  len_trim       =>  oop_len_trim
   procedure  ::  lenset         =>  oop_lenset
   procedure  ::  match          =>  oop_matchw
   procedure  ::  lower          =>  oop_lower
   procedure  ::  noesc          =>  oop_noesc
   procedure  ::  notabs         =>  oop_notabs
   procedure  ::  real           =>  oop_real
   procedure  ::  reverse        =>  oop_reverse
   procedure  ::  substitute     =>  oop_substitute
   procedure  ::  transliterate  =>  oop_transliterate
   procedure  ::  trim           =>  oop_trim
   procedure  ::  upper          =>  oop_upper
   procedure  ::  chars          =>  oop_switch
!!   procedure  ::  split          =>  oop_split
   !DECLARATION OF OVERLOADED OPERATORS FOR TYPE(STRING)
   procedure,private :: eq
   generic           :: operator(==) => eq
   procedure,private :: lt
   generic           :: operator(<)  => lt
   procedure,private :: gt
   generic           :: operator(>)  => gt
   procedure,private :: ge
   generic           :: operator(>=) => ge
   procedure,private :: le
   generic           :: operator(<=) => le
   procedure,private :: ne
   generic           :: operator(/=) => ne

   procedure,private :: string_plus_value
   generic           :: operator(+) => string_plus_value   ! string + integer|real|doubleprecision|string|character
   procedure,private :: string_minus_value
   generic           :: operator(-) => string_minus_value  ! string - integer|real|doubleprecision|string|character
   procedure,private :: string_multiply_value
   generic           :: operator(*) => string_multiply_value  ! string * integer|real|doubleprecision
   procedure,private :: string_append_value
   generic           :: operator(//) => string_append_value

!!   procedure,private :: minus_string
!!   generic           :: operator(-)  => minus_string
end type
!===================================================================================================================================
! User-defined constructors are created by defining a generic interface
! with the same name as the derived type they're supposed to construct.
interface string
   module procedure construct_from_fill
end interface string
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
contains
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! this function is used internally in the module, but is also declared to be a constructor for creating TYPE(DATE_TYPE) structures
!
function construct_from_fill(chars,len)
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=*),parameter::ident="@(#)M_strings::construct_from_fill(3f): construct TYPE(STRING)"
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=*),intent(in),optional :: chars
integer,intent(in),optional          :: len
type(string)                         :: construct_from_fill
   if(present(chars))then
      construct_from_fill%str=chars
   else
      construct_from_fill%str=''
   endif
   if(present(len))then
      construct_from_fill%str=lenset(construct_from_fill%str,len)
   endif
end function construct_from_fill
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! DEFINE THE METHODS FOR THE TYPE
! These functions are privately used to define the methods that TYPE(STRING) will support
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function oop_len(self) result (length)
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=*),parameter::ident="@(#)M_strings::oop_len(3f): length of string"
!-----------------------------------------------------------------------------------------------------------------------------------
class(string),intent(in)    :: self
integer                     :: length
   length=len(self%str)
end function oop_len
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function oop_len_trim(self) result (length)
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=*),parameter::ident="@(#)M_strings::oop_len_trim(3f): trimmed length of string"
!-----------------------------------------------------------------------------------------------------------------------------------
class(string),intent(in)    :: self
integer                     :: length
   length=len_trim(self%str)
end function oop_len_trim
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function oop_switch(self) result (array)
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=*),parameter::ident="@(#)M_strings::oop_switch(3f): convert string to array of single characters"
!-----------------------------------------------------------------------------------------------------------------------------------
class(string),intent(in)    :: self
character(len=1)            :: array(len(self%str))
   array=switch(self%str)
end function oop_switch
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function oop_index(self,substring,back) result (location)
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=*),parameter::ident="@(#)M_strings::oop_index(3f): find starting position of a substring in a string"
!-----------------------------------------------------------------------------------------------------------------------------------
class(string),intent(in)    :: self
character(len=*),intent(in) :: substring
integer                     :: location
logical,optional,intent(in) :: back
   if(present(back))then
      location=index(self%str,substring,back)
   else
      location=index(self%str,substring)
   endif
end function oop_index
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function oop_upper(self) result (string_out)
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=*),parameter::ident="@(#)M_strings::oop_upper(3f): convert string to uppercase"
!-----------------------------------------------------------------------------------------------------------------------------------
class(string),intent(in)     :: self
type(string)                 :: string_out
   string_out%str=upper(self%str)
end function oop_upper
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function oop_lower(self) result (string_out)
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=*),parameter::ident="@(#)M_strings::oop_lower(3f): convert string to miniscule"
!-----------------------------------------------------------------------------------------------------------------------------------
class(string),intent(in)     :: self
type(string)                 :: string_out
   string_out%str=lower(self%str)
end function oop_lower
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function oop_expand(self,escape_char) result (string_out)
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=*),parameter::ident="@(#)M_strings::oop_expand(3f): expand common escape sequences by calling expand(3f)"
!-----------------------------------------------------------------------------------------------------------------------------------
class(string),intent(in)      :: self
character,intent(in),optional :: escape_char
type(string)                  :: string_out
   if(present(escape_char))then
      string_out%str=expand(self%str,escape_char)
   else
      string_out%str=expand(self%str)
   endif
end function oop_expand
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function oop_trim(self) result (string_out)
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=*),parameter::ident="@(#)M_strings::oop_trim(3f): trim trailing spaces"
!-----------------------------------------------------------------------------------------------------------------------------------
class(string),intent(in)     :: self
type(string)                 :: string_out
   string_out%str=trim(self%str)
end function oop_trim
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function oop_crop(self) result (string_out)
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=*),parameter::ident="@(#)M_strings::oop_crop(3f): crop leading and trailing spaces"
!-----------------------------------------------------------------------------------------------------------------------------------
class(string),intent(in)     :: self
type(string)                 :: string_out
   string_out%str=crop(self%str)
end function oop_crop
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function oop_reverse(self) result (string_out)
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=*),parameter::ident="@(#)M_strings::oop_reverse(3f): reverse string"
!-----------------------------------------------------------------------------------------------------------------------------------
class(string),intent(in)     :: self
type(string)                 :: string_out
   string_out%str=reverse(self%str)
end function oop_reverse
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function oop_adjustl(self) result (string_out)
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=*),parameter::ident="@(#)M_strings::oop_adjustl(3f): adjust string to left"
!-----------------------------------------------------------------------------------------------------------------------------------
class(string),intent(in)     :: self
type(string)                 :: string_out
   string_out%str=adjustl(self%str)
end function oop_adjustl
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function oop_adjustr(self) result (string_out)
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=*),parameter::ident="@(#)M_strings::oop_adjustr(3f): adjust string to right"
!-----------------------------------------------------------------------------------------------------------------------------------
class(string),intent(in)     :: self
type(string)                 :: string_out
   string_out%str=adjustr(self%str)
end function oop_adjustr
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function oop_adjustc(self,length) result (string_out)
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=*),parameter::ident="@(#)M_strings::oop_adjustc(3f): adjust string to center"
!-----------------------------------------------------------------------------------------------------------------------------------
class(string),intent(in)     :: self
type(string)                 :: string_out
integer,intent(in),optional  :: length
   if(present(length))then
      string_out%str=lenset(adjustc(self%str,length),length)
   else
      string_out%str=adjustc(self%str)
   endif
end function oop_adjustc
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function oop_int(self) result (value)
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=*),parameter::ident="@(#)M_strings::oop_int(3f): string to integer"
!-----------------------------------------------------------------------------------------------------------------------------------
class(string),intent(in)     :: self
integer                      :: value
integer                      :: ierr
   call string_to_value(self%str,value,ierr)
end function oop_int
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function oop_real(self) result (value)
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=*),parameter::ident="@(#)M_strings::oop_real(3f): string to real"
!-----------------------------------------------------------------------------------------------------------------------------------
class(string),intent(in)     :: self
real                         :: value
integer                      :: ierr
   call string_to_value(self%str,value,ierr)
end function oop_real
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function oop_dble(self) result (value)
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=*),parameter::ident="@(#)M_strings::oop_dble(3f): string to double"
!-----------------------------------------------------------------------------------------------------------------------------------
class(string),intent(in)     :: self
doubleprecision              :: value
integer                      :: ierr
   call string_to_value(self%str,value,ierr)
end function oop_dble
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function oop_compact(self,char) result (string_out)
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=*),parameter::ident="@(#)M_strings::oop_compact(3f): adjust string to center"
!-----------------------------------------------------------------------------------------------------------------------------------
class(string),intent(in)     :: self
type(string)                 :: string_out
character(len=*),optional    :: char
   if(present(char))then
      string_out%str=compact(self%str,char)
   else
      string_out%str=compact(self%str)
   endif
   string_out%str=trim(string_out%str)
end function oop_compact
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function oop_substitute(self,old,new) result (string_out)
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=*),parameter::ident="&
&@(#)M_strings::oop_substitute(3f): change all occurrences of oldstring to newstring non-recursively"
!-----------------------------------------------------------------------------------------------------------------------------------
class(string),intent(in)     :: self
type(string)                 :: string_out
character(len=*),intent(in)  :: old
character(len=*),intent(in)  :: new
   string_out%str=self%str
   call substitute(string_out%str,old,new)
end function oop_substitute
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function oop_transliterate(self,old,new) result (string_out)
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=*),parameter::ident="&
&@(#)M_strings::oop_transliterate(3f): change all occurrences of oldstring to newstring non-recursively"
!-----------------------------------------------------------------------------------------------------------------------------------
class(string),intent(in)     :: self
type(string)                 :: string_out
character(len=*),intent(in)  :: old
character(len=*),intent(in)  :: new
   string_out%str=transliterate(self%str,old,new)
end function oop_transliterate
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function oop_lenset(self,length) result (string_out)
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=*),parameter::ident="@(#)M_strings::oop_lenset(3f): set string to specific length"
!-----------------------------------------------------------------------------------------------------------------------------------
class(string),intent(in)     :: self
type(string)                 :: string_out
integer,intent(in)           :: length
   string_out%str=lenset(self%str,length)
end function oop_lenset
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function oop_matchw(self,pattern) result (answer)
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=*),parameter::ident="@(#)M_strings::oop_matchw(3f): test if wildcard pattern matches string"
!-----------------------------------------------------------------------------------------------------------------------------------
class(string),intent(in)     :: self
character(len=*),intent(in)  :: pattern
logical                      :: answer
   answer=matchw(self%str,pattern)
end function oop_matchw
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function oop_notabs(self) result (string_out)
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=*),parameter::ident="&
&@(#)M_strings::oop_notabs(3f): expand tab characters assuming tab stops every eight(8) characters"
!-----------------------------------------------------------------------------------------------------------------------------------
class(string),intent(in)     :: self
type(string)                 :: string_out
   integer                   :: length
   string_out%str=lenset('',8*len(self%str)) ! make long enough assuming all tab characters
   call notabs(self%str,string_out%str,length)
   string_out%str=trim(string_out%str)
end function oop_notabs
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function oop_noesc(self) result (string_out)
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=*),parameter::ident="@(#)M_strings::oop_noesc(3f): replace non-printable characters with spaces"
!-----------------------------------------------------------------------------------------------------------------------------------
class(string),intent(in)     :: self
type(string)                 :: string_out
   string_out%str=noesc(self%str)
end function oop_noesc
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function p(self) result (string_out)
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=*),parameter::ident="@(#)M_strings::oop_p(3f): return CHARACTER string from TYPE(STRING)"
!-----------------------------------------------------------------------------------------------------------------------------------
class(string),intent(in)     :: self
character(len=len(self%str)) :: string_out
   string_out=self%str
end function p
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine init_string(self)
!
! allow for TYPE(STRING) object to be initialized.
!
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=*),parameter::ident="@(#)M_strings::init_dt(3f): initialize TYPE(STRING)"
!-----------------------------------------------------------------------------------------------------------------------------------
class(string)                        :: self
   self%str=''
end subroutine init_string
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! FUNCTIONS FOR DEFINING OVERLOADED OPERATORS
!===================================================================================================================================
function string_plus_value(self,value) result (other)
character(len=*),parameter::ident="@(#)M_strings::string_plus_value(3f): add value to TYPE(STRING)"
class(string),intent(in)      :: self
type(string)                  :: other
class(*),intent(in)           :: value
!  This function is primarily intended to provide behaviors for the "+" operator for TYPE(STRING) values
!  Notice that the value can be any of several types ( INTEGER,REAL,DOUBLEPRECISION,CHARACTER,TYPE(STRING) )
   select type(value)
   type is (integer);          other%str=v2s(value+s2v(self%str))  ! convert string%str to integer, add to value
   type is (real);             other%str=v2s(value+s2v(self%str))  ! convert string%str to real, add to value
   type is (doubleprecision);  other%str=v2s(value+s2v(self%str))  ! convert string%str to doubleprecision, add to value
   type is (character(len=*)); other%str=self%str//' '//value      ! append space and CHARACTER to string %str
   type is (string);           other%str=self%str//' '//value%str  ! append string %str values with space between
   end select
end function string_plus_value
!===================================================================================================================================
function string_minus_value(self,value) result (other)
character(len=*),parameter::ident="@(#)M_strings::string_minus_value(3f): subtract value from TYPE(STRING)"
class(string),intent(in)      :: self
type(string)                  :: other
class(*),intent(in)           :: value
!  This function is primarily intended to provide behaviors for the "-" operator for TYPE(STRING) values
!  Notice that the value can be any of several types ( INTEGER,REAL,DOUBLEPRECISION,CHARACTER,TYPE(STRING) )
   select type(value)
   type is (integer);         other%str=v2s(value-s2v(self%str))
   type is (real);            other%str=v2s(value-s2v(self%str))
   type is (doubleprecision); other%str=v2s(value-s2v(self%str))
   type is (character(len=*))
      other%str=self%str
      call substitute(other%str,value,'')
   type is (string)
      other%str=self%str
      call substitute(other%str,value%str,'')
   end select
end function string_minus_value
!===================================================================================================================================
function string_append_value(self,value) result (other)
character(len=*),parameter::ident="@(#)M_strings::string_append_value(3f): append value to TYPE(STRING)"
class(string),intent(in)      :: self
type(string)                  :: other
class(*),intent(in)           :: value
!  This function is primarily intended to provide behaviors for the "//" operator for TYPE(STRING) values
!  Notice that the value can be any of several types ( INTEGER,REAL,DOUBLEPRECISION,CHARACTER,TYPE(STRING) )
   select type(value)
   type is (integer);          other%str=self%str//v2s(value)
   type is (real);             other%str=self%str//v2s(value)
   type is (doubleprecision);  other%str=self%str//v2s(value)
   type is (character(len=*)); other%str=self%str//value
   type is (string);           other%str=self%str//value%str
   end select
end function string_append_value
!===================================================================================================================================
function string_multiply_value(self,value) result (other)
character(len=*),parameter::ident="@(#)M_strings::string_multiply_value(3f): multiply TYPE(STRING) value times"
class(string),intent(in)      :: self
type(string)                  :: other
class(*),intent(in)           :: value
!  This function is primarily intended to provide behaviors for the "//" operator for TYPE(STRING) values
!  Notice that the value can be any of several types ( INTEGER,REAL,DOUBLEPRECISION )
   select type(value)
   type is (integer);          other%str=repeat(self%str,value)
   type is (real);             other%str=repeat(self%str,nint(value))
   type is (doubleprecision);  other%str=repeat(self%str,nint(value))
   end select
end function string_multiply_value
!===================================================================================================================================
logical function eq(self,other)
character(len=*),parameter::ident="@(#)M_strings::eq(3f): compare derived type string objects (eq,lt,gt,le,ge,ne)"
   class(string),intent(in) :: self
   type(string),intent(in)  :: other
   eq= self%str .eq.  other%str
end function eq
logical function lt(self,other)
   class(string),intent(in) :: self
   type(string),intent(in)  :: other
   lt= self%str .lt.  other%str
end function lt
logical function gt(self,other)
   class(string),intent(in) :: self
   type(string),intent(in)  :: other
   gt= self%str .gt.  other%str
end function gt
logical function le(self,other)
   class(string),intent(in) :: self
   type(string),intent(in)  :: other
   le= self%str .le.  other%str
end function le
logical function ge(self,other)
   class(string),intent(in) :: self
   type(string),intent(in)  :: other
   ge= self%str .ge.  other%str
end function ge
logical function ne(self,other)
   class(string),intent(in) :: self
   type(string),intent(in)  :: other
   ne= self%str .ne.  other%str
end function ne
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
end module M_strings_oop
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!! FODDER
!!
!! join(seq) Merges (concatenates) the string representations of elements in sequence seq into a string, with separator string.

!!    REPEAT        (STRING, NCOPIES)              T    Concatenation of several copies of a string.

!!   * TOKENS
!!       + split subroutine parses string using specified delimiter characters and stores tokens into an array
!!   * EDITING
!!       + modif subroutine modifies a string with a directive like the XEDIT line editor MODIFY command
!!   * WHITE SPACE
!!       + indent find number of leading spaces
!!   * CHARACTER ARRAY VERSUS STRING
!!       + s2c convert string to array of single characters and add null terminator for passing to C
!!
!!        elemental function FUNCNAME(onechar)
!!        character,intent(in) :: onechar
!!        logical              :: FUNC_NAME
!!
!! DESCRIPTION
!!     These elemental functions test if a character belongs to various subsets of
!!     the ASCII character set.
!!    + isalnum: returns .T. if character is a letter (a-z,A-Z) or digit (0-9)
!!    + isalpha: returns .T. if character is a letter and ..F.. otherwise
!!    + isascii: returns .T. if character is in the range char(0) to char (127)
!!    + isblank: returns .T. if character is a blank (space or horizontal tab).
!!    + iscntrl: returns .T. if character is a delete character or ordinary control character (0x7F or 0x00-0x1F).
!!    + isdigit: returns .T. if character is a digit (0,1,...,9) and ..F..  otherwise
!!    + isgraph: returns .T. if character is a printable ASCII character excluding space
!!    + islower: returns .T. if character is a miniscule letter (a-z)
!!    + isprint: returns .T. if character is a printable ASCII character
!!    + ispunct: returns .T. if character is a printable punctuation character (isgraph(c) && !isalnum(c)).
!!    + isspace: returns .T. if character is a null, space, tab, carriage return, new line, vertical tab, or formfeed
!!    + isupper: returns .T. if character is an uppercase letter (A-Z)
!!   ++ isupper: returns .T. if string has at least one cased character and all cased characters are in uppercase and .F. otherwise.
!!    + isxdigit: returns .T. if character is a hexadecimal digit (0-9, a-f, or A-F).
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
