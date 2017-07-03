[UP]

-----------------------------------------------------------------------------------------------------------------------------------
                                              Manual Reference Pages  - M_strings (3)
-----------------------------------------------------------------------------------------------------------------------------------
                                                                 
NAME

    M_strings - [M_strings]Fortran string module

CONTENTS

    Synopsis
    Description
    Examples

SYNOPSIS

    public entities:


       use m_strings, only : split,delim,chomp
       use m_strings, only : substitute,change,modif,transliterate,reverse
       use M_strings, only : upper,lower
       use M_strings, only : adjustc,compact,nospace,indent,crop
       use M_strings, only : len_white,lenset,merge_str
       use M_strings, only : switch,s2c,c2s
       use M_strings, only : noesc,notabs,expand
       use M_strings, only : string_to_value,string_to_values,s2v,s2vs,value_to_string,v2s
       use M_strings, only : matchw
       use M_strings, only : isalnum, isalpha, iscntrl, isdigit, isgraph, islower,
                             isprint, ispunct, isspace, isupper, isascii, isblank, isxdigit



    TOKENS

           split subroutine parses string using specified delimiter characters and stores tokens into an array

           delim subroutine parses string using specified delimiter characters and store tokens into an array

           chomp function consumes input line as it returns next token in a string using specified delimiters

    EDITING

             substitute subroutine non-recursively globally replaces old substring with new substring

             change subroutine non-recursively globally replaces old substring with new substring with a directive like line editor

             modif subroutine modifies a string with a directive like the XEDIT line editor MODIFY command

             transliterate replace characters found in set one with characters from set two

             reverse reverse character order in a string

    CASE

        upper function converts string to uppercase

        lower function converts string to miniscule

    WHITE SPACE

           adjustc elemental function centers text within the length of the input string

           compact left justify string and replace duplicate whitespace with single characters or nothing

           nospace function replaces whitespace with nothing

           indent find number of leading spaces

           crop function trims leading and trailing spaces

    STRING LENGTH

          len_white find location of last non-whitespace character

          lenset return a string of specified length

          merge_str make strings of equal length and then call MERGE(3f) intrinsic

    CHARACTER ARRAY VERSUS STRING

            switch switch between a string and an array of single characters

            s2c convert string to array of single characters and add null terminator for passing to C

            c2s convert null-terminated array of single characters to string for converting strings returned from C

    NONALPHA

           noesc convert non-printable ASCII8 characters to a space

           notabs convert tabs to spaces while maintaining columns, assuming tabs are set every 8 characters

           expand expand escape sequences in a string

    NUMERIC STRINGS

            string_to_value generic subroutine returns numeric value (REAL, DOUBLEPRECISION, INTEGER) from string

            string_to_values subroutine reads an array of numbers from a string

            s2v function returns DOUBLEPRECISION numeric value from string

            s2vs function returns a DOUBLEPRECISION array of numbers from a string

            value_to_string generic subroutine returns string given numeric value (REAL, DOUBLEPRECISION, INTEGER )

            v2s generic function returns string from numeric value (REAL, DOUBLEPRECISION, INTEGER )

    LOGICAL TESTS

          matchw compares given string for match to pattern which may contain wildcard characters

    CHARACTER TESTS

            o isalnum returns .true. if character is a letter or digit

            o isalpha returns .true. if character is a letter and .false. otherwise

            o iscntrl returns .true. if character is a delete character or ordinary control character

            o isdigit returns .true. if character is a digit (0,1,...,9) and .false. otherwise

            o isgraph returns .true. if character is a printable character except a space is considered non-printable

            o islower returns .true. if character is a miniscule letter (a-z)

            o isprint returns .true. if character is an ASCII printable character

            o ispunct returns .true. if character is a printable punctuation character

            o isspace returns .true. if character is a null, space, tab, carriage return, new line, vertical tab, or formfeed

            o isupper returns .true. if character is an uppercase letter (A-Z)

            o isascii returns .true. if the character is in the range char(0) to char(127)

            o isblank returns .true. if character is a blank character (space or horizontal tab.

            o isxdigit returns .true. if character is a hexadecimal digit (0-9, a-f, or A-F).

DESCRIPTION

    The M_strings module is a collection of Fortran procedures that process character strings. Routines for parsing, tokenizing,
    changing case, substituting new strings for substrings, locating strings with simple wildcard expressions, removing tabs and
    line terminators and other string manipulations are included.

    M_strings_oop is a companion module that provides an OOP interface to the M_strings module.

    As newer Fortran features become more widely available a significant amount of the code (much of which originated as
    pre-Fortran90 routines) is subject to updating so new versions of this module are not expected to be compatible with older
    versions.

    OOPS INTERFACE

    If you prefer an Object-oriented interface the M_strings_oop module (included with the M_strings module source) provides an OOP
    interface to the M_strings module; as described in the example program OBJECT_ORIENTED shown in the example program found
    below.

EXAMPLES

    Each of the procedural functions includes an example program in the corresponding man(1) page for the function. The
    object-oriented interface does not have individual man(1) pages, but is instead demonstrated using the following example
    program:

     program  object_oriented
     !
     ! This is an example using the object-oriented class/type model defined in M_strings_oop
     ! This is essentially the same functionality as the procedures combined with several Fortran intrinsics and overloaded operators
     !
     use M_strings_oop,only : string, p
     implicit none
     TYPE(string) :: str1
     TYPE(string) :: str2
     TYPE(string) :: str3
     !==============================================================================
       write(*,*) exercise the M_STRING_OOP module interface 
       ! draw a break line in the output
       write(*,*)repeat( = ,78)
       write(*,*) Call methods of type(STRING) 
       ! define TYPE(STRING) with constructor
       str2=string(    This  is  a  String!        )
       write(*,*)repeat( = ,78)
       ! print members of type
       write(*,101) str2%str is ................  ,str2%str
       ! same as intrinsic LEN()
       write(*,202) len ........................  ,str2%len()
       ! same as intrinsic INDEX()
       write(*,202) len_trim ...................  ,str2%len_trim()
       ! same as intrinsic INDEX()
       write(*,202) index("is").................  ,str2%index("is")
       ! same as intrinsic INDEX()
       write(*,202) index("is",back=.T.) .......  ,str2%index("is",back=.TRUE.)
       ! output TYPE(STRING) with %str all uppercase
       write(*,101) upper ......................  ,p(str2%upper())
       ! output TYPE(STRING) with %str all miniscule
       write(*,101) lower ......................  ,p(str2%lower())
       ! output TYPE(STRING) with %str reversed
       write(*,101) reverse ....................  ,p(str2%reverse())
       ! same as intrinsic ADJUSTL()
       write(*,101) adjustl ....................  ,p(str2%adjustl())
       ! same as intrinsic ADJUSTR()
       write(*,101) adjustr ....................  ,p(str2%adjustr())
       ! center string in current string length
       write(*,101) adjustc ....................  ,p(str2%adjustc())
       ! center string in string length of NN
       write(*,101) adjustc(49) ................  ,p(str2%adjustc(49))
       ! force %str to be NN characters long
       write(*,101) lenset(49) .................  ,p(str2%lenset(49))
       ! same as intrinsic TRIM()
       write(*,101) trim .......................  ,p(str2%trim())
       ! trim leading and trailing spaces
       write(*,101) crop .......................  ,p(str2%crop())
       ! calls M_strings procedure SUBSTITUTE()
       write(*,101) substitute("This","Here") ..  ,p(str2%substitute("This","Here"))
       ! calls M_strings procedure COMPACT()
       write(*,101) compact ....................  ,p(str2%compact())
       write(*,101) compact("") ................  ,p(str2%compact(""))
       write(*,101) compact(":") ...............  ,p(str2%compact(":"))
       ! calls M_strings procedure TRANSLITERATE()
       write(*,101) transliterate("aei","VWX") .  ,p(str2%transliterate("aei","VWX"))
       write(*,101) transliterate("aeiou"," ") .  ,p(str2%transliterate("aeiou"," "))
       write(*,101) transliterate("aeiou","") ..  ,p(str2%transliterate("aeiou",""))
       write(*,101) transliterate(" aeiou","") .  ,p(str2%transliterate(" aeiou",""))
       ! calls M_strings procedure SWITCH()
       write(*,404) chars .................... .  ,str2%chars()


       write(*,*)repeat( = ,78)
       str2%str= \t\tSome tabs\t   x\bX  
       write(*,101) str2%str ...................  ,str2%str
       write(*,101) expand .....................  ,p(str2%expand())
       str2=str2%expand()
       ! calls M_strings procedure NOTABS()
       write(*,101) notabs .....................  ,p(str2%notabs())
       ! calls M_strings procedure NOESC()
       write(*,101) noesc ......................  ,p(str2%noesc())


       write(*,*)repeat( = ,78)
       write(*,*) Casting to numeric variables 
       str3=string(    12.345678901234567e1         )
       write(*,101) str3%str ...................  ,str3%str
       ! calls M_strings procedure STRING_TO_VALUE()
       write(*,*) int  .......................  , str3%int()
       ! calls M_strings procedure STRING_TO_VALUE()
       write(*,*) real .......................  , str3%real()
       ! calls M_strings procedure STRING_TO_VALUE()
       write(*,*) dble .......................  , str3%dble()


       write(*,*)repeat( = ,78)
       write(*,*) Matching simple globbing patterns 
       str3=string(    12.345678901234567e1         )
       str3=string( Four score and seven years ago )
       write(*,101) str3%str ...................  ,str3%str
       ! calls M_strings procedure MATCHW
       write(*,*) match("Fo*") ...............  , str3%match("Fo*")
       ! calls M_strings procedure MATCHW
       write(*,*) match("and") ...............  , str3%match("and")
       ! calls M_strings procedure MATCHW
       write(*,*) match("*and*") .............  , str3%match("*and*")


       101 format(1x,a,"[",a,"]")
       202 format(1x,a,i0)
       303 format(1x,*(l3))
       404 format(1x,a,*("[",a1,"]":))


       write(*,*)repeat( = ,78)
       write(*,*) OVERLOADED OPERATORS (add and subtract,return TYPE(STRING)) 
       str1%str= 123.456 
       str2%str= AaBbCcDdEeFfGgHhIiJj AaBbCcDdEeFfGgHhIiJj 
       write(*,101) str1%str ...................  ,str1%str
       write(*,101) str2%str ...................  ,str2%str
       write(*,*) str1 + str2 ................  ,p(str1 + str2)
       ! a string that looks like a numeric value can have a value added
       write(*,*) str1 + 20000 ...............  ,p(str1 +20000)
       write(*,*) str1 - 20.0 ................  ,p(str1 -20.0)
       write(*,*) str2 - "Aa" (removes ALL) ..  ,p(str2 -  Aa )


       write(*,*)repeat( = ,78)
       write(*,*) OVERLOADED OPERATORS (multiply,return TYPE(STRING)) 
       str1%str= AaBbCcDdEeFfGgHhIiJj 
       write(*,101) str1%str ...................  ,str1%str
       write(*,*) str1 * 3 ...................  ,p(str1 * 3)


       write(*,*)repeat( = ,78)
       write(*,*) OVERLOADED OPERATORS (//,return TYPE(STRING)) 
       str1%str= String one: 
       str2%str= String two: 
       write(*,101) str1%str ...................  ,str1%str
       write(*,101) str2%str ...................  ,str2%str
       write(*,*) str1 // str2 ................  ,p(str1 // str2)
       ! numeric values are converted to strings
       write(*,*) str1 // 20000 ...............  ,p(str1 // 20000)
       write(*,*) str1 // 20.0 ................  ,p(str1 // 20.0)


       write(*,*)repeat( = ,78)
       write(*,*) OVERLOADED OPERATORS (logical comparisons,return logical) 
       ! NOTE: comparisons are performed on the character variable members
       !       of the type(string)
       str1%str= abcdefghij 
       str2%str= klmnopqrst 
       write(*,101) str1%str ...................  ,str1%str
       write(*,101) str2%str ...................  ,str2%str
       write(*,*) : EQ LT GT LE GE NE 
       write(*,*) compare str1 to str1 
       write(*,303)str1.eq.str1  ,str1.lt.str1  ,str1.gt.str1  ,str1.le.str1 &
                  & ,str1.ge.str1  ,str1.ne.str1
       write(*,*) compare str1 to str2 
       write(*,303)str1.eq.str2  ,str1.lt.str2  ,str1.gt.str2  ,str1.le.str2 &
                  & ,str1.ge.str2  ,str1.ne.str2
       write(*,*) compare str2 to str1 
       write(*,303)str2.eq.str1  ,str2.lt.str1  ,str2.gt.str1  ,str2.le.str1 &
                  & ,str2.ge.str1  ,str2.ne.str1


       write(*,*)repeat( = ,78)


     end program object_oriented




     Expected output


      exercise the M_STRING_OOP module interface
      =============================================================================
      Call methods of type(STRING)
      =============================================================================
      str2%str is ................ [   This  is  a  String!             ]
      len ........................ 36
      len_trim ................... 23
      index("is")................. 6
      index("is",back=.T.) ....... 10
      upper ...................... [   THIS  IS  A  STRING!             ]
      lower ...................... [   this  is  a  string!             ]
      reverse .................... [             !gnirtS  a  si  sihT   ]
      adjustl .................... [This  is  a  String!                ]
      adjustr .................... [                This  is  a  String!]
      adjustc .................... [        This  is  a  String!        ]
      adjustc(49) ................ [              This  is  a  String!               ]
      lenset(49) ................. [   This  is  a  String!                          ]
      trim ....................... [   This  is  a  String!]
      crop ....................... [This  is  a  String!]
      substitute("This","Here") .. [   Here  is  a  String!             ]
      compact .................... [This is a String!]
      compact("") ................ [ThisisaString!]
      compact(":") ............... [This:is:a:String!]
      transliterate("aei","VWX") . [   ThXs  Xs  V  StrXng!             ]
      transliterate("aeiou"," ") . [   Th s   s     Str ng!             ]
      transliterate("aeiou","") .. [   Ths  s    Strng!                 ]
      transliterate(" aeiou","") . [ThssStrng!                          ]
      chars .................... . [ ][ ][ ][T][h][i][s][ ][ ][i][s][ ][ ][a][ ][ ][S][t][r][i][n][g][!][ ][ ][ ][ ][ ][ ][ ]
      =============================================================================
      str2%str ................... [\t\tSome tabs\t   x\bX ]
      expand ..................... [         Some tabs          x X]
      notabs ..................... [                Some tabs          x X]
      noesc ...................... [  Some tabs    x X]
      =============================================================================
      Casting to numeric variables
      str3%str ................... [   12.345678901234567e1        ]
      int  .......................          123
      real .......................    123.456787
      dble .......................    123.45678901234567
      =============================================================================
      Matching simple globbing patterns
      str3%str ................... [Four score and seven years ago]
      match("Fo*") ...............  T
      match("and") ...............  F
      match("*and*") .............  T
      ==============================================================================
      OVERLOADED OPERATORS (add and subtract, return TYPE(STRING))
      str1%str ................... [123.456]
      str2%str ................... [AaBbCcDdEeFfGgHhIiJj AaBbCcDdEeFfGgHhIiJj]
      str1 + str2 ................ 123.456 AaBbCcDdEeFfGgHhIiJj AaBbCcDdEeFfGgHhIiJj
      str1 + 20000 ............... 20123.455999999998
      str1 - 20.0 ................ -103.456
      str2 - "Aa" (removes ALL) .. BbCcDdEeFfGgHhIiJj BbCcDdEeFfGgHhIiJj
      =============================================================================
      OVERLOADED OPERATORS (multiply, return TYPE(STRING))
      str1%str ................... [AaBbCcDdEeFfGgHhIiJj]
      str1 * 3 ................... AaBbCcDdEeFfGgHhIiJjAaBbCcDdEeFfGgHhIiJjAaBbCcDdEeFfGgHhIiJj
      =============================================================================
      OVERLOADED OPERATORS (//, return TYPE(STRING))
      str1%str ................... [String one:]
      str2%str ................... [String two:]
      str1 // str2 ................ String one:String two:
      str1 // 20000 ............... String one:20000
      str1 // 20.0 ................ String one:20.0
      =============================================================================
      OVERLOADED OPERATORS (logical comparisons, return logical)
      str1%str ................... [abcdefghij]
      str2%str ................... [klmnopqrst]
      : EQ LT GT LE GE NE
      compare str1 to str1
      :  T  F  F  T  T  F
      compare str1 to str2
      :  F  T  F  T  F  T
      compare str2 to str1
      :  F  F  T  F  T  T
      =============================================================================

-----------------------------------------------------------------------------------------------------------------------------------

                                                           M_strings (3)                                              July 02, 2017

Generated by manServer 1.08 from a5701843-00f4-467d-9853-29c28d2a8921 using man macros.
                                                            [M_strings]
