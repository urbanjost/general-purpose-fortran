










!>
!!##NAME
!!    M_strings(3f) - [M_strings::INTRO] Fortran string module
!!
!!##DESCRIPTION
!!    The M_strings(3fm) module is a collection of Fortran procedures
!!    that supplement the built-in intrinsic string routines. Routines
!!    for parsing, tokenizing, changing case, substituting new strings for
!!    substrings, locating strings with simple wildcard expressions, removing
!!    tabs and line terminators and other string manipulations are included.
!!
!!    M_strings_oop(3fm) is a companion module that provides an OOP interface
!!    to the M_strings module.
!!
!!##SYNOPSIS
!!
!!  public entities:
!!
!!      use M_strings, only : split,sep,delim,chomp,strtok
!!      use M_strings, only : substitute,change,modif,transliterate,reverse
!!      use M_strings, only : replace,join
!!      use M_strings, only : upper,lower,upper_quoted
!!      use M_strings, only : rotate13
!!      use M_strings, only : adjustc,compact,nospace,indent
!!      use M_strings, only : crop,clip,unquote,quote
!!      use M_strings, only : len_white,atleast,stretch,lenset,merge_str
!!      use M_strings, only : switch,s2c,c2s
!!      use M_strings, only : noesc,notabs,dilate,expand,visible
!!      use M_strings, only : longest_common_substring
!!      !x!use M_strings, only : uc
!!      use M_strings, only : string_to_value,string_to_values,s2v,s2vs
!!      use M_strings, only : value_to_string,v2s,msg
!!      use M_strings, only : listout,getvals
!!      use M_strings, only : glob, ends_with
!!      use M_strings, only : paragraph
!!      use M_strings, only : base, decodebase, codebase, base2
!!      use M_strings, only : isalnum, isalpha, iscntrl, isdigit
!!      use M_strings, only : isgraph, islower, isprint, ispunct
!!      use M_strings, only : isspace, isupper, isascii, isblank, isxdigit
!!      use M_strings, only : isnumber
!!      use M_strings, only : fortran_name
!!      use M_strings, only : describe
!!      use M_strings, only : edit_distance
!!      use M_strings, only : cc
!!
!!   TOKENS
!!
!!       split  subroutine parses string using specified delimiter characters
!!              and stores tokens into an array
!!       sep    function interface to split(3f)
!!       delim  subroutine parses string using specified delimiter characters
!!              and store tokens into an array
!!       chomp  function consumes input line as it returns next token in a
!!              string using specified delimiters
!!       paragraph    convert a string into a paragraph
!!       strtok tokenize a string like C strtok(3c) routine
!!
!!   EDITING
!!
!!       substitute     subroutine non-recursively globally replaces old
!!                      substring with new substring
!!       replace        function non-recursively globally replaces old
!!                      substring with new substring using allocatable string
!!                      (version of substitute(3f) without limitation on
!!                      length of output string)
!!       change         subroutine non-recursively globally replaces old
!!                      substring with new substring with a directive like
!!                      line editor
!!       modif          subroutine modifies a string with a directive like the
!!                      XEDIT line editor MODIFY command
!!       transliterate  replace characters found in set one with characters
!!                      from set two
!!       reverse        reverse character order in a string
!!       join           join an array of CHARACTER variables with specified
!!                      separator
!!       rotate13       apply trivial encryption algorithm ROT13 to a string
!!       squeeze        delete adjacent duplicate characters from a string
!!
!!   CASE
!!
!!       upper          function converts string to uppercase
!!       lower          function converts string to miniscule
!!       upper_quoted   function converts string to uppercase skipping strings
!!                      quoted per Fortran rules
!!
!!   WHITE SPACE
!!
!!       adjustc  elemental function centers text within the length of the
!!                input string
!!       compact  left justify string and replace duplicate whitespace with
!!                single characters or nothing
!!       nospace  function replaces whitespace with nothing
!!       indent   find number of leading spaces
!!       crop     function trims leading and trailing spaces and control
!!                characters
!!       clip     function trims leading and trailing spaces
!!
!!       See Also: squeeze
!!
!!   QUOTES
!!
!!       unquote  remove quotes from string as if read with list-directed input
!!       quote    add quotes to string as if written with list-directed input
!!
!!   STRING LENGTH
!!
!!       len_white  find location of last non-whitespace character
!!       lenset     return a string of specified length
!!       atleast    return a string of at least specified length
!!       stretch    return a string of at least specified length with suffix
!!       merge_str  make strings of equal length and then call MERGE(3f)
!!                  intrinsic
!!
!!   CHARACTER ARRAY VERSUS STRING
!!
!!       switch  switch between a string and an array of single characters
!!       s2c     convert string to array of single characters and add null
!!               terminator for passing to C
!!       c2s     convert null-terminated array of single characters to
!!               string for converting strings returned from C
!!
!!   NONALPHA
!!
!!       noesc    convert non-printable ASCII8 characters to a space
!!       notabs   convert tabs to spaces while maintaining columns,
!!                assuming tabs are set every 8 characters
!!       dilate   function to convert tabs to spaces assuming tabs are set
!!                every 8 characters
!!       expand   expand escape sequences in a string
!!       visible  expand escape sequences in a string to "control" and
!!                meta-control representations
!!
!!   NUMERIC STRINGS
!!
!!       string_to_value   generic subroutine returns numeric value (REAL,
!!                         DOUBLEPRECISION, INTEGER) from string
!!       string_to_values  subroutine reads an array of numbers from a string
!!       getvals           subroutine reads a relatively arbitrary number
!!                         of values from a string using list-directed read
!!       s2v               function returns DOUBLEPRECISION numeric value
!!                         from string
!!       s2vs              function returns a DOUBLEPRECISION array of numbers
!!                         from a string
!!       msg               append the values of up to nine values into a string
!!
!!       value_to_string   generic subroutine returns string given numeric value
!!                         (REAL, DOUBLEPRECISION, INTEGER, LOGICAL )
!!       v2s               generic function returns string from numeric value
!!                         (REAL, DOUBLEPRECISION, INTEGER )
!!       listout           expand a list of numbers where negative numbers
!!                         denote range ends (1 -10 means 1 thru 10)
!!       isnumber          determine if string represents a number
!!
!!   CHARACTER TESTS
!!
!!       glob        compares given string for match to pattern which may
!!                   contain wildcard characters
!!       ends_with   test whether strings ends with one of the specified suffixs
!!
!!       o isalnum   returns .true. if character is a letter or digit
!!       o isalpha   returns .true. if character is a letter and
!!                   .false. otherwise
!!       o iscntrl   returns .true. if character is a delete character or
!!                   ordinary control character
!!       o isdigit   returns .true. if character is a digit (0,1,...,9)
!!                   and .false. otherwise
!!       o isgraph   returns .true. if character is a printable character
!!                   except a space is considered non-printable
!!       o islower   returns .true. if character is a miniscule letter (a-z)
!!       o isprint   returns .true. if character is an ASCII printable
!!                   character
!!       o ispunct   returns .true. if character is a printable punctuation
!!                   character
!!       o isspace   returns .true. if character is a null, space, tab,
!!                   carriage return, new line, vertical tab, or formfeed
!!       o isupper   returns .true. if character is an uppercase letter (A-Z)
!!       o isascii   returns .true. if the character is in the range char(0)
!!                   to char(127)
!!       o isblank   returns .true. if character is a blank character
!!                   (space or horizontal tab.
!!       o isxdigit  returns .true. if character is a hexadecimal digit
!!                   (0-9, a-f, or A-F).
!!
!!       fortran_name   returns .true. if input string is a valid Fortran name
!!
!!   BASE CONVERSION
!!
!!       base       convert whole number string in base [2-36] to string
!!                  in alternate base [2-36]
!!       base2      convert INTEGER to a string representing a binary value
!!       codebase   convert whole number string in base [2-36] to base
!!                  10 number
!!       decodebase convert whole number in base 10 to string in base [2-36]
!!
!!   MISCELLANEOUS
!!
!!       cc         return up to twenty strings of arbitrary length as an array
!!       describe   returns a string describing the name of a single character
!!       edit_distance  returns a naive edit distance using the Levenshtein
!!                      distance algorithm
!!       longest_common_substring  function that returns the longest common
!!                                 substring of two strings.
!!
!!   INTRINSICS
!!
!!    The M_strings(3fm) module supplements and works in combination with
!!    the Fortran built-in intrinsics. Stand-alone Fortran lets you access
!!    the characters in a string using ranges much like they are character
!!    arrays, assignment, comparisons with standard operators, supports
!!    dynamically allocatable strings and supports concatenation using the //
!!    operator, as well as a number of intrinsic string routines:
!!
!!        adjustl             Left adjust a string
!!        adjustr             Right adjust a string
!!        index               Position of a substring within a string
!!        repeat              Repeated string concatenation
!!        scan                Scan a string for the presence of a set
!!                            of characters
!!        trim                Remove trailing blank characters of a string
!!        verify              Scan a string for the absence of a set of
!!                            characters
!!        len                 It returns the length of a character string
!!        achar               converts an integer into a character
!!        iachar              converts a character into an integer
!!        len_trim            finds length of string with trailing spaces
!!                            ignored
!!        new_line            Newline character
!!        selected_char_kind  Choose character kind
!!        lge                 Lexical greater than or equal
!!        lgt                 Lexical greater than
!!        lle                 Lexical less than or equal
!!        llt                 Lexical less than
!!
!!   OOPS INTERFACE
!!
!!    The M_strings_oop(3fm) module (included with the M_strings(3fm)
!!    module) provides an OOP (Object-Oriented Programming) interface to
!!    the M_strings(3fm) module.
!!
!!##SEE ALSO
!!    There are additional routines in other GPF modules for working with
!!    expressions (M_calculator), time strings (M_time), random strings
!!    (M_random, M_uuid), lists (M_list), and interfacing with the C regular
!!    expression library (M_regex).
!!
!!##EXAMPLES
!!
!!    Each of the procedural functions includes an example program in the
!!    corresponding man(1) page for the function. The object-oriented
!!    interface does not have individual man(1) pages, but is instead
!!    demonstrated using the following example program:
!!
!!     program demo_M_strings
!!     use M_strings, only : split, delim, chomp, sep
!!     use M_strings, only : substitute, change, modif
!!     use M_strings, only : transliterate, reverse
!!     use M_strings, only : replace, join
!!     use M_strings, only : upper, lower, upper_quoted
!!     use M_strings, only : rotate13
!!     use M_strings, only : adjustc, compact, nospace, indent, crop, clip, squeeze
!!     use M_strings, only : unquote, quote
!!     use M_strings, only : len_white, atleast, stretch, lenset, merge_str
!!     use M_strings, only : switch, s2c, c2s
!!     use M_strings, only : noesc, notabs, dilate, expand, visible
!!     use M_strings, only : longest_common_substring
!!     !x!use M_strings, only : uc
!!     use M_strings, only : string_to_value, string_to_values, s2v, s2vs
!!     use M_strings, only : value_to_string, v2s, msg
!!     use M_strings, only : listout, getvals
!!     use M_strings, only : glob, ends_with
!!     use M_strings, only : paragraph
!!     use M_strings, only : base, decodebase, codebase, base2
!!     use M_strings, only : isalnum, isalpha, iscntrl, isdigit, isgraph
!!     use M_strings, only : islower, isprint, ispunct, isspace, isupper
!!     use M_strings, only : isascii, isblank, isxdigit
!!     use M_strings, only : fortran_name
!!     end program demo_M_strings
!!
!!   Expected output
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
MODULE M_strings !
use, intrinsic :: iso_fortran_env, only : ERROR_UNIT        ! access computing environment
use, intrinsic :: iso_fortran_env, only : output_unit, stderr=>error_unit
use M_journal,       only : journal
implicit none    ! change default for every procedure contained in the module

! ident_1="@(#) M_strings(3f) Fortran module containing routines that deal with character strings"

!-----------------------------------------------------------------------------------------------------------------------------------
private

!----------------------# TOKENS
public split           !  subroutine parses a string using specified delimiter characters and store tokens into an allocatable array
public sep             !  function interface to split
public chomp           !  function consumes input line as it returns next token in a string using specified delimiters
public delim           !  subroutine parses a string using specified delimiter characters and store tokens into an array
public strtok          !  gets next token. Used by change(3f)
public paragraph       !  convert a long string into a paragraph
!----------------------# EDITING
public substitute      !  subroutine non-recursively globally replaces old substring with new substring in string
public replace         !  function non-recursively globally replaces old substring with new substring in string
public change          !  replaces old substring with new substring in string with a directive like a line editor
public modif           !  change string using a directive using rules similar to XEDIT line editor MODIFY command
public transliterate   !  when characters in set one are found replace them with characters from set two
public reverse         !  elemental function reverses character order in a string
public join            !  append an array of character variables with specified separator into a single CHARACTER variable
public squeeze         !  delete adjacent duplicate characters from a string
public rotate13        !  apply trivial encryption algorithm ROT13 to string
!----------------------# CHARACTER ARRAY VERSUS STRING
public switch          !  generic switch between a string and an array of single characters (a2s,s2a)
private a2s            !  function to copy char array to string
private s2a            !  function to copy string(1:Clen(string)) to char array
public s2c             !  convert character variable to array of character(len=1) with null terminator for C compatibility
public c2s             !  convert null-terminated array of character(len=1) to string for strings returned by C
!----------------------# CASE
public upper           !  elemental function converts string to uppercase
public lower           !  elemental function converts string to miniscule
public upper_quoted    !  elemental function converts string to miniscule skipping strings quoted per Fortran syntax rules
!----------------------# WHITE SPACE
public adjustc         !  elemental function centers string within the length of the input string
public compact         !  left justify string and replace duplicate whitespace with single characters or nothing
public nospace         !  function replaces whitespace with nothing
public indent          !  count number of leading spaces
public crop            !  function trims leading and trailing spaces and control characters
public clip            !  function trims leading and trailing spaces
!----------------------# QUOTES
public unquote         !  remove quotes from string as if read with list-directed input
public quote           !  add quotes to string as if written with list-directed input
!----------------------# STRING LENGTH
public lenset          !  return a string as specified length
public atleast         !  return a string of at least specified length
public stretch         !  return a string of at least specified length with suffix
public merge_str       !  make strings of equal length and then call MERGE(3f) intrinsic
public len_white       !  find location of last non-whitespace character
!----------------------# NONALPHA
public noesc           !  elemental function converts non-printable ASCII8 characters to a space
public notabs          !  convert tabs to spaces in output while maintaining columns, assuming a tab is set every 8 characters
public dilate          !  convert tabs to spaces in output while maintaining columns, assuming a tab is set every 8 characters
public expand          !  expand escape sequences in a string
public visible         !  expand escape sequences in a string to control and meta-control representations
!----------------------# NUMERIC STRINGS
public string_to_value !  generic subroutine returns REAL|DOUBLEPRECISION|INTEGER value from string (a2d,a2r,a2i)
 private a2d           !  subroutine returns double value from string
 private a2r           !  subroutine returns real value from string
 private a2i           !  subroutine returns integer value from string
public string_to_values!  subroutine returns values from a string
public getvals         !  subroutine returns values from a string
public s2v             !  function returns doubleprecision value from string
public s2vs            !  function returns a doubleprecision array of numbers from a string
                       !------------------------------------------------------------------------------------------------------------
public msg             !  function returns a string representing up to nine scalar intrinsic values
public value_to_string !  generic subroutine returns string given numeric REAL|DOUBLEPRECISION|INTEGER|LOGICAL value
public v2s             !  generic function returns string given numeric REAL|DOUBLEPRECISION|INTEGER|LOGICAL value
 private d2s           !  function returns string from doubleprecision value
 private r2s           !  function returns string from real value
 private i2s           !  function returns string from integer value
 private l2s           !  function returns string from logical value
public v2s_bug         !  generic function returns string given numeric REAL|DOUBLEPRECISION|INTEGER value
public isnumber        !  determine if string represents a number
 private trimzeros_    !  Delete trailing zeros from numeric decimal string
public listout         !  expand a list of numbers where  negative numbers denote range ends (1 -10 means 1 thru 10)
!-----------------------------------------------------------------------------------------------------------------------------------
!
! extend intrinsics to accept CHARACTER values
!
public int, real, dble

interface int;     module procedure int_s2v;           end interface
interface real;    module procedure real_s2v;          end interface
interface dble;    module procedure dble_s2v;          end interface

interface int;     module procedure ints_s2v;          end interface
interface real;    module procedure reals_s2v;         end interface
interface dble;    module procedure dbles_s2v;         end interface

!-----------------------------------------------------------------------------------------------------------------------------------
!----------------------# BIT ROUTINES
public setbits8        !  use a string representing a positive binary value to fill the bits of an INTEGER value
public setbits16       !  use a string representing a positive binary value to fill the bits of an INTEGER value
public setbits32       !  use a string representing a positive binary value to fill the bits of an INTEGER value
public setbits64       !  use a string representing a positive binary value to fill the bits of an INTEGER value
!----------------------# BASE CONVERSION
public base            !  convert whole number string in base [2-36] to string in alternate base [2-36]
public codebase        !  convert whole number string in base [2-36] to base 10 number
public decodebase      !  convert whole number in base 10 to string in base [2-36]
public base2           !  convert INTEGER to a string representing a binary value
!----------------------# LOGICAL TESTS
public glob            !  compares given string for match to pattern which may contain wildcard characters
public matchw          !  clone of glob -- for backward compatibiity
public ends_with       !  test whether strings ends with one of the specified suffix
public isalnum         !  elemental function returns .true. if CHR is a letter or digit
public isalpha         !  elemental function returns .true. if CHR is a letter and .false. otherwise
public isascii         !  elemental function returns .true. if the low order byte of c is in the range char(0) to char(127)
public isblank         !  elemental function returns .true. if CHR is a blank character (space or horizontal tab.
public iscntrl         !  elemental function returns .true. if CHR is a delete character or ordinary control character
public isdigit         !  elemental function returns .true. if CHR is a digit (0,1,...,9) and .false. otherwise
public isgraph         !  elemental function true if CHR is an ASCII printable character except considers a space non-printable
public islower         !  elemental function returns .true. if CHR is a miniscule letter (a-z)
public isprint         !  elemental function determines if CHR is an ASCII printable character
public ispunct         !  elemental function returns .true. if CHR is a printable punctuation character
public isspace         !  elemental function true if CHR is a null, space, tab, carriage return, new line, vertical tab, or formfeed
public isupper         !  elemental function returns .true. if CHR is an uppercase letter (A-Z)
public isxdigit        !  elemental function returns .true. if CHR is a hexadecimal digit (0-9, a-f, or A-F).
!----------------------#
!-------------------------------#
public fortran_name             !  elemental function returns .true. if LINE is a valid Fortran name
public describe                 !  returns a string describing character
public edit_distance            !  returns a naive edit distance using the Levenshtein distance algorithm
public cc                       !  return up to twenty strings of arbitrary length as an array
public longest_common_substring !  function that returns the longest common substring of two strings.
!-------------------------------#

!-----------------------------------------------------------------------------------------------------------------------------------

! ident_2="@(#) M_strings switch(3f) toggle between string and array of characters; generic{a2s s2a}"

interface switch
   module procedure a2s, s2a
end interface switch
! note how returned result is "created" by the function
!-----------------------------------------------------------------------------------------------------------------------------------

! ident_3="@(#) M_strings string_to_value(3f) Generic subroutine converts numeric string to a number (a2d a2r a2i)"

interface string_to_value
   module procedure a2d, a2r, a2i
end interface
!-----------------------------------------------------------------------------------------------------------------------------------

! ident_4="@(#) M_strings v2s(3f) Generic function returns string given REAL|INTEGER|DOUBLEPRECISION value(d2s r2s i2s)"

interface v2s
   module procedure d2s, r2s, i2s, l2s
end interface
!-----------------------------------------------------------------------------------------------------------------------------------
!-!interface setbits ! boz
!-!        module procedure setbits8, setbits16, setbits32, setbits64
!-!end interface
!-----------------------------------------------------------------------------------------------------------------------------------

! ident_5="@(#) M_strings msg(3f) convert up to nine scalar values to a string. Alternatively can also handle one-dimensional arrays"

interface msg
   module procedure msg_scalar, msg_one
end interface msg
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
interface ends_with
    procedure :: ends_with_str
    procedure :: ends_with_any
end interface ends_with
!-----------------------------------------------------------------------------------------------------------------------------------
public :: split2020, string_tokens

interface split2020
   module procedure :: split_tokens, split_first_last, split_pos
end interface split2020
!-----------------------------------------------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------------------------------------------------
! for compatibility allow old name for renamed procedures
interface matchw; module procedure glob ;  end interface
!-----------------------------------------------------------------------------------------------------------------------------------
CONTAINS
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    glob(3f) - [M_strings:COMPARE] compare given string for match to
!!    a pattern which may contain globbing wildcard characters
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    logical function glob(string, pattern )
!!
!!     character(len=*),intent(in) :: string
!!     character(len=*),intent(in) :: pattern
!!
!!##DESCRIPTION
!!    glob(3f) compares given (entire) STRING for a match to PATTERN which may
!!    contain basic wildcard "globbing" characters.
!!
!!    In this version to get a match the entire string must be described
!!    by PATTERN. Trailing whitespace is significant, so trim the input
!!    string to have trailing whitespace ignored.
!!
!!    Patterns like "b*ba" fail on a string like "babababa" because the
!!    algorithm finds an early match. To skip over the early matches insert
!!    an extra character at the end of the string and pattern that does
!!    not occur in the pattern. Typically a NULL is used (char(0)).
!!
!!##OPTIONS
!!    string   the input string to test to see if it contains the pattern.
!!    pattern  the following simple globbing options are available
!!
!!             o "?" matching any one character
!!             o "*" matching zero or more characters.
!!               Do NOT use adjacent asterisks.
!!             o spaces are significant and must be matched or pretrimmed
!!             o There is no escape character, so matching strings with
!!               literal question mark and asterisk is problematic.
!!
!!##EXAMPLES
!!
!!   Example program
!!
!!    program demo_glob
!!    implicit none
!!    ! This main() routine passes a bunch of test strings
!!    ! into the above code.  In performance comparison mode,
!!    ! it does that over and over. Otherwise, it does it just
!!    ! once. Either way, it outputs a passed/failed result.
!!    !
!!    integer :: nReps
!!    logical :: allpassed
!!    integer :: i
!!     allpassed = .true.
!!
!!     nReps = 10000
!!     ! Can choose as many repetitions as you're expecting
!!     ! in the real world.
!!     nReps = 1
!!
!!     do i=1,nReps
!!         ! Cases with repeating character sequences.
!!         allpassed=  test("a*abab",       "a*b",    .true.)   .and.  allpassed
!!         allpassed=  test("ab",           "*?",     .true.)   .and.  allpassed
!!         allpassed=  test("abc",          "*?",     .true.)   .and.  allpassed
!!         allpassed=  test("abcccd",       "*ccd",   .true.)   .and.  allpassed
!!         allpassed=  test("bLah",         "bLaH",   .false.)  .and.  allpassed
!!         allpassed=  test("mississippi",  "*sip*",  .true.)   .and.  allpassed
!!         allpassed= &
!!          & test("xxxx*zzzzzzzzy*f", "xxx*zzy*f", .true.) .and. allpassed
!!         allpassed= &
!!          & test("xxxx*zzzzzzzzy*f", "xxxx*zzy*fffff", .false.) .and. allpassed
!!         allpassed= &
!!          & test("mississipissippi", "*issip*ss*", .true.) .and. allpassed
!!         allpassed= &
!!          & test("xxxxzzzzzzzzyf", "xxxx*zzy*fffff", .false.) .and. allpassed
!!         allpassed= &
!!          & test("xxxxzzzzzzzzyf", "xxxx*zzy*f", .true.) .and. allpassed
!!         allpassed=  test("xyxyxyzyxyz",  "xy*z*xyz",  .true.)   .and.  allpassed
!!         allpassed=  test("xyxyxyxyz",    "xy*xyz",    .true.)   .and.  allpassed
!!         allpassed=  test("mississippi",  "mi*sip*",   .true.)   .and.  allpassed
!!         allpassed=  test("ababac",       "*abac*",    .true.)   .and.  allpassed
!!         allpassed=  test("aaazz",        "a*zz*",     .true.)   .and.  allpassed
!!         allpassed=  test("a12b12",       "*12*23",    .false.)  .and.  allpassed
!!         allpassed=  test("a12b12",       "a12b",      .false.)  .and.  allpassed
!!         allpassed=  test("a12b12",       "*12*12*",   .true.)   .and.  allpassed
!!
!!         ! Additional cases where the '*' char appears in the tame string.
!!         allpassed=  test("*",     "*",      .true.)   .and.  allpassed
!!         allpassed=  test("a*r",   "a*",     .true.)   .and.  allpassed
!!         allpassed=  test("a*ar",  "a*aar",  .false.)  .and.  allpassed
!!
!!         ! More double wildcard scenarios.
!!         allpassed=  test("XYXYXYZYXYz",  "XY*Z*XYz",   .true.)   .and.  allpassed
!!         allpassed=  test("missisSIPpi",  "*SIP*",      .true.)   .and.  allpassed
!!         allpassed=  test("mississipPI",  "*issip*PI",  .true.)   .and.  allpassed
!!         allpassed=  test("xyxyxyxyz",    "xy*xyz",     .true.)   .and.  allpassed
!!         allpassed=  test("miSsissippi",  "mi*sip*",    .true.)   .and.  allpassed
!!         allpassed=  test("miSsissippi",  "mi*Sip*",    .false.)  .and.  allpassed
!!         allpassed=  test("abAbac",       "*Abac*",     .true.)   .and.  allpassed
!!         allpassed=  test("aAazz",        "a*zz*",      .true.)   .and.  allpassed
!!         allpassed=  test("A12b12",       "*12*23",     .false.)  .and.  allpassed
!!         allpassed=  test("a12B12",       "*12*12*",    .true.)   .and.  allpassed
!!         allpassed=  test("oWn",          "*oWn*",      .true.)   .and.  allpassed
!!
!!         ! Completely tame (no wildcards) cases.
!!         allpassed= test("bLah", "bLah", .true.) .and. allpassed
!!
!!         ! Simple mixed wildcard tests suggested by IBMer Marlin Deckert.
!!         allpassed= test("a", "*?", .true.) .and. allpassed
!!
!!         ! More mixed wildcard tests including coverage for false positives.
!!         allpassed=  test("a",      "??",         .false.)  .and.  allpassed
!!         allpassed=  test("ab",     "?*?",        .true.)   .and.  allpassed
!!         allpassed=  test("ab",     "*?*?*",      .true.)   .and.  allpassed
!!         allpassed=  test("abc",    "?**?*?",     .true.)   .and.  allpassed
!!         allpassed=  test("abc",    "?**?*&?",    .false.)  .and.  allpassed
!!         allpassed=  test("abcd",   "?b*??",      .true.)   .and.  allpassed
!!         allpassed=  test("abcd",   "?a*??",      .false.)  .and.  allpassed
!!         allpassed=  test("abcd",   "?**?c?",     .true.)   .and.  allpassed
!!         allpassed=  test("abcd",   "?**?d?",     .false.)  .and.  allpassed
!!         allpassed=  test("abcde",  "?*b*?*d*?",  .true.)   .and.  allpassed
!!
!!         ! Single-character-match cases.
!!         allpassed=  test("bLah",   "bL?h",  .true.)   .and.  allpassed
!!         allpassed=  test("bLaaa",  "bLa?",  .false.)  .and.  allpassed
!!         allpassed=  test("bLah",   "bLa?",  .true.)   .and.  allpassed
!!         allpassed=  test("bLaH",   "?Lah",  .false.)  .and.  allpassed
!!         allpassed=  test("bLaH",   "?LaH",  .true.)   .and.  allpassed
!!
!!         allpassed=  test('abcdefghijk'  ,  '?b*',      .true.)  .and.  allpassed
!!         allpassed=  test('abcdefghijk'  ,  '*c*',      .true.)  .and.  allpassed
!!         allpassed=  test('abcdefghijk'  ,  '*c',       .false.) .and.  allpassed
!!         allpassed=  test('abcdefghijk'  ,  '*c*k',     .true.)  .and.  allpassed
!!         allpassed=  test('LS'           ,  '?OW',      .false.) .and.  allpassed
!!         allpassed=  test('teztit'       ,  'tez*t*t',  .true.)  .and.  allpassed
!!           ! Two pattern match problems that might pose difficulties
!!         allpassed=  test('e '           , '*e* ',         .true.)  .and.  allpassed
!!         allpassed=  test('abcde       ' , '*e      *',    .true.)  .and.  allpassed
!!         allpassed=  test('bababa'       , 'b*ba',         .true.)  .and.  allpassed
!!         allpassed=  test('baaaaax'      , 'b*ax',         .true.)  .and.  allpassed
!!         allpassed=  test('baaaaa'       , 'b*ax',         .false.) .and.  allpassed
!!         allpassed=  test('baaaaax'      , 'b*a',          .false.) .and.  allpassed
!!         allpassed=  test(''             , 'b*',           .false.) .and.  allpassed
!!         allpassed=  test(''             , '*',            .true.) .and.  allpassed
!!         allpassed=  test('b'            , '',             .false.) .and.  allpassed
!!         allpassed=  test('3'            , '??',           .false.) .and.  allpassed
!!         ! known flaws
!!         allpassed=  test(''             , '',             .true.) .and.  allpassed
!!         allpassed=  test('baaaaa'       , 'b*a',          .true.)  .and.  allpassed
!!         ! add unused character to work around
!!         allpassed=  test(''//char(0)       , ''//char(0),    .true.) .and.  allpassed
!!         allpassed=  test('baaaaa'//char(0) , 'b*a'//char(0), .true.)  .and.  allpassed
!!
!!         ! Many-wildcard scenarios.
!!         allpassed= test(&
!!         &"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa&
!!         &aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaab",&
!!         &"a*a*a*a*a*a*aa*aaa*a*a*b",&
!!         &.true.) .and. allpassed
!!         allpassed= test(&
!!         &"abababababababababababababababababababaacacacacacacac&
!!         &adaeafagahaiajakalaaaaaaaaaaaaaaaaaffafagaagggagaaaaaaaab",&
!!         &"*a*b*ba*ca*a*aa*aaa*fa*ga*b*",&
!!         &.true.) .and. allpassed
!!         allpassed= test(&
!!         &"abababababababababababababababababababaacacacacacaca&
!!         &cadaeafagahaiajakalaaaaaaaaaaaaaaaaaffafagaagggagaaaaaaaab",&
!!         &"*a*b*ba*ca*a*x*aaa*fa*ga*b*",&
!!         &.false.) .and. allpassed
!!         allpassed= test(&
!!         &"abababababababababababababababababababaacacacacacacacad&
!!         &aeafagahaiajakalaaaaaaaaaaaaaaaaaffafagaagggagaaaaaaaab",&
!!         &"*a*b*ba*ca*aaaa*fa*ga*gggg*b*",&
!!         &.false.) .and. allpassed
!!         allpassed= test(&
!!         &"abababababababababababababababababababaacacacacacacacad&
!!         &aeafagahaiajakalaaaaaaaaaaaaaaaaaffafagaagggagaaaaaaaab",&
!!         &"*a*b*ba*ca*aaaa*fa*ga*ggg*b*",&
!!         &.true.) .and. allpassed
!!         allpassed= test("aaabbaabbaab", "*aabbaa*a*", .true.) .and. allpassed
!!         allpassed= &
!!         test("a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*",&
!!         &"a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*", .true.) .and. allpassed
!!         allpassed= test("aaaaaaaaaaaaaaaaa",&
!!         &"*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*", .true.) .and. allpassed
!!         allpassed= test("aaaaaaaaaaaaaaaa",&
!!         &"*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*", .false.) .and. allpassed
!!         allpassed= test(&
!!         &"abc*abcd*abcde*abcdef*abcdefg*abcdefgh*abcdefghi*abcdefghij&
!!         &*abcdefghijk*abcdefghijkl*abcdefghijklm*abcdefghijklmn",&
!!         & "abc*abc*abc*abc*abc*abc*abc*abc*abc*abc*abc*abc*abc*abc&
!!         &*abc*abc*abc*",&
!!         &.false.) .and. allpassed
!!         allpassed= test(&
!!         &"abc*abcd*abcde*abcdef*abcdefg*abcdefgh*abcdefghi*abcdefghij&
!!         &*abcdefghijk*abcdefghijkl*abcdefghijklm*abcdefghijklmn",&
!!         &"abc*abc*abc*abc*abc*abc*abc*abc*abc*abc*abc*abc*",&
!!         &.true.) .and. allpassed
!!         allpassed= test("abc*abcd*abcd*abc*abcd",&
!!         &"abc*abc*abc*abc*abc", .false.) .and. allpassed
!!         allpassed= test( "abc*abcd*abcd*abc*abcd*abcd&
!!         &*abc*abcd*abc*abc*abcd", &
!!         &"abc*abc*abc*abc*abc*abc*abc*abc*abc*abc*abcd",&
!!         &.true.) .and. allpassed
!!         allpassed= test("abc",&
!!         &"********a********b********c********", .true.) .and. allpassed
!!         allpassed=&
!!         &test("********a********b********c********", "abc",.false.).and.allpassed
!!         allpassed= &
!!         &test("abc", "********a********b********b********",.false.).and.allpassed
!!         allpassed= test("*abc*", "***a*b*c***", .true.) .and. allpassed
!!
!!         ! A case-insensitive algorithm test.
!!         ! allpassed=test("mississippi", "*issip*PI", .true.) .and. allpassed
!!     enddo
!!
!!     if (allpassed)then
!!        write(*,'(*(g0,1x))')"Passed",nReps
!!     else
!!        write(*,'(a)')"Failed"
!!     endif
!!    contains
!!    ! This is a test program for wildcard matching routines.
!!    ! It can be used either to test a single routine for correctness,
!!    ! or to compare the timings of two (or more) different wildcard
!!    ! matching routines.
!!    !
!!    function test(tame, wild, bExpectedResult) result(bPassed)
!!    use M_strings, only : glob
!!       character(len=*) :: tame
!!       character(len=*) :: wild
!!       logical          :: bExpectedResult
!!       logical          :: bResult
!!       logical          :: bPassed
!!       bResult = .true.    ! We'll do "&=" cumulative checking.
!!       bPassed = .false.   ! Assume the worst.
!!       write(*,*)repeat('=',79)
!!       bResult = glob(tame, wild) ! Call a wildcard matching routine.
!!
!!       ! To assist correctness checking, output the two strings in any
!!       ! failing scenarios.
!!       if (bExpectedResult .eqv. bResult) then
!!          bPassed = .true.
!!          if(nReps == 1) write(*,*)"Passed match on ",tame," vs. ", wild
!!       else
!!          if(nReps == 1) write(*,*)"Failed match on ",tame," vs. ", wild
!!       endif
!!
!!    end function test
!!    end program demo_glob
!!
!!   Expected output
!!
!!##AUTHOR
!!   John S. Urban
!!
!!##REFERENCE
!!   The article "Matching Wildcards: An Empirical Way to Tame an Algorithm"
!!   in Dr Dobb's Journal, By Kirk J. Krauss, October 07, 2014
!!
!!##LICENSE
!!   Public Domain
function glob(tame,wild)

! ident_6="@(#) M_strings glob(3f) function compares text strings one of which can have wildcards ('*' or '?')."

logical                    :: glob
character(len=*)           :: tame       ! A string without wildcards
character(len=*)           :: wild       ! A (potentially) corresponding string with wildcards
character(len=len(tame)+1) :: tametext
character(len=len(wild)+1) :: wildtext
character(len=1),parameter :: NULL=char(0)
integer                    :: wlen
integer                    :: ti, wi
integer                    :: i
character(len=:),allocatable :: tbookmark, wbookmark
! These two values are set when we observe a wildcard character. They
! represent the locations, in the two strings, from which we start once we have observed it.
   tametext=tame//NULL
   wildtext=wild//NULL
   tbookmark = NULL
   wbookmark = NULL
   wlen=len(wild)
   wi=1
   ti=1
   do                                            ! Walk the text strings one character at a time.
      if(wildtext(wi:wi) == '*')then             ! How do you match a unique text string?
         do i=wi,wlen                            ! Easy: unique up on it!
            if(wildtext(wi:wi) == '*')then
               wi=wi+1
            else
               exit
            endif
         enddo
         if(wildtext(wi:wi) == NULL) then        ! "x" matches "*"
            glob=.true.
            return
         endif
         if(wildtext(wi:wi)  /=  '?') then
            ! Fast-forward to next possible match.
            do while (tametext(ti:ti)  /=  wildtext(wi:wi))
               ti=ti+1
               if (tametext(ti:ti) == NULL)then
                  glob=.false.
                  return                         ! "x" doesn't match "*y*"
               endif
            enddo
         endif
         wbookmark = wildtext(wi:)
         tbookmark = tametext(ti:)
      elseif(tametext(ti:ti)  /=  wildtext(wi:wi) .and. wildtext(wi:wi)  /=  '?') then
         ! Got a non-match. If we've set our bookmarks, back up to one or both of them and retry.
         if(wbookmark /= NULL) then
            if(wildtext(wi:) /=  wbookmark) then
               wildtext = wbookmark
               wlen=len_trim(wbookmark)
               wi=1
               ! Don't go this far back again.
               if (tametext(ti:ti)  /=  wildtext(wi:wi)) then
                  tbookmark=tbookmark(2:)
                  tametext = tbookmark
                  ti=1
                  cycle                          ! "xy" matches "*y"
               else
                  wi=wi+1
               endif
            endif
            if (tametext(ti:ti) /= NULL) then
               ti=ti+1
               cycle                             ! "mississippi" matches "*sip*"
            endif
         endif
         glob=.false.
         return                                  ! "xy" doesn't match "x"
      endif
      ti=ti+1
      wi=wi+1
      if (ti > len(tametext)) then
         glob=.false.
         return
      elseif (tametext(ti:ti) == NULL) then          ! How do you match a tame text string?
         if(wildtext(wi:wi) /= NULL)then
            do while (wildtext(wi:wi) == '*')    ! The tame way: unique up on it!
               wi=wi+1                           ! "x" matches "x*"
               if(wildtext(wi:wi) == NULL)exit
            enddo
         endif
         if (wildtext(wi:wi) == NULL)then
            glob=.true.
            return                               ! "x" matches "x"
         endif
         glob=.false.
         return                                  ! "x" doesn't match "xy"
      endif
   enddo
end function glob
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    ends_with(3f) - [M_strings:MATCH] test if string ends with specified
!!                    suffix(es)
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function ends_with(source_string,suffix)
!!
!!     or
!!
!!    function ends_with(source_string,[suffixs])
!!
!!     character(len=*),intent(in)          :: source_string
!!     character(len=*),intent(in)          :: suffix
!!     logical                              :: ends_with
!!
!!##DESCRIPTION
!!
!!##OPTIONS
!!     SOURCE_STRING  string to tokenize
!!     SUFFIX         list of separator characters. May be scalar or an array.
!!
!!##RETURNS
!!     ENDS_WITH      returns .TRUE. if one of the suffix match the end
!!                    of SOURCE_STRING.
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!    program demo_ends_with
!!    use M_strings, only : ends_with
!!    use, intrinsic :: iso_fortran_env, only : stdout=>output_unit
!!    implicit none
!!       write(stdout,*)ends_with('prog.a',['.o','.i','.s'])
!!       write(stdout,*)ends_with('prog.f90',['.F90','.f90'])
!!       write(stdout,*)ends_with('prog.pdf','.pdf')
!!       write(stdout,*)ends_with('prog.doc','.txt')
!!    end program demo_ends_with
!!
!!   Results:
!!
!!     F
!!     T
!!     T
!!     F
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
pure function ends_with_str(string, ending) result(matched)
character(*), intent(in) :: string, ending
integer                  :: n1, n2
logical                  :: matched
   n1 = len(string) - len(ending) + 1
   n2 = len(string)
   if (n1 < 1) then
       matched = .false.
   else
       matched = (string(n1:n2) == ending)
   endif
end function ends_with_str
!-----------------------------------------------------------------------------------------------------------------------------------
pure function ends_with_any(string, endings) result(matched)
character(*), intent(in) :: string
character(*), intent(in) :: endings(:)
logical                  :: matched
integer                  :: i
   matched = .true.
   FINDIT: block
   do i=1, size(endings)
       if(ends_with_str(string,trim(endings(i)))) exit FINDIT
   enddo
   matched = .false.
   endblock FINDIT
end function ends_with_any
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    sep(3f) - [M_strings:TOKENS] function to parse string into an array using
!!    specified delimiters
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function sep(input_line,delimiters,nulls)
!!
!!     character(len=*),intent(in)              :: input_line
!!     character(len=*),optional,intent(in)     :: delimiters
!!     character(len=*),optional,intent(in)     :: nulls
!!     character(len=:),allocatable             :: sep(:)
!!
!!##DESCRIPTION
!!     sep(3f) parses a string using specified delimiter characters and
!!     store tokens into an allocatable array
!!
!!##OPTIONS
!!    INPUT_LINE  Input string to tokenize
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
!!    NULLS=IGNORE|RETURN|IGNOREEND  Treatment of null fields.
!!                By default adjacent delimiters in the input string
!!                do not create an empty string in the output array. if
!!                NULLS='return' adjacent delimiters create an empty element
!!                in the output ARRAY. If NULLS='ignoreend' then only
!!                trailing delimiters at the right of the string are ignored.
!!    ORDER='ASCENDING'|'DESCENDING'  by default the tokens are returned from
!!                                    last to first; order='ASCENDING' returns
!!                                    them from first to last (left to right).
!!##RETURNS
!!    SEP       Output array of tokens
!!
!!##EXAMPLES
!!
!!  Sample program:
!!
!!    program demo_sep
!!    use M_strings, only: sep
!!    character(len=*),parameter :: fo='(/,a,*(/,"[",g0,"]":,","))'
!!    character(len=*),parameter :: line=&
!!    '  aBcdef   ghijklmnop qrstuvwxyz  1:|:2     333|333 a B cc    '
!!       write(*,'(a)') 'INPUT LINE:['//LINE//']'
!!       write(*,fo) 'typical call:',sep(line)
!!       write(*,fo) 'delimiters ":|":',sep(line,':|')
!!       write(*,fo) 'count null fields ":|":',sep(line,':|','return')
!!    end program demo_sep
!!
!!  Output
!!
!!    INPUT LINE:[  aBcdef   ghijklmnop qrstuvwxyz  1:|:2     333|333 a B cc    ]
!!
!!    typical call:
!!    [cc        ],
!!    [B         ],
!!    [a         ],
!!    [333|333   ],
!!    [1:|:2     ],
!!    [qrstuvwxyz],
!!    [ghijklmnop],
!!    [aBcdef    ]
!!
!!    delimiters ":|":
!!    [333 a B cc                         ],
!!    [2     333                          ],
!!    [  aBcdef   ghijklmnop qrstuvwxyz  1]
!!
!!    count null fields ":|":
!!    [333 a B cc                         ],
!!    [2     333                          ],
!!    [                                   ],
!!    [                                   ],
!!    [  aBcdef   ghijklmnop qrstuvwxyz  1]
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
function sep(input_line,delimiters,nulls,order)
!-----------------------------------------------------------------------------------------------------------------------------------

! ident_7="@(#) M_strings sep(3f) parse string on delimiter characters and store tokens into an allocatable array"

!  John S. Urban
!-----------------------------------------------------------------------------------------------------------------------------------
intrinsic index, min, present, len
!-----------------------------------------------------------------------------------------------------------------------------------
!  given a line of structure " par1 par2 par3 ... parn " store each par(n) into a separate variable in array.
!    o by default adjacent delimiters in the input string do not create an empty string in the output array
!    o no quoting of delimiters is supported
character(len=*),intent(in)              :: input_line  ! input string to tokenize
character(len=*),optional,intent(in)     :: delimiters  ! list of delimiter characters
character(len=*),optional,intent(in)     :: nulls       ! return strings composed of delimiters or not ignore|return|ignoreend
character(len=*),optional,intent(in)     :: order       ! return strings composed of delimiters or not ignore|return|ignoreend

character(len=:),allocatable             :: sep(:)      ! output array of tokens
integer                                  :: isize
   call split(input_line,sep,delimiters,'right',nulls)
   if(present(order))then
   select case(order)
   case('ascending','ASCENDING')
    isize=size(sep)
    if(isize > 1)then
       sep=sep(isize:1:-1)
    endif
   end select
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
end function sep
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    split(3f) - [M_strings:TOKENS] parse string into an array using
!!    specified delimiters
!!    (LICENSE:PD)
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
!!     store tokens into an allocatable array
!!
!!##OPTIONS
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
!!                (This can be accomplished with array syntax in modern
!!                Fortran, but was more useful pre-fortran90).
!!
!!    NULLS=IGNORE|RETURN|IGNOREEND  Treatment of null fields.
!!                By default adjacent delimiters in the input string
!!                do not create an empty string in the output array. if
!!                NULLS='return' adjacent delimiters create an empty element
!!                in the output ARRAY. If NULLS='ignoreend' then only
!!                trailing delimiters at the right of the string are ignored.
!!
!!##EXAMPLES
!!
!!  Sample program:
!!
!!    program demo_split
!!    use M_strings, only: split
!!    implicit none
!!    integer :: i
!!    character(len=*),parameter     :: line=&
!!    '  aBcdef   ghijklmnop qrstuvwxyz  1:|:2     333|333 a B cc    '
!!    character(len=:),allocatable :: array(:) ! output array of tokens
!!       write(*,*)'INPUT LINE:['//line//']'
!!       write(*,'(70("="))')
!!       write(*,*)'typical call:'
!!       call split(line,array)
!!       write(*,'(i0," ==> ",a)')(i,trim(array(i)),i=1,size(array))
!!       write(*,*)'SIZE:',size(array)
!!       write(*,'(70("-"))')
!!       write(*,*)'custom list of delimiters (colon and vertical line):'
!!       call split(line,array,delimiters=':|',&
!!       & order='sequential',nulls='ignore')
!!       write(*,'(i0," ==> ",a)')(i,trim(array(i)),i=1,size(array))
!!       write(*,*)'SIZE:',size(array)
!!       write(*,'(70("-"))')
!!       write(*,*) 'custom list of delimiters, &
!!       &reverse array order and count null fields:'
!!       call split(line,array,delimiters=':|',&
!!       &order='reverse',nulls='return')
!!       write(*,'(i0," ==> ",a)')(i,trim(array(i)),i=1,size(array))
!!       write(*,*)'SIZE:',size(array)
!!       write(*,'(70("-"))')
!!       write(*,*)'INPUT LINE:['//line//']'
!!       write(*,*) 'default delimiters and reverse array order &
!!       &and return null fields:'
!!       call split(line,array,delimiters='',order='reverse',nulls='return')
!!       write(*,'(i0," ==> ",a)')(i,trim(array(i)),i=1,size(array))
!!       write(*,*)'SIZE:',size(array)
!!    end program demo_split
!!
!!  Output
!!
!!   >INPUT LINE:[  aBcdef   ghijklmnop qrstuvwxyz  1:|:2     333|
!!   333 a B cc    ]
!!   >=================================================================
!!   > typical call:
!!   >1 ==> aBcdef
!!   >2 ==> ghijklmnop
!!   >3 ==> qrstuvwxyz
!!   >4 ==> 1:|:2
!!   >5 ==> 333|333
!!   >6 ==> a
!!   >7 ==> B
!!   >8 ==> cc
!!   > SIZE:           8
!!   >----------------------------------------------------------------
!!   > custom list of delimiters (colon and vertical line):
!!   >1 ==>   aBcdef   ghijklmnop qrstuvwxyz  1
!!   >2 ==> 2     333
!!   >3 ==> 333 a B cc
!!   > SIZE:           3
!!   >----------------------------------------------------------------
!!   > custom list of delimiters, reverse array order and
!!   return null fields:
!!   >1 ==> 333 a B cc
!!   >2 ==> 2     333
!!   >3 ==>
!!   >4 ==>
!!   >5 ==>   aBcdef   ghijklmnop qrstuvwxyz  1
!!   > SIZE:           5
!!   >----------------------------------------------------------------
!!   > INPUT LINE:[  aBcdef   ghijklmnop qrstuvwxyz  1:|:2     333|
!!   333 a B cc    ]
!!   > default delimiters and reverse array order and count null fields:
!!   >1 ==>
!!   >2 ==>
!!   >3 ==>
!!   >4 ==> cc
!!   >5 ==> B
!!   >6 ==> a
!!   >7 ==> 333|333
!!   >8 ==>
!!   >9 ==>
!!   >10 ==>
!!   >11 ==>
!!   >12 ==> 1:|:2
!!   >13 ==>
!!   >14 ==> qrstuvwxyz
!!   >15 ==> ghijklmnop
!!   >16 ==>
!!   >17 ==>
!!   >18 ==> aBcdef
!!   >19 ==>
!!   >20 ==>
!!   > SIZE:          20
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
subroutine split(input_line,array,delimiters,order,nulls)
!-----------------------------------------------------------------------------------------------------------------------------------

! ident_8="@(#) M_strings split(3f) parse string on delimiter characters and store tokens into an allocatable array"

!  John S. Urban
!-----------------------------------------------------------------------------------------------------------------------------------
intrinsic index, min, present, len
!-----------------------------------------------------------------------------------------------------------------------------------
!  given a line of structure " par1 par2 par3 ... parn " store each par(n) into a separate variable in array.
!    o by default adjacent delimiters in the input string do not create an empty string in the output array
!    o no quoting of delimiters is supported
character(len=*),intent(in)              :: input_line  ! input string to tokenize
character(len=*),optional,intent(in)     :: delimiters  ! list of delimiter characters
character(len=*),optional,intent(in)     :: order       ! order of output array sequential|[reverse|right]
character(len=*),optional,intent(in)     :: nulls       ! return strings composed of delimiters or not ignore|return|ignoreend
character(len=:),allocatable,intent(out) :: array(:)    ! output array of tokens
!-----------------------------------------------------------------------------------------------------------------------------------
integer                       :: n                      ! max number of strings INPUT_LINE could split into if all delimiter
integer,allocatable           :: ibegin(:)              ! positions in input string where tokens start
integer,allocatable           :: iterm(:)               ! positions in input string where tokens end
character(len=:),allocatable  :: dlim                   ! string containing delimiter characters
character(len=:),allocatable  :: ordr                   ! string containing order keyword
character(len=:),allocatable  :: nlls                   ! string containing nulls keyword
integer                       :: ii,iiii                ! loop parameters used to control print order
integer                       :: icount                 ! number of tokens found
integer                       :: lgth                   ! length of input string with trailing spaces trimmed
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
      if(delimiters /= '')then                                       ! if DELIMITERS was specified and not null use it
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
   if(allocated(ibegin))deallocate(ibegin)    !x! intel compiler says allocated already ?
   if(allocated(iterm))deallocate(iterm)      !x! intel compiler says allocated already ?
   allocate(ibegin(n))                        ! allocate enough space to hold starting location of tokens if string all tokens
   allocate(iterm(n))                         ! allocate enough space to hold ending location of tokens if string all tokens
   ibegin(:)=1
   iterm(:)=1
!-----------------------------------------------------------------------------------------------------------------------------------
   lgth=len(input_line)                                           ! lgth is the column position of the last non-blank character
   icount=0                                                       ! how many tokens found
   inotnull=0                                                     ! how many tokens found not composed of delimiters
   imax=0                                                         ! length of longest token found
!-----------------------------------------------------------------------------------------------------------------------------------
   if(lgth > 0)then                                              ! there is at least one non-delimiter in INPUT_LINE if get here
      icol=1                                                      ! initialize pointer into input line
      INFINITE: do i30=1,lgth,1                                   ! store into each array element
         ibegin(i30)=icol                                         ! assume start new token on the character
         if(index(dlim(1:idlim),input_line(icol:icol)) == 0)then  ! if current character is not a delimiter
            iterm(i30)=lgth                                       ! initially assume no more tokens
            do i10=1,idlim                                        ! search for next delimiter
               ifound=index(input_line(ibegin(i30):lgth),dlim(i10:i10))
               IF(ifound > 0)then
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
         if(icol > lgth)then                                     ! no text left
            exit INFINITE
         endif
      enddo INFINITE
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   select case (trim(adjustl(nlls)))
   case ('ignore','','ignoreend')
      ireturn=inotnull
   case default
      ireturn=icount
   end select
   allocate(character(len=imax) :: array(ireturn))                ! allocate the array to return
   !allocate(array(ireturn))                                       ! allocate the array to return
!-----------------------------------------------------------------------------------------------------------------------------------
   select case (trim(adjustl(ordr)))                              ! decide which order to store tokens
   case ('reverse','right') ; ii=ireturn ; iiii=-1                ! last to first
   case default             ; ii=1       ; iiii=1                 ! first to last
   end select
!-----------------------------------------------------------------------------------------------------------------------------------
   do i20=1,icount                                                ! fill the array with the tokens that were found
      if(iterm(i20) < ibegin(i20))then
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
!!    chomp(3f) - [M_strings:TOKENS] Tokenize a string, consuming it one
!!    token per call
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function chomp(source_string,token[,delimiters])
!!
!!     character(len=*)                     :: source_string
!!     character(len=:),intent(out)         :: token
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
!!  Sample program:
!!
!!    program demo_chomp
!!
!!    use M_strings, only : chomp
!!    implicit none
!!    character(len=100)            :: inline
!!    character(len=:),allocatable  :: token
!!    character(len=*),parameter    :: delimiters=' ;,'
!!    integer                       :: ios
!!    integer                       :: icount
!!    integer                       :: itoken
!!       icount=0
!!       do        ! read lines from stdin until end-of-file or error
!!          read (unit=*,fmt="(a)",iostat=ios) inline
!!          if(ios /= 0)stop
!!          icount=icount+1
!!          itoken=0
!!          write(*,*)'INLINE ',trim(inline)
!!          do while ( chomp(inline,token,delimiters) >=  0)
!!             itoken=itoken+1
!!             print *, itoken,'TOKEN=['//trim(token)//']'
!!          enddo
!!       enddo
!!
!!    end program demo_chomp
!!
!!   sample input file
!!
!!     this is a test of chomp; A:B :;,C;;
!!
!!   sample output file
!!
!!     > INLINE     this is a test of chomp; A:B :;,C;;
!!     >           1 TOKEN=[this]
!!     >           2 TOKEN=[is]
!!     >           3 TOKEN=[a]
!!     >           4 TOKEN=[test]
!!     >           5 TOKEN=[of]
!!     >           6 TOKEN=[chomp]
!!     >           7 TOKEN=[A:B]
!!     >           8 TOKEN=[:]
!!     >           9 TOKEN=[C]
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
FUNCTION chomp(source_string,token,delimiters)

! ident_9="@(#) M_strings chomp(3f) Tokenize a string JSU- 20151030"

character(len=*)                         :: source_string    ! string to tokenize
character(len=:),allocatable,intent(out) :: token            ! returned token
character(len=*),intent(in),optional     :: delimiters       ! list of separator characters
integer                                  :: chomp            ! returns copy of shifted source_string
character(len=:),allocatable             :: delimiters_local
integer                                  :: token_start      ! beginning of token found if function result is .true.
integer                                  :: token_end        ! end of token found if function result is .true.
integer                                  :: isource_len
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
   do while (token_start  <=  isource_len)       ! step thru each character to find next delimiter, if any
      if(index(delimiters_local,source_string(token_start:token_start))  /=  0) then
         token_start = token_start + 1
      else
         exit
      endif
   enddo
!-----------------------------------------------------------------------------------------------------------------------------------
   token_end=token_start
   do while (token_end  <=  isource_len-1)                         ! step thru each character to find next delimiter, if any
      if(index(delimiters_local,source_string(token_end+1:token_end+1))  /=  0) then  ! found a delimiter in next character
         exit
      endif
      token_end = token_end + 1
   enddo
   !write(*,*)'TOKEN_START ',token_start
   !write(*,*)'TOKEN_END   ',token_end
   chomp=isource_len-token_end
   if(chomp >= 0)then
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
!!      delim(3f) - [M_strings:TOKENS] parse a string and store tokens into
!!      an array
!!      (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine delim(line,array,n,icount,ibegin,iterm,lgth,dlim)
!!
!!     character(len=*),intent(in)  :: line
!!     integer,integer(in)          :: n
!!     integer,intent(out)          :: icount
!!     character(len=*)             :: array(n)
!!     integer,intent(out)          :: ibegin(n)
!!     integer,intent(out)          :: iterm(n)
!!     integer,intent(out)          :: lgth
!!     character(len=*)             :: dlim
!!
!!##DESCRIPTION
!!      Given a LINE of structure " par1 par2 par3 ... parn "
!!      store each par(n) into a separate variable in ARRAY (UNLESS
!!      ARRAY(1) == '#N#')
!!
!!      Also set ICOUNT to number of elements of array initialized, and
!!      return beginning and ending positions for each element in IBEGIN(N)
!!      and ITERM(N).
!!
!!      Return position of last non-blank character (even if more
!!      than N elements were found) in lgth
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
!!    LGTH      position of last non-blank character in input string LINE
!!    DLIM      delimiter characters
!!
!!##EXAMPLES
!!
!!  Sample program:
!!
!!     program demo_delim
!!
!!     use M_strings, only: delim
!!     implicit none
!!     character(len=80) :: line
!!     character(len=80) :: dlm
!!     integer,parameter :: n=10
!!     character(len=20) :: array(n)=' '
!!     integer           :: ibegin(n),iterm(n)
!!     integer           :: i20, icount, lgth, i10
!!     line=' first  second 10.3 words_of_stuff  '
!!     do i20=1,4
!!        ! change delimiter list and what is calculated or parsed
!!        if(i20 == 1)dlm=' '
!!        if(i20 == 2)dlm='o'
!!        if(i20 == 3)dlm=' aeiou'    ! NOTE SPACE IS FIRST
!!        if(i20 == 3)ARRAY(1)='#N#'  ! QUIT RETURNING STRING ARRAY
!!        if(i20 == 4)line='AAAaBBBBBBbIIIIIi  J K L'
!!
!!        ! write out a break line composed of =========== ..
!!        write(*,'(57("="))')
!!        ! show line being parsed
!!        write(*,'(a)')'PARSING=['//trim(line)//'] on '//trim(dlm)
!!        ! call parsing procedure
!!        call delim(line,array,n,icount,ibegin,iterm,lgth,dlm)
!!        write(*,*)'number of tokens found=',icount
!!        write(*,*)'last character in column ',lgth
!!        if(icount > 0)then
!!           if(lgth /= iterm(icount))then
!!              write(*,*)'ignored from column ',iterm(icount)+1,' to ',lgth
!!           endif
!!           do i10=1,icount
!!              ! check flag to see if ARRAY() was set
!!              if(array(1) /= '#N#')then
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
!!     end program demo_delim
!!
!!   Results:
!!
!!    =========================================================
!!    PARSING=[ first  second 10.3 words_of_stuff] on
!!     number of tokens found=           4
!!     last character in column           34
!!    [first][second][10.3][words_of_stuff]
!!    [first][second][10.3][words_of_stuff]
!!    =========================================================
!!    PARSING=[ first  second 10.3 words_of_stuff] on o
!!     number of tokens found=           4
!!     last character in column           34
!!    [ first  sec][nd 10.3 w][rds_][f_stuff]
!!    [ first  sec][nd 10.3 w][rds_][f_stuff]
!!    =========================================================
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
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
subroutine delim(line,array,n,icount,ibegin,iterm,lgth,dlim)

! ident_10="@(#) M_strings delim(3f) parse a string and store tokens into an array"

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
integer,intent(out)            :: lgth
character(len=*),intent(in)    :: dlim
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=len(line)):: line_local
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
      lgth=len_trim(line)
      line_local=line

      idlim=len(dlim)
      if(idlim > 5)then
         idlim=len_trim(dlim)      ! dlim a lot of blanks on some machines if dlim is a big string
         if(idlim == 0)then
            idlim=1     ! blank string
         endif
      endif

      if(lgth == 0)then                                        ! command was totally blank
         return
      endif
!
!     there is at least one non-blank character in the command
!     lgth is the column position of the last non-blank character
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
               iend=lgth-istart+1+1                            ! assume no delimiters so put past end of line
               do i10=1,idlim
                  ifound=index(line_local(istart:lgth),dlim(i10:i10))
                  if(ifound > 0)then
                     iend=min(iend,ifound)
                  endif
               enddo
               if(iend <= 0)then                               ! no remaining delimiters
                 iterm(iarray)=lgth
                 if(lstore)then
                    array(iarray)=line_local(istart:lgth)
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
         if(icol > lgth)then
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
!!    replace(3f) - [M_strings:EDITING] function replaces one
!!    substring for another in string
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!! syntax:
!!
!!      function replace(targetline,old,new,cmd,&
!!       & occurrence, &
!!       & repeat, &
!!       & ignorecase, &
!!       & ierr) result (newline)
!!      character(len=*)                       :: targetline
!!      character(len=*),intent(in),optional   :: old
!!      character(len=*),intent(in),optional   :: new
!!      character(len=*),intent(in),optional   :: cmd
!!      integer,intent(in),optional            :: occurrence
!!      integer,intent(in),optional            :: repeat
!!      logical,intent(in),optional            :: ignorecase
!!      integer,intent(out),optional           :: ierr
!!      character(len=:),allocatable           :: newline
!!
!!##DESCRIPTION
!!    Replace one substring for another in string.
!!    Either CMD or OLD and NEW must be specified.
!!
!!##OPTIONS
!!     targetline  input line to be changed
!!     old         old substring to replace
!!     new         new substring
!!     cmd         alternate way to specify old and new string, in
!!                 the form c/old/new/; where "/" can be any character
!!                 not in "old" or "new".
!!     occurrence  if present, start changing at the Nth occurrence of the
!!                 OLD string. If negative start replacing from the left
!!                 end of the string.
!!     repeat      number of replacements to perform. Defaults to a global
!!                 replacement.
!!     ignorecase  whether to ignore ASCII case or not. Defaults
!!                 to .false. .
!!##RETURNS
!!     newline     allocatable string returned
!!     ierr        error code. iF ier = -1 bad directive, >= 0 then
!!                 count of changes made.
!!
!!##EXAMPLES
!!
!!   Sample Program:
!!
!!    program demo_replace
!!    use M_strings, only : replace
!!    implicit none
!!    character(len=:),allocatable :: line
!!
!!    write(*,*)replace('Xis is Xe string','X','th')
!!    write(*,*)replace('Xis is xe string','x','th',ignorecase=.true.)
!!    write(*,*)replace('Xis is xe string','X','th',ignorecase=.false.)
!!
!!    ! a null old substring means "at beginning of line"
!!    write(*,*) replace('my line of text','','BEFORE:')
!!
!!    ! a null new string deletes occurrences of the old substring
!!    write(*,*) replace('I wonder i ii iii','i','')
!!
!!    ! Examples of the use of RANGE
!!
!!    line=replace('aaaaaaaaa','a','A',occurrence=1,repeat=1)
!!    write(*,*)'replace first a with A ['//line//']'
!!
!!    line=replace('aaaaaaaaa','a','A',occurrence=3,repeat=3)
!!    write(*,*)'replace a with A for 3rd to 5th occurrence ['//line//']'
!!
!!    line=replace('ababababa','a','',occurrence=3,repeat=3)
!!    write(*,*)'replace a with null instances 3 to 5 ['//line//']'
!!
!!    line=replace( &
!!     & 'a b ab baaa aaaa aa aa a a a aa aaaaaa',&
!!     & 'aa','CCCC',occurrence=-1,repeat=1)
!!    write(*,*)'replace lastaa with CCCC ['//line//']'
!!
!!    write(*,*)replace('myf90stuff.f90.f90','f90','for',occurrence=-1,repeat=1)
!!    write(*,*)replace('myf90stuff.f90.f90','f90','for',occurrence=-2,repeat=2)
!!
!!    end program demo_replace
!!
!!   Results:
!!
!!     this is the string
!!     this is the string
!!     this is xe string
!!     BEFORE:my line of text
!!     I wonder
!!     replace first a with A [Aaaaaaaaa]
!!     replace a with A for 3rd to 5th occurrence [aaAAAaaaa]
!!     replace a with null instances 3 to 5 [ababbb]
!!     replace lastaa with CCCC [a b ab baaa aaaa aa aa a a a aa aaaaCCCC]
!!     myf90stuff.f90.for
!!     myforstuff.for.f90
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
subroutine crack_cmd(cmd,old,new,ierr)
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=*),intent(in)              :: cmd
character(len=:),allocatable,intent(out) :: old,new                ! scratch string buffers
integer                                  :: ierr
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=1)                         :: delimiters
integer                                  :: itoken
integer,parameter                        :: id=2                   ! expected location of delimiter
logical                                  :: ifok
integer                                  :: lmax                   ! length of target string
integer                                  :: start_token,end_token
!-----------------------------------------------------------------------------------------------------------------------------------
   ierr=0
   old=''
   new=''
   lmax=len_trim(cmd)                       ! significant length of change directive

   if(lmax >= 4)then                      ! strtok ignores blank tokens so look for special case where first token is really null
      delimiters=cmd(id:id)               ! find delimiter in expected location
      itoken=0                            ! initialize strtok(3f) procedure

      if(strtok(cmd(id:),itoken,start_token,end_token,delimiters)) then        ! find OLD string
         old=cmd(start_token+id-1:end_token+id-1)
      else
         old=''
      endif

      if(cmd(id:id) == cmd(id+1:id+1))then
         new=old
         old=''
      else                                                                     ! normal case
         ifok=strtok(cmd(id:),itoken,start_token,end_token,delimiters)         ! find NEW string
         if(end_token  ==  (len(cmd)-id+1) )end_token=len_trim(cmd(id:))       ! if missing ending delimiter
         new=cmd(start_token+id-1:min(end_token+id-1,lmax))
      endif
   else                                                                        ! command was two or less characters
      ierr=-1
      call journal('sc','*crack_cmd* incorrect change directive -too short')
   endif

end subroutine crack_cmd
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function replace(targetline,old,new,cmd,occurrence,repeat,ignorecase,ierr) result (newline)

! ident_11="@(#) M_strings replace(3f) replace one substring for another in string"

!-----------------------------------------------------------------------------------------------------------------------------------
! parameters
character(len=*),intent(in)            :: targetline   ! input line to be changed
character(len=*),intent(in),optional   :: old          ! old substring to replace
character(len=*),intent(in),optional   :: new          ! new substring
character(len=*),intent(in),optional   :: cmd          ! contains the instructions changing the string
integer,intent(in),optional            :: occurrence   ! Nth occurrence of OLD string to start replacement at
integer,intent(in),optional            :: repeat       ! how many replacements
logical,intent(in),optional            :: ignorecase
integer,intent(out),optional           :: ierr         ! error code. if ierr = -1 bad directive, >=0 then ierr changes made
!-----------------------------------------------------------------------------------------------------------------------------------
! returns
character(len=:),allocatable  :: newline               ! output string buffer
!-----------------------------------------------------------------------------------------------------------------------------------
! local
character(len=:),allocatable  :: new_local, old_local, old_local_for_comparison
integer                       :: icount,ichange,ier2
integer                       :: original_input_length
integer                       :: len_old, len_new
integer                       :: ladd
integer                       :: left_margin, right_margin
integer                       :: ind
integer                       :: ic
integer                       :: ichr
integer                       :: range_local(2)
character(len=:),allocatable  :: targetline_for_comparison   ! input line to be changed
logical                       :: ignorecase_local
logical                       :: flip
character(len=:),allocatable  :: targetline_local   ! input line to be changed
!-----------------------------------------------------------------------------------------------------------------------------------
   flip=.false.
   ignorecase_local=.false.
   original_input_length=len_trim(targetline)          ! get non-blank length of input line

!  get old_local and new_local from cmd or old and new
   if(present(cmd))then
      call crack_cmd(cmd,old_local,new_local,ier2)
      if(ier2 /= 0)then
         newline=targetline  ! if no changes are made return original string on error
         if(present(ierr))ierr=ier2
         return
      endif
   elseif(present(old).and.present(new))then
      old_local=old
      new_local=new
   else
      newline=targetline  ! if no changes are made return original string on error
      call journal('sc','*replace* must specify OLD and NEW or CMD')
      return
   endif
   if(present(ignorecase))then
      ignorecase_local=ignorecase
   else
      ignorecase_local=.false.
   endif
   if(present(occurrence))then
      range_local(1)=abs(occurrence)
   else
      range_local(1)=1
   endif
   if(present(repeat))then
      range_local(2)=range_local(1)+repeat-1
   else
      range_local(2)=original_input_length
   endif
   if(ignorecase_local)then
      targetline_for_comparison=lower(targetline)
      old_local_for_comparison=lower(old_local)
   else
      targetline_for_comparison=targetline
      old_local_for_comparison=old_local
   endif
   if(present(occurrence))then
      if(occurrence < 0)then
         flip=.true.
         targetline_for_comparison=reverse(targetline_for_comparison)
         targetline_local=reverse(targetline)
         old_local_for_comparison=reverse(old_local_for_comparison)
         old_local=reverse(old_local)
         new_local=reverse(new_local)
      else
         targetline_local=targetline
      endif
   else
      targetline_local=targetline
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   icount=0                                            ! initialize error flag/change count
   ichange=0                                           ! initialize error flag/change count
   len_old=len(old_local)                              ! length of old substring to be replaced
   len_new=len(new_local)                              ! length of new substring to replace old substring
   left_margin=1                                       ! left_margin is left margin of window to change
   right_margin=len(targetline)                        ! right_margin is right margin of window to change
   newline=''                                          ! begin with a blank line as output string
!-----------------------------------------------------------------------------------------------------------------------------------
   if(len_old == 0)then                                ! c//new/ means insert new at beginning of line (or left margin)
      ichr=len_new + original_input_length
      if(len_new > 0)then
         newline=new_local(:len_new)//targetline_local(left_margin:original_input_length)
      else
         newline=targetline_local(left_margin:original_input_length)
      endif
      ichange=1                                        ! made one change. actually, c/// should maybe return 0
      if(present(ierr))ierr=ichange
      if(flip) newline=reverse(newline)
      return
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   ichr=left_margin                                    ! place to put characters into output string
   ic=left_margin                                      ! place looking at in input string
   loop: do
                                                       ! try finding start of OLD in remaining part of input in change window
      ind=index(targetline_for_comparison(ic:),old_local_for_comparison(:len_old))+ic-1
      if(ind == ic-1.or.ind > right_margin)then       ! did not find old string or found old string past edit window
         exit loop                                     ! no more changes left to make
      endif
      icount=icount+1                                  ! found an old string to change, so increment count of change candidates
      if(ind > ic)then                                ! if found old string past at current position in input string copy unchanged
         ladd=ind-ic                                   ! find length of character range to copy as-is from input to output
         newline=newline(:ichr-1)//targetline_local(ic:ind-1)
         ichr=ichr+ladd
      endif
      if(icount >= range_local(1).and.icount <= range_local(2))then    ! check if this is an instance to change or keep
         ichange=ichange+1
         if(len_new /= 0)then                                          ! put in new string
            newline=newline(:ichr-1)//new_local(:len_new)
            ichr=ichr+len_new
         endif
      else
         if(len_old /= 0)then                                          ! put in copy of old string
            newline=newline(:ichr-1)//old_local(:len_old)
            ichr=ichr+len_old
         endif
      endif
      ic=ind+len_old
   enddo loop
!-----------------------------------------------------------------------------------------------------------------------------------
   select case (ichange)
   case (0)                                            ! there were no changes made to the window
      newline=targetline_local                         ! if no changes made output should be input
   case default
      if(ic <= len(targetline))then                    ! if there is more after last change on original line add it
         newline=newline(:ichr-1)//targetline_local(ic:max(ic,original_input_length))
      endif
   end select
   if(present(ierr))ierr=ichange
   if(flip) newline=reverse(newline)
!-----------------------------------------------------------------------------------------------------------------------------------
end function replace
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    substitute(3f) - [M_strings:EDITING] subroutine globally substitutes
!!    one substring for another in string
!!    (LICENSE:PD)
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
!!     TARGETLINE  input line to be changed. Must be long enough to
!!                 hold altered output.
!!     OLD         substring to find and replace
!!     NEW         replacement for OLD substring
!!     IERR        error code. If IER = -1 bad directive, >= 0 then
!!                 count of changes made.
!!     START       sets the left margin to be scanned for OLD in
!!                 TARGETLINE.
!!     END         sets the right margin to be scanned for OLD in
!!                 TARGETLINE.
!!
!!##EXAMPLES
!!
!!   Sample Program:
!!
!!    program demo_substitute
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
!!    end program demo_substitute
!!
!!   Expected output
!!
!!     ORIGINAL    : this is the input string
!!     th => TH    : THis is THe input string
!!     "" => BEFORE: BEFORE:THis is THe input string
!!     i => ""     : BEFORE:THs s THe nput strng
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
subroutine substitute(targetline,old,new,ierr,start,end)

! ident_12="@(#) M_strings substitute(3f) Globally substitute one substring for another in string"

!-----------------------------------------------------------------------------------------------------------------------------------
character(len=*)               :: targetline         ! input line to be changed
character(len=*),intent(in)    :: old                ! old substring to replace
character(len=*),intent(in)    :: new                ! new substring
integer,intent(out),optional   :: ierr               ! error code. if ierr = -1 bad directive, >=0 then ierr changes made
integer,intent(in),optional    :: start              ! start sets the left margin
integer,intent(in),optional    :: end                ! end sets the right margin
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=len(targetline)) :: dum1               ! scratch string buffers
integer                        :: ml, mr, ier1
integer                        :: maxlengthout       ! MAXIMUM LENGTH ALLOWED FOR NEW STRING
integer                        :: original_input_length
integer                        :: len_old, len_new
integer                        :: ladd
integer                        :: ir
integer                        :: ind
integer                        :: il
integer                        :: id
integer                        :: ic
integer                        :: ichr
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
   id=mr-ml                                            ! check for window option ! change to optional parameter(s)
!-----------------------------------------------------------------------------------------------------------------------------------
   len_old=len(old)                                    ! length of old substring to be replaced
   len_new=len(new)                                    ! length of new substring to replace old substring
   if(id <= 0)then                                     ! no window so change entire input string
      il=1                                             ! il is left margin of window to change
      ir=maxlengthout                                  ! ir is right margin of window to change
      dum1(:)=' '                                      ! begin with a blank line
   else                                                ! if window is set
      il=ml                                            ! use left margin
      ir=min0(mr,maxlengthout)                         ! use right margin or rightmost
      dum1=targetline(:il-1)                           ! begin with what's below margin
   endif                                               ! end of window settings
!-----------------------------------------------------------------------------------------------------------------------------------
   if(len_old == 0)then                                ! c//new/ means insert new at beginning of line (or left margin)
      ichr=len_new + original_input_length
      if(ichr > maxlengthout)then
         call journal('sc','*substitute* new line will be too long')
         ier1=-1
         if (present(ierr))ierr=ier1
         return
      endif
      if(len_new > 0)then
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
   ichr=il                                            ! place to put characters into output string
   ic=il                                               ! place looking at in input string
   loop: do
      ind=index(targetline(ic:),old(:len_old))+ic-1    ! try to find start of old string in remaining part of input in change window
      if(ind == ic-1.or.ind > ir)then                 ! did not find old string or found old string past edit window
         exit loop                                     ! no more changes left to make
      endif
      ier1=ier1+1                                      ! found an old string to change, so increment count of changes
      if(ind > ic)then                                ! if found old string past at current position in input string copy unchanged
         ladd=ind-ic                                   ! find length of character range to copy as-is from input to output
         if(ichr-1+ladd > maxlengthout)then
            ier1=-1
            exit loop
         endif
         dum1(ichr:)=targetline(ic:ind-1)
         ichr=ichr+ladd
      endif
      if(ichr-1+len_new > maxlengthout)then
         ier1=-2
         exit loop
      endif
      if(len_new /= 0)then
         dum1(ichr:)=new(:len_new)
         ichr=ichr+len_new
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
      if(ichr+ladd > maxlengthout)then
         call journal('sc','*substitute* new line will be too long')
         ier1=-1
         if(present(ierr))ierr=ier1
         return
      endif
      if(ic < len(targetline))then
         dum1(ichr:)=targetline(ic:max(ic,original_input_length))
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
!!    change(3f) - [M_strings:EDITING] change old string to new string with
!!    a directive like a line editor
!!    (LICENSE:PD)
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
!!   Sample program:
!!
!!    program demo_change
!!
!!     use M_strings, only : change
!!     implicit none
!!     character(len=132) :: line='This is a test string to change'
!!     integer            :: ierr
!!        write(*,*)trim(line)
!!        ! change miniscule a to uppercase A
!!        call change(line,'c/a/A/',ierr)
!!        write(*,*)trim(line)
!!        ! put string at beginning of line
!!        call change(line,'c//prefix: /',ierr)
!!        write(*,*)trim(line)
!!        ! remove blanks
!!        call change(line,'c/ //',ierr)
!!        write(*,*)trim(line)
!!    end program demo_change
!!
!!   Expected output
!!
!!     This is a test string to change
!!     This is A test string to chAnge
!!     prefix: This is A test string to chAnge
!!     prefix:ThisisAteststringtochAnge
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
subroutine change(target_string,cmd,ierr)
! Change a string assumed long enough to accommodate the change, with a directive that resembles a line editor directive of the form
!    C/old_string/new_string/
! where / may be any character which is not included in old_string or new_string.
! a null old_string implies "beginning of string"
!===================================================================================================================================

! ident_13="@(#) M_strings change(3f) change a character string like a line editor"

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
   if(lmax >= 4)then                         ! strtok ignores blank tokens so look for special case where first token is really null
      delimiters=cmd(id:id)                                                    ! find delimiter in expected location
      itoken=0                                                                 ! initialize strtok(3f) procedure

      if(strtok(cmd(id:),itoken,start_token,end_token,delimiters)) then        ! find OLD string
         old=cmd(start_token+id-1:end_token+id-1)
      else
         old=''
      endif

      if(cmd(id:id) == cmd(id+1:id+1))then
         new=old
         old=''
      else                                                                     ! normal case
         ifok=strtok(cmd(id:),itoken,start_token,end_token,delimiters)         ! find NEW string
         if(end_token  ==  (len(cmd)-id+1) )end_token=len_trim(cmd(id:))       ! if missing ending delimiter
         new=cmd(start_token+id-1:min(end_token+id-1,lmax))
      endif

      call substitute(target_string,old,new,ierr,1,len_trim(target_string))    ! change old substrings to new substrings
   else                                                                        ! command was two or less characters
      ierr=-1
      call journal('sc','*change* incorrect change directive -too short')
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine change
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!     strtok(3f) - [M_strings:TOKENS] Tokenize a string
!!     (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!  function strtok(source_string,itoken,token_start,token_end,delimiters)
!!  result(strtok_status)
!!
!!   ! returned value
!!   logical                      :: strtok_status
!!   ! string to tokenize
!!   character(len=*),intent(in)  :: source_string
!!   ! token count since started
!!   integer,intent(inout)        :: itoken
!!   ! beginning of token
!!   integer,intent(out)          :: token_start
!!   ! end of token
!!   integer,intent(inout)        :: token_end
!!   ! list of separator characters
!!   character(len=*),intent(in)  :: delimiters
!!
!!##DESCRIPTION
!!     The STRTOK(3f) function is used to isolate sequential tokens in a
!!     string, SOURCE_STRING. These tokens are delimited in the string by
!!     at least one of the characters in DELIMITERS. The first time that
!!     STRTOK(3f) is called, ITOKEN should be specified as zero. Subsequent
!!     calls, wishing to obtain further tokens from the same string,
!!     should pass back in TOKEN_END  and ITOKEN until the function result
!!     returns .false.
!!
!!     This routine assumes no other calls are made to it using any other
!!     input string while it is processing an input line.
!!
!!##OPTIONS
!!     source_string  input string to parse
!!     itoken         token count should be set to zero for a new string
!!     delimiters     characters used to determine the end of tokens
!!
!!##RETURN
!!     token_start    beginning position in SOURCE_STRING where token was found
!!     token_end      ending position in SOURCE_STRING where token was found
!!     strtok_status
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!     program demo_strtok
!!     use M_strings, only : strtok
!!     implicit none
!!     character(len=264)          :: inline
!!     character(len=*),parameter  :: delimiters=' ;,'
!!     integer                     :: ios, itoken, istart, iend
!!        do ! read lines from stdin until end-of-file or error
!!           read (unit=*,fmt="(a)",iostat=ios) inline
!!           if(ios /= 0)stop
!!           ! must set ITOKEN=0 before looping on strtok(3f)
!!           ! on a new string.
!!           itoken=0
!!           do while &
!!           &( strtok(inline,itoken,istart,iend,delimiters) )
!!              print *, itoken,&
!!              & 'TOKEN=['//(inline(istart:iend))//']',istart,iend
!!           enddo
!!        enddo
!!     end program demo_strtok
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
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
FUNCTION strtok(source_string,itoken,token_start,token_end,delimiters) result(strtok_status)
! JSU- 20151030

! ident_14="@(#) M_strings strtok(3f) Tokenize a string"

character(len=*),intent(in)  :: source_string    ! Source string to tokenize.
character(len=*),intent(in)  :: delimiters       ! list of separator characters. May change between calls
integer,intent(inout)        :: itoken           ! token count since started
logical                      :: strtok_status    ! returned value
integer,intent(out)          :: token_start      ! beginning of token found if function result is .true.
integer,intent(inout)        :: token_end        ! end of token found if function result is .true.
integer,save                 :: isource_len
!----------------------------------------------------------------------------------------------------------------------------
!  calculate where token_start should start for this pass
   if(itoken <= 0)then                           ! this is assumed to be the first call
      token_start=1
   else                                          ! increment start to previous end + 1
      token_start=token_end+1
   endif
!----------------------------------------------------------------------------------------------------------------------------
   isource_len=len(source_string)                ! length of input string
!----------------------------------------------------------------------------------------------------------------------------
   if(token_start > isource_len)then            ! user input error or at end of string
      token_end=isource_len                      ! assume end of token is end of string until proven otherwise so it is set
      strtok_status=.false.
      return
   endif
!----------------------------------------------------------------------------------------------------------------------------
   ! find beginning of token
   do while (token_start  <=  isource_len)       ! step thru each character to find next delimiter, if any
      if(index(delimiters,source_string(token_start:token_start))  /=  0) then
         token_start = token_start + 1
      else
         exit
      endif
   enddo
!----------------------------------------------------------------------------------------------------------------------------
   token_end=token_start
   do while (token_end  <=  isource_len-1)       ! step thru each character to find next delimiter, if any
      if(index(delimiters,source_string(token_end+1:token_end+1))  /=  0) then  ! found a delimiter in next character
         exit
      endif
      token_end = token_end + 1
   enddo
!----------------------------------------------------------------------------------------------------------------------------
   if (token_start  >  isource_len) then        ! determine if finished
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
!!    modif(3f) - [M_strings:EDITING] emulate the MODIFY command from the
!!    line editor XEDIT
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine modif(cline,cmod)
!!
!!     character(len=*) :: cline ! input string to change
!!     ! directive provides directions on changing string
!!     character(len=*) :: cmod
!!
!!##DESCRIPTION
!!   MODIF(3f) Modifies the line currently pointed at using a directive
!!   that acts much like a line editor directive.
!!   Primarily used to create interactive utilities such as input history
!!   editors for interactive line-mode programs.
!!
!!   the modify directives are as follows-
!!
!!    DIRECTIVE EXPLANATION
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
!!   Example input/output:
!!
!!    THE INPUT LINE........ 10 THIS STRING  TO BE MORTIFD
!!    THE DIRECTIVES LINE...        ^ IS THE#        D#  ^IE
!!    ALTERED INPUT LINE.... 10 THIS IS THE STRING  TO BE MODIFIED
!!
!!   Sample program:
!!
!!    program demo_modif
!!    use M_strings, only : modif
!!    implicit none
!!    character(len=256)           :: line
!!    integer                      :: ios
!!    integer                      :: count
!!    integer                      :: COMMAND_LINE_LENGTH
!!    character(len=:),allocatable :: COMMAND_LINE
!!       ! get command name length
!!       call get_command_argument(0,length=count)
!!       ! get command line length
!!       call get_command(length=COMMAND_LINE_LENGTH)
!!       ! allocate string big enough to hold command line
!!       allocate(character(len=COMMAND_LINE_LENGTH+200) :: COMMAND_LINE)
!!       ! get command line as a string
!!       call get_command(command=COMMAND_LINE)
!!       ! trim leading spaces just in case
!!       COMMAND_LINE=adjustl(COMMAND_LINE)
!!       ! remove command name
!!       COMMAND_LINE=adjustl(COMMAND_LINE(COUNT+2:))
!!       INFINITE: do
!!          read(*,'(a)',iostat=ios)line
!!          if(ios /= 0)exit
!!          call modif(line,COMMAND_LINE)
!!          write(*,'(a)')trim(line)
!!       enddo INFINITE
!!    end program demo_modif
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
subroutine modif(cline,mod)

!$@(#) M_strings::modif(3f): Emulate the MODIFY command from the line editor XEDIT

!
! MODIF
! =====
! ACTION- MODIFIES THE LINE CURRENTLY POINTED AT. THE INPUT STRING CLINE IS ASSUMED TO BE LONG ENOUGH TO ACCOMMODATE THE CHANGES
!         THE MODIFY DIRECTIVES ARE AS FOLLOWS-
!
!   DIRECTIVE                       EXPLANATION
!   ---------                       ------------
!   ^STRING#   CAUSES THE STRING OF CHARACTERS BETWEEN THE ^ AND THE
!              NEXT  # TO BE INSERTED BEFORE THE CHARACTERS POINTED TO
!              BY THE ^. AN ^ OR & WITHIN THE STRING IS TREATED AS A
!              REGULAR CHARACTER. IF THE CLOSING # IS NOT SPECIFIED,
!              MODIF(3f) INSERTS THE REMAINDER OFTHELINE AS IF A # WAS
!              SPECIFIED AFTER THE LAST NONBLANK CHARACTER.
!
!              THERE ARE TWO EXCEPTIONS. THE COMBINATION ^# CAUSES A #
!              TO BE INSERTED BEFORE THE CHARACTER POINTED TO BY THE
!              ^,  AND AN ^ AS THE LAST CHARACTER OF THE DIRECTIVES
!              CAUSES A BLANK TO BE INSERTED.
!
!   #          (WHEN NOT THE FIRST # AFTER AN ^) CAUSES THE CHARACTER
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
character(len=*)            :: cline        !STRING TO BE MODIFIED
character(len=*),intent(in) :: mod          !STRING TO DIRECT MODIFICATION
character(len=len(cline))   :: cmod
character(len=3),parameter  :: c='#&^'      !ASSIGN DEFAULT EDIT CHARACTERS
integer                     :: maxscra      !LENGTH OF SCRATCH BUFFER
character(len=len(cline))   :: dum2         !SCRATCH CHARACTER BUFFER
logical                     :: linsrt       !FLAG FOR INSERTING DATA ON LINE
integer :: i, j, ic, ichr, iend, lmax, lmx1
maxscra=len(cline)
   cmod=trim(mod)
   lmax=min0(len(cline),maxscra)         !DETERMINE MAXIMUM LINE LENGTH
   lmx1=lmax-1                           !MAX LINE LENGTH -1
   dum2=' '                              !INITIALIZE NEW LINE
   linsrt=.false.                        !INITIALIZE INSERT MODE
   iend=len_trim(cmod)                   !DETERMINE END OF MODS
   i=0                                   !CHAR COUNTER FOR MOD LINE CMOD
   ic=0                                  !CHAR COUNTER FOR CURRENT LINE CLINE
   ichr=0                                !CHAR COUNTER NEW LINE DUM2
11 continue
   i=i+1                                 !NEXT CHAR IN MOD LINE
   if(ichr > lmx1)goto 999              !IF TOO MANY CHARS IN NEW LINE
   if(linsrt) then                       !IF INSERTING NEW CHARS
      if(i > iend) cmod(i:i)=c(1:1)     !FORCE END OF INSERT MODE
      if(cmod(i:i) == c(1:1))then        !IF END OF INSERT MODE
         linsrt=.false.                  !RESET INSERT MODE FLAG
         if(ic+1 == i)then               !NULL INSERT STRING
            ichr=ichr+1                  !INCREMENT COUNTER FOR NEW LINE
            dum2(ichr:ichr)=c(1:1)       !INSERT INSERT MODE TERMINATOR
         endif
         do j=ic,i                       !LOOP OF NUMBER OF CHARS INSERTED
            ichr=ichr+1                  !INCREMENT COUNTER FOR NEW LINE
            if(ichr > lmax)goto 999     !IF AT BUFFER LIMIT, QUIT
            dum2(ichr:ichr)=cline(j:j)   !APPEND CHARS FROM ORIG LINE
         enddo                           !...WHICH ALIGN WITH INSERTED CHARS
         ic=i                            !RESET CHAR COUNT TO END OF INSERT
         goto 1                          !CHECK NEW LINE LENGTH AND CYCLE
      endif                              !END OF TERMINATED INSERT LOGIC
      ichr=ichr+1                        !INCREMENT NEW LINE COUNT
      dum2(ichr:ichr)=cmod(i:i)          !SET NEWLINE CHAR TO INSERTED CHAR
   else                                  !IF NOT INSERTING CHARACTERS
      ic=ic+1                            !INCREMENT ORIGINAL LINE COUNTER
      if(cmod(i:i) == c(1:1))goto 1      !IF DELETE CHAR. NO COPY AND CYCLE
      if(cmod(i:i) == c(3:3))then        !IF BEGIN INSERT MODE
         linsrt=.true.                   !SET INSERT FLAG TRUE
         goto 1                          !CHECK LINE LENGTH AND CONTINUE
      endif                              !IF NOT BEGINNING INSERT MODE
      ichr=ichr+1                        !INCREMENT NEW LINE COUNTER
      if(cmod(i:i) == c(2:2))then        !IF REPLACE WITH BLANK
         dum2(ichr:ichr)=' '             !SET NEWLINE CHAR TO BLANK
         goto 1                          !CHECK LINE LENGTH AND CYCLE
      endif                              !IF NOT REPLACE WITH BLANK
      if(cmod(i:i) == ' ')then           !IF BLANK, KEEP ORIGINAL CHARACTER
         dum2(ichr:ichr)=cline(ic:ic)    !SET NEW CHAR TO ORIGINAL CHAR
      else                               !IF NOT KEEPING OLD CHAR
         dum2(ichr:ichr)=cmod(i:i)       !REPLACE ORIGINAL CHAR WITH NEW
      endif                              !END CHAR KEEP OR REPLACE
   endif                                 !END INSERT OR NO-INSERT
1  continue
   if(i < lmax)goto 11                  !CHECK FOR END OF LINE REACHED
                                         !AND CYCLE IF OK
999   continue
   cline=dum2                            !SET ORIGINAL CHARS TO NEW CHARS
end subroutine modif                     !RETURN
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!      len_white(3f) - [M_strings:LENGTH] get length of string trimmed
!!      of whitespace.
!!      (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    integer function len_white(string)
!!
!!     character(len=*) :: string
!!
!!##DESCRIPTION
!!      len_white(3f) returns the position of the last character in
!!      string that is not a whitespace character. The Fortran90 intrinsic
!!      LEN_TRIM() should be used when trailing whitespace can be assumed
!!      to always be spaces.
!!
!!      This procedure was heavily used in the past because ANSI FORTRAN
!!      77 character objects are fixed length and blank padded and the
!!      LEN_TRIM() intrinsic did not exist. It should now be used only when
!!      whitespace characters other than blanks are likely.
!!
!!##OPTIONS
!!      string     input string whose trimmed length is being calculated
!!                 ignoring all trailing whitespace characters.
!!##RETURNS
!!      len_white  the number of characters in the trimmed string
!!
!!##EXAMPLE
!!
!!   Sample Program:
!!
!!    program demo_len_white
!!
!!      use M_strings, only : len_white
!!      implicit none
!!      character(len=80) ::  s
!!      integer           :: lgth, lastnb
!!      intrinsic len
!!
!!      s=' ABCDEFG abcdefg '
!!      lgth = len(s)
!!      lastnb = len_white(s)
!!
!!      write(*,*) 'total length of variable is ',lgth
!!      write(*,*) 'trimmed length of variable is ',lastnb
!!      write(*,*) 'trimmed string=[',s(:lastnb),']'
!!
!!     end program demo_len_white
!!
!!   Results:
!!
!!     total length of variable is           80
!!     trimmed length of variable is           16
!!     trimmed string=[ ABCDEFG abcdefg]
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
!! o Some compilers seem to have trouble passing a string of variable
!!   length properly. To be safe, use something like this:
!!
!!       subroutine message(s)
!!        character(len=*) :: s ! s is of variable length
!!           lgth=len(s)        ! get total length of variable
!!           ! explicitly specify a substring instead of just variable name
!!           lastnb = len_white(s(:lgth))
!!           write(*,*)'error:[',s(:lastnb),']'
!!       end subroutine messages
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
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

! ident_15="@(#) M_strings len_white(3f) return position of last non-blank/non-null character in string"

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
!!    crop(3f) - [M_strings:WHITESPACE] trim leading and trailing blanks
!!               and control characters from a string
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function crop(strin) result (strout)
!!
!!     character(len=*),intent(in)  :: strin
!!     character(len=:),allocatable :: strout
!!
!!##DESCRIPTION
!!    All control characters throughout the string are replaced with spaces
!!    and leading and trailing spaces are trimmed from the resulting string.
!!    Tabs are expanded assuming a stop every eight characters.
!!
!!##OPTIONS
!!    strin   input string to trim leading and trailing space and control
!!            characters from
!!
!!##RETURNS
!!    strout  cropped version of input string
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_crop
!!    use M_strings, only: crop
!!    implicit none
!!    character(len=20) ::  untrimmed = '   ABCDEFG abcdefg  '
!!       write(*,*) 'untrimmed string=[',untrimmed,']'
!!       write(*,*) 'cropped string=[',crop(untrimmed),']'
!!    end program demo_crop
!!
!!   Expected output
!!
!!      untrimmed string=[   ABCDEFG abcdefg                      ]
!!      cropped string=[ABCDEFG abcdefg]
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
function crop(strin) result (strout)

! ident_16="@(#) M_strings crop(3f) replace control characters with whitespace and trim leading and trailings spaces from resulting string"

character(len=*),intent(in)  :: strin
character(len=:),allocatable :: strout
   strout=trim(adjustl(noesc(dilate(strin))))
end function crop
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    clip(3f) - [M_strings:WHITESPACE] trim leading and trailing blanks from a string
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function clip(strin) result (strout)
!!
!!     character(len=*),intent(in)  :: strin
!!     character(len=:),allocatable :: strout
!!
!!##DESCRIPTION
!!    leading and trailing spaces are trimmed from the resulting string.
!!
!!##OPTIONS
!!    strin   input string to trim leading and trailing space characters from
!!
!!##RETURNS
!!    strout  clipped version of input string
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_clip
!!    use M_strings, only: clip
!!    implicit none
!!    character(len=20) ::  untrimmed = '   ABCDEFG abcdefg  '
!!       write(*,*) 'untrimmed string=[',untrimmed,']'
!!       write(*,*) 'clipped string=[',clip(untrimmed),']'
!!    end program demo_clip
!!
!!   Expected output
!!
!!      untrimmed string=[   ABCDEFG abcdefg                      ]
!!      clipped string=[ABCDEFG abcdefg]
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
function clip(string) result(lopped)

! ident_17="@(#) M_strings clip(3f) trim leading and trailings spaces from resulting string"

logical,parameter            :: T=.true.,F=.false.
character(len=*),intent(in)  :: string
character(len=:),allocatable :: lopped
integer                      :: ends(2)
   ends=verify( string, " ", [F,T] )
   if(ends(1) == 0)then
      lopped=""
   else
      lopped=string(ends(1):ends(2))
   endif
end function clip
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    transliterate(3f) - [M_strings:EDITING] replace characters from old
!!                        set with new set
!!    (LICENSE:PD)
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
!!##OPTIONS
!!    instr    input string to change
!!    old_set  list of letters to change in INSTR if found
!!
!!             Each character in the input string that matches a character
!!             in the old set is replaced.
!!
!!    new_set  list of letters to replace letters in OLD_SET with.
!!
!!             If the new_set is the empty set the matched characters
!!             are deleted.
!!
!!             If the new_set is shorter than the old set the last character
!!             in the new set is used to replace the remaining characters
!!             in the new set.
!!
!!##RETURNS
!!    outstr   instr with substitutions applied
!!
!!##EXAMPLES
!!
!!   Sample Program:
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
!!     write(*,*) TRANSLITERATE(STRING, &
!!     & 'abcdefghijklmnopqrstuvwxyz','ABCDEFGHIJKLMNOPQRSTUVWXYZ')
!!
!!     ! change all miniscule letters to a colon (":"):
!!     write(*,*) TRANSLITERATE(STRING, &
!!     & 'abcdefghijklmnopqrstuvwxyz',':')
!!
!!     ! delete all miniscule letters
!!     write(*,*) TRANSLITERATE(STRING, &
!!     & 'abcdefghijklmnopqrstuvwxyz','')
!!
!!    end program demo_transliterate
!!
!!    Expected output
!!
!!     > aAbBcCdDeEfFgGhHiIjJkKlLmMnNoOpPqQrRsStTuUvVwWxXyYzZ
!!     > AABBCCDDEEFFGGHHIIJJKKLLMMNNOOPPQQRRSSTTUUVVWWXXYYZZ
!!     > :A:B:C:D:E:F:G:H:I:J:K:L:M:N:O:P:Q:R:S:T:U:V:W:X:Y:Z
!!     > ABCDEFGHIJKLMNOPQRSTUVWXYZ
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
PURE FUNCTION transliterate(instr,old_set,new_set) RESULT(outstr)

! ident_18="@(#) M_strings transliterate(3f) replace characters from old set with new set"

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
            if(ii <= jj)then                                      ! use corresponding character in new_set
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
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    rotate13(3f) - [M_strings] apply trivial ROT13 encryption to a string
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    rotate13(input) result(output)
!!
!!     character(len=*),intent(in) :: input
!!     character(len=len(input))   :: output
!!
!!##DESCRIPTION
!!    ROT13 ("rotate by 13 places", sometimes hyphenated ROT-13) is a simple
!!    letter substitution cipher that replaces a letter with the 13th letter
!!    after it in the alphabet; wrapping around if necessary.
!!
!!    The transformation can be done using a lookup table, such as the
!!    following:
!!
!!       Input  ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz
!!       Output NOPQRSTUVWXYZABCDEFGHIJKLMnopqrstuvwxyzabcdefghijklm
!!
!!    ROT13 is used in online forums as a means of hiding spoilers,
!!    punchlines, puzzle solutions, and offensive materials from the casual
!!    glance. ROT13 has inspired a variety of letter and word games on-line,
!!    and is frequently mentioned in newsgroup conversations.
!!
!!    The algorithm provides virtually no cryptographic security, and is
!!    often cited as a canonical example of weak encryption.
!!
!!    ROT13 is a special case of the Caesar cipher which was developed in
!!    ancient Rome.
!!
!!    ALGORITHM
!!
!!    Applying ROT13 to a piece of text merely requires examining its
!!    alphabetic characters and replacing each one by the letter 13 places
!!    further along in the alphabet, wrapping back to the beginning if
!!    necessary. A becomes N, B becomes O, and so on up to M, which becomes
!!    Z, then the sequence continues at the beginning of the alphabet: N
!!    becomes A, O becomes B, and so on to Z, which becomes M. Only those
!!    letters which occur in the English alphabet are affected; numbers,
!!    symbols, whitespace, and all other characters are left unchanged.
!!
!!    SAME ALGORITHM FOR ENCODING AND DECODING
!!
!!    Because there are 26 letters in the English alphabet and 26 = 2 x 13,
!!    the ROT13 function is its own inverse: so the same action can be used
!!    for encoding and decoding. In other words, two successive applications
!!    of ROT13 restore the original text (in mathematics, this is sometimes
!!    called an involution; in cryptography, a reciprocal cipher).
!!
!!    TRIVIAL SECURITY
!!
!!    The use of a constant shift means that the encryption effectively
!!    has no key, and decryption requires no more knowledge than the fact
!!    that ROT13 is in use. Even without this knowledge, the algorithm is
!!    easily broken through frequency analysis.
!!
!!    In encrypted normal English-language text of any significant size,
!!    ROT13 is recognizable from some letter/word patterns. The words "n",
!!    "V" (capitalized only), and "gur" (ROT13 for "a", "I", and "the"),
!!    and words ending in "yl" ("ly") are examples.
!!
!!##REFERENCES
!!    Wikipedia, the free encyclopedia
!!
!!##EXAMPLE
!!
!!   Sample program
!!
!!    program demo_rotate13
!!    use M_strings, only : rotate13
!!    implicit none
!!    character(len=256) :: line
!!    integer            :: ios
!!    do
!!       read(*,'(a)',iostat=ios)line
!!       if(ios /= 0)exit
!!       write(*,'(a)')rotate13(line)
!!    enddo
!!    end program demo_rotate13
!!
!!  Sample usage:
!!
!!    demo_rotate13
!!    United we stand, divided we fall.
!!    Havgrq jr fgnaq, qvivqrq jr snyy.
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
function rotate13 (input)
implicit none

! ident_19="@(#) M_strings rotate13(3f) converts a character to its ROT13 equivalent which is a trivial encryption."

character(len=*),intent(in) :: input
character(len=len(input))   :: rotate13
integer                     :: itemp
integer                     :: i
   rotate13=' '
   do i=1,len_trim(input)
      itemp = iachar(input(i:i))
      select case(itemp)
       case(65:77,97:109)
         itemp = itemp + 13
       case(78:90,110:122)
         itemp = itemp - 13
      end select
      rotate13(i:i) = char ( itemp )
   enddo

end function rotate13
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    join(3f) - [M_strings:EDITING] append CHARACTER variable array into
!!    a single CHARACTER variable with specified separator
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    pure function join(str,sep,trm,left,right,start,end) result (string)
!!
!!     character(len=*),intent(in)          :: str(:)
!!     character(len=*),intent(in),optional :: sep
!!     logical,intent(in),optional          :: trm
!!     character(len=*),intent(in),optional :: right
!!     character(len=*),intent(in),optional :: left
!!     character(len=*),intent(in),optional :: start
!!     character(len=*),intent(in),optional :: end
!!     character(len=:),allocatable         :: string
!!
!!##DESCRIPTION
!!   JOIN(3f) appends the elements of a CHARACTER array into a single
!!   CHARACTER variable, with elements 1 to N joined from left to right.
!!   By default each element is trimmed of trailing spaces and the
!!   default separator is a null string.
!!
!!##OPTIONS
!!      STR(:)  array of CHARACTER variables to be joined
!!      SEP     separator string to place between each variable. defaults
!!              to a null string.
!!      LEFT    string to place at left of each element
!!      RIGHT   string to place at right of each element
!!      START   prefix string
!!      END     suffix string
!!      TRM     option to trim each element of STR of trailing
!!              spaces. Defaults to .TRUE.
!!
!!##RESULT
!!      STRING  CHARACTER variable composed of all of the elements of STR()
!!              appended together with the optional separator SEP placed
!!              between the elements.
!!
!!##EXAMPLE
!!
!!  Sample program:
!!
!!   program demo_join
!!   use M_strings, only: join
!!   implicit none
!!   character(len=:),allocatable  :: s(:)
!!   character(len=:),allocatable  :: out
!!   integer                       :: i
!!     s=[character(len=10) :: 'United',' we',' stand,', &
!!     & ' divided',' we fall.']
!!     out=join(s)
!!     write(*,'(a)') out
!!     write(*,'(a)') join(s,trm=.false.)
!!     write(*,'(a)') (join(s,trm=.false.,sep='|'),i=1,3)
!!     write(*,'(a)') join(s,sep='<>')
!!     write(*,'(a)') join(s,sep=';',left='[',right=']')
!!     write(*,'(a)') join(s,left='[',right=']')
!!     write(*,'(a)') join(s,left='>>')
!!   end program demo_join
!!
!!  Expected output:
!!
!!   United we stand, divided we fall.
!!   United     we        stand,    divided   we fall.
!!   United    | we       | stand,   | divided  | we fall.
!!   United    | we       | stand,   | divided  | we fall.
!!   United    | we       | stand,   | divided  | we fall.
!!   United<> we<> stand,<> divided<> we fall.
!!   [United];[ we];[ stand,];[ divided];[ we fall.]
!!   [United][ we][ stand,][ divided][ we fall.]
!!   >>United>> we>> stand,>> divided>> we fall.
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
pure function join(str,sep,trm,left,right,start,end) result (string)

! ident_20="@(#) M_strings join(3f) merge string array into a single CHARACTER value adding specified separators caps prefix and suffix"

character(len=*),intent(in)          :: str(:)
character(len=*),intent(in),optional :: sep, right, left, start, end
logical,intent(in),optional          :: trm
character(len=:),allocatable         :: sep_local, left_local, right_local
character(len=:),allocatable         :: string
logical                              :: trm_local
integer                              :: i
   if(present(sep))then   ; sep_local=sep     ; else ; sep_local=''     ; endif
   if(present(trm))then   ; trm_local=trm     ; else ; trm_local=.true. ; endif
   if(present(left))then  ; left_local=left   ; else ; left_local=''    ; endif
   if(present(right))then ; right_local=right ; else ; right_local=''   ; endif
   string=''
   if(size(str) == 0)then
      string=string//left_local//right_local
   else
      do i = 1,size(str)-1
         if(trm_local)then
            string=string//left_local//trim(str(i))//right_local//sep_local
         else
            string=string//left_local//str(i)//right_local//sep_local
         endif
      enddo
      if(trm_local)then
         string=string//left_local//trim(str(i))//right_local
      else
         string=string//left_local//str(i)//right_local
      endif
   endif
   if(present(start))string=start//string
   if(present(end))string=string//end
end function join
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!      reverse(3f) - [M_strings:EDITING] Return a string reversed
!!      (LICENSE:PD)
!!
!!##SYNOPSIS
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
!!    Sample program:
!!
!!       program demo_reverse
!!       use M_strings, only: reverse
!!       implicit none
!!       character(len=:),allocatable  :: s
!!          write(*,*)'REVERSE STRINGS:',reverse('Madam, I''m Adam')
!!          s='abcdefghijklmnopqrstuvwxyz'
!!          write(*,*) 'original input string is ....',s
!!          write(*,*) 'reversed output string is ...',reverse(s)
!!       end program demo_reverse
!!
!!    Expected output
!!
!!      original input string is ....abcdefghijklmnopqrstuvwxyz
!!      reversed output string is ...zyxwvutsrqponmlkjihgfedcba
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
elemental function reverse(string ) result (rev)

! ident_21="@(#) M_strings reverse(3f) Return a string reversed"

character(len=*),intent(in)    :: string   ! string to reverse
character(len=len(string))     :: rev      ! return value (reversed string)
integer                        :: length
integer                        :: i
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
!! upper_quoted(3f) - [M_strings:CASE] elemental function converts string to
!!                miniscule skipping strings quoted per Fortran syntax rules
!! (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    elemental pure function upper_quoted(str) result (string)
!!
!!     character(*), intent(in)    :: str
!!     character(len(str))         :: string  ! output string
!!
!!##DESCRIPTION
!!    upper_quoted(string) returns a copy of the input string with all not-quoted
!!    characters converted to uppercase, assuming ASCII character sets
!!    are being used. The quoting rules are the same as for Fortran source.
!!    Either a single or double quote starts a quoted string, and a quote
!!    character of the same type is doubled when it appears internally in
!!    the quoted string. If a double quote quotes the string single quotes
!!    may appear in the quoted string as single characters, and vice-versa
!!    for single quotes.
!!
!!##OPTIONS
!!    str    string to convert to uppercase
!!
!!##RESULTS
!!    upper  copy of the input string with all unquoted characters converted
!!           to uppercase
!!
!!##EXAMPLE
!!
!!    Sample program:
!!
!!     program demo_upper_quoted
!!     use M_strings, only: upper_quoted
!!     implicit none
!!     character(len=:),allocatable  :: s
!!     s=' ABCDEFG abcdefg "Double-Quoted" ''Single-Quoted'' "with ""&
!!        & Quote" everything else'
!!        write(*,*) 'mixed-case input string is ....',s
!!        write(*,*) 'upper-case output string is ...',upper_quoted(s)
!!        write(*,'(1x,a,*(a:,"+"))') 'upper_quoted(3f) is elemental ==>', &
!!        & upper_quoted(["abc","def","ghi"])
!!     end program demo_upper_quoted
!!
!!    Expected output:
!!
!!     mixed-case input string is .... ABCDEFG abcdefg "Double-Quoted"
!!     'Single-Quoted' "with "" Quote" everything else
!!     upper-case output string is ... ABCDEFG ABCDEFG "Double-Quoted"
!!     'Single-Quoted' "with "" Quote" EVERYTHING ELSE
!!     upper_quoted(3f) is elemental ==>ABC+DEF+GHI
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
elemental pure function upper_quoted(str) result (string)

! ident_22="@(#) M_strings upper_quoted(3f) elemental function converts string to miniscule skipping strings quoted per Fortran syntax rules"

character(len=*), intent(in)   :: str     ! The input string
character(len=len(str))        :: string  ! The output string
logical                        :: toggle
character(len=1)               :: togglechar
integer                        :: irnk
integer                        :: i
character(len=26), parameter   :: large="ABCDEFGHIJKLMNOPQRSTUVWXYZ"
character(len=26), parameter   :: small="abcdefghijklmnopqrstuvwxyz"

   string=str
   toggle = .TRUE.
   do i = 1, len_trim(string)
      if(toggle) then
         if(string(i:i) == '"' .or. string(i:i) == "'") then
            toggle = .not. toggle
            togglechar = string(i:i)
         endif
         irnk = index(small, string(i:i))
         if(irnk > 0) then
            string(i:i) = large(irnk:irnk)
         endif
      else
         if(string(i:i) == togglechar) toggle = .not. toggle
      endif
   enddo
end function upper_quoted
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!! upper(3f) - [M_strings:CASE] changes a string to uppercase
!! (LICENSE:PD)
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
!!      converted in the optionally specified range to uppercase, assuming
!!      ASCII character sets are being used. If no range is specified the
!!      entire string is converted to uppercase.
!!
!!##OPTIONS
!!    str    string to convert to uppercase
!!    begin  optional starting position in "str" to begin converting to
!!           uppercase
!!    end    optional ending position in "str" to stop converting to
!!           uppercase
!!
!!##RESULTS
!!    upper  copy of the input string with all characters converted to
!!           uppercase over optionally specified range.
!!
!!##TRIVIA
!!    The terms "uppercase" and "lowercase" date back to the early days of
!!    the mechanical printing press. Individual metal alloy casts of each
!!    needed letter, or punctuation symbol, were meticulously added to a
!!    press block, by hand, before rolling out copies of a page. These
!!    metal casts were stored and organized in wooden cases. The more
!!    often needed miniscule letters were placed closer to hand, in the
!!    lower cases of the work bench. The less often needed, capitalized,
!!    majuscule letters, ended up in the harder to reach upper cases.
!!
!!##EXAMPLE
!!
!!    Sample program:
!!
!!     program demo_upper
!!     use M_strings, only: upper
!!     implicit none
!!     character(len=:),allocatable  :: s
!!        s=' ABCDEFG abcdefg '
!!        write(*,*) 'mixed-case input string is ....',s
!!        write(*,*) 'upper-case output string is ...',upper(s)
!!        write(*,*) 'make first character uppercase  ... ',&
!!        & upper('this is a sentence.',1,1)
!!        write(*,'(1x,a,*(a:,"+"))') 'UPPER(3f) is elemental ==>',&
!!        & upper(["abc","def","ghi"])
!!     end program demo_upper
!!
!!    Expected output
!!
!!     mixed-case input string is .... ABCDEFG abcdefg
!!     upper-case output string is ... ABCDEFG ABCDEFG
!!     make first character uppercase  ... This is a sentence.
!!     UPPER(3f) is elemental ==>ABC+DEF+GHI
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
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

! ident_23="@(#) M_strings upper(3f) returns a trimmed uppercase string"

character(*), intent(in)      :: str                 ! input string to convert to all uppercase
integer, intent(in), optional :: begin,end
character(len(str))           :: string              ! output string that contains no miniscule letters
integer                       :: i                   ! loop counter
integer                       :: ibegin,iend
integer,parameter             :: diff = iachar('A')-iachar('a')
   string = str                                      ! initialize output string to input string
   ibegin=1
   iend=len_trim(str)

   if (present(begin))then
      ibegin = min(max(ibegin,begin),iend)
   endif

   if (present(end))then
      iend= max(1,min(iend,end))
   endif

   do concurrent (i = ibegin:iend)                    ! step thru each letter in the string in specified range
       select case (str(i:i))
       case ('a':'z')                                 ! located miniscule letter
          string(i:i) = achar(iachar(str(i:i))+diff)  ! change miniscule letter to majascule
       end select
   enddo

end function upper
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    lower(3f) - [M_strings:CASE] changes a string to lowercase over
!!    specified range
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
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
!!    begin  optional starting position in "str" to begin converting to
!!           miniscule
!!    end    optional ending position in "str" to stop converting to
!!           miniscule
!!
!!##RESULTS
!!    lower  copy of the input string with all characters converted to
!!           miniscule over optionally specified range.
!!
!!##TRIVIA
!!    The terms "uppercase" and "lowercase" date back to the early days of
!!    the mechanical printing press. Individual metal alloy casts of each
!!    needed letter, or punctuation symbol, were meticulously added to a
!!    press block, by hand, before rolling out copies of a page. These
!!    metal casts were stored and organized in wooden cases. The more
!!    often needed miniscule letters were placed closer to hand, in the
!!    lower cases of the work bench. The less often needed, capitalized,
!!    majuscule letters, ended up in the harder to reach upper cases.
!!
!!##EXAMPLE
!!
!!    Sample program:
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
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
elemental pure function lower(str,begin,end) result (string)

! ident_24="@(#) M_strings lower(3f) Changes a string to lowercase over specified range"

character(*), intent(in)     :: str
character(len(str))          :: string
integer,intent(in),optional  :: begin, end
integer                      :: i
integer                      :: ibegin, iend
integer,parameter             :: diff = iachar('A')-iachar('a')
   string = str
   ibegin=1
   iend=len_trim(str)

   if (present(begin))then
      ibegin = min(max(1,begin),iend)
   endif

   if (present(end))then
      iend= max(1,min(iend,end))
   endif

   do concurrent (i = ibegin:iend)                   ! step thru each letter in the string in specified range
      select case (str(i:i))
      case ('A':'Z')
         string(i:i) = achar(iachar(str(i:i))-diff)   ! change letter to miniscule
      case default
      end select
   enddo

end function lower
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!
!!    switch(3f) - [M_strings:ARRAY] converts between CHARACTER scalar and
!!    array of single characters
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    pure function switch(array) result (string)
!!
!!     character(len=1),intent(in) :: array(:)
!!     character(len=SIZE(array))  :: string
!!
!!      or
!!
!!    pure function switch(string) result (array)
!!
!!     character(len=1),intent(in) :: array(:)
!!     character(len=SIZE(array))  :: string
!!
!!##DESCRIPTION
!!    SWITCH(3f): generic function that switches CHARACTER string to an array
!!    of single characters or an array of single characters to a CHARACTER
!!    string. Useful in passing strings to C. New Fortran features may
!!    supersede these routines.
!!
!!##EXAMPLES
!!
!!  Sample program:
!!
!!    program demo_switch
!!    use M_strings, only : switch, isalpha, islower, nospace
!!    character(len=*),parameter :: &
!!    & dashes='-----------------------------------'
!!    character(len=*),parameter :: string='This is a string'
!!    character(len=1024)        :: line
!!
!!    ! First, examples of standard Fortran features
!!    ! returns array [F,T,T,T,T,T]
!!    write(*,*)['A','=','=','=','=','='] == '='
!!    ! this would return T
!!    write(*,*)all(['=','=','=','=','=','='] == '=')
!!    ! this would return F
!!    write(*,*)all(['A','=','=','=','=','='] == '=')
!!
!!    ! so to test if the string DASHES is all dashes
!!    ! using SWITCH(3f) is
!!    if(all(switch(dashes) == '-'))then
!!       write(*,*)'DASHES is all dashes'
!!    endif
!!
!!    ! so to test is a string is all letters
!!    ! isalpha(3f) returns .true. only if character is a letter
!!    ! false because dashes are not a letter
!!    write(*,*) all(isalpha(switch(dashes)))
!!    ! false because of spaces
!!    write(*,*) all(isalpha(switch(string)))
!!    ! true because removed whitespace
!!    write(*,*) all(isalpha(switch(nospace(string))))
!!
!!    ! to see if a string is all uppercase
!!    ! show the string
!!    write(*,*) string
!!    ! converted to character array
!!    write(*,'(1x,*("[",a,"]":))') switch(string)
!!    write(*,'(*(l3))') islower(switch(string))
!!
!!    ! we need a string that is all letters
!!    line=nospace(string)
!!    write(*,*)'LINE=',trim(line)
!!    ! all true except first character
!!    write(*,*) islower(switch(nospace(string)))
!!    ! should be false
!!    write(*,*) all(islower(switch(nospace(string))))
!!    ! should be true
!!    write(*,*) all(islower(switch(nospace(string(2:)))))
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
!!     This is a string
!!     [T][h][i][s][ ][i][s][ ][a][ ][s][t][r][i][n][g]
!!      F  T  T  T  F  T  T  F  T  F  T  T  T  T  T  T
!!     LINE=Thisisastring
!!     F T T T T T T T T T T T T
!!     F
!!     T
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
pure function a2s(array)  result (string)

! ident_25="@(#) M_strings a2s(3fp) function to copy char array to string"

character(len=1),intent(in) :: array(:)
character(len=SIZE(array))  :: string
integer                     :: i
! ----------------------------------------------------------------------------------------------------------------------------------
   forall( i = 1:size(array)) string(i:i) = array(i)
! ----------------------------------------------------------------------------------------------------------------------------------
!  string=transfer(array,string)
end function a2s
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
pure function s2a(string)  RESULT (array)

! ident_26="@(#) M_strings s2a(3fp) function to copy string(1 Clen(string)) to char array"

character(len=*),intent(in) :: string
character(len=1)            :: array(len(string))
integer                     :: i
! ----------------------------------------------------------------------------------------------------------------------------------
   forall(i=1:len(string)) array(i) = string(i:i)
! ----------------------------------------------------------------------------------------------------------------------------------
!  array=transfer(string,array)
end function s2a
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!      s2c(3f) - [M_strings:ARRAY] convert character variable to array of
!!      characters with last element set to null
!!      (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function s2c(string)
!!
!!     character(len=*),intent=(in)  :: string
!!     character(len=1),allocatable  :: s2c(:)
!!
!!##DESCRIPTION
!!    Given a character variable convert it to an array of single-character
!!    character variables with the last element set to a null character.
!!    This is generally used to pass character variables to C procedures.
!!
!!##EXAMPLES
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
!!             & merge('XXX',array,iachar(array(:)(1:1)) < 32)
!!        write(*,'(1x,*("[",i3,"]":))')&
!!             & iachar(array(:)(1:1))
!!     end program demo_s2c
!!
!!   Expected output:
!!
!!    INPUT STRING single string
!!    [s  ][i  ][n  ][g  ][l  ][e  ][   ][s  ][t  ][r  ][i  ][n  ][g  ][XXX]
!!    [115][105][110][103][108][101][ 32][115][116][114][105][110][103][  0]
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
pure function s2c(string)  RESULT (array)
use,intrinsic :: ISO_C_BINDING, only : C_CHAR

! ident_27="@(#) M_strings s2c(3f) copy string(1 Clen(string)) to char array with null terminator"

character(len=*),intent(in)     :: string

! This is changing, but currently the most portable way to pass a CHARACTER variable to C is to convert it to an array of
! character variables with length one and add a null character to the end of the array. The s2c(3f) function helps do this.
character(kind=C_CHAR,len=1)    :: array(len_trim(string)+1)
integer                         :: i
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
!!      c2s(3f) - [M_strings:ARRAY] convert C string pointer to Fortran
!!      character string
!!      (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function c2s(c_string_pointer) result(f_string)
!!
!!     type(c_ptr), intent(in)       :: c_string_pointer
!!     character(len=:), allocatable :: f_string
!!
!!##DESCRIPTION
!!    Given a C pointer to a character string return a Fortran character
!!    string.
!!
!!##OPTIONS
!!    c_string_pointer  C pointer to convert
!!
!!##RETURNS
!!    f_string          Fortran character variable to return
!!
!!##EXAMPLE
!!
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
function c2s(c_string_pointer) result(f_string)
! gets a C string (pointer), and returns the corresponding Fortran string;
! If the C string is null, it returns "NULL", similar to C's "(null)" printed in similar cases:
use, intrinsic :: iso_c_binding, only: c_ptr,c_f_pointer,c_char,c_null_char

! ident_28="@(#) M_strings c2s(3f) copy pointer to C char array till a null is encountered to a Fortran string up to 4096 characters"

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
     if (char_array_pointer(i)==c_null_char) then
       length=i-1
       exit
     endif
     aux_string(i:i)=char_array_pointer(i)
   enddo
   allocate(character(len=length)::f_string)
   f_string=aux_string(1:length)

end function c2s
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!      indent(3f) - [M_strings:WHITESPACE] count number of leading spaces
!!      in a string
!!      (LICENSE:PD)
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
!!  Sample Program:
!!
!!    program demo_indent
!!    !  test filter to count leading spaces in a character variable
!!    !  might want to call notabs(3f) to expand tab characters
!!    use M_strings, only : indent
!!    implicit none
!!    character(len=1024) :: in
!!    integer             :: ios
!!       READFILE: do
!!          read(*,'(A)',iostat=ios)in
!!          if(ios /= 0) exit READFILE
!!          write(*,'(i3,"",a)')indent(in),trim(in)
!!       enddo READFILE
!!    end program demo_indent
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
function indent(line)
implicit none

! ident_29="@(#) M_strings indent(3f) find number of leading spaces in a string"

integer                        :: indent
character(len=*),intent(in)    :: line
integer                        :: i
   indent=0
   NOTSPACE: block
      SCAN: do i=1,len(line)
         if(line(i:i) /= ' ')then
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
!!    visible(3f) - [M_strings:NONALPHA] expand a string to control and
!!    meta-control representations
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function visible(input) result(output)
!!
!!     character(len=*),intent(in)           :: input
!!     character(len=:),allocatable          :: output
!!
!!##DESCRIPTION
!!     visible(3f) expands characters to commonly used sequences used
!!     to represent the characters as control sequences or meta-control
!!     sequences.
!!
!!##EXAMPLES
!!
!!   Sample Program:
!!
!!     program demo_visible
!!     use M_strings, only : visible
!!     integer :: i
!!        do i=0,255
!!           write(*,'(i0,1x,a)')i,visible(char(i))
!!        enddo
!!     end program demo_visible
!!##BUGS
!!     The expansion is not reversible, as input sequences such as "M-" or
!!     "^a" will look like expanded sequences.
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
function visible(input) result(output)
character(len=*),intent(in)  :: input
character(len=:),allocatable :: output

! ident_30="@(#) M_strings visible(3f) expand escape sequences in a string to control and meta-control representations"

integer                      :: i
character(len=1)             :: c

character(len=*),parameter :: chars(0:255)= [ &
'^@  ', '^A  ', '^B  ', '^C  ', '^D  ', '^E  ', '^F  ', '^G  ', '^H  ', '^I  ', &
'^J  ', '^K  ', '^L  ', '^M  ', '^N  ', '^O  ', '^P  ', '^Q  ', '^R  ', '^S  ', &
'^T  ', '^U  ', '^V  ', '^W  ', '^X  ', '^Y  ', '^Z  ', '^[  ', '^\  ', '^]  ', &
'^^  ', '^_  ', '    ', '!   ', '"   ', '#   ', '$   ', '%   ', '&   ', '''   ', &
'(   ', ')   ', '*   ', '+   ', ',   ', '-   ', '.   ', '/   ', '0   ', '1   ', &
'2   ', '3   ', '4   ', '5   ', '6   ', '7   ', '8   ', '9   ', ':   ', ';   ', &
'<   ', '=   ', '>   ', '?   ', '@   ', 'A   ', 'B   ', 'C   ', 'D   ', 'E   ', &
'F   ', 'G   ', 'H   ', 'I   ', 'J   ', 'K   ', 'L   ', 'M   ', 'N   ', 'O   ', &
'P   ', 'Q   ', 'R   ', 'S   ', 'T   ', 'U   ', 'V   ', 'W   ', 'X   ', 'Y   ', &
'Z   ', '[   ', '\   ', ']   ', '^   ', '_   ', '`   ', 'a   ', 'b   ', 'c   ', &
'd   ', 'e   ', 'f   ', 'g   ', 'h   ', 'i   ', 'j   ', 'k   ', 'l   ', 'm   ', &
'n   ', 'o   ', 'p   ', 'q   ', 'r   ', 's   ', 't   ', 'u   ', 'v   ', 'w   ', &
'x   ', 'y   ', 'z   ', '{   ', '|   ', '}   ', '~   ', '^?  ', 'M-^@', 'M-^A', &
'M-^B', 'M-^C', 'M-^D', 'M-^E', 'M-^F', 'M-^G', 'M-^H', 'M-^I', 'M-^J', 'M-^K', &
'M-^L', 'M-^M', 'M-^N', 'M-^O', 'M-^P', 'M-^Q', 'M-^R', 'M-^S', 'M-^T', 'M-^U', &
'M-^V', 'M-^W', 'M-^X', 'M-^Y', 'M-^Z', 'M-^[', 'M-^\', 'M-^]', 'M-^^', 'M-^_', &
'M-  ', 'M-! ', 'M-" ', 'M-# ', 'M-$ ', 'M-% ', 'M-& ', 'M-'' ', 'M-( ', 'M-) ', &
'M-* ', 'M-+ ', 'M-, ', 'M-- ', 'M-. ', 'M-/ ', 'M-0 ', 'M-1 ', 'M-2 ', 'M-3 ', &
'M-4 ', 'M-5 ', 'M-6 ', 'M-7 ', 'M-8 ', 'M-9 ', 'M-: ', 'M-; ', 'M-< ', 'M-= ', &
'M-> ', 'M-? ', 'M-@ ', 'M-A ', 'M-B ', 'M-C ', 'M-D ', 'M-E ', 'M-F ', 'M-G ', &
'M-H ', 'M-I ', 'M-J ', 'M-K ', 'M-L ', 'M-M ', 'M-N ', 'M-O ', 'M-P ', 'M-Q ', &
'M-R ', 'M-S ', 'M-T ', 'M-U ', 'M-V ', 'M-W ', 'M-X ', 'M-Y ', 'M-Z ', 'M-[ ', &
'M-\ ', 'M-] ', 'M-^ ', 'M-_ ', 'M-` ', 'M-a ', 'M-b ', 'M-c ', 'M-d ', 'M-e ', &
'M-f ', 'M-g ', 'M-h ', 'M-i ', 'M-j ', 'M-k ', 'M-l ', 'M-m ', 'M-n ', 'M-o ', &
'M-p ', 'M-q ', 'M-r ', 'M-s ', 'M-t ', 'M-u ', 'M-v ', 'M-w ', 'M-x ', 'M-y ', &
'M-z ', 'M-{ ', 'M-| ', 'M-} ', 'M-~ ', 'M-^?']
output=''
do i=1,len(input)
   c=input(i:i)
   if(c == ' ')then
      output=output//' '
   else
      output=output//trim(chars(iachar(c)))
   endif
enddo
end function visible
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    expand(3f) - [M_strings:NONALPHA] expand C-like escape sequences
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   function expand(line,escape) result(lineout)
!!
!!    character(len=*)                      :: line
!!    character(len=1),intent(in),optional  :: escape
!!    character(len=:),allocatable          :: lineout
!!
!!##DESCRIPTION
!!     EXPAND() expands sequences used to represent commonly used escape
!!     sequences or control characters. By default ...
!!
!!     Escape sequences
!!       \      backslash
!!       a      alert (BEL) -- g is an alias for a
!!       b      backspace
!!       c      suppress further output
!!       e      escape
!!       f      form feed
!!       n      new line
!!       r      carriage return
!!       t      horizontal tab
!!       v      vertical tab
!!       oNNN   byte with octal value NNN (3 digits)
!!       dNNN   byte with decimal value NNN (3 digits)
!!       xHH    byte with hexadecimal value HH (2 digits) -- h is an alias for x
!!
!!     The default escape character is the backslash, but this may be
!!     changed using the optional parameter ESCAPE.
!!
!!##EXAMPLES
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
!!
!!    Sample input:
!!
!!      \e[2J
!!      \tABC\tabc
!!      \tA\a
!!      \nONE\nTWO\nTHREE
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
function expand(line,escape) result(lineout)
!x!USE ISO_C_BINDING ,ONLY: c_horizontal_tab
implicit none

! ident_31="@(#) M_strings expand(3f) return string with escape sequences expanded"

character(len=*),parameter            :: c_horizontal_tab=char(9)
character(len=*),intent(in)           :: line
character(len=1),intent(in),optional  :: escape ! escape character. Default is backslash
! expand escape sequences found in input string
! Escape sequences
!    %%      escape character           %a     alert (BEL) -- gi is an alias for a
!    %b      backspace                  %c     suppress further output
!    %e      escape                     %E     escape
!    %f      form feed                  %n     new line
!    %r      carriage return            %t     horizontal tab
!    %v      vertical tab
!    %oNNN   byte with octal value NNN (3 digits)
!    %dNNN   byte with decimal value NNN (3 digits)
!    %xHH    byte with hexadecimal value HH (2 digits) -- h is an alias for x
character(len=1)                      :: esc    ! escape character. Default is %
character(len=:),allocatable          :: lineout
integer                               :: i
integer                               :: lgth
character(len=3)                      :: thr
integer                               :: xxx
integer                               :: ios
   i=0 ! pointer into input

   lgth=len_trim(line)
   lineout=''

   if(lgth == 0)return

   if (present(escape))then
      esc=escape
   else
      esc=char(92)
   endif

   EXP: do
      i=i+1
      if(i > lgth)exit
      if(line(i:i) == esc)then
         i=i+1
         if(i > lgth)exit
         if(line(i:i) /= esc)then
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
           !case('n','N');lineout=lineout//new_line('A')     ! %n     new line
            case('o','O')
                      thr=line(i+1:)
                   read(thr,'(o3)',iostat=ios)xxx
                      lineout=lineout//char(xxx)
                   i=i+3
            case('r','R');lineout=lineout//char( 13)         ! %r     carriage return
            case('t','T');lineout=lineout//c_horizontal_tab  ! %t     horizontal tab
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
         lineout=lineout//line(i:i)
      endif
      if(i >= lgth)exit EXP
   enddo EXP

end function expand
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    notabs(3f) - [M_strings:NONALPHA] expand tab characters
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine notabs(INSTR,OUTSTR,lgth)
!!
!!     character(len=*),intent=(in)  :: INSTR
!!     character(len=*),intent=(out) :: OUTSTR
!!     integer,intent=(out)          :: lgth
!!
!!##DESCRIPTION
!!     NOTABS() converts tabs in INSTR to spaces in OUTSTR while maintaining
!!     columns. It assumes a tab is set every 8 characters. Trailing spaces
!!     are removed.
!!
!!     In addition, trailing carriage returns and line feeds are removed
!!     (they are usually a problem created by going to and from MSWindows).
!!
!!     What are some reasons for removing tab characters from an input line?
!!     Some Fortran compilers have problems with tabs, as tabs are not
!!     part of the Fortran character set. Some editors and printers will
!!     have problems with tabs. It is often useful to expand tabs in input
!!     files to simplify further processing such as tokenizing an input line.
!!
!!##OPTIONS
!!     instr     Input line to remove tabs from
!!
!!##RESULTS
!!     outstr    Output string with tabs expanded. Assumed to be of sufficient
!!               length
!!     lgth      Significant length of returned string
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!    program demo_notabs
!!
!!    !  test filter to remove tabs and trailing white space from input
!!    !  on files up to 1024 characters wide
!!    use M_strings, only : notabs
!!    character(len=1024) :: in,out
!!    integer             :: ios,iout
!!       do
!!          read(*,'(A)',iostat=ios)in
!!          if(ios /= 0) exit
!!          call notabs(in,out,iout)
!!          write(*,'(a)')out(:iout)
!!       enddo
!!    end program demo_notabs
!!
!!##SEE ALSO
!!     GNU/Unix commands expand(1) and unexpand(1)
!!
!!##AUTHOR
!!     John S. Urban
!!
!!##LICENSE
!!    Public Domain
elemental impure subroutine notabs(instr,outstr,lgth)

! ident_32="@(#) M_strings notabs(3f) convert tabs to spaces while maintaining columns remove CRLF chars"

character(len=*),intent(in)   :: instr        ! input line to scan for tab characters
character(len=*),intent(out)  :: outstr       ! tab-expanded version of INSTR produced
integer,intent(out)           :: lgth         ! column position of last character put into output string
                                              ! that is, lgth holds the position of the last non-blank character in OUTSTR
!===================================================================================================================================
integer,parameter             :: tabsize=8    ! assume a tab stop is set every 8th column
integer                       :: ipos         ! position in OUTSTR to put next character of INSTR
integer                       :: lenin        ! length of input string trimmed of trailing spaces
integer                       :: lenout       ! number of characters output string can hold
integer                       :: istep        ! counter that advances thru input string INSTR one character at a time
character(len=1)              :: c            ! character in input line being processed
integer                       :: iade         ! ADE (ASCII Decimal Equivalent) of character being tested
!===================================================================================================================================
   ipos=1                                     ! where to put next character in output string OUTSTR
   lenin=len_trim(instr( 1:len(instr) ))      ! length of INSTR trimmed of trailing spaces
   lenout=len(outstr)                         ! number of characters output string OUTSTR can hold
   outstr=" "                                 ! this SHOULD blank-fill string, a buggy machine required a loop to set all characters
!===================================================================================================================================
      SCAN_LINE: do istep=1,lenin             ! look through input string one character at a time
         c=instr(istep:istep)                 ! get next character
         iade=iachar(c)                       ! get ADE of the character
         EXPAND_TABS : select case (iade)     ! take different actions depending on which character was found
         case(9)                              ! test if character is a tab and move pointer out to appropriate column
            ipos = ipos + (tabsize - (mod(ipos-1,tabsize)))
         case(10,13)                          ! convert carriage-return and new-line to space ,typically to handle DOS-format files
            ipos=ipos+1
         case default                         ! c is anything else other than a tab,newline,or return  insert it in output string
            if(ipos > lenout)then
               call journal("*notabs* output string overflow")
               exit
            else
               outstr(ipos:ipos)=c
               ipos=ipos+1
            endif
         end select EXPAND_TABS
      enddo SCAN_LINE
!===================================================================================================================================
      ipos=min(ipos,lenout)                   ! tabs or newline or return characters or last character might have gone too far
      lgth=len_trim(outstr(:ipos))            ! trim trailing spaces
!===================================================================================================================================
end subroutine notabs
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    dilate(3f) - [M_strings:NONALPHA] expand tab characters
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function dilate(INSTR) result(OUTSTR)
!!
!!     character(len=*),intent=(in)  :: INSTR
!!     character(len=:),allocatable  :: OUTSTR
!!
!!##DESCRIPTION
!!     dilate() converts tabs in INSTR to spaces in OUTSTR.  It assumes a
!!     tab is set every 8 characters. Trailing spaces are removed.
!!
!!     In addition, trailing carriage returns and line feeds are removed
!!     (they are usually a problem created by going to and from MSWindows).
!!
!!##OPTIONS
!!     instr     Input line to remove tabs from
!!
!!##RESULTS
!!     outstr    Output string with tabs expanded.
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!    program demo_dilate
!!
!!    !  test filter to remove tabs and trailing white space from input
!!    !  on files up to 1024 characters wide
!!    use M_strings, only : dilate
!!    implicit none
!!    character(len=:),allocatable :: in
!!    integer                      :: i
!!       in='  this is my string  '
!!       ! change spaces to tabs to make a sample input
!!       do i=1,len(in)
!!          if(in(i:i) == ' ')in(i:i)=char(9)
!!       enddo
!!       write(*,'(a)')in,dilate(in)
!!    end program demo_dilate
!!
!!##AUTHOR
!!     John S. Urban
!!
!!##LICENSE
!!    Public Domain
function dilate(INSTR) result(OUTSTR)

! ident_33="@(#) M_strings dilate(3f) convert tabs to spaces and trims line removing CRLF chars"

CHARACTER(LEN=*),INTENT(IN)   :: instr        ! input line to scan for tab characters
CHARACTER(LEN=:),allocatable  :: outstr       ! tab-expanded version of INSTR produced
integer                       :: i
integer                       :: icount
integer                       :: lgth
   icount=0
   do i=1,len(instr)
      if(instr(i:i) == char(9))icount=icount+1
   enddo
   allocate(character(len=(len(instr)+8*icount)) :: outstr)
   call notabs(instr,outstr,lgth)
   outstr=outstr(:lgth)
!===================================================================================================================================
END function dilate
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    adjustc(3f) - [M_strings:WHITESPACE] center text
!!    (LICENSE:PD)
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
!!
!!##OPTIONS
!!     string  input string to trim and center
!!     length  line length to center text in, optional.
!!
!!##RETURNS
!!     adjustc  centered output string
!!
!!##EXAMPLES
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
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
pure function adjustc(string,length)

! ident_34="@(#) M_strings adjustc(3f) center text"

!>
!! PROCEDURE   adjustc(3f)
!! DESCRIPTION center text using implicit or explicit length
!!##VERSION     2.0, 20160711
!! AUTHOR      John S. Urban
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=*),intent(in)  :: string         ! input string to trim and center
integer,intent(in),optional  :: length         ! line length to center text in
character(len=:),allocatable :: adjustc        ! output string
integer                      :: inlen
integer                      :: ileft          ! left edge of string if it is centered
!-----------------------------------------------------------------------------------------------------------------------------------
   if(present(length))then                     ! optional length
      inlen=length                             ! length will be requested length
      if(inlen <= 0)then                       ! bad input length
         inlen=len(string)                     ! could not use input value, fall back to length of input string
      endif
   else                                        ! output length was not explicitly specified, use input string length
      inlen=len(string)
   endif
   allocate(character(len=inlen):: adjustc)    ! create output at requested length
   adjustc(1:inlen)=' '                        ! initialize output string to all blanks
!-----------------------------------------------------------------------------------------------------------------------------------
   ileft =(inlen-len_trim(adjustl(string)))/2  ! find starting point to start input string to center it
   if(ileft > 0)then                          ! if string will fit centered in output
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
!!    nospace(3f) - [M_strings:WHITESPACE] remove all whitespace from
!!    input string
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function nospace(str) - remove all whitespace from input string
!!
!!     character(len=*),intent(in)          :: str
!!     character(len=:),allocatable         :: nospace
!!
!!##DESCRIPTION
!!    nospace(3f) removes space, tab, carriage return, new line, vertical
!!    tab, formfeed and null characters (called "whitespace"). The output
!!    is returned trimmed.
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!     program demo_nospace
!!     use M_strings, only: nospace
!!     implicit none
!!     character(len=:),allocatable  :: s
!!        s='  This     is      a     test  '
!!        write(*,*) 'original input string is ....',s
!!        write(*,*) 'processed output string is ...',nospace(s)
!!        if(nospace(s) == 'Thisisatest')then
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
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
function nospace(line)

! ident_35="@(#) M_strings nospace(3f) remove all whitespace from input string"

character(len=*),intent(in)    ::  line             ! remove whitespace from this string and return it
character(len=:),allocatable   ::  nospace          ! returned string
integer                        ::  ipos             ! position to place next output character at
integer                        ::  i                ! counter to increment from beginning to end of input string
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
!!    stretch(3f) - [M_strings:LENGTH] return string padded to at least
!!    specified length
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function stretch(str,length,pattern,suffix) result(strout)
!!
!!     character(len=*),intent(in)         :: str
!!     integer,intent(in)                  :: length
!!     character(len=*)intent(in),optional :: pattern
!!     character(len=*)intent(in),optional :: suffix
!!     character(len=:),allocatable        :: strout
!!
!!##DESCRIPTION
!!    stretch(3f) pads a string with spaces to at least the specified
!!    length. If the trimmed input string is longer than the requested
!!    length the original string is returned trimmed of trailing spaces.
!!
!!##OPTIONS
!!    str      the input string to return trimmed, but then padded to
!!             the specified length if shorter than length
!!    length   The minimum string length to return
!!    pattern  optional string to use as padding. Defaults to a space.
!!    suffix   optional string to append to output string
!!
!!##RETURNS
!!    strout  The input string padded to the requested length or
!!            the trimmed input string if the input string is
!!            longer than the requested length.
!!
!!##EXAMPLE
!!
!!  Sample Program:
!!
!!   program demo_stretch
!!    use M_strings, only : stretch
!!    implicit none
!!    character(len=10)            :: string='abcdefghij'
!!    character(len=:),allocatable :: answer
!!    integer                      :: i
!!       answer=stretch(string,5)
!!       write(*,'("[",a,"]")') answer
!!       answer=stretch(string,20)
!!       write(*,'("[",a,"]")') answer
!!       i=30
!!       write(*,*)
!!       write(*,'(1x,a,i0)') &
!!        & stretch('CHAPTER 1 : The beginning ',i,'.'), 1    ,&
!!        & stretch('CHAPTER 2 : The end ',i,'.'),       1234 ,&
!!        & stretch('APPENDIX ',i,'.'),                  1235
!!       write(*,*)
!!       write(*,'(1x,a,i7)') &
!!        & stretch('CHAPTER 1 : The beginning ',i,'.'), 1    ,&
!!        & stretch('CHAPTER 2 : The end ',i,'.'),       1234 ,&
!!        & stretch('APPENDIX ',i,'.'),                  1235
!!       write(*,*)
!!       write(*,*) &
!!        & stretch('CHAPTER 1 : The beginning ',i,suffix=': '), 1
!!       write(*,*) &
!!        & stretch('CHAPTER 2 : The end ',i,suffix=': '),1234
!!       write(*,*) &
!!        & stretch('APPENDIX ',i,suffix=': '),           1235
!!   end program demo_stretch
!!
!!   Results:
!!
!!    [abcdefghij]
!!    [abcdefghij          ]
!!
!!     CHAPTER 1 : The beginning ....1
!!     CHAPTER 2 : The end ..........1234
!!     APPENDIX .....................1235
!!
!!     CHAPTER 1 : The beginning ....      1
!!     CHAPTER 2 : The end ..........   1234
!!     APPENDIX .....................   1235
!!
!!     CHAPTER 1 : The beginning     :            1
!!     CHAPTER 2 : The end           :         1234
!!     APPENDIX                      :         1235
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
function stretch(line,length,pattern,suffix) result(strout)

! ident_36="@(#) M_strings stretch(3f) return string padded to at least specified length"

character(len=*),intent(in)                  :: line
integer,intent(in)                           :: length
character(len=*),intent(in),optional         :: pattern
character(len=*),intent(in),optional         :: suffix
!-!character(len=max(length,len(trim(line)))) :: strout
character(len=:),allocatable                 :: strout
   if(present(pattern))then
      strout=atleast(line,length,pattern)
   else
      strout=atleast(line,length)
   endif
   if(present(suffix))then
      strout=strout//suffix
   endif
end function stretch
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!   atleast(3f) - [M_strings:LENGTH] return string padded to at least
!!   specified length
!!   (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!
!!   function atleast(str,length,pattern) result(strout)
!!
!!    character(len=*)                           :: str
!!    integer,intent(in)                         :: length
!!    character(len=max(length,len(trim(line)))) ::  strout
!!    character(len=*),optional                  ::  pattern
!!
!!##DESCRIPTION
!!   atleast(3f) pads a string with spaces to at least the specified
!!   length. If the trimmed input string is longer than the requested
!!   length the trimmed string is returned.
!!
!!##OPTIONS
!!   str      the input string to return trimmed, but then padded to
!!            the specified length if shorter than length
!!   length   The minimum string length to return
!!   pattern  optional string to use as padding. Defaults to a space.
!!
!!##RETURNS
!!   strout  The input string padded to the requested length or
!!           the trimmed input string if the input string is
!!           longer than the requested length.
!!
!!##EXAMPLE
!!
!!  Sample Program:
!!
!!    program demo_atleast
!!     use M_strings, only : atleast
!!     implicit none
!!     character(len=10)            :: string='abcdefghij'
!!     character(len=:),allocatable :: answer
!!     integer                      :: i
!!        answer=atleast(string,5)
!!        write(*,'("[",a,"]")') answer
!!        answer=atleast(string,20)
!!        write(*,'("[",a,"]")') answer
!!        i=30
!!        write(*,*)
!!        write(*,'(1x,a,1x,i0)') &
!!         & atleast('CHAPTER 1 : The beginning ',i,'.'), 1   , &
!!         & atleast('CHAPTER 2 : The end ',i,'.'),       1234, &
!!         & atleast('APPENDIX ',i,'.'),                  1235
!!        write(*,*)
!!        write(*,'(1x,a,i7)') &
!!         & atleast('CHAPTER 1 : The beginning ',i,'.'), 1   , &
!!         & atleast('CHAPTER 2 : The end ',i,'.'),       1234, &
!!         & atleast('APPENDIX ',i,'.'),                  1235
!!    end program demo_atleast
!!
!!  Results:
!!
!!    [abcdefghij]
!!    [abcdefghij          ]
!!
!!     CHAPTER 1 : The beginning .... 1
!!     CHAPTER 2 : The end .......... 1234
!!     APPENDIX ..................... 1235
!!
!!     CHAPTER 1 : The beginning ....      1
!!     CHAPTER 2 : The end ..........   1234
!!     APPENDIX .....................   1235
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
!===================================================================================================================================
function atleast(line,length,pattern) result(strout)

! ident_37="@(#) M_strings atleast(3f) return string padded to at least specified length"

character(len=*),intent(in)                :: line
integer,intent(in)                         :: length
character(len=*),intent(in),optional       :: pattern
character(len=max(length,len(trim(line)))) :: strout
if(present(pattern))then
   strout=line//repeat(pattern,len(strout)/len(pattern)+1)
else
   strout=line
endif
end function atleast
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    lenset(3f) - [M_strings:LENGTH] return string trimmed or padded to
!!                 specified length
!!    (LICENSE:PD)
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
!!##OPTIONS
!!    str     input string
!!    length  output string length
!!
!!##RESULTS
!!    strout  output string
!!
!!##EXAMPLE
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
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
function lenset(line,length) result(strout)

! ident_38="@(#) M_strings lenset(3f) return string trimmed or padded to specified length"

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
!!    merge_str(3f) - [M_strings:LENGTH] pads strings to same length and
!!    then calls MERGE(3f)
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function merge_str(str1,str2,expr) result(strout)
!!
!!     character(len=*),intent(in),optional :: str1
!!     character(len=*),intent(in),optional :: str2
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
!!    NOTE: STR1 and STR2 are always required even though declared optional.
!!          this is so the call "STR_MERGE(A,B,present(A))" is a valid call.
!!          The parameters STR1 and STR2 when they are optional parameters
!!          can be passed to a procedure if the options are optional on the
!!          called procedure.
!!
!!##OPTIONS
!!    STR1    string to return if the logical expression EXPR is true
!!    STR2    string to return if the logical expression EXPR is false
!!    EXPR    logical expression to evaluate to determine whether to return
!!            STR1 when true, and STR2 when false.
!!##RESULT
!!     MERGE_STR  a trimmed string is returned that is otherwise the value
!!                of STR1 or STR2, depending on the logical expression EXPR.
!!
!!##EXAMPLES
!!
!!   Sample Program:
!!
!!     program demo_merge_str
!!     use M_strings, only : merge_str
!!     implicit none
!!     character(len=:), allocatable :: answer
!!        answer=merge_str('first string', &
!!         & 'second string is longer',10 == 10)
!!        write(*,'("[",a,"]")') answer
!!        answer=merge_str('first string', &
!!         & 'second string is longer',10 /= 10)
!!        write(*,'("[",a,"]")') answer
!!     end program demo_merge_str
!!
!!   Expected output
!!
!!     [first string]
!!     [second string is longer]
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
function merge_str(str1,str2,expr) result(strout)
! for some reason the MERGE(3f) intrinsic requires the strings it compares to be of equal length
! make an alias for MERGE(3f) that makes the lengths the same before doing the comparison by padding the shorter one with spaces

! ident_39="@(#) M_strings merge_str(3f) pads first and second arguments to MERGE(3f) to same length"

character(len=*),intent(in),optional :: str1
character(len=*),intent(in),optional :: str2
character(len=:),allocatable         :: str1_local
character(len=:),allocatable         :: str2_local
logical,intent(in)                   :: expr
character(len=:),allocatable         :: strout
integer                              :: big
   if(present(str2))then
      str2_local=str2
   else
      str2_local=''
   endif
   if(present(str1))then
      str1_local=str1
   else
      str1_local=''
   endif
   big=max(len(str1_local),len(str2_local))
   ! note: perhaps it would be better to warn or fail if an optional value that is not present is returned, instead of returning ''
   strout=trim(merge(lenset(str1_local,big),lenset(str2_local,big),expr))
end function merge_str
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    squeeze(3f) - [M_strings:EDITING] delete adjacent duplicate occurrences
!!    of a character from a string
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function squeeze(STR,CHAR) result (OUTSTR)
!!
!!     character(len=*),intent(in)          :: STR
!!     character(len=*),intent(in),optional :: CHAR
!!     character(len=len(str))              :: OUTSTR
!!
!!##DESCRIPTION
!!    squeeze(3f) reduces adjacent duplicates of the specified character to
!!    a single character
!!
!!##OPTIONS
!!    STR     input string in which to reduce adjacent duplicate characters
!!            to a single character
!!    CHAR    The character to remove adjacent duplicates of
!!
!!##RETURNS
!!    OUTSTR  string with all contiguous adjacent occurrences of CHAR removed
!!
!!##EXAMPLES
!!
!!   Sample Program:
!!
!!    program demo_squeeze
!!    use M_strings, only : squeeze
!!    implicit none
!!    character(len=:),allocatable :: strings(:)
!!
!!    strings=[ character(len=72) :: &
!!    '', &
!!    '"If I were two-faced, would I be wearing this one?" --- Abraham Lincoln',  &
!!    '..1111111111111111111111111111111111111111111111111111111111111117777888', &
!!    'I never give ''em hell, I just tell the truth, and they think it''s hell.',&
!!    '                                                    --- Harry S Truman'    &
!!    ]
!!       call printme( trim(strings(1)), ' ' )
!!       call printme( strings(2:4),     ['-','7','.'] )
!!       call printme( strings(5),       [' ','-','r'] )
!!    contains
!!    impure elemental subroutine printme(str,chr)
!!    character(len=*),intent(in) :: str
!!    character(len=1),intent(in) :: chr
!!    character(len=:),allocatable :: answer
!!       write(*,'(a)')repeat('=',11)
!!       write(*,'("IN:   <<<",g0,">>>")')str
!!       answer=squeeze(str,chr)
!!       write(*,'("OUT:  <<<",g0,">>>")')answer
!!       write(*,'("LENS: ",*(g0,1x))')"from",len(str),"to",len(answer), &
!!               & "for a change of",len(str)-len(answer)
!!       write(*,'("CHAR: ",g0)')chr
!!    end subroutine printme
!!    end program demo_squeeze
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
function squeeze(str,charp) result (outstr)

character(len=*),intent(in)  :: str
character(len=1),intent(in)  :: charp
character(len=:),allocatable :: outstr
character(len=1)             :: ch, last_one
integer                      :: i, pio ! position in output

   outstr=repeat(' ',len(str))      ! start with a string big enough to hold any output
   if(len(outstr)==0)return         ! handle edge condition
   last_one=str(1:1)                ! since at least this long start output with first character
   outstr(1:1)=last_one
   pio=1

   do i=2,len(str)
      ch=str(i:i)
      pio=pio+merge(0,1, ch == last_one.and.ch == charp) ! decide whether to advance before saving
      outstr(pio:pio)=ch  ! store new one or overlay the duplcation
      last_one=ch
   enddo

   outstr=outstr(:pio)              ! trim the output string to just what was set
end function squeeze
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    compact(3f) - [M_strings:WHITESPACE] converts contiguous whitespace
!!    to a single character (or nothing)
!!    (LICENSE:PD)
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
!!
!!##RETURNS
!!    OUTSTR  string of same length as input string but with all contiguous
!!            whitespace reduced to a single space and leading whitespace
!!            removed
!!
!!##EXAMPLES
!!
!!   Sample Program:
!!
!!    program demo_compact
!!     use M_strings, only : compact
!!     implicit none
!!     ! produces 'This is a test               '
!!     write(*,*)compact('  This     is      a     test  ')
!!     ! produces 'Thisisatest                  '
!!     write(*,*)compact('  This     is      a     test  ',char='')
!!     ! produces 'This:is:a:test               '
!!     write(*,*)compact('  This     is      a     test  ',char=':')
!!     ! note CHAR is used to replace the whitespace, but if CHAR is
!!     ! in the original string it is just copied
!!     write(*,*)compact('A  AA    A   AAAAA',char='A')
!!     ! produces (original A characters are left as-is) 'AAAAAAAAAAAA'
!!     ! not 'A'
!!    end program demo_compact
!!
!!    Expected output
!!
!!     >This is a test
!!     >Thisisatest
!!     >This:is:a:test
!!     >AAAAAAAAAAAA
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
!elemental pure function compact(str,char) result (outstr)
function compact(str,char) result (outstr)

! ident_40="@(#) M_strings compact(3f) Converts white-space to single spaces; removes leading spaces"

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
   if(len(char) == 0)then
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
     select case(iachar(ch))
       case(0:32,127)                                         ! space or tab character or control character
         if(position_in_output == 0)then                      ! still at beginning so ignore leading whitespace
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
   enddo IFSPACE

end function compact
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!     noesc(3f) - [M_strings:NONALPHA] convert non-printable characters
!!     to a space
!!     (LICENSE:PD)
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
!!   Sample Program:
!!
!!    program demo_noesc
!!
!!     use M_strings, only : noesc
!!     implicit none
!!     character(len=128) :: ascii
!!     character(len=128) :: cleared
!!     integer            :: i
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
!!       integer :: lgth
!!       ! counter used to step thru string
!!       integer :: i
!!          ! get trimmed length of input string
!!          lgth=len_trim(string(:len(string)))
!!
!!          ! replace lower unprintable characters with spaces
!!          write(*,101)(merge(string(i:i),' ',&
!!          & iachar(string(i:i)) >= 32        &
!!          & .and.                            &
!!          & iachar(string(i:i)) <= 126)      &
!!          & ,i=1,lgth)
!!
!!          ! print ADE value of character underneath it
!!          write(*,202)     (iachar(string(i:i))/100,    i=1,lgth)
!!          write(*,202)(mod( iachar(string(i:i)),100)/10,i=1,lgth)
!!          write(*,202)(mod((iachar(string(i:i))),10),   i=1,lgth)
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
!!    >                                 !"#$%&'()*+,-./0123456789
!!    :;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmnopqrstuvwxyz{|}~
!!    >0000000000000000000000000000000000000000000000000000000000
!!    0000000000000000000000000000000000000000001111111111111111111111111111
!!    >00000000001111111111222222222233333333334444444444555555555566666666
!!    667777777777888888888899999999990000000000111111111122222222
!!    >012345678901234567890123456789012345678901234567890123456789012345678
!!    90123456789012345678901234567890123456789012345678901234567
!!
!!   Cleared of non-printable characters
!!
!!    >                                 !"#$%&'()*+,-./0123456789
!!    :;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmnopqrstuvwxyz{|}~
!!    >0000000000000000000000000000000000000000000000000000000000
!!    000000000000000000000000000000000000000000111111111111111111111111111
!!    >3333333333333333333333333333333333333333444444444455555555
!!    556666666666777777777788888888889999999999000000000011111111112222222
!!    >2222222222222222222222222222222223456789012345678901234567
!!    890123456789012345678901234567890123456789012345678901234567890123456
!!
!!   Cleared string:
!!
!!    >                                  !"#$%&'()*+,-./0123456789:;<=>?@
!!    ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmnopqrstuvwxyz{|}~
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
elemental function noesc(INSTR)

! ident_41="@(#) M_strings noesc(3f) convert non-printable characters to a space"

character(len=*),intent(in) :: INSTR      ! string that might contain nonprintable characters
character(len=len(instr))   :: noesc
integer                     :: ic,i10
!-----------------------------------------------------------------------------------------------------------------------------------
   noesc=''                               ! initialize output string
   do i10=1,len_trim(INSTR(1:len(INSTR)))
      ic=iachar(INSTR(i10:i10))
      if(ic <= 31.or.ic == 127)then       ! find characters with ADE of 0-31, 127
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
!!      string_to_value(3f) - [M_strings:NUMERIC] subroutine returns numeric
!!      value from string
!!      (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine string_to_value(chars,valu,ierr)
!!
!!     character(len=*),intent(in)              :: chars   ! input string
!!     integer|real|doubleprecision,intent(out) :: valu
!!     integer,intent(out)                      :: ierr
!!
!!##DESCRIPTION
!!    Returns a numeric value from a numeric character string.
!!
!!    Works with any g-format input, including integer, real, and
!!    exponential. If the input string begins with "B", "Z", or "O"
!!    and otherwise represents a positive whole number it is assumed to
!!    be a binary, hexadecimal, or octal value. If the string contains
!!    commas they are removed. If the string is of the form NN:MMM... or
!!    NN#MMM then NN is assumed to be the base of the whole number.
!!
!!    If an error occurs in the READ, IOSTAT is returned in IERR and
!!    value is set to zero. if no error occurs, IERR=0.
!!
!!##OPTIONS
!!       CHARS  input string to read numeric value from
!!
!!##RETURNS
!!    VALU   numeric value returned. May be INTEGER, REAL, or
!!              DOUBLEPRECISION.
!!    IERR   error flag (0 == no error)
!!
!!##EXAMPLE
!!
!!   Sample Program:
!!
!!    program demo_string_to_value
!!     use M_strings, only: string_to_value
!!     implicit none
!!     real :: value
!!     integer :: ierr
!!     character(len=80) :: string
!!        string=' -40.5e-2 '
!!        call string_to_value(string,value,ierr)
!!        write(*,*) 'value of string ['//trim(string)//'] is ',value
!!    end program demo_string_to_value
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
subroutine a2r(chars,valu,ierr)

! ident_42="@(#) M_strings a2r(3fp) subroutine returns real value from string"

character(len=*),intent(in) :: chars                      ! input string
real,intent(out)            :: valu                       ! value read from input string
integer,intent(out)         :: ierr                       ! error flag (0 == no error)
doubleprecision             :: valu8
   valu8=0.0d0
   call a2d(chars,valu8,ierr,onerr=0.0d0)
   if(ierr == 0)then
      if(valu8 <= huge(valu))then
         valu=real(valu8)
      else
         call journal('sc','*a2r*','- value too large',valu8,'>',huge(valu))
         valu=huge(valu)
         ierr=-1
      endif
   endif
end subroutine a2r
!----------------------------------------------------------------------------------------------------------------------------------
subroutine a2i(chars,valu,ierr)

! ident_43="@(#) M_strings a2i(3fp) subroutine returns integer value from string"

character(len=*),intent(in) :: chars                      ! input string
integer,intent(out)         :: valu                       ! value read from input string
integer,intent(out)         :: ierr                       ! error flag (0 == no error)
doubleprecision             :: valu8
   valu8=0.0d0
   call a2d(chars,valu8,ierr,onerr=0.0d0)
   if(valu8 <= huge(valu))then
      if(valu8 <= huge(valu))then
         valu=int(valu8)
      else
         call journal('sc','*a2i*','- value too large',valu8,'>',huge(valu))
         valu=huge(valu)
         ierr=-1
      endif
   endif
end subroutine a2i
!----------------------------------------------------------------------------------------------------------------------------------
subroutine a2d(chars,valu,ierr,onerr)

! ident_44="@(#) M_strings a2d(3fp) subroutine returns double value from string"

!     1989,2016 John S. Urban.
!
!  o works with any g-format input, including integer, real, and exponential.
!  o if an error occurs in the read, iostat is returned in ierr and value is set to zero. If no error occurs, ierr=0.
!  o if the string happens to be 'eod' no error message is produced so this string may be used to act as an end-of-data.
!    IERR will still be non-zero in this case.
!----------------------------------------------------------------------------------------------------------------------------------
character(len=*),intent(in)  :: chars                        ! input string
character(len=:),allocatable :: local_chars
doubleprecision,intent(out)  :: valu                         ! value read from input string
integer,intent(out)          :: ierr                         ! error flag (0 == no error)
class(*),optional,intent(in) :: onerr
!----------------------------------------------------------------------------------------------------------------------------------
character(len=*),parameter   :: fmt="('(bn,g',i5,'.0)')"     ! format used to build frmt
character(len=15)            :: frmt                         ! holds format built to read input string
character(len=256)           :: msg                          ! hold message from I/O errors
integer                      :: intg
integer                      :: pnd
integer                      :: basevalue, ivalu
character(len=3),save        :: nan_string='NaN'
!----------------------------------------------------------------------------------------------------------------------------------
   ierr=0                                                       ! initialize error flag to zero
   local_chars=unquote(chars)
   msg=''
   if(len(local_chars) == 0)local_chars=' '
   call substitute(local_chars,',','')                          ! remove any comma characters
   pnd=scan(local_chars,'#:')
   if(pnd /= 0)then
      write(frmt,fmt)pnd-1                                      ! build format of form '(BN,Gn.0)'
      read(local_chars(:pnd-1),fmt=frmt,iostat=ierr,iomsg=msg)basevalue   ! try to read value from string
      if(decodebase(local_chars(pnd+1:),basevalue,ivalu))then
         valu=real(ivalu,kind=kind(0.0d0))
      else
         valu=0.0d0
         ierr=-1
      endif
   else
      select case(local_chars(1:1))
      case('z','Z','h','H')                                     ! assume hexadecimal
         frmt='(Z'//v2s(len(local_chars))//')'
         read(local_chars(2:),frmt,iostat=ierr,iomsg=msg)intg
         valu=dble(intg)
      case('b','B')                                             ! assume binary (base 2)
         frmt='(B'//v2s(len(local_chars))//')'
         read(local_chars(2:),frmt,iostat=ierr,iomsg=msg)intg
         valu=dble(intg)
      case('o','O')                                             ! assume octal
         frmt='(O'//v2s(len(local_chars))//')'
         read(local_chars(2:),frmt,iostat=ierr,iomsg=msg)intg
         valu=dble(intg)
      case default
         write(frmt,fmt)len(local_chars)                        ! build format of form '(BN,Gn.0)'
         read(local_chars,fmt=frmt,iostat=ierr,iomsg=msg)valu   ! try to read value from string
      end select
   endif
   if(ierr /= 0)then                                            ! if an error occurred ierr will be non-zero.
      if(present(onerr))then
         select type(onerr)
         type is (integer)
            valu=onerr
         type is (real)
            valu=onerr
         type is (doubleprecision)
            valu=onerr
         end select
      else                                                      ! set return value to NaN
         read(nan_string,'(g3.3)')valu
      endif
      if(local_chars /= 'eod')then                           ! print warning message except for special value "eod"
         call journal('sc','*a2d* - cannot produce number from string ['//trim(chars)//']')
         if(msg /= '')then
            call journal('sc','*a2d* - ['//trim(msg)//']')
         endif
      endif
   endif
end subroutine a2d
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    s2v(3f) - [M_strings:NUMERIC] function returns doubleprecision
!!    numeric value from a string
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function s2v(string[,ierr][,onerr])
!!
!!     character(len=*)             :: string
!!     doubleprecision              :: s2v
!!     integer,intent(out),optional :: ierr
!!     class(*),intent(in),optional :: onerr
!!
!!##DESCRIPTION
!!    This function converts a string to a DOUBLEPRECISION numeric value.
!!
!!    The intrinsics INT(3f), REAL(3f), and DBLE(3f) are also extended
!!    to take CHARACTER variables. The KIND= keyword is not supported
!!    on the extensions.
!!
!!##OPTIONS
!!
!!     string   holds string assumed to represent a numeric value
!!     ierr     If an error occurs the program is stopped if the optional
!!              parameter IERR is not present. If IERR returns a non-zero
!!              value an error occurred.
!!     onerr    The value to return on error. A value of NaN is
!!              returned on error by default.
!!
!!##RETURNS
!!     s2v      numeric value read from string
!!
!!##EXAMPLE
!!
!!   Sample Program:
!!
!!    program demo_s2v
!!
!!     use M_strings, only: s2v, int, real, dble
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
!!     ! a numeric value is returned,
!!     ! so it can be used in numeric expression
!!     write(*,*) '1/2 value of string is ',s2v(s)/2.0d0
!!     write(*,*)
!!     write(*,*)' STRING            VALUE                    ERROR_NUMBER'
!!     do i=1,size(strings)
!!        ! Note: not a good idea to use s2v(3f) in a WRITE(3f) statement,
!!        ! as it does I/O when errors occur, so called on a separate line
!!        dv=s2v(strings(i),errnum)
!!        write(*,*) strings(i)//'=',dv,errnum
!!     enddo
!!     write(*,*)"Extended intrinsics"
!!     write(*,*)'given inputs:',s,strings(:8)
!!     write(*,*)'INT(3f):',int(s),int(strings(:8))
!!     write(*,*)'REAL(3f):',real(s),real(strings(:8))
!!     write(*,*)'DBLE(3f):',dble(s),dble(strings(:8))
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
!!     >Extended intrinsics
!!     >given inputs: 10.345 10.345 +10 -3 -4.94e-2 0.1
!!     12345.678910d0 1 2 1 2 1 . 0
!!     >INT(3f): 10 10 10 -3 0 0 12345 0 12121
!!     >REAL(3f): 10.3450003 10.3450003 10.0000000 -3.00000000
!!     -4.94000018E-02
!!     >          0.100000001 12345.6787 0.00000000 12121.0000
!!     >DBLE(3f): 10.345000000000001 10.345000000000001
!!     10.000000000000000
!!     >          -3.0000000000000000 -4.9399999999999999E-002
!!     0.10000000000000001
!!     >          12345.678910000001 0.0000000000000000
!!     12121.000000000000
!!     >That's all folks!
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
!>
!!##PROCEDURE:
!! DESCRIPTION: s2v(3f): function returns doubleprecision number from string;zero if error occurs
!!##VERSION:     2.0, 20160704
!! AUTHOR:      John S. Urban
doubleprecision function s2v(chars,ierr,onerr)
!  1989 John S. Urban

! ident_45="@(#) M_strings s2v(3f) returns doubleprecision number from string;zero if error occurs"

character(len=*),intent(in)  :: chars
integer,optional             :: ierr
doubleprecision              :: valu
integer                      :: ierr_local
class(*),intent(in),optional :: onerr

   ierr_local=0
   if(present(onerr))then
      call a2d(chars,valu,ierr_local,onerr)
   else
      call a2d(chars,valu,ierr_local)
   endif
   if(present(ierr))then ! if error is not returned stop program on error
      ierr=ierr_local
      s2v=valu
   elseif(ierr_local /= 0)then
      write(*,*)'*s2v* stopped while reading '//trim(chars)
      stop 1
   else
      s2v=valu
   endif
end function s2v
!===================================================================================================================================
! calls to s2v(3f) for extending intrinsics int(3f), real(3f), dble(3f)
!===================================================================================================================================
doubleprecision function dble_s2v(chars)
character(len=*),intent(in) :: chars
   dble_s2v=s2v(chars)
end function dble_s2v
!===================================================================================================================================
real function real_s2v(chars)
character(len=*),intent(in) :: chars
   real_s2v=real(s2v(chars))
end function real_s2v
!===================================================================================================================================
integer function int_s2v(chars)
character(len=*),intent(in) :: chars
   int_s2v=int(s2v(chars))
end function int_s2v
!===================================================================================================================================
function ints_s2v(chars)
integer,allocatable         :: ints_s2v(:)
character(len=*),intent(in) :: chars(:)
integer                     :: i,isize
   isize=size(chars)
   allocate(ints_s2v(isize))
   do i=1,isize
      ints_s2v(i)=int(s2v(chars(i)))
   enddo
end function ints_s2v
!===================================================================================================================================
function reals_s2v(chars)
real,allocatable            :: reals_s2v(:)
character(len=*),intent(in) :: chars(:)
integer                     :: i,isize
   isize=size(chars)
   allocate(reals_s2v(isize))
   do i=1,isize
      reals_s2v(i)=real(s2v(chars(i)))
   enddo
end function reals_s2v
!===================================================================================================================================
function dbles_s2v(chars)
doubleprecision,allocatable :: dbles_s2v(:)
character(len=*),intent(in) :: chars(:)
integer                     :: i,isize
   isize=size(chars)
   allocate(dbles_s2v(isize))
   do i=1,isize
      dbles_s2v(i)=s2v(chars(i))
   enddo
end function dbles_s2v
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()())()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!      value_to_string(3f) - [M_strings:NUMERIC] return numeric string
!!      from a numeric value
!!      (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine value_to_string(value,chars[,lgth,ierr,fmt,trimz])
!!
!!     character(len=*) :: chars  ! minimum of 23 characters required
!!     !--------
!!     ! VALUE may be any <em>one</em> of the following types:
!!     doubleprecision,intent(in)               :: value
!!     real,intent(in)                          :: value
!!     integer,intent(in)                       :: value
!!     logical,intent(in)                       :: value
!!     !--------
!!     character(len=*),intent(out)             :: chars
!!     integer,intent(out),optional             :: lgth
!!     integer,optional                         :: ierr
!!     character(len=*),intent(in),optional     :: fmt
!!     logical,intent(in)                       :: trimz
!!
!!##DESCRIPTION
!!    value_to_string(3f) returns a numeric representation of a numeric
!!    value in a string given a numeric value of type REAL, DOUBLEPRECISION,
!!    INTEGER or LOGICAL. It creates the string using internal writes. It
!!    then removes trailing zeros from non-zero values, and left-justifies
!!    the string.
!!
!!##OPTIONS
!!       VALUE   input value to be converted to a string
!!       FMT     You may specify a specific format that produces a string
!!               up to the length of CHARS; optional.
!!       TRIMZ   If a format is supplied the default is not to try to trim
!!               trailing zeros. Set TRIMZ to .true. to trim zeros from a
!!               string assumed to represent a simple numeric value.
!!
!!##RETURNS
!!       CHARS   returned string representing input value, must be at least
!!               23 characters long; or what is required by optional FMT
!!               if longer.
!!       LGTH    position of last non-blank character in returned string;
!!               optional.
!!       IERR    If not zero, error occurred; optional.
!!
!!##EXAMPLE
!!
!!    Sample program:
!!
!!      program demo_value_to_string
!!      use M_strings, only: value_to_string
!!      implicit none
!!      character(len=80) :: string
!!      integer           :: lgth
!!         call value_to_string(3.0/4.0,string,lgth)
!!         write(*,*) 'The value is [',string(:lgth),']'
!!
!!         call value_to_string(3.0/4.0,string,lgth,fmt='')
!!         write(*,*) 'The value is [',string(:lgth),']'
!!
!!         call value_to_string&
!!         &(3.0/4.0,string,lgth,fmt='("THE VALUE IS ",g0)')
!!         write(*,*) 'The value is [',string(:lgth),']'
!!
!!         call value_to_string(1234,string,lgth)
!!         write(*,*) 'The value is [',string(:lgth),']'
!!
!!         call value_to_string(1.0d0/3.0d0,string,lgth)
!!         write(*,*) 'The value is [',string(:lgth),']'
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
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
subroutine value_to_string(gval,chars,length,err,fmt,trimz)

! ident_46="@(#) M_strings value_to_string(3fp) subroutine returns a string from a value"

class(*),intent(in)                      :: gval
character(len=*),intent(out)             :: chars
integer,intent(out),optional             :: length
integer,optional                         :: err
integer                                  :: err_local
character(len=*),optional,intent(in)     :: fmt         ! format to write value with
logical,intent(in),optional              :: trimz
character(len=:),allocatable             :: fmt_local
character(len=1024)                      :: msg

!  Notice that the value GVAL can be any of several types ( INTEGER,REAL,DOUBLEPRECISION,LOGICAL)

   if (present(fmt)) then
      select type(gval)
      type is (integer)
         fmt_local='(i0)'
         if(fmt /= '') fmt_local=fmt
         write(chars,fmt_local,iostat=err_local,iomsg=msg)gval
      type is (real)
         fmt_local='(bz,g23.10e3)'
         fmt_local='(bz,g0.8)'
         if(fmt /= '') fmt_local=fmt
         write(chars,fmt_local,iostat=err_local,iomsg=msg)gval
      type is (doubleprecision)
         fmt_local='(bz,g0)'
         if(fmt /= '') fmt_local=fmt
         write(chars,fmt_local,iostat=err_local,iomsg=msg)gval
      type is (logical)
         fmt_local='(l1)'
         if(fmt /= '') fmt_local=fmt
         write(chars,fmt_local,iostat=err_local,iomsg=msg)gval
      class default
         call journal('*value_to_string* UNKNOWN TYPE')
         chars=' '
      end select
      if(fmt == '') then
         chars=adjustl(chars)
         call trimzeros_(chars)
      endif
   else                                                  ! no explicit format option present
      err_local=-1
      select type(gval)
      type is (integer)
         write(chars,*,iostat=err_local,iomsg=msg)gval
      type is (real)
         write(chars,*,iostat=err_local,iomsg=msg)gval
      type is (doubleprecision)
         write(chars,*,iostat=err_local,iomsg=msg)gval
      type is (logical)
         write(chars,*,iostat=err_local,iomsg=msg)gval
      class default
         chars=''
      end select
      chars=adjustl(chars)
      if(index(chars,'.') /= 0) call trimzeros_(chars)
   endif
   if(present(trimz))then
      if(trimz)then
         chars=adjustl(chars)
         call trimzeros_(chars)
      endif
   endif

   if(present(length)) then
      length=len_trim(chars)
   endif

   if(present(err)) then
      err=err_local
   elseif(err_local /= 0)then
       ! cannot currently do I/O from a function being called from I/O
       !write(ERROR_UNIT,'(a)')'*value_to_string* WARNING:['//trim(msg)//']'
      chars=chars//' *value_to_string* WARNING:['//trim(msg)//']'
   endif

end subroutine value_to_string
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!      v2s(3f) - [M_strings:NUMERIC] return numeric string from a numeric value
!!      (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!       function v2s(value) result(outstr)
!!
!!        integer|real|doubleprecision|logical,intent(in ) :: value
!!        character(len=:),allocatable :: outstr
!!        character(len=*),optional,intent(in) :: fmt
!!
!!##DESCRIPTION
!!    v2s(3f) returns a representation of a numeric value as a
!!    string when given a numeric value of type REAL, DOUBLEPRECISION,
!!    INTEGER or LOGICAL. It creates the strings using internal WRITE()
!!    statements. Trailing zeros are removed from non-zero values, and the
!!    string is left-justified.
!!
!!##OPTIONS
!!    VALUE   input value to be converted to a string
!!    FMT     format can be explicitly given, but is limited to
!!            generating a string of eighty or less characters.
!!
!!##RETURNS
!!    OUTSTR  returned string representing input value,
!!
!!##EXAMPLE
!!
!!   Sample Program:
!!
!!    program demo_v2s
!!    use M_strings, only: v2s
!!    write(*,*) 'The value of 3.0/4.0 is ['//v2s(3.0/4.0)//']'
!!    write(*,*) 'The value of 1234    is ['//v2s(1234)//']'
!!    write(*,*) 'The value of 0d0     is ['//v2s(0d0)//']'
!!    write(*,*) 'The value of .false. is ['//v2s(.false.)//']'
!!    write(*,*) 'The value of .true. is  ['//v2s(.true.)//']'
!!    end program demo_v2s
!!
!!   Expected output
!!
!!     The value of 3.0/4.0 is [0.75]
!!     The value of 1234    is [1234]
!!     The value of 0d0     is [0]
!!     The value of .false. is [F]
!!     The value of .true. is  [T]
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
! very odd compiler problems in many (but not all) programs using this routine; GNU Fortran (GCC) 5.4.0; 20161030
function v2s_bug(gval) result(outstr)

! ident_47="@(#) M_strings v2s_bug(3f) function returns string given numeric value"

class(*),intent(in)          :: gval                         ! input value to convert to a string
character(len=:),allocatable :: outstr                       ! output string to generate
character(len=80)            :: string
   call value_to_string(gval,string)
   outstr=trim(string)
end function v2s_bug
!===================================================================================================================================
function d2s(dvalue,fmt) result(outstr)

! ident_48="@(#) M_strings d2s(3fp) private function returns string given doubleprecision value"

doubleprecision,intent(in)   :: dvalue                         ! input value to convert to a string
character(len=*),intent(in),optional :: fmt
character(len=:),allocatable :: outstr                         ! output string to generate
character(len=80)            :: string
   if(present(fmt))then
      call value_to_string(dvalue,string,fmt=fmt)
   else
      call value_to_string(dvalue,string)
   endif
   outstr=trim(string)
end function d2s
!===================================================================================================================================
function r2s(rvalue,fmt) result(outstr)

! ident_49="@(#) M_strings r2s(3fp) private function returns string given real value"

real,intent(in)              :: rvalue                         ! input value to convert to a string
character(len=*),intent(in),optional :: fmt
character(len=:),allocatable :: outstr                         ! output string to generate
character(len=80)            :: string
   if(present(fmt))then
      call value_to_string(rvalue,string,fmt=fmt)
   else
      call value_to_string(rvalue,string)
   endif
   outstr=trim(string)
end function r2s
!===================================================================================================================================
function i2s(ivalue,fmt) result(outstr)

! ident_50="@(#) M_strings i2s(3fp) private function returns string given integer value"

integer,intent(in)           :: ivalue                         ! input value to convert to a string
character(len=*),intent(in),optional :: fmt
character(len=:),allocatable :: outstr                         ! output string to generate
character(len=80)            :: string
   if(present(fmt))then
      call value_to_string(ivalue,string,fmt=fmt)
   else
      call value_to_string(ivalue,string)
   endif
   outstr=trim(string)
end function i2s
!===================================================================================================================================
function l2s(lvalue,fmt) result(outstr)

! ident_51="@(#) M_strings l2s(3fp) private function returns string given logical value"

logical,intent(in)           :: lvalue                         ! input value to convert to a string
character(len=*),intent(in),optional :: fmt
character(len=:),allocatable :: outstr                         ! output string to generate
character(len=80)             :: string
   if(present(fmt))then
      call value_to_string(lvalue,string,fmt=fmt)
   else
      call value_to_string(lvalue,string)
   endif
   outstr=trim(string)
end function l2s
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    isnumber(3f) - [M_strings:NUMERIC] determine if a string represents a number
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function isnumber(str,msg)
!!
!!     character(len=*),intent(in)  :: str
!!     character(len=:),intent(out),allocatable,optional  :: msg
!!
!!##DESCRIPTION
!!     ISNUMBER(3f) returns a value greater than zero if the string represents
!!     a number, and a number less than or equal to zero if it is a bad number.
!!     Blank characters are ignored.
!!
!!##OPTIONS
!!     str  the string to evaluate as to whether it represents a numeric value
!!          or not
!!     msg  An optional message describing the string
!!
!!##RETURNS
!!     isnumber  the following values are returned
!!
!!                1 for an integer             [-+]NNNNN
!!                2 for a whole number         [-+]NNNNN.
!!                3 for a real value           [-+]NNNNN.MMMM
!!                4 for a exponential value    [-+]NNNNN.MMMM[-+]LLLL
!!                                             [-+]NNNNN.MMMM[ed][-+]LLLL
!!
!!               values less than 1 represent an error
!!
!!##EXAMPLES
!!
!!   As the example shows, you can use an internal READ(3f) along with the
!!   IOSTAT= parameter to check (and read) a string as well.
!!
!!     program demo_isnumber
!!     use M_strings, only : isnumber
!!     implicit none
!!     character(len=256) :: line
!!     real               :: value
!!     integer            :: ios1, ios2
!!     integer            :: answer
!!     character(len=256) :: message
!!     character(len=:),allocatable :: description
!!        write(*,*)'Begin entering values, one per line'
!!        do
!!           read(*,'(a)',iostat=ios1)line
!!           !
!!           ! try string as number using list-directed input
!!           line=''
!!           read(line,*,iostat=ios2,iomsg=message) value
!!           if(ios2 == 0)then
!!              write(*,*)'VALUE=',value
!!           elseif( is_iostat_end(ios1) ) then
!!              stop 'end of file'
!!           else
!!              write(*,*)'ERROR:',ios2,trim(message)
!!           endif
!!           !
!!           ! try string using isnumber(3f)
!!           answer=isnumber(line,msg=description)
!!           if(answer > 0)then
!!              write(*,*) &
!!              & ' for ',trim(line),' ',answer,':',description
!!           else
!!              write(*,*) &
!!              & ' ERROR for ',trim(line),' ',answer,':',description
!!           endif
!!           !
!!        enddo
!!     end program demo_isnumber
!!
!!  Example run
!!
!!    > Begin entering values
!!    > ERROR:          -1 End of file
!!    >  ERROR for            -1 :null string
!!    >10
!!    > VALUE=   10.0000000
!!    >  for 10            1 :integer
!!    >20
!!    > VALUE=   20.0000000
!!    >  for 20            1 :integer
!!    >20.
!!    > VALUE=   20.0000000
!!    >  for 20.            2 :whole number
!!    >30.1
!!    > VALUE=   30.1000004
!!    >  for 30.1            3 :real number
!!    >3e1
!!    > VALUE=   30.0000000
!!    >  for 3e1            4 :value with exponent
!!    >1-2
!!    > VALUE=   9.99999978E-03
!!    >  for 1-2            4 :value with exponent
!!    >100.22d-4
!!    > VALUE=   1.00220004E-02
!!    >  for 100.22d-4            4 :value with exponent
!!    >1--2
!!    > ERROR:        5010 Bad real number in item 1 of list input
!!    >  ERROR for 1--2           -5 :bad number
!!    >e
!!    > ERROR:        5010 Bad real number in item 1 of list input
!!    >  ERROR for e           -6 :missing leading value before exponent
!!    >e1
!!    > ERROR:        5010 Bad real number in item 1 of list input
!!    >  ERROR for e1           -6 :missing leading value before exponent
!!    >1e
!!    > ERROR:        5010 Bad real number in item 1 of list input
!!    >  ERROR for 1e           -3 :missing exponent
!!    >1e+
!!    > ERROR:        5010 Bad real number in item 1 of list input
!!    >  ERROR for 1e+           -4 :missing exponent after sign
!!    >1e+2.0
!!    > ERROR:        5010 Bad real number in item 1 of list input
!!    >  ERROR for 1e+2.0           -5 :bad number
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
function isNumber(string,msg,verbose)
implicit none

! ident_52="@(#) M_strings isnumber(3f) Determines if a string is a number of not."

character(len=*),intent(in)    :: string
character(len=:),intent(out),allocatable,optional :: msg
logical,intent(in),optional                      :: verbose
integer                      :: isnumber

integer             :: i,iend
character(len=1),allocatable :: z(:)
character(len=:),allocatable :: message
logical                      :: founddigit
logical                      :: verbose_local

   i=1
   founddigit=.false.
   isnumber=0
   z=switch(trim(nospace(string)))
   iend=size(z)
   message='not a number'
   if(present(verbose))then
      verbose_local=verbose
   else
      verbose_local=.false.
   endif
   DONE : block
      if(iend == 0)then
         isnumber=-1                   ! string is null
         message='null string'
         exit DONE
      endif

      if(index('+-',z(i)) /= 0) i=i+1  ! skip optional leading sign
      if(i > iend)then
         isnumber=-2                   ! string was just a sign
         message='just a sign'
         exit DONE
      endif

      call next()                      ! position I to next non-digit or end of string+1

      if(i > iend)then
         isnumber=1                    ! [+-]NNNNNN
         message='integer'
         exit DONE
      endif
      if(z(i) == '.')then              ! a period would be OK at this point
         i=i+1
      endif

      if(i > iend)then                ! [+-]NNNNNN.
         isnumber=2
         message='whole number'
         exit DONE
      endif

      call next()                      ! position I to next non-digit or end of string+1
      if(i > iend)then
         isnumber=3                    ! [+-]NNNNNN.MMMM
         message='real number'
         exit DONE
      endif

      if(index('eEdD',z(i)) /= 0)then
         i=i+1
         if(i == 2)then
            isnumber=-6                   ! [+-]NNNNNN[.[MMMM]]e but a value must follow
            message='missing leading value before exponent'
            exit DONE
         endif
      endif
      if(i > iend)then
         isnumber=-3                   ! [+-]NNNNNN[.[MMMM]]e but a value must follow
         message='missing exponent'
         exit DONE
      endif
      if(.not.founddigit)then
         isnumber=-7
         message='missing value before exponent'
         exit DONE
      endif
      if(index('+-',z(i)) /= 0) i=i+1
      if(i > iend)then
         isnumber=-4                   ! [+-]NNNNNN[.[MMMM]]e[+-] but a value must follow
         message='missing exponent after sign'
         exit DONE
      endif
      call next()                      ! position I to next non-digit or end of string+1
      if(i > iend)then
         isnumber=4                    ! [+-]NNNNNN.MMMMe[+-]LL
         message='value with exponent'
         exit DONE
      endif
      isnumber=-5
      message='bad number'
   endblock DONE
   if(verbose_local)then
      write(*,*)trim(string)//' is '//message
   endif
   if(present(msg))then
      msg=message
   endif

contains
   subroutine next() ! move to next non-digit or end of string+1
      integer :: j
      do j=i,iend
         if(.not.isdigit(z(j)))then
            exit
         endif
         founddigit=.true.
         if(verbose_local) write(*,*)'I=',i,' J=',j,' Z(j)=',z(j)
      enddo
      i=j
      if(verbose_local)then
         write(*,*)'I and J=',i
         if(i <= iend) then
            write(*,*)'Z(I)=',z(i)
         else
            write(*,*)'====>'
         endif
      endif
   end subroutine next
end function isNumber
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    trimzeros_(3fp) - [M_strings:NUMERIC] Delete trailing zeros from
!!    numeric decimal string
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine trimzeros_(str)
!!
!!     character(len=*)  :: str
!!
!!##DESCRIPTION
!!    TRIMZEROS_(3f) deletes trailing zeros from a string representing a
!!    number. If the resulting string would end in a decimal point, one
!!    trailing zero is added.
!!
!!##OPTIONS
!!    str   input string will be assumed to be a numeric value and have
!!          trailing zeros removed
!!##EXAMPLES
!!
!!    Sample program:
!!
!!       program demo_trimzeros_
!!       use M_strings, only : trimzeros_
!!       character(len=:),allocatable :: string
!!          write(*,*)trimzeros_('123.450000000000')
!!          write(*,*)trimzeros_('12345')
!!          write(*,*)trimzeros_('12345.')
!!          write(*,*)trimzeros_('12345.00e3')
!!       end program demo_trimzeros_
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
subroutine trimzeros_(string)

! ident_53="@(#) M_strings trimzeros_(3fp) Delete trailing zeros from numeric decimal string"

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
   if(index(str,'.') == 0)then               ! if no decimal character in original string add one to end of string
      ii=len_trim(str)
      str(ii+1:ii+1)='.'                     ! add decimal to end of string
   endif
   do i=len_trim(str),1,-1                   ! scanning from end find a non-zero character
      select case(str(i:i))
      case('0')                              ! found a trailing zero so keep trimming
         cycle
      case('.')                              ! found a decimal character at end of remaining string
         if(i <= 1)then
            str='0'
         else
            str=str(1:i-1)
         endif
         exit
      case default
         str=str(1:i)                        ! found a non-zero character so trim string and exit
         exit
      end select
   enddo
   if(ipos>0)then                            ! if originally had an exponent place it back on
      string=trim(str)//trim(exp)
   else
      string=str
   endif
end subroutine trimzeros_
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    listout(3f) - [M_strings:NUMERIC] expand a list of numbers where negative
!!    numbers denote range ends (1 -10 means 1 thru 10)
!! (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   subroutine listout(icurve_lists,icurve_expanded,inums,ierr)
!!
!!    integer,intent(in)    :: icurve_lists(:)
!!    integer,intent(out)   :: icurve_expanded(:)
!!    integer,intent(out)   :: inums
!!    integer,intent(out)   :: ierr
!!
!!##DESCRIPTION
!!    expand a list of whole numbers where negative numbers indicate a range.
!!    So [10,-20] would be expanded to [10,11,12,13,14,15,16,17,18,19,20].
!!
!!##OPTIONS
!!    icurve_lists(:)      input array
!!
!!##RETURNS
!!    icurve_expanded(:)   output array; assumed large enough to hold
!!                         returned list
!!    inums                number of icurve_expanded numbers on output
!!    ierr                 zero if no error occurred
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!     program demo_listout
!!     use M_strings, only : listout
!!     implicit none
!!     integer,allocatable :: icurve_lists(:)
!!     integer :: icurve_expanded(1000)
!!     ! icurve_lists is input array
!!     integer :: inums
!!     ! icurve_expanded is output array
!!     integer :: i
!!     ! number of icurve_lists values on input,
!!     ! number of icurve_expanded numbers on output
!!     integer :: ierr
!!        icurve_lists=[1, 20, -30, 101, 100, 99, 100, -120, 222, -200]
!!        inums=size(icurve_lists)
!!        call listout(icurve_lists,icurve_expanded,inums,ierr)
!!        if(ierr == 0)then
!!           write(*,'(i0)')(icurve_expanded(i),i=1,inums)
!!        else
!!           write(*,'(a,i0)')'error occurred in *listout* ',ierr
!!           write(*,'(i0)')(icurve_expanded(i),i=1,inums)
!!        endif
!!     end program demo_listout
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
subroutine listout(icurve_lists,icurve_expanded,inums_out,ierr)
implicit none

! ident_54="@(#) M_strings listout(3f) copy icurve_lists to icurve_expanded expanding negative numbers to ranges (1 -10 means 1 thru 10)"

!   Created: 19971231
integer,intent(in)    :: icurve_lists(:)             ! input array
integer,intent(out)   :: icurve_expanded(:)          ! output array
integer,intent(out)   :: inums_out                   ! number of icurve_expanded numbers on output
integer,intent(out)   :: ierr                        ! status variable

character(len=80)     :: temp1
integer               :: i80, i90
integer               :: imin, imax
integer               :: idirection, icount
integer               :: iin
integer               :: inums_max

   ierr=0
   icurve_expanded=0                          ! initialize output array
   inums_out=0                                ! initialize number of significant values in output array

   inums_max=size(icurve_expanded)
   if(inums_max == 0)then
      ierr=-2
      return
   endif

   iin=size(icurve_lists)
   if(iin > 0)then
      icurve_expanded(1)=icurve_lists(1)
   endif

   icount=2
      do i90=2,iin
         if(icurve_lists(i90) < 0)then
            imax=abs(icurve_lists(i90))
            imin=abs(icurve_lists(i90-1))
            if(imin > imax)then
               idirection=-1
               imin=imin-1
            elseif(imax > imin)then
               idirection=1
               imin=imin+1
            else
               idirection=1
            endif
            do i80=imin,imax,idirection
               if(icount > inums_max) then
                  write(temp1,'(a,i5,a)')'*listout* only ',inums_max,' values allowed'
                  ierr=-1
                  call journal(temp1)
                  inums_out=icount-1
                  exit
               endif
               icurve_expanded(icount)=i80
               icount=icount+1
            enddo
         else
            icurve_expanded(icount)=icurve_lists(i90)
            icount=icount+1
         endif
      enddo
   inums_out=icount-1

end subroutine listout
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!     quote(3f) - [M_strings:QUOTES] add quotes to string as if written
!!     with list-directed input
!!     (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   function quote(str,mode,clip) result (quoted_str)
!!
!!    character(len=*),intent(in)          :: str
!!    character(len=*),optional,intent(in) :: mode
!!    logical,optional,intent(in)          :: clip
!!    character(len=:),allocatable         :: quoted_str
!!
!!##DESCRIPTION
!!    Add quotes to a CHARACTER variable as if it was written using
!!    list-directed input. This is particularly useful for processing
!!    strings to add to CSV files.
!!
!!##OPTIONS
!!    str    input string to add quotes to, using the rules of
!!           list-directed input (single quotes are replaced by two
!!           adjacent quotes)
!!    mode   alternate quoting methods are supported:
!!
!!             DOUBLE   default. replace quote with double quotes
!!             ESCAPE   replace quotes with backslash-quote instead of
!!                      double quotes
!!
!!    clip   default is to trim leading and trailing spaces from the
!!           string. If CLIP is .FALSE. spaces are not trimmed
!!
!!##RESULT
!!    quoted_str  The output string, which is based on adding quotes to STR.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_quote
!!    use M_strings, only : quote
!!    implicit none
!!    character(len=:),allocatable :: str
!!    character(len=1024)          :: msg
!!    integer                      :: ios
!!    character(len=80)            :: inline
!!       do
!!          write(*,'(a)',advance='no')'Enter test string:'
!!          read(*,'(a)',iostat=ios,iomsg=msg)inline
!!          if(ios /= 0)then
!!             write(*,*)trim(inline)
!!             exit
!!          endif
!!
!!          ! the original string
!!          write(*,'(a)')'ORIGINAL     ['//trim(inline)//']'
!!
!!          ! the string processed by quote(3f)
!!          str=quote(inline)
!!          write(*,'(a)')'QUOTED     ['//str//']'
!!
!!          ! write the string list-directed to compare the results
!!          write(*,'(a)',iostat=ios,iomsg=msg) 'LIST DIRECTED:'
!!          write(*,*,iostat=ios,iomsg=msg,delim='none') inline
!!          write(*,*,iostat=ios,iomsg=msg,delim='quote') inline
!!          write(*,*,iostat=ios,iomsg=msg,delim='apostrophe') inline
!!       enddo
!!    end program demo_quote
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
function quote(str,mode,clip) result (quoted_str)
character(len=*),intent(in)          :: str                ! the string to be quoted
character(len=*),optional,intent(in) :: mode
logical,optional,intent(in)          :: clip
character(len=:),allocatable         :: quoted_str

character(len=1),parameter           :: double_quote = '"'
character(len=20)                    :: local_mode

   if(present(clip))then
      if(clip)then
         quoted_str=adjustl(str)
      else
         quoted_str=str
      endif
   else
      quoted_str=str
   endif

   local_mode=merge_str(mode,'DOUBLE',present(mode))

   select case(lower(local_mode))
   case('double')
      quoted_str=double_quote//trim(replace(quoted_str,'"','""'))//double_quote
   case('escape')
      quoted_str=double_quote//trim(replace(quoted_str,'"','\"'))//double_quote
   case default
      call journal('sc','*quote* ERROR: unknown quote mode ',local_mode)
      quoted_str=str
   end select
!-----------------------------------------------------------------------------------------------------------------------------------
end function quote
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!     unquote(3f) - [M_strings:QUOTES] remove quotes from string as if
!!     read with list-directed input
!!     (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   function unquote(quoted_str,esc) result (unquoted_str)
!!
!!    character(len=*),intent(in)          :: quoted_str
!!    character(len=1),optional,intent(in) :: esc
!!    character(len=:),allocatable         :: unquoted_str
!!
!!##DESCRIPTION
!!    Remove quotes from a CHARACTER variable as if it was read using
!!    list-directed input. This is particularly useful for processing
!!    tokens read from input such as CSV files.
!!
!!    Fortran can now read using list-directed input from an internal file,
!!    which should handle quoted strings, but list-directed input does not
!!    support escape characters, which UNQUOTE(3f) does.
!!
!!##OPTIONS
!!    quoted_str  input string to remove quotes from, using the rules of
!!                list-directed input (two adjacent quotes inside a quoted
!!                region are replaced by a single quote, a single quote or
!!                double quote is selected as the delimiter based on which
!!                is encountered first going from left to right, ...)
!!    esc         optional character used to protect the next quote
!!                character from being processed as a quote, but simply as
!!                a plain character.
!!
!!##RESULT
!!    unquoted_str  The output string, which is based on removing quotes
!!                  from quoted_str.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_unquote
!!       use M_strings, only : unquote
!!       implicit none
!!       character(len=128)           :: quoted_str
!!       character(len=:),allocatable :: unquoted_str
!!       character(len=1),parameter   :: esc='\'
!!       character(len=1024)          :: msg
!!       integer                      :: ios
!!       character(len=1024)          :: dummy
!!       do
!!          write(*,'(a)',advance='no')'Enter test string:'
!!          read(*,'(a)',iostat=ios,iomsg=msg)quoted_str
!!          if(ios /= 0)then
!!             write(*,*)trim(msg)
!!             exit
!!          endif
!!
!!          ! the original string
!!          write(*,'(a)')'QUOTED       ['//trim(quoted_str)//']'
!!
!!          ! the string processed by unquote(3f)
!!          unquoted_str=unquote(trim(quoted_str),esc)
!!          write(*,'(a)')'UNQUOTED     ['//unquoted_str//']'
!!
!!          ! read the string list-directed to compare the results
!!          read(quoted_str,*,iostat=ios,iomsg=msg)dummy
!!          if(ios /= 0)then
!!             write(*,*)trim(msg)
!!          else
!!             write(*,'(a)')'LIST DIRECTED['//trim(dummy)//']'
!!          endif
!!       enddo
!!    end program demo_unquote
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
function unquote(quoted_str,esc) result (unquoted_str)
character(len=*),intent(in)          :: quoted_str              ! the string to be unquoted
character(len=1),optional,intent(in) :: esc                     ! escape character
character(len=:),allocatable         :: unquoted_str
integer                              :: inlen
character(len=1),parameter           :: single_quote = "'"
character(len=1),parameter           :: double_quote = '"'
integer                              :: quote                   ! whichever quote is to be used
integer                              :: before
integer                              :: current
integer                              :: iesc
integer                              :: iput
integer                              :: i
logical                              :: inside
!-----------------------------------------------------------------------------------------------------------------------------------
   if(present(esc))then                           ! select escape character as specified character or special value meaning not set
      iesc=iachar(esc)                            ! allow for an escape character
   else
      iesc=-1                                     ! set to value that matches no character
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   inlen=len(quoted_str)                          ! find length of input string
   allocate(character(len=inlen) :: unquoted_str) ! initially make output string length of input string
!-----------------------------------------------------------------------------------------------------------------------------------
   if(inlen >= 1)then                             ! double_quote is the default quote unless the first character is single_quote
      if(quoted_str(1:1) == single_quote)then
         quote=iachar(single_quote)
      else
         quote=iachar(double_quote)
      endif
   else
      quote=iachar(double_quote)
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   before=-2                                      ! initially set previous character to impossible value
   unquoted_str(:)=''                             ! initialize output string to null string
   iput=1
   inside=.false.
   STEPTHROUGH: do i=1,inlen
      current=iachar(quoted_str(i:i))
      if(before == iesc)then                      ! if previous character was escape use current character unconditionally
           iput=iput-1                            ! backup
           unquoted_str(iput:iput)=char(current)
           iput=iput+1
           before=-2                              ! this could be second esc or quote
      elseif(current == quote)then                ! if current is a quote it depends on whether previous character was a quote
         if(before == quote)then
           unquoted_str(iput:iput)=char(quote)    ! this is second quote so retain it
           iput=iput+1
           before=-2
         elseif(.not.inside.and.before /= iesc)then
            inside=.true.
         else                                     ! this is first quote so ignore it except remember it in case next is a quote
            before=current
         endif
      else
         unquoted_str(iput:iput)=char(current)
         iput=iput+1
         before=current
      endif
   enddo STEPTHROUGH
!-----------------------------------------------------------------------------------------------------------------------------------
   unquoted_str=unquoted_str(:iput-1)
!-----------------------------------------------------------------------------------------------------------------------------------
end function unquote
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    edit_distance(3f) - [M_strings] returns a naive edit distance using
!!    the Levenshtein distance algorithm
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    pure elemental function edit_distance(str1,str2) result (distance)
!!
!!     character(len=*),intent(in)   :: str1, str2
!!     integer :: distance
!!
!!##DESCRIPTION
!!
!!   The Levenshtein distance function returns how many edits (deletions,
!!   insertions, transposition) are required to turn one string into another.
!!
!!##EXAMPLES
!!
!!   Sample Program:
!!
!!    program demo_edit_distance
!!    use M_strings, only : edit_distance
!!       write(*,*)edit_distance('kittens','sitting')==3
!!       write(*,*)edit_distance('geek','gesek')==1
!!       write(*,*)edit_distance('Saturday','Sunday')==3
!!    end program demo_edit_distance
!!
!!   Expected output
!!
!!     T
!!     T
!!     T
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
! The Levenshtein distance function returns how many edits (deletions,
! insertions, transposition) are required to turn one string into another.
pure elemental integer function edit_distance (a,b)
character(len=*), intent(in) :: a, b
integer                      :: len_a, len_b, i, j, cost
! matrix for calculating Levenshtein distance
integer                      :: matrix(0:len_trim(a), 0:len_trim(b))
   len_a = len_trim(a)
   len_b = len_trim(b)
   matrix(:,0) = [(i,i=0,len_a)]
   matrix(0,:) = [(j,j=0,len_b)]
   do i = 1, len_a
      do j = 1, len_b
         cost=merge(0,1,a(i:i)==b(j:j))
         matrix(i,j) = min(matrix(i-1,j)+1, matrix(i,j-1)+1, matrix(i-1,j-1)+cost)
      enddo
   enddo
   edit_distance = matrix(len_a,len_b)
end function edit_distance
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    cc(3f) - [M_strings] return up to twenty strings of arbitrary length
!!             as an array
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function cc(str1,str2,...str20,len) result (vec)
!!
!!     character(len=*),intent(in),optional   :: str1, str2 ... str20
!!     integer,intent(in),optional            :: len
!!
!!##DESCRIPTION
!!    Given a list of up to twenty strings create a string array. The
!!    length of the variables with be the same as the maximum length
!!    of the input strings unless explicitly specified via LEN.
!!
!!    This is an alternative to the syntax
!!
!!      [ CHARACTER(LEN=NN) :: str1, str2, ... ]
!!
!!    that calulates the minimum length required to prevent truncation by
!!    default.
!!
!!##OPTIONS
!!    str1,str2, ... str20  input strings to combine into a vector
!!    len   length of returned array variables
!!
!!##EXAMPLES
!!
!!   Sample Program:
!!
!!    program demo_cc
!!    use M_strings, only: cc
!!    implicit none
!!       print "(*('""',a,'""':,',',1x))", cc("one")
!!       print "(*('""',a,'""':,',',1x))", cc("one","two")
!!       print "(*('""',a,'""':,',',1x))", cc("one","two","three")
!!       print "(*('""',a,'""':,',',1x))", cc("one","two","three",&
!!               & "four","five","six","seven")
!!    end program demo_cc
!!
!!   Expected output
!!
!!    "one"
!!    "one", "two"
!!    "one  ", "two  ", "three"
!!    "one  ", "two  ", "three", "four ", "five ", "six  ", "seven"
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
function cc(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19,x20,len) result(vec)
! return character array containing present arguments
character(len=*),intent(in),optional  :: x1,x2,x3,x4,x5,x6,x7,x8,x9,x10
character(len=*),intent(in),optional  :: x11,x12,x13,x14,x15,x16,x17,x18,x19,x20
integer,intent(in),optional           :: len
character(len=:),allocatable          :: vec(:)
integer                               :: ilen, icount, iset
   ilen=0
   icount=0
   iset=0
   call increment(x1)
   call increment(x2)
   call increment(x3)
   call increment(x4)
   call increment(x5)
   call increment(x6)
   call increment(x7)
   call increment(x8)
   call increment(x9)
   call increment(x10)
   call increment(x11)
   call increment(x12)
   call increment(x13)
   call increment(x14)
   call increment(x15)
   call increment(x16)
   call increment(x17)
   call increment(x18)
   call increment(x19)
   call increment(x20)

   if(present(len)) ilen=len
   allocate (character(len=ilen) ::vec(icount))

   call set(x1)
   call set(x2)
   call set(x3)
   call set(x4)
   call set(x5)
   call set(x6)
   call set(x7)
   call set(x8)
   call set(x9)
   call set(x10)
   call set(x11)
   call set(x12)
   call set(x13)
   call set(x14)
   call set(x15)
   call set(x16)
   call set(x17)
   call set(x18)
   call set(x19)
   call set(x20)

contains

subroutine increment(str)
character(len=*),intent(in),optional :: str
   if(present(str))then
      ilen=max(ilen,len_trim(str))
      icount=icount+1
   endif
end subroutine increment

subroutine set(str)
character(len=*),intent(in),optional :: str
   if(present(str))then
      iset=iset+1
      vec(iset)=str
   endif
end subroutine set

end function cc
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    describe(3f) - [M_strings] returns a string describing the name of
!!    a single character
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function describe(ch) result (string)
!!
!!     character(len=1),intent(in)   :: ch
!!     character(len=:),allocatable  :: string
!!
!!##DESCRIPTION
!!    describe(3f) returns a string describing long name of a single
!!    character
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
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
function describe(ch) result (string)

! ident_55="@(#) M_strings describe(3f) return string describing long name of a single character"

character(len=1),intent(in)   :: ch
character(len=:),allocatable  :: string
! LATER: add hex, octal, decimal, key-press description, alternate names
!  ASCII character codes
   select case (iachar(ch))
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
   case(    33  ); STRING="! exclamation point (screamer, gasper, slammer, startler, bang, shriek, pling)"
   case(    34  ); STRING=""" quotation marks"
   case(    35  ); STRING="# number sign (hash, pound sign, hashtag)"
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
         STRING='UNKNOWN'//v2s(IACHAR(ch))
   end select
end function describe
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    getvals(3f) - [M_strings:NUMERIC] read arbitrary number of REAL values
!!    from a character variable up to size of VALUES() array
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine getvals(line,values,icount,ierr)
!!
!!     character(len=*),intent(in)  :: line
!!     class(*),intent(out)         :: values(:)
!!     integer,intent(out)          :: icount
!!     integer,intent(out),optional :: ierr
!!
!!##DESCRIPTION
!!   GETVALS(3f) reads a relatively arbitrary number of numeric values from
!!   a character variable into a REAL array using list-directed input.
!!
!!   NOTE: In this version null values are skipped instead of meaning to leave
!!         that value unchanged
!!
!!        1,,,,,,,2 / reads VALUES=[1.0,2.0]
!!
!!   Per list-directed rules when reading values, allowed delimiters are
!!   comma, semi-colon and space.
!!
!!   the slash separator can be used to add inline comments.
!!
!!        10.1, 20.43e-1 ; 11 / THIS IS TREATED AS A COMMENT
!!
!!   Repeat syntax can be used up to the size of the output array. These are
!!   equivalent input lines:
!!
!!        4*10.0
!!        10.0, 10.0, 10.0, 10.0
!!
!!##OPTIONS
!!   LINE      A character variable containing the characters representing
!!             a list of numbers
!!
!!##RETURNS
!!   VALUES()  array holding numbers read from string. May be of type
!!             INTEGER, REAL, DOUBLEPRECISION, or CHARACTER. If CHARACTER the
!!             strings are returned as simple words instead of numeric values.
!!   ICOUNT    number of defined numbers in VALUES(). If ICOUNT reaches
!!             the size of the VALUES() array parsing stops.
!!   IERR      zero if no error occurred in reading numbers. Optional.
!!             If not present and an error occurs the program is terminated.
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!       program demo_getvals
!!       use M_strings, only: getvals
!!       implicit none
!!       integer,parameter  :: longest_line=256
!!       character(len=longest_line) :: line
!!       real               :: values(longest_line/2+1)
!!       integer            :: ios,icount,ierr
!!       INFINITE: do
!!          read(*,'(a)',iostat=ios) line
!!          if(ios /= 0)exit INFINITE
!!          call getvals(line,values,icount,ierr)
!!          write(*,'(4(g0,1x))')'VALUES=',values(:icount)
!!       enddo INFINITE
!!       end program demo_getvals
!!
!!  Sample input lines
!!
!!        10,20 30.4
!!        1 2 3
!!        1
!!
!!        3 4*2.5 8
!!        32.3333 / comment 1
!!        30e3;300,    30.0, 3
!!        even 1 like this! 10
!!        11,,,,22,,,,33
!!
!!  Expected output:
!!
!!     VALUES=   10.0000000       20.0000000       30.3999996
!!     VALUES=   1.00000000       2.00000000       3.00000000
!!     VALUES=   1.00000000
!!     VALUES=
!!     VALUES=   3.00000000       2.50000000       2.50000000
!!     2.50000000       2.50000000       8.00000000
!!     VALUES=   32.3333015
!!     VALUES=   30000.0000       300.000000       30.0000000
!!     3.00000000
!!     *getvals* WARNING:[even] is not a number
!!     *getvals* WARNING:[like] is not a number
!!     *getvals* WARNING:[this!] is not a number
!!     VALUES=   1.00000000       10.0000000
!!     VALUES=   11.0000000       22.0000000       33.0000000
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
subroutine getvals(line,values,icount,ierr)
implicit none

! ident_56="@(#) M_strings getvals(3f) read arbitrary number of values from a character variable"

! JSU 20170831

character(len=*),intent(in)  :: line
class(*),intent(out)         :: values(:)
integer,intent(out)          :: icount
integer,intent(out),optional :: ierr

character(len=:),allocatable :: buffer
character(len=len(line))     :: words(size(values))
integer                      :: ios, i, ierr_local,isize

   isize=0
   select type(values)
   type is (integer);          isize=size(values)
   type is (real);             isize=size(values)
   type is (doubleprecision);  isize=size(values)
   type is (character(len=*)); isize=size(values)
   end select

   ierr_local=0

   words=' '                            ! make sure words() is initialized to null+blanks
   buffer=trim(unquote(line))//"/"      ! add a slash to the end so how the read behaves with missing values is clearly defined
   read(buffer,*,iostat=ios) words      ! undelimited strings are read into an array
   icount=0
   do i=1,isize                         ! loop thru array and convert non-blank words to numbers
      if(words(i) == ' ')cycle

      select type(values)
      type is (integer);          read(words(i),*,iostat=ios)values(icount+1)
      type is (real);             read(words(i),*,iostat=ios)values(icount+1)
      type is (doubleprecision);  read(words(i),*,iostat=ios)values(icount+1)
      type is (character(len=*)); values(icount+1)=words(i)
      end select

      if(ios == 0)then
         icount=icount+1
      else
         ierr_local=ios
         write(ERROR_UNIT,*)'*getvals* WARNING:['//trim(words(i))//'] is not a number of specified type'
      endif
   enddo

   if(present(ierr))then
      ierr=ierr_local
   elseif(ierr_local /= 0)then        ! error occurred and not returning error to main program to print message and stop program
      write(ERROR_UNIT,*)'*getval* error reading line ['//trim(line)//']'
      stop 2
   endif

end subroutine getvals
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!      string_to_values(3f) - [M_strings:NUMERIC] read a string representing
!!      numbers into a numeric array
!!      (LICENSE:PD)
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
!!       LINE     Input string containing numbers
!!       IREAD    maximum number of values to try to read from input string
!!
!!##RESULTS
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
!!       implicit none
!!       character(len=80)  :: s=' 10 20e3;3.45 -400.3e-2;1234; 5678 '
!!       integer,parameter  :: isz=10
!!       real               :: array(isz)
!!       integer            :: inums, ierr, ii
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
!!     values...................   10.0000000  20000.0000  3.45000005
!!     -4.00299978  1234.00000  5678.00000
!!     string_to_values:
!!     input string............. 10 20e3;3.45 -400.3e-2;1234; 5678
!!     number of values found...           3
!!     values...................   10.0000000  2.29999995  3.14159989
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
subroutine string_to_values(line,iread,values,inums,delims,ierr)
implicit none
!----------------------------------------------------------------------------------------------------------------------------------
!   1989,1997-12-31,2014 John S. Urban

!   given a line of structure , string , string , string process each
!   string as a numeric value and store into an array.
!   DELIMS contain the legal delimiters. If a space is an allowed delimiter, it must not appear last in DELIMS.
!   There is no direct checking for more values than can fit in VALUES.
!   Quits if encounters any errors in read.
!----------------------------------------------------------------------------------------------------------------------------------

! ident_57="@(#) M_strings string_to_values(3f) reads an array of numbers from a numeric string"

character(len=*),intent(in)  :: line          ! input string
integer,intent(in)           :: iread         ! maximum number of values to try to read into values
real,intent(inout)           :: values(iread) ! real array to be filled with values
integer,intent(out)          :: inums         ! number of values successfully read from string
character(len=*),intent(in)  :: delims        ! allowed delimiters
integer,intent(out)          :: ierr          ! 0 if no error, else column number undecipherable string starts at
!----------------------------------------------------------------------------------------------------------------------------------
character(len=256)           :: delims_local        ! mutable copy of allowed delimiters
integer                      :: istart,iend,lgth,icol
integer                      :: i10,i20,i40
real                         :: rval
integer                      :: ier
integer                      :: delimiters_length
!----------------------------------------------------------------------------------------------------------------------------------
      delims_local=delims                                 ! need a mutable copy of the delimiter list
      if(delims_local == '')then                          ! if delimiter list is null or all spaces make it a space
         delims_local=' '                                 ! delimiter is a single space
         delimiters_length=1                        ! length of delimiter list
      else
         delimiters_length=len_trim(delims)         ! length of variable WITH TRAILING WHITESPACE TRIMMED
      endif
!----------------------------------------------------------------------------------------------------------------------------------
      ierr=0                                        ! initialize error code returned
      inums=0                                       ! initialize count of values successfully returned
      istart=0
!----------------------------------------------------------------------------------------------------------------------------------
      lgth=0                                        ! lgth will be the position of the right-most non-delimiter in the input line
      do i20=len(line),1,-1                         ! loop from end of string to beginning to find right-most non-delimiter
         if(index(delims_local(:delimiters_length),line(i20:i20)) == 0)then   ! found a non-delimiter
            lgth=i20
            exit
         endif
      enddo
      if(lgth == 0)then                             ! command was totally composed of delimiters
         call journal('*string_to_values* blank line passed as a list of numbers')
         return
      endif
!----------------------------------------------------------------------------------------------------------------------------------
!     there is at least one non-delimiter sub-string
!     lgth is the column position of the last non-delimiter character
!     now, starting at beginning of string find next non-delimiter
      icol=1                                                     ! pointer to beginning of unprocessed part of LINE
      LOOP: dO i10=1,iread,1                                     ! each pass should find a value
         if(icol > lgth) EXIT LOOP                              ! everything is done
         INFINITE: do
            if(index(delims_local(:delimiters_length),line(icol:icol)) == 0)then           ! found non-delimiter
               istart=icol
               iend=0                                            ! FIND END OF SUBSTRING
               do i40=istart,lgth                                ! look at each character starting at left
                  if(index(delims_local(:delimiters_length),line(i40:i40)) /= 0)then       ! determine if character is a delimiter
                     iend=i40                                    ! found a delimiter. record where it was found
                     EXIT                                        ! found end of substring so leave loop
                  endif
               enddo
              if(iend == 0)iend=lgth+1                           ! no delimiters found, so this substring goes to end of line
               iend=iend-1                                       ! do not want to pass delimiter to be converted
               rval=0.0
               call string_to_value(line(istart:iend),rval,ier)  ! call procedure to convert string to a numeric value
               if(ier == 0)then                                  ! a substring was successfully converted to a numeric value
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
!!      s2vs(3f) - [M_strings:NUMERIC] given a string representing numbers
!!      return a numeric array
!!      (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!       function s2vs(line[,delim])
!!
!!        character(len=*) :: line
!!        doubleprecision,allocatable :: s2vs(:)
!!
!!##DESCRIPTION
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
!!
!!##RESULTS
!!       S2VS   doubleprecision array
!!
!!##EXAMPLE
!!
!!   Sample Program:
!!
!!      program demo_s2vs
!!      use M_strings, only : s2vs
!!      implicit none
!!      character(len=80) :: s=' 10 20e3;3.45 -400.3e-2;1234; 5678 '
!!      real,allocatable :: values(:)
!!      integer,allocatable :: ivalues(:)
!!      integer :: ii
!!
!!      values=s2vs(s)
!!      ivalues=int(s2vs(s))
!!      call reportit()
!!
!!      contains
!!        subroutine reportit()
!!          write(*,*)'S2VS:'
!!          write(*,*)'input string.............',&
!!           & trim(s)
!!          write(*,*)'number of values found...',&
!!           & size(values)
!!          write(*,*)'values...................',&
!!           & (values(ii),ii=1,size(values))
!!          write(*,'(*(g0,1x))')'ivalues..................',&
!!           & (ivalues(ii),ii=1,size(values))
!!        end subroutine reportit
!!      end program demo_s2vs
!!
!!   Expected output
!!
!!     S2VS:
!!     input string............. 10 20e3;3.45 -400.3e-2;1234; 5678
!!     number of values found...           6
!!     values...................   10.0000000 20000.0000 3.45000005
!!     -4.00299978 1234.00000 5678.00000
!!    ivalues.................. 10 20000 3 -4 1234 5678
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
function s2vs(string,delim) result(darray)

! ident_58="@(#) M_strings s2vs(3f) function returns array of values from a string"

character(len=*),intent(in)        :: string                       ! keyword to retrieve value for from dictionary
character(len=*),optional          :: delim                        ! delimiter characters
character(len=:),allocatable       :: delim_local
doubleprecision,allocatable        :: darray(:)                    ! function type

character(len=:),allocatable       :: carray(:)                    ! convert value to an array using split(3f)
integer                            :: i
integer                            :: ier
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
!>
!!##NAME
!!     isprint(3f) - [M_strings:COMPARE] returns .true. if character is an
!!     ASCII printable character
!!     (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!
!!    elemental function isprint(onechar)
!!
!!     character,intent(in) :: onechar
!!     logical              :: isprint
!!
!!##DESCRIPTION
!!     isprint(3f) returns .true. if character is an ASCII printable character
!!
!!##OPTIONS
!!    onechar  character to test
!!
!!##RETURNS
!!    isprint  logical value returns true if character is a
!!             printable ASCII character else false.
!!##EXAMPLE
!!
!!   Sample Program:
!!
!!    program demo_isprint
!!    use M_strings, only : isprint
!!    implicit none
!!    integer                    :: i
!!    character(len=1),parameter :: string(*)=[(char(i),i=0,127)]
!!       write(*,'(40(a))')'ISPRINT: ',pack( string, isprint(string) )
!!    end program demo_isprint
!!
!!   Results:
!!
!!    ISPRINT:  !"#$%&'()*+,-./0123456789:;<=>?@ABCDEF
!!    GHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmn
!!    opqrstuvwxyz{|}~
!!
!!##AUTHOR
!!     John S. Urban
!!
!!##LICENSE
!!     Public Domain
elemental function isprint(onechar)

! ident_59="@(#) M_strings isprint(3f) indicates if input character is a printable ASCII character"

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
!>
!!##NAME
!!     isgraph(3f) - [M_strings:COMPARE] returns .true. if character is a
!!     printable character except a space is considered non-printable
!!     (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!
!!    elemental function isgraph(onechar)
!!
!!     character,intent(in) :: onechar
!!     logical              :: isgraph
!!
!!##DESCRIPTION
!!    isgraph(3f) returns .true. if character is a printable character
!!    except a space is considered non-printable
!!
!!##OPTIONS
!!    onechar   character to test
!!
!!##RETURNS
!!    isgraph   logical value returns true if character is a printable
!!              non-space character
!!##EXAMPLE
!!
!!   Sample Program:
!!
!!    program demo_isgraph
!!    use M_strings, only : isgraph
!!    implicit none
!!    integer                    :: i
!!    character(len=1),parameter :: string(*)=[(char(i),i=0,127)]
!!       write(*,'(40(a))')'ISGRAPH: ',pack( string, isgraph(string) )
!!    end program demo_isgraph
!!
!!   Results:
!!
!!    ISGRAPH: !"#$%&'()*+,-./0123456789:;<=>?@ABCDEFG
!!    HIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmno
!!    pqrstuvwxyz{|}~
!!
!!##AUTHOR
!!     John S. Urban
!!
!!##LICENSE
!!     Public Domain
elemental function isgraph(onechar)

! ident_60="@(#) M_strings isgraph(3f) indicates if character is printable ASCII character excluding space"

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
!>
!!##NAME
!!    isalpha(3f) - [M_strings:COMPARE] returns .true. if character is a
!!    letter and .false. otherwise
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!
!!   elemental function isalpha(onechar)
!!
!!    character,intent(in) :: onechar
!!    logical              :: isalpha
!!
!!##DESCRIPTION
!!    isalpha(3f) returns .true. if character is a letter and
!!    .false. otherwise
!!
!!##OPTIONS
!!    onechar  character to test
!!
!!##RETURNS
!!    isalpha  logical value returns .true. if character is a ASCII letter
!!             or false otherwise.
!!##EXAMPLE
!!
!!
!!   Sample program
!!
!!     program demo_isalpha
!!     use M_strings, only : isalpha
!!     implicit none
!!     integer                    :: i
!!     character(len=1),parameter :: string(*)=[(char(i),i=0,127)]
!!        write(*,'(40(a))')'ISGRAPH: ',pack( string, isalpha(string) )
!!     end program demo_isalpha
!!
!!   Results:
!!
!!    ISGRAPH: ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklm
!!    nopqrstuvwxyz
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
elemental function isalpha(ch) result(res)

! ident_61="@(#) M_strings isalpha(3f) Return .true. if character is a letter and .false. otherwise"

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
!>
!!##NAME
!!     isxdigit(3f) - [M_strings:COMPARE] returns .true. if character is a
!!     hexadecimal digit (0-9, a-f, or A-F).
!!     (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!
!!    elemental function isxdigit(onechar)
!!
!!     character,intent(in) :: onechar
!!     logical              :: isxdigit
!!
!!##DESCRIPTION
!!     isxdigit(3f) returns .true. if character is a hexadecimal digit (0-9,
!!     a-f, or A-F).
!!
!!##OPTIONS
!!    onechar   character to test
!!
!!##RETURNS
!!    isxdigit  logical value returns true if character is a hexadecimal digit
!!
!!##EXAMPLE
!!
!!   Sample program
!!
!!     program demo_isxdigit
!!     use M_strings, only : isxdigit
!!     implicit none
!!     integer                    :: i
!!     character(len=1),parameter :: string(*)=[(char(i),i=0,127)]
!!        write(*,'(40(a))')'ISXDIGIT: ',pack( string, isxdigit(string) )
!!     end program demo_isxdigit
!!
!!   Results:
!!
!!    ISXDIGIT: 0123456789ABCDEFabcdef
!!
!!##AUTHOR
!!     John S. Urban
!!
!!##LICENSE
!!     Public Domain
elemental function isxdigit(ch) result(res)

! ident_62="@(#) M_strings isxdigit(3f) returns .true. if c is a hexadecimal digit (0-9 a-f or A-F)"

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
!>
!!##NAME
!!     isdigit(3f) - [M_strings:COMPARE] returns .true. if character is a
!!     digit (0,1,...,9) and .false. otherwise
!!     (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!
!!    elemental function isdigit(onechar)
!!
!!     character,intent(in) :: onechar
!!     logical              :: isdigit
!!
!!##DESCRIPTION
!!     isdigit(3f) returns .true. if character is a digit (0,1,...,9)
!!     and .false. otherwise
!!
!!##EXAMPLES
!!
!!
!!  Sample Program:
!!
!!     program demo_isdigit
!!     use M_strings, only : isdigit, isspace, switch
!!     implicit none
!!     character(len=10),allocatable :: string(:)
!!     integer                       :: i
!!        string=[&
!!        & '1 2 3 4 5 ' ,&
!!        & 'letters   ' ,&
!!        & '1234567890' ,&
!!        & 'both 8787 ' ]
!!        ! if string is nothing but digits and whitespace return .true.
!!        do i=1,size(string)
!!           write(*,'(a)',advance='no')'For string['//string(i)//']'
!!           write(*,*) &
!!            & all(isdigit(switch(string(i))).or.&
!!            & isspace(switch(string(i))))
!!        enddo
!!     end program demo_isdigit
!!
!!  Expected output:
!!
!!        For string[1 2 3 4 5 ] T
!!        For string[letters   ] F
!!        For string[1234567890] T
!!        For string[both 8787 ] F
!!
!!##AUTHOR
!!     John S. Urban
!!
!!##LICENSE
!!     Public Domain
elemental function isdigit(ch) result(res)

! ident_63="@(#) M_strings isdigit(3f) Returns .true. if ch is a digit (0-9) and .false. otherwise"

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
!>
!!##NAME
!!     isblank(3f) - [M_strings:COMPARE] returns .true. if character is a
!!     blank character (space or horizontal tab).
!!     (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!
!!    elemental function isblank(onechar)
!!
!!     character,intent(in) :: onechar
!!     logical              :: isblank
!!
!!##DESCRIPTION
!!     isblank(3f) returns .true. if character is a blank character (space
!!     or horizontal tab).
!!
!!##OPTIONS
!!    onechar  character to test
!!
!!##RETURNS
!!    isblank  logical value returns true if character is a "blank"
!!             ( an ASCII  space or horizontal tab character).
!!##EXAMPLE
!!
!!   Sample program:
!!
!!     program demo_isblank
!!     use M_strings, only : isblank
!!     implicit none
!!     integer                    :: i
!!     character(len=1),parameter :: string(*)=[(char(i),i=0,127)]
!!        write(*,'(*(g0,1x))')'ISXBLANK: ',&
!!        & iachar(pack( string, isblank(string) ))
!!     end program demo_isblank
!!
!!   Results:
!!
!!    ISXBLANK:  9 32
!!
!!##AUTHOR
!!     John S. Urban
!!
!!##LICENSE
!!     Public Domain
elemental function isblank(ch) result(res)

! ident_64="@(#) M_strings isblank(3f) returns .true. if character is a blank (space or horizontal tab)"

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
!>
!!##NAME
!!     isascii(3f) - [M_strings:COMPARE] returns .true. if the character is
!!     in the range char(0) to char(256)
!!     (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!
!!    elemental function isascii(onechar)
!!
!!     character,intent(in) :: onechar
!!     logical              :: isascii
!!
!!##DESCRIPTION
!!     isascii(3f) returns .true. if the character is in the range char(0)
!!     to char(127)
!!
!!##OPTIONS
!!    onechar  character to test
!!
!!##RETURNS
!!    isupper  logical value returns true if character is an ASCII
!!             character.
!!##EXAMPLE
!!
!!  Sample program
!!
!!     program demo_isascii
!!     use M_strings, only : isascii
!!     implicit none
!!     integer                    :: i
!!     character(len=1),parameter :: string(*)=[(char(i),i=0,255)]
!!        write(*,'(10(g0,1x))')'ISASCII: ', &
!!        & iachar(pack( string, isascii(string) ))
!!     end program demo_isascii
!!
!!  Results:
!!
!!    ISASCII:  0 1 2 3 4 5 6 7 8
!!    9 10 11 12 13 14 15 16 17 18
!!    19 20 21 22 23 24 25 26 27 28
!!    29 30 31 32 33 34 35 36 37 38
!!    39 40 41 42 43 44 45 46 47 48
!!    49 50 51 52 53 54 55 56 57 58
!!    59 60 61 62 63 64 65 66 67 68
!!    69 70 71 72 73 74 75 76 77 78
!!    79 80 81 82 83 84 85 86 87 88
!!    89 90 91 92 93 94 95 96 97 98
!!    99 100 101 102 103 104 105 106 107 108
!!    109 110 111 112 113 114 115 116 117 118
!!    119 120 121 122 123 124 125 126 127
!!
!!##AUTHOR
!!     John S. Urban
!!
!!##LICENSE
!!     Public Domain
elemental function isascii(ch) result(res)

! ident_65="@(#) M_strings isascii(3f) returns .true. if character is in the range char(0) to char(127)"

character,intent(in) :: ch
logical              :: res
   select case(iachar(ch))
   case(0:127)
     res=.true.
   case default
     res=.false.
   end select
end function isascii
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!     isspace(3f) - [M_strings:COMPARE] returns .true. if character is a
!!     null, space, tab, carriage return, new line, vertical tab, or formfeed
!!     (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!
!!    elemental function isspace(onechar)
!!
!!     character,intent(in) :: onechar
!!     logical              :: isspace
!!
!!##DESCRIPTION
!!     isspace(3f) returns .true. if character is a null, space, tab,
!!     carriage return, new line, vertical tab, or formfeed
!!
!!##OPTIONS
!!    onechar  character to test
!!
!!##RETURNS
!!    isspace  returns true if character is ASCII white space
!!
!!##EXAMPLE
!!
!!  Sample program:
!!
!!     program demo_isspace
!!     use M_strings, only : isspace
!!     implicit none
!!     integer                    :: i
!!     character(len=1),parameter :: string(*)=[(char(i),i=0,127)]
!!        write(*,'(20(g0,1x))')'ISSPACE: ', &
!!        & iachar(pack( string, isspace(string) ))
!!     end program demo_isspace
!!
!!   Results:
!!
!!    ISSPACE:  0 9 10 11 12 13 32
!!
!!##AUTHOR
!!     John S. Urban
!!
!!##LICENSE
!!     Public Domain
elemental function isspace(ch) result(res)

! ident_66="@(#) M_strings isspace(3f) true if null space tab return new line vertical tab or formfeed"

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
!>
!!##NAME
!!     iscntrl(3f) - [M_strings:COMPARE] returns .true. if character is a
!!     delete character or ordinary control character
!!     (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!
!!    elemental function iscntrl(onechar)
!!
!!     character,intent(in) :: onechar
!!     logical              :: iscntrl
!!
!!##DESCRIPTION
!!     iscntrl(3f) returns .true. if character is a delete character or
!!     ordinary control character
!!
!!##OPTIONS
!!    onechar  character to test
!!
!!##RETURNS
!!    iscntrl  logical value returns true if character is a control character
!!
!!##EXAMPLE
!!
!!  Sample program
!!
!!     program demo_iscntrl
!!     use M_strings, only : iscntrl
!!     implicit none
!!     integer                    :: i
!!     character(len=1),parameter :: string(*)=[(char(i),i=0,127)]
!!        write(*,'(20(g0,1x))')'ISCNTRL: ', &
!!        & iachar(pack( string, iscntrl(string) ))
!!     end program demo_iscntrl
!!
!!   Results:
!!
!!    ISCNTRL:  0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19
!!    20 21 22 23 24 25 26 27 28 29 30 31 127
!!
!!##AUTHOR
!!     John S. Urban
!!
!!##LICENSE
!!     Public Domain
elemental function iscntrl(ch) result(res)

! ident_67="@(#) M_strings iscntrl(3f) true if a delete or ordinary control character(0x7F or 0x00-0x1F)"

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
!>
!!##NAME
!!     ispunct(3f) - [M_strings:COMPARE] returns .true. if character is a
!!     printable punctuation character
!!     (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!
!!    elemental function ispunct(onechar)
!!
!!     character,intent(in) :: onechar
!!     logical              :: ispunct
!!
!!##DESCRIPTION
!!     ispunct(3f) returns .true. if character is a printable punctuation
!!     character
!!
!!##OPTIONS
!!    onechar  character to test
!!
!!##RETURNS
!!    ispunct  logical value returns true if character is a printable
!!             punctuation character.
!!
!!##EXAMPLE
!!
!!  Sample program:
!!
!!     program demo_ispunct
!!     use M_strings, only : ispunct
!!     implicit none
!!     integer                    :: i
!!     character(len=1),parameter :: string(*)=[(char(i),i=0,127)]
!!        write(*,'(20(g0,1x))')'ISPUNCT: ', &
!!        & iachar(pack( string, ispunct(string) ))
!!        write(*,'(20(g0,1x))')'ISPUNCT: ', &
!!        & pack( string, ispunct(string) )
!!     end program demo_ispunct
!!   Results:
!!
!!    ISPUNCT:  33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 58 59 60 61
!!    62 63 64 91 92 93 94 95 96 123 124 125 126
!!    ISPUNCT:  ! " # $ % & ' ( ) * + , - . / : ; < =
!!    > ? @ [ \ ] ^ _ ` { | } ~
!!
!!##AUTHOR
!!     John S. Urban
!!
!!##LICENSE
!!     Public Domain
elemental function ispunct(ch) result(res)

! ident_68="@(#) M_strings ispunct(3f) true if a printable punctuation character (isgraph(c)&&!isalnum(c))"

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
!>
!!##NAME
!!     fortran_name(3f) - [M_strings:COMPARE] test if string meets criteria
!!     for being a fortran name
!!
!!##SYNOPSIS
!!
!!
!!     elemental function fortran_name(line) result (lout)
!!
!!      character(len=*),intent(in)  :: line
!!      logical                      :: lout
!!
!!##DESCRIPTION
!!     Determines if a string is an allowed Fortran name. To pass the input
!!     string must be composed of 1 to 63 ASCII characters and start with a
!!     letter and be composed entirely of alphanumeric characters [a-zA-Z0-9]
!!     and underscores.
!!
!!##OPTIONS
!!     LINE   input string to test. Leading spaces are significant but
!!            trailing spaces are ignored.
!!
!!##RETURNS
!!     LOUT   a logical value indicating if the input string passed or failed
!!            the test to see if it is a valid Fortran name or not.
!!
!!##EXAMPLE
!!
!!    Sample program
!!
!!      program demo_fortran_name
!!      use M_strings, only : fortran_name
!!      implicit none
!!      character(len=20),parameter :: names(*)=[character(len=20) ::  &
!!       & '_name',         'long_variable_name', 'name_',         &
!!       & '12L',           'a__b__c  ',          'PropertyOfGas', &
!!       & '3%3',           '$NAME',              ' ',             &
!!       & 'Variable-name', 'A',                  'x@x' ]
!!      integer :: i
!!         write(*,'(i3,1x,a20,1x,l1)')&
!!         & (i,names(i),fortran_name(names(i)),i=1,size(names))
!!      end program demo_fortran_name
!!
!!    Results:
!!
!!       1 _name                F
!!       2 long_variable_name   T
!!       3 name_                T
!!       4 12L                  F
!!       5 a__b__c              T
!!       6 PropertyOfGas        T
!!       7 3%3                  F
!!       8 $NAME                F
!!       9                      F
!!      10 Variable-name        F
!!      11 A                    T
!!      12 x@x                  F
elemental function fortran_name(line) result (lout)

! ident_69="@(#) M_strings fortran_name(3f) Return .true. if name is a valid Fortran name"

! determine if a string is a valid Fortran name ignoring trailing spaces (but not leading spaces)
character(len=*),parameter   :: int='0123456789'
character(len=*),parameter   :: lower='abcdefghijklmnopqrstuvwxyz'
character(len=*),parameter   :: upper='ABCDEFGHIJKLMNOPQRSTUVWXYZ'
character(len=*),parameter   :: allowed=upper//lower//int//'_'

character(len=*),intent(in)  :: line
character(len=:),allocatable :: name
logical                      :: lout
   name=trim(line)
   if(len(name) /= 0)then
      lout = verify(name(1:1), lower//upper) == 0  &
       & .and. verify(name,allowed) == 0           &
       & .and. len(name) <= 63
   else
      lout = .false.
   endif
end function fortran_name
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!     isupper(3f) - [M_strings:COMPARE] returns .true. if character is an
!!     uppercase letter (A-Z)
!!     (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!
!!    elemental function isupper(onechar)
!!
!!     character,intent(in) :: onechar
!!     logical              :: isupper
!!
!!##DESCRIPTION
!!     isupper(3f) returns .true. if character is an uppercase letter (A-Z)
!!
!!##OPTIONS
!!    onechar  character to test
!!##RETURNS
!!    isupper  logical value returns true if character is an uppercase
!!             ASCII character else false.
!!##EXAMPLE
!!
!!  Sample program:
!!
!!     program demo_isupper
!!     use M_strings, only : isupper
!!     implicit none
!!     integer                    :: i
!!     character(len=1),parameter :: string(*)=[(char(i),i=0,127)]
!!        write(*,'(10(g0,1x))')'ISUPPER: ', &
!!        & iachar(pack( string, isupper(string) ))
!!        write(*,'(10(g0,1x))')'ISUPPER: ', &
!!        & pack( string, isupper(string) )
!!     end program demo_isupper
!!
!!  Results:
!!
!!    ISUPPER:  65 66 67 68 69 70 71 72 73
!!    74 75 76 77 78 79 80 81 82 83
!!    84 85 86 87 88 89 90
!!    ISUPPER:  A B C D E F G H I
!!    J K L M N O P Q R S
!!    T U V W X Y Z
!!
!!##AUTHOR
!!     John S. Urban
!!
!!##LICENSE
!!     Public Domain
pure elemental function isupper(ch) result(res)

! ident_70="@(#) M_strings isupper(3f) returns true if character is an uppercase letter (A-Z)"

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
!>
!!##NAME
!!     islower(3f) - [M_strings:COMPARE] returns .true. if character is a
!!     miniscule letter (a-z)
!!     (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!
!!    elemental function islower(onechar)
!!
!!     character,intent(in) :: onechar
!!     logical              :: islower
!!
!!##DESCRIPTION
!!     islower(3f) returns .true. if character is a miniscule letter (a-z)
!!
!!##OPTIONS
!!    onechar  character to test
!!
!!##RETURNS
!!    islowe  logical value returns true if character is a lowercase
!!             ASCII character else false.
!!##EXAMPLE
!!
!!  Sample program
!!
!!     program demo_islower
!!     use M_strings, only : islower
!!     implicit none
!!     integer                    :: i
!!     character(len=1),parameter :: string(*)=[(char(i),i=0,127)]
!!        write(*,'(15(g0,1x))')'ISLOWER: ', &
!!        & iachar(pack( string, islower(string) ))
!!        write(*,'(15(g0,1x))')'ISLOWER: ', &
!!        & pack( string, islower(string) )
!!     end program demo_islower
!!   Results:
!!
!!    ISLOWER:  97 98 99 100 101 102 103 104 105 106 107 108 109 110
!!    111 112 113 114 115 116 117 118 119 120 121 122
!!    ISLOWER:  a b c d e f g h i j k l m n
!!    o p q r s t u v w x y z
!!
!!##AUTHOR
!!     John S. Urban
!!
!!##LICENSE
!!     Public Domain
elemental function islower(ch) result(res)

! ident_71="@(#) M_strings islower(3f) returns true if character is a miniscule letter (a-z)"

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
!!    isprint,ispunct,isspace,isupper,
!!    isascii,isblank,isxdigit(3f) - [M_strings:COMPARE] test membership in
!!    subsets of ASCII set
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    Where "FUNCNAME" is one of the function names in the group, the
!!    functions are defined by
!!
!!     elemental function FUNCNAME(onechar)
!!     character,intent(in) :: onechar
!!     logical              :: FUNC_NAME
!!##DESCRIPTION
!!
!!       These elemental functions test if a character belongs to various
!!       subsets of the ASCII character set.
!!
!!       isalnum    returns .true. if character is a letter (a-z,A-Z)
!!                  or digit (0-9)
!!       isalpha    returns .true. if character is a letter and
!!                  .false. otherwise
!!       isascii    returns .true. if character is in the range char(0)
!!                  to char(127)
!!       isblank    returns .true. if character is a blank (space or
!!                  horizontal tab).
!!       iscntrl    returns .true. if character is a delete character or
!!                  ordinary control character (0x7F or 0x00-0x1F).
!!       isdigit    returns .true. if character is a digit (0,1,...,9)
!!                  and .false. otherwise
!!       isgraph    returns .true. if character is a printable ASCII
!!                  character excluding space
!!       islower    returns .true. if character is a miniscule letter (a-z)
!!       isprint    returns .true. if character is a printable ASCII character
!!       ispunct    returns .true. if character is a printable punctuation
!!                  character (isgraph(c) && !isalnum(c)).
!!       isspace    returns .true. if character is a null, space, tab,
!!                  carriage return, new line, vertical tab, or formfeed
!!       isupper    returns .true. if character is an uppercase letter (A-Z)
!!       isxdigit   returns .true. if character is a hexadecimal digit
!!                  (0-9, a-f, or A-F).
!!
!!##EXAMPLES
!!
!!   Sample Program:
!!
!!    program demo_isdigit
!!
!!     use M_strings, only : isdigit, isspace, switch
!!     implicit none
!!     character(len=10),allocatable :: string(:)
!!     integer                       :: i
!!        string=[&
!!        & '1 2 3 4 5 ' ,&
!!        & 'letters   ' ,&
!!        & '1234567890' ,&
!!        & 'both 8787 ' ]
!!        ! if string is nothing but digits and whitespace return .true.
!!        do i=1,size(string)
!!           write(*,'(a)',advance='no')'For string['//string(i)//']'
!!           write(*,*) &
!!           all(isdigit(switch(string(i))) .or. &
!!           & isspace(switch(string(i))))
!!        enddo
!!
!!     end program demo_isdigit
!!
!!   Expected output:
!!
!!    For string[1 2 3 4 5 ] T
!!    For string[letters   ] F
!!    For string[1234567890] T
!!    For string[both 8787 ] F
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
elemental function isalnum(ch) result(res)

! ident_72="@(#) M_strings isalnum(3f) returns true if character is a letter (a-z A-Z) or digit(0-9)"

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
!>
!!##NAME
!!    base(3f) - [M_strings:BASE] convert whole number string in base [2-36]
!!    to string in alternate base [2-36]
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   logical function base(x,b,y,a)
!!
!!    character(len=*),intent(in)  :: x
!!    character(len=*),intent(out) :: y
!!    integer,intent(in)           :: b,a
!!##DESCRIPTION
!!
!!    Convert a numeric string from base B to base A. The function returns
!!    FALSE if B is not in the range [2..36] or if string X contains invalid
!!    characters in base B or if result Y is too big
!!
!!    The letters A,B,...,Z represent 10,11,...,36 in the base > 10.
!!
!!##OPTIONS
!!    x   input string representing numeric whole value
!!    b   assumed base of input string
!!    y   output string
!!    a   base specified for output string
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_base
!!    use M_strings, only : base
!!    implicit none
!!    integer           :: ba,bd
!!    character(len=40) :: x,y
!!
!!    print *,' BASE CONVERSION'
!!    write(*,'("Start   Base (2 to 36): ")',advance='no'); read *, bd
!!    write(*,'("Arrival Base (2 to 36): ")',advance='no'); read *, ba
!!    INFINITE: do
!!       write(*,'("Enter number in start base (0 to quit): ")',advance='no')
!!       read *, x
!!       if(x == '0') exit INFINITE
!!       if(base(x,bd,y,ba))then
!!            write(*,'("In base ",I2,": ",A20)')  ba, y
!!        else
!!          print *,'Error in decoding/encoding number.'
!!        endif
!!     enddo INFINITE
!!
!!     end program demo_base
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
logical function base(x,b,y,a)
implicit none
character(len=*),intent(in)  :: x
character(len=*),intent(out) :: y
integer,intent(in)           :: b,a
integer                      :: temp

! ident_73="@(#) M_strings base(3f) convert whole number string in base [2-36] to string in alternate base [2-36]"

base=.true.
if(decodebase(x,b,temp)) then
   if(codebase(temp,a,y)) then
   else
      print *,'Error in coding number.'
      base=.false.
   endif
else
   print *,'Error in decoding number.'
   base=.false.
endif

end function base
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    base2(3f) - [M_strings:BASE] convert whole number to string in base 2
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   logical function base2(int)
!!
!!    integer,intent(in)           :: int
!!    character(len=:),allocatable :: base2
!!##DESCRIPTION
!!
!!    Convert a whole number to a string in base 2.
!!
!!    The letters A,B,...,Z represent 10,11,...,36 in the base > 10.
!!
!!##OPTIONS
!!    int   input string representing numeric whole value
!!##RETURNS
!!    base2   string representing input value in base 2
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_base2
!!    use M_strings, only : base2
!!    implicit none
!!       write(*,'(a)') base2(huge(0))
!!       write(*,'(a)') base2(0)
!!       write(*,'(a)') base2(1-huge(0))
!!    end program demo_base2
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
! 0 in binary: 0
! 42 in binary: 101010
! huge(int) in binary: 1111111111111111111111111111111
! 032 in binary is 100000
! itimes=10000000
!      G_TRICK=base2_f(32)   <BASE2_F  >Processor Time =  0.766 seconds.
!      G_TRICK=base2_fdo(32) <BASE2_FDO>Processor Time =  0.958 seconds.
!      G_TRICK=base2_a(32)   <BASE2_A  >Processor Time =  1.022 seconds.
!      G_TRICK=base2_c(32)   <BASE2_C  >Processor Time =  7.208 seconds.
!      G_TRICK=empty(32)     <EMPTY    >Processor Time =  0.132 seconds.
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function base2(x) result(str)
!  return string representing number as a binary number.  Fixed-length string:
integer, intent(in) :: x
integer           :: i
character(len=max(1,bit_size(x)-leadz(x))) :: str
    associate(n => len(str))
      str = repeat('0',n)
      do i = 0,n-1
        if (btest(x,i)) str(n-i:n-i) = '1'
      end do
    end associate
end function base2
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function base2_fdo(x) result(str)
!  return string representing number as a binary number.  Fixed-length string: do concurrent
integer, intent(in) :: x
character(len=max(1,bit_size(x)-leadz(x))) :: str

integer :: n, i

    if (x == 0) then
      str(1:1) = '0'
      return
    endif
    n = len(str)
    str = repeat('0',n)
    do concurrent (i = 0:n-1, btest(x,i))
      str(n-i:n-i) = '1'
    end do
end function base2_fdo
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function base2_a(x) result(str)
!  return string representing number as a binary number. Allocatable-length string:
integer, intent(in) :: x
character(len=:), allocatable :: str

integer :: n, i

    n = max(1,bit_size(x)-leadz(x))
    allocate(character(len=n) :: str)
    if (x == 0) then
      str(1:1) = '0'
      return
    endif

    str = repeat('0',n)
    do concurrent (i = 0:n-1, btest(x,i))
      str(n-i:n-i) = '1'
    end do
end function base2_a
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function base2_c(x) result(str)
! internal write
integer, intent(in) :: x
character(len=max(1,bit_size(x)-leadz(x))) :: str
    write( str, fmt="(b0)" ) x
end function base2_c
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!
!!    decodebase(3f) - [M_strings:BASE] convert whole number string in base
!!    [2-36] to base 10 number
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   logical function decodebase(string,basein,out10)
!!
!!    character(len=*),intent(in)  :: string
!!    integer,intent(in)           :: basein
!!    integer,intent(out)          :: out10
!!##DESCRIPTION
!!
!!    Convert a numeric string representing a whole number in base BASEIN
!!    to base 10. The function returns FALSE if BASEIN is not in the range
!!    [2..36] or if string STRING contains invalid characters in base BASEIN
!!    or if result OUT10 is too big
!!
!!    The letters A,B,...,Z represent 10,11,...,36 in the base > 10.
!!
!!##OPTIONS
!!    string   input string. It represents a whole number in
!!             the base specified by BASEIN unless BASEIN is set
!!             to zero. When BASEIN is zero STRING is assumed to
!!             be of the form BASE#VALUE where BASE represents
!!             the function normally provided by BASEIN.
!!    basein   base of input string; either 0 or from 2 to 36.
!!    out10    output value in base 10
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_decodebase
!!    use M_strings, only : codebase, decodebase
!!    implicit none
!!    integer           :: ba,bd
!!    character(len=40) :: x,y
!!    integer           :: r
!!
!!    print *,' BASE CONVERSION'
!!    write(*,'("Start   Base (2 to 36): ")',advance='no'); read *, bd
!!    write(*,'("Arrival Base (2 to 36): ")',advance='no'); read *, ba
!!    INFINITE: do
!!       print *,''
!!       write(*,'("Enter number in start base: ")',advance='no'); read *, x
!!       if(x == '0') exit INFINITE
!!       if(decodebase(x,bd,r)) then
!!          if(codebase(r,ba,y)) then
!!            write(*,'("In base ",I2,": ",A20)')  ba, y
!!          else
!!            print *,'Error in coding number.'
!!          endif
!!       else
!!          print *,'Error in decoding number.'
!!       endif
!!    enddo INFINITE
!!
!!    end program demo_decodebase
!!
!!##AUTHOR
!!    John S. Urban
!!
!!       Ref.: "Math matiques en Turbo-Pascal by
!!              M. Ducamp and A. Reverchon (2),
!!              Eyrolles, Paris, 1988".
!!
!!    based on a F90 Version By J-P Moreau (www.jpmoreau.fr)
!!
!!##LICENSE
!!    Public Domain
logical function decodebase(string,basein,out_baseten)
implicit none

! ident_74="@(#) M_strings decodebase(3f) convert whole number string in base [2-36] to base 10 number"

character(len=*),intent(in)  :: string
integer,intent(in)           :: basein
integer,intent(out)          :: out_baseten

character(len=len(string))   :: string_local
integer           :: long, i, j, k
real              :: y
real              :: mult
character(len=1)  :: ch
real,parameter    :: XMAXREAL=real(huge(1))
integer           :: out_sign
integer           :: basein_local
integer           :: ipound
integer           :: ierr

  string_local=upper(trim(adjustl(string)))
  decodebase=.false.

  ipound=index(string_local,'#')                                       ! determine if in form [-]base#whole
  if(basein == 0.and.ipound > 1)then                                  ! split string into two values
     call string_to_value(string_local(:ipound-1),basein_local,ierr)   ! get the decimal value of the base
     string_local=string_local(ipound+1:)                              ! now that base is known make string just the value
     if(basein_local >= 0)then                                         ! allow for a negative sign prefix
        out_sign=1
     else
        out_sign=-1
     endif
     basein_local=abs(basein_local)
  else                                                                 ! assume string is a simple positive value
     basein_local=abs(basein)
     out_sign=1
  endif

  out_baseten=0
  y=0.0
  ALL: if(basein_local<2.or.basein_local>36) then
    print *,'(*decodebase* ERROR: Base must be between 2 and 36. base=',basein_local
  else ALL
     out_baseten=0;y=0.0; mult=1.0
     long=LEN_TRIM(string_local)
     do i=1, long
        k=long+1-i
        ch=string_local(k:k)
        if(ch == '-'.and.k == 1)then
           out_sign=-1
           cycle
        endif
        if(ch<'0'.or.ch>'Z'.or.(ch>'9'.and.ch<'A'))then
           write(*,*)'*decodebase* ERROR: invalid character ',ch
           exit ALL
        endif
        if(ch<='9') then
              j=IACHAR(ch)-IACHAR('0')
        else
              j=IACHAR(ch)-IACHAR('A')+10
        endif
        if(j>=basein_local)then
           exit ALL
        endif
        y=y+mult*j
        if(mult>XMAXREAL/basein_local)then
           exit ALL
        endif
        mult=mult*basein_local
     enddo
     decodebase=.true.
     out_baseten=nint(out_sign*y)*sign(1,basein)
  endif ALL
end function decodebase
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    codebase(3f) - [M_strings:BASE] convert whole number in base 10 to
!!    string in base [2-36]
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   logical function codebase(in_base10,out_base,answer)
!!
!!    integer,intent(in)           :: in_base10
!!    integer,intent(in)           :: out_base
!!    character(len=*),intent(out) :: answer
!!
!!##DESCRIPTION
!!    Convert a number from base 10 to base OUT_BASE. The function returns
!!    .FALSE. if OUT_BASE is not in [2..36] or if number IN_BASE10 is
!!    too big.
!!
!!    The letters A,B,...,Z represent 10,11,...,36 in the base > 10.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_codebase
!!    use M_strings, only : codebase
!!    implicit none
!!    character(len=20) :: answer
!!    integer           :: i, j
!!    logical           :: ierr
!!    do j=1,100
!!       do i=2,36
!!          ierr=codebase(j,i,answer)
!!          write(*,*)'VALUE=',j,' BASE=',i,' ANSWER=',answer
!!       enddo
!!    enddo
!!    end program demo_codebase
!!
!!##AUTHOR
!!    John S. Urban
!!
!!     Ref.: "Math matiques en Turbo-Pascal by
!!            M. Ducamp and A. Reverchon (2),
!!            Eyrolles, Paris, 1988".
!!
!!    based on a F90 Version By J-P Moreau (www.jpmoreau.fr)
!!
!!##LICENSE
!!    Public Domain
logical function codebase(inval10,outbase,answer)
implicit none

! ident_75="@(#) M_strings codebase(3f) convert whole number in base 10 to string in base [2-36]"

integer,intent(in)           :: inval10
integer,intent(in)           :: outbase
character(len=*),intent(out) :: answer
integer                      :: n
real                         :: inval10_local
integer                      :: outbase_local
integer                      :: in_sign
  answer=''
  in_sign=sign(1,inval10)*sign(1,outbase)
  inval10_local=abs(inval10)
  outbase_local=abs(outbase)
  if(outbase_local<2.or.outbase_local>36) then
    print *,'*codebase* ERROR: base must be between 2 and 36. base was',outbase_local
    codebase=.false.
  else
     do while(inval10_local>0.0 )
        n=INT(inval10_local-outbase_local*INT(inval10_local/outbase_local))
        if(n<10) then
           answer=ACHAR(IACHAR('0')+n)//answer
        else
           answer=ACHAR(IACHAR('A')+n-10)//answer
        endif
        inval10_local=INT(inval10_local/outbase_local)
     enddo
     codebase=.true.
  endif
  if(in_sign == -1)then
     answer='-'//trim(answer)
  endif
  if(answer == '')then
     answer='0'
  endif
end function codebase
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function todecimal(base, instr)

! ident_76="@(#) M_strings todecimal(3f) given string and base return decimal integer"

! based on an example at rosetta code.
character(len=36),parameter  :: alphanum = "0123456789abcdefghijklmnopqrstuvwxyz"
integer,intent(in)           :: base
character(*),intent(in)      :: instr
character(len=:),allocatable :: instr_local
integer                      :: todecimal
integer                      :: length, i, n

   instr_local=trim(lower(instr))
   todecimal = 0
   length = len(instr_local)
   do i = 1, length
      n = index(alphanum, instr_local(i:i)) - 1
      n = n * base**(length-i)
      todecimal = todecimal + n
   enddo
end function todecimal
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function tobase(base, number)

! ident_77="@(#) M_strings tobase(3f) given integer and base return string"

! based on an example at rosetta code.
character(len=36),parameter  :: alphanum = "0123456789abcdefghijklmnopqrstuvwxyz"
integer,intent(in)           :: base
integer,intent(in)           :: number
character(len=:),allocatable :: tobase
character(len=31)            :: holdit
integer                      :: number_local, i, rem
   number_local=number

   holdit = "                               "
   do i = 31, 1, -1
      if(number_local < base) then
         holdit(i:i) = alphanum(number_local+1:number_local+1)
         exit
      endif
      rem = mod(number_local, base)
      holdit(i:i) = alphanum(rem+1:rem+1)
      number_local = number_local / base
   enddo
   tobase = adjustl(holdit)
end function tobase

!SUBROUTINE DectoBase(decimal, string, base)
! CHARACTER string
!    string = '0'
!    temp = decimal
!    length = CEILING( LOG(decimal+1, base) )   !<<<<<<<< INTERESTING
!    DO i = length, 1, -1
!      n = MOD( temp, base )
!      string(i) = "0123456789abcdefghijklmnopqrstuvwxyz"(n+1)
!      temp = INT(temp / base)
!    ENDDO
! END
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    paragraph(3f) - [M_strings:TOKENS] break a long line into a paragraph
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   function paragraph(source_string,length)
!!
!!    character(len=*),intent(in)       :: source_string
!!    integer,intent(in)                :: length
!!    character(allocatable(len=length)    :: paragraph(:)
!!
!!##DESCRIPTION
!!    paragraph(3f) breaks a long line into a simple paragraph of specified
!!    line length.
!!
!!    Given a long string break it on spaces into an array such that no
!!    variable is longer than the specified length. Individual words longer
!!    than LENGTH will be placed in variables by themselves.
!!
!!##OPTIONS
!!     SOURCE_STRING  input string to break into an array of shorter strings
!!                    on blank delimiters
!!     LENGTH         length of lines to break the string into.
!!
!!##RETURNS
!!     PARAGRAPH  character array filled with data from source_string
!!                broken at spaces into variables of length LENGTH.
!!
!!##EXAMPLE
!!
!!  sample program
!!
!!    program demo_paragraph
!!    use M_strings, only : paragraph
!!    implicit none
!!    character(len=:),allocatable :: paragrph(:)
!!    character(len=*),parameter    :: string= '&
!!     &one two three four five &
!!     &six seven eight &
!!     &nine ten eleven twelve &
!!     &thirteen fourteen fifteen sixteen &
!!     &seventeen'
!!
!!    write(*,*)'LEN=',len(string)
!!    write(*,*)'INPUT:'
!!    write(*,*)string
!!
!!    paragrph=paragraph(string,40)
!!    write(*,*)'LEN=',len(paragrph),' SIZE=',size(paragrph)
!!    write(*,*)'OUTPUT:'
!!    write(*,'(a)')paragrph
!!
!!    write(*,'(a)')paragraph(string,0)
!!    write(*,'(3x,a)')paragraph(string,47)
!!
!!    end program demo_paragraph
!!
!!   Results:
!!
!!     LEN=         106
!!     INPUT:
!!     one two three four five six seven eight nine ten eleven twelve
!!     thirteen fourteen fifteen sixteen seventeen
!!     LEN=          40  SIZE=           3
!!     OUTPUT:
!!    one two three four five six seven eight
!!    nine ten eleven twelve thirteen fourteen
!!    fifteen sixteen seventeen
!!    one
!!    two
!!    three
!!    four
!!    five
!!    six
!!    seven
!!    eight
!!    nine
!!    ten
!!    eleven
!!    twelve
!!    thirteen
!!    fourteen
!!    fifteen
!!    sixteen
!!    seventeen
!!       one two three four five six seven eight nine
!!       ten eleven twelve thirteen fourteen fifteen
!!       sixteen seventeen
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
function paragraph(source_string,length)

! ident_78="@(#) M_strings paragraph(3f) wrap a long string into a paragraph"

character(len=*),intent(in)       :: source_string
integer,intent(in)                :: length
integer                           :: itoken
integer                           :: istart
integer                           :: iend
character(len=*),parameter        :: delimiters=' '
character(len=:),allocatable      :: paragraph(:)
integer                           :: ilines
integer                           :: ilength
integer                           :: iword, iword_max
integer                           :: i
!-----------------------------------------------------------------------------------------------------------------------------------
!  parse string once to find out how big to make the returned array, then redo everything but store the data
!  could store array of endpoints and leave original whitespace alone or many other options
   do i=1,2
      iword_max=0                                  ! length of longest token
      ilines=1                                     ! number of output line output will go on
      ilength=0                                    ! length of output line so far
      itoken=0                                     ! must set ITOKEN=0 before looping on strtok(3f) on a new string.
      do while ( strtok(source_string,itoken,istart,iend,delimiters) )
         iword=iend-istart+1
         iword_max=max(iword_max,iword)
         if(iword > length)then                   ! this token is longer than the desired line length so put it on a line by itself
            if(ilength /= 0)then
               ilines=ilines+1
            endif
            if(i == 2)then     ! if paragraph has been allocated store data, else just gathering data to determine size of paragraph
               paragraph(ilines)=source_string(istart:iend)//' '
            endif
            ilength=iword+1
         elseif(ilength+iword <= length)then       ! this word will fit on current line
            if(i == 2)then
               paragraph(ilines)=paragraph(ilines)(:ilength)//source_string(istart:iend)
            endif
            ilength=ilength+iword+1
         else                                      ! adding this word would make line too long so start new line
            ilines=ilines+1
            ilength=0
            if(i == 2)then
               paragraph(ilines)=paragraph(ilines)(:ilength)//source_string(istart:iend)
            endif
            ilength=iword+1
         endif
      enddo
      if(i==1)then                                 ! determined number of lines needed so allocate output array
         allocate(character(len=max(length,iword_max)) :: paragraph(ilines))
         paragraph=' '
      endif
   enddo
   paragraph=paragraph(:ilines)
end function paragraph
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
function setbits8(string) result(answer)
use, intrinsic :: iso_fortran_env, only : int8, int16, int32, int64
use, intrinsic :: iso_fortran_env, only : stdin=>input_unit, stdout=>output_unit, stderr=>error_unit
implicit none
integer(kind=int8)          :: answer
character(len=8),intent(in) :: string
integer                     :: pos
integer                     :: lgth
   answer=0_int8
   lgth=len(string)
   if(lgth /= bit_size(answer))then
      write(stderr,*)'*setbits8* wrong string length =',lgth
      lgth=min(lgth,int(bit_size(answer)))
   endif
   do pos=1,lgth
      select case(string(pos:pos))
       case('1')
         answer = ibset(answer, pos-1)
       case('0')
         answer = ibclr(answer, pos-1)
       case default
         write(stderr,*)'*setbits8* unknown value. must be 0 or 1. found [',string(pos:pos),'] at position ',pos,' in ',string
      end select
   enddo
end function setbits8
!-----------------------------------------------------------------------------------------------------------------------------------
function setbits16(string) result(answer)
use, intrinsic :: iso_fortran_env, only : int8, int16, int32, int64
use, intrinsic :: iso_fortran_env, only : stdin=>input_unit, stdout=>output_unit, stderr=>error_unit
implicit none
integer(kind=int16)          :: answer
character(len=16),intent(in) :: string
integer                      :: pos
integer                      :: lgth
   answer=0_int16
   lgth=len(string)
   if(lgth /= bit_size(answer))then
      write(stderr,*)'*setbits16* wrong string length =',lgth
      lgth=min(lgth,int(bit_size(answer)))
   endif
   do pos=1,len(string)
      select case(string(pos:pos))
       case('1')
         answer = ibset(answer, pos-1)
       case('0')
         answer = ibclr(answer, pos-1)
       case default
         write(stderr,*)'*setbits16* unknown value. must be 0 or 1. found [',string(pos:pos),'] at position ',pos,' in ',string
      end select
   enddo
end function setbits16
!-----------------------------------------------------------------------------------------------------------------------------------
function setbits32(string) result(answer)
use, intrinsic :: iso_fortran_env, only : int8, int16, int32, int64
use, intrinsic :: iso_fortran_env, only : stdin=>input_unit, stdout=>output_unit, stderr=>error_unit
implicit none
integer(kind=int32)          :: answer
character(len=32),intent(in) :: string
integer                      :: pos
integer                      :: lgth
   answer=0_int32
   lgth=len(string)
   if(lgth /= bit_size(answer))then
      write(stderr,*)'*setbits32* wrong string length =',lgth
      lgth=min(lgth,int(bit_size(answer)))
   endif
   do pos=1,len(string)
      select case(string(pos:pos))
       case('1')
         answer = ibset(answer, pos-1)
       case('0')
         answer = ibclr(answer, pos-1)
       case default
         write(stderr,*)'*setbits32* unknown value. must be 0 or 1. found [',string(pos:pos),'] at position ',pos,' in ',string
      end select
   enddo
end function setbits32
!-----------------------------------------------------------------------------------------------------------------------------------
function setbits64(string) result(answer)
use, intrinsic :: iso_fortran_env, only : int8, int16, int32, int64
use, intrinsic :: iso_fortran_env, only : stdin=>input_unit, stdout=>output_unit, stderr=>error_unit
implicit none
integer(kind=int64)          :: answer
character(len=64),intent(in) :: string
integer                      :: pos
integer                      :: lgth
   answer=0_int64
   lgth=len(string)
   if(lgth /= bit_size(answer))then
      write(stderr,*)'*setbits64* wrong string length =',lgth
      lgth=min(lgth,int(bit_size(answer)))
   endif
   do pos=1,len(string)
      select case(string(pos:pos))
       case('1')
         answer = ibset(answer, pos-1)
       case('0')
         answer = ibclr(answer, pos-1)
       case default
         write(stderr,*)'*setbits64* unknown value. must be 0 or 1. found [',string(pos:pos),'] at position ',pos,' in ',string
      end select
   enddo
end function setbits64
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!     msg(3f) - [M_strings] converts any standard scalar type to a string
!!     (LICENSE:PD)
!!##SYNOPSIS
!!
!!
!!     function msg(g1,g2g3,g4,g5,g6,g7,g8,g9,sep)
!!
!!      class(*),intent(in),optional  :: g1,g2,g3,g4,g5,g6,g7,g8,g9
!!      character(len=*),intent(in),optional :: sep
!!      character(len=:),allocatable :: msg
!!
!!##DESCRIPTION
!!     msg(3f) builds a space-separated string from up to nine scalar values.
!!
!!##OPTIONS
!!     g[1-9]  optional value to print the value of after the message. May
!!             be of type INTEGER, LOGICAL, REAL, DOUBLEPRECISION, COMPLEX,
!!             or CHARACTER.
!!     sep     separator between values. Defaults to a space
!!
!!##RETURNS
!!     msg     description to print
!!
!!##EXAMPLES
!!
!!
!!   Sample program:
!!
!!        program demo_msg
!!        use M_strings, only : msg
!!        implicit none
!!        character(len=:),allocatable :: pr
!!        character(len=:),allocatable :: frmt
!!        integer                      :: biggest
!!
!!        pr=msg('HUGE(3f) integers',huge(0),&
!!        & 'and real',huge(0.0),'and double',huge(0.0d0))
!!        write(*,'(a)')pr
!!        pr=msg('real            :',&
!!         & huge(0.0),0.0,12345.6789,tiny(0.0) )
!!        write(*,'(a)')pr
!!        pr=msg('doubleprecision :',&
!!         & huge(0.0d0),0.0d0,12345.6789d0,tiny(0.0d0) )
!!        write(*,'(a)')pr
!!        pr=msg('complex         :',&
!!         & cmplx(huge(0.0),tiny(0.0)) )
!!        write(*,'(a)')pr
!!
!!        ! create a format on the fly
!!        biggest=huge(0)
!!        frmt=msg('(*(i',int(log10(real(biggest))),':,1x))',sep='')
!!        write(*,*)'format=',frmt
!!
!!        ! although it will often work, using msg(3f) in an I/O statement
!!        ! is not recommended
!!        write(*,*)msg('program will now stop')
!!
!!        end program demo_msg
!!
!!   Output
!!
!!       HUGE(3f) integers 2147483647 and real 3.40282347E+38
!!       and double 1.7976931348623157E+308
!!       real            : 3.40282347E+38 0.00000000
!!       12345.6787 1.17549435E-38
!!       doubleprecision : 1.7976931348623157E+308 0.0000000000000000
!!       12345.678900000001 2.2250738585072014E-308
!!       complex         : (3.40282347E+38,1.17549435E-38)
!!        format=(*(i9:,1x))
!!        program will now stop
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
!===================================================================================================================================
function msg_scalar(generic1, generic2, generic3, generic4, generic5, generic6, generic7, generic8, generic9,sep)
implicit none

! ident_79="@(#) M_strings msg_scalar(3fp) writes a message to a string composed of any standard scalar types"

class(*),intent(in),optional  :: generic1 ,generic2 ,generic3 ,generic4 ,generic5
class(*),intent(in),optional  :: generic6 ,generic7 ,generic8 ,generic9
character(len=*),intent(in),optional :: sep
character(len=:),allocatable  :: sep_local
character(len=:), allocatable :: msg_scalar
character(len=4096)           :: line
integer                       :: istart
integer                       :: increment
   if(present(sep))then
      sep_local=sep
      increment=len(sep)+1
   else
      sep_local=' '
      increment=2
   endif

   istart=1
   line=' '
   if(present(generic1))call print_generic(generic1)
   if(present(generic2))call print_generic(generic2)
   if(present(generic3))call print_generic(generic3)
   if(present(generic4))call print_generic(generic4)
   if(present(generic5))call print_generic(generic5)
   if(present(generic6))call print_generic(generic6)
   if(present(generic7))call print_generic(generic7)
   if(present(generic8))call print_generic(generic8)
   if(present(generic9))call print_generic(generic9)
   msg_scalar=trim(line)
contains
!===================================================================================================================================
subroutine print_generic(generic)
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64, real32, real64, real128
class(*),intent(in) :: generic
   select type(generic)
      type is (integer(kind=int8));     write(line(istart:),'(i0)') generic
      type is (integer(kind=int16));    write(line(istart:),'(i0)') generic
      type is (integer(kind=int32));    write(line(istart:),'(i0)') generic
      type is (integer(kind=int64));    write(line(istart:),'(i0)') generic
      type is (real(kind=real32));      write(line(istart:),'(1pg0)') generic
      type is (real(kind=real64));      write(line(istart:),'(1pg0)') generic
      !x!type is (real(kind=real128));     write(line(istart:),'(1pg0)') generic
      !x!type is (real(kind=real256));     write(line(istart:),'(1pg0)') generic
      type is (logical);                write(line(istart:),'(l1)') generic
      type is (character(len=*));       write(line(istart:),'(a)') trim(generic)
      type is (complex);                write(line(istart:),'("(",1pg0,",",1pg0,")")') generic
   end select
   istart=len_trim(line)+increment
   line=trim(line)//sep_local
end subroutine print_generic
!===================================================================================================================================
end function msg_scalar
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function msg_one(generic1, generic2, generic3, generic4, generic5, generic6, generic7, generic8, generic9,sep)
implicit none

! ident_80="@(#) M_strings msg_one(3fp) writes a message to a string composed of any standard one dimensional types"

class(*),intent(in)           :: generic1(:)
class(*),intent(in),optional  :: generic2(:), generic3(:), generic4(:), generic5(:)
class(*),intent(in),optional  :: generic6(:), generic7(:), generic8(:), generic9(:)
character(len=*),intent(in),optional :: sep
character(len=:),allocatable   :: sep_local
character(len=:), allocatable :: msg_one
character(len=4096)           :: line
integer                       :: istart
integer                       :: increment
   if(present(sep))then
      sep_local=sep
      increment=len(sep)+1
   else
      sep_local=' '
      increment=2
   endif

   istart=1
   line=' '
   call print_generic(generic1)
   if(present(generic2))call print_generic(generic2)
   if(present(generic3))call print_generic(generic3)
   if(present(generic4))call print_generic(generic4)
   if(present(generic5))call print_generic(generic5)
   if(present(generic6))call print_generic(generic6)
   if(present(generic7))call print_generic(generic7)
   if(present(generic8))call print_generic(generic8)
   if(present(generic9))call print_generic(generic9)
   msg_one=trim(line)
contains
!===================================================================================================================================
subroutine print_generic(generic)
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64, real32, real64, real128
class(*),intent(in),optional :: generic(:)
integer :: i
   select type(generic)
      type is (integer(kind=int8));     write(line(istart:),'("[",*(i0,1x))') generic
      type is (integer(kind=int16));    write(line(istart:),'("[",*(i0,1x))') generic
      type is (integer(kind=int32));    write(line(istart:),'("[",*(i0,1x))') generic
      type is (integer(kind=int64));    write(line(istart:),'("[",*(i0,1x))') generic
      type is (real(kind=real32));      write(line(istart:),'("[",*(1pg0,1x))') generic
      type is (real(kind=real64));      write(line(istart:),'("[",*(1pg0,1x))') generic
      !x!type is (real(kind=real128));     write(line(istart:),'("[",*(1pg0,1x))') generic
      !x!type is (real(kind=real256));     write(line(istart:),'("[",*(1pg0,1x))') generic
      type is (logical);                write(line(istart:),'("[",*(l1,1x))') generic
      type is (character(len=*));       write(line(istart:),'("[",:*("""",a,"""",1x))') (trim(generic(i)),i=1,size(generic))
      type is (complex);                write(line(istart:),'("[",*("(",1pg0,",",1pg0,")",1x))') generic
   end select
   istart=len_trim(line)+increment
   line=trim(line)//"]"//sep_local
end subroutine print_generic
!===================================================================================================================================
end function msg_one
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    split2020(3f) - parse a string into tokens
!!
!!##SYNOPSIS
!!
!!   TOKEN form
!!
!!    subroutine split2020 (string, set, tokens, separator)
!!    character(len=*),intent(in) :: string
!!    character(len=*),intent(in) :: set
!!    character(len=:),allocatable,intent(out) :: tokens(:)
!!    character(len=1),allocatable,intent(out),optional :: separator(:)
!!
!!   BOUNDS ARRAY form
!!
!!    subroutine split2020 (string, set, first, last)
!!    character(len=*),intent(in) :: string
!!    character(len=*),intent(in) :: set
!!    integer,allocatable,intent(out) :: first(:)
!!    integer,allocatable,intent(out) :: last(:)
!!
!!   STEP THROUGH BY POSITION form
!!
!!    subroutine split2020 (string, set, pos [, back])
!!    character(len=*),intent(in) :: string
!!    character(len=*),intent(in) :: set
!!    integer,intent(inout)       :: pos
!!    logical,intent(in),optional :: back
!!
!!##DESCRIPTION
!!    Parse a string into tokens. STRING, SET, TOKENS and SEPARATOR must
!!    all be of the same CHARACTER kind type parameter.
!!
!!##OPTIONS
!!    STRING      string to break into tokens
!!
!!    SET         Each character in SET is a token delimiter. A
!!                sequence of zero or more characters in STRING delimited by
!!                any token delimiter, or the beginning or end of STRING,
!!                comprise a token. Thus, two consecutive token delimiters
!!                in STRING, or a token delimiter in the first or last
!!                character of STRING, indicate a token with zero length.
!!
!!                ??? how about if null defaults to all whitespace characters
!!
!!    TOKENS      It is allocated with the lower bound equal to
!!                one and the upper bound equal to the number of tokens in
!!                STRING, and with character length equal to the length of
!!                the longest token. The tokens in STRING are assigned by
!!                intrinsic assignment, in the order found, to the elements
!!                of TOKENS, in array element order.
!!
!!                ???If input is null it still must be of size 1?
!!
!!    SEPARATOR   Each element in SEPARATOR(i) is assigned the value of
!!                the ith token delimiter in STRING.
!!                It is allocated with the lower bound equal to
!!                one and the upper bound equal to one less than the number
!!                of tokens in STRING, and with character length equal to
!!                one.
!!
!!                ???one less than? '' ' '
!!
!!    FIRST     It is allocated with the lower bound equal to one and the
!!              upper bound equal to the number of tokens in STRING. Each
!!              element is assigned, in array element order, the starting
!!              position of each token in STRING, in the order found. If a
!!              token has zero length, the starting position is equal to one
!!              if the token is at the beginning of STRING, and one greater
!!              than the position of the preceding delimiter otherwise.
!!
!!    LAST      It is allocated with the lower bound equal to one and the
!!              upper bound equal to the number of tokens in STRING. Each
!!              element is assigned, in array element order, the ending
!!              position of each token in STRING, in the order found. If
!!              a token has zero length, the ending position is one less
!!              than the starting position.
!!
!!    POS       If BACK is present with the value .TRUE., the value
!!              of POS shall be in the range 0 < POS     LEN (STRING)+1;
!!              otherwise it shall be in the range 0     POS LEN (STRING).
!!
!!              If BACK is absent or is present with the value .FALSE., POS
!!              is assigned the position of the leftmost token delimiter in
!!              STRING whose position is greater than POS, or if there is
!!              no such character, it is assigned a value one greater than
!!              the length of STRING. This identifies a token with starting
!!              position one greater than the value of POS on invocation,
!!              and ending position one less than the value of POS on return.
!!
!!              If BACK is present with the value true, POS is assigned the
!!              position of the rightmost token delimiter in STRING whose
!!              position is less than POS, or if there is no such character,
!!              it is assigned the value zero. This identifies a token with
!!              ending position one less than the value of POS on invocation,
!!              and starting position one greater than the value of POS
!!              on return.
!!
!!              When SPLIT is invoked with a value for POS of
!!              1 <= POS <= LEN(STRING) and STRING(POS:POS) is not a
!!              token delimiter present in SET, the token identified by
!!              SPLIT does not comprise a complete token as described in the
!!              description of the SET argument, but rather a partial token.
!!
!!    BACK      shall be a logical scalar. It is an INTENT (IN) argument. If
!!              POS does not appear and BACK is present with the value true,
!!              STRING is scanned backwards for tokens starting from the
!!              end. If POS does not appear and BACK is absent or present
!!              with the value false, STRING is scanned forwards for tokens
!!              starting from the beginning.
!!
!!##EXAMPLES
!!
!! Sample of uses
!!
!!    program demo_sort2020
!!    use M_strings, only : split2020
!!    implicit none
!!    character(len=*),parameter :: gen='(*("[",g0,"]":,","))'
!!
!!     ! Execution of TOKEN form
!!     block
!!       character (len=:), allocatable :: string
!!       character (len=:), allocatable :: tokens(:)
!!       character (len=*),parameter :: set = " ,"
!!       string = 'first,second,third'
!!       call split2020(string, set, tokens )
!!       write(*,gen)tokens
!!
!!     ! assigns the value ['first ','second','third ' ]
!!     ! to TOKENS.
!!     endblock
!!
!!     ! Execution of BOUNDS form
!!
!!     block
!!       character (len=:), allocatable :: string
!!       character (len=*),parameter :: set = " ,"
!!       integer, allocatable        :: first(:), last(:)
!!       string =    'first,second,,forth'
!!       call split2020 (string, set, first, last)
!!       write(*,gen)first
!!       write(*,gen)last
!!
!!     ! will assign the value [ 1, 7, 14, 15 ] to FIRST,
!!     ! and the value [ 5, 12, 13, 19 ] to LAST.
!!     endblock
!!
!!     ! Execution of STEP form
!!     block
!!       character (len=:), allocatable :: string
!!       character (len=*),parameter :: set = " ,"
!!       integer :: p, istart, iend
!!       string = " one,   last  example  "
!!       do while (p < len(string))
!!         istart = p + 1
!!         call split2020 (string, set, p)
!!         iend=p-1
!!         if(iend > istart)then
!!            print '(t3,a,1x,i0,1x,i0)', string (istart:iend),istart,iend
!!         endif
!!       enddo
!!     endblock
!!    end program demo_sort2020
!!
!!   Results:
!!
!!    [first ],[second],[third ]
!!    [1],[7],[14],[15]
!!    [5],[12],[13],[19]
!!      one 2 4
!!      last 9 12
!!      example 15 21
!!
!!      > ??? option to skip adjacent delimiters (not return null tokens)
!!      >     common with whitespace
!!      > ??? quoted strings, especially CSV both " and ', Fortran adjacent
!!      >     is insert versus other rules
!!      > ??? escape character like \\ .
!!      > ??? multi-character delimiters like \\n, \\t,
!!      > ??? regular expression separator
!!
!!##AUTHOR
!!    Milan Curcic, "milancurcic@hey.com"
!!
!!##LICENSE
!!    MIT
!!
!!##VERSION
!!    version 0.1.0, copyright 2020, Milan Curcic
  pure subroutine split_tokens(string, set, tokens, separator)
     ! Splits a string into tokens using characters in set as token delimiters.
     ! If present, separator contains the array of token delimiters.
    character(*), intent(in) :: string
    character(*), intent(in) :: set
    character(:), allocatable, intent(out) :: tokens(:)
    character, allocatable, intent(out), optional :: separator(:)

    integer, allocatable :: first(:), last(:)
    integer :: n

    call split2020(string, set, first, last)
    allocate(character(len=maxval(last - first) + 1) :: tokens(size(first)))

    do concurrent (n = 1:size(tokens))
      tokens(n) = string(first(n):last(n))
    enddo

    if (present(separator)) then
      allocate(separator(size(tokens) - 1))
      do concurrent (n = 1:size(tokens) - 1)
        separator(n) = string(first(n+1)-1:first(n+1)-1)
      enddo
    endif

  end subroutine split_tokens
!===================================================================================================================================
  pure subroutine split_first_last(string, set, first, last)
     ! Computes the first and last indices of tokens in input string, delimited
     ! by the characters in set, and stores them into first and last output
     ! arrays.
    character(*), intent(in) :: string
    character(*), intent(in) :: set
    integer, allocatable, intent(out) :: first(:)
    integer, allocatable, intent(out) :: last(:)

    character :: set_array(len(set))
    logical, dimension(len(string)) :: is_first, is_last, is_separator
    integer :: n, slen

    slen = len(string)

    do concurrent (n = 1:len(set))
      set_array(n) = set(n:n)
    enddo

    do concurrent (n = 1:slen)
      is_separator(n) = any(string(n:n) == set_array)
    enddo

    is_first = .false.
    is_last = .false.

    if (.not. is_separator(1)) is_first(1) = .true.

    do concurrent (n = 2:slen-1)
      if (.not. is_separator(n)) then
        if (is_separator(n - 1)) is_first(n) = .true.
        if (is_separator(n + 1)) is_last(n) = .true.
      else
        if (is_separator(n - 1)) then
          is_first(n) = .true.
          is_last(n-1) = .true.
        endif
      endif
    enddo

    if (.not. is_separator(slen)) is_last(slen) = .true.

    first = pack([(n, n = 1, slen)], is_first)
    last = pack([(n, n = 1, slen)], is_last)

  end subroutine split_first_last
!===================================================================================================================================
  pure subroutine split_pos(string, set, pos, back)
     ! If back is absent, computes the leftmost token delimiter in string whose
     ! position is > pos. If back is present and true, computes the rightmost
     ! token delimiter in string whose position is < pos. The result is stored
     ! in pos.
    character(*), intent(in) :: string
    character(*), intent(in) :: set
    integer, intent(in out) :: pos
    logical, intent(in), optional :: back

    logical :: backward
    character :: set_array(len(set))
    integer :: n, result_pos

    !TODO use optval when implemented in stdlib
    !backward = optval(back, .false.)
    backward = .false.
    if (present(back)) backward = back

    do concurrent (n = 1:len(set))
      set_array(n) = set(n:n)
    enddo

    if (backward) then
      result_pos = 0
      do n = pos - 1, 1, -1
        if (any(string(n:n) == set_array)) then
          result_pos = n
          exit
        endif
      enddo
    else
      result_pos = len(string) + 1
      do n = pos + 1, len(string)
        if (any(string(n:n) == set_array)) then
          result_pos = n
          exit
        endif
      enddo
    endif

    pos = result_pos

  end subroutine split_pos
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
  pure function string_tokens(string, set) result(tokens)
     ! Splits a string into tokens using characters in set as token delimiters.
    character(*), intent(in) :: string
    character(*), intent(in) :: set
    character(:), allocatable :: tokens(:)
    call split_tokens(string, set, tokens)
  end function string_tokens
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function lowercase(str) result(lcstr)

! convert string to lower case leaving quoted strings as is

character (len=*):: str
character (len=len_trim(str)):: lcstr
integer :: lgth
integer :: ioffset
integer :: iquote
integer :: i
integer :: iav
integer :: iqc

lgth=len_trim(str)
ioffset=iachar('A')-iachar('a')
iquote=0
lcstr=str
do i=1,lgth
  iav=iachar(str(i:i))
  if(iquote==0 .and. (iav==34 .or.iav==39)) then
    iquote=1
    iqc=iav
    cycle
  endif
  if(iquote==1 .and. iav==iqc) then
    iquote=0
    cycle
  endif
  if (iquote==1) cycle
  if(iav >= iachar('A') .and. iav <= iachar('Z')) then
    lcstr(i:i)=achar(iav-ioffset)
  else
    lcstr(i:i)=str(i:i)
  endif
enddo

end function lowercase
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function uppercase(str) result(ucstr)

! convert string to upper case leaving quoted strings as is

character (len=*):: str
character (len=len_trim(str)):: ucstr
integer :: lgth
integer :: ioffset
integer :: iquote
integer :: i
integer :: iav
integer :: iqc

lgth=len_trim(str)
ioffset=iachar('A')-iachar('a')
iquote=0
ucstr=str
do i=1,lgth
  iav=iachar(str(i:i))
  if(iquote==0 .and. (iav==34 .or.iav==39)) then
    iquote=1
    iqc=iav
    cycle
  endif
  if(iquote==1 .and. iav==iqc) then
    iquote=0
    cycle
  endif
  if (iquote==1) cycle
  if(iav >= iachar('a') .and. iav <= iachar('z')) then
    ucstr(i:i)=achar(iav+ioffset)
  else
    ucstr(i:i)=str(i:i)
  endif
enddo

end function uppercase
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine matching_delimiter(str,ipos,imatch)

! Sets imatch to the position in string of the delimiter matching the delimiter
! in position ipos. Allowable delimiters are (), [], {}, <>.

character(len=*) :: str
character :: delim1,delim2,ch
integer :: ipos
integer :: imatch
integer :: lenstr
integer :: idelim2
integer :: istart, iend
integer :: inc
integer :: isum
integer :: i

lenstr=len_trim(str)
delim1=str(ipos:ipos)
select case(delim1)
   case('(')
      idelim2=iachar(delim1)+1
      istart=ipos+1
      iend=lenstr
      inc=1
   case(')')
      idelim2=iachar(delim1)-1
      istart=ipos-1
      iend=1
      inc=-1
   case('[','{','<')
      idelim2=iachar(delim1)+2
      istart=ipos+1
      iend=lenstr
      inc=1
   case(']','}','>')
      idelim2=iachar(delim1)-2
      istart=ipos-1
      iend=1
      inc=-1
   case default
      write(*,*) delim1,' is not a valid delimiter'
      return
end select
if(istart < 1 .or. istart > lenstr) then
   write(*,*) delim1,' has no matching delimiter'
   return
endif
delim2=achar(idelim2) ! matching delimiter

isum=1
do i=istart,iend,inc
   ch=str(i:i)
   if(ch /= delim1 .and. ch /= delim2) cycle
   if(ch == delim1) isum=isum+1
   if(ch == delim2) isum=isum-1
   if(isum == 0) exit
enddo
if(isum /= 0) then
   write(*,*) delim1,' has no matching delimiter'
   return
endif
imatch=i

end subroutine matching_delimiter
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    longest_common_substring(3f) - [M_strings] function that returns the
!!                                   longest common substring of two strings.
!!##SYNOPSIS
!!
!!    function longest_common_substring(a,b) result(match)
!!
!!     character(len=*),intent(in)  :: a, b
!!     character(len=:),allocatable :: match
!!##DESCRIPTION
!!    function that returns the longest common substring of two strings.
!!
!!    Note that substrings are consecutive characters within a string.
!!    This distinguishes them from subsequences, which is any sequence of
!!    characters within a string, even if there are extraneous characters in
!!    between them.
!!
!!    Hence, the longest common subsequence between "thisisatest" and
!!    "testing123testing" is "tsitest", whereas the longest common substring
!!    is just "test".
!!##OPTIONS
!!    a,b  strings to search for the longest common substring.
!!##RETURNS
!!    longest_common_substring  the longest common substring found
!!##EXAMPLE
!!
!!  Sample program
!!
!!    program demo_longest_common_substring
!!    use M_strings, only : longest_common_substring
!!    implicit none
!!       call compare('testing123testingthing','thisis',              'thi')
!!       call compare('testing',             'sting',               'sting')
!!       call compare('thisisatest_stinger', 'testing123testingthing','sting')
!!       call compare('thisisatest_stinger', 'thisis',              'thisis')
!!       call compare('thisisatest',         'testing123testing',   'test')
!!       call compare('thisisatest',         'thisisatest',         'thisisatest')
!!    contains
!!
!!    subroutine compare(a,b,answer)
!!    character(len=*),intent(in) :: a, b, answer
!!    character(len=:),allocatable :: match
!!    character(len=*),parameter :: g='(*(g0))'
!!    integer :: i
!!       match=longest_common_substring(a,b)
!!       write(*,g) 'comparing "',a,'" and "',b,'"'
!!       write(*,g) merge('(PASSED) "','(FAILED) "',answer == match), &
!!       & match,'"; expected "',answer,'"'
!!    end subroutine compare
!!
!!    end program demo_longest_common_substring
!!
!!   expected output
!!
!!    comparing "testing123testingthing" and "thisis"
!!    (PASSED) "thi"; expected "thi"
!!    comparing "testing" and "sting"
!!    (PASSED) "sting"; expected "sting"
!!    comparing "thisisatest_stinger" and "testing123testingthing"
!!    (PASSED) "sting"; expected "sting"
!!    comparing "thisisatest_stinger" and "thisis"
!!    (PASSED) "thisis"; expected "thisis"
!!    comparing "thisisatest" and "testing123testing"
!!    (PASSED) "test"; expected "test"
!!    comparing "thisisatest" and "thisisatest"
!!    (PASSED) "thisisatest"; expected "thisisatest"
function longest_common_substring(a,b) result(match)
character(len=*),intent(in)  :: a, b
character(len=:),allocatable :: match
character(len=:),allocatable :: a2, b2
integer :: left, foundat, len_a, i
   if(len(a) < len(b))then ! to reduce required comparisions look for shortest string in longest string
      a2=a
      b2=b
   else
      a2=b
      b2=a
   endif

   match=''

   do i=1,len(a2)-1
      len_a=len(a2)
      do left=1,len_a
         foundat=index(b2,a2(left:))
         if(foundat /= 0.and.len(match) < len_a-left+1)then
            if(len(a2(left:)) > len(match))then
               match=a2(left:)
               exit
            endif
         endif
      enddo

      if(len(a2) < len(match))exit
      a2=a2(:len(a2)-1)

   enddo

end function longest_common_substring
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
end module M_strings
