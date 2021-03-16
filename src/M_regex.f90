










!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!      M_regex(3fm) - [M_regex] Fortran interface to POSIX 1003.2 regular expression library using ISO_C_BINDING.
!!##SYNOPSIS
!!
!!      use M_regex, only : regcomp(3f), regexec(3f), regerror(3f), regfree(3f)
!!      use M_regex, only : regmatch(3f), regsub(3f)
!!
!!##DESCRIPTION
!!
!!    These routines interface with the C implementation of IEEE Std 1003.2 ( POSIX.2 ) RE (Regular
!!    Expressions).
!!
!!    o The regcomp(3c) function compiles a RE string into an internal form
!!    o regexec(3c) matches that internal form against a string and reports results
!!    o regerror(3c) transforms error codes from either into human-readable messages
!!    o and regfree(3c) frees any dynamically-allocated storage used by the internal form of an RE.
!!
!!    The Fortran interface is composed of wrapper routines that call the C
!!    library, plus some extensions (ie. regmatch(3f), regsub(3f)). See the
!!    C documentation for further details about implementation, performance,
!!    and limitations.
!!
!!    The following constructs are recognized in a ERE (Extended Regular Expression):
!!
!!        .     Matches any character except newline.
!!        *     (postfix) Matches the preceding expression zero, one or several times
!!        +     (postfix) Matches the preceding expression one or several times
!!        ?     (postfix) Matches the preceding expression once or not at all
!!        [..]  Character set. Ranges are denoted with - , as in [a-z] .
!!              An initial ^ , as in [^0-9] , complements the set. To
!!              include a ] character in a set, make it the first character
!!              of the set. To include a - character in a set, make it
!!              the first or the last character of the set.
!!
!!        ^        Matches at beginning of line: either at the beginning
!!                 of the matched string, or just after a '\n' character.
!!        $        Matches at end of line: either at the end of the matched
!!                 string, or just before a '\n' character.
!!        |        (infix) Alternative between two expressions.
!!        (..)     Grouping and naming of the enclosed expression.
!!        \1       The text matched by the first \(...\) expression (
!!                 \2 for the second expression, and so on up to \9 ).
!!        \b       Matches word boundaries.
!!        \        Quotes special characters. The special characters are $^\.*+?[] .
!!
!!##DESCRIPTION
!!
!!    Regular expressions ( REs ), as defined in IEEE Std 1003.2
!!    ( POSIX.2 ), come in two forms: modern REs (roughly those of
!!    egrep(1); 1003.2 calls these extended REs or ERE) and obsolete REs
!!    (roughly those of ed(1); 1003.2 basic REs or BRE). BRE mostly
!!    exist for backward compatibility in some old programs; they will be
!!    discussed at the end. IEEE Std 1003.2 ( POSIX.2 ) leaves some
!!    aspects of RE syntax and semantics open; decisions on
!!    these aspects mean that conforming implementations of IEEE Std 1003.2
!!    ( POSIX.2 ) may not be completely compatible.
!!
!!    A (modern) RE is one or more non-empty branches, separated by |.
!!    It matches anything that matches one of the branches.
!!
!!    A branch is one or more pieces, concatenated. It matches a match
!!    for the first, followed by a match for the second, etc.
!!
!!    A piece is an atom possibly followed by a single * , + , ? ,
!!    or bound. An atom followed by * matches a sequence
!!    of 0 or more matches of the atom. An atom followed by + matches
!!    a sequence of 1 or more matches of the atom. An atom followed by ?
!!    matches a sequence of 0 or 1 matches of the atom.
!!
!!    A bound is { followed by an unsigned decimal integer, possibly
!!    followed by , possibly followed by another unsigned decimal
!!    integer, always followed by } . The integers must lie between
!!    0 and RE_DUP_MAX (255 ) inclusive, and if there are two of them,
!!    the first may not exceed the second. An atom followed by a bound
!!    containing one integer i and no comma matches a sequence of exactly
!!    i matches of the atom. An atom followed by a bound containing one
!!    integer i and a comma matches a sequence of i or more matches of the
!!    atom. An atom followed by a bound containing two integers i and j
!!    matches a sequence of i through j (inclusive) matches of the atom.
!!
!!    An atom is a regular expression enclosed in () (matching a match
!!    for the regular expression), an empty set of () (matching the null
!!    string) , a bracket expression (see below), . (matching any
!!    single character), ^ (matching the null string at the beginning
!!    of a line), $ (matching the null string at the end of a line), a
!!    \\ followed by one of the characters ^.[$()|*+?{\ (matching
!!    that character taken as an ordinary character), a \ followed
!!    by any other character (matching that character taken as an ordinary
!!    character, as if the \ had not been present ), or a single
!!    character with no other significance (matching that character). A {
!!    followed by a character other than a digit is an ordinary character,
!!    not the beginning of a bound . It is illegal to end an RE with
!!    \ .
!!
!!    A bracket expression is a list of characters enclosed in []
!!    . It normally matches any single character from the list (but see
!!    below). If the list begins with ^ , it matches any single
!!    character (but see below) not from the rest of the list. If two
!!    characters in the list are separated by - , this is shorthand
!!    for the full range of characters between those two (inclusive) in the
!!    collating sequence, e.g. [0-9] in ASCII matches any decimal
!!    digit. It is illegal for two ranges to share an endpoint, e.g.
!!    a-c-e . Ranges are very collating-sequence-dependent, and portable
!!    programs should avoid relying on them.
!!
!!    To include a literal ] in the list, make it the first character
!!    (following a possible ^ ). To include a literal - , make
!!    it the first or last character, or the second endpoint of a range. To
!!    use a literal - as the first endpoint of a range, enclose it in
!!    [. and .] to make it a collating element (see below). With
!!    the exception of these and some combinations using [ (see
!!    next paragraphs), all other special characters, including \ ,
!!    lose their special significance within a bracket expression.
!!
!!    Within a bracket expression, a collating element (a character,
!!    a multi-character sequence that collates as if it were a single
!!    character, or a collating-sequence name for either) enclosed in
!!    [. and .] stands for the sequence of characters of that
!!    collating element. The sequence is a single element of the bracket
!!    expression's list. A bracket expression containing a multi-character
!!    collating element can thus match more than one character, e.g. if the
!!    collating sequence includes a ch collating element, then the RE
!!    [[.ch.]]*c matches the first five characters of chchcc .
!!
!!    Within a bracket expression, a collating element enclosed in [=
!!    and =] is an equivalence class, standing for the sequences of
!!    characters of all collating elements equivalent to that one, including
!!    itself. (If there are no other equivalent collating elements, the
!!    treatment is as if the enclosing delimiters were [. and .]
!!    .) For example, if x and y are the members of an equivalence
!!    class, then [[=x=]] , [[=y=]] , and [xy] are all
!!    synonymous. An equivalence class may not be an endpoint of a range.
!!
!!    Within a bracket expression, the name of a character class enclosed in
!!    [: and :] stands for the list of all characters belonging
!!    to that class. Standard character class names are:
!!
!!          alnum    digit    punct
!!          alpha    graph    space
!!          blank    lower    upper
!!          cntrl    print    xdigit
!!
!!    These stand for the character classes defined in ctype(3). A locale
!!    may provide others. A character class may not be used as an endpoint
!!    of a range.
!!
!!    A bracketed expression like [[:class:]] can be used to match a single
!!    character that belongs to a character class. The reverse, matching
!!    any character that does not belong to a specific class, the negation
!!    operator of bracket expressions may be used: [^[:class:]] .
!!
!!    There are two special cases of bracket expressions: the bracket
!!    expressions [[:<:]] and [[:>:]] match the null string at the beginning
!!    and end of a word respectively. A word is defined as a sequence of word
!!    characters which is neither preceded nor followed by word characters. A
!!    word character is an alnum character (as defined by ctype(3)) or an
!!    underscore. This is an extension, compatible with but not specified
!!    by IEEE Std 1003.2 ( POSIX.2 ), and should be used with caution in
!!    software intended to be portable to other systems.
!!
!!    In the event that an RE could match more than one substring of a given
!!    string, the RE matches the one starting earliest in the string. If the RE
!!    could match more than one substring starting at that point, it matches
!!    the longest. Subexpressions also match the longest possible substrings,
!!    subject to the constraint that the whole match be as long as possible,
!!    with subexpressions starting earlier in the RE taking priority over
!!    ones starting later. Note that higher-level subexpressions thus take
!!    priority over their lower-level component subexpressions.
!!
!!    Match lengths are measured in characters, not collating elements. A
!!    null string is considered longer than no match at all. For example, bb*
!!    matches the three middle characters of abbbc , (wee|week)(knights|nights)
!!    matches all ten characters of weeknights, when (.*).* is matched against
!!    abc the parenthesized subexpression matches all three characters, and
!!    when (a*)* is matched against bc both the whole RE and the parenthesized
!!    subexpression match the null string.
!!
!!    If case-independent matching is specified, the effect is much as if all
!!    case distinctions had vanished from the alphabet. When an alphabetic
!!    that exists in multiple cases appears as an ordinary character outside
!!    a bracket expression, it is effectively transformed into a bracket
!!    expression containing both cases, e.g. x becomes [xX] . When it appears
!!    inside a bracket expression, all case counterparts of it are added
!!    to the bracket expression, so that (e.g.) [x] becomes [xX] and [^x]
!!    becomes [^xX] .
!!
!!    No particular limit is imposed on the length of REs . Programs intended
!!    to be portable should not employ REs longer than 256 bytes, as an
!!    implementation can refuse to accept such REs and remain POSIX-compliant.
!!
!!    Obsolete ( basic ) regular expressions differ in several respects. | is
!!    an ordinary character and there is no equivalent for its functionality. +
!!    and ? are ordinary characters, and their functionality can be expressed
!!    using bounds ( {1,} or {0,1} respectively). Also note that x+ in modern,
!!    REs is equivalent to xx* . The delimiters for bounds are \{ and \}
!!    with { and } by themselves ordinary characters. The parentheses for
!!    nested subexpressions are \( and \) , with ( and ) by themselves ordinary
!!    characters. ^ is an ordinary character except at the beginning of the
!!    RE or the beginning of a parenthesized subexpression, $ is an ordinary
!!    character except at the end of the RE or the end of a parenthesized
!!    subexpression, and * is an ordinary character if it appears at the
!!    beginning of the RE or the beginning of a parenthesized subexpression
!!    (after a possible leading ^ ). Finally, there is one new type of atom, a
!!    back reference: \ followed by a non-zero decimal digit d matches the same
!!    sequence of characters matched by the dth parenthesized subexpression
!!    (numbering subexpressions by the positions of their opening parentheses,
!!    left to right), so that (e.g.) \([bc]\)\1 matches bb or cc but not bc .
!!
!!##C WRAPPERS
!!
!!    Regex is defined as an API using C headers. It does not define the
!!    exact value of flag tokens, just the names. It also uses an opaque
!!    data structure and a declared numeric type for the match array.
!!    Therefore, the code must either be generated for each target
!!    platform, or it must use wrapper functions written in C.
!!
!!##FORTRAN WRAPPERS
!!
!!    Fortran wrapper functions are also required to present a normal
!!    Fortran API, and to not require C conversions by the caller.
!!
!!    The interface here is not strictly correct, because it does not
!!    explicitly convert Fortran strings to the C character kind.
!!    Fortran only supports conversion of string kinds by assignment,
!!    or by a rather slow internal WRITE. For now, the easiest approach
!!    is to assume that C and Fortran default character kinds are the
!!    same. This is generally true, but UTF-8 strings are likely to
!!    cause problems.
!!
!!##GENERAL
!!
!!    By default, the NUL-terminated string pointed to by string is
!!    considered to be the text of an entire line, minus any terminating
!!    newline. The eflags argument is the bitwise OR of zero or more of
!!    the following flags:
!!
!!    REG_NOTBOL    The first character of the string is not the beginning
!!                  of a line, so the ^ anchor should not match before
!!                  it. This does not affect the behavior of newlines
!!                  under REG_NEWLINE.
!!
!!    REG_NOTEOL    The NUL terminating the string does not end a line,
!!                  so the $ anchor should not match before it. This does
!!                  not affect the behavior of newlines under REG_NEWLINE.
!!
!!    REG_STARTEND  The string is considered to start at string +
!!                  pmatch[0].rm_so and to have a terminating NUL located at string
!!                  + pmatch[0].rm_eo (there need not actually be a NUL
!!                  at that location), regardless of the value of nmatch.
!!                  See below for the definition of pmatch and nmatch. This
!!                  is an extension, compatible with but not specified by
!!                  IEEE Std 1003.2 ( POSIX.2 ), and should be used with
!!                  caution in software intended to be portable to other
!!                  systems. Note that a non-zero rm_so does not imply
!!                  REG_NOTBOL; REG_STARTEND affects only the location of
!!                  the string, not how it is matched.
!!
!!    If REG_NOSUB was specified in the compilation of the RE, or if nmatch
!!    is 0, regexec() ignores the pmatch argument (but see below for the
!!    case where REG_STARTEND is specified). Otherwise, pmatch points to
!!    an array of nmatch structures of type regmatch_t. Such a structure
!!    has at least the members rm_so and rm_eo, both of type regoff_t (a
!!    signed arithmetic type at least as large as an off_t and a ssize_t),
!!    containing respectively the offset of the first character of a
!!    substring and the offset of the first character after the end of
!!    the substring. Offsets are measured from the beginning of the string
!!    argument given to regexec(). An empty substring is denoted by equal
!!    offsets, both indicating the character following the empty substring.
!!
!!    The 0th member of the pmatch array is filled in to indicate what
!!    substring of string was matched by the entire RE. Remaining members
!!    report what substring was matched by parenthesized subexpressions
!!    within the RE; member i reports subexpression i, with subexpressions
!!    counted (starting at 1) by the order of their opening parentheses in
!!    the RE, left to right. Unused entries in the array (corresponding
!!    either to subexpressions that did not participate in the match at
!!    all, or to subexpressions that do not exist in the RE (that is, i >
!!    preg->re_nsub)) have both rm_so and rm_eo set to -1. If a subexpression
!!    participated in the match several times, the reported substring
!!    is the last one it matched. (Note, as an example in particular ,
!!    that when the RE (b*)+ matches bbb , the parenthesized
!!    subexpression matches each of the three b's and then an infinite
!!    number of empty strings following the last b , so the reported
!!    substring is one of the empties.)
!!
!!    If REG_STARTEND is specified, pmatch must point to at least one
!!    regmatch_t (even if nmatch is 0 or REG_NOSUB was specified), to hold
!!    the input offsets for REG_STARTEND. Use for output is still entirely
!!    controlled by nmatch; if nmatch is 0 or REG_NOSUB was specified,
!!    the value of pmatch[0] will not be changed by a successful regexec().
!!
!!    The regerror() function maps a non-zero errcode from either
!!    regcomp() or regexec() to a human-readable, printable message.
!!    If preg is non-NULL, the error code should have arisen from use of
!!    the regex_t pointed to by preg, and if the error code came from
!!    regcomp(), it should have been the result from the most recent
!!    regcomp() using that regex_t. The (regerror() may be able to
!!    supply a more detailed message using information from the regex_t.)
!!    The regerror() function places the NUL-terminated message into the
!!    buffer pointed to by errbuf, limiting the length (including the NUL)
!!    to at most errbuf_size bytes. If the whole message will not fit,
!!    as much of it as will fit before the terminating NUL is supplied.
!!    In any case, the returned value is the size of buffer needed to hold
!!    the whole message (including terminating NUL). If errbuf_size is 0,
!!    errbuf is ignored but the return value is still correct.
!!
!!    The regfree() function frees any dynamically-allocated storage
!!    associated with the compiled RE pointed to by preg. The remaining
!!    regex_t is no longer a valid compiled RE and the effect of supplying
!!    it to regexec() or regerror() is undefined.
!!
!!    None of these functions references global variables except for
!!    tables of constants; all are safe for use from multiple threads if
!!    the arguments are safe.
!!
!!##IMPLEMENTATION CHOICES
!!
!!    RE_DUP_MAX, the limit on repetition counts in bounded repetitions,
!!    is 255.
!!
!!    A repetition operator ( ? , * , + , or bounds) cannot
!!    follow another repetition operator. A repetition operator cannot
!!    begin an expression or subexpression or follow ^ or | .
!!
!!    | cannot appear first or last in a (sub)expression or after
!!    another | , i.e., an operand of | cannot be an empty sub
!!    expression. An empty parenthesized subexpression, () , is legal
!!    and matches an empty (sub)string. An empty string is not a legal RE.
!!
!!    A { followed by a digit is considered the beginning of bounds for
!!    a bounded repetition, which must then follow the syntax for bounds. A
!!    { not followed by a digit is considered an ordinary character.
!!
!!    ^ and $ beginning and ending subexpressions in obsolete
!!    ( basic ) REs are anchors, not ordinary characters.
!!##UNIT TEST
!!
!!     When porting to a new programming environment use the
!!     built-in unit test ...
!!
!!      program test_M_regex
!!      use M_regex, only : test_suite_M_regex
!!         call test_suite_M_regex()
!!      end program test_M_regex
!!
!!##SEE ALSO
!!
!!    Regular Expression Notation, IEEE Std, 1003.2, section 2.8.
!!
!!    grep(1), re_format(7), regex(3)
!!
!!    These routines implement IEEE Std 1003.2 ("POSIX.2") regular expressions ("RE"s); see re_format(7).
!!
!!    IEEE Std 1003.2 (POSIX.2), sections 2.8 (Regular Expression Notation) and B.5 (C Binding for Regular Expression Matching).
!!
!!     regcomp (3p)  - regular expression matching
!!     regex (3)     - regular-expression library
!!     regex (7)     - POSIX 1003.2 regular expressions
!!     regex.h (0p)  - regular expression matching types
!!     regexp (n)    - Match a regular expression against a string
!!     regsub (n)    - Perform substitutions based on regular expression pattern matching
module M_regex
use ISO_C_Binding, only: C_ptr, C_int, C_size_t, C_char, C_NULL_char, C_NULL_ptr
use iso_c_binding, only: c_associated , c_f_pointer
use, intrinsic :: ISO_Fortran_Env, only: ERROR_UNIT
use M_strings, only : s2c
implicit none
private

! Fortran regex structure holds a pointer to an opaque C structure
type regex_type
   type(C_ptr) :: preg
end type regex_type

! API:
public regcomp  ! subroutine regcomp(this,pattern,flags,nmatch,status)             ! Compile a regex into a regex object
public regexec  ! function regexec(this,string,matches,flags,status) result(match) ! Execute a compiled regex against a string
public regerror ! function regerror(this,errcode) result(errmsg)                   ! Get the string message for a status error value
public regfree  ! subroutine regfree(this)                                         ! Release a compiled regular expression

public regmatch !  function regmatch(match,string,matches)                         ! Match a regular expression against a string
public regsub   !  subroutine regsub(matchline, matches, source, dest)             ! Perform substitutions based on pattern matching

public test_suite_M_regex

public regex_type
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
contains
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!     regcomp(3f) - [M_regex] Compile a regular expression into a regex object
!!##SYNOPSIS
!!
!!    subroutine regcomp(this,pattern,flags,status)
!!
!!      type(regex_type), intent(out)          :: this
!!      character(len=*), intent(in)           :: pattern
!!      character(len=*), intent(in), optional :: flags
!!      integer, intent(out), optional         :: nmatch
!!      integer, intent(out), optional         :: status
!!##DESCRIPTION
!!      The regcomp() function compiles an RE written as a string into an
!!      internal form for use by regexec().
!!##OPTIONS
!!      THIS     new regex object
!!      PATTERN  regex pattern string
!!      FLAGS    flag characters:
!!                 x=  extended regex (REG_EXTENDED). Default is obsolete
!!                     ("basic") REs.
!!                 m=  multi-line (REG_NEWLINE).
!!                     Compile for newline-sensitive matching. By default,
!!                     newline is a completely ordinary character with no
!!                     special meaning in either REs or strings. With this
!!                     flag, '[^' bracket expressions and . never match
!!                     newline, a '^' anchor matches the null string after
!!                     any newline in the string in addition to its normal
!!                     function, and the '$' anchor matches the null string
!!                     before any newline in the string in addition to its
!!                     normal function.
!!                 i=  case-insensitive (REG_ICASE)
!!                 n=  no MATCH required (REG_NOSUB)
!!                     Compile for matching that need only report success
!!                     or failure, not what was matched.
!!      NMATCH   number of subexpressions in regular expression
!!      STATUS   If absent, errors are fatal
!!
!!##EXAMPLE
!!
!!
!!   Sample program
!!
!!    program demo_regcomp
!!    use M_regex, only: regex_type, regcomp, regexec, regfree
!!    use M_regex, only: regmatch
!!    implicit none
!!    type(regex_type)             :: regex
!!    integer                      :: matches(2,1)
!!    character(len=:),allocatable :: input_line
!!    character(len=:),allocatable :: output_line
!!    character(len=:),allocatable :: expression
!!    logical                      :: match
!!    integer                      :: ipass
!!    integer                      :: istart
!!       expression= "[-0-9.*/+]+"                              ! define extended regular expression
!!       input_line= "30*0 250*1 5 AND 6 7:and some text8 999"  ! define an input line to search for the expression
!!       call regcomp(regex,expression,'x')                     ! compile the regex
!!       ipass=1                                                ! initialize pass counter
!!       INFINITE: do                     ! find match, then look again in remainder of line till all matches found
!!          match=regexec(regex,input_line,matches)        ! look for a match in (remaining) string
!!          if(.not.match)exit INFINITE                    ! if no match found exit
!!          output_line=regmatch(1,input_line,matches)     ! use bounds in MATCHES to select the matching substring
!!          write(*,'(8x,*(a))') 'match="',output_line,'"' ! show match
!!          istart=matches(2,1)+1                          ! find beginning of remainder of string
!!          if(istart.gt.len(input_line))exit INFINITE     ! reached end of string
!!          input_line=input_line(istart:)                 ! reduce string by any previous match
!!          ipass=ipass+1                                  ! increment count of passes made
!!       enddo INFINITE                                    ! free memory used for compiled regular expression
!!       call regfree(regex)
!!    end program demo_regcomp
!!
!!   Expected output
!!
!!    match="30*0"
!!    match="250*1"
!!    match="5"
!!    match="6"
!!    match="7"
!!    match="8"
!!    match="999"
!!##SEE ALSO
!!      This routine calls an implementation of IEEE Std 1003.2 ( POSIX.2 ) regular expressions ( RE's); see re_format(7).
subroutine regcomp(this,pattern,flags,nmatch,status)

! ident_1="@(#)M_exec::regcomp(3f): compile regular expression"

type(regex_type), intent(out)          :: this
character(len=*), intent(in)           :: pattern
character(len=*), intent(in), optional :: flags
integer, intent(out), optional         :: nmatch
integer, intent(out), optional         :: status
   character(len=10,kind=C_char)       :: flags_
   integer(C_int)                      :: nmatch_
   integer(C_int)                      :: status_

   interface
     subroutine C_regcomp(preg,pattern,flags,nmatch,status) bind(C,name="C_regcomp")
       import
       type(C_ptr), intent(in), value           :: preg
       character(len=1,kind=C_char), intent(in) :: pattern(*)
       character(len=1,kind=C_char), intent(in) :: flags(*)
       integer(C_int), intent(inout)            :: nmatch
       integer(C_int), intent(inout)            :: status
     end subroutine C_regcomp

     subroutine C_regalloc(preg_return) bind(C,name="C_regalloc")
       import
       type(C_ptr), intent(out)                 :: preg_return
     end subroutine C_regalloc
   end interface

   flags_=' '
   if (present(flags)) flags_=flags
   this%preg = C_NULL_ptr
   call C_regalloc(this%preg)
   call C_regcomp(this%preg, s2c(trim(pattern)), s2c(trim(flags)), nmatch_, status_)
   if (present(nmatch)) then
     nmatch=nmatch_
   endif
   if (present(status)) then
     status=status_
   elseif (status_/=0) then
     stop 'Regex runtime error: regcomp failed.'
   endif
end subroutine regcomp
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    regexec(3f) - [M_regex] Execute a compiled regex against a string
!!##SYNOPSIS
!!
!!    function regexec(this,string,matches,flags,status) result(match)
!!
!!      logical :: match ! .TRUE. if the pattern matched
!!      type(regex_type),intent(in)           :: this
!!      character(len=*),intent(in)           :: string
!!      integer, intent(out),optional         :: matches(:,:)
!!      character(len=*),intent(in), optional :: flags
!!      integer, intent(out),optional         :: status
!!##DESCRIPTION
!!
!!##OPTIONS
!!     THIS      regex object
!!     STRING    target string
!!     MATCHES   match locations dimension(2,*)
!!     FLAGS     flag characters for partial lines:
!!
!!                  o b = no beginning-of-line (REG_NOTBOL)
!!                  o e = no end-of-line (REG_NOTEOL)
!!     STATUS    if absent, errors are fatal
!!               0      successfully found match
!!               1      successfully found no match
!!               other  regexec(3f) failed
!!
!!##RETURNS
!!    regexec    LOGICAL value is .TRUE. if a match was found
!!##EXAMPLE
!!
!!   Sample program
!!
!!    program demo_regexec
!!    ! read regular expression from command line and look for it in lines read from stdin.
!!    use M_regex, only: regex_type, regcomp, regexec, regfree
!!    implicit none
!!    integer                      :: command_argument_length
!!    character(len=:),allocatable :: command_argument
!!    character(len=1024)          :: input_line
!!    type(regex_type)             :: regex
!!    logical                      :: match
!!    integer                      :: ios
!!       call get_command_argument(number=1,length=command_argument_length)
!!       allocate(character(len=command_argument_length) :: command_argument)
!!       call get_command_argument(1, command_argument)
!!       call regcomp(regex,command_argument,'xn') ! compile up regular expression
!!       INFINITE: do
!!          read(*,'(a)',iostat=ios)input_line
!!          if(ios.ne.0)exit INFINITE
!!          match=regexec(regex,input_line) ! look for a match in (remaining) string
!!          if(.not.match)cycle INFINITE    ! if no match found go for next line
!!          write(*,'(a)') trim(input_line) ! show line with match
!!       enddo INFINITE
!!       call regfree(regex)                ! free memory used for compiled regular expression
!!    end program demo_regexec
!!
!!   Sample Output
!!
!!      demo_regexec '\<integer\>' < demo_regexec.f90
!!      integer                      :: command_argument_length
!!      integer                      :: ios
logical function regexec(this,string,matches,flags,status) result(match)

! ident_2="@(#)M_exec::regexec(3f): Execute a compiled RE(regular expression) against a string"

type(regex_type), intent(in)           :: this
character(len=*), intent(in)           :: string
character(len=*), intent(in), optional :: flags
integer, intent(out), optional         :: matches(:,:)
integer, intent(out), optional         :: status
   integer(C_int)                      :: status_, matches_(2,1)
   character(len=10,kind=C_char)       :: flags_
   integer                             :: maxlen
   interface
      subroutine C_regexec(preg,string,nmatch,matches,flags,status) bind(C,name="C_regexec")
        import
        type(C_ptr), intent(in), value           :: preg
        character(len=1,kind=C_char), intent(in) :: string(*)
        integer(C_int), intent(in), value        :: nmatch
        integer(C_int), intent(out)              :: matches(2,nmatch)
        character(len=1,kind=C_char), intent(in) :: flags(*)
        integer(C_int), intent(out)              :: status
      end subroutine C_regexec
   end interface

   if(present(flags))then
      flags_=flags
   else
      flags_=' '
   endif

   status_=0
   maxlen=len(string)
   if(present(matches))then
      matches=0
      call C_regexec(this%preg, s2c(trim(string)), size(matches,dim=2),matches, s2c(trim(flags_)), status_)
      !write(*,'("<",a,*(sp,i0.4:,","))')'MATCHES(1,:)=',matches(1,:)
      !write(*,'("<",a,*(sp,i0.4:,","))')'MATCHES(2,:)=',matches(2,:)
      where(matches(1,:).ge.0)matches(1,:)=matches(1,:)+1
      where(matches.gt.maxlen)matches=-1
      !write(*,'(">",a,*(sp,i0.4:,","))')'MATCHES(1,:)=',matches(1,:)
      !write(*,'(">",a,*(sp,i0.4:,","))')'MATCHES(2,:)=',matches(2,:)
   else
      call C_regexec(this%preg, s2c(trim(string)), int(0,C_int), matches_, s2c(trim(flags_)), status_)
   endif

   match = status_==0

   if (present(status))then
      status=status_
   elseif(status_/=0.and.status_/=1)then
      stop 'Regex runtime error: regexec failed.'
   endif

end function regexec
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    regmatch(3f) - [M_regex] return selected substring defined by the
!!                   MATCHES(2,:) array
!!
!!##SYNOPSIS
!!
!!   function regmatch(match,string,matches)
!!
!!    character(len=matches(2,match)-matches(1,match)+1) :: regmatch
!!    integer, intent(in)                                :: match, matches(2,*)
!!    character(len=*), intent(in)                       :: string
!!
!!##DESCRIPTION
!!    A convenience function to return a specific substring from the input
!!    STRING, defined by the MATCHES() array.
!!
!!##OPTIONS
!!    match      the count into the start and end matrix matches(:,MATCH)
!!    string     the string to find the substrings in
!!    matches    the start and end matrix generated from calling REGEXEC(3f).
!!
!!##RETURNS
!!    regmatch   the selected substring extracted from STRING.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_regmatch
!!    ! read regular expression from command line and look for it in lines read from stdin.
!!    use M_regex, only: regex_type, regcomp, regexec, regmatch, regfree
!!    implicit none
!!    integer                      :: command_argument_length
!!    character(len=:),allocatable :: command_argument
!!    character(len=1024)          :: input_line
!!    type(regex_type)             :: regex
!!    logical                      :: match
!!    integer,parameter            :: max_subexpressions=10
!!    integer                      :: matches(2,max_subexpressions)
!!    integer                      :: ios
!!       !find length of command argument
!!       call get_command_argument(number=1,length=command_argument_length)
!!       ! allocate a string long enough to hold the argument
!!       allocate(character(len=command_argument_length) :: command_argument)
!!       ! get the command argument
!!       call get_command_argument(1, command_argument)
!!
!!       ! compile up regular expression
!!       call regcomp(regex,command_argument,'x')
!!
!!       ! read lines and look for match to expression
!!       INFINITE: do
!!          read(*,'(a)',iostat=ios)input_line
!!          if(ios.ne.0)exit INFINITE
!!          ! look for a match in (remaining) string
!!          match=regexec(regex,input_line,matches)
!!          ! if no match found go for next line
!!          if(.not.match)cycle INFINITE
!!          ! show line with match
!!          write(*,'(a)') trim(input_line)
!!       enddo INFINITE
!!
!!       ! free memory used for compiled regular expression
!!       call regfree(regex)
!!
!!    end program demo_regmatch
function regmatch(match,string,matches)

! ident_3="@(#)M_exec::regmatch(3f): return selected substrings defined by the regexec(3f) procedure"

integer, intent(in)                                :: match, matches(2,*)
character(len=*), intent(in)                       :: string
!!character(len=matches(2,match)-matches(1,match)+1) :: regmatch  ! intel internal compiler error version 19
character(len=:),allocatable :: regmatch

   regmatch = string(matches(1,match):matches(2,match))

end function regmatch
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    regerror(3f) - [M_regex] maps a non-zero errcode from either regcomp(3f) or regexec(3f) to a human-readable, printable message.
!!##SYNOPSIS
!!
!!    function regerror(this,errcode) result(errmsg)
!!
!!      type(regex_type), intent(in) :: this
!!      integer, intent(in)          :: errcode
!!      character(len=:),allocatable :: errmsg
!!
!!##DESCRIPTION
!!      The REGCOMP() function compiles an RE written as a string into
!!      an internal form, REGEXEC() matches that internal form against
!!      a string and reports results, REGERROR() transforms error codes
!!      from either into human-readable messages, and REGFREE() frees any
!!      dynamically-allocated storage used by the internal form of an RE.
!!
!!      These routines implement IEEE Std 1003.2 (POSIX.2) regular
!!      expressions (RE s); see re_format(7).
!!
!!      Specifically, The REGERROR(3f) function maps a non-zero errcode from
!!      either REGCOMP(3f) or REGEXEC(3f) to a human-readable, printable
!!      message. If THIS is non-NULL, the error code should have arisen
!!      from use of the regex_t pointed to by THIS, and if the error code
!!      came from REGCOMP(), it should have been the result from the most
!!      recent REGCOMP() using that regex_t.
!!
!!      Non-zero error codes from regcomp() and regexec() include the
!!      following:
!!
!!      REG_NOMATCH   The regexec() function failed to match
!!      REG_BADPAT    invalid regular expression
!!      REG_ECOLLATE  invalid collating element
!!      REG_ECTYPE    invalid character class
!!      REG_EESCAPE      \    applied to unescapable character
!!      REG_ESUBREG   invalid backreference number
!!      REG_EBRACK    brackets    [ ]    not balanced
!!      REG_EPAREN    parentheses    ( )    not balanced
!!      REG_EBRACE    braces    { }    not balanced
!!      REG_BADBR     invalid repetition count(s) in    { }
!!      REG_ERANGE    invalid character range in    [ ]
!!      REG_ESPACE    ran out of memory
!!      REG_BADRPT       ?   ,    *   , or    +    operand invalid
!!      REG_EMPTY     empty (sub)expression
!!      REG_ASSERT    cannot happen - you found a bug
!!      REG_INVARG    invalid argument, e.g. negative-length string
!!      REG_ILLSEQ    illegal byte sequence (bad multibyte character)
!!
!!##OPTIONS
!!      THIS         a compiled regular expression from REGCOMP(3f)
!!      ERRCODE      the last error code generated by REGCOMP(3f) or REGEXEC(3f)
!!      ERRMSG       the error message
!!      ERRMSG_LEN   size required of ERRMSG to display the entire message
!!
!!##EXAMPLE
!!
!!   Sample program
!!
!!    program demo_regerror
!!    use M_regex, only: regex_type, regcomp, regexec, regmatch, regfree, regerror
!!    type(regex_type)             :: regex
!!    integer,parameter            :: maxmatch=10
!!    integer                      :: matches(2,maxmatch)
!!
!!    character(len=:),allocatable :: input_line
!!    character(len=:),allocatable :: expression
!!    logical                      :: match
!!
!!       expression= "([0-9\.\-\*\/]+)+"
!!       expression= "([0-9\.\-\*\/+)+"  ! intentionally bad RE (Regular Expression)
!!       input_line= "30*0 250*1 5 6 7"
!!       call regcomp(regex,expression,'x',status=istat)
!!       if (istat/=0) then
!!         write(*,'("Regex runtime error in regcomp(3f):",a,", expression=",a)') regerror(regex,istat),expression
!!         stop 1
!!       endif
!!       match=regexec(regex,input_line,matches,status=istat)
!!       if (istat/=0) then
!!         write(*,'("Regex runtime error in regexec:(3f)",a)') regerror(regex,istat)
!!         stop 2
!!       endif
!!       if(match)then
!!          do i=1,maxmatch
!!             if(matches(1,i).le.0)exit
!!             write(*,*) 'match="',regmatch(i,input_line,matches),'"'
!!          enddo
!!       endif
!!       call regfree(regex)
!!
!!    end program demo_regerror
!!
!!   Expected output:
!!
!!    Regex runtime error in regcomp(3f):brackets ([ ]) not balanced , expression=([0-9\.\-\*\/+)+
!!    stop 1
function regerror(this,errcode) result(errmsg)

! ident_4="@(#)M_exec::regerror(3f): describe error generated by regcomp(3f) or regexec(3f)"

type(regex_type), intent(in)                :: this
integer,intent(in)                          :: errcode
character(len=:),allocatable                :: errmsg

   integer                                  :: errmsg_len
   character(len=1,kind=c_char)             :: buffer2(1024)
   integer                                  :: i
   interface
      function C_regerror(preg ,errc, errbuf)result(regerror)bind(C,name="my_regerror")
        import c_int, c_char, c_ptr, c_size_t
        type(C_ptr),intent(in),value        :: preg
        integer(C_size_t),value             :: errc
        character(len=1,kind=c_char)        :: errbuf(*)
        integer(C_size_t)                   :: regerror
      end function C_regerror
   end interface

   buffer2=' '
   errmsg_len = C_regerror(this%preg,int(errcode,C_size_t), buffer2)
   allocate(character(len=errmsg_len) :: errmsg)
   errmsg(:)=' '
   do i=1,min(errmsg_len,1024)
      if(buffer2(i).eq.char(0))exit
      errmsg(i:i)=buffer2(i)
   enddo

end function regerror
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    regfree(3f) - [M_regex] Release storage used by the internal form of the RE (Regular Expression)
!!##SYNOPSIS
!!
!!    subroutine regfree(this)
!!
!!      type(regex_type), intent(inout) :: this
!!
!!##DESCRIPTION
!!
!!      regfree(3f) frees any dynamically-allocated storage used by the internal form of an RE.
!!
!!      The regfree(3f) function frees any dynamically-allocated storage associated with the compiled RE pointed to by THIS. The
!!      remaining regex_type is no longer a valid compiled RE and the effect of supplying it to regexec() or regerror() is undefined.
!!
!!##OPTIONS
!!      THIS  a compiled regular expression previously allocated using regcomp(3f).
!!
!!##EXAMPLE
!!
!!   Sample program
!!
!!    program demo_regfree
!!    use M_regex, only: regex_type, regcomp, regexec, regmatch, regfree, regerror
!!    type(regex_type)             :: regex
!!    character(len=:),allocatable :: expression
!!       expression= "([0-9\.\-\*\/]+)+"
!!       call regcomp(regex,expression,'x')
!!       if (istat/=0) then
!!         stop 'Regex runtime error: regcomp failed.'
!!       endif
!!       call regfree(regex)
!!    end program demo_regfree
subroutine regfree(this)

! ident_5="@(#)M_exec::regfree(3f): release storage created by regcomp(3f) compiled regular expression"

type(regex_type), intent(inout) :: this

   interface
      subroutine C_regfree(preg) bind(C,name="regfree")
         import
         type(C_ptr), intent(in), value :: preg
      end subroutine C_regfree
   end interface

   call C_regfree(this%preg)
   this%preg = C_NULL_ptr

end subroutine regfree
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    regsub(3f) - [M_regex] perform regex substitutions
!!##SYNOPSIS
!!
!!   subroutine regsub(matchline, matches, source, dest)
!!
!!    character(len=*),intent(in)              :: matchline
!!    integer,intent(in)                       :: matches(:,:)
!!    character(len=*),intent(in)              :: source
!!    character(len=:),allocatable,intent(out) :: dest
!!
!!##DESCRIPTION
!!    The regsub() function copies source to dest, making substitutions
!!    according to the most recent regexec() performed using MATCHES(:,:).
!!
!!    Each instance of "&" in source is replaced by the substring indicated by
!!    the start and end array MATCHES(:,:).
!!
!!    Each instance of "\n", where n is a digit,
!!    is replaced by the substring indicated by MATCHES(1,n) and MATCHES(2,n).
!!
!!    To get a literal "&" or "\n" into dest, prefix it with "\"; to get
!!    a literal "\" preceding "&" or "\n", prefix it with another "\".
!!
!!##OPTIONS
!!    MATCHLINE  line REGEXEC(3f) was run against
!!    MATCHES    output array from REGEXEC(3f) call
!!    SOURCE     output template string containing "&" and/or "\n" indicating where to
!!               make substitutions
!!    DEST       output string
!!
!!##EXAMPLES
!!
!!
!!   Sample program that reads an array of lines representing a Unix /etc/passwd file
!!   and uses an RE (Regular Expression) to read fields from the line and then uses
!!   REGSUB(3f) to print strings using the matched strings
!!
!!    program demo_regsub
!!    use M_regex, only: regex_type, regcomp, regexec, regerror, regmatch, regfree, regsub
!!    use M_strings, only : replace
!!    implicit none
!!    type(regex_type)             :: regex
!!    integer,parameter            :: maxmatch=10
!!    integer                      :: matches(2,maxmatch)
!!    character(len=:),allocatable :: input_line(:)
!!    character(len=:),allocatable :: output_line
!!    character(len=:),allocatable :: expression
!!    logical                      :: match
!!    integer                      :: stat
!!    integer                      :: i
!!    logical                      :: BRE
!!       ! The /etc/passwd file is a colon-separated file that contains the following information:
!!       !
!!       !     User name.
!!       !     Encrypted password.
!!       !     User ID number (UID)
!!       !     User's group ID number (GID)
!!       !     Full name of the user (GECOS)
!!       !     User home directory.
!!       !     Login shell.
!!       !
!!       BRE=.true.
!!       if(BRE)then  ! BRE (Basic Regular Expression)
!!          expression= '\([^:]*\):*\([^:]*\):*\([^:]*\):*\([^:]*\):*\([^:]*\):*\([^:]*\):*\([^:]*\):*.*'
!!          call regcomp(regex,expression,status=stat)
!!       else         ! ERE (Extended Regular Expression)
!!          expression= '([^:]*):*([^:]*):*([^:]*):*([^:]*):*([^:]*):*([^:]*):*([^:]*):*.*'
!!          call regcomp(regex,expression,'x',status=stat)
!!       endif
!!
!!       if(stat.ne.0)then                                        ! if RE did not compile report error
!!          write(*,*)'*regcomp* ERROR:',regerror(regex,stat)
!!          stop 1
!!       endif
!!
!!       ! simulate /etc/password file in array, with common aberrant input lines included
!!       input_line= [character(len=128) ::                                     &
!!       "doeqj:xxxxx:1001:200:John Q. Doe:/home/doeqj:/bin/tcsh: and more",    &
!!       "doeqj:xxxxx:1001:200:John Q. Doe:/home/doeqj:/bin/tcsh: and more:",   &
!!       "doeqj: :1001: :John Q. Doe:/home/doeqj:/bin/tcsh:",                   &
!!       "doeqj:xxxxx:1001:200:John Q. Doe:/home/doeqj:/bin/tcsh:",             &
!!       "doeqj:xxxxx:1001:200:John Q. Doe:/home/doeqj:/bin/tcsh",              &
!!       "doeqj:xxxxx:1001:200:John Q. Doe:/home/doeqj",                        &
!!       "doeqj:xxxxx:",                                                        &
!!       "doeqj:xxxxx",                                                         &
!!       "doeqj:",                                                              &
!!       "doeqj",                                                               &
!!       ":::::::::::::::",                                                     &
!!       ":::",                                                                 &
!!       "" ]
!!
!!       do i=1,size(input_line)
!!          input_line=replace(input_line(i),'::',': :')          ! the RE shown needs the field to have at least one character
!!          match=regexec(regex,input_line(i),matches)            ! generate the matches array using the compiled RE
!!
!!          write(*,'(a)')repeat('-',80)                          ! put out a number line
!!          write(*,'(a)') input_line(i)
!!                                                                ! replace \n lines
!!          call regsub(input_line(i),matches,'username="\1" &
!!          &password="\2" UID="\3" GID="\4" name="\5" &
!!          &home="\6" shell="\7" ',output_line)
!!          write(*,'(a)') output_line
!!
!!          if(i.eq.1)then                                        ! show other examples of formatting for first entry
!!             call regsub(input_line(i),matches,&
!!             & 'The username for \5 is "\1"',output_line)
!!             write(*,'(a)') output_line
!!
!!             call regsub(input_line(i),matches,&
!!             & 'username "\1" has UID=\3, GID=\4 &
!!             & and default shell "\7"',output_line)
!!             write(*,'(a)') output_line
!!          endif
!!       enddo
!!
!!       call regfree(regex)
!!
!!    end program demo_regsub
!!    --------------------------------------------------------------------------------
!!    doeqj:xxxxx:1001:200:John Q. Doe:/home/doeqj:/bin/tcsh: and more
!!    username="doeqj" password="xxxxx" UID="1001" GID="200" name="John Q. Doe" home="/home/doeqj" shell="/bin/tcsh"
!!    The username for John Q. Doe is "doeqj"
!!    username "doeqj" has UID=1001, GID=200 and default shell "/bin/tcsh"
!!    --------------------------------------------------------------------------------
!!    doeqj:xxxxx:1001:200:John Q. Doe:/home/doeqj:/bin/tcsh: and more:
!!    username="doeqj" password="xxxxx" UID="1001" GID="200" name="John Q. Doe" home="/home/doeqj" shell="/bin/tcsh"
!!    --------------------------------------------------------------------------------
!!    doeqj: :1001: :John Q. Doe:/home/doeqj:/bin/tcsh:
!!    username="doeqj" password=" " UID="1001" GID=" " name="John Q. Doe" home="/home/doeqj" shell="/bin/tcsh"
!!    --------------------------------------------------------------------------------
!!    doeqj:xxxxx:1001:200:John Q. Doe:/home/doeqj:/bin/tcsh:
!!    username="doeqj" password="xxxxx" UID="1001" GID="200" name="John Q. Doe" home="/home/doeqj" shell="/bin/tcsh"
!!    --------------------------------------------------------------------------------
!!    doeqj:xxxxx:1001:200:John Q. Doe:/home/doeqj:/bin/tcsh
!!    username="doeqj" password="xxxxx" UID="1001" GID="200" name="John Q. Doe" home="/home/doeqj" shell="/bin/tcsh"
!!    --------------------------------------------------------------------------------
!!    doeqj:xxxxx:1001:200:John Q. Doe:/home/doeqj
!!    username="doeqj" password="xxxxx" UID="1001" GID="200" name="John Q. Doe" home="/home/doeqj" shell=""
!!    --------------------------------------------------------------------------------
!!    doeqj:xxxxx:
!!    username="doeqj" password="xxxxx" UID="" GID="" name="" home="" shell=""
!!    --------------------------------------------------------------------------------
!!    doeqj:xxxxx
!!    username="doeqj" password="xxxxx" UID="" GID="" name="" home="" shell=""
!!    --------------------------------------------------------------------------------
!!    doeqj:
!!    username="doeqj" password="" UID="" GID="" name="" home="" shell=""
!!    --------------------------------------------------------------------------------
!!    doeqj
!!    username="doeqj" password="" UID="" GID="" name="" home="" shell=""
!!    --------------------------------------------------------------------------------
!!    : :: :: :: :: :: :: ::
!!    username="" password=" " UID=" " GID=" " name=" " home=" " shell=" "
!!    --------------------------------------------------------------------------------
!!    : ::
!!    username="" password=" " UID="" GID="" name="" home="" shell=""
!!    --------------------------------------------------------------------------------
!!
!!    username="" password="" UID="" GID="" name="" home="" shell=""
subroutine regsub(matchline,matches, source, dest)

! ident_6="@(#)M_regex::regsub(3f): perform regex substitutions"

character(len=*),intent(in)              :: matchline
integer,intent(in)                       :: matches(:,:)
character(len=*),intent(in)              :: source
character(len=:),allocatable,intent(out) :: dest
   character(len=:),allocatable :: src
   character(len=1)             :: c
   character(len=1)             :: cc
   integer                      :: no
   integer                      :: len
   integer                      :: i
   logical                      :: skip
   integer,parameter            :: inine=ichar('9'), izero=ichar('0')
   !!if(debug)then
   !!   write(*,'(a,*(i0.3:,","))')'MATCHES(1,:)=',matches(1,:)
   !!   write(*,'(a,*(i0.3:,","))')'MATCHES(2,:)=',matches(2,:)
   !!   write(*,*)'SOURCE=['//SOURCE//'] LEN=',len(SOURCE)
   !!endif
   if(source.eq.'')then
      dest=''
      write(ERROR_UNIT,*)"*regsub* NULL parameter to regsub"
      return
   endif
   src = source//' '

   dest = ''
   skip=.false.
   do i=1,len(source)
      if(skip)then
         skip=.false.
         cycle
      endif
      c=src(i:i)
      cc=src(i+1:i+1)
      if (c == '&')then                                                           ! & is replaced by match 1
         if(any(matches(:,1).le.0))cycle
         dest=dest//matchline( matches(1,1):matches(2,1) )
         cycle
      elseif (c == '\' .and. (ichar(cc) >= izero .and. ichar(cc) <= inine) )then  ! \[0-9] is replaced by match 1 to 10
         no = ichar(cc) - ichar('0')
         if(.not.any(matches(:,no+1).le.0))then
            dest=dest//matchline( matches(1,no+1):matches(2,no+1) )
         endif
         skip=.true.
         cycle
      elseif (c == '\' .and. (cc == '\' .or. cc == '&'))then                      ! escaping literal ("\\" or "\&")
         skip=.true.
         cycle
      else
         dest=dest//c                                                             ! non-escaped ordinary character
      endif
   enddo
end subroutine regsub
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine test_suite_m_regex()
use M_verify, only: unit_check, unit_check_good, unit_check_bad, unit_check_done, unit_check_start, unit_check_level
implicit none
call test_regex()
!===================================================================================================================================
contains
!===================================================================================================================================
subroutine test_regex()
call unit_check_start('M_regex')
!              "Regexp,      String,          expected result"
call mymatch("Foo",        "FooBar",        .true.   )
call mymatch("Poo",        "FooBar",        .false.  )
call mymatch("Bar",        "FooBar",        .true.   )
call mymatch("Par",        "FooBar",        .false.  )
call mymatch("Foo",        "Foo",           .true.   )
call mymatch("Fo",         "Foo",           .true.   )
call mymatch("Foo",        "Fo",            .false.  )
call mymatch("ooB",        "FooBar",        .true.   )
call mymatch("ooP",        "FooBar",        .false.  )
call mymatch(".",          "FooBar",        .true.   )
call mymatch("P.",         "FooBar",        .false.  )
call mymatch("^Foo",       "FooBar",        .true.   )
call mymatch("^Bar",       "FooBar",        .false.  )
call mymatch("Foo$",       "FooBar",        .false.  )
call mymatch("Bar$",       "FooBar",        .true.   )
call mymatch(".*o",        "FooBar",        .true.   )
call mymatch("o*o",        "FooBar",        .true.   )
call mymatch("P*o",        "FooBar",        .true.   )
call mymatch("Fo*o",       "FooBar",        .true.   )
call mymatch("Po*o",       "FooBar",        .false.  )
call mymatch(".+o",        "FooBar",        .true.   )
call mymatch("o+o",        "FooBar",        .true.   )
call mymatch("P+o",        "FooBar",        .false.  )
call mymatch("Fo+o",       "FooBar",        .true.   )
call mymatch("Po+o",       "FooBar",        .false.  )
call mymatch(".?o",        "FooBar",        .true.   )
call mymatch("o?o",        "FooBar",        .true.   )
call mymatch("P?o",        "FooBar",        .true.   )
call mymatch("Fo?o",       "FooBar",        .true.   )
call mymatch("Po?o",       "FooBar",        .false.  )
call mymatch("F[po]o",     "FooBar",        .true.   )
call mymatch("F[op]o",     "FooBar",        .true.   )
call mymatch("F[qp]o",     "FooBar",        .false.  )
call mymatch("F[^po]o",    "FooBar",        .false.  )
call mymatch("F[^op]o",    "FooBar",        .false.  )
call mymatch("F[^qp]o",    "FooBar",        .true.   )
call mymatch("F[po]*o",    "FooBar",        .true.   )
call mymatch("F[56]*o",    "F5oBar",        .true.   )
call mymatch("F[46]*o",    "F5oBar",        .false.  )
call mymatch("F[46]*5",    "F5oBar",        .true.   )
call mymatch("F[46]*5o",   "F5oBar",        .true.   )
call mymatch("F[op]*o",    "FooBar",        .true.   )
call mymatch("F[qp]*o",    "FooBar",        .true.   )
call mymatch("P[qp]*o",    "FooBar",        .false.  )
call mymatch("F[^po]*o",   "FooBar",        .true.   )
call mymatch("F[^op]*o",   "FooBar",        .true.   )
call mymatch("F[^qp]*o",   "FooBar",        .true.   )
call mymatch("P[^qp]*o",   "FooBar",        .false.  )
call mymatch("F[po]?o",    "FooBar",        .true.   )
call mymatch("F[56]?o",    "F5oBar",        .true.   )
call mymatch("F[46]?o",    "F5oBar",        .false.  )
call mymatch("F[46]?5",    "F5oBar",        .true.   )
call mymatch("F[46]?5o",   "F5oBar",        .true.   )
call mymatch("F[op]?o",    "FooBar",        .true.   )
call mymatch("F[qp]?o",    "FooBar",        .true.   )
call mymatch("P[qp]?o",    "FooBar",        .false.  )
call mymatch("F[^po]?o",   "FooBar",        .true.   )
call mymatch("F[^op]?o",   "FooBar",        .true.   )
call mymatch("F[^qp]?o",   "FooBar",        .true.   )
call mymatch("P[^qp]?o",   "FooBar",        .false.  )
call mymatch("F[po]+o",    "FooBar",        .true.   )
call mymatch("F[56]+o",    "F5oBar",        .true.   )
call mymatch("F[46]+o",    "F5oBar",        .false.  )
call mymatch("F[46]+5",    "F5oBar",        .false.  )
call mymatch("F[46]+5o",   "F5oBar",        .false.  )
call mymatch("F[op]+o",    "FooBar",        .true.   )
call mymatch("F[qp]+o",    "FooBar",        .false.  )
call mymatch("P[qp]+o",    "FooBar",        .false.  )
call mymatch("F[^po]+o",   "FooBar",        .false.  )
call mymatch("F[^op]+o",   "FooBar",        .false.  )
call mymatch("F[^qp]+o",   "FooBar",        .true.   )
call mymatch("P[^qp]+o",   "FooBar",        .false.  )
call mymatch("[0-9]+\.[0-9]*",   "1.9",           .true.   )
call mymatch("[0-9]+\.[0-9]*",   "1.99",          .true.   )
call mymatch("[0-9]+\.[0-9]*",   "1.999",         .true.   )
call mymatch("[0-9]+\.[0-9]*",   "1.9999",        .true.   )
call mymatch("[0-9]+\.[0-9]*",   "1.99999",       .true.   )
call mymatch("[0-9]+\.[0-9]*",   "11.99999",      .true.   )
call mymatch("[0-9]+\.[0-9]*",   "111.99999",     .true.   )
call mymatch("[0-9]+\.[0-9]*",   "1111.99999",    .true.   )
call mymatch("[0-9]+\.[0-9]*",   "11111.99999",   .true.   )
call mymatch("[0-9]+\.[0-9]*",   "111111.99999",  .true.   )
call mymatch("^[0-9]+\.[0-9]*",  "1.9",           .true.   )
call mymatch("^[0-9]+\.[0-9]*",  "1.99",          .true.   )
call mymatch("^[0-9]+\.[0-9]*",  "1.999",         .true.   )
call mymatch("^[0-9]+\.[0-9]*",  "1.9999",        .true.   )
call mymatch("^[0-9]+\.[0-9]*",  "1.99999",       .true.   )
call mymatch("^[0-9]+\.[0-9]*",  "11.99999",      .true.   )
call mymatch("^[0-9]+\.[0-9]*",  "111.99999",     .true.   )
call mymatch("^[0-9]+\.[0-9]*",  "1111.99999",    .true.   )
call mymatch("^[0-9]+\.[0-9]*",  "11111.99999",   .true.   )
call mymatch("^[0-9]+\.[0-9]*",  "111111.99999",  .true.   )
call mymatch("a[0-9]+\.[0-9]*",  "a1.9",          .true.   )
call mymatch("a[0-9]+\.",     "a1.9",          .true.   )
call mymatch("a[0-9]+",       "a1.9",          .true.   )
call mymatch("a",          "a1.9",          .true.   )
call mymatch("\\",         "\",             .true.   )
call mymatch("\.",         "\",             .false.  )
call mymatch(".",          "\",             .true.   )
call mymatch("F[qpo", "FooBar", .false.) ! intentional bad REGEX

call unit_check_done('M_regex')
end subroutine test_regex
!===================================================================================================================================
subroutine mymatch(expression,string,expected)
use, intrinsic :: iso_fortran_env, only : ERROR_UNIT
use M_verify, only: unit_check, unit_check_good, unit_check_bad, unit_check_done, unit_check_start, unit_check_level
character(len=*),intent(in) :: expression
character(len=*),intent(in) :: string
logical,intent(in)          :: expected
   type(regex_type)             :: regex
   integer,parameter            :: maxmatch=10
   integer                      :: matches(2,maxmatch)
   logical                      :: match
   integer                      :: istat
   integer                      :: i
   call regcomp(regex,expression,'x',status=istat)
   match=.false.
   if(istat/=0) then
      if(unit_check_level.gt.0)then
         write(ERROR_UNIT,'("runtime error in regcomp(3f):",a,", expression=",a)') regerror(regex,istat),expression
      endif
   else
      match=regexec(regex,string,matches,status=istat)
      if(istat/=0) then
         if(unit_check_level.gt.0)then
            write(ERROR_UNIT,'("runtime error in regexec:(3f)",a)') regerror(regex,istat)
         endif
      else if(match)then
         do i=1,maxmatch
            if(matches(1,i).le.0)exit
            if(unit_check_level.gt.0)then
               write(*,*) 'match="',regmatch(i,string,matches),'"'
            endif
         enddo
      endif
   endif
   call regfree(regex)
   call unit_check('M_regex',match .eqv. expected,msg='for REGEX '//trim(expression)//' and string '//trim(string))
end subroutine mymatch
!===================================================================================================================================
end subroutine test_suite_m_regex
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
end module M_regex
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
