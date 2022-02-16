subroutine help_usage(l_help)
implicit none
character(len=*),parameter     :: ident="@(#)help_usage(3f): prints help information"
logical,intent(in)             :: l_help
character(len=:),allocatable :: help_text(:)
integer                        :: i
logical                        :: stopit=.false.
stopit=.false.
if(l_help)then
help_text=[ CHARACTER(LEN=128) :: &
'NAME                                                                                                                            ',&
'    grep-(1f) - [FUNIX] search a file for a pattern                                                                             ',&
'    (LICENSE:PD)                                                                                                                ',&
'                                                                                                                                ',&
'SYNOPSIS                                                                                                                        ',&
'    grep- pattern [ -i][ -E| -G]|[ --help| --version]                                                                           ',&
'                                                                                                                                ',&
'DESCRIPTION                                                                                                                     ',&
'                                                                                                                                ',&
'    Find lines on stdin that contain the specified regular expression pattern.                                                  ',&
'                                                                                                                                ',&
'OPTIONS                                                                                                                         ',&
'    pattern  regular expression                                                                                                 ',&
'    -i       Ignore case distinctions in both the PATTERN and the input files.                                                  ',&
'                                                                                                                                ',&
'   MATCHER SELECTION                                                                                                            ',&
'                                                                                                                                ',&
'    -E       Interpret PATTERN as an ERE(extended regular expression).                                                          ',&
'    -G       Interpret PATTERN as a BRE(basic regular expression). This is the default.                                         ',&
'             If present, it takes precedence over -E.                                                                           ',&
'                                                                                                                                ',&
'   BASIC VS EXTENDED REGULAR EXPRESSIONS                                                                                        ',&
'                                                                                                                                ',&
'   In basic regular expressions the meta-characters ?, +, {, |,                                                                 ',&
'   (, and ) lose their special meaning; instead use the                                                                         ',&
'   backslashed versions \?, \+, \{, \|, \(, and \).                                                                             ',&
'                                                                                                                                ',&
'   INFORMATIVE                                                                                                                  ',&
'                                                                                                                                ',&
'    --help     display this help and exit                                                                                       ',&
'    --version  output version information and exit                                                                              ',&
'                                                                                                                                ',&
'EXAMPLES                                                                                                                        ',&
'    Sample commands                                                                                                             ',&
'                                                                                                                                ',&
'       grep-  ''^T.*found it'' <foundit                                                                                         ',&
'                                                                                                                                ',&
'REPORTING BUGS                                                                                                                  ',&
'    Report grep- bugs to <http://www.urbanjost.altervista.org/index.html>                                                       ',&
'                                                                                                                                ',&
'SEE ALSO                                                                                                                        ',&
'                                                                                                                                ',&
'AUTHOR                                                                                                                          ',&
'   John S. Urban                                                                                                                ',&
'LICENSE                                                                                                                         ',&
'   Public Domain                                                                                                                ',&
'']
   WRITE(*,'(a)')(trim(help_text(i)),i=1,size(help_text))
   stop ! if --help was specified, stop
endif
end subroutine help_usage
!>
!!##NAME
!!     grep-(1f) - [FUNIX] search a file for a pattern
!!     (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!     grep- pattern [ -i][ -E| -G]|[ --help| --version]
!!
!!##DESCRIPTION
!!
!!     Find lines on stdin that contain the specified regular expression pattern.
!!
!!##OPTIONS
!!     pattern  regular expression
!!     -i       Ignore case distinctions in both the PATTERN and the input files.
!!
!!    MATCHER SELECTION
!!
!!     -E       Interpret PATTERN as an ERE(extended regular expression).
!!     -G       Interpret PATTERN as a BRE(basic regular expression). This is the default.
!!              If present, it takes precedence over -E.
!!
!!    BASIC VS EXTENDED REGULAR EXPRESSIONS
!!
!!    In basic regular expressions the meta-characters ?, +, {, |,
!!    (, and ) lose their special meaning; instead use the
!!    backslashed versions \?, \+, \{, \|, \(, and \).
!!
!!    INFORMATIVE
!!
!!     --help     display this help and exit
!!     --version  output version information and exit
!!
!!##EXAMPLES
!!
!!     Sample commands
!!
!!        grep-  '^T.*found it' <foundit
!!
!!##REPORTING BUGS
!!     Report grep- bugs to <http://www.urbanjost.altervista.org/index.html>
!!
!!##SEE ALSO
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine help_version(l_version)
implicit none
character(len=*),parameter     :: ident="@(#)help_version(3f): prints version information"
logical,intent(in)             :: l_version
character(len=:),allocatable   :: help_text(:)
integer                        :: i
logical                        :: stopit=.false.
stopit=.false.
if(l_version)then
help_text=[ CHARACTER(LEN=128) :: &
'@(#)PRODUCT:        GPF (General Purpose Fortran) utilities and examples>',&
'@(#)PROGRAM:        grep-(1)>',&
'@(#)DESCRIPTION:    search a file for a pattern>',&
'@(#)VERSION:        1.0, 20180120>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)HOME PAGE:      http://www.urbanjost.altervista.org/index.html>',&
'@(#)COMPILED:       2022-01-09 23:08:27 UTC-300>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if --version was specified, stop
endif
end subroutine help_version
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
program grep
use M_kracken, only : kracken, lget, sget                             ! add command-line parser module
use M_regex,   only : regex_type, regcomp, regexec, regmatch, regfree
use M_io, only      : read_line
implicit none
   character(len=:),allocatable :: pattern
   character(len=:),allocatable :: input_line
   character(len=:),allocatable :: buffer
   integer                      :: status=0
   logical                      :: match=.false.
   integer                      :: nmatch
   integer,allocatable          :: matches(:,:)
   type(regex_type)             :: regex
   integer                      :: i
   logical                      :: extended=.false.
   logical                      :: icase=.false.
   character(len=:),allocatable :: options
!-----------------------------------------------------------------------------------------------------------------------------------
   ! define command arguments, default values, crack command line
   call kracken('grep','-help .false. -i .f. -E .f. -G .f. -version .false. -repeat -1 -o .false.')
   call help_usage(lget('grep_help'))                                ! if -help option is present, display help text and exit
   call help_version(lget('grep_version'))                           ! if -version option is present, display version text and exit
   pattern = trim(sget('grep_oo'))                                   ! get -oo pattern
   options='m'
   extended=lget('grep_E')
   if(lget('grep_G'))extended=.FALSE.
   if(extended)options=options//'x'

   icase=lget('grep_i')
   if(icase)options=options//'i'
   if(.not.lget('grep_o')) options=options//'n'
!-----------------------------------------------------------------------------------------------------------------------------------
   call regcomp(regex,   & ! new regex object
   & pattern,            & ! regex pattern string
   & options,            & ! flag characters:
                           !   x = extended regex (REG_EXTENDED)
                           !   m = multi-line     (REG_NEWLINE)
                           !   i = case-insensitive (REG_ICASE)
                           !   n = no MATCH required (REG_NOSUB)
   & nmatch,             & ! number of subexpressions in regular expression
   & status)               ! If absent, errors are fatal
   if(status.ne.0)then                        ! Leave program if regex is faulty.  we could use regerror to decode the error ...
      write(*,*) "*regcomp* ERROR. status=", status
   endif
   allocate(matches(2,nmatch))
!-----------------------------------------------------------------------------------------------------------------------------------
   if(pattern.eq.'')then
      write(*,*) '*grep* null pattern'
      stop
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   INFINITE: do while (read_line(input_line)==0)
      match=regexec(regex,'('//input_line//new_line('a')//')',matches)
      if(match)then
         write(*,'(a)')trim(input_line)
         DO i=1,nmatch
            if(matches(2,i).le.-1)exit
            buffer=regmatch(i,input_line,matches)
         ENDDO
      endif
   enddo INFINITE
!-----------------------------------------------------------------------------------------------------------------------------------
   call regfree(regex)
end program grep
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
