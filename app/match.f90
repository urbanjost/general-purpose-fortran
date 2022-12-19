!----------------------------------------------------------------------------------------------------------------------------------!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!----------------------------------------------------------------------------------------------------------------------------------!
program match_exe
use M_kracken, only : kracken, sget, lget, UNNAMED
use M_io,      only : read_line
use M_match,   only : getpat, match, regex_pattern, yes, no, err
use M_strings, only : upper
implicit none
character(len=:),allocatable :: argument, line, prefix
integer                      :: lineno, test, lun, ios, i
type(regex_pattern)          :: p
logical                      :: ignore_case
   prefix=''
   call kracken('match','-e -help .false. -version .false. -i .false. -v .false.',style='args') ! define and crack command line
   call help_usage(lget('match_help'))             ! if -help option is present, display help text and exit
   call help_version(lget('match_version'))        ! if -version option is present, display version text and exit
   ignore_case=lget('match_i')
   test=merge(NO,YES,lget('match_v'))
   argument=sget('match_e')
   if(argument.eq.'')then
      stop '*match* missing expression.'
   endif
   if(ignore_case)then
      argument=upper(argument)
   endif

   if (getpat(argument, p%pat) .eq. ERR) then
      stop '*match* Illegal pattern.'
   endif
   deallocate(argument)

   if(size(unnamed).eq.0)then
      unnamed=[character(len=len(unnamed)) :: unnamed,'-']
   endif

   do i=1,size(unnamed)
      lineno = 0
      if(unnamed(i).eq.'-')then
         lun=5
         call scan()
      else
         open(newunit=lun,file=unnamed(i),iostat=ios)
         if(ios.eq.0)then
            if(size(unnamed).gt.2)then
               prefix=trim(unnamed(i))//': '
            else
               prefix=''
            endif
            call scan()
         endif
      endif
   enddo
contains
!----------------------------------------------------------------------------------------------------------------------------------!
subroutine scan()
   INFINITE: do while (read_line(line,lun)==0)
      lineno = lineno + 1
      ! silly efficiency as in convert to uppercase unconditionally
      !if (match(merge(upper(line),line,ignore_case), p%pat) .eq. test) then
      if(ignore_case)then
         if (match(upper(line), p%pat) .eq. test) then
            write(*,'(*(g0,1x))')prefix,lineno,trim(line)
         endif
      elseif (match(line, p%pat) .eq. test) then
         write(*,'(*(g0,1x))')prefix,lineno,trim(line)
      endif
   enddo INFINITE
end subroutine scan
!----------------------------------------------------------------------------------------------------------------------------------!
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
'    match(1f) - find occurrences of a Regular Expression in a file(s).                                                          ',&
'    (LICENSE:PD)                                                                                                                ',&
'SYNOPSIS                                                                                                                        ',&
'    match [-v] [-i] -e EXPRESSION [FILES]| --help| --version                                                                    ',&
'DESCRIPTION                                                                                                                     ',&
'    Search for Regular Expressions in input files.                                                                              ',&
'                                                                                                                                ',&
'       ^   beginning of line                                                                                                    ',&
'       $   end of line                                                                                                          ',&
'       []  class of characters. In a class definition                                                                           ',&
'            ^  as the first character, means to negate                                                                          ',&
'            -  if not the first character denotes a range                                                                       ',&
'               of characters                                                                                                    ',&
'       .   any character                                                                                                        ',&
'       *   repeat previous character zero or more times                                                                         ',&
'       \   escape next character                                                                                                ',&
'OPTIONS                                                                                                                         ',&
'    -e EXPRESSION  Regular expression describing strings to locate                                                              ',&
'                   in the input file(s)                                                                                         ',&
'    FILES          Names of files to search. Defaults to stdin.                                                                 ',&
'    -v             veto mode. Show lines NOT matching expression                                                                ',&
'    -i             ignore case                                                                                                  ',&
'    --help         display this help and exit                                                                                   ',&
'    --version      output version information and exit                                                                          ',&
'EXAMPLES                                                                                                                        ',&
'    Sample commands                                                                                                             ',&
'                                                                                                                                ',&
'     match ''^[A-Z ][A-Z ]*$'' <file.txt                                                                                        ',&
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
!!     match(1f) - find occurrences of a Regular Expression in a file(s).
!!     (LICENSE:PD)
!!##SYNOPSIS
!!
!!     match [-v] [-i] -e EXPRESSION [FILES]| --help| --version
!!##DESCRIPTION
!!     Search for Regular Expressions in input files.
!!
!!        ^   beginning of line
!!        $   end of line
!!        []  class of characters. In a class definition
!!             ^  as the first character, means to negate
!!             -  if not the first character denotes a range
!!                of characters
!!        .   any character
!!        *   repeat previous character zero or more times
!!        \   escape next character
!!##OPTIONS
!!     -e EXPRESSION  Regular expression describing strings to locate
!!                    in the input file(s)
!!     FILES          Names of files to search. Defaults to stdin.
!!     -v             veto mode. Show lines NOT matching expression
!!     -i             ignore case
!!     --help         display this help and exit
!!     --version      output version information and exit
!!##EXAMPLES
!!
!!     Sample commands
!!
!!      match '^[A-Z ][A-Z ]*$' <file.txt
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
!----------------------------------------------------------------------------------------------------------------------------------!
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
'@(#)PROGRAM:        match(1)>',&
'@(#)DESCRIPTION:    find occurrences of a Regular Expression in a file(s).>',&
'@(#)VERSION:        1.0, 20191231>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)LICENSE:        Public Domain. This is free software: you are free to change and redistribute it.>',&
'@(#)                There is NO WARRANTY, to the extent permitted by law.>',&
'@(#)COMPILED:       Fri, Apr 24th, 2020 3:14:50 PM>',&
'@(#)COMPILED:       2022-12-18 00:51:46 UTC-300>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if --version was specified, stop
endif
end subroutine help_version
!----------------------------------------------------------------------------------------------------------------------------------!
end program match_exe
!----------------------------------------------------------------------------------------------------------------------------------!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!----------------------------------------------------------------------------------------------------------------------------------!
