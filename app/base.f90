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
'NAME                                                                            ',&
'                                                                                ',&
'   base(1f) - [CONVERT] convert numbers between bases                           ',&
'   (LICENSE:PD)                                                                 ',&
'                                                                                ',&
'SYNOPSIS                                                                        ',&
'   base values [ -ibase NN][ -obase MM][ -brief] |[[ --help]|[ --version]]      ',&
'                                                                                ',&
'DESCRIPTION                                                                     ',&
'                                                                                ',&
'   This program converts a whole number in base IBASE into a number in          ',&
'   base OBASE (IBASE and OBASE must be between 2 and 36).                       ',&
'                                                                                ',&
'   The letters A,B,...,Z represent 10,11,...,36 in a base > 10.                 ',&
'                                                                                ',&
'   The number is first converted from base IBASE to base 10 by                  ',&
'   DecodeBase(3f), then converted from base 10 to base OBASE by                 ',&
'   CodeBase(3f).                                                                ',&
'                                                                                ',&
'OPTIONS                                                                         ',&
'                                                                                ',&
'    values     space-delimited strings representing input values                ',&
'    -ibase NN  default base for input values                                    ',&
'               If the input values are whole numbers that are of the            ',&
'               form NN:MMMM or NN#MMMM where NN is the base and MMMM the        ',&
'               value the numbers are interpreted in the explicit base           ',&
'               the numbers represent. Otherwise, they are interpreted           ',&
'               as in base NN. NN defaults to 10                                 ',&
'    -obase MM  base for output values. The Default is 10.                       ',&
'    --brief    just show output value with no base designation                  ',&
'    --help     display this help and exit                                       ',&
'    --version  output version information and exit                              ',&
'                                                                                ',&
'    Having a pound character (#) in an input line is problematic,               ',&
'    as most shell programs and the M_kracken(3f) command line                   ',&
'    parser independently treat the character as beginning an in-line            ',&
'    comment. Avoid using pound characters and use the colon instead when        ',&
'    using explicit base numbers in values.                                      ',&
'                                                                                ',&
'EXAMPLE                                                                         ',&
'                                                                                ',&
'  Sample runs:                                                                  ',&
'                                                                                ',&
'    # convert base2 values to base10                                            ',&
'    base 10 1010 101010 10101010 1010101010 101010101010 -ibase 2               ',&
'      2#10=10#2                                                                 ',&
'      2#1010=10#10                                                              ',&
'      2#101010=10#42                                                            ',&
'      2#10101010=10#170                                                         ',&
'      2#1010101010=10#682                                                       ',&
'      2#101010101010=10#2730                                                    ',&
'                                                                                ',&
'    # convert base2 values to base10 in brief mode                              ',&
'    base 10 1010 101010 10101010 1010101010 101010101010 -ibase 2 -brief        ',&
'      2 10 42 170 682 2730                                                      ',&
'                                                                                ',&
'    # convert base10 values to base2                                            ',&
'    base 2 10 42 170 682 2730 -obase 2                                          ',&
'      10#2=2#10                                                                 ',&
'      10#10=2#1010                                                              ',&
'      10#42=2#101010                                                            ',&
'      10#170=2#10101010                                                         ',&
'      10#682=2#1010101010                                                       ',&
'      10#2730=2#101010101010                                                    ',&
'                                                                                ',&
'    # convert base10 values to base3                                            ',&
'    base 10 20 30 40 50 -obase 3                                                ',&
'      10#10=3#101                                                               ',&
'      10#20=3#202                                                               ',&
'      10#30=3#1010                                                              ',&
'      10#40=3#1111                                                              ',&
'      10#50=3#1212                                                              ',&
'                                                                                ',&
'    # convert values of various explicit bases to base10                        ',&
'    base 2:11 3:1212 4:123123                                                   ',&
'      2:11=10#3                                                                 ',&
'      3:1212=10#50                                                              ',&
'      4:123123=10#1755                                                          ',&
'                                                                                ',&
'    # convert values of various explicit bases to base10                        ',&
'    # note the use of both single and double quotes to avoid                    ',&
'    # problems with a pound character being treated as the start                ',&
'    # of an in-line comment                                                     ',&
'    base ''"2#11 3#1212 4#123123"''                                             ',&
'      2#11=10#3                                                                 ',&
'      3#1212=10#50                                                              ',&
'      4#123123=10#1755                                                          ',&
'                                                                                ',&
'    # convert values of various explicit bases to base2 in brief mode           ',&
'    base 2:1111 3:10 4:10 8:10 16:10 -obase 2 -brief                            ',&
'      1111 11 100 1000 10000                                                    ',&
'                                                                                ',&
'AUTHOR                                                                          ',&
'   John S. Urban                                                                ',&
'LICENSE                                                                         ',&
'   Public Domain                                                                ',&
'']
   WRITE(*,'(a)')(trim(help_text(i)),i=1,size(help_text))
   stop ! if --help was specified, stop
endif
end subroutine help_usage
!>
!!##NAME
!!
!!    base(1f) - [CONVERT] convert numbers between bases
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    base values [ -ibase NN][ -obase MM][ -brief] |[[ --help]|[ --version]]
!!
!!##DESCRIPTION
!!
!!    This program converts a whole number in base IBASE into a number in
!!    base OBASE (IBASE and OBASE must be between 2 and 36).
!!
!!    The letters A,B,...,Z represent 10,11,...,36 in a base > 10.
!!
!!    The number is first converted from base IBASE to base 10 by
!!    DecodeBase(3f), then converted from base 10 to base OBASE by
!!    CodeBase(3f).
!!
!!##OPTIONS
!!
!!     values     space-delimited strings representing input values
!!     -ibase NN  default base for input values
!!                If the input values are whole numbers that are of the
!!                form NN:MMMM or NN#MMMM where NN is the base and MMMM the
!!                value the numbers are interpreted in the explicit base
!!                the numbers represent. Otherwise, they are interpreted
!!                as in base NN. NN defaults to 10
!!     -obase MM  base for output values. The Default is 10.
!!     --brief    just show output value with no base designation
!!     --help     display this help and exit
!!     --version  output version information and exit
!!
!!     Having a pound character (#) in an input line is problematic,
!!     as most shell programs and the M_kracken(3f) command line
!!     parser independently treat the character as beginning an in-line
!!     comment. Avoid using pound characters and use the colon instead when
!!     using explicit base numbers in values.
!!
!!##EXAMPLE
!!
!!
!!   Sample runs:
!!
!!     # convert base2 values to base10
!!     base 10 1010 101010 10101010 1010101010 101010101010 -ibase 2
!!       2#10=10#2
!!       2#1010=10#10
!!       2#101010=10#42
!!       2#10101010=10#170
!!       2#1010101010=10#682
!!       2#101010101010=10#2730
!!
!!     # convert base2 values to base10 in brief mode
!!     base 10 1010 101010 10101010 1010101010 101010101010 -ibase 2 -brief
!!       2 10 42 170 682 2730
!!
!!     # convert base10 values to base2
!!     base 2 10 42 170 682 2730 -obase 2
!!       10#2=2#10
!!       10#10=2#1010
!!       10#42=2#101010
!!       10#170=2#10101010
!!       10#682=2#1010101010
!!       10#2730=2#101010101010
!!
!!     # convert base10 values to base3
!!     base 10 20 30 40 50 -obase 3
!!       10#10=3#101
!!       10#20=3#202
!!       10#30=3#1010
!!       10#40=3#1111
!!       10#50=3#1212
!!
!!     # convert values of various explicit bases to base10
!!     base 2:11 3:1212 4:123123
!!       2:11=10#3
!!       3:1212=10#50
!!       4:123123=10#1755
!!
!!     # convert values of various explicit bases to base10
!!     # note the use of both single and double quotes to avoid
!!     # problems with a pound character being treated as the start
!!     # of an in-line comment
!!     base '"2#11 3#1212 4#123123"'
!!       2#11=10#3
!!       3#1212=10#50
!!       4#123123=10#1755
!!
!!     # convert values of various explicit bases to base2 in brief mode
!!     base 2:1111 3:10 4:10 8:10 16:10 -obase 2 -brief
!!       1111 11 100 1000 10000
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
'@(#)PROGRAM:        base(1)>',&
'@(#)DESCRIPTION:    convert numbers between bases>',&
'@(#)VERSION:        1.0, 20170916>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)REPORTING BUGS: http://www.urbanjost.altervista.org/>',&
'@(#)COMPILED:       2021-07-01 09:05:23 UTC-240>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if --version was specified, stop
endif
end subroutine help_version
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
program demo_base
use M_strings, only : codebase, decodebase, lower, string_to_value
use M_kracken, only : kracken, sgets, lget, sget, iget                  ! add command-line parser module
use M_kracken, only : kracken, kracken_comment
use M_verify,  only : debug
implicit none
character(len=80)             :: endstring
integer                       :: value_in_base10
integer                       :: ibase, obase, i, ierr
character(len=80),allocatable :: values(:)
logical                       :: brief
!-----------------------------------------------------------------------------------------------------------------------------------
   ! define command arguments,default values and crack command line
   kracken_comment='!'
   call kracken('base','-help .f. -version .f. -ibase 10 -obase 10 -brief .f. -debug .f. ')
!-----------------------------------------------------------------------------------------------------------------------------------
   call help_usage(lget('base_help'))                                ! if -help option is present, display help text and exit
   call help_version(lget('base_version'))                           ! if -version option is present, display version text and exit
!-----------------------------------------------------------------------------------------------------------------------------------
   ibase=iget('base_ibase')
   obase=iget('base_obase')
   brief=lget('base_brief')
   debug=lget('base_debug')
   values=sgets('base_oo')
   do i=1,size(values)
      if(debug)write(*,*)i,'==>',lower(values(i))
      if(scan(lower(values(i)),'#:').eq.0)then ! REGULAR NUMBER
         if(decodebase(values(i),ibase,value_in_base10)) then
            if(CodeBase(value_in_base10,obase,endstring)) then
               if(brief)then
                  write(*,'(A,1x)',advance='no')trim(endstring)
               else
                  write(*,'(i0,"#",A,"=",I0,"#",A)')  ibase,trim(values(i)),obase, trim(endstring)
               endif
            else
               print *,'Error in coding number ',trim(values(i)), 'ibase=',ibase,'obase=',obase
            endif
         else
            print *,'Error in decoding number ',trim(values(i)), 'ibase=',ibase,'obase=',obase
         endif
      else ! EXPLICIT BASE SPECIFIED (NN#MMMMM)
         call string_to_value(values(i),value_in_base10,ierr)
         if(debug)write(*,*)i,'==>',lower(values(i)),'==>',value_in_base10
         if(ierr.eq.0) then
            if(CodeBase(value_in_base10,obase,endstring)) then
               if(brief)then
                  write(*,'(A,1x)',advance='no')trim(endstring)
               else
                  write(*,'(A,"=",I0,"#",A)')  trim(values(i)),obase, trim(endstring)
               endif
            else
               print *,'Error in coding number ',trim(values(i)), 'ibase=',ibase,'obase=',obase
            endif
         else
            print *,'Error in decoding number NN#MMMM=',trim(values(i)), 'ibase=',ibase,'obase=',obase
         endif
      endif
   enddo

   if(brief)then
      write(*,*)
   endif

end program demo_base
