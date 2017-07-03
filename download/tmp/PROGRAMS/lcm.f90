program find_lcm
Use M_factor, only : lcm=>least_common_multiple
Use M_strings, only : s2vs
use M_kracken, only : kracken,igets,lget                  ! command line parsing
implicit none
integer             :: ios
integer,allocatable :: values(:)
integer             :: answer
character(len=4096) :: line
logical             :: verbose=.false.

   call kracken('lcm','--help F --version F -verbose F ') ! crack command line options
   call help_usage(lget('lcm_help'))                      ! if help text requested display it and quit
   call help_version(lget('lcm_version'))                 ! if version information requested display it and quit
   verbose=lget('lcm_verbose')
   values=igets('lcm_oo')                                 ! get numbers from command line

   if(size(values).eq.0)then                              ! if did not print a range and no number specified prompt for values
      INFINITE: do
        read(*,'(a)',iostat=ios) line                     ! read line of numbers
        if(is_iostat_end(ios))then
           exit
        elseif(ios.ne.0)then
           cycle
        endif
        values=int(s2vs(line))                            ! convert text line to array of numbers
        if(size(values).ne.0)then
           answer=lcm(values) ! function can return error messages, so do not call from WRITE(3f)
           if(verbose)then
              write(*,'(i0,"=lcm([",*(i0:,","))',advance='no') answer, values
              write(*,'("])")')
           else
              write(*,'(i0)') answer
           endif
        endif
      enddo INFINITE
   else
      answer=lcm(values) ! function can return error messages, so do not call from WRITE(3f)
      if(verbose)then
         write(*,'(i0,"=lcm([",*(i0:,","))',advance='no')answer, values
         write(*,'("])")')
      else
         write(*,'(i0)') answer
      endif
   endif

end program find_lcm
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
'       lcm(1f) - [NUMBERS]display least common multiple of a list of whole numbers',&
'                                                                                ',&
'SYNOPSIS                                                                        ',&
'       lcm [NUMBERS]...                                                         ',&
'                                                                                ',&
'DESCRIPTION                                                                     ',&
'   Print the LCM (Least Common Multiple) of a list of integer whole             ',&
'   NUMBERS. If none are specified on the command line, read them from           ',&
'   standard input, one list per line.                                           ',&
'                                                                                ',&
'   Typically, the numbers must be positive integers where                       ',&
'                                                                                ',&
'      2 <= NUMBER <= (2**31)-1 or 2147483647.                                   ',&
'                                                                                ',&
'   but if the result would be larger than (2**31)-1 an error is returned        ',&
'                                                                                ',&
'OPTIONS                                                                         ',&
'       NUMBERS    list of numbers whose least common multiple is to be          ',&
'                  determined                                                    ',&
'       --help     display this help and exit                                    ',&
'       --version  output version information and exit                           ',&
'       --verbose  produce verbose answer instead of simple integer result       ',&
'EXAMPLE                                                                         ',&
' Sample Usage:                                                                  ',&
'                                                                                ',&
'  >lcm 10 34 82                                                                 ',&
'  >6970                                                                         ',&
'                                                                                ',&
'  >lcm 10 34 82 -verbose                                                        ',&
'  >6970=lcm([10,34,82])                                                         ',&
'                                                                                ',&
'  >lcm 202023 2147483647                                                        ',&
'  >STOP 1                                                                       ',&
'  >*lcm* result larger than a standard integer = 433841088817881                ',&
'                                                                                ',&
'']
   WRITE(*,'(a)')(trim(help_text(i)),i=1,size(help_text))
   stop ! if -help was specified, stop
endif
end subroutine help_usage
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!        lcm(1f) - [NUMBERS]display least common multiple of a list of whole numbers
!!
!!##SYNOPSIS
!!
!!        lcm [NUMBERS]...
!!
!!##DESCRIPTION
!!    Print the LCM (Least Common Multiple) of a list of integer whole
!!    NUMBERS. If none are specified on the command line, read them from
!!    standard input, one list per line.
!!
!!    Typically, the numbers must be positive integers where
!!
!!       2 <= NUMBER <= (2**31)-1 or 2147483647.
!!
!!    but if the result would be larger than (2**31)-1 an error is returned
!!
!!##OPTIONS
!!        NUMBERS    list of numbers whose least common multiple is to be
!!                   determined
!!        --help     display this help and exit
!!        --version  output version information and exit
!!        --verbose  produce verbose answer instead of simple integer result
!!##EXAMPLE
!!
!!  Sample Usage:
!!
!!   >lcm 10 34 82
!!   >6970
!!
!!   >lcm 10 34 82 -verbose
!!   >6970=lcm([10,34,82])
!!
!!   >lcm 202023 2147483647
!!   >STOP 1
!!   >*lcm* result larger than a standard integer = 433841088817881
!===================================================================================================================================
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
'@(#)PRODUCT:        CLI library utilities and examples>',&
'@(#)PROGRAM:        lcm(1f)>',&
'@(#)DESCRIPTION:    Determine least common multiple of a list of integers>',&
'@(#)!VERSION:       1.0, 20170317>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)REPORTING BUGS: http://www.urbanjost.altervista.org/>',&
'@(#)HOME PAGE:      http://www.urbanjost.altervista.org/index.html>',&
'@(#)UUID:           8f39b1e5-592f-4a22-946f-b8da2aa49633>',&
'@(#)COMPILED:       Wed, Jun 14th, 2017 10:02:45 AM>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if -version was specified, stop
endif
end subroutine help_version
!-----------------------------------------------------------------------------------------------------------------------------------
