program find_gcd
Use M_factor, only : gcd=>greatest_common_divisor
Use M_strings, only : s2vs
use M_kracken, only : kracken,igets,lget                  ! command line parsing
implicit none
integer             :: ios, ios_count=0
integer,allocatable :: values(:)
integer             :: answer
character(len=4096) :: line
logical             :: verbose=.false.
!-----------------------------------------------------------------------------------------------------------------------------------
   call kracken('gcd','--help F --version F -verbose F ') ! crack command line options
   call help_usage(lget('gcd_help'))                      ! if help text requested display it and quit
   call help_version(lget('gcd_version'))                 ! if version information requested display it and quit
   verbose=lget('gcd_verbose')                            ! get -verbose flag
   values=igets('gcd_oo')                                 ! get numbers from command line
!-----------------------------------------------------------------------------------------------------------------------------------
   if(size(values).eq.0)then                              ! if did not print a range and no number specified prompt for values
      INFINITE: do
        read(*,'(a)',iostat=ios) line                     ! read line of numbers
        if(is_iostat_end(ios))then                        ! if got EOF(end of file) exit loop
           exit INFINITE
        elseif(ios.ne.0)then                              ! if I/O error reading line read another (could loop on system errors)
           ios_count=ios_count+1
           if(ios_count.gt.1000)exit INFINITE             ! check to avoid infinite loop if stuck on system I/O error not an EOF
           cycle INFINITE                                 ! get a new line, had error reading last one
        endif
        values=int(s2vs(line))                            ! convert text line to array of numbers
        if(size(values).ne.0)then
           answer=gcd(values)                             ! function can return error messages, so do not call from WRITE(3f)
           if(verbose)then                                ! write in format "ANSWER=gcd([VAL(1),VAL(2),VAL(3),...])
              write(*,'(i0,"=gcd([",*(i0:,","))',advance='no') answer, values
              write(*,'("])")')
           else                                           ! just simply write integer answer
              write(*,'(i0)') answer
           endif
        endif
      enddo INFINITE
   else
      answer=gcd(values)                                  ! function can return error messages, so do not call from WRITE(3f)
      if(verbose)then
         write(*,'(i0,"=gcd([",*(i0:,","))',advance='no')answer, values
         write(*,'("])")')
      else
         write(*,'(i0)') answer
      endif
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
end program find_gcd
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
'       gcd(1f) - [NUMBERS]display greatest common divisor of a list of whole numbers',&
'                                                                                ',&
'SYNOPSIS                                                                        ',&
'       gcd [NUMBERS]...                                                         ',&
'                                                                                ',&
'DESCRIPTION                                                                     ',&
'   Print the gcd (Greatest Common Divisor) of a list of integer whole           ',&
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
'       NUMBERS    list of numbers whose greatest common divisor is to be        ',&
'                  determined                                                    ',&
'       --help     display this help and exit                                    ',&
'       --version  output version information and exit                           ',&
'       --verbose  produce verbose answer instead of simple integer result       ',&
'EXAMPLE                                                                         ',&
' Sample Usage:                                                                  ',&
'                                                                                ',&
'  gcd 10 34 82                                                                  ',&
'  2                                                                             ',&
'                                                                                ',&
'  gcd 10 34 82 -verbose                                                         ',&
'  2=gcd([10,34,82])                                                             ',&
'                                                                                ',&
'']
   WRITE(*,'(a)')(trim(help_text(i)),i=1,size(help_text))
   stop ! if -help was specified, stop
endif
end subroutine help_usage
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!        gcd(1f) - [NUMBERS]display greatest common divisor of a list of whole numbers
!!
!!##SYNOPSIS
!!
!!        gcd [NUMBERS]...
!!
!!##DESCRIPTION
!!    Print the gcd (Greatest Common Divisor) of a list of integer whole
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
!!        NUMBERS    list of numbers whose greatest common divisor is to be
!!                   determined
!!        --help     display this help and exit
!!        --version  output version information and exit
!!        --verbose  produce verbose answer instead of simple integer result
!!##EXAMPLE
!!
!!  Sample Usage:
!!
!!   gcd 10 34 82
!!   2
!!
!!   gcd 10 34 82 -verbose
!!   2=gcd([10,34,82])
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
'@(#)PROGRAM:        gcd(1f)>',&
'@(#)DESCRIPTION:    Determine greatest common divisor of a list of integers>',&
'@(#)!VERSION:       1.0, 20170317>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)REPORTING BUGS: http://www.urbanjost.altervista.org/>',&
'@(#)HOME PAGE:      http://www.urbanjost.altervista.org/index.html>',&
'@(#)UUID:           8f39b1e5-592f-4a22-946f-b8da2aa49633>',&
'@(#)COMPILED:       Wed, Jun 14th, 2017 10:03:02 AM>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if -version was specified, stop
endif
end subroutine help_version
!-----------------------------------------------------------------------------------------------------------------------------------
