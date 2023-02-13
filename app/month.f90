!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
program month_exe
use M_kracken95, only : kracken,iget,lget,sget                       ! command line parsing
use M_time,    only : box_month, mo2v                                ! date and time
implicit none
character(len=*),parameter :: ident="@(#)month(1f):print calendar"
character(len=21)       :: calen(8)='                    '           ! character array to hold month
character(len=(21+2)*3) :: calenyear(8*4)='                      '   ! character array to hold year
integer                 :: month                                     ! values of command line options
integer                 :: dat_values(8)                             ! date array
integer                 :: r,c                                       ! row and column for month in one-year calendar
character(len=21)       :: cscr
   call date_and_time(values=dat_values)                             ! get current time and date
   call kracken('month',' -year -month 0 -help .f. -version .f.')    ! crack command line arguments
   call help_usage(lget('month_help'))                               ! print help information and stop if requested
   call help_version(lget('month_version'))                          ! print version information and stop if requested
!-----------------------------------------------------------------------------------------------------------------------------------
! use user-specified year date instead of current year. Try reading year from two places (-oo and -year) on command line
   if(sget('month_year').ne.' ')then                                 ! check -year option for a year value
      dat_values(1)=iget('month_year')                               ! if value was specified use it
   elseif(sget('month_oo').ne.' ')then
         dat_values(1)=iget('month_oo')                              ! check -oo option for a year value if did not find -year VALUE
   endif
   !write(*,*)'YEAR=',dat_values(1)
!-----------------------------------------------------------------------------------------------------------------------------------
   cscr=sget('month_month')                                          ! get month as string so can see if name or number or blank
   if(cscr.ne.'')then
      select case(cscr(1:1))
      case('A':'Z','a':'z')                                          ! assume month name instead of month number
        month=mo2v(trim(cscr))
      case default                                                   ! month is number
         month=iget('month_month')
      end select
   else                                                              ! keyword given but no value, default to current month
      month=dat_values(2)
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   if(month.eq.0)then                                                ! no month specified, display an entire year
      do r=1,4                                                       ! display year in four rows
         do c=1,3                                                    ! three months per row
            dat_values(2)=c+(r-1)*3
            call box_month(dat_values,calen)
            calenyear(8*r-7:8*r)(23*c-22:23*c)=calen                 ! copy month into large year array
         enddo
      enddo
      write(*,'(a)')calenyear
   else                                                              ! do a month
      dat_values(2)=month
      call box_month(dat_values,calen)
      write(*,'(a)')calen
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
contains
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
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
'   month(1f) - [TIME] display a calendar                                                                                        ',&
'   (LICENSE:PD)                                                                                                                 ',&
'                                                                                                                                ',&
'SYNOPSIS                                                                                                                        ',&
'   month [[ -year] NNNN] [ -month NN|month_name]                                                                                ',&
'                                                                                                                                ',&
'DESCRIPTION                                                                                                                     ',&
'   month(1) displays a simple calendar. If no arguments are specified,                                                          ',&
'   the current year is displayed.                                                                                               ',&
'                                                                                                                                ',&
'OPTIONS                                                                                                                         ',&
'   -month      Display single month output. The month is numeric (1-12)                                                         ',&
'               or a month name or blank. If blank the current month is assumed.                                                 ',&
'                                                                                                                                ',&
'   -year NNNN  Select the year to display. A year starts on Jan 1st.                                                            ',&
'                                                                                                                                ',&
'   -help       Display help text and exit.                                                                                      ',&
'   -version    Display version information and exit.                                                                            ',&
'                                                                                                                                ',&
'EXAMPLES                                                                                                                        ',&
'      month -month 12                                                                                                           ',&
'                                                                                                                                ',&
'       >    December 2015                                                                                                       ',&
'       >Mo Tu We Th Fr Sa Su                                                                                                    ',&
'       >    1  2  3  4  5  6                                                                                                    ',&
'       > 7  8  9 10 11 12 13                                                                                                    ',&
'       >14 15 16 17 18 19 20                                                                                                    ',&
'       >21 22 23 24 25 26 27                                                                                                    ',&
'       >28 29 30 31                                                                                                             ',&
'                                                                                                                                ',&
'      month -month April # month names may be given instead of numbers                                                          ',&
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
!!    month(1f) - [TIME] display a calendar
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    month [[ -year] NNNN] [ -month NN|month_name]
!!
!!##DESCRIPTION
!!    month(1) displays a simple calendar. If no arguments are specified,
!!    the current year is displayed.
!!
!!##OPTIONS
!!    -month      Display single month output. The month is numeric (1-12)
!!                or a month name or blank. If blank the current month is assumed.
!!
!!    -year NNNN  Select the year to display. A year starts on Jan 1st.
!!
!!    -help       Display help text and exit.
!!    -version    Display version information and exit.
!!
!!##EXAMPLES
!!
!!       month -month 12
!!
!!        >    December 2015
!!        >Mo Tu We Th Fr Sa Su
!!        >    1  2  3  4  5  6
!!        > 7  8  9 10 11 12 13
!!        >14 15 16 17 18 19 20
!!        >21 22 23 24 25 26 27
!!        >28 29 30 31
!!
!!       month -month April # month names may be given instead of numbers
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
'@(#)PROGRAM:        month(1f)>',&
'@(#)DESCRIPTION:    displays simple calendar>',&
'@(#)VERSION:        1.0, 2015-12-20>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)REPORTING BUGS: http://www.urbanjost.altervista.org/>',&
'@(#)HOME PAGE:      http://www.urbanjost.altervista.org/index.html>',&
'@(#)LICENSE:        Public Domain. This is free software: you are free to change and redistribute it.>',&
'@(#)                There is NO WARRANTY, to the extent permitted by law.>',&
'@(#)COMPILED:       2023-02-12 18:34:45 UTC-300>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if --version was specified, stop
endif
end subroutine help_version
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
end program month_exe
