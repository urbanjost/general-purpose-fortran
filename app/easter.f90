!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
program demo_easter
use m_time, only : easter, fmtdate
use M_kracken, only : kracken, lget, iget  ! add command-line parser module
implicit none
integer          :: year
integer          :: dat(8)
!-----------------------------------------------------------------------------------------------------------------------------------
   call kracken('easter','0 -help .f. -version .f. ') ! define command arguments,default values and crack command line
   call help_usage(lget('easter_help'))               ! if -help option is present, display help text and exit
   call help_version(lget('easter_version'))          ! if -version option is present, display version text and exit
!-----------------------------------------------------------------------------------------------------------------------------------
   call date_and_time(values=dat)  ! get current year
   year=iget('easter_oo')          ! get year off command line
   if(year.eq.0)then               ! if year from command line is 0, use current year
        year=dat(1)
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   if(year.lt.0)then
      do year = 1980, 2200
        call easter(year, dat)
        write(*,*)fmtdate(dat,"Easter day: the %d day of %L in the year of our Lord %Y")
      end do
   else
!-----------------------------------------------------------------------------------------------------------------------------------
      call easter(year, dat)   ! given year get month and day Easter falls on
      write(*,*)fmtdate(dat,"Easter day: the %d day of %L in the year of our Lord %Y")
!-----------------------------------------------------------------------------------------------------------------------------------
   endif
contains
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
'@(#)PROGRAM:        easter(1)>',&
'@(#)DESCRIPTION:    output the month and day Easter falls on for a particular year>',&
'@(#)VERSION:        1.0, 20170223>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)COMPILED:       2022-08-14 13:34:54 UTC-240>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if --version was specified, stop
endif
end subroutine help_version
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
'   easter(1f) - [FUNIX] print day and month Easter falls on for given year                                                      ',&
'   (LICENSE:PD)                                                                                                                 ',&
'                                                                                                                                ',&
'SYNOPSIS                                                                                                                        ',&
'   easter [year]|[ --help|--version]                                                                                            ',&
'                                                                                                                                ',&
'DESCRIPTION                                                                                                                     ',&
'   Prints day Easter falls on                                                                                                   ',&
'                                                                                                                                ',&
'OPTIONS                                                                                                                         ',&
'   year       year for which to calculate Easter day. Defaults to current year                                                  ',&
'   --help     display this help and exit                                                                                        ',&
'   --version  output version information and exit                                                                               ',&
'                                                                                                                                ',&
'EXAMPLES                                                                                                                        ',&
'  Sample commands                                                                                                               ',&
'                                                                                                                                ',&
'   easter 2017                                                                                                                  ',&
'   Easter day: the 16th day of April in the year of our Lord 2017                                                               ',&
'AUTHOR                                                                                                                          ',&
'   John S. Urban                                                                                                                ',&
'LICENSE                                                                                                                         ',&
'   Public License                                                                                                               ',&
'']
   WRITE(*,'(a)')(trim(help_text(i)),i=1,size(help_text))
   stop ! if --help was specified, stop
endif
end subroutine help_usage
!>
!!##NAME
!!    easter(1f) - [FUNIX] print day and month Easter falls on for given year
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    easter [year]|[ --help|--version]
!!
!!##DESCRIPTION
!!    Prints day Easter falls on
!!
!!##OPTIONS
!!    year       year for which to calculate Easter day. Defaults to current year
!!    --help     display this help and exit
!!    --version  output version information and exit
!!
!!##EXAMPLES
!!
!!   Sample commands
!!
!!    easter 2017
!!    Easter day: the 16th day of April in the year of our Lord 2017
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public License
   end program demo_easter
!-----------------------------------------------------------------------------------------------------------------------------------
