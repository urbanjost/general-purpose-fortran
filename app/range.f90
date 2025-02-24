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
'   range(1f) - [M_strings] expand list of whole numbers where negative curve numbers designate ranges (1 -10 means 1 thru 10)',&
'   (LICENSE:PD)                                                                 ',&
'SYNOPSIS                                                                        ',&
'   range VALUES                                                                 ',&
'                                                                                ',&
'DESCRIPTION                                                                     ',&
'   Given a list of integers expand it to a list of up to 1000 positive          ',&
'   integers assuming negative numbers denote the end of a range                 ',&
'   (1 -10 means 1 through 10, for example).                                     ',&
'                                                                                ',&
'OPTIONS                                                                         ',&
'   VALUES  list of whole numbers. Negative numbers designate the end of ranges. ',&
'                                                                                ',&
'EXAMPLE                                                                         ',&
'   range 1 20 -30 101 100 99 100 -120 222 -200                                  ',&
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
!!    range(1f) - [M_strings] expand list of whole numbers where negative curve numbers designate ranges (1 -10 means 1 thru 10)
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    range VALUES
!!
!!##DESCRIPTION
!!    Given a list of integers expand it to a list of up to 1000 positive
!!    integers assuming negative numbers denote the end of a range
!!    (1 -10 means 1 through 10, for example).
!!
!!##OPTIONS
!!    VALUES  list of whole numbers. Negative numbers designate the end of ranges.
!!
!!##EXAMPLE
!!
!!    range 1 20 -30 101 100 99 100 -120 222 -200
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
'@(#)PROGRAM:        _range(1)>',&
'@(#)DESCRIPTION:    expand list of whole numbers where negative curve numbers designate ranges (1 -10 means 1 thru 10)>',&
'@(#)VERSION:        1.0, 20170812>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)COMPILED:       2025-02-23 19:26:15 UTC-300>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if --version was specified, stop
endif
end subroutine help_version
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
program demo_range
use M_strings, only : listout, s2vs, replace
use M_kracken, only : kracken, lget, sget     ! add command-line parser module
implicit none
integer,allocatable :: icurve_lists(:)        ! icurve_lists is input array
integer             :: icurve_expanded(1000)  ! icurve_expanded is output array
integer             :: inums                  ! number of icurve_lists values on input, number of icurve_expanded numbers on output
integer             :: i
integer             :: ierr
logical             :: debug
!-----------------------------------------------------------------------------------------------------------------------------------
   call kracken('range','-help F -version F -repeat -1 -debug F')  ! define command arguments,default values and crack command line
   call help_usage(lget('range_help'))                             ! if -help option is present, display help text and exit
   call help_version(lget('range_version'))                        ! if -version option is present, display version text and exit
   debug=lget('range_debug')
   icurve_lists=s2vs(replace(sget('range_oo'),'-',' -'))
   if(debug)then
      write(*,'(*(g0:,1x))') sget('range_oo')
      write(*,'(*(g0:,1x))') replace(sget('range_oo'),'-',' -')
      write(*,'(*(g0:,1x))') s2vs(replace(sget('range_oo'),'-',' -'))
      write(*,'(*(g0:,1x))') icurve_lists
   endif
   inums=size(icurve_expanded)
   call listout(icurve_lists,icurve_expanded,inums,ierr) ! copy ICURVE() to ICURVE_EXPANDED() expanding negative numbers to ranges
   if(debug)then
      write(*,'(*(g0:,1x))') icurve_expanded
   endif
   write(*,'(i0)')(icurve_expanded(i),i=1,inums)
end program demo_range
!-----------------------------------------------------------------------------------------------------------------------------------
