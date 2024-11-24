!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
program demo_read_table
implicit none
! ident_1="@(#) area_of_simple_polygon(1f) determine area of simple (non-intersecting) polygon"
call main()
contains
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
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
'     area_of_simple_polygon(1f) - determine area of simple (non-intersecting) polygon given coordinates of points',&
'     (LICENSE:PD)                                                               ',&
'SYNOPSIS                                                                        ',&
'     area_of_simple_polygon [file(s)|[ --help| --version]]                      ',&
'DESCRIPTION                                                                     ',&
'     read file(s) composed of lines of xy pairs and calculate                   ',&
'     area of polygon, assuming points define a simple (non-intersecting)        ',&
'     polygon. If not closed, the last point will be connected to the            ',&
'     first point.                                                               ',&
'                                                                                ',&
'     Shows example of reading arguments from command line of arbitrary          ',&
'     length and files of numbers of arbitrary size.                             ',&
'OPTIONS                                                                         ',&
'     filenames  name of files containing xy pairs defining a simple polygon     ',&
'     --help     display this help and exit                                      ',&
'     --version   output version information and exit                            ',&
'EXAMPLE                                                                         ',&
'  Given file in.1 with the following contents:                                  ',&
'                                                                                ',&
'   -5 -5                                                                        ',&
'   0 0                                                                          ',&
'   -5 5                                                                         ',&
'   5 5                                                                          ',&
'   0 0                                                                          ',&
'   5 -5                                                                         ',&
'                                                                                ',&
'  Expected output:                                                              ',&
'                                                                                ',&
'             14           7           2                                         ',&
'   -5.0000000000000000 -5.0000000000000000                                      ',&
'   0.0000000000000000 0.0000000000000000                                        ',&
'   -5.0000000000000000 5.0000000000000000                                       ',&
'   5.0000000000000000 5.0000000000000000                                        ',&
'   0.0000000000000000 0.0000000000000000                                        ',&
'   5.0000000000000000 -5.0000000000000000                                       ',&
'    area=  -50.000000000000000                                                  ',&
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
!!      area_of_simple_polygon(1f) - determine area of simple (non-intersecting) polygon given coordinates of points
!!      (LICENSE:PD)
!!##SYNOPSIS
!!
!!      area_of_simple_polygon [file(s)|[ --help| --version]]
!!##DESCRIPTION
!!      read file(s) composed of lines of xy pairs and calculate
!!      area of polygon, assuming points define a simple (non-intersecting)
!!      polygon. If not closed, the last point will be connected to the
!!      first point.
!!
!!      Shows example of reading arguments from command line of arbitrary
!!      length and files of numbers of arbitrary size.
!!##OPTIONS
!!      filenames  name of files containing xy pairs defining a simple polygon
!!      --help     display this help and exit
!!      --version   output version information and exit
!!##EXAMPLE
!!
!!   Given file in.1 with the following contents:
!!
!!    -5 -5
!!    0 0
!!    -5 5
!!    5 5
!!    0 0
!!    5 -5
!!
!!   Expected output:
!!
!!              14           7           2
!!    -5.0000000000000000 -5.0000000000000000
!!    0.0000000000000000 0.0000000000000000
!!    -5.0000000000000000 5.0000000000000000
!!    5.0000000000000000 5.0000000000000000
!!    0.0000000000000000 0.0000000000000000
!!    5.0000000000000000 -5.0000000000000000
!!     area=  -50.000000000000000
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
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
'@(#)PRODUCT:        GPF (General Purpose Fortran) utilities and examples>',&
'@(#)PROGRAM:        area_of_simple_polygon(1)>',&
'@(#)DESCRIPTION:    find area of simple (non-intersecting) polygon>',&
'@(#)VERSION:        1.0, 20181231>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)HOME PAGE:      http://www.urbanjost.altervista.org/index.html>',&
'@(#)COMPILED:       2024-11-24 04:45:49 UTC-300>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if --version was specified, stop
endif
end subroutine help_version
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine main()
   use M_kracken, only : kracken, lget, sget, iget                  ! add command-line parser module
use M_io, only : read_table
use M_math, only : polyarea_shoelace
doubleprecision,allocatable  :: array(:,:)
doubleprecision              :: area
integer                      :: ierr
integer                      :: ios
integer                      :: count,i,j,longest,argument_length
character(len=:),allocatable :: arguments(:)

   call kracken('area','-help .f. -version .f. -repeat -1 ')        ! define command arguments,default values and crack command line
   call help_usage(lget('area_help'))                               ! if -help option is present, display help text and exit
   call help_version(lget('area_version'))                          ! if -version option is present, display version text and exit

!  use intrinsics instead of sgets as an example
   count = command_argument_count() ! get number of arguments
   longest=0                        ! find longest argument
   do i=0,count
      call get_command_argument(number=i,length=argument_length)
      longest=max(longest,argument_length)
   enddo
   allocate(character(len=longest) :: arguments(0:count)) ! allocate string array big enough to hold command line
   ! read the arguments into the array
   do i=0,count
      call get_command_argument(i, arguments(i))
   enddo
   do i=1,count
      ! read file as a table
      call read_table(arguments(i),array,ierr)
      if(ierr.ne.0)then
         write(*,*)'*demo_read_table* ERROR: could not read input file'
         stop
      elseif(size(array,dim=2).ne.2)then
         write(*,*)'*demo_read_table* ERROR: two columns required'
         stop
      else
         write(*,*)size(array),size(array,dim=1),size(array,dim=2)
         write(*,'(g0,1x,g0)')(array(j,:),j=1,size(array,dim=1))
         area=polyarea_shoelace(array(:,1),array(:,2))
         write(*,*)'area=',area
      endif
      close(unit=10,iostat=ios)
   enddo
end subroutine main
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
end program demo_read_table
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
