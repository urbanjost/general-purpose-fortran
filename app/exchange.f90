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
'    exchange(1f) - [CONVERT] test of basic unit conversion functions            ',&
'    (LICENSE:PD)                                                                ',&
'                                                                                ',&
'SYNOPSIS                                                                        ',&
'                                                                                ',&
'  \                                                                             ',&
'                                                                                ',&
'    exchange [[ -feet VALUES] [ -meters VALUES]                                 ',&
'             [ -celsius VALUES] [ -fahrenheit VALUES]                           ',&
'             [ -degrees VALUES] [ -radians VALUES]]                             ',&
'             |[ --help| -version]                                               ',&
'                                                                                ',&
'DESCRIPTION                                                                     ',&
'     Makes common unit conversions such as between feet and meters,             ',&
'     celsius and fahrenheit, and degrees and radians.                           ',&
'                                                                                ',&
'OPTIONS                                                                         ',&
'    -feet VALUES        convert feet to meters                                  ',&
'    -meters VALUES      convert meters to feet                                  ',&
'    -fahrenheit VALUES  convert fahrenheit to celsius                           ',&
'    -celsius VALUES     convert celsius to fahrenheit                           ',&
'    -radians VALUES     convert radians to degrees                              ',&
'    -degrees VALUES     convert degrees to radians                              ',&
'    --help              display this help and exit                              ',&
'    --version           output version information and exit                     ',&
'                                                                                ',&
'EXAMPLES                                                                        ',&
'                                                                                ',&
'     Sample commands:                                                           ',&
'                                                                                ',&
'      exchange -degrees 0 90 180 360                                            ',&
'                                                                                ',&
'      > 0.00000000      degrees is    0.00000000      radians                   ',&
'      > 90.0000000      degrees is    1.57079637      radians                   ',&
'      > 180.000000      degrees is    3.14159274      radians                   ',&
'      > 360.000000      degrees is    6.28318548      radians                   ',&
'                                                                                ',&
'      exchange -fahrenheit -40 32 98.6 212 -celsius -40 0 37 100                ',&
'                                                                                ',&
'      > -40.0000000      celsius is   -40.0000000      fahrenheit               ',&
'      >  0.00000000      celsius is    32.0000000      fahrenheit               ',&
'      >  37.0000000      celsius is    98.6000061      fahrenheit               ',&
'      >  100.000000      celsius is    212.000000      fahrenheit               ',&
'      > -40.0000000      fahrenheit is   -40.0000000      celsius               ',&
'      >  32.0000000      fahrenheit is    0.00000000      celsius               ',&
'      >  98.5999985      fahrenheit is    37.0000000      celsius               ',&
'      >  212.000000      fahrenheit is    100.000000      celsius               ',&
'                                                                                ',&
'AUTHOR                                                                          ',&
'    John S. Urban                                                               ',&
'LICENSE                                                                         ',&
'    Public Domain                                                               ',&
'']
   WRITE(*,'(a)')(trim(help_text(i)),i=1,size(help_text))
   stop ! if --help was specified, stop
endif
end subroutine help_usage
!>
!!##NAME
!!     exchange(1f) - [CONVERT] test of basic unit conversion functions
!!     (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!
!!   \
!!
!!     exchange [[ -feet VALUES] [ -meters VALUES]
!!              [ -celsius VALUES] [ -fahrenheit VALUES]
!!              [ -degrees VALUES] [ -radians VALUES]]
!!              |[ --help| -version]
!!
!!##DESCRIPTION
!!      Makes common unit conversions such as between feet and meters,
!!      celsius and fahrenheit, and degrees and radians.
!!
!!##OPTIONS
!!     -feet VALUES        convert feet to meters
!!     -meters VALUES      convert meters to feet
!!     -fahrenheit VALUES  convert fahrenheit to celsius
!!     -celsius VALUES     convert celsius to fahrenheit
!!     -radians VALUES     convert radians to degrees
!!     -degrees VALUES     convert degrees to radians
!!     --help              display this help and exit
!!     --version           output version information and exit
!!
!!##EXAMPLES
!!
!!
!!      Sample commands:
!!
!!       exchange -degrees 0 90 180 360
!!
!!       > 0.00000000      degrees is    0.00000000      radians
!!       > 90.0000000      degrees is    1.57079637      radians
!!       > 180.000000      degrees is    3.14159274      radians
!!       > 360.000000      degrees is    6.28318548      radians
!!
!!       exchange -fahrenheit -40 32 98.6 212 -celsius -40 0 37 100
!!
!!       > -40.0000000      celsius is   -40.0000000      fahrenheit
!!       >  0.00000000      celsius is    32.0000000      fahrenheit
!!       >  37.0000000      celsius is    98.6000061      fahrenheit
!!       >  100.000000      celsius is    212.000000      fahrenheit
!!       > -40.0000000      fahrenheit is   -40.0000000      celsius
!!       >  32.0000000      fahrenheit is    0.00000000      celsius
!!       >  98.5999985      fahrenheit is    37.0000000      celsius
!!       >  212.000000      fahrenheit is    100.000000      celsius
!!
!!##AUTHOR
!!     John S. Urban
!!##LICENSE
!!     Public Domain
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
'@(#)PROGRAM:        exchange(1f)>',&
'@(#)DESCRIPTION:    test of basic unit conversion functions>',&
'@(#)VERSION:        1.0, 20160603>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)REPORTING BUGS: http://www.urbanjost.altervista.org/>',&
'@(#)HOME PAGE:      http://www.urbanjost.altervista.org/index.html>',&
'@(#)LICENSE:        Public Domain>',&
'@(#)COMPILED:       2021-07-01 09:04:31 UTC-240>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if --version was specified, stop
endif
end subroutine help_version
program testit
use M_kracken, only : kracken, rgets, lget
use M_units
implicit none
real,allocatable :: values(:)
integer                     :: i
   ! define command line options and parse command line
   call kracken('units',' -feet -meters -celsius -fahrenheit -degrees -radians -help .F. -version .F.')
   ! process -help and -version switches
   call help_usage(lget('units_help'))
   call help_version(lget('units_version'))
   ! convert values to new units

   values=rgets('units_feet'); do i=1,size(values)
      write(*,*) values(i),' feet is ',feet_to_meters(values(i)),' meters'
   enddo
   values=rgets('units_meters'); do i=1,size(values)
      write(*,*) values(i),' meters is ',meters_to_feet(values(i)),' feet'
   enddo

   values=rgets('units_radians'); do i=1,size(values)
      write(*,*) values(i),' radians is ',r2d(values(i)),' degrees'
   enddo
   values=rgets('units_degrees'); do i=1,size(values)
      write(*,*) values(i),' degrees is ',d2r(values(i)),' radians'
   enddo

   values=rgets('units_celsius'); do i=1,size(values)
      write(*,*) values(i),' celsius is ',c2f(values(i)),' fahrenheit'
   enddo
   values=rgets('units_fahrenheit'); do i=1,size(values)
      write(*,*) values(i),' fahrenheit is ',f2c(values(i)),' celsius'
   enddo

end program testit
!***********************************************************************************************************************************
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!***********************************************************************************************************************************
!===============================================================================
! message
! Current state of prep(1):(09:04  1 Jul 2021)
! Total lines read ............... 107
! Conditional nesting level....... 0
! G_WRITE (general processing).... T
! G_LLWRITE (write input lines)... T
! Arguments ...................... TESTPRG90 -D Linux -D Linux_ifort -D ENDCON=ENDCON -D NOUNDERSCORE=NOUNDERSCORE -I /tmp/USH.35191/CCALL_Linux_ifort_113478 -I /home/urbanjs/venus/V600/LIBRARY/libDL/inc -I /home/urbanjs/venus/V600/LIBRARY/libGCS/inc -I /home/urbanjs/venus/V600/LIBRARY/libgdi/inc -I /home/urbanjs/venus/V600/LIBRARY/libmachine/inc -I /home/urbanjs/venus/V600/LIBRARY/libMULTI/inc -I /home/urbanjs/venus/V600/LIBRARY/libncar/inc -I /home/urbanjs/venus/V600/LIBRARY/libnswc/inc -I /home/urbanjs/venus/V600/LIBRARY/libObsolete/inc -I /home/urbanjs/venus/V600/LIBRARY/librandlib/inc -I /home/urbanjs/venus/V600/LIBRARY/libslatec/inc -I /home/urbanjs/venus/V600/LIBRARY/libSTUG/inc -I /home/urbanjs/venus/V600/LIBRARY/libtemplate/inc -I /home/urbanjs/venus/V600/LIBRARY/libvogle/inc -I /home/urbanjs/venus/V600/LIBRARY/libvopl/inc -I /home/urbanjs/venus/V600/LIBRARY/EXE/BLINK/inc -I /home/urbanjs/venus/V600/LIBRARY/EXE/FLECS90/inc -I /home/urbanjs/venus/V600/LIBRARY/FROZEN/libvogle/inc -I /home/urbanjs/venus/V600/LIBRARY/libGPF/draw/inc -I /home/urbanjs/venus/V600/LIBRARY/libGPF/messages/inc -I EXE/EXCHANGE -verbose -system .true. -i EXE/EXCHANGE/exchange.ff -o /tmp/USH.35191/CCALL_Linux_ifort_113478/exchange.113478.f90

! Open files:
!    unit ! line number ! filename
!      50 !         107 ! EXE/EXCHANGE/exchange.ff
! INCLUDE directories:
!    EXE/EXCHANGE
! Variables:
!    $DEFINE UNKNOWN  =  0                              
!    $DEFINE LINUX  =  1                              
!    $DEFINE MACOS  =  2                              
!    $DEFINE WINDOWS  =  3                              
!    $DEFINE CYGWIN  =  4                              
!    $DEFINE SOLARIS  =  5                              
!    $DEFINE FREEBSD  =  6                              
!    $DEFINE OPENBSD  =  7                              
!    $DEFINE OS  =  1                              
!    $DEFINE TESTPRG90  =  1                              
!    $DEFINE NOUNDERSCORE  =  NOUNDERSCORE                   
!-------------------------------------------------------------------------------
