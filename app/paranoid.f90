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
'    paranoid(1f) - [DEVELOPER] call doubleprecision and real versions                                                           ',&
'    of paranoia(3f)                                                                                                             ',&
'    (LICENSE:PD)                                                                                                                ',&
'SYNOPSIS                                                                                                                        ',&
'    paranoid                                                                                                                    ',&
'DESCRIPTION                                                                                                                     ',&
'   This program and the routines it calls can be used to test various                                                           ',&
'   Fortran compiler options.                                                                                                    ',&
'                                                                                                                                ',&
'   The paranoid(1f) command is strictly for use by developers. This                                                             ',&
'   program and the sparanoi(3f) and dparanoi(3f) procedures all need                                                            ',&
'   recompiled with the compiler options being tested. Then the program                                                          ',&
'   is run and the resulting tests and their output are examined.                                                                ',&
'                                                                                                                                ',&
'   The results require interpretation and an understanding of program                                                           ',&
'   internals.                                                                                                                   ',&
'                                                                                                                                ',&
'   Because programs are often built with a variety of compilers and                                                             ',&
'   compiler options on a number of different platforms it is prudent to                                                         ',&
'   select options that choose operations that meet the double precision                                                         ',&
'   specification defined in the IEEE 754-1985 standard when available;                                                          ',&
'   but "failure" of the strict testing performed does not imply a flaw                                                          ',&
'   in the program.                                                                                                              ',&
'                                                                                                                                ',&
'   This permits developers to verify that the compiler and loader options                                                       ',&
'   selected while building a program and the system hardware currently                                                          ',&
'   being used reasonably perform floating point operations.                                                                     ',&
'                                                                                                                                ',&
'LICENSE                                                                                                                         ',&
'   Public Domain                                                                                                                ',&
'']
   WRITE(*,'(a)')(trim(help_text(i)),i=1,size(help_text))
   stop ! if --help was specified, stop
endif
end subroutine help_usage
!>
!!##NAME
!!     paranoid(1f) - [DEVELOPER] call doubleprecision and real versions
!!     of paranoia(3f)
!!     (LICENSE:PD)
!!##SYNOPSIS
!!
!!     paranoid
!!##DESCRIPTION
!!    This program and the routines it calls can be used to test various
!!    Fortran compiler options.
!!
!!    The paranoid(1f) command is strictly for use by developers. This
!!    program and the sparanoi(3f) and dparanoi(3f) procedures all need
!!    recompiled with the compiler options being tested. Then the program
!!    is run and the resulting tests and their output are examined.
!!
!!    The results require interpretation and an understanding of program
!!    internals.
!!
!!    Because programs are often built with a variety of compilers and
!!    compiler options on a number of different platforms it is prudent to
!!    select options that choose operations that meet the double precision
!!    specification defined in the IEEE 754-1985 standard when available;
!!    but "failure" of the strict testing performed does not imply a flaw
!!    in the program.
!!
!!    This permits developers to verify that the compiler and loader options
!!    selected while building a program and the system hardware currently
!!    being used reasonably perform floating point operations.
!!
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
'@(#)PROGRAM:        paranoid(1)>',&
'@(#)DESCRIPTION:    call doubleprecision and real versions of paranoia(3f)>',&
'@(#)VERSION:        1.0, 20150508>',&
'@(#)COMPILED:       2022-05-01 09:49:45 UTC-240>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if --version was specified, stop
endif
end subroutine help_version
!===============================================================================
! *prep* ERROR(008) - NOT LOGICAL OR INTEGER EXPRESSION:HAVE_ISO_FORTRAN_ENV=FALSE
! Current state of prep(1):(09:49  1 May 2022)
! Total lines read ............... 51
! Conditional nesting level....... 2
! G_WRITE (general processing).... T
! G_LLWRITE (write input lines)... T
! Arguments ...................... TESTPRG90 -D Linux -D Linux_gfortran -D ENDCON=1 -I /tmp/USH.1425621/CCALL_Linux_gfortran_1437025 -I /home/urbanjs/venus/V600/LIBRARY/libDL/inc -I /home/urbanjs/venus/V600/LIBRARY/libGCS/inc -I /home/urbanjs/venus/V600/LIBRARY/libgdi/inc -I /home/urbanjs/venus/V600/LIBRARY/libgks2/inc -I /home/urbanjs/venus/V600/LIBRARY/libmachine/inc -I /home/urbanjs/venus/V600/LIBRARY/libMULTI/inc -I /home/urbanjs/venus/V600/LIBRARY/libncar/inc -I /home/urbanjs/venus/V600/LIBRARY/libnswc/inc -I /home/urbanjs/venus/V600/LIBRARY/libObsolete/inc -I /home/urbanjs/venus/V600/LIBRARY/librandlib/inc -I /home/urbanjs/venus/V600/LIBRARY/libSTUG/inc -I /home/urbanjs/venus/V600/LIBRARY/libtemplate/inc -I /home/urbanjs/venus/V600/LIBRARY/libvg320/inc -I /home/urbanjs/venus/V600/LIBRARY/libvogle/inc -I /home/urbanjs/venus/V600/LIBRARY/libvopl/inc -I /home/urbanjs/venus/V600/LIBRARY/EXE/BLINK/inc -I /home/urbanjs/venus/V600/LIBRARY/EXE/FLECS90/inc -I /home/urbanjs/venus/V600/LIBRARY/FROZEN/libvogle/inc -I /home/urbanjs/venus/V600/github/programs/app/source -verbose -system .true. -i /home/urbanjs/venus/V600/github/programs/app/source/paranoid.FF -o /tmp/USH.1425621/CCALL_Linux_gfortran_1437025/paranoid.1437025.f90 
! Open files:
!    unit ! line number ! filename
!      50 !          51 ! /home/urbanjs/venus/V600/github/programs/app/source/paranoid.FF
! INCLUDE directories:
!    /home/urbanjs/venus/V600/github/programs/app/source
! Variables:
!    $DEFINE WINDOWS  =  3
!    $DEFINE UNKNOWN  =  0
!    $DEFINE TRUE  =  1
!    $DEFINE TESTPRG90  =  1
!    $DEFINE SYSTEMON  =  .TRUE.
!    $DEFINE SOLARIS  =  5
!    $DEFINE OS  =  1
!    $DEFINE OPENBSD  =  7
!    $DEFINE MACOS  =  2
!    $DEFINE LINUX  =  1
!    $DEFINE FREEBSD  =  6
!    $DEFINE FALSE  =  0
!    $DEFINE ENDCON  =  1
!    $DEFINE CYGWIN  =  4
! Parcels:
!-------------------------------------------------------------------------------
