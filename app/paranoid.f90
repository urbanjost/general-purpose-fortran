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
'    paranoid(1f) - [DEVELOPER] call doubleprecision and real versions of paranoia(3f)                                           ',&
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
'EXAMPLE                                                                                                                         ',&
'                                                                                                                                ',&
'                                                                                                                                ',&
' Sample beginning of dialog ...                                                                                                 ',&
'                                                                                                                                ',&
'   ============================================================                                                                 ',&
'   Tuesday, February 7th, 2017 4:35:06 AM UTC-0300                                                                              ',&
'    sysname:   CYGWIN_NT-10.0                                                                                                   ',&
'    release:  2.6.0(0.304/5/3)                                                                                                  ',&
'    version:  2016-08-31 14:32                                                                                                  ',&
'    nodename: buzz                                                                                                              ',&
'    machine:  x86_64                                                                                                            ',&
'   This file was compiled by GCC version 5.4.0                                                                                  ',&
'   using the options                                                                                                            ',&
'        -I /usr/include/w32api                                                                                                  ',&
'        -I /home/urbanjs/V600/lib/CYGWIN64_GFORTRAN                                                                             ',&
'        -mtune=generic                                                                                                          ',&
'        -march=x86-64                                                                                                           ',&
'        -g                                                                                                                      ',&
'        -Wunused                                                                                                                ',&
'        -Wuninitialized                                                                                                         ',&
'        -Wall                                                                                                                   ',&
'        -std=f2008                                                                                                              ',&
'        -fbounds-check                                                                                                          ',&
'        -fbacktrace                                                                                                             ',&
'        -finit-real=nan                                                                                                         ',&
'        -fno-range-check                                                                                                        ',&
'        -frecord-marker=4                                                                                                       ',&
'        -J /home/urbanjs/V600/lib/CYGWIN64_GFORTRAN                                                                             ',&
'   ============================================================                                                                 ',&
'   *paranoid*" double precision test(3f)                                                                                        ',&
'    Is this a program restart after failure (1)                                                                                 ',&
'    or a start from scratch (0) ?                                                                                               ',&
'                                                                                                                                ',&
'AUTHOR                                                                                                                          ',&
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
!!     paranoid(1f) - [DEVELOPER] call doubleprecision and real versions of paranoia(3f)
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
!!##EXAMPLE
!!
!!
!!
!!  Sample beginning of dialog ...
!!
!!    ============================================================
!!    Tuesday, February 7th, 2017 4:35:06 AM UTC-0300
!!     sysname:   CYGWIN_NT-10.0
!!     release:  2.6.0(0.304/5/3)
!!     version:  2016-08-31 14:32
!!     nodename: buzz
!!     machine:  x86_64
!!    This file was compiled by GCC version 5.4.0
!!    using the options
!!         -I /usr/include/w32api
!!         -I /home/urbanjs/V600/lib/CYGWIN64_GFORTRAN
!!         -mtune=generic
!!         -march=x86-64
!!         -g
!!         -Wunused
!!         -Wuninitialized
!!         -Wall
!!         -std=f2008
!!         -fbounds-check
!!         -fbacktrace
!!         -finit-real=nan
!!         -fno-range-check
!!         -frecord-marker=4
!!         -J /home/urbanjs/V600/lib/CYGWIN64_GFORTRAN
!!    ============================================================
!!    *paranoid*" double precision test(3f)
!!     Is this a program restart after failure (1)
!!     or a start from scratch (0) ?
!!
!!##AUTHOR
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
'@(#)COMPILED:       2022-01-09 10:17:49 UTC-300>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if --version was specified, stop
endif
end subroutine help_version
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
program test_paranoia
use M_kracken, only  : kracken,lget
use M_strings, only  : substitute
use M_time, only     : now
use M_system, only   : system_uname
use M_messages, only : signs
implicit none

character(len=*),parameter   :: ident="@(#)paranoid(1f): call doubleprecision and real versions of paranoia(3f)"

character(len=:),allocatable :: options
integer,parameter            :: is=100
character(len=is)            :: string=' '
!-----------------------------------------------------------------------------------------------------------------------------------
   call kracken('paranoid','-help .F. -version .F.')     ! define command arguments,default values and crack command line
   call help_usage(lget('paranoid_help'))                ! if -help option is present, display help text and exit
   call help_version(lget('paranoid_version'))           ! if -version option is present, display version text and exit
!-----------------------------------------------------------------------------------------------------------------------------------
                                                         ! options can be very long. Take a guess on how it can be pretty-printed
   options='UNKNOWN'

   print '(a)', repeat('=',80)                           ! print break line
   print '(a)', now()                                    ! print date and time

   call system_uname('s',string)                         ! print system information
   write(*,*)'sysname:  '//trim(string)

   call system_uname('r',string)
   write(*,*)'release:  '//trim(string)

   call system_uname('v',string)
   write(*,*)'version:  '//trim(string)

   call system_uname('n',string)
   write(*,*)'nodename: '//trim(string)

   call system_uname('m',string)
   write(*,*)'machine:  '//trim(string)


   print '(a)', repeat('=',80)

   call signs('DOUBLE',6)
   call paranoia('double')                               ! start tests
   call signs('SINGLE',6)
   call paranoia('single')

end program test_paranoia
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
