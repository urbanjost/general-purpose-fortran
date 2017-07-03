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
'   pendulum(1f) - [NUMBERS]calculate pendulum period                            ',&
'                                                                                ',&
'SYNOPSIS                                                                        ',&
'   pendulum [-length Length_In_Feet ][-verbose]|[--help]|[--version]            ',&
'                                                                                ',&
'DESCRIPTION                                                                     ',&
'   Given the length to the center of gravity from the fulcrum (pivot            ',&
'   point) of a pendulum in feet calculate the pendulum frequency in             ',&
'   swings/sec, and how long the swing takes (the period of the pendulum).       ',&
'                                                                                ',&
'   If the pendulum weight or bob of a simple pendulum is pulled to a            ',&
'   relatively small angle and let go, it will swing back and forth at a         ',&
'   regular frequency. If damping effects from air resistance and friction       ',&
'   are negligible, equations concerning the frequency and period of the         ',&
'   the pendulum, as well as the length of the string can be calculated.         ',&
'                                                                                ',&
'   The period of the motion for a pendulum is how long it takes to swing        ',&
'   back-and-forth, measured in seconds.  The period equation is:                ',&
'                                                                                ',&
'      T = 2*PI*sqrt(L/g)                                                        ',&
'                                                                                ',&
'   The frequency of a pendulum is how many back-and-forth swings there          ',&
'   are in a second, measured in hertz.  Frequency f is the reciprocal           ',&
'   of the period T:                                                             ',&
'                                                                                ',&
'      f = 1/T                                                                   ',&
'   Therefore in terms of the length the frequency is                            ',&
'                                                                                ',&
'      f = [√sqrt(g/L)]/2π                                                    ',&
'                                                                                ',&
'   The length equations are:                                                    ',&
'                                                                                ',&
'      L = g/(4*PI**2*f**2)                                                      ',&
'   and                                                                          ',&
'                                                                                ',&
'      L = (g*T**2)/(4*PI**2)                                                    ',&
'                                                                                ',&
'   The generally accepted length of a seconds pendulum at sea level,            ',&
'   lat. 45 degrees, is 99.356 cm or 39.116 in. = 3.25966 feet.                  ',&
'                                                                                ',&
'OPTIONS                                                                         ',&
'   -length Length_In_Feet  distance from pivot point to center                  ',&
'                           of gravity of pendulum.                              ',&
'   -verbose [T|F]          verbose mode, default is .true. .                    ',&
'   --help                  display this help and exit                           ',&
'   --version               output version information and exit                  ',&
'                                                                                ',&
'EXAMPLES                                                                        ',&
'  Sample command lines ...                                                      ',&
'                                                                                ',&
'   # A 1/4 of a meter pendulum has a period of about 1 second.                  ',&
'   pendulum -length 0.820209980                                                 ',&
'   For a pendulum with length  0.820209980      feet                            ',&
'   The frequency of the pendulum is  0.996806502     swings/sec.                ',&
'   Each swing takes   1.00320375     sec.                                       ',&
'                                                                                ',&
'   pendulum -length 3.00                                                        ',&
'   For a pendulum with length   3.00000000      feet                            ',&
'   The frequency of the pendulum is  0.287753224     swings/sec.                ',&
'   Each swing takes   3.47519994     sec.                                       ',&
'                                                                                ',&
'']
   WRITE(*,'(a)')(trim(help_text(i)),i=1,size(help_text))
   stop ! if -help was specified, stop
endif
end subroutine help_usage
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!    pendulum(1f) - [NUMBERS]calculate pendulum period
!!
!!##SYNOPSIS
!!
!!    pendulum [-length Length_In_Feet ][-verbose]|[--help]|[--version]
!!
!!##DESCRIPTION
!!    Given the length to the center of gravity from the fulcrum (pivot
!!    point) of a pendulum in feet calculate the pendulum frequency in
!!    swings/sec, and how long the swing takes (the period of the pendulum).
!!
!!    If the pendulum weight or bob of a simple pendulum is pulled to a
!!    relatively small angle and let go, it will swing back and forth at a
!!    regular frequency. If damping effects from air resistance and friction
!!    are negligible, equations concerning the frequency and period of the
!!    the pendulum, as well as the length of the string can be calculated.
!!
!!    The period of the motion for a pendulum is how long it takes to swing
!!    back-and-forth, measured in seconds.  The period equation is:
!!
!!       T = 2*PI*sqrt(L/g)
!!
!!    The frequency of a pendulum is how many back-and-forth swings there
!!    are in a second, measured in hertz.  Frequency f is the reciprocal
!!    of the period T:
!!
!!       f = 1/T
!!    Therefore in terms of the length the frequency is
!!
!!       f = [√sqrt(g/L)]/2π
!!
!!    The length equations are:
!!
!!       L = g/(4*PI**2*f**2)
!!    and
!!
!!       L = (g*T**2)/(4*PI**2)
!!
!!    The generally accepted length of a seconds pendulum at sea level,
!!    lat. 45 degrees, is 99.356 cm or 39.116 in. = 3.25966 feet.
!!
!!##OPTIONS
!!    -length Length_In_Feet  distance from pivot point to center
!!                            of gravity of pendulum.
!!    -verbose [T|F]          verbose mode, default is .true. .
!!    --help                  display this help and exit
!!    --version               output version information and exit
!!
!!##EXAMPLES
!!
!!   Sample command lines ...
!!
!!    # A 1/4 of a meter pendulum has a period of about 1 second.
!!    pendulum -length 0.820209980
!!    For a pendulum with length  0.820209980      feet
!!    The frequency of the pendulum is  0.996806502     swings/sec.
!!    Each swing takes   1.00320375     sec.
!!
!!    pendulum -length 3.00
!!    For a pendulum with length   3.00000000      feet
!!    The frequency of the pendulum is  0.287753224     swings/sec.
!!    Each swing takes   3.47519994     sec.
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
'@(#)PROGRAM:        pendulum(1f)>',&
'@(#)DESCRIPTION:    calculate simple pendulum period>',&
'@(#)VERSION:        1.0, 2015-12-20>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)REPORTING BUGS: http://www.urbanjost.altervista.org/>',&
'@(#)HOME PAGE:      http://www.urbanjost.altervista.org/index.html>',&
'@(#)LICENSE:        Public Domain. This is free software: you are free to change and redistribute it.>',&
'@(#)                There is NO WARRANTY, to the extent permitted by law.>',&
'@(#)COMPILED:       Wed, Jun 14th, 2017 10:05:40 AM>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if -version was specified, stop
endif
end subroutine help_version
!-----------------------------------------------------------------------------------------------------------------------------------
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
module M_geometry
private
public pendulum_period
public pendulum_time
contains
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine pendulum_period(L,f,T,VERBOSE)
implicit none
character(len=*),parameter :: ident='@(#)pendulum(3f): Calculates the frequency and period of a pendulum of length L'
real,intent(in)             :: L
real,intent(out)            :: f, T
logical,optional,intent(in) :: verbose
   real                     :: L_meters
   real, parameter          :: pi = 3.14159, g = 9.80665 ! m/sec**2

!  The period equation is:    T = 2*PI*sqrt(L/g)

   if(L.ne.0)then
      L_meters=L*12*2.54/100.0 ! 1 meter is 3.28083989501312335958 ft
      write(*,*)'L_meters=',L_meters
      T= 2.0 * pi * sqrt (L_meters/g)
      f = 1.0/T
   else
      write(*,*)'*PENDULUM* ERROR: LENGTH OF 0'
      f=huge(0.0)
      T=tiny(0.0)
   endif

   if(present(verbose))then
      print *, "For a pendulum with length", L, " feet"
      print *, "The frequency of the pendulum is", f, "swings/sec."
      print *, "Each swing takes", T, "sec."
   endif

end subroutine pendulum_period
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine pendulum_time(L,f,T,VERBOSE)
implicit none
character(len=*),parameter :: ident='@(#)pendulum(3f): Calculates the frequency and length given the period of a pendulum'
real,intent(in)             :: T
real,intent(out)            :: f, L
logical,optional,intent(in) :: verbose
real, parameter :: pi = 3.14159, g = 9.80665

   L=g*(T/2/PI)**2
   L=L/2.54*12
   f = (1.0 / (2.0 * pi)) * sqrt (g / L)

   if(present(verbose))then
      print *, "For a pendulum with length", L, " feet"
      print *, "The frequency of the pendulum is", f, "swings/sec."
      print *, "Each swing takes", T, "sec."
   endif

end subroutine pendulum_time
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
end module M_geometry
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
program pendulum_test
use M_kracken, only  : kracken, rget, lget, sget
use M_geometry, only : pendulum_period
real :: L, f, T
   call kracken('pendulum','-length 1.0 -verbose T -help F -version F')
   call help_usage(lget('pendulum_help'))
   call help_version(lget('pendulum_version'))

   if(sget('pendulum_oo').ne.'')then
      write(*,*)'*pendulum* syntax error'
   endif

   if(rget('pendulum_length').eq.0)then
      write(*,*)' Run pendulum tests'
      L=1.0
      call pendulum_period(L,f,T)
      call pendulum_period(1.0,f,T,verbose=.true.)
   else
      call pendulum_period(rget('pendulum_length'),f,T,lget('pendulum_verbose'))
   endif

   if (.not.lget('pendulum_verbose'))then
      write(*,*)f
   endif

end program pendulum_test
