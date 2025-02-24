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
'       quadratic(1f) - [MATH] Calculate and print the roots of a quadratic formula even if they are complex',&
'       (LICENSE:PD)                                                             ',&
'                                                                                ',&
'SYNOPSIS                                                                        ',&
'       quadratic A B C [ --verbose]|[ --help| --version]                        ',&
'                                                                                ',&
'DESCRIPTION                                                                     ',&
'   Given the equation                                                           ',&
'                                                                                ',&
'      A*x**2 + B*x + C = 0                                                      ',&
'                                                                                ',&
'   Use the quadratic formula to determine the root values of the equation.      ',&
'                                                                                ',&
'OPTIONS                                                                         ',&
'       A,B,C       coefficients                                                 ',&
'       --verbose   echo input values                                            ',&
'       --help      display help text and exit                                   ',&
'       --version   display version information and exit                         ',&
'                                                                                ',&
'EXAMPLE                                                                         ',&
'  This program uses NAMELIST to crack the command line arguments,               ',&
'  and complex numbers.                                                          ',&
'                                                                                ',&
'  Sample usage:                                                                 ',&
'                                                                                ',&
'   quadratic A=1.0  B=5.0  C=2.0                                                ',&
'    for 1*x**2 + 5*x + 2 = 0                                                    ',&
'    the roots (ie. "x intercepts") are real so the parabola crosses the x-axis at two points:',&
'    z1 =-0.438447237                                                            ',&
'    z2 =-4.561553                                                               ',&
'    discriminant =17                                                            ',&
'                                                                                ',&
'   quadratic A=1.0  B=2.0  C=5.0 # There are no real roots (Discriminant = -16)!',&
'    for 1*x**2 + 2*x + 5 = 0                                                    ',&
'    the roots(ie. "x intercepts")  are complex:                                 ',&
'    z1 = ( -1.00000000    ,  2.00000000    )                                    ',&
'    z2 = ( -1.00000000    , -2.00000000    )                                    ',&
'    discriminant =-16                                                           ',&
'                                                                                ',&
'   quadratic A=9 B=12 C=4                                                       ',&
'    for 9*x**2 + 12*x + 4 = 0                                                   ',&
'    the roots (ie. "x intercepts") are repeated (real and equal) so the parabola just touches the x-axis at:',&
'    z1 = z2 =-0.666666687                                                       ',&
'    discriminant =0                                                             ',&
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
!!        quadratic(1f) - [MATH] Calculate and print the roots of a quadratic formula even if they are complex
!!        (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!        quadratic A B C [ --verbose]|[ --help| --version]
!!
!!##DESCRIPTION
!!    Given the equation
!!
!!       A*x**2 + B*x + C = 0
!!
!!    Use the quadratic formula to determine the root values of the equation.
!!
!!##OPTIONS
!!        A,B,C       coefficients
!!        --verbose   echo input values
!!        --help      display help text and exit
!!        --version   display version information and exit
!!
!!##EXAMPLE
!!
!!   This program uses NAMELIST to crack the command line arguments,
!!   and complex numbers.
!!
!!   Sample usage:
!!
!!    quadratic A=1.0  B=5.0  C=2.0
!!     for 1*x**2 + 5*x + 2 = 0
!!     the roots (ie. "x intercepts") are real so the parabola crosses the x-axis at two points:
!!     z1 =-0.438447237
!!     z2 =-4.561553
!!     discriminant =17
!!
!!    quadratic A=1.0  B=2.0  C=5.0 # There are no real roots (Discriminant = -16)!
!!     for 1*x**2 + 2*x + 5 = 0
!!     the roots(ie. "x intercepts")  are complex:
!!     z1 = ( -1.00000000    ,  2.00000000    )
!!     z2 = ( -1.00000000    , -2.00000000    )
!!     discriminant =-16
!!
!!    quadratic A=9 B=12 C=4
!!     for 9*x**2 + 12*x + 4 = 0
!!     the roots (ie. "x intercepts") are repeated (real and equal) so the parabola just touches the x-axis at:
!!     z1 = z2 =-0.666666687
!!     discriminant =0
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
'@(#)PROGRAM:        quadratic(1f)>',&
'@(#)DESCRIPTION:    Calculate and print the roots of a quadratic formula even if they are complex>',&
'@(#)VERSION:        2.0, 20180825>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)COMPILED:       2025-02-23 19:26:16 UTC-300>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if --version was specified, stop
endif
end subroutine help_version
program demo_quadratic
use M_kracken,  only : kracken, lget, sget
use M_math,     only : quadratic
use M_strings,  only : v2s
implicit none
! ident_1="@(#) Calculate and print the roots of a quadratic formula even if they are complex"
real               :: A=0.0, B=0.0, C=0.0
character(len=255) :: coeff_string
complex            :: z1, z2  ! roots
real               :: discriminant
logical            :: verbose
namelist /coeff/ A, B, C

   call kracken('quadratic',' --help .false. --version .false. -verbose .false.')
   call help_usage(lget('quadratic_help'))          ! check for help text
   call help_version(lget('quadratic_version'))     ! check for version text
   verbose=lget('quadratic_verbose')

   ! get list of arguments in a format readable by NAMELIST
   coeff_string='&coeff '//trim(sget('quadratic_oo'))//' /'
   read(coeff_string,NML=coeff)

   call quadratic(a,b,c,z1,z2,discriminant) !  Calculate the roots

   if(verbose)then
      print *, "a ............... ", a
      print *, "b ............... ", b
      print *, "c ............... ", c
      print *, "z1 .............. ", z1
      print *, "z2 .............. ", z2
      print *, "discriminant .... ", discriminant
      print *
   endif

!  Print the roots

   write(*,*)'for ',v2s(a),'*x**2 + ',v2s(b),'*x + ',v2s(c),' = 0'
   if (discriminant == 0) then
      write(*,*) 'the roots (ie. "x intercepts") are repeated (real and equal) so the parabola just touches the x-axis at:'
      print *, "z1 = z2 =", v2s(real(z1))
   else if (discriminant > 0) then
      write(*,*) 'the roots (ie. "x intercepts") are real so the parabola crosses the x-axis at two points:'
      print *, "z1 =", v2s(real(z1))
      print *, "z2 =", v2s(real(z2))
   else
      write(*,*) 'the roots(ie. "x intercepts")  are complex:'
      print *, "z1 =", z1
      print *, "z2 =", z2
   endif

   print *, "discriminant =", v2s(discriminant)

end program demo_quadratic
