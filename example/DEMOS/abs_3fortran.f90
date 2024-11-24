      program demo_abs
         implicit none
         integer,parameter :: dp=kind(0.0d0)

      ! some values to use with ABS(3)
         integer           :: i = -1
         real              :: x = -1.0
         complex           :: z = (-3.0,-4.0)
         doubleprecision   :: rr = -45.78_dp

      ! some formats for pretty-printing some information
         character(len=*),parameter :: &
            frmt  =  '(1x,a15,1x," In: ",g0,            T51," Out: ",g0)', &
            frmtc = '(1x,a15,1x," In: (",g0,",",g0,")",T51," Out: ",g0)',  &
            gen   = '(*(g0,1x))'

         ! the basics
         print gen,  'basic usage:'
         ! any integer, real, or complex type
         write(*, frmt)  'integer         ',  i, abs(i)
         write(*, frmt)  'real            ',  x, abs(x)
         write(*, frmt)  'doubleprecision ', rr, abs(rr)
         write(*, frmtc) 'complex         ',  z, abs(z)

         ! elemental
         print gen, 'abs is elemental:', abs([20,  0,  -1,  -3,  100])

         ! the returned value for complex input can be thought of as the
         ! distance from the origin <0,0>
         print gen, 'distance of (', z, ') from zero is', abs( z )

         call DUSTY_CORNERS_1("beware of abs(-huge(0)-1)")
         call DUSTY_CORNERS_2("beware of losing precision using CMPLX(3)")
         call DUSTY_CORNERS_3("beware of overflow of complex values")
         call DUSTY_CORNERS_4("custom meaning for absolute value of COMPLEX")

      contains

         subroutine DUSTY_CORNERS_1(message)
            character(len=*),intent(in) :: message

            ! A dusty corner is that abs(-huge(0)-1) of an integer would be
            ! a representable negative value on most machines but result in a
            ! positive value out of range.

            print gen,  message
            ! By definition:
            !   You can take the absolute value of any value whose POSITIVE value
            !   is representable with the same type and kind.

            print gen, 'abs range test : ', abs(huge(0)), abs(-huge(0))
            print gen, 'abs range test : ', abs(huge(0.0)), abs(-huge(0.0))
            print gen, 'abs range test : ', abs(tiny(0.0)), abs(-tiny(0.0))

         end subroutine DUSTY_CORNERS_1

         subroutine DUSTY_CORNERS_2(message)
            character(len=*),intent(in) :: message

            ! dusty corner: "kind=dp" is required or the value returned by
            ! CMPLX() is a default real instead of double precision.

            ! Working with complex values you often encounter the CMPLX(3)
            ! function. CMPLX(3) defaults to returning a default REAL regardless
            ! of input type. Not really a direct problem with ABS(2f) per-se,
            ! but a common error when working with doubleprecision complex values

            print gen,  message
            print gen, 'real result versus doubleprecision result', &
            & abs(cmplx(30.0_dp,40.0_dp)), &
            & abs(cmplx(30.0_dp,40.0_dp,kind=dp))

         end subroutine DUSTY_CORNERS_2

         subroutine DUSTY_CORNERS_3(message)
            character(len=*),intent(in) :: message
            print gen, message

            ! this will probably cause an overflow error, or
            !print gen,  abs(cmplx( huge(0.0), huge(0.0) ))

            print gen, 'because the biggest default real is',huge(0.0)
            print gen, 'because returning magnitude of sqrt(x%re**2,x%im**2)'

         end subroutine DUSTY_CORNERS_3

         subroutine DUSTY_CORNERS_4(message)
            character(len=*),intent(in) :: message
            print gen, message

            ! if you do not want the distance for a complex value you
            ! might want something like returning a complex value with
            ! both the imaginary and real parts. One way to do that is

            print gen, cmplx(abs(z%re),abs(z%im),kind=kind(z))

         end subroutine DUSTY_CORNERS_4

      end program demo_abs
