        program demo_abs
        implicit none
        integer,parameter :: dp=kind(0.0d0)

        integer           :: i = -1
        real              :: x = -1.0
        complex           :: z = (-3.0,-4.0)
        doubleprecision   :: rr = -45.78_dp

        character(len=*),parameter :: &
           ! some formats
           frmt  =  '(1x,a15,1x," In: ",g0,            T51," Out: ",g0)', &
           frmtc = '(1x,a15,1x," In: (",g0,",",g0,")",T51," Out: ",g0)',  &
           g     = '(*(g0,1x))'

          ! basic usage
            ! any integer, real, or complex type
            write(*, frmt)  'integer         ',  i, abs(i)
            write(*, frmt)  'real            ',  x, abs(x)
            write(*, frmt)  'doubleprecision ', rr, abs(rr)
            write(*, frmtc) 'complex         ',  z, abs(z)

          ! You can take the absolute value of any value whose positive value
          ! is representable with the same type and kind.
            write(*, *) 'abs range test : ', abs(huge(0)), abs(-huge(0))
            write(*, *) 'abs range test : ', abs(huge(0.0)), abs(-huge(0.0))
            write(*, *) 'abs range test : ', abs(tiny(0.0)), abs(-tiny(0.0))
            ! A dusty corner is that abs(-huge(0)-1) of an integer would be
            ! a representable negative value on most machines but result in a
            ! positive value out of range.

          ! elemental
            write(*, g) ' abs is elemental:', abs([20,  0,  -1,  -3,  100])

          ! COMPLEX input produces REAL output
            write(*, g)' complex input produces real output', &
            & abs(cmplx(30.0_dp,40.0_dp,kind=dp))
            ! dusty corner: "kind=dp" is required or the value returned by
            ! CMPLX() is a default real instead of double precision

          ! the returned value for complex input can be thought of as the
          ! distance from the origin <0,0>
            write(*, g) ' distance of (', z, ') from zero is', abs( z )
            write(*, g) ' so beware of overflow with complex values'
            write(*, g) abs(cmplx( huge(0.0), huge(0.0) ))
            write(*, g) ' because the biggest default real is',huge(0.0)

        end program demo_abs
