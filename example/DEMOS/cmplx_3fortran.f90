          program demo_aimag
          implicit none
          integer,parameter :: dp=kind(0.0d0)
          complex          :: z4
          complex(kind=dp) :: z8
             z4 = cmplx(1.23456789, 1.23456789)
             print *, 'Z4=',z4
             ! using kind=dp makes it keep DOUBLEPRECISION precision
             z8 = cmplx(1.2345678901234567d0, 1.2345678901234567d0,kind=dp)
             print *, 'Z8=',z8
             ! NOTE:
             ! The following is intuitive and works without calling cmplx(3f)
             ! but does not work for variables just constants
             z8 = (1.2345678901234567d0 , 1.2345678901234567d0 )
             print *, 'Z8 defined with constants=',z8
          end program demo_aimag
