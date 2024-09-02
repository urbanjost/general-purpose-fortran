      program demo_mod
      implicit none

         ! basics
          print *, mod( -17,  3 ), modulo( -17,  3 )
          print *, mod(  17, -3 ), modulo(  17, -3 )
          print *, mod(  17,  3 ), modulo(  17,  3 )
          print *, mod( -17, -3 ), modulo( -17, -3 )

          print *, mod(-17.5, 5.2), modulo(-17.5, 5.2)
          print *, mod( 17.5,-5.2), modulo( 17.5,-5.2)
          print *, mod( 17.5, 5.2), modulo( 17.5, 5.2)
          print *, mod(-17.5,-5.2), modulo(-17.5,-5.2)

        ! with a divisor of 1 the fractional part is returned
          print *, mod(-17.5, 1.0), modulo(-17.5, 1.0)
          print *, mod( 17.5,-1.0), modulo( 17.5,-1.0)
          print *, mod( 17.5, 1.0), modulo( 17.5, 1.0)
          print *, mod(-17.5,-1.0), modulo(-17.5,-1.0)

      end program demo_mod
