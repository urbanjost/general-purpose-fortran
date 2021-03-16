          program demo_modulo
          implicit none
            print *, modulo(17,3)        ! yields 2
            print *, modulo(17.5,5.5)    ! yields 1.0

            print *, modulo(-17,3)       ! yields 1
            print *, modulo(-17.5,5.5)   ! yields 4.5

            print *, modulo(17,-3)       ! yields -1
            print *, modulo(17.5,-5.5)   ! yields -4.5
          end program demo_modulo
