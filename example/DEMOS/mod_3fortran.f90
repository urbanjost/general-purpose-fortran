          program demo_mod
          implicit none
            print *, mod(17,3)           ! yields 2
            print *, mod(17.5,5.5)       ! yields 1.0
            print *, mod(17.5d0,5.5d0)   ! yields 1.0d0
            print *, mod(17.5d0,5.5d0)   ! yields 1.0d0

            print *, mod(-17,3)          ! yields -2
            print *, mod(-17.5,5.5)      ! yields -1.0
            print *, mod(-17.5d0,5.5d0)  ! yields -1.0d0
            print *, mod(-17.5d0,5.5d0)  ! yields -1.0d0

            print *, mod(17,-3)          ! yields 2
            print *, mod(17.5,-5.5)      ! yields 1.0
            print *, mod(17.5d0,-5.5d0)  ! yields 1.0d0
            print *, mod(17.5d0,-5.5d0)  ! yields 1.0d0
          end program demo_mod
