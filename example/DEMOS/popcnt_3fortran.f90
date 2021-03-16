          program demo_popcnt
          use, intrinsic :: iso_fortran_env, only : integer_kinds, &
          & int8, int16, int32, int64
          implicit none
            print *, popcnt(127),       poppar(127)
            print *, popcnt(huge(0)), poppar(huge(0))
            print *, popcnt(huge(0_int8)), poppar(huge(0_int8))
            print *, popcnt(huge(0_int16)), poppar(huge(0_int16))
            print *, popcnt(huge(0_int32)), poppar(huge(0_int32))
            print *, popcnt(huge(0_int64)), poppar(huge(0_int64))
          end program demo_popcnt
