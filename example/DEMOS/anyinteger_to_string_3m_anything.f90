          program demo_anyinteger_to_string
          use, intrinsic :: iso_fortran_env, only : int8, int16, int32, int64
          use M_anything, only : itoc=>anyinteger_to_string
          implicit none
             write(*,*)itoc(huge(0_int8)),       '=> 127'
             write(*,*)itoc(huge(0_int16)),      '=> 32767'
             write(*,*)itoc(huge(0_int32)),      '=> 2147483647'
             write(*,*)itoc(huge(0_int64)),      '=> 9223372036854775807',huge(0_int64)
             write(*,*)itoc(-(huge(0_int64)-1)), '=> -9223372036854775806'
          end program demo_anyinteger_to_string
