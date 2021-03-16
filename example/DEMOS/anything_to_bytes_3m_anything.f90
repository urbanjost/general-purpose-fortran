          program demo_anything_to_bytes
          use M_anything,      only : anything_to_bytes
          !!use, intrinsic :: iso_fortran_env, only : int8, int16, int32, int64
          !!use, intrinsic :: iso_fortran_env, only : real32, real64, real128
          implicit none
          integer :: i
             write(*,'(/,4(1x,z2.2))')anything_to_bytes([(i*i,i=1,10)])
             write(*,'(/,4(1x,z2.2))')anything_to_bytes([11.11,22.22,33.33])
             write(*,'(/,4(1x,z2.2))')anything_to_bytes('This is a string')
          end program demo_anything_to_bytes
