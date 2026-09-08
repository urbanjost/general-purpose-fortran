      program demo_poppar
      use, intrinsic :: iso_fortran_env, only : integer_kinds, &
         & int8, int16, int32, int64
      implicit none
      character(len=*),parameter :: pretty='(b64,1x,i0)'
         ! basic usage
         print pretty, 127,     poppar(127)
         print pretty, 128,     poppar(128)
         print pretty, int(b"01010"), poppar(int(b"01010"))

         ! any kind of an integer can be used
         print pretty, huge(0_int8),  poppar(huge(0_int8))
         print pretty, huge(0_int16), poppar(huge(0_int16))
         print pretty, huge(0_int32), poppar(huge(0_int32))
         print pretty, huge(0_int64), poppar(huge(0_int64))
      end program demo_poppar
