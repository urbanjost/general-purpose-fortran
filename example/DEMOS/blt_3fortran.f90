      program demo_blt
      use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64
      implicit none
      integer            :: i
      integer(kind=int8) :: byte
        ! Compare some one-byte values to 64.
         ! Notice that the values are tested as bits not as integers
         ! so sign bits in the integer are treated just like any other
         do i=-128,127,32
            byte=i
            write(*,'(sp,i0.4,*(1x,1l,1x,b0.8))')i,blt(byte,64_int8),byte
         enddo
        ! BOZ literals
         write(*,*)blt(z'1000', z'101011010')
         ! see the BGE() description for an extended description
         ! of related information

      end program demo_blt
