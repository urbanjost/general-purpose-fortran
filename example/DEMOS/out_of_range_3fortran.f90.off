      program demo_out_of_range
      use, intrinsic :: iso_fortran_env, only : int8, int16, int32, int64
      use, intrinsic :: iso_fortran_env, only : real32, real64, real128
      implicit none
      integer            :: i
      integer(kind=int8) :: i8, j8

          ! compilers are not required to produce an error on out of range.
          ! here storing the default integers into 1-byte integers
          ! incorrectly can have unexpected results
          do i=127,130
             i8=i
             j8=-i
             ! OUT_OF_RANGE(3f) can let you check if the value will fit
             write(*,*)i8,j8,' might have expected',i,-i, &
              & out_of_range( i,i8), &
              & out_of_range(-i,i8)
          enddo
          write(*,*) 'RANGE IS ',-1-huge(0_int8),'TO',huge(0_int8)
          ! the real -128.5 is truncated to -128 and is in range
          write(*,*) out_of_range (  -128.5, 0_int8)         ! false

          ! the real -128.5 is rounded to -129 and is not in range
          write(*,*) out_of_range (  -128.5, 0_int8, .true.) ! true

      end program demo_out_of_range
