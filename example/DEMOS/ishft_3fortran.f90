      program demo_ishft
      use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64
      implicit none
      integer             :: shift
      character(len=*),parameter :: g='(b32.32,1x,i0)'

         write(*,*) ishft(3, 1),' <== typically should have the value 6'

         shift=4
         write(*,g) ishft(huge(0),shift), shift
         shift=0
         write(*,g) ishft(huge(0),shift), shift
         shift=-4
         write(*,g) ishft(huge(0),shift), shift
      end program demo_ishft
