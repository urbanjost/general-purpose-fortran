      program demo_maxexponent
      use, intrinsic :: iso_fortran_env, only : real32,real64,real128
      implicit none
      character(len=*),parameter :: g='(*(g0,1x))'
         print  g,  minexponent(0.0_real32),   maxexponent(0.0_real32)
         print  g,  minexponent(0.0_real64),   maxexponent(0.0_real64)
         print  g,  minexponent(0.0_real128),  maxexponent(0.0_real128)
      end program demo_maxexponent
