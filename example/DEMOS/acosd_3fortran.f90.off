      program demo_acosd
      use, intrinsic :: iso_fortran_env, only : real32,real64,real128
      implicit none
      character(len=*),parameter :: all='(*(g0,1x))'
      real(kind=real64) :: x , d2r

         ! basics
          print *,'acosd(-1.0) -->',acosd( -1.0 )
          print *,'acosd( 0.0) -->',acosd( -1.0 )
          print *,'acosd( 1.0) -->',acosd(  0.0 )
          x = 0.866_real64
          print all,'acosd(',x,') is ', acosd(x)
         ! any real kind
          write(*,*) acosd(-1.0_real64)
         ! elemental
          print all,'elemental',acosd([-1.0,-0.5,0.0,0.50,1.0])
         !
      end program demo_acosd
