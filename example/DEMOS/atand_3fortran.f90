      program demo_atand
      use, intrinsic :: iso_fortran_env, only : real32, real64, real128
      implicit none
      character(len=*),parameter :: all='(*(g0,1x))'
      real(kind=real64),parameter :: &
       Deg_Per_Rad = 57.2957795130823208767981548_real64
      real(kind=real64) :: x
          x=2.866_real64
          print all, atand(x)

          print all, atand( 2.0d0, 2.0d0),atand( 2.0d0, 2.0d0)/Deg_Per_Rad
          print all, atand( 2.0d0,-2.0d0),atand( 2.0d0,-2.0d0)/Deg_Per_Rad
          print all, atand(-2.0d0, 2.0d0),atand(-2.0d0, 2.0d0)/Deg_Per_Rad
          print all, atand(-2.0d0,-2.0d0),atand(-2.0d0,-2.0d0)/Deg_Per_Rad

      end program demo_atand
