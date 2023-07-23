      program demo_aimag
      use, intrinsic :: iso_fortran_env, only : real_kinds, &
       & real32, real64, real128
      implicit none
      character(len=*),parameter :: g='(*(1x,g0))'
      complex              :: z4
      complex(kind=real64) :: z8
         ! basics
          z4 = cmplx(1.e0, 2.e0)
          print *, 'value=',z4
          print g, 'imaginary part=',aimag(z4),'or', z4%im

          ! other kinds other than the default may be supported
          z8 = cmplx(3.e0_real64, 4.e0_real64,kind=real64)
          print *, 'value=',z8
          print g, 'imaginary part=',aimag(z8),'or', z8%im

          ! an elemental function can be passed an array
          print *
          print *, [z4,z4/2.0,z4+z4,z4**3]
          print *
          print *, aimag([z4,z4/2.0,z4+z4,z4**3])

      end program demo_aimag
