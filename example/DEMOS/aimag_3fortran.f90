      program demo_aimag
      use, intrinsic :: iso_fortran_env, only : real32, real64, real128
      implicit none
      character(len=*),parameter :: it='(*(1x,g0))'
      integer              :: i
      complex              :: z4
      complex              :: arr(3)
      complex(kind=real64) :: z8

          print it, 'basics:'

          z4 = cmplx(1.e0, 2.e0)
          print *,  'value=',z4
          print it, 'imaginary part=',aimag(z4),'or', z4%im

          print it, 'kinds other than the default may be supported'

          z8 = cmplx(3.e0_real64, 4.e0_real64,kind=real64)
          print *,  'value=',z8
          print it, 'imaginary part=',aimag(z8),'or', z8%im

          print it, 'an elemental function can be passed an array'
          print it, 'given a complex array:'

          arr=[z4,z4/2.0,z4+z4]
          print *,  (arr(i),new_line('a'),i=1,size(arr))
          print it, 'the imaginary component is:'
          print it, aimag( arr )

      end program demo_aimag
