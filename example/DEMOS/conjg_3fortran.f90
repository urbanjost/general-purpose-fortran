      program demo_conjg
      use, intrinsic :: iso_fortran_env, only : real32, real64, real128
      implicit none
      complex :: z = (2.0, 3.0)
      complex(kind=real64) :: dz = (   &
         &  1.2345678901234567_real64, -1.2345678901234567_real64)
      complex :: arr(3,3)
      integer :: i
         ! basics
          ! notice the sine of the imaginary component changes
          print *, z, conjg(z)

          ! any complex kind is supported. z is of default kind but
          ! dz is kind=real64.
          print *, dz
          dz = conjg(dz)
          print *, dz
          print *

          ! the function is elemental so it can take arrays
          arr(1,:)=[(-1.0, 2.0),( 3.0, 4.0),( 5.0,-6.0)]
          arr(2,:)=[( 7.0,-8.0),( 8.0, 9.0),( 9.0, 9.0)]
          arr(3,:)=[( 1.0, 9.0),( 2.0, 0.0),(-3.0,-7.0)]

          write(*,*)'original'
          write(*,'(3("(",g8.2,",",g8.2,")",1x))')(arr(i,:),i=1,3)
          arr = conjg(arr)
          write(*,*)'conjugate'
          write(*,'(3("(",g8.2,",",g8.2,")",1x))')(arr(i,:),i=1,3)

      end program demo_conjg
