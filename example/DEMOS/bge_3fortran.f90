      program demo_bge
      use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64
      implicit none
      integer            :: i
      integer(kind=int8) :: byte
      integer(kind=int8),allocatable :: arr1(:), arr2(:)

        ! BASIC USAGE
         write(*,*)'bge(-127,127)=',bge( -127, 127 )
         ! on (very common) "two's complement" machines that are
         ! little-endian -127 will be greater than 127

         ! BOZ constants
         ! BOZ constants are subject to truncation, so make sure
         ! your values are valid for the integer kind being compared to
         write(*,*)'bge(b"0001",2)=',bge( b"1", 2)

        ! ELEMENTAL
         ! an array and scalar
         write(*, *)'compare array of values [-128, -0, +0, 127] to 127'
         write(*, *)bge(int([-128, -0, +0, 127], kind=int8), 127_int8)

         ! two arrays
         write(*, *)'compare two arrays'
         arr1=int( [ -127, -0, +0,  127], kind=int8 )
         arr2=int( [  127,  0,  0, -127], kind=int8 )
         write(*,*)'arr1=',arr1
         write(*,*)'arr2=',arr2
         write(*, *)'bge(arr1,arr2)=',bge( arr1, arr2 )

        ! SHOW TESTS AND BITS
         ! actually looking at the bit patterns should clarify what affect
         ! signs have ...
         write(*,*)'Compare some one-byte values to 64.'
         write(*,*)'Notice that the values are tested as bits not as integers'
         write(*,*)'so the results are as if values are unsigned integers.'
         do i=-128,127,32
            byte=i
            write(*,'(sp,i0.4,*(1x,1l,1x,b0.8))')i,bge(byte,64_int8),byte
         enddo

        ! SIGNED ZERO
         ! are +0 and -0 the same on your platform? When comparing at the
         ! bit level this is important
         write(*,'("plus zero=",b0)')  +0
         write(*,'("minus zero=",b0)') -0

      end program demo_bge
