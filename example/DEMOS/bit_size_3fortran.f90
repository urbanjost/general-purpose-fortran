      program demo_bit_size
      use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64
      use,intrinsic :: iso_fortran_env, only : integer_kinds
      implicit none
      character(len=*),parameter   :: fmt=&
      & '(a,": bit size is ",i3," which is kind=",i3," on this platform")'

          ! default integer bit size on this platform
          write(*,fmt) "default", bit_size(0), kind(0)

          write(*,fmt) "int8   ", bit_size(0_int8),   kind(0_int8)
          write(*,fmt) "int16  ", bit_size(0_int16),  kind(0_int16)
          write(*,fmt) "int32  ", bit_size(0_int32),  kind(0_int32)
          write(*,fmt) "int64  ", bit_size(0_int64),  kind(0_int64)

          write(*,'(a,*(i0:,", "))') "The available kinds are ",integer_kinds

      end program demo_bit_size
