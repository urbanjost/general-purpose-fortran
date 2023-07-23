      program demo_dshiftl
      use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64
      implicit none
      integer(kind=int32) :: i, j
      integer             :: shift

        ! basic usage
         write(*,*) dshiftl (1, 2**30, 2) ! int32 values on little-endian => 5

        ! print some simple calls as binary to better visual the results
         i=-1
         j=0
         shift=5
         call printit()

         ! the leftmost SHIFT bits of J are copied to the rightmost result bits
         j=int(b"11111000000000000000000000000000")
         ! and the other bits are the rightmost bits of I
         i=int(b"00000000000000000000000000000000")
         call printit()

         j=int(b"11111000000000000000000000000000")
         i=int(b"00000111111111111111111111111111")
         ! result should be all 1s
         call printit()

      contains
      subroutine printit()
         ! print i,j,shift and then i,j, and the result as binary values
          write(*,'(*(g0))')'I=',i,' J=',j,' SHIFT=',shift
          write(*,'(b32.32)') i,j, dshiftl (i, j, shift)
      end subroutine printit

      end program demo_dshiftl
