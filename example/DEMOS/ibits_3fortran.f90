      program demo_ibits
      use,intrinsic :: iso_fortran_env,  only : int8, int16, int32, int64
      implicit none
      integer(kind=int16) :: i,j
        ! basic usage
         print *,ibits (14, 1, 3) ! should be seven
         print *,ibits(-1,10,3)   ! and so is this
         ! it is easier to see using binary representation
         i=int(b'0101010101011101',kind=int16)
         write(*,'(b16.16,1x,i0)') ibits(i,3,3), ibits(i,3,3)

        ! we can illustrate this as
         !        #-- position 15
         !        |              #-- position 0
         !        |   <-- +len   |
         !        V              V
         !        5432109876543210
         i =int(b'1111111111111111',kind=int16)
         !          ^^^^
         j=ibits(i,10,4) ! start at 10th from left and proceed
                         ! left for a total of 4 characters
         write(*,'(a,b16.16)')'j=',j
        ! lets do something less ambiguous
         i =int(b'0010011000000000',kind=int16)
         j=ibits(i,9,5)
         write(*,'(a,b16.16)')'j=',j
      end program demo_ibits
