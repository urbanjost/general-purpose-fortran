      program demo_ieor
      use,intrinsic :: iso_fortran_env,  only : int8, int16, int32, int64
      implicit none
      integer(kind=int16) :: i,j
        ! basic usage
         print *,ieor (16, 1), ' ==> ieor(16,1) has the value 17'

         ! it is easier to see using binary representation
         i=int(b'0000000000111111',kind=int16)
         j=int(b'0000001111110000',kind=int16)
         write(*,'(a,b16.16,1x,i0)')'i=     ',i, i
         write(*,'(a,b16.16,1x,i0)')'j=     ',j, j
         write(*,'(a,b16.16,1x,i0)')'result=',ieor(i,j), ieor(i,j)

        ! elemental
         print *,'arguments may be arrays. If both are arrays they '
         print *,'must have the same shape.                        '
         print *,ieor(i=[7,4096,9], j=2)

         ! both may be arrays if of the same size

      end program demo_ieor
