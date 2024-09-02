      program demo_ibset
      use,intrinsic :: iso_fortran_env,  only : int8, int16, int32, int64
      implicit none
      integer(kind=int16) :: i
        ! basic usage
         print *,ibset (12, 1), 'ibset(12,1) has the value 14'

         ! it is easier to see using binary representation
         i=int(b'0000000000000110',kind=int16)
         write(*,'(b16.16,1x,i0,1x,i0)') ibset(i,12), ibset(i,12), i

        ! elemental
         print *,'an array of initial values may be given as well'
         print *,ibset(i=[0,4096], pos=2)
         print *
         print *,'a list of positions results in multiple returned values'
         print *,'not multiple bits set in one value, as the routine is  '
         print *,'a scalar function; calling it elementally essentially  '
         print *,'calls it multiple times.                               '
         write(*,'(b16.16)') ibset(i=0, pos=[1,2,3,4])

         ! both may be arrays if of the same size

      end program demo_ibset
