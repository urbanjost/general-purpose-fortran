      program demo_eoshift
      implicit none
      integer, dimension(3,3) :: a
      integer :: i

          a = reshape( [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ], [ 3, 3 ])
          print '(3i3)', (a(i,:),i=1,3)

          print *

          ! shift it
          a = eoshift(a, SHIFT=[1, 2, 1], BOUNDARY=-5, DIM=2)
          print '(3i3)', (a(i,:),i=1,3)

      end program demo_eoshift
