      program demo_cshift
      implicit none
      integer, dimension(5)   :: i1
      integer, dimension(3,4) :: a, b
         !basics
          i1=[10,20,30,40,50]
          print *,'start with:'
          print '(1x,5i3)', i1
          print *,'shift -2'
          print '(1x,5i3)', cshift(i1,-2)
          print *,'shift +2'
          print '(1x,5i3)', cshift(i1,+2)

          print *,'start with a matrix'
          a = reshape( [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 ], [ 3, 4 ])
          print '(4i3)', a(1,:)
          print '(4i3)', a(2,:)
          print '(4i3)', a(3,:)
          print *,'matrix shifted along rows, each by its own amount [-1,0,1]'
          b = cshift(a, SHIFT=[1, 0, -1], DIM=2)
          print *
          print '(4i3)', b(1,:)
          print '(4i3)', b(2,:)
          print '(4i3)', b(3,:)
      end program demo_cshift
