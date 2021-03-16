          program demo_count
          implicit none
          integer, dimension(2,3) :: a, b
          logical, dimension(2,3) :: mymask
             a = reshape( [ 1, 2, 3, 4, 5, 6 ], [ 2, 3 ])
             b = reshape( [ 0, 7, 3, 4, 5, 8 ], [ 2, 3 ])
             print '(3i3)', a(1,:)
             print '(3i3)', a(2,:)
             print *
             print '(3i3)', b(1,:)
             print '(3i3)', b(2,:)
             print *
             mymask = a.ne.b
             print '(3l3)', mymask(1,:)
             print '(3l3)', mymask(2,:)
             print *
             print '(3i3)', count(mymask)
             print *
             print '(3i3)', count(mymask, 1)
             print *
             print '(3i3)', count(mymask, 2)
          end program demo_count
