          program demo_merge
          implicit none
          integer :: tvals(2,3), fvals(2,3), answer(2,3)
          logical :: mask(2,3)
          integer :: i

             tvals(1,:)=[  10, -60,  50 ]
             tvals(2,:)=[ -20,  40, -60 ]

             fvals(1,:)=[ 0, 3, 2 ]
             fvals(2,:)=[ 7, 4, 8 ]

             mask(1,:)=[ .true.,  .false., .true. ]
             mask(2,:)=[ .false., .false., .true. ]

             write(*,*)'mask of logicals'
             answer=merge( tvals, fvals, mask )
             write(*, '(3i4)')(answer(i, :), i=1, size(answer, dim=1))

             write(*, *)'highest values'
             answer=merge( tvals, fvals, tvals > fvals )
             write(*, '(3i4)')(answer(i, :), i=1, size(answer, dim=1))

             write(*, *)'lowest values'
             answer=merge( tvals, fvals, tvals < fvals )
             write(*, '(3i4)')(answer(i, :), i=1, size(answer, dim=1))

              end program demo_merge
