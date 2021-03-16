          program demo_move_alloc
          implicit none
          ! Example to allocate a bigger GRID
          real, allocatable :: grid(:), tempgrid(:)
          integer :: n, i

             ! initialize small GRID
             n = 3
             allocate (grid(1:n))
             grid = [ (real (i), i=1,n) ]

             ! initialize TEMPGRID which will be used to replace GRID
             allocate (tempgrid(1:2*n))    ! Allocate bigger grid
             tempgrid(::2)  = grid         ! Distribute values to new locations
             tempgrid(2::2) = grid + 0.5   ! initialize other values

             ! move TEMPGRID to GRID
             call MOVE_ALLOC (from=tempgrid, to=grid)

             ! TEMPGRID should no longer be allocated
             ! and GRID should be the size TEMPGRID was
             if (size (grid) /= 2*n .or. allocated (tempgrid)) then
                print *, "Failure in move_alloc!"
             endif
             print *, allocated(grid), allocated(tempgrid)
             print '(99f8.3)', grid

              end program demo_move_alloc
