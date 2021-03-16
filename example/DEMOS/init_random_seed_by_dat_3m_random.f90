           program demo_init_random_seed_by_dat
           use M_random, only : init_random_seed_by_dat
           integer :: i
           real    :: x
              call init_random_seed_by_dat()
              do i=1,10
                 ! generate real pseudo-random numbers from 0 to <1.0
                 call random_number(x)
                 write(*,*)i,x
              enddo
           end program demo_init_random_seed_by_dat
