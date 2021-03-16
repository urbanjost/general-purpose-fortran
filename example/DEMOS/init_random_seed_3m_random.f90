           program demo_init_random_seed
           use M_random, only : init_random_seed
           integer :: iseed
           integer :: i
           real    :: x
              iseed=218595421
              call init_random_seed(iseed)
              do i=1,10
                 ! generate real pseudo-random numbers from 0 to <1.0
                 call random_number(x)
                 write(*,*)i,x
              enddo
           end program demo_init_random_seed
