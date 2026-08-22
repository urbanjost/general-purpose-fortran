      program demo_random_seed
         implicit none
         integer, allocatable :: seed(:),initial_seed(:)
         integer :: i,j,n
         real :: x(3)
         call random_seed() ! set random seed if f2023

         call random_seed(size = n)
         allocate(seed(n))
         call random_seed(get=seed)
         initial_seed=seed

         write (*, *) 'queried initial seed=',seed
         write (*,*) 'get three sets of random numbers'
         do i=1,3
            call random_number(x)
            write(*,*)x
         enddo

         ! now randomize the seed several times, query
         ! and print it, and then generate an array of PRN
         do i=1,3
            call random_seed() ! randomize seed if f2023
            call random_seed(get=seed)
            write (*, *) 'new seed=',seed
            call random_number(x)
            write(*,*)'set with new seed=',x
         enddo

         ! now go back to initial seed and should reproduce
         ! initial set
         write(*,*)'back to initial'
         call random_seed(put=initial_seed)

         ! repeat first display
         call random_seed(get=seed)
         write (*, *) 'queried current seed=',seed
         write (*,*) 'get three sets of random numbers,'
         write (*,*) 'should be duplicates of first set'
         do i=1,3
            call random_number(x)
            write(*,*)x
         enddo
      end program demo_random_seed
