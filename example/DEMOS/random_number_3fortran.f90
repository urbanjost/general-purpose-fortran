          program demo_random_number
          implicit none
          integer, allocatable :: seed(:)
          integer              :: n
          integer              :: first,last
          integer              :: i
          integer              :: rand_int
          integer,allocatable  :: count(:)
          real                 :: rand_val
          call random_seed(size = n)
          allocate(seed(n))
          call random_seed(get=seed)
          first=1
          last=10
          allocate(count(last-first+1))
          ! To have a discrete uniform distribution on the integers {first, first+1,
          ! ..., last-1, last} carve the continuous distribution up into last+1-first
          ! equal sized chunks, mapping each chunk to an integer.
          !
          ! One way is:
          !   call random_number(rand_val)
          ! choose one from last-first+1 integers
          !   rand_int = first + FLOOR((last+1-first)*rand_val)
             count=0
          ! generate a lot of random integers from 1 to 10 and count them.
          ! with a large number of values you should get about the same number
          ! of each value
             do i=1,100000000
                call random_number(rand_val)
                rand_int=first+floor((last+1-first)*rand_val)
                if(rand_int.ge.first.and.rand_int.le.last)then
                   count(rand_int)=count(rand_int)+1
                else
                   write(*,*)rand_int,' is out of range'
                endif
             enddo
             write(*,'(i0,1x,i0)')(i,count(i),i=1,size(count))
          end program demo_random_number
