     program demo_rank_decreasing
     ! rank input array ranking duplicates the same
     use M_orderpack, only : rank_decreasing
     implicit none
     character(len=*),parameter :: fmt='(a,*(g3.3,1x))'
     integer,allocatable,dimension(:) :: INVALS, igoest, distinct, count
     integer :: imx, i
        ! create an input array
        INVALS=[11, 11, 22, 11, 33, 33, 22, 33, 33]
        ! make an index array of the same size
        if(allocated(igoest))deallocate(igoest)
        allocate(igoest(size(INVALS)))
        print fmt, 'Original:                 ',INVALS
        print fmt, 'Number of indices to sort:',size(INVALS)
        ! rank input array ranking duplicates the same
        call rank_decreasing(INVALS,igoest)
        print fmt, 'Returned Indices:         ',igoest(:)
        !
        ! interrogate the results
        !
        imx=maxval(igoest)
        print fmt, 'Number of unique indices :',imx
        ! squeeze it down to just IMX unique values
        count=[(0,i=1,imx)] ! count how many times a value occurs
        distinct=count      ! array to set of unique values
        do i=1,size(INVALS)
           distinct(igoest(i))=INVALS(i)
           count(igoest(i))= count(igoest(i))+1
        enddo
        print fmt, 'Sorted unique values:     ',distinct
        print fmt, 'count of occurrences:     ',count
     end program demo_rank_decreasing
