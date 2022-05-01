     program demo_orderval
     !  Return value of Nth lowest value of array
     use M_orderpack, only : orderval
     implicit none
     character(len=*),parameter :: list= '(*(g0:,", "))'
     character(len=*),parameter :: sp='(*(g0,1x))'
     real,parameter ::  INVALS(*)=[1.1,20.20,3.3,10.10,5.5,4.4,2.2]
     integer :: i
     integer :: imiddle
        write(*,list) 'ORIGINAL:',INVALS
        ! can return the same values as intrinsics minval(3f) and maxval(3f)
        print sp, 'minval',orderval(INVALS,1),          minval(INVALS)
        print sp, 'maxval',orderval(INVALS,size(INVALS)), maxval(INVALS)
        ! but more generally it can return the Nth lowest value.
        print sp,'nord=',4, ' fractile=',orderval(INVALS,4)
        ! so a value at the middle would be
        imiddle=(size(INVALS)+1)/2
        print sp,'median',orderval(INVALS,imiddle)
        ! sorting the hard way
        do i=1,size(INVALS)
           write(*,list)i,orderval(INVALS,i)
        enddo
     end program demo_orderval
