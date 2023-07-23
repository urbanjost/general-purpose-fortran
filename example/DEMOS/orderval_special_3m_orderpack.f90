     program demo_orderval_special
     ! return Nth ordered value of an array
     use M_orderpack, only : orderval_special, medianval
     implicit none
     character(len=*),parameter :: list= '(*(g0:,", "))',sp='(*(g0,1x))'
     integer,allocatable :: iarr(:)
     integer :: i
        iarr=[80,70,30,40,-50,60,20,10]
        print sp, 'ORIGINAL:',iarr
        ! can return the same values as intrinsics minval(3f) and maxval(3f)
        print sp, 'minval',orderval_special(iarr,1),          minval(iarr)
        print sp, 'maxval',orderval_special(iarr,size(iarr)), maxval(iarr)
        ! but more generally it can return the Nth lowest value.
        print sp, 'median',orderval_special(iarr,(size(iarr+1))/2), &
        & medianval(iarr)
        ! so only Nth ordered value can be found
        print sp,'inord=',3, ' fractile=',orderval_special(iarr,3)
        ! sorting the hard way
        print sp, 'ORIGINAL:',iarr
        do i=1,size(iarr)
           write(*,list)i,orderval_special(iarr,i)
        enddo
        print *
     end program demo_orderval_special
