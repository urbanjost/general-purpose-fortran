     program demo_sort_quick_rx
     use M_sort, only : sort_quick_rx
     implicit none
     integer,parameter            :: isz=10000
     real                         :: rr(isz)
     integer                      :: ii(isz)
     integer                      :: i
     write(*,*)'initializing array with ',isz,' random numbers'
     CALL RANDOM_NUMBER(RR)
     rr=rr*450000.0
     write(*,*)'sort real array with sort_quick_rx(3f)'
     call sort_quick_rx(rr,ii)
     write(*,*)'checking index of sort_quick_rx(3f)'
     do i=1,isz-1
        if(rr(ii(i)).gt.rr(ii(i+1)))then
           write(*,*)'Error in sorting reals small to large ', &
           & i,rr(ii(i)),rr(ii(i+1))
        endif
     enddo
     write(*,*)'test of sort_quick_rx(3f) complete'
     ! use the index array to actually move the input array into a sorted
     ! order
     rr=rr(ii)
     do i=1,isz-1
        if(rr(i).gt.rr(i+1))then
           write(*,*)'Error in sorting reals small to large ', &
           & i,rr(i),rr(i+1)
        endif
     enddo
     write(*,*)'test of sort_quick_rx(3f) complete'
     end program demo_sort_quick_rx
