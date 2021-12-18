     program demo_sort_indexed
     use M_sort, only : sort_indexed
     implicit none
     integer,parameter            :: isz=10000
     real                         :: rr(isz)
     integer                      :: i
     write(*,*)'initializing array with ',isz,' random numbers'
     CALL RANDOM_NUMBER(RR)
     rr=rr*450000.0
     ! use the index array to actually move the input array into a sorted order
     rr=rr(sort_indexed(rr))
     ! or
     !rr(sort_indexed(rr))=rr
     write(*,*)'checking if values are sorted(3f)'
     do i=1,isz-1
        if(rr(i).gt.rr(i+1))then
           write(*,*)'Error in sorting reals small to large ',i,rr(i),rr(i+1)
        endif
     enddo
     write(*,*)'test of sort_indexed(3f) complete'
     end program demo_sort_indexed
