       program demo_sort_quick_compact
       use M_sort, only : sort_quick_compact
       implicit none
       integer,parameter            :: isz=10000
       real                         :: rrin(isz)
       real                         :: rrout(isz)
       integer                      :: i
       write(*,*)'initializing array with ',isz,' random numbers'
       CALL RANDOM_NUMBER(rrin)
       rrin=rrin*450000.0
       write(*,*)'sort real array with sort_quick_compact(3f)'
       rrout=sort_quick_compact(rrin)
       write(*,*)'checking '
       do i=1,isz-1
          if(rrout(i).lt.rrout(i+1))then
             write(*,*)'Error in sorting reals', &
             & i,rrout(i),rrout(i+1)
          endif
       enddo
       write(*,*)'test of sort_quick_compact(3f) complete'
       end program demo_sort_quick_compact
