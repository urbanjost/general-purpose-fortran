       program demo_sort_shell
       use M_sort, only : sort_shell
       implicit none
       character(len=:),allocatable :: array(:)
       integer :: i

       array = [                                                     &
       & 'red    ','green  ','blue   ','yellow ','orange ','black  ',&
       & 'white  ','brown  ','gray   ','cyan   ','magenta',          &
       & 'purple ']

       write(*,'(a,*(a:,","))')'BEFORE ',(trim(array(i)),i=1,size(array))
       call sort_shell(array,order='a')
       write(*,'(a,*(a:,","))')'A-Z    ',(trim(array(i)),i=1,size(array))
       do i=1,size(array)-1
          if(array(i).gt.array(i+1))then
             write(*,*)'Error in sorting strings a-z'
          endif
       enddo

       array= [                                                      &
       & 'RED    ','GREEN  ','BLUE   ','YELLOW ','ORANGE ','BLACK  ',&
       & 'WHITE  ','BROWN  ','GRAY   ','CYAN   ','MAGENTA',          &
       & 'PURPLE ']

       write(*,'(a,*(a:,","))')'Before ',(trim(array(i)),i=1,size(array))
       call sort_shell(array,order='d')
       write(*,'(a,*(a:,","))')'Z-A    ',(trim(array(i)),i=1,size(array))
       do i=1,size(array)-1
          if(array(i).lt.array(i+1))then
             write(*,*)'Error in sorting strings z-a'
          endif
       enddo

       end program demo_sort_shell
