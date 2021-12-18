     program demo_percent_done
     use m_time, only : system_sleep
     use m_messages, only : percent_done
     implicit none
     integer :: i, nr=10

     do i=1,nr
        call percent_done(i,nr)
        call system_sleep(1)  !give a delay in seconds
     enddo

     end program demo_percent_done
