       program demo_system_getchar
       use M_system,      only : system_getchar, system_putchar
       implicit none
       integer :: i
       integer :: iostat
       character(len=1) :: ch
       do while(system_getchar(ch).ge.0)
          iostat= system_putchar('[')
          iostat= system_putchar(ch)
          iostat= system_putchar(']')
       enddo
       end program demo_system_getchar
