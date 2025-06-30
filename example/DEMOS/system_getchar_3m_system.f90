     program demo_system_getchar
     use M_system, only : system_getchar, system_putchar
     implicit none
     character(len=1) :: ch
     integer :: iostat
     integer :: icount
     icount=0
     ! copy first 1024 characters from stdin to stdout
        do while(system_getchar(ch).ge.0)
           iostat=system_putchar(ch)
           icount=icount+1
           if(icount > 1024 ) exit
        enddo
     end program demo_system_getchar
