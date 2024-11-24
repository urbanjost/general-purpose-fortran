     program demo_putchar
     use M_io, only : getchar, putchar
     implicit none
     character(len=1) :: byte
     integer :: istat
        ! copy stdin to stdout as a stream one byte at a time
        do while (getchar(byte).ge.0)
           istat=putchar(byte)
        enddo
     end program demo_putchar
