     program demo_rotate13
     use M_strings, only : rotate13
     implicit none
     character(len=256) :: line
     integer            :: iostat
     do
        read(*,'(a)',iostat=iostat)line
        if(iostat /= 0)exit
        write(*,'(a)')rotate13(line)
     enddo
     end program demo_rotate13
