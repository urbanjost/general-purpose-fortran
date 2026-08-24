     program demo_number_of_lines
     use M_io,      only : number_of_lines, fileopen
     implicit none
     integer :: iostat
     integer :: lun
        lun=fileopen('test.txt','r',iostat)
        if(iostat == 0)then
           write(*,*) number_of_lines(lun)
        else
           write(*,*)'ERROR: iostat=',iostat
        endif
     end program demo_number_of_lines
