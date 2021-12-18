     program demo_number_of_lines
     use M_io,      only : number_of_lines, fileopen
     implicit none
     integer :: ios
     integer :: lun
        lun=fileopen('test.txt','r',ios)
        if(ios.eq.0)then
           write(*,*) number_of_lines(lun)
        else
           write(*,*)'ERROR: IOS=',ios
        endif
     end program demo_number_of_lines
