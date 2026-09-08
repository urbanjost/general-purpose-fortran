     program demo_min
     use M_datapac, only : minn, label
     implicit none
     real :: xmin
        call label('minn')
        call minn([-100.0, 200.0, 0.0, 400.0, -200.0],5,1,xmin)
        write(*,*)xmin
     end program demo_min
