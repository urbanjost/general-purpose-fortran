     program demo_min
     use M_datapac, only : min, label
     implicit none
     real :: xmin
        call label('min')
        call min([-100.0, 200.0, 0.0, 400.0, -200.0],5,1,xmin)
        write(*,*)xmin
     end program demo_min
