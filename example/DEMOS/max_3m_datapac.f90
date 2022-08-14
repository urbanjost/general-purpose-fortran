     program demo_max
     use M_datapac, only : max, label
     !use M_datapac, only : intel_max=>max, label !  ifort (IFORT) 2021.3.0 20210609 bug

     implicit none
     real :: xmax
        call label('max')
        !call intel_max([-100.0, 200.0, 0.0, 400.0, -200.0],5,1,xmax)
        call max([-100.0, 200.0, 0.0, 400.0, -200.0],5,1,xmax)
        write(*,*)xmax
     end program demo_max
