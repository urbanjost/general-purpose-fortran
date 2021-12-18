     program demo_cosd
     use M_units, only :  cosd
     implicit none
        write(*,*)'With REAL array input    ', &
        & cosd([ 0.0, 15.0, 30.0, 45.0, 60.0, 75.0, 90.0, 180.0, 270.0 ])
        write(*,*)'With INTEGER array input ',  &
        & cosd([0,15,30,45,60,75,90,180,270])
        write(*,*)'With DOUBLEPRECISION     ',        &
        & cosd(0.0d0),cosd(15.0d0),cosd(90.0/3.0d0),  &
        & cosd(90.0/2.0d0),cosd(60.0d0),cosd(75.0d0), &
        & cosd(90.0d0),cosd(180.0d0),cosd(270.0d0)
     end program demo_cosd
