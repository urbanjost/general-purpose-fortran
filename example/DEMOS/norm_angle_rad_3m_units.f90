     program demo_norm_angle_rad
     use M_units, only : norm_angle_rad
     implicit none
     real,parameter :: PI=4*atan(1.0)
     real           :: val
     integer        :: i
     do i=-2,2
        val=i*2*pi;   write(*,*)val,norm_angle_rad(val)
        val=i*pi;     write(*,*)val,norm_angle_rad(val)
        write(*,*)
     enddo
     write(*,*)norm_angle_rad([-PI/8.0,-PI/4.0,-PI/2.0,-PI,-0.0,PI/8.0,PI/4.0,PI/2.0,PI,0.0])
     end program demo_norm_angle_rad
