          program demo_norm_angle_360
          use M_units, only : norm_angle_360
          implicit none
          real,parameter :: PI=360
          real           :: val
          integer        :: i
          do i=-2,2
             val=i*2*pi;   write(*,*)val,norm_angle_360(val)
             val=i*pi;     write(*,*)val,norm_angle_360(val)
             val=i*pi/2;   write(*,*)val,norm_angle_360(val)
             write(*,*)
          enddo
          end program demo_norm_angle_360
