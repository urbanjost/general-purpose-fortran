     program demo_spherical_to_cartesian
     use M_units, only : spherical_to_cartesian
     implicit none
     real    :: x,y,z
     real    :: r,i,a
     integer :: ios
     INFINITE: do
        read(*,*,iostat=ios) x, y, z
        if(ios.ne.0)exit INFINITE
        call spherical_to_cartesian(r,i,a,x,y,z)
        write(*,*)'x=',x,' y=',y,' z=',z,'radius=',r,'inclination=',i,'azimuth=',a
     enddo INFINITE
     end program demo_spherical_to_cartesian
