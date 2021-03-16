          program demo_cartesian_to_spherical
          use M_units, only : cartesian_to_spherical
          implicit none
          real    :: x,y,z
          real    :: r,i,a
          integer :: ios
          INFINITE: do
             read(*,*,iostat=ios) x, y, z
             if(ios.ne.0)exit INFINITE
             call cartesian_to_spherical(x,y,z,r,i,a)
             write(*,*)'x=',x,' y=',y,' z=',z,'radius=',r,'inclination=',i,'azimuth=',a
          enddo INFINITE
          end program demo_cartesian_to_spherical
