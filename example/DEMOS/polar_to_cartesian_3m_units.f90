          program demo_polar_to_cartesian
          use M_units, only : polar_to_cartesian
          implicit none
          real    :: x,y
          real    :: r,i
          integer :: ios
          INFINITE: do
             write(*,'(g0)',advance='no')'Enter radius and inclination(in radians):'
             read(*,*,iostat=ios) r, i
             if(ios.ne.0)exit INFINITE
             call polar_to_cartesian(r,i,x,y)
             write(*,*)'x=',x,' y=',y,'radius=',r,'inclination=',i
          enddo INFINITE
          end program demo_polar_to_cartesian
