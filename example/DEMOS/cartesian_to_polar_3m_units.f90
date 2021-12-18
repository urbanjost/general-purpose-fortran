     program demo_cartesian_to_polar
     use M_units, only : cartesian_to_polar
     implicit none
     real    :: x,y
     real    :: r,i
     integer :: ios
     INFINITE: do
        read(*,*,iostat=ios) x, y
        if(ios.ne.0)exit INFINITE
        call cartesian_to_polar(x,y,r,i)
        write(*,*)'x=',x,' y=',y,'radius=',r,'inclination=',i
     enddo INFINITE
     end program demo_cartesian_to_polar
