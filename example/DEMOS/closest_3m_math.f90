          program demo_closest
          use M_math, only : closest
          implicit none
          real,allocatable :: x(:),y(:)
          real             :: x1, y1
          integer          :: id
          x=[ 11.0,  100.0, -22.34, 26.4, -50.66 ]
          y=[-21.0,  150.0, -82.00, 40.0, 350.00 ]
          x1=30.0
          y1=44.0
          id=closest(x1,y1,x,y)
          write(*,*)'Closest point: ', x(id), y(id), ' at index ',id
          end program demo_closest
