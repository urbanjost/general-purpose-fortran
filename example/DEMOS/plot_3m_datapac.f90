     program demo_plot
     use M_datapac, only : plot
     implicit none
     integer ::  i
     real, allocatable ::  x(:), y(:)
       x=[(real(i),i=1,30)]
       y=0.075*(x**4)-0.525*(x**3)+0.75*(x**2)+2.40
       call plot(x,y,size(x))
       y=[(real(i)/10.0,i=1,30)]
       x=y**3.78-6*y**2.52+9*y**1.26
       call plot(x,y,size(x))
     end program demo_plot
