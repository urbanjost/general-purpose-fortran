     program demo_tail
     use M_datapac, only : tail, label
     implicit none
     real,allocatable :: x(:)
     integer :: i
        call label('tail')
        x=[(real(i)/10.0,i=1,2000)]
        x=x**3.78-6*x**2.52+9*x**1.26
        call tail(x,size(x))
     end program demo_tail
