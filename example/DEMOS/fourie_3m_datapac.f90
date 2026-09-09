     program demo_fourie
     use M_datapac, only : fourie
     implicit none
     real :: x(100)
     integer :: i
     x=0.0
     do i=1,size(x)
        x(i)=200.0*sin(real(i))*i
        x(i)=x(i)+cos(2.4*x(i))
        x(i)=x(i)+3.1
     enddo
     write(*,*)x
     call fourie(x,size(x))
     end program demo_fourie
