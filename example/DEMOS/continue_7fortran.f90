     program demo_continue
     implicit none
     integer :: i,j
           i=10
           j=5
           if(i.lt.5)goto 100
           j=3
     100   continue
           write(*,*)'J=',j
     end program demo_continue
