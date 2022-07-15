     program demo_logppf
     use M_datapac, only : logppf, plott, label
     implicit none
     integer,parameter :: n=40
     real              :: x(n), y(n)
     integer           :: i
        call label('logppf')
        x=[(real(i)/real(n+1),i=1,n)]
        do i=1,n
           call logppf(x(i),y(i))
        enddo
        call plott(x,y,n)
     end program demo_logppf
