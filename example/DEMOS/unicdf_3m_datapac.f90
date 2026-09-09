     program demo_unicdf
     !@(#) line plotter graph of function
     use M_datapac, only : unicdf, plott, label
     implicit none
     integer,parameter :: n=40
     real              :: x(0:n), y(0:n)
     integer           :: i
        call label('unicdf')
        x=[(real(i)/real(n),i=0,n)]
        do i=0,n
           call unicdf(x(i),y(i))
        enddo
        call plott(x,y,n+1)
     end program demo_unicdf
