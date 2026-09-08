     program demo_lgnppf
     !@(#) line plotter graph of function
     use M_datapac, only : lgnppf, plott, label
     implicit none
     integer,parameter :: n=200
     real              :: x(n), y(n)
     integer           :: i
        call label('lgnppf')
        x=[(real(i)/real(n+1),i=1,n)]
        do i=1,n
           call lgnppf(x(i),y(i))
        enddo
        call plott(x,y,n)
     end program demo_lgnppf
