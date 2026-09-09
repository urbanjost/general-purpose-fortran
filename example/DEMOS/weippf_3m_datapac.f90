     program demo_weippf
     !@(#) line plotter graph of function
     use M_datapac, only : weippf, plott, label
     implicit none
     integer,parameter :: n=200
     real              :: x(n), y(n)
     real              :: gamma
     integer           :: i
        gamma=2.0
        call label('weippf')
        x=[(real(i)/real(n+1),i=1,n)]
        do i=1,n
           call weippf(x(i),gamma,y(i))
        enddo
        call plott(x,y,n)
     end program demo_weippf
