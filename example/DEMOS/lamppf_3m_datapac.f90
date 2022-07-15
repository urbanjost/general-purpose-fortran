     program demo_lamppf
     !@(#) line plotter graph of function
     use M_datapac, only : lamppf, plott, label
     implicit none
     integer,parameter :: n=200
     real              :: x(n), y(n)
     real              :: alamba
     integer           :: i
        alamba=3.3333
        call label('lamppf')
        x=[(real(i)/real(n+1),i=1,n)]
        do i=1,n
           call lamppf(x(i),alamba,y(i))
        enddo
        call plott(x,y,n)
     end program demo_lamppf
