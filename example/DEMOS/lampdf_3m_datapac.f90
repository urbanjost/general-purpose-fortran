     program demo_lampdf
     !@(#) line plotter graph of probability density function
     use M_datapac, only : lampdf, plott, label
     implicit none
     real,allocatable  :: x(:), y(:)
     real              :: alamba
     integer           :: i
        call label('lampdf')
        alamba=0.0
        x=[(real(i),i=-100,100,1)]
        if(allocated(y))deallocate(y)
        allocate(y(size(x)))
        do i=1,size(x)
           call LAMPDF(X(i)/100.0,Alamba,y(i))
        enddo
        call plott(x,y,size(x))
     end program demo_lampdf
