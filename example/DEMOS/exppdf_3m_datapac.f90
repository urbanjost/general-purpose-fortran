     program demo_exppdf
     !@(#) line plotter graph of probability density function
     use M_datapac, only : exppdf, plott, label
     implicit none
     real,allocatable  :: x(:), y(:)
     integer           :: i
        call label('exppdf')
        x=[(real(i),i=0,100,1)]
        if(allocated(y))deallocate(y)
        allocate(y(size(x)))
        do i=1,size(x)
           call exppdf(x(i)/10.0,y(i))
        enddo
        call plott(x,y,size(x))
     end program demo_exppdf
