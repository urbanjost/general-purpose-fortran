     program demo_lgncdf
     !@(#) line plotter graph of cumulative distribution function
     use M_datapac, only : lgncdf, plott, label
     implicit none
     real,allocatable  :: x(:), y(:)
     integer           :: i
        call label('lgncdf')
        x=[((real(i)+epsilon(0.0))/10.0,i=0,100,1)]
        if(allocated(y))deallocate(y)
        allocate(y(size(x)))
        do i=1,size(x)
           call lgncdf(x(i),y(i))
        enddo
        call plott(x,y,size(x))
     end program demo_lgncdf
