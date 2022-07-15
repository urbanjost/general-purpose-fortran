     program demo_caucdf
     !@(#) line plotter graph of cumulative distribution function
     use M_datapac, only : caucdf, plott, label
     implicit none
     real,allocatable  :: x(:), y(:)
     integer           :: i
        call label('caucdf')
        x=[(real(i),i=-100,100,1)]
        if(allocated(y))deallocate(y)
        allocate(y(size(x)))
        do i=1,size(x)
           call caucdf(x(i)/10.0,y(i))
        enddo
        call plott(x,y,size(x))
     end program demo_caucdf
