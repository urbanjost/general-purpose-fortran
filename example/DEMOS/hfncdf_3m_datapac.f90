     program demo_hfncdf
     !@(#) line plotter graph of cumulative distribution function
     !@(#) for the halfnormal distribution
     use M_datapac, only : hfncdf, plott, label
     implicit none
     real,allocatable  :: x(:), y(:)
     integer           :: i
        call label('hfncdf')
        x=[(real(i),i=0,100,1)]
        if(allocated(y))deallocate(y)
        allocate(y(size(x)))
        do i=1,size(x)
           call hfncdf(x(i)/10.0,y(i))
        enddo
        call plott(x,y,size(x))
     end program demo_hfncdf
