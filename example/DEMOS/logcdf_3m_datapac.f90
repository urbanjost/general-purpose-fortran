     program demo_logcdf
     !@(#) line plotter graph of cumulative distribution function
     use M_datapac, only : logcdf, plott, label
     implicit none
     real,allocatable  :: x(:), y(:)
     integer           :: i
        call label('logcdf')
        x=[(real(i),i=-100,100,1)]
        if(allocated(y))deallocate(y)
        allocate(y(size(x)))
        do i=1,size(x)
           call logcdf(x(i)/10.0,y(i))
        enddo
        call plott(x,y,size(x))
     end program demo_logcdf
