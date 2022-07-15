     program demo_nbcdf
     !@(#) line plotter graph of cumulative distribution function
     use M_datapac, only : nbcdf, plott, label
     implicit none
     real,allocatable  :: x(:), y(:)
     real              :: p
     integer           :: i
     integer           :: n
        call label('nbcdf')
        x=[(real(i),i=0,100,1)]
        if(allocated(y))deallocate(y)
        allocate(y(size(x)))
        p=0.50
        n=size(x)
        do i=1,size(x)
           call NBCDF(X(i),P,N,y(i))
        enddo
        call plott(x,y,size(x))
     end program demo_nbcdf
