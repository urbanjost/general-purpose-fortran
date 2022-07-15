     program demo_tcdf
     !@(#) line plotter graph of cumulative distribution function
     use M_datapac, only : tcdf, plott, label
     implicit none
     real,allocatable  :: x(:), y(:)
     integer           :: nu
     integer           :: i
        call label('tcdf')
        x=[(real(i)/20.0,i=0,100,1)]
        if(allocated(y))deallocate(y)
        allocate(y(size(x)))
        nu=12
        do i=1,size(x)
           call tcdf(X(i),Nu,y(i))
        enddo
        call plott(x,y,size(x))
     end program demo_tcdf
