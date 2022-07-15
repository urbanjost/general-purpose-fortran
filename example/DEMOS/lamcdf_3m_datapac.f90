     program demo_lamcdf
     !@(#) line plotter graph of cumulative distribution function
     use M_datapac, only : lamcdf, plott, label
     implicit none
     real,allocatable  :: x(:), y(:)
     real              :: alamba
     integer           :: i
        call label('lamcdf')
        alamba=4.0
        x=[(real(i)/100.0/alamba,i=-100,100,1)]
        if(allocated(y))deallocate(y)
        allocate(y(size(x)))
        do i=1,size(x)
           call lamcdf(X(i),Alamba,y(i))
        enddo
        call plott(x,y,size(x))
     end program demo_lamcdf
