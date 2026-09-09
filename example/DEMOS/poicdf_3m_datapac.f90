     program demo_poicdf
     !@(#) line plotter graph of cumulative distribution function
     use M_datapac, only : poicdf, plott, label
     implicit none
     real,allocatable :: x(:), y(:)
     real             :: alamba
     integer          :: i
        call label('poicdf')
        x=[(real(i),i=0,100,1)]
        if(allocated(y))deallocate(y)
        allocate(y(size(x)))
        alamba=29.5
        do i=1,size(x)
           call poicdf(X(i),Alamba,y(i))
        enddo
        call plott(x,y,size(x))
     end program demo_poicdf
