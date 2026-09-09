     program demo_parcdf
     !@(#) line plotter graph of cumulative distribution function
     use M_datapac, only : parcdf, plott, label
     implicit none
     real,allocatable  :: x(:), y(:)
     real              :: gamma
     integer           :: i
        call label('parcdf')
        x=[(real(i)/10.0+1.0,i=1,100,1)]
        if(allocated(y))deallocate(y)
        allocate(y(size(x)))
        gamma=0.3
        do i=1,size(x)
           call parcdf(X(i),Gamma,y(i))
        enddo
        call plott(x,y,size(x))
     end program demo_parcdf
