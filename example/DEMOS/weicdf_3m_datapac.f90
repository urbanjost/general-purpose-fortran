     program demo_weicdf
     !@(#) line plotter graph of cumulative distribution function
     use M_datapac, only : weicdf, plott, label
     implicit none
     real,allocatable  :: x(:), y(:)
     real              :: gamma
     integer           :: i
        call label('weicdf')
        x=[((real(i)+epsilon(0.0))/30.0,i=0,100,1)]
        if(allocated(y))deallocate(y)
        allocate(y(size(x)))
        gamma=12.2
        do i=1,size(x)
           call weicdf(X(i),Gamma,y(i))
        enddo
        call plott(x,y,size(x))
     end program demo_weicdf
