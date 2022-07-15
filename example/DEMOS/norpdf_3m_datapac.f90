     program demo_norpdf
     !@(#) line plotter graph of probability density function
     use M_datapac, only : norpdf, plott, label
     implicit none
     real,allocatable  :: x(:), y(:)
     integer           :: i
        call label('norpdf')
        x=[(real(i),i=-100,100,1)]
        if(allocated(y))deallocate(y)
        allocate(y(size(x)))
        do i=1,size(x)
           call norpdf(x(i)/10.0,y(i))
        enddo
        call plott(x,y,size(x))
     end program demo_norpdf
