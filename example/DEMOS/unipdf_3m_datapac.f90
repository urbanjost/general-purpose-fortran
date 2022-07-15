     program demo_unipdf
     !@(#) line plotter graph of probability density function
     use M_datapac, only : unipdf, label
     implicit none
     real,allocatable  :: x(:), y(:)
     integer           :: i
        call label('unipdf')
        x=[(real(i)/10.0,i=0,10,1)]
        if(allocated(y))deallocate(y)
        allocate(y(size(x)))
        do i=1,size(x)
           call unipdf( x(i), y(i) )
        enddo
        write(*,*)y
     end program demo_unipdf
