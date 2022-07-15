     program demo_move
     use M_datapac, only : move, label
     real,allocatable :: x(:), y(:)
        call label('move')
        x=[10.0,20.0,30.0,40.0,50.0,60.0,70.0,80.0,90.0,100.0,110.0,120.0]
        if(allocated(y))deallocate(y)
        allocate(y(size(x)))
        y=99.0
        call MOVE(X,4,5,1,Y)
        write(*,*)int(y)
     end program demo_move
