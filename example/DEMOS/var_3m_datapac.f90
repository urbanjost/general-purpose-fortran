     program demo_var
     use M_datapac, only : var, label
     implicit none
     real,allocatable :: x(:)
     real :: Xvar
        call label('var')
        x = [46.0, 69.0, 32.0, 60.0, 52.0, 41.0]
        call VAR(X,size(x),1,Xvar)
        write(*,*)merge('GOOD','BAD ',Xvar == 177.2), Xvar
     end program demo_var
