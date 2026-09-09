     program demo_dot
     use M_datapac, only : dot, label
     real, dimension(3) :: a, b
     real :: dotpro , parpro
     integer i , imax , imin
        call label('dot')
        a = [ 1.0, 2.0, 3.0 ]
        b = [ 4.0, 5.0, 6.0 ]
        imin=1
        imax=size(a)
        parpro=0.0
        call dot(a,b,imin,imax,parpro,dotpro)
        write(*,*)a
        write(*,*)b
        write(*,*)dotpro, dot_product(a,b), dotpro == dot_product(a,b)
     end program demo_dot
