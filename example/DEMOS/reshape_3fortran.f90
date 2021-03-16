           program demo_reshape
           implicit none
           integer :: i
           integer, dimension(4) :: x=[(i,i=10,40,10)]
             ! X is originally a vector with four elements
             write(*,*) shape(x)                     ! prints "4"
             write(*,*) shape(reshape(x, [2, 2]))    ! prints "2 2"
           end program demo_reshape
