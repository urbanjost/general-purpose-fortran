           program demo_allocated
           implicit none
           integer :: i = 4
           real(4), allocatable :: x(:)
              if (allocated(x) .eqv. .false.) allocate(x(i))
           end program demo_allocated
