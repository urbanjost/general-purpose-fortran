           program demo_c_associated
           contains
           subroutine association_test(a,b)
           use iso_c_binding, only: c_associated, c_loc, c_ptr
           implicit none
           real, pointer :: a
           type(c_ptr) :: b
              if(c_associated(b, c_loc(a))) &
                 stop 'b and a do not point to same target'
           end subroutine association_test
           end program demo_c_associated
