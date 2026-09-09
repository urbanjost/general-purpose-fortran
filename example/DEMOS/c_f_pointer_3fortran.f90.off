      program demo_c_f_pointer
      use iso_c_binding
      implicit none
      interface
         subroutine my_routine(p) bind(c,name='myC_func')
            import :: c_ptr
            type(c_ptr), intent(out) :: p
         end subroutine
      end interface
      type(c_ptr) :: cptr
      real,pointer :: a(:)
         call my_routine(cptr)
         call c_f_pointer(cptr, a, [12])
      end program demo_c_f_pointer
