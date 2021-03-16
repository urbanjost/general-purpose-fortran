           ! program demo_c_funloc and module
           module x
           use iso_c_binding
           implicit none
           contains
           subroutine sub(a) bind(c)
           real(c_float) :: a
              a = sqrt(a)+5.0
           end subroutine sub
           end module x
           !
           program demo_c_funloc
           use iso_c_binding
           use x
           implicit none
           interface
              subroutine my_routine(p) bind(c,name='myC_func')
                import :: c_funptr
                type(c_funptr), intent(in) :: p
              end subroutine
           end interface
              call my_routine(c_funloc(sub))
           !
           end program demo_c_funloc
