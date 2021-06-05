           program demo_c_f_procpointer
           use iso_c_binding
           implicit none
           abstract interface
              function func(a)
              import :: c_float
              real(c_float), intent(in) :: a
              real(c_float) :: func
              end function
           end interface
           interface
              function getIterFunc() bind(c,name="getIterFunc")
              import :: c_funptr
              type(c_funptr) :: getIterFunc
              end function
           end interface
           type(c_funptr) :: cfunptr
           procedure(func), pointer :: myFunc
              cfunptr = getIterFunc()
              call c_f_procpointer(cfunptr, myFunc)
           end program demo_c_f_procpointer
