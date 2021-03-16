           program demo_present
           implicit none
             write(*,*) f(), f(42)      ! "f t"
           contains
             logical function f(x)
               integer, intent(in), optional :: x
               f = present(x)
             end function
           end program demo_present
