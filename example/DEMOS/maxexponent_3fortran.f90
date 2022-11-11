        program demo_maxexponent
        use,intrinsic :: iso_fortran_env, only : dp=>real64,sp=>real32
        implicit none
        real(kind=sp) :: x
        real(kind=dp) :: y

           print *, minexponent(x), maxexponent(x)
           print *, minexponent(y), maxexponent(y)
        end program demo_maxexponent
