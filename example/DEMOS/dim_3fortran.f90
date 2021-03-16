           program demo_dim
           use, intrinsic :: iso_fortran_env, only : real_kinds, &
           & real32, real64, real128
           implicit none
           integer :: i
           real(kind=real64) :: x
               i = dim(4, 15)
               x = dim(4.345_real64, 2.111_real64)
               print *, i
               print *, x
           end program demo_dim
