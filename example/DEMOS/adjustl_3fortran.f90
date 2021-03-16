           program demo_adjustl
           implicit none
           character(len=20) :: str = '   gfortran'
              str = adjustl(str)
              print *, str
           end program demo_adjustl
