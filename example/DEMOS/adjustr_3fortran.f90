           program demo_adjustr
           implicit none
           character(len=20) :: str = 'gfortran'
              str = adjustr(str)
              print *, str
           end program demo_adjustr
