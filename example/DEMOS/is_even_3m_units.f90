           program demo_is_even
           use M_units, only : is_even
              write(*,*)is_even(0)
              write(*,*)is_even(-1)
              write(*,*)is_even(-2)
              write(*,*)is_even(+1)
              write(*,*)is_even(+2)
              write(*,*)is_even([10,11,17,19,22])
              write(*,*)(is_even(i),i=-10,10)
           end program demo_is_even
