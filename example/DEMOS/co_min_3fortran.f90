          program demo_co_min
          implicit none
          integer :: val
             val = this_image()
             call co_min(val, result_image=1)
             if (this_image() == 1) then
               write(*,*) "Minimal value", val  ! prints 1
             endif
          end program demo_co_min
