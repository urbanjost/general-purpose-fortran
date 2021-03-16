          program demo_co_max
          implicit none
          integer :: val
             val = this_image()
             call co_max(val, result_image=1)
             if (this_image() == 1) then
               write(*,*) "Maximal value", val  ! prints num_images()
             endif
          end program demo_co_max
