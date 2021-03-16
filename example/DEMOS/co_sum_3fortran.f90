          program demo_co_sum
          implicit none
          integer :: val
             val = this_image()
             call co_sum(val, result_image=1)
             if (this_image() == 1) then
                ! prints (n**2 + n)/2, with n = num_images()
                write(*,*) "The sum is ", val
             endif
          end program demo_co_sum
