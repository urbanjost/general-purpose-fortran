          program demo_log
          implicit none
            real(kind(0.0d0)) :: x = 2.71828182845904518d0
            complex :: z = (1.0, 2.0)
            x = log(x)    ! will yield (approximately) 1
            z = log(z)
          end program demo_log
