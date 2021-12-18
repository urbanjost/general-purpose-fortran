            program demo_atan2d
            use M_units, only : atan2d
            real(4) :: x = 1.e0_4, y = 0.5e0_4
              write(*,*)atan2d(y,x)
            end program demo_atan2d
