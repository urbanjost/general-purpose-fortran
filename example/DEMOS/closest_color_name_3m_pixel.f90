              program demo_closest_color_name
              use M_pixel, only : closest_color_name
              implicit none
              character(len=100) :: string ! at least 20 characters
                 string=' '

                 call closest_color_name(100.0,  0.0,  0.0,string)
                 write(*,*)trim(string)

                 call closest_color_name(  0.0,100.0,  0.0,string)
                 write(*,*)trim(string)

                 call closest_color_name(  0.0,  0.0,100.0,string)
                 write(*,*)trim(string)

              end program demo_closest_color_name
