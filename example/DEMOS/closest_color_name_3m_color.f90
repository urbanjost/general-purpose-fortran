          program demo_closest_color_name
          use M_color, only : closest_color_name
          character(len=100) :: string ! at least 20 characters
             string=' '

             call closest_color_name(100.0,  0.0,  0.0,string)
             write(*,*)trim(string)

             call closest_color_name(  0.0,100.0,  0.0,string)
             write(*,*)trim(string)

             call closest_color_name(  0.0,  0.0,100.0,string)
             write(*,*)trim(string)

          end program demo_closest_color_name
