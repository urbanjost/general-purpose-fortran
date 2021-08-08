            program demo_value_to_string
            use M_strings, only: value_to_string
            implicit none
            character(len=80) :: string
            integer           :: lgth
               call value_to_string(3.0/4.0,string,lgth)
               write(*,*) 'The value is [',string(:lgth),']'

               call value_to_string(3.0/4.0,string,lgth,fmt='')
               write(*,*) 'The value is [',string(:lgth),']'

               call value_to_string&
               &(3.0/4.0,string,lgth,fmt='("THE VALUE IS ",g0)')
               write(*,*) 'The value is [',string(:lgth),']'

               call value_to_string(1234,string,lgth)
               write(*,*) 'The value is [',string(:lgth),']'

               call value_to_string(1.0d0/3.0d0,string,lgth)
               write(*,*) 'The value is [',string(:lgth),']'

            end program demo_value_to_string
