            program demo_value_to_string
            use M_strings, only: value_to_string
            implicit none
            character(len=80) :: string
            integer           :: ilen
               call value_to_string(3.0/4.0,string,ilen)
               write(*,*) 'The value is [',string(:ilen),']'

               call value_to_string(3.0/4.0,string,ilen,fmt='')
               write(*,*) 'The value is [',string(:ilen),']'

               call value_to_string&
               &(3.0/4.0,string,ilen,fmt='("THE VALUE IS ",g0)')
               write(*,*) 'The value is [',string(:ilen),']'

               call value_to_string(1234,string,ilen)
               write(*,*) 'The value is [',string(:ilen),']'

               call value_to_string(1.0d0/3.0d0,string,ilen)
               write(*,*) 'The value is [',string(:ilen),']'

            end program demo_value_to_string
