          program demo_event_query
            use iso_fortran_env
            implicit none
            type(event_type) :: event_value_has_been_set[*]
            integer :: cnt
            if (this_image() == 1) then
              call event_query(event_value_has_been_set, cnt)
              if (cnt > 0) write(*,*) "Value has been set"
            elseif (this_image() == 2) then
              event post(event_value_has_been_set[1])
            endif
          end program demo_event_query
