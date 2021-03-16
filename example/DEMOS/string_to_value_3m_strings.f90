          program demo_string_to_value
           use M_strings, only: string_to_value
           implicit none
           real :: value
           integer :: ierr
           character(len=80) :: string
              string=' -40.5e-2 '
              call string_to_value(string,value,ierr)
              write(*,*) 'value of string ['//trim(string)//'] is ',value
          end program demo_string_to_value
