            program demo_len
            implicit none
            character(len=:),allocatable :: string
               string=' how long is this string?     '
               write(*,*)'LENGTH=',len(string)
               write(*,*)'TRIMMED LENGTH=',len_trim(string)
            end program demo_len
