            program demo_len_trim
            implicit none
            character(len=:),allocatable :: string
               string=' how long is this string?     '
               write(*,*)'LENGTH=',len(string)
               write(*,*)'TRIMMED LENGTH=',len_trim(string)
               !
               ELE:block ! elemental example
               character(len=:),allocatable :: tablet(:)
               tablet=[character(len=256) :: &
               & ' how long is this string?     ',&
               & 'and this one?']
                  write(*,*)'LENGTH=',len(tablet)
                  write(*,*)'TRIMMED LENGTH=',len_trim(tablet)
                  write(*,*)'SUM TRIMMED LENGTH=',sum(len_trim(tablet))
               endblock ELE
               !
            end program demo_len_trim
