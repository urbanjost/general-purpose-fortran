          program demo_base2
          use M_strings, only : base2
          implicit none
          integer                      :: i
          character(len=:),allocatable :: string
             write(*,'(a)') base2(huge(0))
             write(*,'(a)') base2(0)
             write(*,'(a)') base2(1-huge(0))
          end program demo_base2
