         program demo_trimzeros_
         use M_unicode, only : trimzeros_
         character(len=:),allocatable :: string
            string= '123.450000000000'
            call trimzeros_(string)
            write(*,*)string
            string='12345'
            call trimzeros_(string)
            write(*,*)string
            string='12345.'
            call trimzeros_(string)
            write(*,*)string
            string='12345.00e3'
            call trimzeros_(string)
            write(*,*)string
         end program demo_trimzeros_
