          program demo_system_getlogin
          use M_system, only : system_getlogin
          implicit none
          character(len=:),allocatable :: name
          name=system_getlogin()
          write(*,'("login[",a,"]")')name
          end program demo_system_getlogin
