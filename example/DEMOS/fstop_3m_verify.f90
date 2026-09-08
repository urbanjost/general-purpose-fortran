     program demo_fstop
     use M_verify, only: fstop
     implicit none
     integer :: int
     !*!write(*,*)'Enter stop value'
     !*!read(*,*) int
     int=25
     select case(int)
     case(10) ; call fstop(int)
     case(20) ; call fstop(int,stderr='error: program will now stop')
     case(25) ; call fstop(int,stdout='stdout message',stderr='stderr message')
     case(30) ; call fstop(int,stdout='error: program will now stop')
     case default
                call fstop(int)
     endselect

     end program demo_fstop
