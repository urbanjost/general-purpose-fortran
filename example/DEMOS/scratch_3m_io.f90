           program demo_scratch
           use M_io, only : scratch
           implicit none
           write(*,*)'find good scratch file name candidates; one should test if writable'
           call printit('JUNK:')
           call printit('./')
           call printit('/var/tmp/')
           call printit('')
           call printit()
           contains
           subroutine printit(NAME)
           character(len=*),intent(in),optional :: NAME
           if(present(NAME))then
              write(*,'(a,t20,a)')NAME,scratch(NAME)
           else
              write(*,'(a,t20,a)')'*NOT PRESENT*',scratch()
           endif
           end subroutine printit
           end program demo_scratch
