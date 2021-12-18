     program demo_system_remove
     use M_system, only : system_remove
     character(len=*),parameter :: FILE='MyJunkFile.txt'
     integer :: ierr
     write(*,*)'BEFORE CREATED '//FILE
     call execute_command_line('ls -l '//FILE)
     write(*,*)

     ! note intentionally causes error if file exists
     open(unit=10,file=FILE,status='NEW')
     write(*,*)'AFTER OPENED '//FILE
     call execute_command_line('ls -l '//FILE)
     write(*,*)

     write(10,'(a)') 'This is a file I want to delete'
     close(unit=10)
     write(*,*)'AFTER CLOSED '
     call execute_command_line('ls -l '//FILE)
     write(*,*)

     ierr=system_remove(FILE)
     write(*,*)'AFTER REMOVED',IERR
     call execute_command_line('ls -l '//FILE)
     write(*,*)

     end program demo_system_remove
