     program demo_fileglob  ! simple unit test
        call tryit('*.*')
        call tryit('/tmp/__notthere.txt')
     contains

     subroutine tryit(string)
        use M_system, only : fileglob
        character(len=255),pointer :: list(:)
        character(len=*) :: string
        call fileglob(string, list)
        write(*,*)'Files:',size(list)
        write(*,'(a)')(trim(list(i)),i=1,size(list))
        deallocate(list)
     end subroutine tryit

     end program demo_fileglob  ! simple unit test
