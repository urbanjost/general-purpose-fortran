     program demo_slurp
     use M_unicode, only : slurp, ut=>unicode_type
     use M_unicode, only : add_backslash, escape
     use M_unicode, only : assignment(=)
     implicit none
     type(ut),allocatable         :: text(:)
     integer                      :: i
     character(len=:),allocatable :: iomsg
     character(len=*),parameter   :: FILENAME='._inputfile'

     call create_test_file()

     text=slurp(FILENAME,iomsg=iomsg)

     if(iomsg.ne.'')then
        write(*,*)'*demo_slurp* failed to load file '//FILENAME
        write(*,*) iomsg
     else
        ! write out slurped data
        call write_text()

        ! encode with escape sequences and write data again
        do i=1,size(text)
           text(i)=add_backslash(text(i))
        enddo
        call write_text()

        ! deencode escape sequences and write data again
        do i=1,size(text)
           text(i)=escape(text(i))
        enddo
        call write_text()

          ! teardown
        deallocate(text)  ! release memory
        open(file=FILENAME,unit=10)
        close(unit=10,status='delete')
     endif
     contains
     subroutine write_text()
        write(*,'(a)')repeat('=',80)
        write(*,'(*(a:))')(text(i)%character(),new_line('a'),i=1,size(text))
     end subroutine write_text

     subroutine create_test_file()
     ! create test file
     open(file=FILENAME,unit=10,action='write')
     ! (Used by Microsoft Office as sample text for Croatian language.)
     write( *,'(a)')'Croation pangram:'
     write( *,'(a)')''
     write(10,'(a)')'Gojazni đačić s biciklom drži hmelj i finu'
     write(10,'(a)')'vatu u džepu nošnje.'
     write(10,'(a)')''
     write( *,'(a)')'The overweight little schoolboy with a bike is holding'
     write( *,'(a)')'hops and fine cotton in the pocket of his attire.'
     close(unit=10)
     end subroutine create_test_file

     end program demo_slurp
