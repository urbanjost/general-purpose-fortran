     program demo_system_realpath
     use M_system, only : system_realpath, system_perror
     implicit none
     ! resolve each pathname given on command line
     character(len=:),allocatable :: pathi,patho
     integer                      :: i
     integer                      :: filename_length
        do i = 1, command_argument_count()
           ! get pathname from command line arguments
           call get_command_argument (i , length=filename_length)
           if(allocated(pathi))deallocate(pathi)
           allocate(character(len=filename_length) :: pathi)
           call get_command_argument (i , value=pathi)
           !
           ! resolve each pathname
           patho=system_realpath(pathi)
           if(patho.ne.char(0))then
              write(*,*)trim(pathi),'=>',trim(patho)
           else
              call system_perror('*system_realpath* error for pathname '//trim(pathi)//':')
              write(*,*)trim(pathi),'=>',trim(patho)
           endif
           deallocate(pathi)
        enddo
        ! if there were no pathnames given resolve the pathname "."
        if(i.eq.1)then
           patho=system_realpath('.')
           write(*,*)'.=>',trim(patho)
        endif
     end program demo_system_realpath
