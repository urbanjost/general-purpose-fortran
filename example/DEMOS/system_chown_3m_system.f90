     program demo_system_chown
     use M_system, only : system_chown
     use M_system, only : system_getuid
     use M_system, only : system_getgid
     use M_system, only : system_perror
     implicit none
     integer                     :: i
     character(len=80),parameter :: names(*)=[&
             & character(len=80) :: &
             & 'myfile1',&
             & '/usr/local']
     do i=1,size(names)
        if(.not. system_chown(&
        & trim(names(i)),  &
        & system_getuid(), &
        & system_getgid()) &
           )then
           call system_perror('*demo_system_chown* '//trim(names(i)))
        endif
     enddo
     end program demo_system_chown
