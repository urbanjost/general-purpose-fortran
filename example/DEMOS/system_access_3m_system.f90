            program demo_system_access
            use M_system, only : system_access, F_OK, R_OK, W_OK, X_OK
            implicit none
            integer                     :: i
            character(len=80),parameter :: names(*)=[ &
            '/usr/bin/bash   ', &
            '/tmp/NOTTHERE   ', &
            '/usr/local      ', &
            '.               ', &
            'PROBABLY_NOT    ']
            do i=1,size(names)
               write(*,*)' does ',trim(names(i)),' exist?    ', system_access(names(i),F_OK)
               write(*,*)' is ',trim(names(i)),' readable?     ', system_access(names(i),R_OK)
               write(*,*)' is ',trim(names(i)),' writable?     ', system_access(names(i),W_OK)
               write(*,*)' is ',trim(names(i)),' executable?   ', system_access(names(i),X_OK)
            enddo
            end program demo_system_access
