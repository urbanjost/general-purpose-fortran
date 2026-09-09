     program demo_system_uname
        use M_system, only : system_uname
        implicit none
        integer,parameter          :: is=100
        integer                    :: i
        character(len=*),parameter :: letters='srvnmxT'
        character(len=is)          :: string=' '

        do i=1,len(letters)
           write(*,'(80("="))')
           call system_uname(letters(i:i),string)
           write(*,*)&
           &'=====> TESTING system_uname('//letters(i:i)//')--->'//trim(string)
        enddo

     end program demo_system_uname
