     program demo_system_getenv
     use M_system, only : system_getenv
     use M_system, only : ge=>system_getenv
     implicit none
     character(len=:),allocatable :: TMPDIR

        write(*,'("USER     : ",a)')system_getenv('USER')
        write(*,'("LOGNAME  : ",a)')system_getenv('LOGNAME')
        write(*,'("USERNAME : ",a)')system_getenv('USERNAME')

        ! look first for USER then LOGNAME then USERNAME
        write(*, *)ge('USER', ge('LOGNAME', ge('USERNAME', 'UNKNOWN')))

        TMPDIR= ge('TMPDIR', ge('TMP', ge('TEMPDIR', ge('TEMP', '/tmp'))))
        write(*,*)'favorite scratch area is ',TMPDIR

    end program demo_system_getenv
