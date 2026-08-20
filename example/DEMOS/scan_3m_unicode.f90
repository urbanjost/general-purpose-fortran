     program demo_scan
     use iso_fortran_env, only : stdout => output_unit
     use M_unicode,       only : scan, unicode_type, assignment(=)
     use M_unicode,       only : ut=>unicode_type
     implicit none
     character(len=*),parameter :: g='(*(g0,1x))'
     type(ut)                   :: line
     type(ut)                   :: set
        !
        write(*,*) scan("fortran", "ao")          ! 2, found ’o’
        write(*,*) scan("fortran", "ao", .true.)  ! 6, found ’a’
        write(*,*) scan("fortran", "c++")         ! 0, found none
        !
        line='parsley😃sage😃rosemary😃😃thyme'
        set='😃'
        write(stdout,g) '12345678901234567890123456789012345678901234567890'
        write(stdout,g) line%character()
        write(stdout,g) scan(line, set)
        write(stdout,g) scan(line, set, back=.true.)
        write(stdout,g) scan(line, set, back=.false.)
        write(stdout,g) scan(line, unicode_type("NOT"))
        write(stdout,g) 'OOP'
        write(stdout,g) line%scan(set)
        write(stdout,g) line%scan(ut("o"))
     end program demo_scan
