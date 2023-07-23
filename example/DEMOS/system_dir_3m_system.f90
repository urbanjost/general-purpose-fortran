     program demo_system_dir
     use M_system, only : system_dir, system_isdir
     implicit none
     character(len=:),allocatable :: dirname
        write(*, '(a)')system_dir(pattern='*.f90')
        dirname='/tmp'
        if(system_isdir(dirname))then
           write(*, '(a)')system_dir(pattern='*.f90')
        else
           write(*, '(a)')'<WARNING:>'//dirname//' does not exist'
        endif
     end program demo_system_dir
