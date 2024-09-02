     program demo_system_dir
     use M_system, only : system_dir, system_isdir
     implicit none
     character(len=:),allocatable :: dirname
        write(*, '("*.f90: ",a)')system_dir(pattern='*.f90')
        write(*, '("*.F90: ",a)')system_dir(pattern='*.F90')
        write(*, '("ignorecase:*.F90: ",a)')system_dir(pattern='*.F90',ignorecase=.true.)
        write(*, '("ignorecase:*.f90: ",a)')system_dir(pattern='*.F90',ignorecase=.true.)
        dirname='/tmp'
        if(system_isdir(dirname))then
           write(*, '("/tmp/*.f90: ",a)')system_dir(directory='/tmp',pattern='*.f90')
        else
           write(*, '(a)')'<WARNING:>'//dirname//' does not exist'
        endif
     end program demo_system_dir
