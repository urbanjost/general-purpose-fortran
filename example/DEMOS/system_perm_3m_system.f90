     program demo_system_perm
     use M_system, only : system_perm, system_stat
     use,intrinsic     :: iso_fortran_env, only : int64
     implicit none
     character(len=4096) :: string
     integer(kind=int64)     :: values(13)
     integer             :: ierr
     character(len=:),allocatable :: perms
        values=0
        ! get pathname from command line
        call get_command_argument(1, string)
        ! get pathname information
        call system_stat(string,values,ierr)
        if(ierr.eq.0)then
           ! convert permit mode to a string
           perms=system_perm(values(3))
           ! print permits as a string, decimal value, and octal value
           write(*,'("for ",a," permits[",a,"]",1x,i0,1x,o0)') &
            & trim(string),perms,values(3),values(3)
        endif
     end program demo_system_perm
