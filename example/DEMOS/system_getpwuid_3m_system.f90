          program demo_system_getpwuid
          use M_system, only : system_getpwuid
          use M_system, only : system_getuid
          use,intrinsic     :: iso_fortran_env, only : int64
          implicit none
          character(len=:),allocatable :: name
          integer(kind=int64)              :: uid
             uid=system_getuid()
             name=system_getpwuid(uid)
             write(*,'("login[",a,"] has UID ",i0)')name,uid
          end program demo_system_getpwuid
