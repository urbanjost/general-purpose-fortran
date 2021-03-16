          program demo_getenv
          implicit none
          character(len=:),allocatable :: var
          character(len=:),allocatable :: homedir
          integer :: howbig, stat
            var='HOME'
            ! get length required to hold value
            call get_environment_variable(var, length=howbig,status=stat)
            select case (stat)
            case (1)
               print *, "HOME is not defined in the environment. Strange..."
            case (2)
               print *, "This processor doesn't support environment variables. Boooh!"
            case default
               ! make string to hold value of sufficient size
               allocate(character(len=howbig) :: homedir)
               ! get value
               call get_environment_variable(var, homedir)
               ! print environment variable name value
               write (*,'(a,"=""",a,"""")')var,trim(homedir)
            end select
          end program demo_getenv
