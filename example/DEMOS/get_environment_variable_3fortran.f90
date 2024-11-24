      program demo_getenv
      implicit none
      character(len=:),allocatable :: homedir
      character(len=:),allocatable :: var

           var='HOME'
           homedir=get_env(var)
           write (*,'(a,"=""",a,"""")')var,homedir

      contains

      function get_env(name,default) result(value)
      ! a function that makes calling get_environment_variable(3) simple
      use, intrinsic :: iso_fortran_env, only : stderr=>ERROR_UNIT
      implicit none
      character(len=*),intent(in)          :: name
      character(len=*),intent(in),optional :: default
      character(len=:),allocatable         :: value
      integer                              :: howbig
      integer                              :: stat
      integer                              :: length
         length=0
         value=''
         if(name.ne.'')then
            call get_environment_variable( name, &
            & length=howbig,status=stat,trim_name=.true.)
            select case (stat)
            case (1)
             write(stderr,*) &
             & name, " is not defined in the environment. Strange..."
             value=''
            case (2)
             write(stderr,*) &
             & "This processor does not support environment variables. Boooh!"
             value=''
            case default
             ! make string of sufficient size to hold value
             if(allocated(value))deallocate(value)
             allocate(character(len=max(howbig,1)) :: value)
             ! get value
             call get_environment_variable( &
             & name,value,status=stat,trim_name=.true.)
             if(stat.ne.0)value=''
            end select
         endif
         if(value.eq.''.and.present(default))value=default
      end function get_env

      end program demo_getenv
