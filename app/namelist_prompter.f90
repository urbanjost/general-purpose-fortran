!# Using a NAMELIST group to create an interactive prompt for variables by name
!
!NAMELIST input has some underutilized uses. Unlike similar file formats
!it is built into the standard, allows multiple sets in a single file
!which it searches sequentially for by name, and ignores lines in the
!file not in a NAMELIST group format. One perhaps unexpected use is
!to let you simulate exposing variables in the program for the user to
!change interactively.
!
!Taking advantage of NAMELIST reads not requiring all values to be specified,
!it takes very little code to make an interactive prompt for values of the form
!
!    NAME=VALUE(S)
!
!For example, the following relatively short program shows placing a number of
!variables into a NAMELIST and then letting you interactively change them with
!a session looking something like:
!
!    args>>show
!    args>>f='courier' t='new title'
!    args>>view=1,2,3
!    args>>a=456.789
!    args>>! run with new values
!    args>> .
!    args>>h=t
!    args>>! run again
!    args>> .
!    args>>stop
!
!~~~~~~~~~~ {: lang=fortran}
program namelist_prompter
   implicit none
   ! create a NAMELIST group with lots of options
   ! this is just a sample
   real :: a=0.0
   real :: view(3)=[0.0,0.0,0.0]
   character(len=80) :: t='title'
   character(len=80) :: f='roman'
   logical :: h=.false.
   namelist /args/ a,view,t,h,f

   character(len=:),allocatable :: status
   do
      call readargs(status) ! interactively change NAMELIST group
      if(status.eq.'stop')exit
      call dosomething() ! use the NAMELIST values
   enddo
contains
   subroutine readargs(status)
      character(len=:),intent(out),allocatable :: status
      character(len=256) :: line
      character(len=256) :: answer
      integer            :: lun
      integer            :: ios
      status=''
      write(*,'(a)')'args>> "." to run, "stop" to end, "show" to show keywords, "read","write","sh"'
      do
         write(*,'(a)',advance='no')'args>>'
         read(*,'(a)')line
         if(line(1:1).eq.'!')cycle
         select case(line)
          case('.')
            exit
          case('show')
            write(*,*)'SO FAR'
            write(*,nml=args)
            !! something where you could restrict nml output to just listed names would be nice
            !!write(*,nml=args)['A','H']
            !!write(*,nml=*NML)args['A','H']
          case('stop')
            status='stop'
            exit
          case('sh')
             call execute_command_line('bash')
          case('read')
             write(*,'(a)',advance='no')'filename:'
             read(*,'(a)',iostat=ios)answer
             if(ios.ne.0)exit
             open(file=answer,iostat=ios,newunit=lun)
             if(ios.ne.0)exit
             read(lun,args,iostat=ios)
             close(unit=lun,iostat=ios)
          case('write')
             write(*,'(a)',advance='no')'filename:'
             read(*,'(a)',iostat=ios)answer
             if(ios.ne.0)exit
             open(file=answer,iostat=ios,newunit=lun)
             if(ios.ne.0)exit
             write(lun,args,iostat=ios)
             close(unit=lun,iostat=ios)
          case default
            UPDATE: block
               character(len=:),allocatable :: intmp
               character(len=256)  :: message
               integer :: ios
               intmp='&ARGS '//trim(line)//'/'
               read(intmp,nml=args,iostat=ios,iomsg=message)
               if(ios.ne.0)then
                  write(*,*)'ERROR:',trim(message)
               endif
            endblock UPDATE
         end select
      enddo
   end subroutine readargs
   subroutine dosomething()
      ! placeholder
      write(*,*)'USE ALL THOSE VALUES'
   end subroutine dosomething
end program namelist_prompter
!~~~~~~~~~~
!category: code
! would be nice if could do partial NAMELIST (write(*,nml=args) a,view
