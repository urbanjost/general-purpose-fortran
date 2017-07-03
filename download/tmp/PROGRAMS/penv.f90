subroutine help_usage(l_help)
implicit none
character(len=*),parameter     :: ident="@(#)help_usage(3f): prints help information"
logical,intent(in)             :: l_help
character(len=:),allocatable :: help_text(:)
integer                        :: i
logical                        :: stopit=.false.
stopit=.false.
if(l_help)then
help_text=[ CHARACTER(LEN=128) :: &
'NAME                                                                            ',&
'       penv(1f) - print all or part of environment in formats readable by various shells',&
'SYNOPSIS                                                                        ',&
'       penv [variable...] [-C|-B] [-p PREFIX] [-v]                              ',&
'       penv [--help|--version]                                                  ',&
'DESCRIPTION                                                                     ',&
'       If no arguments are given, penv(1f) prints the entire environment.       ',&
'       If one or more variable names are given, it prints the value of          ',&
'       each one that is set, and nothing for each one that is not set.          ',&
'                                                                                ',&
'       If the -p switch is used variables beginning with that prefix will be    ',&
'       displayed.                                                               ',&
'OPTIONS                                                                         ',&
'       -C           print output in a form that can be sourced into a           ',&
'                    C shell (eg. as a setenv(1) command).                       ',&
'       -B           print output in a form that can be sourced into a           ',&
'                    Bourne shell.                                               ',&
'       -p           print only variables with the given prefix                  ',&
'       -v           values only. Do not print variable names                    ',&
'       variable(s)  if variable names are given, print the value for            ',&
'                    each one that is set.                                       ',&
'       --help       display this help and exit                                  ',&
'       --version    output version information and exit                         ',&
'                                                                                ',&
'USAGE                                                                           ',&
'     Example commands:                                                          ',&
'                                                                                ',&
'      penv           # print entire environment                                 ',&
'      penv -p PREFIX # print environment variables beginning with PREFIX        ',&
'      penv HOME TMP LOGNAME USER # print selected variables                     ',&
'                                                                                ',&
'      # csh(1)/tcsh(1) example:                                                 ',&
'        % penv -C > readme.csh                                                  ',&
'        % source readme.csh                                                     ',&
'      # sh(1)/bash(1)/zsh(1) ... example:                                       ',&
'        $ penv -B > readme.sh                                                   ',&
'        $ . readme.sh                                                           ',&
'      # print specified variable names                                          ',&
'        % penv TMP TEMPDIR TMPDIR TEMP                                          ',&
'      # print variables starting with A or H                                    ',&
'        % penv -p A H                                                           ',&
'                                                                                ',&
'EXIT STATUS                                                                     ',&
'       The exit status is:                                                      ',&
'                                                                                ',&
'        0  if all variables specified were found                                ',&
'        1  otherwise                                                            ',&
'SEE ALSO                                                                        ',&
'       env(1), printenv(1)                                                      ',&
'']
   WRITE(*,'(a)')(trim(help_text(i)),i=1,size(help_text))
   stop ! if -help was specified, stop
endif
end subroutine help_usage
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!        penv(1f) - print all or part of environment in formats readable by various shells
!!##SYNOPSIS
!!
!!        penv [variable...] [-C|-B] [-p PREFIX] [-v]
!!        penv [--help|--version]
!!##DESCRIPTION
!!        If no arguments are given, penv(1f) prints the entire environment.
!!        If one or more variable names are given, it prints the value of
!!        each one that is set, and nothing for each one that is not set.
!!
!!        If the -p switch is used variables beginning with that prefix will be
!!        displayed.
!!##OPTIONS
!!        -C           print output in a form that can be sourced into a
!!                     C shell (eg. as a setenv(1) command).
!!        -B           print output in a form that can be sourced into a
!!                     Bourne shell.
!!        -p           print only variables with the given prefix
!!        -v           values only. Do not print variable names
!!        variable(s)  if variable names are given, print the value for
!!                     each one that is set.
!!        --help       display this help and exit
!!        --version    output version information and exit
!!
!!##USAGE
!!      Example commands:
!!
!!       penv           # print entire environment
!!       penv -p PREFIX # print environment variables beginning with PREFIX
!!       penv HOME TMP LOGNAME USER # print selected variables
!!
!!       # csh(1)/tcsh(1) example:
!!         % penv -C > readme.csh
!!         % source readme.csh
!!       # sh(1)/bash(1)/zsh(1) ... example:
!!         $ penv -B > readme.sh
!!         $ . readme.sh
!!       # print specified variable names
!!         % penv TMP TEMPDIR TMPDIR TEMP
!!       # print variables starting with A or H
!!         % penv -p A H
!!
!!##EXIT STATUS
!!        The exit status is:
!!
!!         0  if all variables specified were found
!!         1  otherwise
!!##SEE ALSO
!!        env(1), printenv(1)
!===================================================================================================================================
subroutine help_version(l_version)
implicit none
character(len=*),parameter     :: ident="@(#)help_version(3f): prints version information"
logical,intent(in)             :: l_version
character(len=:),allocatable   :: help_text(:)
integer                        :: i
logical                        :: stopit=.false.
stopit=.false.
if(l_version)then
help_text=[ CHARACTER(LEN=128) :: &
'@(#)PRODUCT:        CLI library utilities and examples>',&
'@(#)PROGRAM:        penv(1f)>',&
'@(#)DESCRIPTION:    Print values from the environment table>',&
'@(#)VERSION:        1.0 2016-11-27>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)REPORTING BUGS: http://www.urbanjost.altervista.org/>',&
'@(#)HOME PAGE:      http://www.urbanjost.altervista.org/index.html>',&
'@(#)COPYRIGHT:      Copyright (C) 2016 John S. Urban>',&
'@(#)COMPILED:       Wed, Jun 14th, 2017 10:05:06 AM>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if -version was specified, stop
endif
end subroutine help_version
!-----------------------------------------------------------------------------------------------------------------------------------
program penv
use M_kracken, only : kracken, lget, sget, sgets, IPvalue
use M_system, only : system_initenv, system_readenv, system_unsetenv, system_putenv
use M_system, only : system_clearenv
implicit none
character(len=:),allocatable       :: string
logical                            :: Csyntax, Bsyntax, printedsome, valuesonly, missing=.false.
integer                            :: i, ii, ilength, istatus, ierr
character(len=IPvalue),allocatable :: prefixes(:) ! assuming prefixes not greater than this length
character(len=IPvalue),allocatable :: names(:)    ! assuming names not greater than this length
character(len=:),allocatable       :: avalue
!-----------------------------------------------------------------------------------------------------------------------------------
   call kracken('penv',' -p -v .F. -C .F. -B .F. -help .F. -version .F. &
   & -unset -clear .F. -cmd                                            ')    ! crack command line arguments
   call help_usage(lget('penv_help'))                                        ! display help if -help specified and stop
   call help_version(lget('penv_version'))                                   ! display version if -version specified and stop
   Csyntax=lget('penv_C')                                                    ! if -C specified write output for csh(1)
   Bsyntax=lget('penv_B')                                                    ! if -B specified write output for sh(1)
   valuesonly=lget('penv_v')                                                 ! if -v set flag to print values only
   prefixes=sgets('penv_p')                                                  ! get list of prefixes of names to print

   printedsome=.false.                                                       ! flag if nothing else printed to print all values
!-----------------------------------------------------------------------------------------------------------------------------------
   if(lget('penv_clear'))then
      call system_clearenv()
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   names=sgets('penv_oo')                                                    ! get list of individual names (assumed no spaces)
   if(size(names).ne.0)then                                                  ! print variables specified by name
      do i=1,size(names)                                                     ! step thru names
            if(index(names(i),'=').eq.0)then
            call get_environment_variable(name=trim(names(i)),length=ilength,status=istatus)
            select case(istatus)
            case(0)                                                          ! variable is defined
               if(ilength.eq.0)then                                          ! value= apparently cannot have zero length
                  avalue=''
               else
                  allocate(character(len=ilength) :: avalue)                      ! make long enough to hold value
                  call get_environment_variable(name=trim(names(i)),value=avalue) ! get environment variable value by name
               endif
            case(-1)                                                         ! blank value
               avalue=' '
            case(1)                                                          ! name not found
               missing=.true.
               cycle
            case(2)                                                          ! environment variables not supported
               cycle
            case default                                                     ! unknown error
               cycle
            end select
            string=trim(names(i))//'='//avalue(:ilength)                     ! build string to same format system_readenv(3f) gets
            deallocate(avalue)                                               ! release scratch space
            call printformatted()                                            ! print input of form 'NAME=VALUE' as specified
            printedsome=.true.
         else                                                                ! if contains = assumes this is an assignment
            call system_putenv(trim(names(i)))
         endif
      enddo
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   names=sgets('penv_unset')                                                 ! get list of individual names (assumed no spaces)
   do i=1,size(names)                                                        ! step thru names and unset them
      call system_unsetenv(names(i),ierr)
   enddo
!-----------------------------------------------------------------------------------------------------------------------------------
   if(size(prefixes).ne.0)then                                               ! print variables with specified prefixes
      printedsome=.true.
      do i=1,size(prefixes)
         call system_initenv()
         do
            string=system_readenv()
            if(string.eq.'') exit
            if(index(string,trim(prefixes(i))).ne.1)cycle
            call printformatted()
         enddo
      enddo
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   if(.not.printedsome)then                                                  ! did not print specific values so print entire table
      call system_initenv()                                                  ! set to beginning of table
      do                                                                     ! iterate through environment table
         string=system_readenv()
         if(string.eq.'')exit                                                ! if a blank line is returned assume end reached
         call printformatted()                                               ! print variable
      enddo
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
  string=sget('penv_cmd')
  if(string.ne.'')then
     call execute_command_line(trim(string))
  endif
!-----------------------------------------------------------------------------------------------------------------------------------
   if(missing)then                                                           ! a specifically named variable was not found
      stop 1                                                                 ! return non-zero system exit status if supported
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
contains
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine printformatted()
   if(Bsyntax)then
      ii=index(string,'=')
      if(.not.valuesonly)write(*,'("export ''",a,"''=")',advance='no')string(1:ii-1)
      write(*,'(a)')printquoted_SH(string(ii+1:))
   elseif(Csyntax)then
      ii=index(string,'=')
      if(.not.valuesonly)write(*,'("setenv ",a)',advance='no')printquoted_CSH(string(1:ii-1))
      write(*,'(1x,a)')printquoted_CSH(string(ii+1:))
   else
      if(valuesonly)then
         ii=index(string,'=')
         write(*,'(a)')trim(string(ii+1:))
      else
         write(*,'(a)')trim(string)
      endif
   endif
end subroutine printformatted
!-----------------------------------------------------------------------------------------------------------------------------------
function printquoted_SH(string) result (quoted)
! print variable names and variable values with quotes and special escaping of ' for sh(1) shell
character(len=*),intent(in)    :: string
character(len=:),allocatable   :: quoted
   integer                     :: i
   character                   :: c
   quoted="'"
   do i=1,len(string)
      c=string(i:i)
      select case(c)
      case ("'")
         quoted=quoted//"'\''"
      case default
         quoted=quoted//c
      end select
   enddo
   quoted=quoted//"'"
end function printquoted_SH
!-----------------------------------------------------------------------------------------------------------------------------------
function printquoted_CSH(string) result (quoted)
! print variable names and variable values with quotes and special escaping of ' and ! for csh(1) shell
character(len=*),intent(in)    :: string
character(len=:),allocatable   :: quoted
   integer                     :: i
   character                   :: c
   quoted="'"
   do i=1,len(string)
      c=string(i:i)
      select case(c)
      case ("'")
         quoted=quoted//"'\''"
      case ('!')
         quoted=quoted//"\!"
      case default
         quoted=quoted//c
      end select
   enddo
   quoted=quoted//"'"
end function printquoted_CSH
!-----------------------------------------------------------------------------------------------------------------------------------
end program penv
