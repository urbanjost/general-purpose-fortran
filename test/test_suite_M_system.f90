program runtest
use M_verify, only : unit_check_command, unit_check_keep_going, unit_check_level, unit_check_stop
implicit none
   unit_check_command=''
   unit_check_keep_going=.true.
   unit_check_level=0
   call test_suite_M_system_tests()
   call unit_check_stop()
end program runtest

subroutine test_suite_M_system_tests()
use,intrinsic :: iso_c_binding,   only : c_int32_t, c_int, c_ptr, c_size_t, c_short, c_float, c_char, c_null_char
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64, real32, real64, real128
use M_msg,     only : str
use M_verify,   only : unit_check, unit_check_start, unit_check_good, unit_check_bad, unit_check_done
use M_verify,   only : unit_check_msg
use M_verify,   only : unit_check_level
use M_system
use M_process, only : process_readall
use M_time,    only : fmtdate, u2d
integer :: ierr
!! setup
   ierr=system_rmdir('fort.10')
   ierr=system_rmdir('_test1')
   ierr=system_rmdir('_test2')
   ierr=system_rmdir('_test3')

call unit_check_msg('M_system','try to test OS interface routines, given difficulty of trying to test')
call unit_check_msg('M_system','functions not intrinsically part of Fortran and system-dependent.')
call unit_check_msg('M_system','Many assumptions are made, including assuming a GNU Linux/Unix system.')
call unit_check_msg('M_system','Examine the tests on other platforms, as it may well be the assumptions made')
call unit_check_msg('M_system','about the system and not the routines that are generating an error.')
call test_set_environment_variable()
call test_system_rename()
call test_system_getlogin()
call test_system_geteuid()
call test_system_getegid()
call test_system_getgid()
call test_system_getuid()
call test_system_getpid()
call test_system_getppid()
call test_system_isdir()
call test_system_chdir()
call test_system_rmdir()
call test_system_mkdir()
call test_system_opendir()
call test_system_readdir()
call test_system_rewinddir()
call test_system_closedir()
call test_system_putenv()
call test_system_unsetenv()
call test_system_getenv()
call test_system_initenv()
call test_system_readenv()
call test_system_remove()
call test_system_getcwd()

   call test_system_dir()
   call test_system_clearenv()
   call test_system_access()
   call test_system_chmod()
   call test_system_chown()
   call test_system_cpu_time()
   call test_system_errno()
   call test_system_getgrgid()
   call test_system_gethostname()
   call test_fileglob()

   call test_system_getpwuid()
   call test_system_getsid()
   call test_system_setsid()
   call test_system_getumask()
   call test_system_isblk()
   call test_system_ischr()
   call test_system_isfifo()
   call test_system_islnk()
   call test_system_isreg()
   call test_system_issock()
   call test_system_kill()
   call test_system_link()
   call test_system_mkfifo()
   call test_system_perm()
   call test_system_perror()
   call test_system_rand()
   call test_system_srand()
   call test_system_realpath()
   call test_system_setumask()
   call test_system_stat()
   !-!call test_system_stat_print()
   call test_system_uname()
   call test_system_unlink()
   call test_system_utime()
   call test_system_memcpy()
!! teardown
   ierr=system_rmdir('fort.10')
   ierr=system_rmdir('_test1')
   ierr=system_rmdir('_test2')
   ierr=system_rmdir('_test3')
contains
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
!-!subroutine test_system_stat_print()
!-!   call unit_check_start('system_stat_print',msg='')
!-!   call system_stat_print('/tmp')
!-!   call system_stat_print('/etc/hosts')
!-!   !!call unit_check('system_stat_print', 0.eq.0, 'checking',100)
!-!   call unit_check_done('system_stat_print',msg='')
!-!end subroutine test_system_stat_print
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_system_srand()
integer :: i,j
   do j=1,2
      call system_srand(1001)
      do i=1,10
         write(*,*)system_rand()
      enddo
      write(*,*)
   enddo
   call unit_check_start('system_srand',msg='')
   !!call unit_check('system_srand', 0.eq.0, 'checking',100)
   call unit_check_done('system_srand',msg='')
end subroutine test_system_srand
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_system_kill()
integer           :: i,pid,ios,ierr,signal=9
character(len=80) :: argument

   do i=1,command_argument_count()
! get arguments from command line
      call get_command_argument(i, argument)
! convert arguments to integers assuming they are PID numbers
      read(argument,'(i80)',iostat=ios) pid
      if(ios.ne.0)then
         write(*,*)'bad PID=',trim(argument)
      else
         write(*,*)'kill SIGNAL=',signal,' PID=',pid
! send signal SIGNAL to pid PID
         ierr=system_kill(pid,signal)
! write message if an error was detected
         if(ierr.ne.0)then
            call system_perror('*test_system_kill*')
         endif
      endif
   enddo
   call unit_check_start('system_kill',msg='')
   !!call unit_check('system_kill', 0.eq.0, 'checking',100)
   call unit_check_done('system_kill',msg='')
end subroutine test_system_kill
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_system_errno()
integer :: stat
   stat=system_unlink('not there/OR/anywhere')
   if(stat.ne.0)then
      write(*,*)'err=',system_errno()
      call system_perror('*test_system_errno*')
   endif
   call unit_check_start('system_errno',msg='')
   !!call unit_check('system_errno', 0.eq.0, 'checking',100)
   call unit_check_done('system_errno',msg='')
end subroutine test_system_errno
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_system_geteuid()
                     integer :: ierr
character(len=:),allocatable :: string
integer                      :: uid_command
integer                      :: uid
   call unit_check_start('system_geteuid',msg='check using command "id -u"')
   string=process_readall('id -u',ierr=ierr)
   !!call unit_check('system_geteuid', ierr.eq.0, 'using command "id -u" ierr=',ierr,'effective UID=',string)
   call unit_check('system_geteuid', string.ne.' ', 'using command "id -u" ierr=',ierr,'effective UID=',string)
   uid=system_geteuid();
   if(string.ne.'')then
      read(string,*)uid_command
      call unit_check('system_geteuid', uid.eq.uid_command, 'uid=',uid)
      call unit_check_done('system_geteuid',msg='')
   else
      call unit_check_bad('system_geteuid', msg=str(' assuming bad because system command did not work. uid=',uid))
   endif
end subroutine test_system_geteuid
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_system_getuid()
integer                      :: ierr
character(len=:),allocatable :: string
integer                      :: uid_command
integer                      :: uid
integer                      :: ios
   call unit_check_start('system_getuid',msg='check using command "id -u -r"')
   string=process_readall('id -u -r',ierr=ierr)
   !!call unit_check('system_getuid', ierr.eq.0, 'using command "id -u -r" ierr=',ierr,'UID=',string)
   call unit_check('system_getuid', string.ne.' ', 'using command "id -u -r" ierr=',ierr,'UID=',string)
   uid=system_getuid();
   if(string.ne.' ')then
      read(string,*,iostat=ios)uid_command
      call unit_check('system_getuid', ios.eq.0, 'read uid=',uid_command)
      call unit_check('system_getuid', uid.eq.uid_command, 'uid=',uid)
      call unit_check_done('system_getuid',msg='')
   else
      call unit_check_bad('system_getuid', msg=str(' assuming bad because system command did not work. uid=',uid))
   endif
end subroutine test_system_getuid
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_system_getegid()
integer                      :: ierr
character(len=:),allocatable :: string
integer                      :: gid_command
integer                      :: gid
character(len=*),parameter   :: cmd='id -g'
   call unit_check_start('system_getegid','check using command',cmd)
   string=process_readall(cmd,ierr=ierr)
   !!call unit_check('system_getegid', string.ne.' ', 'using command "',cmd,'" ierr=',ierr,'GID=',string)
   gid=system_getegid();
   if(string.ne.' ')then
      read(string,*)gid_command
      call unit_check('system_getegid', gid.eq.gid_command, 'gid=',gid)
      call unit_check_done('system_getegid',msg='')
   else
      call unit_check_bad('system_getegid', msg=str(' assuming bad because system command did not work. gid=',gid))
   endif
end subroutine test_system_getegid
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_system_getgid()
integer                      :: ierr
character(len=:),allocatable :: string
integer                      :: gid_command
integer                      :: gid
character(len=*),parameter   :: cmd='id -g -r'
   call unit_check_start('system_getgid','check using command',cmd)
   string=process_readall(cmd,ierr=ierr)
   !!call unit_check('system_getgid', ierr.eq.0, 'using command "',cmd,'" ierr=',ierr,'GID=',string)
   call unit_check('system_getgid', string.ne.' ', 'using command "',cmd,'" ierr=',ierr,'GID=',string)
   gid=system_getgid();
   if(string.ne.' ')then
      read(string,*)gid_command
      call unit_check('system_getgid', gid.eq.gid_command, 'gid=',gid)
      call unit_check_done('system_getgid',msg='')
   else
      call unit_check_bad('system_getgid', msg=str(' assuming bad because system command did not work. gid=',gid))
   endif
end subroutine test_system_getgid
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_system_getsid()
!!integer                      :: ierr
!!character(len=:),allocatable :: string
!!integer                      :: sid_command
integer                      :: sid
character(len=*),parameter   :: cmd='UNKNOWN'
   call unit_check_start('system_getsid','check using command',cmd)
!!   string=process_readall(cmd,ierr=ierr)
!!   call unit_check('system_getsid', ierr.eq.0, 'using command "',cmd,'" ierr=',ierr,'sid=',string)
   sid=system_getsid(0_c_int);
!!   if(string.ne.' ')then
!!      read(string,*)sid_command
!!      call unit_check('system_getsid', sid.eq.sid_command, 'sid=',sid)
      call unit_check_done('system_getsid',msg='')
!!   else
!!      call unit_check_bad('system_getsid', msg=str(' assuming bad because system command did not work. sid=',sid))
!!   endif
end subroutine test_system_getsid
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_system_setsid()
integer                      :: pid
   call unit_check_start('system_setsid')
   pid=system_setsid();
   !!call unit_check('system_setsid', pid.ge.0, 'just checking PID>0 pid=',pid)
   call unit_check_done('system_setsid',msg='')
end subroutine test_system_setsid
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_system_getpid()
integer                      :: pid
   call unit_check_start('system_getpid','PID (process ID) of current process')
   pid=system_getpid();
   call unit_check('system_getpid', pid.ge.0, 'just checking PID>0 pid=',pid)
   call unit_check_done('system_getpid',msg='')
end subroutine test_system_getpid
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_system_getppid()
integer                      :: ppid
call unit_check_start('system_getppid','only make sure call does not work and returns value >0')
   ppid=system_getppid();
   call unit_check('system_getppid', ppid.ge.0, 'ppid=',ppid)
   call unit_check_done('system_getppid',msg='')
end subroutine test_system_getppid
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_system_rand()
integer :: i

   call system_srand(1001)
   do i=1,10
      write(*,*)system_rand()
   enddo
   write(*,*)

   call unit_check_start('system_rand',msg='')
   !!call unit_check('system_rand', 0.eq.0, 'checking',100)
   call unit_check_done('system_rand',msg='')
end subroutine test_system_rand
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_system_initenv()
character(len=:),allocatable :: string
integer                      :: i
integer                      :: ierr
character(len=:),allocatable :: home
character(len=4096)          :: envname
   call unit_check_start('system_initenv',msg='assuming system always has environment variable HOME set')
   i=0
   home=''
   ! read environment table and look for HOME= at beginning of line
   call system_initenv()
   do
      string=system_readenv()
      if(index(string,'HOME=').eq.1)then
        home=string
      endif
      if(string.eq.'')then
         exit
      else
         i=i+1
      endif
   enddo
   call get_environment_variable("HOME",value=envname, status=ierr)
   envname='HOME='//trim(envname)
   call unit_check('system_initenv',home.eq.envname, 'HOME',home,envname)
   call unit_check_done('system_initenv',msg='')
end subroutine test_system_initenv
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_system_realpath()
! resolve each pathname given on command line
character(len=:),allocatable :: pathi,patho
integer                      :: i
integer                      :: filename_length
   do i = 1, command_argument_count()
! get pathname from command line arguments
      call get_command_argument (i , length=filename_length)
      allocate(character(len=filename_length) :: pathi)
      call get_command_argument (i , value=pathi)
!
! resolve each pathname
      patho=system_realpath(pathi)
      if(system_errno().eq.0)then
         write(*,*)trim(pathi),'=>',trim(patho)
      else
         call system_perror('*system_realpath* error for pathname '//trim(pathi)//':')
         write(*,*)trim(pathi),'=>',trim(patho)
      endif
      deallocate(pathi)
   enddo
! if there were no pathnames give resolve the pathname "."
   if(i.eq.1)then
      patho=system_realpath('.')
      write(*,*)'.=>',trim(patho)
   endif
   call unit_check_start('system_realpath',msg='')
   !!call unit_check('system_realpath', 0.eq.0, 'checking',100)
   call unit_check_done('system_realpath',msg='')
end subroutine test_system_realpath
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_fileglob()
   call unit_check_start('fileglob',msg='')
   !!call unit_check('fileglob', 0.eq.0, 'checking',100)
   call unit_check_done('fileglob',msg='')
end subroutine test_fileglob
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_set_environment_variable()

integer :: ierr
character(len=4096) :: value
   call unit_check_start('set_environment_variable',msg='')
!! CHECK NOT_THERE_S_E_V IS NOT THERE FOR TEST
   call get_environment_variable("NOT_THERE_S_E_V", status=ierr)
   call unit_check('set_environment_variable',ierr.eq.1,'make sure variable does not exist,status=',ierr)
!! SET THE VARIABLE NOT_THERE_S_E_V
   call set_environment_variable('NOT_THERE_S_E_V','this is the value',ierr)
!! CHECK VARIABLE IS NOW SET
   call unit_check('set_environment_variable',ierr.eq.0,'setting, status should be zero when setting=',ierr)
   call get_environment_variable("NOT_THERE_S_E_V", value=value,status=ierr)
   call unit_check('set_environment_variable',ierr.eq.0,'status should be zero when getting=',ierr)
   call unit_check('set_environment_variable',value.eq.'this is the value','value is set to:',value)
!! REPLACE VALUE
   call set_environment_variable('NOT_THERE_S_E_V','this is the new value',ierr)
   call unit_check('set_environment_variable',ierr.eq.0,'setting, status should be zero when setting=',ierr)
   call get_environment_variable("NOT_THERE_S_E_V", value=value,status=ierr)
   call unit_check('set_environment_variable',ierr.eq.0,'status should be zero when getting=',ierr)
   call unit_check('set_environment_variable',value.eq.'this is the new value','value is set to:',value)

   call unit_check_done('set_environment_variable',msg='')
end subroutine test_set_environment_variable
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_system_access()

integer                     :: i
character(len=80),parameter :: names(*)=[ &
      '/usr/bin/bash   ', &
      '/tmp/NOTTHERE   ', &
      '/usr/local      ', &
      '.               ', &
      'PROBABLY_NOT    ']
   do i=1,size(names)
      write(*,*)' does ',trim(names(i)),' exist?    ', system_access(names(i),F_OK)
      write(*,*)' is ',trim(names(i)),' readable?     ', system_access(names(i),R_OK)
      write(*,*)' is ',trim(names(i)),' writeable?    ', system_access(names(i),W_OK)
      write(*,*)' is ',trim(names(i)),' executable?   ', system_access(names(i),X_OK)
   enddo
   call unit_check_start('system_access',msg='')
   !!call unit_check('system_access', 0.eq.0, 'checking',100)
   call unit_check_done('system_access',msg='')
end subroutine test_system_access
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_system_chdir()
character(len=:),allocatable :: dirname
character(len=:),allocatable :: hold
integer             :: ierr
   call unit_check_start('system_chdir',msg='test system_chdir(3f) assuming Unix-like file system and system_getwd(3f) works')
   call system_getcwd(hold,ierr)

   call system_chdir('/tmp',ierr)
   call system_getcwd(dirname,ierr)
   call unit_check('system_chdir', dirname.eq.'/tmp', 'checking /tmp to',dirname)

   call system_chdir('/',ierr)
   call system_getcwd(dirname,ierr)
   call unit_check('system_chdir', dirname.eq.'/', 'checking / to',dirname)

   call system_chdir(hold,ierr)
   call system_getcwd(dirname,ierr)
   call unit_check('system_chdir', dirname.eq.hold, 'checking ',hold,' to',dirname)

   call unit_check_done('system_chdir',msg='')
end subroutine test_system_chdir
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_system_chmod()

integer             :: ierr
integer             :: status
integer(kind=int64) :: buffer(13)
integer             :: ios
character(len=4096) :: message

!Setting Read Permissions for User, Group, and Others
! The following example sets read permissions for the owner, group, and others.
   open(file='_test1',unit=10)
   write(10,*,iostat=ios,iomsg=message)'TEST FILE 1'
   if(ios.ne.0)then
      write(*,*)trim(message)
   endif

   flush(unit=10,iostat=ios,iomsg=message)
   if(ios.ne.0)then
      write(*,*)trim(message)
   endif

   close(unit=10,iostat=ios,iomsg=message)
   if(ios.ne.0)then
      write(*,*)trim(message)
   endif

   ierr=system_chmod('_test1', IANY([R_USR,R_GRP,R_OTH]))

   open(file='_test1',unit=10)
   close(unit=10,status='delete',iostat=ios,iomsg=message)
   if(ios.ne.0)then
      write(*,*)trim(message)
   endif

!Setting Read, Write, and Execute Permissions for the Owner Only
! The following example sets read, write, and execute permissions for the owner, and no permissions for group and others.
   open(file='_test2',unit=10)
   write(10,*)'TEST FILE 2'
   close(unit=10)
   ierr=system_chmod('_test2', RWX_U)
   open(file='_test2',unit=10)
   close(unit=10,status='delete')

!Setting Different Permissions for Owner, Group, and Other
! The following example sets owner permissions for CHANGEFILE to read, write, and execute, group permissions to read and
! execute, and other permissions to read.
   open(file='_test3',unit=10)
   write(10,*)'TEST FILE 3'
   close(unit=10)
   ierr=system_chmod('_test3', IANY([RWX_U,R_GRP,X_GRP,R_OTH]));
   open(file='_test3',unit=10)
   close(unit=10,status='delete')

!Setting and Checking File Permissions
! The following example sets the file permission bits for a file named /home/cnd/mod1, then calls the stat() function to
! verify the permissions.

   ierr=system_chmod("home/cnd/mod1", IANY([RWX_U,RWX_G,R_OTH,W_OTH]))
   call system_stat("home/cnd/mod1", buffer,status)

! In order to ensure that the S_ISUID and S_ISGID bits are set, an application requiring this should use stat() after a
! successful chmod() to verify this.

!    Any files currently open could possibly become invalid if the mode
!    of the file is changed to a value which would deny access to
!    that process.

   call unit_check_start('system_chmod',msg='')
   !!call unit_check('system_chmod', 0.eq.0, 'checking',100)
   call unit_check_done('system_chmod',msg='')
end subroutine test_system_chmod
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_system_chown()

integer                     :: i
character(len=80),parameter :: names(*)=[character(len=80) :: 'myfile1','/usr/local']
   do i=1,size(names)
      if(.not.  system_chown(&
      & trim(names(i)),  &
      & system_getuid(), &
      & system_getgid()) &
         )then
         call system_perror('*test_system_chown* '//trim(names(i)))
      endif
   enddo
   call unit_check_start('system_chown',msg='')
   !!call unit_check('system_chown', 0.eq.0, 'checking',100)
   call unit_check_done('system_chown',msg='')
end subroutine test_system_chown
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_system_clearenv()

! environment before clearing
   call execute_command_line('env|wc -l')
! environment after clearing (not necessarily blank!!)
   call system_clearenv()
   call execute_command_line('env')
   call unit_check_start('system_clearenv',msg='')
   !!call unit_check('system_clearenv', 0.eq.0, 'checking',100)
   call unit_check_done('system_clearenv',msg='')
end subroutine test_system_clearenv
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_system_closedir()

type(c_ptr)                  :: dir
character(len=:),allocatable :: filename
integer                      :: ierr
call unit_check_start('system_closedir',msg='test if can read from current directory, assumed non-empty and close and retry')
   call system_opendir('.',dir,ierr)      !--- open directory stream to read from
   call system_readdir(dir,filename,ierr) !--- read directory stream
   call unit_check('system_closedir', filename.ne.'', 'found a file named',filename)
   call system_closedir(dir,ierr)         !--- close directory stream
   call unit_check('system_closedir', ierr.eq.0, 'closing gave ierr=',ierr)
   !!!!!!! TRYING BAD OPERATION HANGS SYSTEMS. CANNOT FIND GENERIC TEST TO SEE IF OPEN
   !!call system_readdir(dir,filename,ierr)
   !!call unit_check('system_closedir', ierr.ne.0, 'try reading now should give error ierr=',ierr)
   !!!!!!!
   call unit_check_done('system_closedir',msg='')
end subroutine test_system_closedir
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_system_cpu_time()

real    :: user_start, system_start, total_start
real    :: user_finish, system_finish, total_finish
integer :: i
real    :: value

   call system_cpu_time(total_start,user_start,system_start)

   value=0.0
   do i=1,1000000
      value=sqrt(real(i)+value)
   enddo
   write(*,*)'average sqrt value=',value/1000000.0
   call system_cpu_time(total_finish,user_finish,system_finish)
   write(*,*)'USER ......',user_finish-user_start
   write(*,*)'SYSTEM ....',system_finish-system_start
   write(*,*)'TOTAL .....',total_finish-total_start

   call unit_check_start('system_cpu_time',msg='')
   !!call unit_check('system_cpu_time', 0.eq.0, 'checking',100)
   call unit_check_done('system_cpu_time',msg='')
end subroutine test_system_cpu_time
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_system_getcwd()
character(len=:),allocatable :: dirname
character(len=:),allocatable :: hold
integer                      :: ierr
   call unit_check_start('system_getcwd',msg='test system_getcwd(3f) assuming Unix-like file system')
   ! cache current directory so can return
   call system_getcwd(hold,ierr)
   call unit_check('system_getcwd', ierr.eq.0 , 'checking ierr on getting current directory=',ierr)

   call system_chdir('/tmp',ierr)
   call system_getcwd(dirname,ierr)
   call unit_check('system_getcwd', dirname.eq.'/tmp', 'checking /tmp to',dirname)

   call system_chdir('/',ierr)
   call system_getcwd(dirname,ierr)
   call unit_check('system_getcwd', dirname.eq.'/', 'checking / to',dirname)
   ! back to original
   call system_chdir(hold,ierr)
   call system_getcwd(dirname,ierr)
   call unit_check('system_getcwd', dirname.eq.hold, 'checking ',hold,' to',dirname)

   call unit_check_done('system_getcwd',msg='')
end subroutine test_system_getcwd
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_system_getenv()
character(len=256)           :: var
character(len=256)           :: envname
character(len=*),parameter   :: names(*)=[character(len=10)::'USER','HOME','LOGNAME','USERNAME']
integer                      :: i
integer                      :: ierr
   call unit_check_start('system_getenv',msg='')
   do i=1,size(names)
      var=system_getenv(names(i))
      call get_environment_variable(names(i),value=envname, status=ierr)
      call unit_check('system_getenv', envname.eq.var, names(i),var,envname)
   enddo
   call unit_check_done('system_getenv',msg='')
end subroutine test_system_getenv
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_system_getgrgid()
integer(kind=int64)          :: gid
character(len=:),allocatable :: name
   gid=system_getgid()
   name=system_getgrgid( gid )
   write(*,'("group[",a,"] for ",i0)')name,system_getgid()
   call unit_check_start('system_getgrgid',msg='')
   !!call unit_check('system_getgrgid', 0.eq.0, 'checking',100)
   call unit_check_done('system_getgrgid',msg='')
end subroutine test_system_getgrgid
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_system_gethostname()
character(len=:),allocatable :: name
integer                      :: ierr

   call system_gethostname(name,ierr)
   if(ierr.eq.0)then
      write(*,'("hostname[",a,"]")')name
   else
      write(*,'(a)')'ERROR: could not get hostname'
   endif

   call unit_check_start('system_gethostname',msg='')
   !!call unit_check('system_gethostname', 0.eq.0, 'checking',100)
   call unit_check_done('system_gethostname',msg='')
end subroutine test_system_gethostname
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_system_getlogin()
character(len=80) :: envname
character(len=:),allocatable :: name
integer                      :: ierr
   call unit_check_start('system_getlogin',msg=' test system_getlogin(3f) against environment variable')
   call get_environment_variable("USER",value=envname, status=ierr)
   if(envname.eq.'')then
      call get_environment_variable("LOGNAME",value=envname, status=ierr)
   endif
   if(envname.eq.'')then
      call get_environment_variable("USERNAME",value=envname, status=ierr)
   endif
   if(envname.eq.'')then
      call unit_check_msg('system_getlogin',' did not find username in environment, test invalid')
   else
      name=system_getlogin()
      call unit_check('system_getlogin', name.eq.envname, 'checking',envname,'versus',name)
   endif
   call unit_check_done('system_getlogin',msg='')
end subroutine test_system_getlogin
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_system_getpwuid()
character(len=:),allocatable :: name
integer(kind=int64)          :: uid
   uid=system_getuid()
   name=system_getpwuid(uid)
   write(*,'("login[",a,"] has UID ",i0)')name,uid
   call unit_check_start('system_getpwuid',msg='')
   !!call unit_check('system_getpwuid', 0.eq.0, 'checking',100)
   call unit_check_done('system_getpwuid',msg='')
end subroutine test_system_getpwuid
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_system_getumask()
integer :: i
   write(*,101)(system_getumask(),i=1,4)
101 format(1x,i0,1x,"O'",o4.4,"'",1x,'Z"',z0,"'",1x,"B'",b12.12,"'")
   call unit_check_start('system_getumask',msg='')
   !!call unit_check('system_getumask', 0.eq.0, 'checking',100)
   call unit_check_done('system_getumask',msg='')
end subroutine test_system_getumask
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_system_isblk()
integer                     :: i
character(len=80),parameter :: names(*)=[ &
      '/tmp            ', &
      '/tmp/NOTTHERE   ', &
      '/usr/local      ', &
      '.               ', &
      'block_device.tst', &
      'PROBABLY_NOT    ']
   do i=1,size(names)
      write(*,*)' is ',trim(names(i)),' a block device? ', system_isblk(names(i))
   enddo
   call unit_check_start('system_isblk',msg='')
   !!call unit_check('system_isblk', 0.eq.0, 'checking',100)
   call unit_check_done('system_isblk',msg='')
end subroutine test_system_isblk
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_system_ischr()
integer                     :: i
character(len=80),parameter :: names(*)=[ &
      '/tmp            ', &
      '/tmp/NOTTHERE   ', &
      '/usr/local      ', &
      '.               ', &
      'char_dev.test   ', &
      'PROBABLY_NOT    ']
   do i=1,size(names)
      write(*,*)' is ',trim(names(i)),' a character device? ', system_ischr(names(i))
   enddo
   call unit_check_start('system_ischr',msg='')
   !!call unit_check('system_ischr', 0.eq.0, 'checking',100)
   call unit_check_done('system_ischr',msg='')
end subroutine test_system_ischr
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_system_isdir()
integer                     :: i
character(len=80),parameter :: names(*)=[ &
      '/tmp            ', &
      '/tmp/NOTTHERE   ', &
      '/bin/           ', &
      '.               ', &
      'PROBABLY_NOT    ']
logical,parameter           :: expected(*)=[.true., .false., .true., .true., .false.]
logical                     :: answer
   call unit_check_start('system_isdir',msg='')
   do i=1,size(names)
      answer=system_isdir(names(i))
      call unit_check('system_isdir', answer.eqv.expected(i), names(i))
   enddo
   call unit_check_done('system_isdir',msg='')
end subroutine test_system_isdir
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_system_isfifo()
integer                     :: i
character(len=80),parameter :: names(*)=[ &
      '/tmp            ', &
      '/tmp/NOTTHERE   ', &
      '/usr/local      ', &
      '.               ', &
      'fifo.test       ', &
      'PROBABLY_NOT    ']
   do i=1,size(names)
      write(*,*)' is ',trim(names(i)),' a fifo(named pipe)? ', system_isfifo(names(i))
   enddo
   call unit_check_start('system_isfifo',msg='')
   !!call unit_check('system_isfifo', 0.eq.0, 'checking',100)
   call unit_check_done('system_isfifo',msg='')
end subroutine test_system_isfifo
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_system_islnk()
integer                     :: i
character(len=80),parameter :: names(*)=[ &
      '/tmp            ', &
      '/tmp/NOTTHERE   ', &
      '/usr/local      ', &
      '.               ', &
      'link.test       ', &
      'PROBABLY_NOT    ']
   do i=1,size(names)
      write(*,*)' is ',trim(names(i)),' a link? ', system_islnk(names(i))
   enddo
   call unit_check_start('system_islnk',msg='')
   !!call unit_check('system_islnk', 0.eq.0, 'checking',100)
   call unit_check_done('system_islnk',msg='')
end subroutine test_system_islnk
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_system_isreg()
integer                     :: i
character(len=80),parameter :: names(*)=[ &
      '/tmp            ', &
      'test.txt        ', &
      '.               ']
   do i=1,size(names)
      write(*,*)' is ',trim(names(i)),' a regular file? ', system_isreg(names(i))
   enddo
   call unit_check_start('system_isreg',msg='')
   !!call unit_check('system_isreg', 0.eq.0, 'checking',100)
   call unit_check_done('system_isreg',msg='')
end subroutine test_system_isreg
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_system_issock()

integer                     :: i
character(len=80),parameter :: names(*)=[ &
      '/tmp            ', &
      '/tmp/NOTTHERE   ', &
      '/usr/local      ', &
      '.               ', &
      'sock.test       ', &
      'PROBABLY_NOT    ']
   do i=1,size(names)
      write(*,*)' is ',trim(names(i)),' a socket? ', system_issock(names(i))
   enddo
   call unit_check_start('system_issock',msg='')
   !!call unit_check('system_issock', 0.eq.0, 'checking',100)
   call unit_check_done('system_issock',msg='')
end subroutine test_system_issock
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_system_link()

integer :: ierr
   ierr = system_link('myfile1','myfile2')
   if(ierr.ne.0)then
      call system_perror('*test_system_link*')
   endif
   call unit_check_start('system_link',msg='')
   !!call unit_check('system_link', 0.eq.0, 'checking',100)
   call unit_check_done('system_link',msg='')
end subroutine test_system_link
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_system_mkdir()

integer :: ierr
logical :: query
   call unit_check_start('system_mkdir',msg='make and remove _scratch/')
   ierr=system_mkdir('_scratch',IANY([R_USR,W_USR,X_USR]))
   call unit_check('system_mkdir', ierr.eq.0, 'make _scratch/, ierr=',ierr)
   query=system_isdir('_scratch')
   call unit_check_msg('system_mkdir',query,'looks like the directory was made')
   call system_chdir('_scratch',ierr)
   call system_chdir('..',ierr)
   call unit_check_msg('system_mkdir',ierr.eq.0,'looks like it can be entered')
   ierr=system_rmdir('_scratch')
   call unit_check('system_mkdir', ierr.eq.0, 'remove _scratch/, ierr=',ierr)
   call unit_check_done('system_mkdir',msg='')
end subroutine test_system_mkdir
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_system_mkfifo()

integer :: status
   status = system_mkfifo("/home/cnd/mod_done", IANY([W_USR, R_USR, R_GRP, R_OTH]))
   if(status.ne.0)then
      call system_perror('*mkfifo* error:')
   endif
   call unit_check_start('system_mkfifo',msg='')
   !!call unit_check('system_mkfifo', 0.eq.0, 'checking',100)
   call unit_check_done('system_mkfifo',msg='')
end subroutine test_system_mkfifo
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_system_opendir()
type(c_ptr)                  :: dir
character(len=:),allocatable :: filename
integer                      :: i
integer                      :: ierr
   call unit_check_start('system_opendir',msg='')
   call system_opendir('.',dir,ierr)                                              !--- open directory stream to read from
   call unit_check('system_opendir', ierr.eq.0, 'checking ierr=',ierr)
   i=0
   do                                                                             !--- read directory stream
      call system_readdir(dir,filename,ierr)
      if(filename.eq.' ')exit
      i=i+1
   enddo
   call system_closedir(dir,ierr)                                                 !--- close directory stream
   call unit_check_done('system_opendir',msg='')
end subroutine test_system_opendir
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_system_perm()

character(len=4096) :: string
integer(kind=int64) :: values(13)
integer             :: ierr
character(len=:),allocatable :: perms
   values=0
   call get_command_argument(1, string)  ! get pathname from command line
   call system_stat(string,values,ierr)  ! get pathname information
   if(ierr.eq.0)then
      perms=system_perm(values(3))       ! convert permit mode to a string
! print permits as a string, decimal value, and octal value
      write(*,'("for ",a," permits[",a,"]",1x,i0,1x,o0)') &
         trim(string),perms,values(3),values(3)
   endif
   call unit_check_start('system_perm',msg='')
   !!call unit_check('system_perm', 0.eq.0, 'checking',100)
   call unit_check_done('system_perm',msg='')
end subroutine test_system_perm
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_system_perror()

character(len=:),allocatable :: DIRNAME
   DIRNAME='/NOT/THERE/OR/ANYWHERE'
! generate an error with a routine that supports errno and perror(3c)
   if(system_rmdir(DIRNAME).ne.0)then
      call system_perror('*test_system_perror*:'//DIRNAME)
   endif
   write(*,'(a)')"That's all Folks!"
   call unit_check_start('system_perror',msg='')
   !!call unit_check('system_perror', 0.eq.0, 'checking',100)
   call unit_check_done('system_perror',msg='')
end subroutine test_system_perror
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_system_putenv()

character(len=4096) :: value
integer             :: ierr
   call unit_check_start('system_putenv',msg='')
!! CHECK NOT_THERE_S_P IS NOT THERE FOR TEST
   call get_environment_variable("NOT_THERE_S_P", status=ierr)
   call unit_check('system_putenv',ierr.eq.1,'make sure variable does not exist,status=',ierr)
!! SET THE VARIABLE NOT_THERE_S_P
   call system_putenv('NOT_THERE_S_P=this is the value',ierr)
!! CHECK VARIABLE IS NOW SET
   call unit_check('system_putenv',ierr.eq.0,'setting, status should be zero when setting=',ierr)
   call get_environment_variable("NOT_THERE_S_P", value=value,status=ierr)
   call unit_check('system_putenv',ierr.eq.0,'status should be zero when getting=',ierr)
   call unit_check('system_putenv',value.eq.'this is the value','value is set to:',value)
!! REPLACE VALUE
   call system_putenv('NOT_THERE_S_P=this is the new value',ierr)
   call unit_check('system_putenv',ierr.eq.0,'setting, status should be zero when setting=',ierr)
   call get_environment_variable("NOT_THERE_S_P", value=value,status=ierr)
   call unit_check('system_putenv',ierr.eq.0,'status should be zero when getting=',ierr)
   call unit_check('system_putenv',value.eq.'this is the new value','value is set to:',value)
!! DELETE VALUE
   call system_putenv('NOT_THERE_S_P',ierr)
   call get_environment_variable("NOT_THERE_S_P", status=ierr)
   call unit_check('system_putenv',ierr.eq.1,'should be gone, varies with different putenv(3c)',ierr)
   call unit_check_msg('system_putenv','system_unsetenv(3f) is a better way to remove variables')
!!
   call unit_check_done('system_putenv',msg='')
end subroutine test_system_putenv
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_system_readdir()

type(c_ptr)                  :: dir
character(len=:),allocatable :: filename
integer                      :: ierr
character(len=256)           :: message
integer                      :: ios
integer                      :: lun
logical                      :: found1,found2
   call unit_check_start('system_readdir',msg='make some scratch files and look for their name in current directory')
   found1=.false.
   found2=.false.
!--- create two scratch files of known names

   open(newunit=lun,file='__scratch_1__',iostat=ios,iomsg=message)
   if(ios.eq.0)then
      write(lun,*)'SCRATCH FILE 1'
   else
      call unit_check_msg('system_readdir','error:',message)
   endif
   close(unit=lun,iostat=ios,iomsg=message)

   open(newunit=lun,file='__scratch_2__',iostat=ios,iomsg=message)
   if(ios.eq.0)then
      write(lun,*)'SCRATCH FILE 2'
   else
      call unit_check_msg('system_readdir','error:',message)
   endif
   close(unit=lun,iostat=ios,iomsg=message)

!--- open directory stream to read from
   call system_opendir('.',dir,ierr)
   call unit_check('system_opendir', ierr.eq.0, 'system_opendir ierr=',ierr)
!--- read directory stream and look for scratch file names
      do
         call system_readdir(dir,filename,ierr)
         if(filename.eq.' ') exit
         call unit_check('system_readdir', ierr.eq.0, 'system_readdir ierr=',ierr,'filename=',filename)
         if(ierr.ne.0) exit
         if(filename.eq.'__scratch_1__')found1=.true.
         if(filename.eq.'__scratch_2__')found2=.true.
      enddo
!--- close directory stream
   call system_closedir(dir,ierr)
   call unit_check('system_readdir', ierr.eq.0, 'system_closedir ierr=',ierr)

   call unit_check('system_readdir', found1, '__scratch__1',found1)
   call unit_check('system_readdir', found2, '__scratch__2',found2)

!--- remove scratch files
   open(newunit=lun,file='__scratch_1__',iostat=ios,iomsg=message)
   close(unit=lun,iostat=ios,iomsg=message,status='delete')
   open(newunit=lun,file='__scratch_2__',iostat=ios,iomsg=message)
   close(unit=lun,iostat=ios,iomsg=message,status='delete')

   call unit_check_done('system_readdir',msg='')
end subroutine test_system_readdir
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_system_readenv()
character(len=:),allocatable :: string
integer                      :: i
integer                      :: ierr
character(len=:),allocatable :: home
character(len=4096)          :: envname
   call unit_check_start('system_readenv',msg='assuming system always has environment variable HOME set')
   i=0
   home=''
   ! read environment table and look for HOME= at beginning of line
   call system_initenv()
   do
      string=system_readenv()
      if(index(string,'HOME=').eq.1)then
        home=string
      endif
      if(string.eq.'')then
         exit
      else
         i=i+1
      endif
   enddo
   call get_environment_variable("HOME",value=envname, status=ierr)
   envname='HOME='//trim(envname)
   call unit_check('system_readenv',home.eq.envname, 'HOME',home,envname)
   call unit_check_done('system_readenv',msg='')
end subroutine test_system_readenv
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_system_remove()
character(len=*),parameter :: FILE='__MyJunkFile.txt'
integer                    :: ierr
integer                    :: ios
character(len=256)         :: message
   call unit_check_start('system_remove',msg='')
   ierr=system_remove(FILE) ! note intentionally causes error if file exists
   open(unit=10,file=FILE,iostat=ios,status='NEW')
   if(ios.eq.0)then
      write(10,'(a)',iostat=ios)'This is a file to be deleted by the test of system_remove(3f)'
      close(unit=10,iostat=ios)
      call unit_check('system_remove',system_isreg(FILE),msg='checking if test file exists before remove')
   else
      call unit_check('system_remove', ios.eq.0, 'bad I/O IOSTAT=',ios,message)
   endif
   ierr=system_remove(FILE)
   call unit_check('system_remove', ierr.eq.0, 'checking return code',ierr)
   call unit_check('system_remove',.not.system_isreg(FILE),msg='checking if test file exists after remove')
   call unit_check('system_remove',.not.system_access(FILE,F_OK),msg='checking if test file exists after remove')
   call unit_check_done('system_remove',msg='')
end subroutine test_system_remove
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_system_rename()

character(len=256) :: string
character(len=256) :: message
integer            :: ios
integer            :: ierr
   call unit_check_start('system_rename',msg='check system_rename(3f) renaming "_scratch_file_" to "_renamed_scratch_file_"')
   message=''
! try to remove junk files just in case
   ierr=system_remove('_scratch_file_')
   ierr=system_remove('_renamed_scratch_file_')
! create scratch file to rename
   close(unit=10,iostat=ios,status='delete')
   open(unit=10,file='_scratch_file_',status='new',iostat=ios)
   call unit_check('system_rename', ios.eq.0, 'message from OPEN(3f) is:',message,' ios is',ios)
   write(10,'(a)',iostat=ios,iomsg=message) 'IF YOU SEE THIS RENAME WORKED'
   close(unit=10)
! rename scratch file
   ierr=system_rename('_scratch_file_','_renamed_scratch_file_')
   call unit_check('system_rename', ierr.eq.0, 'ierr',ierr)
! read renamed file
   open(unit=11,file='_renamed_scratch_file_',status='old')
   read(11,'(a)',iostat=ios)string
   call unit_check('system_rename', ios.eq.0, 'ios',ierr)
   call unit_check('system_rename', string.eq.'IF YOU SEE THIS RENAME WORKED', string)
   close(unit=11)
! clean up
   ierr=system_remove('_scratch_file_')
   call unit_check('system_rename', ierr.ne.0, 'cleanup',ierr)
   ierr=system_remove('_renamed_scratch_file_')
   call unit_check('system_rename', ierr.eq.0, 'cleanup',ierr)
   call unit_check_done('system_rename',msg='')
end subroutine test_system_rename
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_system_rewinddir()
type(c_ptr)                  :: dir
character(len=:),allocatable :: filename
integer                      :: sum(2)
integer                      :: i
integer                      :: j
integer                      :: ierr
   call unit_check_start('system_rewinddir',msg='')
   call system_opendir('.',dir,ierr)                   ! open directory stream to read from
   do i=1,2                                            ! read directory stream twice
      j=0
      do
         call system_readdir(dir,filename,ierr)
         if(filename.eq.' ')exit
         j=j+1
      enddo
      sum(i)=j
      call system_rewinddir(dir)                       ! rewind directory stream
   enddo
   call system_closedir(dir,ierr)                      ! close directory stream
   call unit_check('system_rewinddir', sum(1).eq.sum(2), 'number of files','PASS 1:',sum(1),'PASS 2:',sum(2))
   call unit_check_done('system_rewinddir',msg='')
end subroutine test_system_rewinddir
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_system_rmdir()

   integer :: ierr
   character(len=*),parameter :: dirname='_scratch_rmdir'
!! setup
   call unit_check_start('system_rmdir',msg='')
   if(system_isdir(dirname))then ! TRY TO CREATE
      call unit_check_msg('system_rmdir',dirname,'directory existed')
   endif
   ierr=system_mkdir(dirname,RWX_U)
   call unit_check('system_rmdir',ierr.eq.0,'try to create',dirname)
   call unit_check('system_rmdir',system_isdir(dirname),'check if',dirname,'exists and is a directory')
!! test
   ierr=system_rmdir(dirname) ! TRY TO REMOVE
   call unit_check('system_rmdir',ierr.eq.0,'check ierr',ierr)
   call unit_check('system_rmdir',.not.system_isdir(dirname),'check if',dirname,'is still a directory')

   if(system_isdir(dirname))then
      call unit_check_bad('system_rmdir',msg=str('testing went bad,',dirname,'is still a directory'))
   else
      ierr=system_rmdir(dirname) ! TRY TO REMOVE scratch directory when it should be gone
      call unit_check('system_rmdir',ierr.ne.0,'check ierr',ierr)
      call system_perror('*test of system_rmdir*')
   endif

   call unit_check_done('system_rmdir',msg='')
end subroutine test_system_rmdir
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_system_setumask()

integer :: newmask
integer :: old_umask
integer :: i
   write(*,101)(system_getumask(),i=1,4)
101 format(1x,i0,1x,"O'",o4.4,"'",1x,'Z"',z0,"'",1x,"B'",b12.12,"'")
   newmask=63
   old_umask=system_setumask(newmask)
   write(*,*)'NEW'
   write(*,101)(system_getumask(),i=1,4)
   call unit_check_start('system_setumask',msg='')
   !!call unit_check('system_setumask', 0.eq.0, 'checking',100)
   call unit_check_done('system_setumask',msg='')
end subroutine test_system_setumask
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_system_stat()

integer(kind=int64)  :: buff(13)
integer(kind=int32)  :: status
character(len=*),parameter :: fmt_date='year-month-day hour:minute:second'
integer(kind=int64)  :: &
   Device_ID,           Inode_number,          File_mode,                  Number_of_links,  Owner_uid,         &
   Owner_gid,           Directory_device,      File_size,                  Last_access,      Last_modification, &
   Last_status_change,  Preferred_block_size,  Number_of_blocks_allocated
character(len=:),allocatable         :: temp
integer              :: dat(8)
equivalence                                    &
   ( buff(1)  , Device_ID                  ) , &
   ( buff(2)  , Inode_number               ) , &
   ( buff(3)  , File_mode                  ) , &
   ( buff(4)  , Number_of_links            ) , &
   ( buff(5)  , Owner_uid                  ) , &
   ( buff(6)  , Owner_gid                  ) , &
   ( buff(7)  , Directory_device           ) , &
   ( buff(8)  , File_size                  ) , &
   ( buff(9)  , Last_access                ) , &
   ( buff(10) , Last_modification          ) , &
   ( buff(11) , Last_status_change         ) , &
   ( buff(12) , Preferred_block_size       ) , &
   ( buff(13) , Number_of_blocks_allocated )

   call system_stat("/etc/hosts", buff, status)

   if (status == 0) then
      write (*, FMT="('Device ID(hex/decimal):',      T30, Z0,'h/',I0,'d')") buff(1),buff(1)
      write (*, FMT="('Inode number:',                T30, I0)") buff(2)
      write (*, FMT="('File mode (octal):',           T30, O19)") buff(3)
      write (*, FMT="('Number of links:',             T30, I0)") buff(4)
      write (*, FMT="('Owner''s uid/username:',       T30, I0,1x, A)") buff(5), system_getpwuid(buff(5))
      write (*, FMT="('Owner''s gid/group:',          T30, I0,1x, A)") buff(6), system_getgrgid(buff(6))
      write (*, FMT="('Device where located:',        T30, I0)") buff(7)
      write (*, FMT="('File size(bytes):',            T30, I0)") buff(8)
      dat=u2d(0+int(buff(9))) ! add 0 to avoid gfortran-11 bug
      temp=fmtdate(dat,fmt_date) ! kludge for ifort (IFORT) 2021.3.0 20210609
      write (*, FMT="('Last access time:',            T30, I0,1x, A)") buff(9), temp
      dat=u2d(0+int(buff(10)))
      temp=fmtdate(dat,fmt_date) ! kludge for ifort (IFORT) 2021.3.0 20210609
      write (*, FMT="('Last modification time:',      T30, I0,1x, A)") buff(10),temp
      dat=u2d(0+int(buff(11)))
      temp=fmtdate(dat,fmt_date) ! kludge for ifort (IFORT) 2021.3.0 20210609
      write (*, FMT="('Last status change time:',     T30, I0,1x, A)") buff(11),temp
      write (*, FMT="('Preferred block size(bytes):', T30, I0)") buff(12)
      write (*, FMT="('No. of blocks allocated:',     T30, I0)") buff(13)
   endif

   call unit_check_start('system_stat',msg='')
   !!call unit_check('system_stat', 0.eq.0, 'checking',100)
   call unit_check_done('system_stat',msg='')
end subroutine test_system_stat
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_system_uname()

integer,parameter          :: is=100
integer                    :: i
character(len=*),parameter :: letters='srvnmxT'
character(len=is)          :: string=' '

   write(*,'(80("="))')
   do i=1,len(letters)
      call system_uname(letters(i:i),string)
      write(*,*)'=====> TESTING system_uname('//letters(i:i)//')--->'//trim(string)
   enddo
   write(*,'(80("="))')
   call unit_check_start('system_uname',msg='')
   !!call unit_check('system_uname', 0.eq.0, 'checking',100)
   call unit_check_done('system_uname',msg='')
end subroutine test_system_uname
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_system_unlink()

integer :: ierr
   ierr = system_unlink('myfile1')
   if(ierr.ne.0)then
      call system_perror('*test_system_unlink*')
   endif
   call unit_check_start('system_unlink',msg='')
   !!call unit_check('system_unlink', 0.eq.0, 'checking',100)
   call unit_check_done('system_unlink',msg='')
end subroutine test_system_unlink
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_system_unsetenv()

integer :: ierr
character(len=4096) :: value

   call system_unsetenv('GRU')

   call unit_check_start('system_unsetenv',msg='')

!! SET THE VARIABLE NOT_THERE_S_U
   call set_environment_variable('NOT_THERE_S_U','this is the value',ierr)
!! CHECK VARIABLE IS NOW SET
   call get_environment_variable("NOT_THERE_S_U", value=value,status=ierr)
   call unit_check('system_unsetenv',ierr.eq.0,'status should be zero when getting=',ierr)
   call unit_check('system_unsetenv',value.eq.'this is the value','value is set to:',value)
   !! REMOVE
   call system_unsetenv('NOT_THERE_S_U',ierr)
   call unit_check('system_unsetenv',ierr.eq.0,'should be zero ierr=',ierr)
   !! CHECK IF GONE
   call get_environment_variable("NOT_THERE_S_U", value=value,status=ierr)
   call unit_check('system_unsetenv',ierr.eq.1,'should be zero ierr=',ierr)
   call unit_check('system_unsetenv',value.eq.' ','value should be blank=',value)

   call unit_check_done('system_unsetenv',msg='')

end subroutine test_system_unsetenv
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_system_memcpy()
   call unit_check_start('system_memcpy',msg='')
   !!call unit_check('system_memcpy', 0.eq.0, 'checking',100)
   call unit_check_done('system_memcpy',msg='')
end subroutine test_system_memcpy
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_system_utime()
character(len=4096) :: pathname
integer             :: times(2)
integer             :: i
   call unit_check_start('system_utime',msg='')
   do i=1,command_argument_count()
      call get_command_argument(i, pathname)
      if(.not.system_utime(pathname,times))then
         call system_perror('*test_system_utime*')
      endif
   enddo
   !!call unit_check('system_utime', 0.eq.0, 'checking',100)
   call unit_check_done('system_utime',msg='')
end subroutine test_system_utime
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_system_dir()

   call unit_check_start('system_dir',msg='')
   !!call unit_check('system_dir', 0.eq.0, 'checking',100)
   call unit_check_done('system_dir',msg='')
end subroutine test_system_dir
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
end subroutine test_suite_M_system_tests
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
