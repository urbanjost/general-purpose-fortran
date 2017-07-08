!>
!!##NAME
!!    M_system(3fm) - [M_system]Fortran interface to C system interface
!!##SYNOPSIS
!!
!!   Public objects:
!!
!!    use m_system, only : set_environment_variable, system_unsetenv, &
!!    system_putenv
!!
!!    use m_system, only :  system_intenv, system_readenv, system_clearenv
!!
!!    use M_system, only : system_getcwd, system_link,       &
!!    system_mkfifo, system_remove, system_rename,           &
!!    system_umask, system_unlink, fileglob                  &
!!    system_rmdir, system_chdir, system_mkdir, system_isdir
!!
!!    use M_system, only :system_errno, system_perror
!!
!!    use M_system, only :system_getegid, system_geteuid, system_getgid, &
!!    system_gethostname, system_getpid, system_getppid, system_getsid, &
!!    system_getuid, system_uname
!!
!!    use M_system, only : system_kill
!!
!!    use M_system, only : system_rand, system_srand
!!
!!##DESCRIPTION
!!    M_system(3fm) is a collection of Fortran procedures that mostly call C
!!    or a C wrapper using the ISO_C_BINDING interface to access system calls.
!!    System calls are special set of functions used by programs to communicate
!!    directly with an operating system.
!!
!!    Generally, system calls are slower than normal function calls because
!!    when you make a call control is relinquished to the operating system
!!    to perform the system call. In addition, depending on the nature of the
!!    system call, your program may be blocked by the OS until the system call
!!    has finished, thus making the execution time of your program even longer.
!!
!!    One rule-of-thumb that should always be followed when calling a system
!!    call -- Always check the return value.
!!##ENVIRONMENT ACCESS
!!        o  system_putenv(3f):    call putenv(3c)
!!        o  system_unsetenv(3f):  call unsetenv(3c) to remove variable from environment
!!        o  set_environment_variable(3f): set environment variable by calling setenv(3c)
!!
!!        o  system_initenv(3f):   initialize environment table for reading
!!        o  system_readenv(3f):   read next entry from environment table
!!        o  system_clearenv(3f):  emulate clearenv(3c) to clear environment
!!##FILE SYSTEM
!!        o  system_chdir(3f):     call chdir(3c) to change current directory of a process
!!        o  system_isdir(3f):     call fstat(3c) to determine if a filename is a directory
!!        o  system_islink(3f):    call lstat(3c) to determine if a filename is a link
!!        o  system_chmod(3f):     call chmod(3c) to set file permission mode
!!        o  system_getcwd(3f):    call getcwd(3c) to get pathname of current working directory
!!        o  system_getumask(3f):  call umask(3c) to get process permission mask
!!        o  system_mkdir(3f):     call mkdir(3c) to create empty directory
!!        o  system_mkfifo(3f):    call mkfifo(3c) to create a special FIFO file
!!        o  system_remove(3f):    call remove(3c) to remove file
!!        o  system_rename(3f):    call rename(3c) to change filename
!!        o  system_rmdir(3f):     call rmdir(3c) to remove empty directory
!!        o  system_setumask(3f):  call umask(3c) to set process permission mask
!!        o  system_link(3f):      call link(3c) to remove a filename link
!!        o  system_unlink(3f):    call unlink(3c) to create a link to a file
!!        o  fileglob(3f): Returns list of files using a file globbing pattern
!!
!!##RANDOM NUMBERS
!!         o  system_srand(3f): call srand(3c)
!!         o  system_rand(3f): call rand(3c)
!!##C ERROR INFORMATION
!!         o  system_errno(3f): return errno(3c)
!!         o  system_perror(3f): call perror(3c) to display error message
!!##QUERIES
!!         o  system_geteuid(3f): call geteuid(3c)
!!         o  system_getuid(3f): call getuid(3c)
!!         o  system_getegid(3f): call getegid(3c)
!!         o  system_getgid(3f): call getgid(3c)
!!         o  system_getpid(3f): call getpid(3c)
!!         o  system_getppid(3f): call getppid(3c)
!!         o  system_gethostname(3f): get name of current host
!!         o  system_uname(3f): call my_uname(3c) which calls uname(3c)
!!         o  system_getlogin(3f): get login name
!!
!!##FUTURE DIRECTIONS
!!    A good idea of what system routines are commonly required is to refer
!!    to the POSIX binding standards. (Note: IEEE 1003.9-1992 was withdrawn 6
!!    February 2003.) The IEEE standard covering Fortran 77 POSIX bindings
!!    is available online, though currently (unfortunately) only from
!!    locations with appropriate subscriptions to the IEEE server (e.g.,
!!    many university networks). For those who do have such access, the link
!!    is: POSIX Fortran 77 Language Interfaces (IEEE Std 1003.9-1992) (pdf)
!!##SEE ALSO
!!    Some vendors provide their own way to access POSIX functions and make
!!    those available as modules; for instance ...
!!       o the IFPORT module of Intel
!!       o or the f90_* modules of NAG.
!!       o There also some compiler-independent efforts to make them
!!         accessible, e.g.
!!          o Posix90 (doc),
!!          o flibs' platform/files and directories,
!!          o fortranposix.
!===================================================================================================================================
module M_system
use,intrinsic     :: iso_c_binding, only: c_float, c_int, c_char
use,intrinsic     :: iso_c_binding, only: c_ptr, c_f_pointer, c_null_char, c_null_ptr
use,intrinsic     :: iso_c_binding
implicit none
private
public :: system_rand
public :: system_srand

public :: system_getpid                  ! return process ID
public :: system_getppid                 ! return parent process ID
public :: system_getuid, system_geteuid  ! return user ID
public :: system_getgid, system_getegid  ! return group ID
public :: system_getsid
public :: system_kill                    ! (pid, signal) kill process (defaults: pid=0, signal=SIGTERM)

public :: system_errno
public :: system_perror

public :: system_putenv
public :: set_environment_variable
public :: system_unsetenv

public :: system_initenv
public :: system_readenv
public :: system_clearenv

public :: system_chdir
public :: system_rmdir
public :: system_remove
public :: system_rename

public :: system_mkdir
public :: system_mkfifo
public :: system_chmod
public :: system_link
public :: system_unlink

public :: system_setumask
public :: system_getumask
private :: system_umask

public :: system_getcwd

public :: system_opendir
public :: system_readdir
public :: system_rewinddir
public :: system_closedir

public :: system_uname
public :: system_gethostname
public :: system_getlogin
public :: fileglob
public :: R_GRP,R_OTH,R_USR,R_WXG,R_WXO,R_WXU,W_GRP,W_OTH,W_USR,X_GRP,X_OTH,X_USR,DEFFILEMODE,ACCESSPERMS

!! WORKING ON
!!stat

!!sysconf()
!!pathconf()
!!permission
!!exist
!!splitname
!!isdir
!!isreg
!!tmpname
!===================================================================================================================================
type, bind(C) :: dirent_SYSTEMA
  integer(c_long)    :: d_ino
  integer(c_long)    :: d_off; ! __off_t, check size
  integer(c_short)   :: d_reclen
  character(len=1,kind=c_char) :: d_name(256)
end type

type, bind(C) :: dirent_CYGWIN
  integer(c_int)       :: d_version
  integer(c_long)      :: d_ino
  character(c_char)    :: d_type
  character(c_char)    :: d_unused1(3)
  integer(c_int)       :: d_internal1
  character(len=1,kind=c_char) ::  d_name(256)
end type

integer,parameter,public :: mode_t=kind(c_int)
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    system_srand(3f) - [M_system]set seed for pseudo-random number generator system_rand(3f)
!!
!!##SYNOPSIS
!!
!!    subroutine system_srand()
!!
!!##DESCRIPTION
!!    system_srand(3f) calls the C routine srand(3c) The
!!    srand(3c)/system_srand(3f) function uses its argument as the seed
!!    for a new sequence of pseudo-random integers to be returned by
!!    system_rand(3f)/rand(3c). These sequences are repeatable by calling
!!    system_srand(3f) with the same seed value. If no seed value is
!!    provided, the system_rand(3f) function is automatically seeded with
!!    a value of 1.
!!
!!##EXAMPLE
!!
!!    Sample program:
!!
!!       program demo_system_srand
!!       use M_system, only : system_srand, system_rand
!!       implicit none
!!       integer :: i,j
!!       do j=1,2
!!          call system_srand(1001)
!!          do i=1,10
!!             write(*,*)system_rand()
!!          enddo
!!          write(*,*)
!!       enddo
!!       end program demo_system_srand
!!   expected results:
!!
!!      1512084687
!!      1329390995
!!      1874040748
!!        60731048
!!       239808950
!!      2017891911
!!        22055588
!!      1105177318
!!       347750200
!!      1729645355
!!
!!      1512084687
!!      1329390995
!!      1874040748
!!        60731048
!!       239808950
!!      2017891911
!!        22055588
!!      1105177318
!!       347750200
!!      1729645355
!!
!!##SEE ALSO
!!    drand48(3c), random(3c)
!===================================================================================================================================
character(len=*),parameter :: ident_srand="@(#)M_system::system_srand(3f): call srand(3c)"
! void srand_system(int *seed)
interface
   subroutine system_srand(seed) bind(c,name='srand')
      import c_int
      integer(kind=c_int),intent(in) :: seed
   end subroutine system_srand
end interface
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    system_kill(3f) - [M_system] send a signal to a process or a group
!!                      of processes
!!
!!##SYNOPSIS
!!
!!    integer(kind=c_int) function system_kill(pid,sig)
!!
!!       integer,intent(in) :: pid
!!       integer,intent(in) :: sig
!!
!!##DESCRIPTION
!!
!!    The kill() function shall send a signal to a process or a group of
!!    processes specified by pid. The signal to be sent is specified by sig
!!    and is either one from the list given in <signal.h> or 0. If sig is 0
!!    (the null signal), error checking is performed but no signal is actually
!!    sent. The null signal can be used to check the validity of pid.
!!
!!    For a process to have permission to send a signal to a process designated
!!    by pid, unless the sending process has appropriate privileges, the real
!!    or effective user ID of the sending process shall match the real or
!!    saved set-user-ID of the receiving process.
!!
!!    If pid is greater than 0, sig shall be sent to the process whose process
!!    ID is equal to pid.
!!
!!    If pid is 0, sig shall be sent to all processes (excluding an unspecified
!!    set of system processes) whose process group ID is equal to the process
!!    group ID of the sender, and for which the process has permission to send
!!    a signal.
!!
!!    If pid is -1, sig shall be sent to all processes (excluding an unspecified
!!    set of system processes) for which the process has permission to send
!!    that signal.
!!
!!    If pid is negative, but not -1, sig shall be sent to all processes
!!    (excluding an unspecified set of system processes) whose process group
!!    ID is equal to the absolute value of pid, and for which the process has
!!    permission to send a signal.
!!
!!    If the value of pid causes sig to be generated for the sending process,
!!    and if sig is not blocked for the calling thread and if no other thread
!!    has sig unblocked or is waiting in a sigwait() function for sig, either
!!    sig or at least one pending unblocked signal shall be delivered to the
!!    sending thread before kill() returns.
!!
!!    The user ID tests described above shall not be applied when sending
!!    SIGCONT to a process that is a member of the same session as the sending
!!    process.
!!
!!    An implementation that provides extended security controls may impose
!!    further implementation-defined restrictions on the sending of signals,
!!    including the null signal. In particular, the system may deny the
!!    existence of some or all of the processes specified by pid.
!!
!!    The kill() function is successful if the process has permission to send
!!    sig to any of the processes specified by pid. If kill() fails, no signal
!!    shall be sent.
!!
!!
!!##RETURN VALUE
!!
!!    Upon successful completion, 0 shall be returned. Otherwise, -1 shall be
!!    returned and errno set to indicate the error.
!!
!!##ERRORS
!!    The kill() function shall fail if:
!!
!!    EINVAL  The value of the sig argument is an invalid or unsupported
!!            signal number.
!!    EPERM   The process does not have permission to send the signal to
!!            any receiving process.
!!    ESRCH   No process or process group can be found corresponding to
!!            that specified by pid.  The following sections are informative.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_system_kill
!!    use M_system, only : system_kill
!!    use M_system, only : system_perror
!!    implicit none
!!    integer           :: i,pid,ios,ierr,signal=9
!!    character(len=80) :: argument
!!
!!       do i=1,command_argument_count()
!!          ! get arguments from command line
!!          call get_command_argument(i, argument)
!!          ! convert arguments to integers assuming they are PID numbers
!!          read(argument,'(i80)',iostat=ios) pid
!!          if(ios.ne.0)then
!!             write(*,*)'bad PID=',trim(argument)
!!          else
!!             write(*,*)'kill SIGNAL=',signal,' PID=',pid
!!          ! send signal SIGNAL to pid PID
!!             ierr=system_kill(pid,signal)
!!          ! write message if an error was detected
!!             if(ierr.ne.0)then
!!                call system_perror('*demo_system_kill*')
!!             endif
!!          endif
!!       enddo
!!    end program demo_system_kill
!!
!!##SEE ALSO
!!    getpid(), raise(), setsid(), sigaction(), sigqueue(),
!===================================================================================================================================
character(len=*),parameter :: ident_kill="@(#)M_system::system_kill(3f):call kill(3c) to send a signal to a process"
! int kill(pid_t pid, int sig);
interface
   function system_kill(c_pid,c_signal) bind(c,name="kill") result(c_ierr)
      import c_int
      integer(kind=c_int),value,intent(in)   :: c_pid
      integer(kind=c_int),value,intent(in)   :: c_signal
      integer(kind=c_int)                    :: c_ierr
   end function
end interface
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    system_errno(3f) - [M_system] XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!!##SYNOPSIS
!!
!!    integer(kind=c_int) function system_errno()
!!
!!##DESCRIPTION
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_system_errno
!!    use M_system, only: system_errno
!!    implicit none
!!    end program demo_system_errno
!===================================================================================================================================
character(len=*),parameter :: ident_errno="@(#)M_system::system_errno(3f): return errno(3c)"

interface
   integer(kind=c_int) function system_errno() bind (C,name="my_errno")
      import c_int
   end function system_errno
end interface
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    system_geteuid(3f) - [M_system]get effective UID of current process from Fortran by calling geteuid(3c)
!!##SYNOPSIS
!!
!!    integer(kind=c_int) function system_geteuid()
!!
!!##DESCRIPTION
!!        The system_geteuid(3f) function shall return the effective user ID of the calling process.
!!        The geteuid() function shall always be successful and no return value is reserved to indicate the error.
!!##EXAMPLE
!!
!!   Get group ID from Fortran:
!!
!!    program demo_system_geteuid
!!    use M_system, only: system_geteuid
!!    implicit none
!!       write(*,*)'EFFECTIVE UID=',system_geteuid()
!!    end program demo_system_geteuid
!===================================================================================================================================
character(len=*),parameter :: ident_euid="@(#)M_system::system_geteuid(3f): call geteuid(3c)"
interface
   integer(kind=c_int) function system_geteuid() bind (C,name="geteuid")
      import c_int
   end function system_geteuid
end interface
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    system_getuid(3f) - [M_system]get real UID of current process from Fortran by calling getuid(3c)
!!##SYNOPSIS
!!
!!    integer(kind=c_int) function system_getuid()
!!
!!##DESCRIPTION
!!        The system_getuid(3f) function shall return the real user ID
!!        of the calling process.  The getuid() function shall always be
!!        successful and no return value is reserved to indicate the error.
!!##EXAMPLE
!!
!!   Get group ID from Fortran:
!!
!!    program demo_system_getuid
!!    use M_system, only: system_getuid
!!    implicit none
!!       write(*,*)'UID=',system_getuid()
!!    end program demo_system_getuid
!===================================================================================================================================
character(len=*),parameter :: ident_uid="@(#)M_system::system_getuid(3f): call getuid(3c)"
interface
   integer(kind=c_int) function system_getuid() bind (C,name="getuid")
      import c_int
   end function system_getuid
end interface
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    system_getegid(3f) - [M_system]get the effective group ID (GID) of current process from Fortran by calling getegid(3c)
!!##SYNOPSIS
!!
!!    integer(kind=c_int) function system_getegid()
!!##DESCRIPTION
!!        The getegid() function returns the effective group ID of the
!!        calling process.
!!
!!##RETURN VALUE
!!        The getegid() should always be successful and no return value is
!!        reserved to indicate an error.
!!
!!##ERRORS
!!        No errors are defined.
!!
!!##SEE ALSO
!!        getegid(), system_geteuid(), getuid(), setegid(), seteuid(), setgid(),
!!        setregid(), setreuid(), setuid()
!!
!!##EXAMPLE
!!
!!   Get group ID from Fortran
!!
!!    program demo_system_getegid
!!    use M_system, only: system_getegid
!!    implicit none
!!       write(*,*)'GID=',system_getegid()
!!    end program demo_system_getegid
!===================================================================================================================================
character(len=*),parameter :: ident_egid="@(#)M_system::system_getegid(3f): call getegid(3c)"
interface
   integer(kind=c_int) function system_getegid() bind (C,name="getegid")
      import c_int
   end function system_getegid
end interface
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    system_getgid(3f) - [M_system]get the real group ID (GID) of current process from Fortran by calling getgid(3c)
!!##SYNOPSIS
!!
!!    integer(kind=c_int) function system_getgid()
!!##DESCRIPTION
!!        The getgid() function returns the real group ID of the calling process.
!!
!!##RETURN VALUE
!!        The getgid() should always be successful and no return value is
!!        reserved to indicate an error.
!!
!!##ERRORS
!!        No errors are defined.
!!
!!##SEE ALSO
!!        getegid(), system_geteuid(), getuid(), setegid(), seteuid(), setgid(),
!!        setregid(), setreuid(), setuid()
!!
!!##EXAMPLE
!!
!!   Get group ID from Fortran
!!
!!    program demo_system_getgid
!!    use M_system, only: system_getgid
!!    implicit none
!!       write(*,*)'GID=',system_getgid()
!!    end program demo_system_getgid
!===================================================================================================================================
character(len=*),parameter :: ident_gid="@(#)M_system::system_getgid(3f): call getgid(3c)"
interface
   integer(kind=c_int) function system_getgid() bind (C,name="getgid")
      import c_int
   end function system_getgid
end interface
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!        system_getsid(3f) - [M_system]get the process group ID of a session leader
!!##SYNOPSIS
!!
!!        integer(kind=c_int) function system_getsid(pid)
!!        integer(kind=c_int) :: pid
!!##DESCRIPTION
!!        The system_getsid() function obtains the process group ID of the
!!        process that is the session leader of the process specified by pid.
!!        If pid is 0, it specifies the calling process.
!!##RETURN VALUE
!!        Upon successful completion, system_getsid() shall return the process group
!!        ID of the session leader of the specified process.  Otherwise,
!!        it shall return -1 and set errno to indicate the error.
!!##EXAMPLE
!!
!!   Get SID from Fortran
!!
!!    program demo_system_getsid
!!    use M_system, only: system_getsid
!!    implicit none
!!       write(*,*)'SID=',system_getsid(0_c_int)
!!    end program demo_system_getsid
!===================================================================================================================================
character(len=*),parameter :: ident_getsid="@(#)M_system::system_getsid(3f): call getsid(3c) to get session leader for given pid"
interface
   integer(kind=c_int) function system_getsid(c_pid) bind (C,name="getsid")
      import c_int
      integer(kind=c_int) :: c_pid
   end function system_getsid
end interface
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    system_getpid(3f) - [M_system]get PID (process ID) of current process from Fortran by calling getpid(3c)
!!##SYNOPSIS
!!
!!    integer function system_getpid()
!!##DESCRIPTION
!!        The system_getpid() function returns the process ID of the
!!        calling process.
!!##RETURN VALUE
!!        The value returned is the integer process ID.  The system_getpid()
!!        function shall always be successful and no return value is reserved
!!        to indicate an error.
!!##EXAMPLE
!!
!!   Get process PID from Fortran
!!
!!    program demo_system_getpid
!!    use M_system, only: system_getpid
!!    implicit none
!!       write(*,*)'PID=',system_getpid()
!!    end program demo_system_getpid
!===================================================================================================================================
character(len=*),parameter :: ident_pid="@(#)M_system::system_getpid(3f): call getpid(3c)"

interface
   integer(kind=c_int) function system_getpid() bind (C,name="getpid")
      import c_int
   end function system_getpid
end interface
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    system_getppid(3f) - [M_system]get parent process ID (PPID) of current process from Fortran by calling getppid(3c)
!!##SYNOPSIS
!!
!!    integer(kind=c_int) function system_getppid()
!!##DESCRIPTION
!!        The system_getppid() function returns the parent process ID of
!!        the calling process.
!!
!!##RETURN VALUE
!!        The system_getppid() function should always be successful and no
!!        return value is reserved to indicate an error.
!!
!!##ERRORS
!!        No errors are defined.
!!
!!##SEE ALSO
!!        exec, fork(), getpgid(), getpgrp(), getpid(), kill(),
!!        setpgid(), setsid()
!!
!!##EXAMPLE
!!
!!   Get parent process PID (PPID) from Fortran
!!
!!    program demo_system_getppid
!!    use M_system, only: system_getppid
!!    implicit none
!!
!!    write(*,*)'PPID=',system_getppid()
!!
!!    end program demo_system_getppid
!===================================================================================================================================
character(len=*),parameter :: ident_ppid="@(#)M_system::system_getppid(3f): call getppid(3c)"
interface
   integer(kind=c_int) function system_getppid() bind (C,name="getppid")
   import c_int
   end function system_getppid
end interface
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    system_umask(3fp) - [M_system] XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!!##SYNOPSIS
!!
!!    integer(kind=c_int) function system_umask(umask_value)
!!
!!##DESCRIPTION
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_system_umask
!!    use M_system, only: system_umask
!!    implicit none
!!    end program demo_system_umask
!===================================================================================================================================
character(len=*),parameter :: ident_umask="@(#)M_system::system_umask(3f): call umask(3c)"
interface
   integer(kind=c_int) function system_umask(umask_value) bind (C,name="umask")
   import c_int
   integer(kind=c_int),value :: umask_value
   end function system_umask
end interface
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    system_rand(3f) - [M_system]call pseudo-random number generator rand(3c)
!!##SYNOPSIS
!!
!!    integer(kind=c_int) :: function system_rand()
!!##DESCRIPTION
!!    Use rand(3c) to generate pseudo-random numbers.
!!
!!##EXAMPLE
!!
!!    Sample program:
!!
!!       program demo_system_srand
!!       use M_system, only : system_srand, system_rand
!!       implicit none
!!       integer :: i
!!
!!       call system_srand(1001)
!!       do i=1,10
!!          write(*,*)system_rand()
!!       enddo
!!       write(*,*)
!!
!!       end program demo_system_srand
!!   expected results:
!!
!!      1512084687
!!      1329390995
!!      1874040748
!!        60731048
!!       239808950
!!      2017891911
!!        22055588
!!      1105177318
!!       347750200
!!      1729645355
!!
!!      1512084687
!!      1329390995
!!      1874040748
!!        60731048
!!       239808950
!!      2017891911
!!        22055588
!!      1105177318
!!       347750200
!!      1729645355
!===================================================================================================================================
character(len=*),parameter :: ident_rand="@(#)M_system::system_rand(3f): call rand(3c)"
interface
   integer(kind=c_int) function system_rand() bind (C,name="rand")
      import c_int
   end function system_rand
end interface
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
interface
  subroutine c_flush() bind(C,name="my_flush")
  end subroutine c_flush
end interface
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    system_initenv(3f) - [M_system]initialize environment table pointer and size so table can be read by readenv(3f)
!!##SYNOPSIS
!!
!!       subroutine system_initenv()
!!##DESCRIPTION
!!    A simple interface allows reading the environment variable table
!!    of the process. Call system_initenv(3f) to initialize reading the
!!    environment table, then call system_readenv(3f) until a blank line
!!    is returned. If more than one thread reads the environment or the
!!    environment is changed while being read the results are undefined.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_system_initenv
!!    use M_system, only : system_initenv, system_readenv
!!    character(len=:),allocatable :: string
!!       call system_initenv()
!!       do
!!          string=system_readenv()
!!          if(string.eq.'')then
!!             exit
!!          else
!!             write(*,'(a)')string
!!          endif
!!       enddo
!!    end program demo_system_initenv
!!
!!   Sample results:
!!
!!    USERDOMAIN_ROAMINGPROFILE=buzz
!!    HOMEPATH=\Users\JSU
!!    APPDATA=C:\Users\JSU\AppData\Roaming
!!    MANPATH=/home/urbanjs/V600/LIBRARY/libjust4/download/tmp/man:/home/urbanjs/V600/doc/man:::
!!    DISPLAYNUM=0
!!    ProgramW6432=C:\Program Files
!!    HOSTNAME=buzz
!!    XKEYSYMDB=/usr/share/X11/XKeysymDB
!!    PUBLISH_CMD=
!!    OnlineServices=Online Services
!!         :
!!         :
!!         :
!===================================================================================================================================
character(len=*),parameter :: ident_initenv="@(#)M_system::system_initenv(3f): initialize environment table for reading"
integer(kind=c_long),bind(c,name="longest_env_variable") :: longest_env_variable
interface
   subroutine system_initenv() bind (C,NAME='my_initenv')
   end subroutine system_initenv
end interface
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!!type(c_ptr),bind(c,name="environ") :: c_environ

integer(kind=c_int),bind(c,name="FS_IRGRP") ::R_GRP
integer(kind=c_int),bind(c,name="FS_IROTH") ::R_OTH
integer(kind=c_int),bind(c,name="FS_IRUSR") ::R_USR
integer(kind=c_int),bind(c,name="FS_IRWXG") ::R_WXG
integer(kind=c_int),bind(c,name="FS_IRWXO") ::R_WXO
integer(kind=c_int),bind(c,name="FS_IRWXU") ::R_WXU
integer(kind=c_int),bind(c,name="FS_IWGRP") ::W_GRP
integer(kind=c_int),bind(c,name="FS_IWOTH") ::W_OTH
integer(kind=c_int),bind(c,name="FS_IWUSR") ::W_USR
integer(kind=c_int),bind(c,name="FS_IXGRP") ::X_GRP
integer(kind=c_int),bind(c,name="FS_IXOTH") ::X_OTH
integer(kind=c_int),bind(c,name="FS_IXUSR") ::X_USR
integer(kind=c_int),bind(c,name="FDEFFILEMODE") :: DEFFILEMODE
integer(kind=c_int),bind(c,name="FACCESSPERMS") :: ACCESSPERMS

! Host names are limited to {HOST_NAME_MAX} bytes.
integer(kind=c_int),bind(c,name="FHOST_NAME_MAX") :: HOST_NAME_MAX
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
contains
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!        system_link(3f) - [M_system]link one file to another file relative to two directory file descriptors
!!
!!##SYNOPSIS
!!
!!    integer function link(oldpath,newpath);
!!
!!     character(len=*),intent(in) :: oldpath
!!     character(len=*),intent(in) :: newpath
!!
!!##DESCRIPTION
!!        The link() function shall create a new link (directory entry) for the existing file, path1.
!!
!!        The  path1  argument points to a pathname naming an existing file. The path2 argument points to a pathname naming the new
!!        directory entry to be created. The link() function shall atomically create a new link for the existing file and the  link
!!        count of the file shall be incremented by one.
!!
!!        If  path1  names a directory, link() shall fail unless the process has appropriate privileges and the implementation
!!        supports
!!        using link() on directories.
!!
!!        If path1 names a symbolic link, it is implementation-defined whether link() follows the symbolic link, or creates  a  new
!!        link to the symbolic link itself.
!!
!!        Upon  successful  completion,  link()  shall mark for update the last file status change timestamp of the file. Also, the
!!        last data modification and last file status change timestamps of the directory that  contains  the  new  entry  shall  be
!!        marked for update.
!!
!!        If link() fails, no link shall be created and the link count of the file shall remain unchanged.
!!
!!        The implementation may require that the calling process has permission to access the existing file.
!!
!!        The linkat() function shall be equivalent to the link() function except that symbolic links shall be handled as specified
!!        by the value of flag (see below) and except in the case where either path1 or path2 or both are relative paths.  In  this
!!        case  a  relative  path path1 is interpreted relative to the directory associated with the file descriptor fd1 instead of
!!        the current working directory and similarly for path2 and the file descriptor fd2.  If the  file  descriptor  was  opened
!!        without  O_SEARCH, the function shall check whether directory searches are permitted using the current permissions of the
!!        directory underlying the file descriptor. If the file descriptor was opened with O_SEARCH, the function shall not perform
!!        the check.
!!
!!        Values for flag are constructed by a bitwise-inclusive OR of flags from the following list, defined in <fcntl.h>:
!!
!!        AT_SYMLINK_FOLLOW
!!              If path1 names a symbolic link, a new link for the target of the symbolic link is created.
!!
!!        If linkat() is passed the special value AT_FDCWD in the fd1 or fd2 parameter, the current working directory shall be used
!!        for the respective path argument. If both fd1 and fd2 have value AT_FDCWD, the behavior shall be identical to a  call  to
!!        link(), except that symbolic links shall be handled as specified by the value of flag.
!!
!!        Some implementations do allow links between file systems.
!!
!!        If path1 refers to a symbolic link, application developers should use linkat() with appropriate flags to  select  whether
!!        or not the symbolic link should be resolved.
!!
!!        If  the  AT_SYMLINK_FOLLOW flag is clear in the flag argument and the path1 argument names a symbolic link, a new link is
!!        created for the symbolic link path1 and not its target.
!!
!!##RETURN VALUE
!!        Upon successful completion, these functions shall return 0. Otherwise, these functions shall return -1 and set  errno  to
!!        indicate the error.
!!
!!##EXAMPLES
!!
!!   Creating a Link to a File
!!
!!    program demo_system_link
!!    use M_system, only : system_link
!!    ierr = system_link('myfile1','myfile2')
!!    if(ierr.ne.0)then
!!       call system_perror('*demo_system_link*')
!!    endif
!!    end program demo_system_link
!===================================================================================================================================
function system_link(oldname,newname) result(ierr)
character(len=*),parameter :: ident="@(#)M_system::system_link(3f): call link(3c) to create a file link"
character(len=*),intent(in) :: oldname
character(len=*),intent(in) :: newname
integer                     :: ierr
integer(kind=c_int)         :: c_ierr

interface
  function c_link(c_oldname,c_newname) bind (C,name="link") result (c_ierr)
  import c_char,c_int
  character(kind=c_char,len=1),intent(in) :: c_oldname(*)
  character(kind=c_char,len=1),intent(in) :: c_newname(*)
  integer(kind=c_int)                     :: c_ierr
  end function c_link
end interface

   c_ierr=c_link(str2arr(oldname),str2arr(newname))
   ierr=c_ierr

end function system_link
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!        system_unlink(3f) - [M_system]remove a directory entry relative to directory file descriptor
!!
!!##SYNOPSIS
!!
!!    integer function unlink(path);
!!
!!     character(len=*) :: path
!!
!!##DESCRIPTION
!!    The unlink() function shall remove a link to a file. If path names a
!!    symbolic link, unlink() shall remove the symbolic link named by path
!!    and shall not affect any file or directory named by the contents of
!!    the symbolic link. Otherwise, unlink() shall remove the link named by
!!    the pathname pointed to by path and shall decrement the link count of
!!    the file referenced by the link.
!!
!!    When the file's link count becomes 0 and no process has the file open,
!!    the space occupied by the file shall be freed and the file shall no
!!    longer be accessible. If one or more processes have the file open when
!!    the last link is removed, the link shall be removed before unlink()
!!    returns, but the removal of the file contents shall be postponed until
!!    all refer- ences to the file are closed.
!!
!!    The path argument shall not name a directory unless the process has
!!    appropriate privileges and the implementation sup- ports using unlink()
!!    on directories.
!!
!!    Upon successful completion, unlink() shall mark for update the last
!!    data modification and last file status change time- stamps of the parent
!!    directory. Also, if the file's link count is not 0, the last file status
!!    change timestamp of the file shall be marked for update.
!!
!!    Values for flag are constructed by a bitwise-inclusive OR of flags from
!!    the following list, defined in <fcntl.h>:
!!
!!       AT_REMOVEDIR
!!
!!     Remove the directory entry specified by fd and path as a
!!     directory, not a normal file.
!!
!!##RETURN VALUE
!!
!!    Upon successful completion, these functions shall return 0. Otherwise,
!!    these functions shall return -1 and set  errno  to indicate the error. If
!!    -1 is returned, the named file shall not be changed.
!!
!!##EXAMPLES
!!
!!   Removing a link to a file
!!
!!    program demo_system_unlink
!!    use M_system, only : system_unlink
!!    ierr = system_unlink('myfile1')
!!    if(ierr.ne.0)then
!!       call system_perror('*demo_system_unlink*')
!!    endif
!!    end program demo_system_unlink
!===================================================================================================================================
function system_unlink(fname) result (ierr)
character(len=*),parameter :: ident="@(#)M_system::system_unlink(3f): call unlink(3c) to rm file link"
character(len=*),intent(in) :: fname
integer                     :: ierr

interface
  function c_unlink(c_fname) bind (C,name="unlink") result (c_ierr)
  import c_char, c_int
  character(kind=c_char,len=1) :: c_fname(*)
  integer(kind=c_int)          :: c_ierr
  end function c_unlink
end interface
   ierr=c_unlink(str2arr(fname))
end function system_unlink
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    system_setumask(3f) - [M_system]set the file mode creation umask
!!##SYNOPSIS
!!
!!    integer function system_setumask(new_umask) result (old_umask)
!!
!!     integer,intent(in)  :: new_umask
!!     integer(kind=c_int) :: umask_c
!!
!!##DESCRIPTION
!!        The  umask()  function sets the file mode creation mask of the
!!        process to cmask and return the previous value of the mask. Only
!!        the file permission bits of cmask (see <sys/stat.h>) are used;
!!        the meaning of the other  bits  is  implementa- tion-defined.
!!
!!        The file mode creation mask of the process is used to turn off
!!        permission bits in the mode argument supplied during calls to
!!        the following functions:
!!
!!         *  open(), openat(), creat(), mkdir(), mkdirat(), mkfifo(), and mkfifoat()
!!         *  mknod(), mknodat()
!!         *  mq_open()
!!         *  sem_open()
!!
!!        Bit positions that are set in cmask are cleared in the mode of
!!        the created file.
!!
!!##RETURN VALUE
!!        The file permission bits in the value returned by umask() shall be
!!        the previous value of the file mode creation mask. The state of any
!!        other bits in that value is unspecified, except that a subsequent
!!        call to umask() with the returned value as cmask shall leave the
!!        state of the mask the same as its state before the first call,
!!        including  any  unspecified  use  of those bits.
!!
!!##ERRORS
!!        No errors are defined.
!!
!!##EXAMPLE
!!
!!   Sample program
!!
!!    program demo_umask
!!    use M_system, only : system_getumask, system_setumask
!!    integer :: newmask
!!    write(*,101)(system_getumask(),i=1,4)
!!    101 format(1x,i0,1x,"O'",o4.4,"'",1x,'Z"',z0,"'",1x,"B'",b12.12,"'")
!!    newmask=63
!!    old_umask=system_setumask(newmask)
!!    write(*,*)'NEW'
!!    write(*,101)(system_getumask(),i=1,4)
!!    end program demo_umask
!!
!!   Expected output
!!
!!     18 O'022' Z"12' B'000010010"
!!     NEW
!!     63 O'077' Z"3F' B'000111111"
!===================================================================================================================================
integer function system_setumask(umask_value) result (old_umask)
integer,intent(in)  :: umask_value
integer(kind=c_int) :: umask_c

   umask_c=umask_value
   old_umask=system_umask(umask_c) ! set current umask

end function system_setumask
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    system_getumask(3f) - [M_system]get current umask
!!##SYNOPSIS
!!
!!   integer function system_getumask() result (umask_value)
!!##DESCRIPTION
!!   The return value from getumask(3f) is the value of the file
!!   creation mask, obtained by using umask(3c).
!!##EXAMPLE
!!
!!   Sample program
!!
!!    program demo_umask
!!    use M_system, only : system_getumask, system_setumask
!!    integer :: newmask
!!    write(*,101)(system_getumask(),i=1,4)
!!    101 format(1x,i0,1x,"O'",o4.4,"'",1x,'Z"',z0,"'",1x,"B'",b12.12,"'")
!!    end program demo_umask
!!
!!   Expected output
!!
!!     18 O'022' Z"12' B'000010010"
!===================================================================================================================================
integer function system_getumask() result (umask_value)
! The return value from umask() is just the previous value of the file
! creation mask, so that this system call can be used both to get and
! set the required values. Sadly, however, there is no way to get the old
! umask value without setting a new value at the same time.

! This means that in order just to see the current value, it is necessary
! to execute a piece of code like the following function:
integer             :: idum
integer(kind=c_int) :: old_umask
   old_umask=system_umask(0_c_int) ! get current umask but by setting umask to 0 (a conservative mask so no vulnerability is open)
   idum=system_umask(old_umask)      ! set back to original mask
   umask_value=old_umask
end function system_getumask
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!      perror(3f) - [M_system] print error message for last C error on stderr
!!##SYNOPSIS
!!
!!      subroutine system_perror(prefix)
!!
!!       character(len=*),intent(in) :: prefix
!!
!!##DESCRIPTION
!!    Use system_perror(3f) to print an error message on stderr
!!    corresponding to the current value of the C global variable errno.
!!    Unless you use NULL as the argument prefix, the error message will
!!    begin with the prefix string, followed by a colon and a space
!!    (:). The remainder of the error message produced is one of the
!!    strings described for strerror(3c).
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_system_perror
!!    use M_system, only : system_perror,system_rmdir
!!    ! generate an error with a routine that supports errno and perror(3c)
!!    ierr=system_rmdir('/NOT/THERE/OR/ANYWHERE')
!!    call system_perror('*demo_system_perror*')
!!    end program demo_system_perror
!!
!!   Expected results:
!!
!!    *demo_system_perror*: No such file or directory
!===================================================================================================================================
subroutine system_perror(prefix)
use iso_fortran_env, only : ERROR_UNIT, INPUT_UNIT, OUTPUT_UNIT     ! access computing environment
character(len=*),parameter :: ident="@(#)M_system::system_perror(3f): call perror(3c) to display error message"
character(len=*),intent(in) :: prefix
   integer                  :: ios

interface
  subroutine c_perror(c_prefix) bind (C,name="perror")
  import c_char
  character(kind=c_char) :: c_prefix(*)
  end subroutine c_perror
end interface

   flush(unit=ERROR_UNIT,iostat=ios)
   flush(unit=OUTPUT_UNIT,iostat=ios)
   flush(unit=INPUT_UNIT,iostat=ios)
   call c_perror(str2arr(prefix))
   call c_flush()

end subroutine system_perror
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    system_chdir(3f) - [M_system]call chdir(3c) from Fortran to change working directory
!!##SYNOPSIS
!!
!!    subroutine system_chdir(path, err)
!!
!!     character(len=*)               :: path
!!     integer, optional, intent(out) :: err
!!
!!##DESCRIPTION
!!
!!    system_chdir(3f) changes the current working directory of the calling
!!    process to the directory specified in path. The current working
!!    directory is the starting point for interpreting relative pathnames
!!    (those not starting with '/').
!!
!!##RETURN VALUE
!!
!!    On success, zero is returned. On error, -1 is returned, and errno is
!!    set appropriately.
!!
!!
!!    Depending on the file system, other errors can be returned. The more
!!    general errors for chdir() are listed below, by their C definitions:
!!
!!    Errors
!!    EACCES        Search permission is denied for one of the components of path.
!!                  (See also path_resolution(7).)
!!    EFAULT        path points outside your accessible address space.
!!    EIO           An I/O error occurred.
!!    ELOOP         Too many symbolic links were encountered in resolving path.
!!    ENAMETOOLONG  path is too long.
!!    ENOENT        The file does not exist.
!!    ENOMEM        Insufficient kernel memory was available.
!!    ENOTDIR       A component of path is not a directory.
!!
!!##SEE ALSO
!!
!!    chroot(2), getcwd(3), path_resolution(7)
!!
!!##EXAMPLE
!!
!!    Change working directory from Fortran
!!
!!      program demo_system_chdir
!!      use M_system, only : system_chdir
!!      implicit none
!!      integer :: ierr
!!
!!      call execute_command_line('pwd')
!!      call system_chdir('/tmp',ierr)
!!      call execute_command_line('pwd')
!!      write(*,*)'*CHDIR TEST* IERR=',ierr
!!
!!      end program demo_system_chdir
!!
!!##RESULTS:
!!   Sample run output:
!!
!!      /home/urbanjs/V600
!!      /tmp
!!      *CHDIR TEST* IERR=           0
!===================================================================================================================================
subroutine system_chdir(path, err)
character(len=*),parameter :: ident="@(#)M_system::system_chdir(3f): call chdir(3c)"
character(len=*)               :: path
integer, optional, intent(out) :: err

interface
   integer(kind=c_int)  function c_chdir(c_path) bind(C,name="chdir")
      import c_char, c_int
      character(kind=c_char)   :: c_path(*)
   end function
end interface
   integer                     :: loc_err
!-----------------------------------------------------------------------------------------------------------------------------------
   loc_err=c_chdir(str2arr(trim(path)))
   if(present(err))then
      err=loc_err
   endif
end subroutine system_chdir
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!      system_remove(3f) - [M_system]call remove(3c) to remove file
!!##SYNOPSIS
!!
!!   function system_remove(path) result(err)
!!
!!    character(*),intent(in) :: path
!!    integer(c_int)          :: err
!!
!!##DESCRIPTION
!!    Fortran supports scratch files via the OPEN(3c) command; but does
!!    not otherwise allow for removing files. The system_remove(3f) command
!!    allows for removing files by name that the user has the authority to
!!    remove by calling the C remove(3c) function.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_system_remove
!!    use M_system, only : system_remove
!!    character(len=*),parameter :: FILE='MyJunkFile.txt'
!!    write(*,*)'BEFORE CREATED '//FILE
!!    call execute_command_line('ls -l '//FILE)
!!    write(*,*)
!!
!!    ! note intentionally causes error if file exists
!!    open(unit=10,file=FILE,status='NEW')
!!    write(*,*)'AFTER OPENED '//FILE
!!    call execute_command_line('ls -l '//FILE)
!!    write(*,*)
!!
!!    write(10,'(a)') 'This is a file I want to delete'
!!    close(unit=10)
!!    write(*,*)'AFTER CLOSED '
!!    call execute_command_line('ls -l '//FILE)
!!    write(*,*)
!!
!!    ierr=system_remove(FILE)
!!    write(*,*)'AFTER REMOVED',IERR
!!    call execute_command_line('ls -l '//FILE)
!!    write(*,*)
!!
!!    end program demo_system_remove
!!
!!   Expected Results:
!!
!!    >  BEFORE CREATED MyJunkFile.txt
!!    > ls: cannot access 'MyJunkFile.txt': No such file or directory
!!    >
!!    >  AFTER OPENED MyJunkFile.txt
!!    > -rw-r--r-- 1 JSU None 0 Nov 19 19:32 MyJunkFile.txt
!!    >
!!    >  AFTER CLOSED
!!    > -rw-r--r-- 1 JSU None 32 Nov 19 19:32 MyJunkFile.txt
!!    >
!!    >  AFTER REMOVED           0
!!    > ls: cannot access 'MyJunkFile.txt': No such file or directory
!!
!===================================================================================================================================
function system_remove(path) result(err)
character(len=*),parameter :: ident="@(#)M_system::system_remove(3f): call remove(3c) to remove file"
character(*),intent(in) :: path
integer(c_int)          :: err

interface
   function c_remove(c_path) bind(c,name="remove") result(c_err)
      import c_char,c_int
      character(kind=c_char,len=1),intent(in) :: c_path(*)
      integer(c_int)                          :: c_err
   end function
end interface
!-----------------------------------------------------------------------------------------------------------------------------------
   err= c_remove(str2arr(trim(path)))
end function system_remove
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!      system_rename(3f) - [M_system]call rename(3c) to rename a system file
!!##SYNOPSIS
!!
!!   function system_rename(input,output) result(ierr)
!!
!!    character(*),intent(in)    :: input,output
!!    integer                    :: ierr
!!##DESCRIPTION
!!     Rename a file by calling rename(3c). It is not recommended that the
!!     rename occur while either filename is being used on a file currently
!!     OPEN(3f) by the program.
!!##OPTIONS
!!     INPUT   system filename of an existing file to rename
!!     OUTPUT  system filename to be created or overwritten by INPUT file
!!##RETURNS
!!     IERR    zero (0) if no error occurs. If not zero a call to
!!             system_errno(3f) or system_perror(3f) is supported
!!             to diagnose error
!!##EXAMPLE
!!
!!    Sample program:
!!
!!      program demo_system_rename
!!      use M_system, only : system_rename
!!      use M_system, only : system_remove
!!      use M_system, only : system_perror
!!      implicit none
!!      character(len=256) :: string
!!      integer            :: ios, ierr
!!
!!      ! try to remove junk files just in case
!!      ierr=system_remove('_scratch_file_')
!!      write(*,'(a,i0)') 'should not be zero ',ierr
!!      call system_perror('*demo_system_rename*')
!!      ierr=system_remove('_renamed_scratch_file_')
!!      write(*,'(a,i0)') 'should not be zero ',ierr
!!      call system_perror('*demo_system_rename*')
!!
!!      ! create scratch file to rename
!!      open(unit=10,file='_scratch_file_',status='new')
!!      write(10,'(a)') 'Test by renaming "_scratch_file_" to "_renamed_scratch_file_"'
!!      write(10,'(a)') 'IF YOU SEE THIS ON OUTPUT THE RENAME WORKED'
!!      close(10)
!!      ! rename scratch file
!!      ierr=system_rename('_scratch_file_','_renamed_scratch_file_')
!!      if(ierr.ne.0)then
!!         write(*,*)'ERROR RENAMING FILE ',ierr
!!      endif
!!      ! read renamed file
!!      open(unit=11,file='_renamed_scratch_file_',status='old')
!!      INFINITE: do
!!         read(11,'(a)',iostat=ios)string
!!         if(ios.ne.0)exit INFINITE
!!         write(*,'(a)')trim(string)
!!      enddo INFINITE
!!      close(unit=11)
!!
!!      ! clean up
!!      ierr=system_remove('_scratch_file_')
!!      write(*,'(a,i0)') 'should not be zero ',ierr
!!      ierr=system_remove('_renamed_scratch_file_')
!!      write(*,'(a,i0)') 'should be zero ',ierr
!!
!!      end program demo_system_rename
!!
!!   Expected output:
!!
!!    > should not be zero -1
!!    > *demo_system_rename*: No such file or directory
!!    > should not be zero -1
!!    > *demo_system_rename*: No such file or directory
!!    > Test by renaming "_scratch_file_" to "_renamed_scratch_file_"
!!    > IF YOU SEE THIS ON OUTPUT THE RENAME WORKED
!!    > should not be zero -1
!!    > should be zero 0
!===================================================================================================================================
function system_rename(input,output) result(ierr)
character(len=*),parameter :: ident="@(#)M_system::system_rename(3f): call rename(3c) to change filename"
character(*),intent(in)    :: input,output
integer                    :: ierr
interface
   function c_rename(c_input,c_output) bind(c,name="rename") result(c_err)
      import c_char, c_int
      character(kind=c_char),intent(in) :: c_input(*)
      character(kind=c_char),intent(in) :: c_output(*)
      integer(c_int)                    :: c_err
   end function
end interface
!-----------------------------------------------------------------------------------------------------------------------------------
   ierr= c_rename(str2arr(trim(input)),str2arr(trim(output)))
end function system_rename
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!       system_chmod(3f) - [M_system]call chmod(3c) to change permission mode of a file relative to directory file descriptor
!!##SYNOPSIS
!!
!!    function system_chmod(filename,mode) result(ierr)
!!
!!       character(len=*),intent(in)  :: filename
!!       integer,value,intent(in)     :: mode
!!       integer                      :: ierr
!!
!!##DESCRIPTION
!!        The  chmod() function shall change UID, _ISGID, S_ISVTX, and the
!!        file permission bits of the file named by the path- name pointed
!!        to by the path argument to the corresponding bits in the mode
!!        argument. The application  shall  ensure  that the  effective user
!!        ID of the process matches the owner of the file or the process
!!        has appropriate privileges in order to do this.
!!
!!        S_ISUID, S_ISGID, S_ISVTX, and the file permission bits are
!!        described in <sys/stat.h>.
!!
!!        If the calling process does not have appropriate privileges,
!!        and if the group ID of the file does not match the effective
!!        group ID or one of the supplementary group IDs and if the file
!!        is a regular file, bit S_ISGID (set-group-ID on execution) in the
!!        file's mode shall be cleared upon successful return from chmod().
!!
!!        Additional implementation-defined restrictions may cause the
!!        S_ISUID and S_ISGID bits in mode to be ignored.
!!
!!        Upon successful completion, system_chmod() marks for update the
!!        last file status change timestamp of the file.
!!
!!        Values for flag are constructed by a bitwise-inclusive OR of
!!        flags from the following list, defined in <fcntl.h>:
!!
!!        AT_SYMLINK_NOFOLLOW
!!              If path names a symbolic link, then the mode of the symbolic
!!              link is changed.
!!
!!
!!##RETURN VALUE
!!        Upon  successful completion, system_chmod(3f) returns 0.
!!        Otherwise, it returns -1 and sets errno to indicate the error. If
!!        -1 is returned, no change to the file mode occurs.
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!    program demo_system_chmod
!!    use M_system, only : system_chmod
!!    use M_system, only : R_GRP,R_OTH,R_USR,R_WXG,R_WXO
!!    use M_system, only : R_WXU,W_GRP,W_OTH,W_USR,X_GRP,X_OTH,X_USR
!!    use M_system, only : DEFFILEMODE, ACCESSPERMS
!!    implicit none
!!    integer :: ierr
!!    integer :: status
!!    !Setting Read Permissions for User, Group, and Others
!!    ! The following example sets read permissions for the owner, group, and others.
!!    open(file='_test1',unit=10)
!!    write(10,*)'TEST FILE 1'
!!    close(unit=10)
!!    ierr=system_chmod('_test1', IANY(R_USR,R_GRP,R_OTH))
!!
!!    !Setting Read, Write, and Execute Permissions for the Owner Only
!!    ! The following example sets read, write, and execute permissions for the owner, and no permissions for group and others.
!!    open(file='_test2',unit=10)
!!    write(10,*)'TEST FILE 2'
!!    close(unit=10)
!!    ierr=system_chmod('_test2', R_WXU)
!!
!!    !Setting Different Permissions for Owner, Group, and Other
!!    ! The  following  example  sets owner permissions for CHANGEFILE to read, write, and execute, group permissions to read and
!!    ! execute, and other permissions to read.
!!    open(file='_test3',unit=10)
!!    write(10,*)'TEST FILE 3'
!!    close(unit=10)
!!    ierr=system_chmod('_test3', IANY([R_WXU,R_GRP,X_GRP,R_OTH]));
!!
!!    !Setting and Checking File Permissions
!!    ! The following example sets the file permission bits for a file named /home/cnd/mod1, then calls the  stat()  function  to
!!    ! verify the permissions.
!!
!!    ierr=system_chmod("home/cnd/mod1", IANY([R_WXU,R_WXG,R_OTH,W_OTH]));
!!    status = stat("home/cnd/mod1", buffer;);
!!
!!    ! In  order  to  ensure  that the S_ISUID and S_ISGID bits are set, an application requiring this should use stat() after a
!!    ! successful chmod() to verify this.
!!
!!    !    Any files currently open could possibly become invalid if the mode
!!    !    of the  file  is changed  to  a value which would deny access to
!!    !    that process.
!!
!!    end program demo_system_chmod
!!
!===================================================================================================================================
function system_chmod(filename,mode) result(ierr)
   character(len=*),intent(in)  :: filename
   integer,value,intent(in)     :: mode
   integer                      :: ierr
   interface
      function c_chmod(c_filename,c_mode) bind(c,name="chmod") result(c_err)
         import c_char,c_int
         character(kind=c_char),intent(in) :: c_filename(*)
         integer(c_int),value,intent(in)   :: c_mode
         integer(c_int)                    :: c_err
      end function
   end interface
!-----------------------------------------------------------------------------------------------------------------------------------
   ierr=c_chmod(str2arr(trim(filename)),int(mode,kind(0_c_int)))
end function system_chmod
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!       system_getcwd(3f) - [M_system]call getcwd(3c) to get the pathname of the current working directory
!!##SYNOPSIS
!!
!!       subroutine system_getcwd(output,ierr)
!!
!!        character(len=:),allocatable,intent(out) :: output
!!        integer,intent(out)                      :: ierr
!!##DESCRIPTION
!!        system_getcwd(3f) calls the C routine getcwd(3c) to obtain the absolute pathname of the current working directory.
!!
!!##RETURN VALUE
!!        FILENAME  The absolute pathname of the current working directory
!!                  The pathname shall contain no components that are dot or dot-dot,
!!                  or are symbolic links.
!!        IERR      is not zero if an error occurs.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!      program demo_system_getcwd
!!      use M_system, only : system_getcwd
!!      implicit none
!!      character(len=:),allocatable :: dirname
!!      integer                      :: ierr
!!      call system_getcwd(dirname,ierr)
!!      if(ierr.eq.0)then
!!         write(*,*)'CURRENT DIRECTORY ',trim(dirname)
!!      else
!!         write(*,*)'ERROR OBTAINING CURRENT DIRECTORY NAME'
!!      endif
!!      end program demo_system_getcwd
!===================================================================================================================================
subroutine system_getcwd(output,ierr)
character(len=*),parameter :: ident="@(#)M_system::system_getcwd(3f):call getcwd(3c) to get pathname of current working directory"
character(len=:),allocatable,intent(out) :: output
integer,intent(out)                      :: ierr
integer(kind=c_long),parameter           :: length=4097_c_long
character(kind=c_char,len=1)             :: buffer(length)
type(c_ptr)                              :: buffer2
interface
   function c_getcwd(buffer,size) bind(c,name="getcwd") result(buffer_result)
      import c_char, c_size_t, c_ptr
      character(kind=c_char) ,intent(out) :: buffer(*)
      integer(c_size_t),value,intent(in)  :: size
      type(c_ptr)                         :: buffer_result
   end function
end interface
!-----------------------------------------------------------------------------------------------------------------------------------
   buffer=' '
   buffer2=c_getcwd(buffer,length)
   if(.not.c_associated(buffer2))then
      output=''
      ierr=-1
   else
      output=trim(arr2str(buffer))
      ierr=0
   endif
end subroutine system_getcwd
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!       system_rmdir(3f) - [M_system]call rmdir(3c) to remove empty directories
!!##SYNOPSIS
!!
!!    function system_rmdir(dirname) result(err)
!!
!!     character(*),intent(in) :: dirname
!!     integer(c_int) :: err
!!##DESCRIPTION
!!        DIRECTORY  The name of a directory to remove if it is empty
!!        err        zero (0) if no error occurred
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_system_rmdir
!!    use M_system, only : system_perror
!!    use M_system, only : system_rmdir, system_mkdir
!!    implicit none
!!    integer :: ierr
!!    write(*,*)'BEFORE TRY TO CREATE _scratch/'
!!    call execute_command_line('ls -ld _scratch')
!!
!!    write(*,*)'TRY TO CREATE _scratch/'
!!    ierr=system_mkdir('_scratch',0+8*0+7)
!!    write(*,*)'IERR=',ierr
!!    call execute_command_line('ls -ld _scratch')
!!
!!    write(*,*)'TRY TO REMOVE _scratch/'
!!    ierr=system_rmdir('_scratch')
!!    write(*,*)'IERR=',ierr
!!    call execute_command_line('ls -ld _scratch')
!!
!!    write(*,*)'TRY TO REMOVE _scratch when it should be gone/'
!!    ierr=system_rmdir('_scratch')
!!    call system_perror('*test of system_rmdir*')
!!    write(*,*)'IERR=',ierr
!!    call execute_command_line('ls -ld _scratch')
!!
!!    end program demo_system_rmdir
!!
!!   Expected output:
!===================================================================================================================================
function system_rmdir(dirname) result(err)
character(len=*),parameter :: ident="@(#)M_system::system_rmdir(3f): call rmdir(3c) to remove empty directory"
character(*),intent(in) :: dirname
integer(c_int) :: err

interface
   function c_rmdir(c_path) bind(c,name="rmdir") result(c_err)
      import c_char,c_int
      character(kind=c_char,len=1),intent(in) :: c_path(*)
      integer(c_int)                          :: c_err
   end function
end interface
!-----------------------------------------------------------------------------------------------------------------------------------
   err= c_rmdir(str2arr(trim(dirname)))
   if(err.ne.0) err=system_errno()
end function system_rmdir
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!        system_mkfifo(3f)  - [M_system]make a FIFO special file relative to directory file descriptor
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!    The mkfifo() function creates a new FIFO special file named by the pathname pointed to by path.
!!    The file permission bits of the new FIFO are initialized from mode.
!!    The file permission bits of the mode argument are modified by the process' file creation mask.
!!
!!    When bits in mode other than the file permission bits are set, the effect is implementation-defined.
!!
!!    If path names a symbolic link, mkfifo() shall fail and set errno to [EEXIST].
!!
!!    The  FIFO's user ID will be set to the process' effective user ID.
!!    The FIFO's group ID shall be set to the group ID of the parent directory or to the effective group ID of the process.
!!    Implementations shall provide a way to initialize the FIFO's group ID to the group ID of the parent directory.
!!    Implementations may, but need not, provide an implementation-defined way to initialize  the FIFO's group ID to the effective group ID of the calling process.
!!
!!    Upon  successful  completion, mkfifo() shall mark for update the last data access, last data modification, and last file status change timestamps of the file.
!!    Also, the last data modification and last file status change timestamps of the directory that contains the new entry shall be marked for update.
!!
!!    Predefined variables are typically used to set permission modes.
!!    You can bytewise-OR together these variables to to create the most common permissions mode:
!!
!!     User:    R_USR  (read),  W_USR  (write),  X_USR(execute)
!!     Group:   R_GRP  (read),  W_GRP  (write),  X_GRP(execute)
!!     Others:  R_OTH  (read),  W_OTH  (write),  X_OTH(execute)
!!
!!    Additionally, some shortcuts are provided (basically a bitwise-OR combination of the above):
!!
!!      Read + Write + Execute: R_WXU (User), R_WXG (Group), R_WXO (Others)
!!      DEFFILEMODE: Equivalent of 0666 =rw-rw-rw-
!!      ACCESSPERMS: Equivalent of 0777 = rwxrwxrwx
!!
!!    Therefore, to give only the user rwx (read+write+execute) rights whereas
!!    group members and others may not do anything, you can use any of the
!!    following mkfifo() calls equivalently:
!!
!!      ierr= mkfifo("myfile", IANY(R_USR, W_USR, X_USR));
!!      ierr= mkfifo("myfile", R_WXU);
!!
!!    In order to give anyone any rights (mode 0777 = rwxrwxrwx), you can
!!    use any of the following calls equivalently:
!!
!!      ierr= mkfifo("myfile",IANY(R_USR,W_USR,X_USR,R_GRP,W_GRP,X_GRP,R_OTH,W_OTH,X_OTH));
!!      ierr= mkfifo("myfile",IANY(R_WXU,R_WXG,R_WXO));
!!      ierr= mkfifo("myfile",ACCESSPERMS);
!!##RETURN VALUE
!!    Upon successful completion, these functions shall return 0.
!!    Otherwise, these functions return -1 and set errno to indicate the error.
!!    If -1 is returned, no FIFO is created.
!!
!!##EXAMPLES
!!
!!   The following example shows how to create a FIFO file named
!!   /home/cnd/mod_done, with read/write permissions for owner, and
!!   with read permissions for group and others.
!!
!!    program demo_system_mkfifo
!!    use M_system, only : system_mkfifo, system_perror
!!    use M_system, only : R_GRP,R_OTH,R_USR,R_WXG,R_WXO
!!    use M_system, only : R_WXU,W_GRP,W_OTH,W_USR,X_GRP,X_OTH,X_USR
!!    use M_system, only : DEFFILEMODE, ACCESSPERMS
!!    implicit none
!!       integer :: status
!!       status = system_mkfifo("/home/cnd/mod_done", IANY([W_USR, R_USR, R_GRP, R_OTH]))
!!       if(status.ne.0)then
!!          call system_perror('*mkfifo* error:')
!!       endif
!!    end program demo_system_mkfifo
!!
!!##EXAMPLE
!!
!!    integer :: ierr
!!    ierr=system_mkfifo('_scratch',IANY(R_USR,W_USER,X_USER))
!!    end program demo_system_mkfifo
!===================================================================================================================================
function system_mkfifo(dirname,mode) result(err)
character(len=*),parameter :: ident="@(#)M_system::system_mkfifo(3f): call mkfifo(3c) to create a new FIFO special file"
character(len=*),intent(in)       :: dirname
integer,intent(in)                :: mode
   integer                        :: c_mode
   integer                        :: err

interface
   function c_mkfifo(c_path,c_mode) bind(c,name="mkfifo") result(c_err)
      import c_char, c_int
      character(len=1,kind=c_char),intent(in) :: c_path(*)
      integer(c_int),intent(in),value         :: c_mode
      integer(c_int)                          :: c_err
   end function c_mkfifo
end interface
!-----------------------------------------------------------------------------------------------------------------------------------
   c_mode=mode
   err= c_mkfifo(str2arr(trim(dirname)),c_mode)
end function system_mkfifo
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!        system_mkdir(3f) - [M_system]call mkdir(3c) to create a new directory
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!
!!    Predefined variables are typically used to set permission modes.
!!    You can bytewise-OR together these variables to to create the most common
!!    permissions mode:
!!
!!     User:    R_USR  (read),  W_USR  (write),  X_USR(execute)
!!     Group:   R_GRP  (read),  W_GRP  (write),  X_GRP(execute)
!!     Others:  R_OTH  (read),  W_OTH  (write),  X_OTH(execute)
!!
!!    Additionally, some shortcuts are provided (basically a bitwise-OR combination of the above):
!!
!!      Read + Write + Execute: R_WXU (User), R_WXG (Group), R_WXO (Others)
!!      DEFFILEMODE: Equivalent of 0666 =rw-rw-rw-
!!      ACCESSPERMS: Equivalent of 0777 = rwxrwxrwx
!!
!!    Therefore, to give only the user rwx (read+write+execute) rights whereas
!!    group members and others may not do anything, you can use any of the
!!    following mkdir() calls equivalently:
!!
!!      ierr= mkdir("mydir", IANY(R_USR, W_USR, X_USR));
!!      ierr= mkdir("mydir", R_WXU);
!!
!!    In order to give anyone any rights (mode 0777 = rwxrwxrwx), you can
!!    use any of the following calls equivalently:
!!
!!      ierr= mkdir("mydir",IANY(R_USR,W_USR,X_USR,R_GRP,W_GRP,X_GRP,R_OTH,W_OTH,X_OTH));
!!      ierr= mkdir("mydir",IANY(R_WXU,R_WXG,R_WXO));
!!      ierr= mkdir("mydir",ACCESSPERMS);
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_system_mkdir
!!    use M_system, only : system_perror
!!    use M_system, only : system_mkdir
!!    use M_system, only : R_GRP,R_OTH,R_USR,R_WXG,R_WXO
!!    use M_system, only : R_WXU,W_GRP,W_OTH,W_USR,X_GRP,X_OTH,X_USR
!!    use M_system, only : DEFFILEMODE, ACCESSPERMS
!!    implicit none
!!    integer :: ierr
!!    ierr=system_mkdir('_scratch',IANY(R_USR,W_USER,X_USER))
!!    end program demo_system_mkdir
!===================================================================================================================================
function system_mkdir(dirname,mode) result(err)
character(len=*),parameter :: ident="@(#)M_system::system_mkdir(3f): call mkdir(3c) to create empty directory"
character(len=*),intent(in)       :: dirname
integer,intent(in)                :: mode
   integer                        :: c_mode
   integer                        :: err

interface
   function c_mkdir(c_path,c_mode) bind(c,name="mkdir") result(c_err)
      import c_char, c_int
      character(len=1,kind=c_char),intent(in) :: c_path(*)
      integer(c_int),intent(in),value         :: c_mode
      integer(c_int)                          :: c_err
   end function c_mkdir
end interface
!-----------------------------------------------------------------------------------------------------------------------------------
   c_mode=mode
   err= c_mkdir(str2arr(trim(dirname)),c_mode)
end function system_mkdir
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    system_opendir(3f) - [M_system]open directory stream by calling opendir(3c)
!!##SYNOPSIS
!!
!!   subroutine system_opendir(dirname,dir,ierr)
!!
!!    character(len=*), intent(in) :: dirname
!!    type(c_ptr)                  :: dir
!!    integer,intent(out)          :: ierr
!!
!!##DESCRIPTION
!!        The system_opendir(3f) procedure opens a directory stream
!!        corresponding to the directory named by the dirname argument.
!!        The directory stream is positioned at the first entry.
!!
!!##RETURN VALUE
!!        Upon successful completion, a pointer to a C dir type is returned.
!!        Otherwise, these functions shall return a null pointer and set
!!        IERR to indicate the error.
!!
!!##ERRORS
!!
!!        An error corresponds to a condition described in opendir(3c):
!!
!!        EACCES    Search permission is denied for the component of the
!!                  path prefix of dirname or read permission is denied
!!                  for dirname.
!!
!!        ELOOP     A loop exists in symbolic links encountered during
!!                  resolution of the dirname argument.
!!
!!        ENAMETOOLONG  The length of a component of a pathname is longer than {NAME_MAX}.
!!
!!        ENOENT        A component of dirname does not name an existing directory or dirname is an empty string.
!!
!!        ENOTDIR       A component of dirname names an existing file that is neither a directory nor a symbolic link to a directory.
!!
!!        ELOOP         More than {SYMLOOP_MAX} symbolic links were encountered during resolution of the dirname argument.
!!
!!        EMFILE        All file descriptors available to the process are currently open.
!!
!!        ENAMETOOLONG  The length of a pathname exceeds {PATH_MAX},
!!                      or pathname resolution of a symbolic link produced an intermediate
!!                      result with a length that exceeds {PATH_MAX}.
!!
!!        ENFILE        Too many files are currently open in the system.
!!
!!##APPLICATION USAGE
!!        The opendir() function should be used in conjunction with readdir(), closedir(), and rewinddir() to examine the contents
!!        of the directory (see the EXAMPLES section in readdir()). This method is recommended for portability.
!!##OPTIONS
!!       dirname name of directory to open a directory stream for
!!##RETURNS
!!       dir   pointer to directory stream. If an
!!             error occurred, it will not be associated.
!!       ierr  0 indicates no error occurred
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_system_opendir
!!    use M_system, only : system_opendir,system_readdir
!!    use M_system, only : system_closedir
!!    use iso_c_binding
!!    implicit none
!!    type(c_ptr)                  :: dir
!!    character(len=:),allocatable :: filename
!!    integer                      :: ierr
!!    !--- open directory stream to read from
!!    call system_opendir('.',dir,ierr)
!!    !--- read directory stream
!!    do
!!       call system_readdir(dir,filename,ierr)
!!       if(filename.eq.' ')exit
!!       write(*,*)filename
!!    enddo
!!    !--- close directory stream
!!    call system_closedir(dir,ierr)
!!    end program demo_system_opendir
!===================================================================================================================================
subroutine system_opendir(dirname,dir,ierr)
character(len=*), intent(in) :: dirname
type(c_ptr)                  :: dir
integer,intent(out)          :: ierr

interface
   function c_opendir(c_dirname) bind(c,name="opendir") result(c_dir)
      import c_char, c_int, c_ptr
      character(kind=c_char),intent(in) :: c_dirname(*)
      type(c_ptr)                       :: c_dir
   end function c_opendir
end interface

   ierr=0
   dir = c_opendir(str2arr(trim(dirname)))
   if(.not.c_associated(dir)) then
      write(*,'(a)')'*system_opendir* Error opening '//trim(dirname)
      ierr=-1
   endif

end subroutine system_opendir
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    system_readdir(3f) - [M_system]read a directory using readdir(3c)
!!##SYNOPSIS
!!
!! subroutine system_readdir(dir,filename,ierr)
!!
!!  type(c_ptr),value                         :: dir
!!  character(len=:),intent(out),allocatable  :: filename
!!  integer,intent(out)                       :: ierr
!!
!!##DESCRIPTION
!!    system_readdir(3f) returns the name of the directory entry at the
!!    current position in the directory stream specified by the argument
!!    DIR, and positions the directory stream at the next entry. It returns
!!    a null name upon reaching the end of the directory stream.
!!
!!    The readdir() function does not return directory entries containing
!!    empty names. If entries for dot or dot-dot exist, one entry is returned
!!    for dot and one entry is returned for dot-dot.
!!
!!    The entry is marked for update of the last data access timestamp each
!!    time it is read.
!!
!!    If IERR is set to non-zero on return, an error occurred.
!!
!!    least {NAME_MAX}+1 elements.
!!
!!    reaching the end of the directory stream, the name is a blank name.
!!
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_system_readdir
!!    use M_system, only : system_opendir,system_readdir
!!    use M_system, only : system_rewinddir,system_closedir
!!    use iso_c_binding
!!    implicit none
!!
!!    type(c_ptr)                  :: dir
!!    character(len=:),allocatable :: filename
!!    integer                      :: i, ierr
!!    !--- open directory stream to read from
!!    call system_opendir('.',dir,ierr)
!!    !--- read directory stream twice
!!    do i=1,2
!!       write(*,'(a,i0)')'PASS ',i
!!       do
!!          call system_readdir(dir,filename,ierr)
!!          if(filename.eq.' ')exit
!!          write(*,*)filename
!!       enddo
!!       call system_rewinddir(dir)
!!    enddo
!!    !--- close directory stream
!!    call system_closedir(dir,ierr)
!!
!!    end program demo_system_readdir
!===================================================================================================================================
subroutine system_readdir(dir,filename,ierr)
type(c_ptr),value                         :: dir
character(len=:),intent(out),allocatable  :: filename
integer,intent(out)                       :: ierr
integer(kind=c_int)                       :: ierr_local

   character(kind=c_char,len=1)           :: buf(4097)

interface
   subroutine c_readdir(c_dir, c_filename,c_ierr) bind (C,NAME='my_readdir')
      import c_char, c_int, c_ptr
      type(c_ptr),value                   :: c_dir
      character(kind=c_char),intent(out)  :: c_filename(*)
      integer(kind=c_int),intent(out)     :: c_ierr
   end subroutine c_readdir
end interface

   buf=' '
   ierr_local=0
   call c_readdir(dir,buf,ierr_local)
   filename=trim(arr2str(buf))
   ierr=ierr_local

end subroutine system_readdir
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!       system_rewinddir(3f) - [M_system]call rewinddir(3c) to rewind directory stream
!!##SYNOPSIS
!!
!!    subroutine system_rewinddir(dir)
!!
!!     type(c_ptr),value :: dir
!!
!!##DESCRIPTION
!!     Return to pointer to the beginning of the list for a currently open directory list.
!!
!!##OPTIONS
!!     DIR  A C_pointer assumed to have been allocated by a call to SYSTEM_OPENDIR(3f).
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_system_rewinddir
!!    use M_system, only : system_opendir,system_readdir
!!    use M_system, only : system_rewinddir,system_closedir
!!    use iso_c_binding
!!    implicit none
!!
!!    type(c_ptr)                  :: dir
!!    character(len=:),allocatable :: filename
!!    integer                      :: i, ierr
!!    !>>> open directory stream to read from
!!    call system_opendir('.',dir,ierr)
!!    !>>> read directory stream twice
!!    do i=1,2
!!       write(*,'(a,i0)')'PASS ',i
!!       do
!!          call system_readdir(dir,filename,ierr)
!!          if(filename.eq.' ')exit
!!          write(*,*)filename
!!       enddo
!!       !>>> rewind directory stream
!!       call system_rewinddir(dir)
!!    enddo
!!    !>>> close directory stream
!!    call system_closedir(dir,ierr)
!!
!!    end program demo_system_rewinddir
!===================================================================================================================================
subroutine system_rewinddir(dir)
type(c_ptr),value            :: dir

interface
   subroutine c_rewinddir(c_dir) bind(c,name="rewinddir")
      import c_char, c_int, c_ptr
      type(c_ptr),value :: c_dir
   end subroutine c_rewinddir
end interface

   call c_rewinddir(dir)

end subroutine system_rewinddir
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!        system_closedir(3f) - [M_system]close a directory stream by calling closedir(3c)
!!##SYNOPSIS
!!
!!        subroutine system_closedir(dir,ierr)
!!
!!         type(c_ptr)         :: dir
!!         integer,intent(out) :: ierr
!!##DESCRIPTION
!!        The SYSTEM_CLOSEDIR(3f) function closes the directory stream referred to by the argument DIR.
!!        Upon return, the value of DIR may no longer point to an accessible object.
!!##OPTIONS
!!        dir     directory stream pointer opened by SYSTEM_OPENDIR(3f).
!!        ierr    Upon successful completion, SYSTEM_CLOSEDIR(3f) returns 0;
!!                otherwise, an error has occurred.
!!##ERRORS
!!        system_closedir(3f) may fail if:
!!
!!        EBADF    The dirp argument does not refer to an open directory stream.
!!        EINTR    The closedir() function was interrupted by a signal.
!!##EXAMPLE
!!
!!   Sample program
!!
!!    program demo_system_closedir
!!    use M_system, only : system_opendir,system_readdir
!!    use M_system, only : system_closedir
!!    use iso_c_binding, only : c_ptr
!!    implicit none
!!    type(c_ptr)                  :: dir
!!    character(len=:),allocatable :: filename
!!    integer                      :: ierr
!!    !--- open directory stream to read from
!!    call system_opendir('.',dir,ierr)
!!    !--- read directory stream
!!    do
!!       call system_readdir(dir,filename,ierr)
!!       if(filename.eq.' ')exit
!!       write(*,*)filename
!!    enddo
!!    call system_rewinddir(dir)
!!    !--- close directory stream
!!    call system_closedir(dir,ierr)
!!    end program demo_system_closedir
!===================================================================================================================================
subroutine system_closedir(dir,ierr)
use iso_c_binding
type(c_ptr),value            :: dir
integer,intent(out),optional :: ierr
   integer                   :: ierr_local

interface
   function c_closedir(c_dir) bind(c,name="closedir") result(c_err)
      import c_char, c_int, c_ptr
      type(c_ptr),value      :: c_dir
      integer(kind=c_int)    :: c_err
   end function c_closedir
end interface

    ierr_local = c_closedir(dir)
    if(present(ierr))then
       ierr=ierr_local
    else
       if(ierr_local /= 0) then
          print *, "*system_closedir* error", ierr_local
          stop 3
       endif
    endif

end subroutine system_closedir
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    system_putenv(3f) - [M_system]set environment variable from Fortran by calling putenv(3c)
!!
!!##SYNOPSIS
!!
!!    subroutine system_putenv(string, err)
!!
!!     character(len=*),intent(in)    :: string
!!     integer, optional, intent(out) :: err
!!
!!##DESCRIPTION
!!    The system_putenv() function adds or changes the value of environment variables.
!!
!!##OPTIONS
!!    string  string of format "NAME=value".
!!            If name does not already exist in the environment, then string is added to the environment.
!!            If name does exist, then the value of name in the environment is changed to value.
!!            The string passed to putenv(3c) becomes part of the environment,
!!            so this routine creates a string each time it is called that increases the amount of
!!            memory the program uses.
!!    err     The system_putenv() function returns zero on success, or nonzero if an error occurs.
!!            A non-zero error usually indicates sufficient memory does not exist to store the
!!            variable.
!!
!!##EXAMPLE
!!
!!   Sample setting an environment variable from Fortran:
!!
!!    program demo_system_putenv
!!    use M_system, only: system_putenv
!!    use iso_c_binding
!!    implicit none
!!    integer :: ierr
!!
!!       write(*,'(a)')'no environment variables containing "GRU":'
!!       call execute_command_line('env|grep GRU')
!!
!!       call system_putenv('GRU=this is the value',ierr)
!!       write(*,'(a,i0)')'now "GRU" should be defined: ',ierr
!!       call execute_command_line('env|grep GRU')
!!
!!       call system_putenv('GRU2=this is the second value',ierr)
!!       write(*,'(a,i0)')'now "GRU" and "GRU2" should be defined: ',ierr
!!       call execute_command_line('env|grep GRU')
!!
!!       call system_putenv('GRU2',ierr)
!!       call system_putenv('GRU',ierr)
!!       write(*,'(a,i0)')'should be gone, varies with different putenv(3c): ',ierr
!!       call execute_command_line('env|grep GRU')
!!       write(*,'(a)')'system_unsetenv(3f) is a better way to remove variables'
!!
!!    end program demo_system_putenv
!!
!!   Results:
!!
!!    no environment variables containing "GRU":
!!    now "GRU" should be defined: 0
!!    GRU=this is the value
!!    now "GRU" and "GRU2" should be defined: 0
!!    GRU2=this is the second value
!!    GRU=this is the value
!!    should be gone, varies with different putenv(3c): 0
!!    system_unsetenv(3f) is a better way to remove variables
!===================================================================================================================================
subroutine system_putenv(string, err)
character(len=*),parameter :: ident="@(#)M_system::system_putenv(3f): call putenv(3c)"

interface
   integer(kind=c_int)  function c_putenv(c_string) bind(C,name="putenv")
      import c_int, c_char
      character(kind=c_char)   :: c_string(*)
   end function
end interface

character(len=*),intent(in)    :: string
integer, optional, intent(out) :: err
   integer                     :: loc_err
   integer                     :: i

   ! PUTENV actually adds the data to the environment so the string passed should be saved or will vanish on exit
   character(len=1,kind=c_char),save, pointer :: memleak(:)

   allocate(memleak(len(string)+1))
   do i=1,len(string)
      memleak(i)=string(i:i)
   enddo
   memleak(len(string)+1)=c_null_char

   loc_err =  c_putenv(memleak)
   if (present(err)) err = loc_err

end subroutine system_putenv
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    set_environment_variable(3f) - [M_system]call setenv(3c) to set environment variable
!!##SYNOPSIS
!!
!!   subroutine set_environment_variable(NAME, VALUE, STATUS)
!!
!!    character(len=*)               :: NAME
!!    character(len=*)               :: VALUE
!!    integer, optional, intent(out) :: STATUS
!!##DESCRIPTION
!!    The set_environment_variable() procedure adds or changes the value of environment variables.
!!
!!    NAME    If name does not already exist in the environment, then string is added to the environment.
!!            If name does exist, then the value of name in the environment is changed to value.
!!    VALUE   Value to assign to environment variable NAME
!!    STATUS  returns zero on success, or nonzero if an error occurs.
!!            A non-zero error usually indicates sufficient memory does not exist to store the
!!            variable.
!!
!!##EXAMPLE
!!
!!   Sample setting an environment variable from Fortran:
!!
!!    program demo_set_environment_variable
!!    use M_system, only: set_environment_variable
!!    use iso_c_binding
!!    implicit none
!!    integer :: ierr
!!
!!       write(*,'(a)')'no environment variables containing "GRU":'
!!       call execute_command_line('env|grep GRU')
!!
!!       call set_environment_variable('GRU','this is the value',ierr)
!!       write(*,'(a,i0)')'now "GRU" should be defined, status=',ierr
!!       call execute_command_line('env|grep GRU')
!!
!!       call set_environment_variable('GRU2','this is the second value',ierr)
!!       write(*,'(a,i0)')'now "GRU" and "GRU2" should be defined, status =',ierr
!!       call execute_command_line('env|grep GRU')
!!
!!    end program demo_set_environment_variable
!!
!!   Results:
!!
!!    no environment variables containing "GRU":
!!    now "GRU" should be defined, status=0
!!    GRU=this is the value
!!    now "GRU" and "GRU2" should be defined, status =0
!!    GRU2=this is the second value
!!    GRU=this is the value
!===================================================================================================================================
subroutine set_environment_variable(NAME, VALUE, STATUS)
character(len=*),parameter :: ident="@(#)M_system::set_environment_variable(3f): call setenv(3c) to set environment variable"
   character(len=*)               :: NAME
   character(len=*)               :: VALUE
   integer, optional, intent(out) :: STATUS
   integer                        :: loc_err

interface
   integer(kind=c_int) function c_setenv(c_name,c_VALUE) bind(C,NAME="setenv")
      import c_int, c_char
      character(kind=c_char)   :: c_name(*)
      character(kind=c_char)   :: c_VALUE(*)
   end function
end interface

   loc_err =  c_setenv(str2arr(trim(NAME)),str2arr(VALUE))
   if (present(STATUS)) STATUS = loc_err
end subroutine set_environment_variable
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    system_clearenv(3f) - [M_system]clear environment by calling clearenv(3c)
!!
!!
!!##SYNOPSIS
!!
!!    subroutine system_clearenv(ierr)
!!
!!     integer,intent(out),optional :: ierr
!!
!!##DESCRIPTION
!!    The clearenv() procedure clears the environment of all name-value pairs.
!!    Typically used in security-conscious applications or ones where configuration
!!    control requires ensuring specific variables are set.
!!
!!##RETURN VALUES
!!    ierr  returns zero on success, and a nonzero value on failure. Optional.
!!          If not present and an error occurs the program stops.
!!
!!##EXAMPLE
!!
!!
!!   Sample program:
!!
!!      program demo_system_clearenv
!!      use M_system, only : system_clearenv
!!      implicit none
!!      ! environment before clearing
!!      call execute_command_line('env|wc')
!!      ! environment after clearing (not necessarily blank!!)
!!      call system_clearenv()
!!      call execute_command_line('env')
!!      end program demo_system_clearenv
!!
!!   Typical output:
!!
!!      89     153    7427
!!      PWD=/home/urbanjs/V600
!!      SHLVL=1
!===================================================================================================================================
subroutine system_clearenv(ierr)
!  emulating because not available on some platforms
character(len=*),parameter :: ident="@(#)M_system::system_clearenv(3f): emulate clearenv(3c) to clear environment"
integer,intent(out),optional    :: ierr
   character(len=:),allocatable :: string
   integer                      :: ierr_local1, ierr_local2
   ierr_local2=0
   INFINITE: do
      call system_initenv()                     ! important -- changing table causes undefined behavior so reset after each unsetenv
      string=system_readenv()                                           ! get first name=value pair
      if(string.eq.'') exit INFINITE
      call system_unsetenv(string(1:index(string,'=')-1) ,ierr_local1)  ! remove first name=value pair
      if(ierr_local1.ne.0)ierr_local2=ierr_local1
   enddo INFINITE
   if(present(ierr))then
      ierr=ierr_local2
   elseif(ierr_local2.ne.0)then                                         ! if error occurs and not being returned, stop
      write(*,*)'*system_clearenv* error=',ierr_local2
      stop
   endif
end subroutine system_clearenv
!--subroutine system_clearenv(ierr)
!--! clearenv(3c) not available on some systems I tried
!--! Found reference that if it is unavailable the assignment "environ = NULL;" will probably do but emulating instead
!--character(len=*),parameter :: ident="@ (#)M_system::system_clearenv(3f): call clearenv(3c) to clear environment"
!--integer,intent(out),optional :: ierr
!--   integer                   :: ierr_local
!--
!--interface
!--   integer(kind=c_int) function c_clearenv() bind(C,NAME="clearenv")
!--   import c_int
!--   end function
!--end interface
!--
!--   ierr_local = c_clearenv()
!--   if(present(ierr))then
!--      ierr=ierr_local
!--   elseif(ierr_local.ne.0)then ! if error occurs and not being returned, stop
!--      write(*,*)'*system_clearenv* error=',ierr_local
!--      stop
!--   endif
!--
!--end subroutine system_clearenv
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    system_unsetenv(3f) - [M_system]change or add an environment variable by calling unsetenv(3c)
!!##SYNOPSIS
!!
!!   subroutine system_unsetenv(name,ierr)
!!
!!    character(len=*),intent(in)  :: name
!!    integer,intent(out),optional :: ierr
!!
!!##DESCRIPTION
!!
!!    The system_unsetenv(3f) function deletes the variable name from the
!!    environment.
!!
!!##OPTIONS
!!    name   name of variable to delete.
!!           If name does not exist in the environment, then the
!!           function succeeds, and the environment is unchanged.
!!
!!    ierr   The system_unsetenv(3f) function returns zero on success, or -1 on error.
!!           name is NULL, points to a string of length 0, or contains an '=' character.
!!           Insufficient memory to add a new variable to the environment.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!      program demo_system_unsetenv
!!      use M_system, only : system_unsetenv, system_putenv
!!      implicit none
!!      call system_putenv('GRU=this is the value')
!!      write(*,'(a)')'The variable GRU should be set'
!!      call execute_command_line('env|grep GRU')
!!      call unsetenv('GRU')
!!      write(*,'(a)')'The variable GRU should not be set'
!!      call execute_command_line('env|grep GRU')
!!      end program demo_system_unsetenv
!===================================================================================================================================
subroutine system_unsetenv(name,ierr)
character(len=*),parameter :: ident="@(#)M_system::system_unsetenv(3f): call unsetenv(3c) to remove variable from  environment"
character(len=*),intent(in)  :: name
integer,intent(out),optional :: ierr
   integer                   :: ierr_local

! int unsetenv(void)
interface
   integer(kind=c_int) function c_unsetenv(c_name) bind(C,NAME="unsetenv")
   import c_int, c_char
   character(len=1,kind=c_char) :: c_name(*)
   end function
end interface

   ierr_local =  c_unsetenv(str2arr(trim(NAME)))

   if(present(ierr))then
      ierr=ierr_local
   elseif(ierr_local.ne.0)then ! if error occurs and not being returned, stop
      write(*,*)'*system_unsetenv* error=',ierr_local
      stop
   endif

end subroutine system_unsetenv
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    system_readenv(3f) - [M_system]step thru and read environment table
!!##SYNOPSIS
!!
!!       function system_readenv() result(string)
!!
!!        character(len=:),allocatable  :: string
!!##DESCRIPTION
!!    A simple interface allows reading the environment variable table of the process. Call
!!    system_initenv(3f) to initialize reading the environment table, then call system_readenv(3f) can
!!    be called until a blank line is returned. If more than one thread
!!    reads the environment or the environment is changed while being read the results are undefined.
!!##OPTIONS
!!    string  the string returned from the environment of the form "NAME=VALUE"
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_system_readenv
!!    use M_system, only : system_initenv, system_readenv
!!    character(len=:),allocatable :: string
!!       call system_initenv()
!!       do
!!          string=system_readenv()
!!          if(string.eq.'')then
!!             exit
!!          else
!!             write(*,'(a)')string
!!          endif
!!       enddo
!!    end program demo_system_readenv
!!
!!   Sample results:
!!
!!    USERDOMAIN_ROAMINGPROFILE=buzz
!!    HOMEPATH=\Users\JSU
!!    APPDATA=C:\Users\JSU\AppData\Roaming
!!    MANPATH=/home/urbanjs/V600/LIBRARY/libjust4/download/tmp/man:/home/urbanjs/V600/doc/man:::
!!    DISPLAYNUM=0
!!    ProgramW6432=C:\Program Files
!!    HOSTNAME=buzz
!!    XKEYSYMDB=/usr/share/X11/XKeysymDB
!!    PUBLISH_CMD=
!!    OnlineServices=Online Services
!!         :
!!         :
!!         :
!===================================================================================================================================
function system_readenv() result(string)
character(len=*),parameter :: ident="@(#)M_system::system_readenv(3f): read next entry from environment table"
character(len=:),allocatable  :: string
character(kind=c_char)        :: c_buff(longest_env_variable+1)

interface
   subroutine c_readenv(c_string) bind (C,NAME='my_readenv')
      import c_char, c_int, c_ptr, c_size_t
      character(kind=c_char),intent(out)  :: c_string(*)
   end subroutine c_readenv
end interface

  c_buff=' '
  c_buff(longest_env_variable+1:longest_env_variable+1)=c_null_char
  call c_readenv(c_buff)
  string=trim(arr2str(c_buff))

end function system_readenv
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!   fileglob(3f) - [M_system] Read output of an ls(1) command from Fortran
!!##SYNOPSIS
!!
!!   subroutine fileglob(glob, list)
!!
!!    character(len=*),intent(in)   :: glob                   ! Pattern for the filenames (like: *.txt)
!!    character(len=*),pointer      :: list(:)                ! Allocated list of filenames (returned), the caller must deallocate it.
!!
!!##DESCRIPTION
!!    Non-portable procedure uses the shell and the ls(1) command to expand a filename
!!    and returns a pointer to a list of expanded filenames.
!!
!!##EXAMPLE
!!
!!   Read output of an ls(1) command from Fortran
!!
!!    program test_fileglob  ! simple unit test
!!       call testit('*.*')
!!       call testit('/tmp/__notthere.txt')
!!    end program test_fileglob
!!
!!    subroutine testit(string)
!!       use M_system, only : fileglob
!!       character(len=255),pointer :: list(:)
!!       character(len=*) :: string
!!       call fileglob(string, list)
!!       write(*,*)'Files:',size(list)
!!       write(*,'(a)')(trim(list(i)),i=1,size(list))
!!       deallocate(list)
!!    end subroutine testit
!===================================================================================================================================
subroutine fileglob(glob, list) ! NON-PORTABLE AT THIS POINT. REQUIRES ls(1) command, assumes 1 line per file
!  The length of the character strings in list() must be long enough for the filenames.
!  The list can be zero names long, it is still allocated.
use M_io,only: notopen,uniq                             ! notopen: needed to open unique scratch file for holding file list
                                                        ! uniq:    adds number suffixs to a filename to make it uniq
implicit none
character(len=*),parameter :: ident="@(#)M_system::fileglob(3f): Returns list of files using a file globbing pattern"
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=*),intent(in)   :: glob                   ! Pattern for the filenames (like: *.txt)
character(len=*),pointer      :: list(:)                ! Allocated list of filenames (returned), the caller must deallocate it.
!-----------------------------------------------------------------------------------------------------------------------------------
   character(len=255)            :: tmpfile             ! scratch filename to hold expanded file list
   character(len=255)            :: cmd                 ! string to build system command in
   integer                       :: iotmp               ! needed to open unique scratch file for holding file list
   integer                       :: i,ios,icount
   iotmp=notopen(90,1000)                               ! get unused logical I/O unit
   tmpfile='/tmp/__filelist__'                          ! preliminary scratch file name
   tmpfile=uniq(tmpfile,70)                             ! make sure unique scratch filename
   cmd='ls -d '//trim(glob)//'>'//trim(tmpfile)//' '    ! build command string
   call execute_command_line(cmd )                      ! Execute the command specified by the string.
   open(iotmp,file=tmpfile,iostat=ios)                  ! open unique scratch filename
   if(ios.ne.0) return                                  ! the open failed
   icount=0                                             ! number of filenames in expanded list
   do                                                   ! count the number of lines (assumed ==files) so know what to allocate
       read(iotmp,'(a)', iostat=ios)                    ! move down a line in the file to count number of lines
       if(ios .ne. 0)exit                               ! hopefully, this is because end of file was encountered so done
       icount=icount+1                                  ! increment line count
   enddo
   rewind(iotmp)                                        ! rewind file list so can read and store it
   allocate(list(icount))                               ! allocate and fill the array
   do i=1,icount
      read(iotmp,'(a)')list(i)                          ! read a filename from a line
   enddo
   close(iotmp, status='delete',iostat=ios)             ! close and delete scratch file
end subroutine fileglob
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!   system_uname(3f) - [M_system]call a C wrapper that calls uname(3c) to get current system information from Fortran
!!##SYNOPSIS
!!
!!    subroutine system_uname(WHICH,NAMEOUT)
!!
!!     character(KIND=C_CHAR),intent(in) :: WHICH
!!     character(len=*),intent(out)      :: NAMEOUT
!!##DESCRIPTION
!!        Given a letter, return a corresponding description of the current operating system.
!!        The NAMEOUT variable is assumed sufficiently large enough to hold the value.
!!
!!        s   return the kernel name
!!        r   return the kernel release
!!        v   return the kernel version
!!        n   return the network node hostname
!!        m   return the machine hardware name
!!        T   test mode -- print all information, in the following order - srvnm
!!
!!##EXAMPLE
!!
!!   Call uname(3c) from Fortran
!!
!!    program demo_system_uname
!!       use M_system, only: system_uname
!!       implicit none
!!       integer,parameter          :: is=100
!!       integer                    :: i
!!       character(len=*),parameter :: letters='srvnmxT'
!!       character(len=is)          :: string=' '
!!
!!       do i=1,len(letters)
!!          write(*,'(80("="))')
!!          call system_uname(letters(i:i),string)
!!          write(*,*)'=====> TESTING system_uname('//letters(i:i)//')--->'//trim(string)
!!       enddo
!!
!!    end program demo_system_uname
!===================================================================================================================================
subroutine system_uname(WHICH,NAMEOUT)
implicit none
character(len=*),parameter :: ident="@(#)M_system::system_uname(3f): call my_uname(3c) which calls uname(3c)"
character(KIND=C_CHAR),intent(in) :: WHICH
character(len=*),intent(out)      :: NAMEOUT

! describe the C routine to Fortran
! void system_uname(char *which, char *buf, int *buflen);
interface
   subroutine system_uname_c(WHICH,BUF,BUFLEN) bind(C,NAME='my_uname')
      import c_char, c_int
      implicit none
      character(KIND=C_CHAR),intent(in)  :: WHICH
      character(KIND=C_CHAR),intent(out) :: BUF(*)
      integer(kind=c_int),intent(in)     :: BUFLEN
   end subroutine system_uname_c
end interface

   NAMEOUT='unknown'
   call system_uname_c(WHICH,NAMEOUT, INT(LEN(NAMEOUT),kind(0_c_int)))

end subroutine system_uname
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!        system_gethostname(3f) - [M_system]get name of current host
!!##SYNOPSIS
!!
!!       subroutine system_gethostname(string,ierr)
!!
!!        character(len=:),allocatable,intent(out) :: NAME
!!        integer,intent(out)                      :: IERR
!!##DESCRIPTION
!!        The system_gethostname(3f) procedure returns the standard host
!!        name for the current machine.
!!
!!##OPTIONS
!!        string  returns the hostname. Must be an allocatable CHARACTER variable.
!!        ierr    Upon successful completion, 0 shall be returned; otherwise, -1
!!                shall be returned.
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_system_gethostname
!!
!!    use M_system, only : system_gethostname
!!    implicit none
!!    character(len=:),allocatable :: name
!!    integer                      :: ierr
!!
!!       call system_gethostname(name,ierr)
!!       if(ierr.eq.0)then
!!          write(*,'("hostname[",a,"]")')name
!!       else
!!          write(*,'(a)')'ERROR: could not get hostname'
!!       endif
!!
!!    end program demo_system_gethostname
!===================================================================================================================================
subroutine system_gethostname(NAME,IERR)
implicit none
character(len=*),parameter :: ident="@(#)M_system::system_gethostname(3f): get name of current host by calling gethostname(3c)"
character(len=:),allocatable,intent(out) :: NAME
integer,intent(out)                      :: IERR
   character(kind=c_char,len=1)          :: C_BUFF(HOST_NAME_MAX+1)

! describe the C routine to Fortran
!int gethostname(char *name, size_t namelen);
interface
   function system_gethostname_c(c_buf,c_buflen) bind(C,NAME='gethostname')
      import c_char, c_int
      implicit none
      integer(kind=c_int)                  :: system_gethostname_c
      character(KIND=C_CHAR),intent(out)   :: c_buf(*)
      integer(kind=c_int),intent(in),value :: c_buflen
   end function system_gethostname_c
end interface

   C_BUFF=' '
   ierr=system_gethostname_c(C_BUFF,HOST_NAME_MAX) ! Host names are limited to {HOST_NAME_MAX} bytes.
   NAME=trim(arr2str(C_BUFF))

end subroutine system_gethostname
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    system_getlogin(3f) - [M_system]get login name
!!##SYNOPSIS
!!
!!   function system_getlogin() result (fname)
!!
!!    character(len=:),allocatable :: FNAME
!!##DESCRIPTION
!!
!!    The system_getlogin() function returns a string containing the user
!!    name associated by the login activity with the controlling terminal
!!    of the current process.  Otherwise, it returns a null string and sets
!!    errno to indicate the error.
!!
!!    Three names associated with the current process can be determined:
!!       o system_getpwuid(system_getuid()) returns the name associated with the real user ID of the process.
!!       o system_getpwuid(system_geteuid()) returns the  name  associated with  the  effective user ID of the process
!!       o system_getlogin() returns the name associated with the current login activity
!!
!!##RETURN VALUE
!!    fname  returns the login name.
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_system_getlogin
!!    use M_system, only : system_getlogin
!!    implicit none
!!    character(len=:),allocatable :: name
!!    name=system_getlogin()
!!    write(*,'("login[",a,"]")')name
!!    end program demo_system_getlogin
!===================================================================================================================================
!--       The  following  example calls the getlogin() function to obtain the name of the user associated with the calling process,
!--       and passes this information to the getpwnam() function to get the associated user database information.
!--           ...
!--           char *lgn;
!--           struct passwd *pw;
!--           ...
!--           if ((lgn = getlogin()) == NULL || (pw = getpwnam(lgn)) == NULL) {
!--               fprintf(stderr, "Get of user information failed.\n"); exit(1);
!--               }
!--APPLICATION USAGE
!--SEE ALSO
!--       getpwnam(), getpwuid(), system_geteuid(), getuid()
function system_getlogin() result (fname)
character(len=:),allocatable :: fname
   type(c_ptr)               :: username

interface
   function c_getlogin() bind(c,name="getlogin") result(c_username)
      import c_int, c_ptr
      type(c_ptr)           :: c_username
   end function c_getlogin
end interface

   username = c_getlogin()
   if(.not.c_associated(username)) then
      write(*,'(a)')'*system_getlogin* Error getting username. not associated'
      fname=c_null_char
   else
      fname=c2f_string(username)
   endif

end function system_getlogin
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
pure function arr2str(array)  result (string)
character(len=*),parameter :: ident="@(#)M_system::arr2str(3fp): function copies null-terminated char array to string"
character(len=1),intent(in)  :: array(:)
character(len=size(array))   :: string
integer                      :: i

   string=' '
   do i = 1,size(array)
      if(array(i).eq.char(0))then
         exit
      else
         string(i:i) = array(i)
      endif
   enddo

end function arr2str
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
pure function str2arr(string) result (array)
character(len=*),parameter :: ident="@(#)M_system::str2arr(3fp): function copies string to null terminated char array"
character(len=*),intent(in)     :: string
character(len=1,kind=c_char)    :: array(len(string)+1)
   integer                      :: i

   do i = 1,len_trim(string)
      array(i) = string(i:i)
   enddo
   array(size(array))=c_null_char

end function str2arr
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function C2F_string(c_string_pointer) result(f_string)

! gets a C string (pointer), and returns the corresponding Fortran string up to 4096(max_len) characters;
! If the C string is null, it returns string C "null" character:

type(c_ptr), intent(in)                       :: c_string_pointer
character(len=:), allocatable                 :: f_string
character(kind=c_char), dimension(:), pointer :: char_array_pointer => null()
integer,parameter                             :: max_len=4096
character(len=max_len)                        :: aux_string
integer                                       :: i
integer                                       :: length

   length=0
   call c_f_pointer(c_string_pointer,char_array_pointer,[max_len])
   if (.not.associated(char_array_pointer)) then
     allocate(character(len=4)::f_string); f_string=c_null_char; return
   endif
   aux_string=" "
   do i=1,max_len
     if (char_array_pointer(i)==c_null_char) then
       length=i-1; exit
     endif
     aux_string(i:i)=char_array_pointer(i)
   enddo
   allocate(character(len=length)::f_string)
   f_string=aux_string(1:length)
end function C2F_string
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
end module M_system
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
