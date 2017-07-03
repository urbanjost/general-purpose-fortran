!>
!!##NAME
!!    M_debug(3fm) - [M_debug] a collection of Fortran routines for supporting the development of
!!                   unit tests, and providing error processing and debugging procedures.
!!##SYNOPSIS
!!
!!    The M_debug(3fm) Fortran module provides procedures and data useful in providing error processing and debugging capabilities.
!!##DESCRIPTION
!!
!!    fstop(3f)             calls 'STOP VALUE' passing in a value (1-32), with optional message
!!    pdec(3f)              write ASCII Decimal Equivalent (ADE) numbers vertically beneath string
!!    stderr(3f)            Write message on stderr
!!
!!    unit_check_start(3f)  call command "goodbad NAME start"
!!    unit_check(3f)        if expression is .F. call command "goodbad NAME bad" and stop program
!!    unit_check_good(3f)   call command "goodbad NAME good"
!!    unit_check_bad(3f)    call command "goodbad NAME bad" and  stop program
!!
!!    The existence of a command called "goodbad" is assumed. This is generally a script that makes entries for each unit in an
!!    SQLite data file which is then used to create CSV and HTML reports on the status of each unit. A sample goodbad(1) command
!!    written in the bash(1) shell and using the sqlite3(1) command should be included in this distribution.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!     program demo_unit_tests
!!     use M_debug, only: unit_check_start, unit_check
!!     use M_debug, only: unit_check_good, unit_check_bad
!!     implicit none
!!     integer :: i, j, k
!!     integer,allocatable :: array(:)
!!     i=1
!!     j=2
!!     k=3
!!     array=[10,20,30,40,50,60,70]
!!
!!     !  register an entry for specified name in database with status of zero (0)
!!     call unit_check_start('myroutine')
!!
!!     !  if mask test fails, change database status for specified entry to -1 and stop program, else continue
!!     call unit_check('myroutine',i.gt.0)
!!
!!     ! use of all(3f), any(3f), merge(3f) can be useful
!!     ! if you know what these would produce
!!     ! write(*,*)['A','X','X','X','X','B'].eq.'B'      ! this would return an array, the last element having the value T, else F
!!     ! write(*,*)all(['A','X','X','X','X','X'].eq.'X') ! this would return F
!!     ! write(*,*)any(['A','X','X','X','X','X'].eq.'B') ! this would return F
!!     ! write(*,*)any(['A','X','X','X','X','B'].eq.'B') ! this would return T
!!     ! write(*,*).not.all(array.lt.100)
!!     ! write(*,*)all(array.lt.100)
!!     ! this will make sense ...
!!
!!     call unit_check('myroutine',all([i,j,k].gt.0),      'testing if everyone greater than zero')
!!     call unit_check('myroutine',all(.not.[i,j,k].eq.4), 'testing if no one is equal to four')
!!
!!     ! for tests that are hard to reduce to a logical test just call unit_check_bad(3f) if fail
!!     if(i+j+k.lt.1)then
!!        call unit_check_bad('myroutine')
!!     endif
!!
!!     ! it is assumed if you got here you should set status in the database to one, meaning tests were conducted and passed
!!     write(*,*)'check on "myroutine" passed'
!!     call unit_check_good('myroutine')
!!
!!     end program demo_unit_tests
!!
!!    TEST-DRIVEN DEVELOPMENT
!!
!!    set-up       perform initialization operations common to all tests within a module
!!    tear-down    perform finalization operations common to all tests within a module
!===================================================================================================================================
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
module m_debug
use iso_fortran_env, only : ERROR_UNIT        ! access computing environment
implicit none
private
   integer,save,public :: io_debug=ERROR_UNIT ! mutable copy of ERROR_UNIT, but initialized to the unit used for stderr
   logical,save,public :: debug=.false.
   integer,parameter,public :: EXIT_SUCCESS=0
   integer,parameter,public :: EXIT_FAILURE=1
   public stderr
   public pdec
   public fstop
   public unit_check
   public unit_check_good
   public unit_check_bad
   public unit_check_start
contains
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    stderr - [M_debug]write message to stderr
!!##SYNOPSIS
!!
!!    subroutine stderr(message)
!!
!!     character(len=*),intent(in)  :: message
!!     class(*),intent(in),optional :: generic
!!##DESCRIPTION
!!    STDERR(3f) writes a message to standard error using a standard f2003 method.
!!##OPTIONS
!!    message  - description to be printed
!!    generic  - optional value to print the value of after the message
!!##EXAMPLES
!!
!!   Sample program:
!!
!!    program demo_stderr
!!    use M_debug, only: stderr
!!    implicit none
!!
!!    call stderr('error: RVALUE=',3.0/4.0)
!!    call stderr('error: IVALUE=',123456789)
!!    call stderr('error: LVALUE=',.true.)
!!
!!    call stderr('error: program will now stop')
!!    stop 1
!!
!!    end program demo_stderr
!!
!!   Results:
!!
!!    error: RVALUE=   0.750000000
!!    error: IVALUE=    123456789
!!    error: LVALUE=  T
!!    error: program will now stop
!!    STOP 1
!===================================================================================================================================
subroutine stderr(message,generic)
implicit none
character(len=*),parameter::ident="@(#)M_debug::stderr(3f): writes a message to standard error using a standard f2003 method"
character(len=*),intent(in)  :: message
class(*),intent(in),optional :: generic
   if(present(generic))then
      write(error_unit,'(a,1x)',advance='no')trim(message)    ! write message to standard error
      select type(generic)
!      type is (integer(kind=int8));    write(error_unit,*) generic
!      type is (integer(kind=int16));   write(error_unit,*) generic
!      type is (integer(kind=int32));   write(error_unit,*) generic
!      type is (integer(kind=int64));   write(error_unit,*) generic
!      type is (integer(kind=int128));  write(error_unit,*) generic
!      type is (real(kind=real32));     write(error_unit,*) generic
!      type is (real(kind=real64));     write(error_unit,*) generic
!      type is (real(kind=real128));    write(error_unit,*) generic
!      type is (real(kind=real256));    write(error_unit,*) generic
      type is (integer);                write(error_unit,*) generic
      type is (real);                   write(error_unit,*) generic
      type is (doubleprecision);        write(error_unit,*) generic
      type is (logical);                write(error_unit,*) generic
      type is (character(len=*));       write(error_unit,*) trim(generic)
      end select
   else
      write(error_unit,'(a)')trim(message)    ! write message to standard error
   endif
end subroutine stderr
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine fstop(ierr,stdout,stderr)
character(len=*),parameter::ident="@(#)M_debug::fstop(3f): calls 'STOP VALUE' passing in a value (1-32), with optional message"
integer,intent(in)                   :: ierr
character(len=*),optional,intent(in) :: stdout
character(len=*),optional,intent(in) :: stderr
   character(len=132)                :: message
!
! What a call to STOP does is very system dependent, so using an abstraction layer is useful, as it allows
! just the fstop() routine to be changed.
!
! The standard states:
!   If the stop-code is an integer, it is recommended that the value also be used as the process exit status, if the
!   processor supports that concept. If the integer stop-code is used as the process exit status, the processor
!   might be able to interpret only values within a limited range, or only a limited portion of the integer value
!   (for example, only the least-significant 8 bits).

!   If the stop-code is of type character or does not appear, or if an END PROGRAM statement is executed,
!   it is recommended that the value zero be supplied as the process exit status, if the processor supports that
!   concept.
!   A STOP statement or ALL STOP statement shall not be executed during execution of an input/output statement.
!
! Conforming varients I have encountered include
!    o printing a message such as 'STOP nnn' when the an integer value is called
!    o having a limit on the length of the message string passed
!    o prefixing the message with the string 'STOP '
!    o different ranges on allowed integer values, and/or not having a one-to-one correspondence between the argument
!      value and what the system is given (usually encountered with large values, which are masked or run thru modulo math, ...)
!    o whether messages appear on stdout or stderr.
!    o no value being returned to the system at all.
!
!  So it is best to test (csh/tcsh sets $status, sh/ksh/bash/... sets $?) to verify what exit codes are supported.
!  What happens with negative values, values above 256; how long of a message is supported? Are messages line-terminated?
!
!  And for some reason STOP only takes constant values. I sometimes want to be able to pass a variable value.
!  Only allowing constants would have the advantage of letting the compiler detect values invalid for a particular system,
!  but I sometimes want to return variables.
!
!  So, using STOP with an argument is not as straight-forward as one might guess, especially if you do not want a message
!  to appear when using integer values
!
!  In practice the C exit(int signal) routine seems to work successfully when called from Fortran but I consider it risky
!  as it seems reasonable to assume Fortran cleanup operations such as removing scratch files and closing and flushing Fortran
!  files may not be properly performed. So it is tempting to call the C function, especially on systesm where C returns a
!  value to the system and Fortran does not, but I do not recommend it.
!
!  Note that the C function "exit(int signal)" not only works more consistently but that the global values EXIT_SUCCESS and
!  EXIT_FAILURE are defined for portability, and that the signal value can be a variable instead of a constant.
!
!  If the system supports calls to produce a traceback on demand, that is a useful option to add to this procedure.
!-----------------------------------------------------------------------------------------------------------------------------------
!STOP       'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa&
!&aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa&
!&aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa&
!&aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa&
!&aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa&
!&aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa&
!&aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa&
!&aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa&
!&aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaab'
!-----------------------------------------------------------------------------------------------------------------------------------
if(present(stdout))then       ! write message to stdout, assuming string length is allowed
   if(stdout.ne.'')then
      write(*,'(a)')trim(stdout)
   endif
endif
if(present(stderr))then       ! write message to stderr, assuming string length is allowed
   if(stderr.ne.'')then
      write(error_unit,'(a)')trim(stderr)
   endif
endif
select case(ierr)             ! have executable return an exit status to the system (IF SUPPORTED)
   case(0); stop 0
   case(1); stop 1
   case(2); stop 2
   case(3); stop 3
   case(4); stop 4
   case(5); stop 5
   case(6); stop 6
   case(7); stop 7
   case(8); stop 8
   case(9); stop 8
   case(10); stop 10
   case(11); stop 11
   case(12); stop 12
   case(13); stop 13
   case(14); stop 14
   case(15); stop 15
   case(16); stop 16
   case(17); stop 17
   case(18); stop 18
   case(19); stop 19
   case(20); stop 20
   case(21); stop 21
   case(22); stop 22
   case(23); stop 23
   case(24); stop 24
   case(25); stop 25
   case(26); stop 26
   case(27); stop 27
   case(28); stop 28
   case(29); stop 29
   case(30); stop 30
   case(31); stop 31
   case(32); stop 32
case default
   write(message,'(a,i0,a)')'*fstop*: stop value of ',ierr,' returning 1 to system'
   write(error_unit,'(a)')trim(message) ! write message to standard error
   stop 1
end select
end subroutine fstop
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!
!!##NAME
!!    unit_check - [M_debug]if logical expression is false, call command "goodbad NAME bad" and stop program
!!
!!##SYNOPSIS
!!
!!    subroutine unit_check(name,expression,message)
!!
!!     character(len=*),intent(in) :: name
!!     logical,intent(in) :: expression
!!     character(len=*),intent(in),optional :: message
!!
!!##DESCRIPTION
!!
!!    unit_check(3f) tests the expression and if it is false, calls the shell command
!!         goodbad NAME bad
!!    and stops the program.
!!##OPTIONS
!!     NAME         the unit test name passed onto the goodbad(1) command
!!     EXPRESSION   the logical expression to evaluate
!!     MESSAGE      optional message to display when performing test
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!     program demo_unit_check
!!     use M_debug, only: unit_check
!!     use M_debug, only: unit_check_start, unit_check_good
!!     implicit none
!!     integer :: x
!!     x=10
!!     call unit_check_start('myroutine')
!!
!!     call unit_check('myroutine', x.gt.3 ,'test if big enough')
!!     call unit_check('myroutine', x.lt.100 ,'test if small enough')
!!
!!     write(*,*)'checks on "myroutine" passed'
!!     call unit_check_good('myroutine')
!!
!!     end program demo_unit_check
!!
!!   Sample output (varies with what goodbad(1) command is used):
!!
!!     unit_check_start: myroutine.3 status initialized in database
!!     unit_check:       myroutine PASSED:test if big enough
!!     unit_check:       myroutine PASSED:test if small enough
!!     checks on "myroutine" passed
!!     data for myroutine.3 is
!!     entryname    description  documentation  filename  library  ufpp ccall archive date                 status
!!     -----------  -----------  -------------  --------- -------- ---- ----- ------- -------------------  ----------
!!     myroutine.3                                                                    2017/02/03 07:23:40  1
!===================================================================================================================================
subroutine unit_check(name,logical_expression,message)
character(len=*),parameter::ident="@(#)M_debug::unit_check(3f):if .not.expression call 'goodbad NAME bad' & stop program"
character(len=*),intent(in)          :: name
logical,intent(in)                   :: logical_expression
character(len=*),intent(in),optional :: message
!-----------------------------------------------------------------------------------------------------------------------------------
   if(present(message))then
      if(.not.logical_expression)then
         call stderr('unit_check: '//trim(name)//'        FAILED:'//trim(message))  ! write message to standard error
      else
         call stderr('unit_check: '//trim(name)//'        PASSED:'//trim(message))  ! write message to standard error
      endif
   endif
   if(.not.logical_expression)then
      call stderr('unit_check:        STOPPING PROGRAM ON TEST OF '//trim(name))    ! write announcement to standard error
      call execute_command_line('goodbad '//trim(name)//' bad')
      stop 1
   endif
end subroutine unit_check
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!
!!##NAME
!!    unit_check_start - [M_debug]call command "goodstart NAME start"
!!
!!##SYNOPSIS
!!
!!    subroutine unit_check_start(name,string)
!!
!!     character(len=*),intent(in) :: name
!!     character(len=*),intent(in) :: string
!!
!!##DESCRIPTION
!!
!!    unit_check_start(3f) calls the shell command
!!         goodstart NAME start [string]
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!     program demo_unit_check_start
!!     use M_debug, only: unit_check_start
!!     use M_debug, only: unit_check
!!     use M_debug, only: unit_check_good, unit_check_bad
!!
!!     implicit none
!!     integer :: x
!!     x=10
!!     call unit_check_start('myroutine')
!!
!!     call unit_check('myroutine', x.gt.3 ,'test if big enough')
!!     call unit_check('myroutine', x.lt.100 ,'test if small enough')
!!
!!     write(*,*)'checks on "myroutine" passed'
!!     call unit_check_good('myroutine')
!!
!!     end program demo_unit_check_start
!!
!===================================================================================================================================
subroutine unit_check_start(name,string)
character(len=*),parameter::ident="@(#)M_debug::unit_check_start(3f): call 'goodstart NAME start'"
character(len=*),intent(in)          :: name
character(len=*),intent(in),optional :: string
!-----------------------------------------------------------------------------------------------------------------------------------
   if(present(string))then
      call execute_command_line('goodbad '//trim(name)//' start '//trim(string))
   else
      call execute_command_line('goodbad '//trim(name)//' start')
   endif
end subroutine unit_check_start
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!
!!##NAME
!!    unit_check_bad - [M_debug]call command "goodbad NAME bad" and stop program
!!
!!##SYNOPSIS
!!
!!    subroutine unit_check_bad(name,string)
!!
!!     character(len=*),intent(in) :: name
!!     character(len=*),intent(in) :: string
!!
!!##DESCRIPTION
!!
!!    unit_check_bad(3f) calls the shell command
!!         goodbad NAME bad [string]
!!    and stops the program.
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!     program demo_unit_check_bad
!!     use M_debug, only: unit_check_start
!!     use M_debug, only: unit_check
!!     use M_debug, only: unit_check_good, unit_check_bad
!!
!!     implicit none
!!     integer :: x
!!     x=10
!!     call unit_check_start('myroutine')
!!
!!     call unit_check('myroutine', x.gt.3 ,'test if big enough')
!!     call unit_check('myroutine', x.lt.100 ,'test if small enough')
!!
!!     !write(*,*)'checks on "myroutine" passed'
!!     !call unit_check_good('myroutine')
!!
!!     write(*,*)'checks on "myroutine" failed'
!!     call unit_check_bad ('myroutine') ! program execution stopped
!!
!!     end program demo_unit_check_bad
!===================================================================================================================================
subroutine unit_check_bad(name,string)
character(len=*),parameter::ident="@(#)M_debug::unit_check_bad(3f): call 'goodbad NAME bad'"
character(len=*),intent(in)          :: name
character(len=*),intent(in),optional :: string
!-----------------------------------------------------------------------------------------------------------------------------------
   if(present(string))then
      call execute_command_line('goodbad '//trim(name)//' bad '//trim(string))
   else
      call execute_command_line('goodbad '//trim(name)//' bad')
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   stop 1
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine unit_check_bad
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!
!!##NAME
!!    unit_check_good - [M_debug]call command "goodbad NAME good"
!!
!!##SYNOPSIS
!!
!!    subroutine unit_check_good(name,string)
!!
!!     character(len=*),intent(in) :: name
!!     character(len=*),intent(in) :: string
!!
!!##DESCRIPTION
!!
!!    unit_check_good(3f) calls the shell command
!!
!!         goodbad NAME good [string]
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!     program demo_unit_check_good
!!     use M_debug, only: unit_check_start
!!     use M_debug, only: unit_check
!!     use M_debug, only: unit_check_good, unit_check_bad
!!
!!     implicit none
!!     integer :: x
!!     x=10
!!     call unit_check_start('myroutine')
!!
!!     call unit_check('myroutine', x.gt.3 ,'test if big enough')
!!     call unit_check('myroutine', x.lt.100 ,'test if small enough')
!!
!!     write(*,*)'checks on "myroutine" passed'
!!     call unit_check_good('myroutine')
!!
!!     end program demo_unit_check_good
!===================================================================================================================================
subroutine unit_check_good(name,string)
character(len=*),parameter::ident="@(#)M_debug::unit_check_good(3f): call 'goodbad NAME good'"
character(len=*),intent(in)          :: name
character(len=*),intent(in),optional :: string
!-----------------------------------------------------------------------------------------------------------------------------------
   if(present(string))then
      call execute_command_line('goodbad '//trim(name)//' good '//trim(string))
   else
      call execute_command_line('goodbad '//trim(name)//' good')
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine unit_check_good
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!      pdec - [M_debug]write out string with ASCII decimal equivalent vertically under it
!!
!!##SYNOPSIS
!!
!!    Usage:
!!
!!     subroutine pdec(string)
!!     character(len=*),intent(in) :: string
!!
!!##DESCRIPTION
!!
!!      Given a string to print, PDEC() writes out the ASCII Decimal equivalent of
!!      the string directly underneath it. This can help you to locate
!!      unprintable characters or non-standard white-space such as a
!!      backspace character or tab character in input strings that your
!!      program could not interpret. On output, non-printable characters
!!      are replaced with a space, and trailing spaces are ignored.
!!
!!      You read the numbers vertically.
!!
!!      1. ignore trailing spaces
!!      2. print the character if it has an ADE of 32 on up
!!      3. print a space if it has an ADE of less than 32
!!      4. underneath each character print the ADE value vertically
!!      5. strings are assumed under 32767 characters in length.
!!         Format integer constants > 32767 are not supported on HP-UX
!!         when newer compilers are available use unlimited
!!
!!##EXAMPLE
!!
!!
!!    Sample program:
!!
!!       program demo
!!       call pdec(' ABCDEFG abcdefg ')
!!       end program demo
!!
!!    would produce (notice trailing space is trimmed):
!!
!!      > ABCDEFG abcdefg
!!      >0000000000001111
!!      >3666667739990000
!!      >2567890127890123
!!
!!##AUTHOR
!!      John S. Urban
!===================================================================================================================================
subroutine pdec(string)
character(len=*),parameter::ident="@(#)M_debug::pdec(3f): write ASCII Decimal Equivalent (ADE) numbers vertically beneath string"
character(len=*),intent(in) :: string  ! the string to print
   integer :: ilen  ! number of characters in string to print
   integer :: i     ! counter used to step thru string
!-----------------------------------------------------------------------------------------------------------------------------------
   ilen=len_trim(string(:len(string)))  ! get trimmed length of input string

   write(*,101)(char(max(32,ichar(string(i:i)))),i=1,ilen) ! replace lower unprintable characters with spaces

   ! print ADE value of character underneath it
   write(*,202)     (ichar(string(i:i))/100,    i=1,ilen)
   write(*,202)(mod( ichar(string(i:i)),100)/10,i=1,ilen)
   write(*,202)(mod((ichar(string(i:i))),10),   i=1,ilen)
101   format(32767a1:)  ! format for printing string characters
202   format(32767i1:)  ! format for printing ADE values
end subroutine pdec
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
end module m_debug
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
