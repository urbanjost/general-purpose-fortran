module M_framework__utility
use, intrinsic :: iso_fortran_env,  only : int8, int16, int32, int64
use, intrinsic :: iso_fortran_env,  only : real32, real64, real128
use, intrinsic :: iso_fortran_env,  only : ERROR_UNIT,OUTPUT_UNIT
implicit none
private
public :: fstop
contains
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    fstop(3f) - [M_framework__utility] call stop with both a number and
!!    a message
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    subroutine fstop(ierr,stdout,stderr)
!!
!!     integer,intent(in)                   :: ierr
!!     character(len=*),intent(in),optional :: stdout
!!     character(len=*),intent(in),optional :: stderr
!!##DESCRIPTION
!!    FSTOP(3f) call STOP(3f). What a call to STOP does is very system
!!    dependent, so using an abstraction layer is useful, as it allows just
!!    the fstop() routine to be changed; and STOP does not allow a variable
!!    to be used on the numeric access status (this has changed at f2015).
!!
!!##OPTIONS
!!    ierr    - value in range 0 to 32
!!    stdout  - description to be printed to standard output
!!    stderr  - description to be printed to standard error
!!##EXAMPLES
!!
!!   Sample program:
!!
!!    program demo_fstop
!!    use M_framework__utility, only: fstop
!!    implicit none
!!    integer :: int
!!    !*!write(*,*)'Enter stop value'
!!    !*!read(*,*) int
!!    int=25
!!    select case(int)
!!    case(10) ; call fstop(int)
!!    case(20) ; call fstop(int,stderr='error: program will now stop')
!!    case(25) ; call fstop(int,stdout='stdout message', &
!!                    & stderr='stderr message')
!!    case(30) ; call fstop(int,stdout='error: program will now stop')
!!    case default
!!               call fstop(int)
!!    endselect
!!
!!    end program demo_fstop
!!
!!   Results:
!!
!!##SEE ALSO
!!   Look for common extensions, such as abort(3f), backtrace(3f)
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine fstop(ierr,stdout,stderr)

! ident_1="@(#) M_framework__utility fstop(3f) calls 'STOP VALUE' passing in a value (1-32) with optional message"

integer,intent(in)                   :: ierr
character(len=*),optional,intent(in) :: stdout
character(len=*),optional,intent(in) :: stderr
character(len=132)                   :: message
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
! Conforming variants I have encountered include
!    o printing a message such as 'STOP nnn' when the integer value is called
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
!  files may not be properly performed. So it is tempting to call the C function, especially on systems where C returns a
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
if(present(stderr))then       ! write message to stderr, assuming string length is allowed
   if(stderr /= '')then
      write(error_unit,'(a)')trim(stderr)
   endif
!f2015!   select case(ierr)             ! have executable return an exit status to the system (IF SUPPORTED)
!f2015!      case(0); allstop 0
!f2015!      case(1); allstop 1
!f2015!      case(2); allstop 2
!f2015!      case(3); allstop 3
!f2015!      case(4); allstop 4
!f2015!      case(5); allstop 5
!f2015!      case(6); allstop 6
!f2015!      case(7); allstop 7
!f2015!      case(8); allstop 8
!f2015!      case(9); allstop 8
!f2015!      case(10); allstop 10
!f2015!      case(11); allstop 11
!f2015!      case(12); allstop 12
!f2015!      case(13); allstop 13
!f2015!      case(14); allstop 14
!f2015!      case(15); allstop 15
!f2015!      case(16); allstop 16
!f2015!      case(17); allstop 17
!f2015!      case(18); allstop 18
!f2015!      case(19); allstop 19
!f2015!      case(20); allstop 20
!f2015!      case(21); allstop 21
!f2015!      case(22); allstop 22
!f2015!      case(23); allstop 23
!f2015!      case(24); allstop 24
!f2015!      case(25); allstop 25
!f2015!      case(26); allstop 26
!f2015!      case(27); allstop 27
!f2015!      case(28); allstop 28
!f2015!      case(29); allstop 29
!f2015!      case(30); allstop 30
!f2015!      case(31); allstop 31
!f2015!      case(32); allstop 32
!f2015!   case default
!f2015!      write(message,'(a,i0,a)')'*fstop*: stop value of ',ierr,' returning 1 to system'
!f2015!      write(error_unit,'(a)')trim(message) ! write message to standard error
!f2015!      allstop 1
!f2015!   end select
endif
if(present(stdout))then       ! write message to stdout, assuming string length is allowed
   if(stdout /= '')then
      write(*,'(a)')trim(stdout)
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
end module M_framework__utility
