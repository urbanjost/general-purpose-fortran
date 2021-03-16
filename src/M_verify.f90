!>
!!##NAME
!!    M_verify(3fm) - [M_verify] a collection of Fortran routines for
!!                    supporting code development by providing error
!!                    processing, debugging procedures and unit testing.
!!                    (LICENSE:PD)
!!##SYNOPSIS
!!
!!
!!  Module procedures
!!
!!    use M_verify, only : unit_check, unit_check_start, unit_check_done
!!    use M_verify, only : unit_check_good, unit_check_bad
!!    use M_verify, only : unit_check_msg
!!    use M_verify, only : debug
!!    use M_verify, only : fstop
!!    use M_verify, only : assert
!!
!!  Module values
!!
!!    use M_verify, only : unit_check_limit, unit_check_keep_going
!!    use M_verify, only : unit_check_command
!!
!!##QUOTE
!!    Do not let your victories go to your head, nor let your failures go
!!    to your heart.
!!
!!##DESCRIPTION
!!    The M_verify(3fm) Fortran module provides procedures and variables
!!    useful for providing error processing, debugging capabilities, and
!!    unit testing.
!!
!!     o allows for a user-defined command to be called to collect results or
!!       mail failure alerts, ...
!!     o supports easily composing a message from up to nine scalar
!!       intrinsic values and different message levels
!!     o allows stopping on first failure or continuing
!!
!!    SET MODES
!!    unit_check_keep_going  logical variable that can be used to turn off
!!                           program termination on errors.
!!    unit_check_level       An integer that can be used to specify
!!                           different debug levels
!!    unit_check_command     name of command to execute. Defaults to the name
!!                           "goodbad".
!!    UNIT TESTS
!!    unit_check_start(3f)   start tests of a procedure and optionally call
!!
!!                              command NAME start ..."
!!    unit_check(3f)         if expression is false optionally call
!!
!!                              command NAME bad
!!
!!                           and stop program (by default)
!!
!!    unit_check_done(3f)    call
!!
!!                              command NAME good
!!
!!                           if no failures; else call
!!
!!                              command NAME bad
!!
!!    unit_check_good(3f)    call command
!!
!!                              command NAME good
!!
!!    unit_check_bad(3f)     call command
!!
!!                              command NAME bad
!!
!!                           and stop program by default
!!    unit_check_msg(3f)     write message
!!
!!    BASIC DEBUGGING
!!    fstop(3f)             calls 'STOP VALUE' passing in a value (1-32),
!!                          with optional message
!!    pdec(3f)              write ASCII Decimal Equivalent (ADE) numbers
!!                          vertically beneath string
!!    debug                 logical variable that can be tested by routines
!!                          as a flag to process debug statements.
!!
!!    For unit testing, the existence of a command called "goodbad" is
!!    initially assumed. This is generally a script that makes entries
!!    for each unit in an SQLite data file which is then used to create
!!    CSV and HTML reports on the status of each unit. A sample goodbad(1)
!!    command written in the bash(1) shell and using the sqlite3(1) command
!!    should be included in this distribution as an example
!!
!!    The flexibility introduced by calling an external script or program
!!    is that The goodbad(1) command can be changed as desired to write CSV
!!    files or simple logs or to notify developers with e-mail as desired.
!!
!!    RELATED FUNCTIONS
!!
!!    The routines in M_verify(3f) are often combined with the M_hashkeys(3fm)
!!    routines and various math and statistical routines to quickly create
!!    unit tests.
!!
!!    Comparisons of real values can be done with a tolerance with
!!    M_Compare_Float_Numbers(3fm), for example.
!!
!!    The intrinsics ANY(3f) and ALL(3f) are particularly useful in calls
!!    to unit_check(3f).
!!
!!##EXAMPLES
!!
!!   Sample program
!!
!!     !!program demo_unit_tests
!!     module M_demo
!!     private
!!     public one !! regular routines
!!     public two !! regular routines
!!     public test_suite_M_demo !! special name for use with test_suite(1bash).
!!     contains
!!
!!     !!  regular routines
!!     subroutine one()
!!     end subroutine one
!!
!!     subroutine two()
!!     end subroutine two
!!
!!     !! unit test
!!     subroutine test_suite_M_demo
!!     use M_verify, only: unit_check_start, unit_check
!!     use M_verify, only: unit_check_good, unit_check_bad, unit_check_done
!!     use M_verify, only: unit_check_msg
!!     implicit none
!!     integer :: i, j, k
!!     integer,allocatable :: array(:)
!!     integer :: arr(4)=[21,51,14,45]
!!     integer :: a=21, b=51, c=14, d=45
!!     ! TEST-DRIVEN DEVELOPMENT
!!     ! optional set-up       perform initialization operations common to all tests within a module
!!        i=1
!!        j=2
!!        k=3
!!        array=[10,20,30,40,50,60,70]
!!        call test_one()
!!        call test_two()
!!     ! optional tear-down    perform finalization operations common to all tests within a module
!!     contains
!!
!!     subroutine test_one()
!!     !  register an entry for specified name ("one") in database with status of zero (0)
!!     call unit_check_start('one')
!!
!!     !  if mask test fails, can
!!     !  * produce a SUCCESS: or FAIL: message and stop program
!!     !  * change database status for specified entry to -1 and stop program, else continue
!!     !  * produce a SUCCESS: or FAIL: message and keep going
!!     !  * produce a FAIL: message if test fails but no SUCCESS: message if test passes
!!     call unit_check('one',i.gt.0,msg='I > 0')
!!
!!     ! using ANY(3f) and ALL(3f)
!!     call unit_check('one',all([i,j,k].gt.0),      'testing if everyone greater than zero')
!!     ! display message built of scalars as well
!!     call unit_check('one',all(.not.[i,j,k].eq.4),'for set ',i,j,k,'testing if no one is equal to four')
!!
!!     ! for tests that are hard to reduce to a logical test just call unit_check_bad(3f) if fail
!!     if(i+j+k.lt.1)then
!!        call unit_check_bad('one')
!!     endif
!!
!!     call unit_check_done('one','checks on "one" ended')
!!     end subroutine test_one
!!
!!     subroutine test_two
!!     ! use of all(3f), any(3f), merge(3f) can be useful
!!     ! if you know what these would produce
!!     ! write(*,*)['A','X','X','X','X','B'].eq.'B'      ! this would return an array, the last element having the value T, else F
!!     ! write(*,*)all(['A','X','X','X','X','X'].eq.'X') ! this would return F
!!     ! write(*,*)any(['A','X','X','X','X','X'].eq.'B') ! this would return F
!!     ! write(*,*)any(['A','X','X','X','X','B'].eq.'B') ! this would return T
!!     ! write(*,*).not.all(array.lt.100)
!!     ! write(*,*)all(array.lt.100)
!!     ! write(*,*)all([a,b,c,d].eq.[21,51,14,45]) ! compare a list. This would return T
!!     ! write(*,*)all(arr.eq.[21,51,14,45])       ! compare an array. This would return T
!!     ! you know how valuable ANY(3f) and ALL(3f) will be
!!     call unit_check_start('two','check on "two" passed')
!!     call unit_check('two', 1.gt.0 .and. abs(10.10000-10.10001).lt.0.0001,msg='two looks good')
!!     call unit_check_done('two','checks on "two" ended')
!!     end subroutine test_two
!!
!!     end subroutine test_suite_M_demo
!!
!!     end module M_demo
!!
!!     program demo_M_verify
!!     use M_demo,  only: test_suite_M_demo
!!     use M_verify, only: unit_check_command, unit_check_keep_going,unit_check_level
!!     unit_check_command=''
!!     unit_check_keep_going=.true.
!!     unit_check_level=0
!!       call test_suite_M_demo
!!     end program demo_M_verify
!!
!!   Expected output:
!!
!!     unit_check:       one                  SUCCESS:I > 0
!!     unit_check:       one                  SUCCESS:testing if everyone greater than zero
!!     unit_check:       one                  SUCCESS:for set 1 2 3 testing if no one is equal to four
!!     unit_check_done:  one                  PASSED   GOOD:3  BAD:0
!!
!!     unit_check:       two                  SUCCESS:two looks good
!!     unit_check_done:  two                  PASSED   GOOD:1  BAD:0
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
module M_verify
use, intrinsic :: iso_fortran_env, only : int8, int16, int32, int64 !  1           2           4           8
use, intrinsic :: iso_fortran_env, only : real32, real64, real128   !  4           8          10
use, intrinsic :: iso_fortran_env, only : ERROR_UNIT,OUTPUT_UNIT    ! access computing environment
use            :: M_msg,           only : str
implicit none
private

integer,save,public :: io_debug=ERROR_UNIT            ! mutable copy of ERROR_UNIT, but initialized to the unit used for stderr
integer,save,public :: unit_check_lun=ERROR_UNIT      ! mutable copy of ERROR_UNIT, but initialized to the unit used for stderr
logical,save,public :: debug=.false.

logical,save,public :: unit_check_keep_going=.false.  ! logical variable that can be used to turn off program termination on errors.
integer,save,public :: unit_check_level=0             ! a level that can be used to select different debug levels
character(len=4096),public ::  unit_check_command='goodbad'  ! name of command to execute. Defaults to the name "goodbad".
public no_news_is_good_news

integer,parameter,public   :: realtime=kind(0.0d0)            ! type for julian days
integer,parameter,public   :: EXIT_SUCCESS=0
integer,parameter,public   :: EXIT_FAILURE=1
real(kind=realtime),save   :: duration=0.0d0
integer,save               :: clicks=0.0d0

logical,save ::  STOP_G=.true.                       ! global value indicating whether failed unit checks should stop program or not
integer,save :: IPASSED_G=0                          ! counter of successes initialized by unit_check_start(3f)
integer,save :: IFAILED_G=0                          ! counter of failures  initialized by unit_check_start(3f)
logical,save :: no_news_is_good_news=.false.         ! flag on whether to display SUCCESS: messages

public stderr
public assert
public pdec
public fstop
public unit_check_start
public unit_check
public unit_check_good
public unit_check_bad
public unit_check_done
public unit_check_msg
! COMPARING AND ROUNDING FLOATING POINT VALUES
public accdig         ! compare two real numbers only up to a specified number of digits
public almost         ! function compares two numbers only up to a specified number of digits
public dp_accdig      ! compare two double numbers only up to a specified number of digits
public in_margin      ! check if two reals are approximately equal using a relative margin
public round          ! round val to specified number of significant digits
!-----------------------------------------------------------------------------------------------------------------------------------
contains
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    unit_check_msg(3f) - [M_verify] converts up to nine standard scalar values to a message for unit testing
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    function unit_check_msg(name,g1,g2g3,g4,g5,g6,g7,g8,g9)
!!
!!     character(len=*),intent(in)  :: name
!!     class(*),intent(in),optional :: g1,g2,g3,g4,g5,g6,g7,g8,g9
!!##DESCRIPTION
!!    unit_check_msg(3f) builds a string from up to nine scalar values and
!!    prints it to the error long.
!!
!!##OPTIONS
!!    name    name of unit being tested
!!    g[1-9]  optional value to print the value of after the message. May
!!            be of type INTEGER, LOGICAL, REAL, DOUBLEPRECISION, COMPLEX,
!!            or CHARACTER.
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!    program demo_unit_check_msg
!!    use M_verify, only : unit_check_start,unit_check_msg,unit_check_done
!!    implicit none
!!
!!    call unit_check_start('myroutine')
!!    call unit_check_msg('myroutine','HUGE(3f) integers',huge(0),'and real',huge(0.0),'and double',huge(0.0d0))
!!    call unit_check_msg('myroutine','real            :',huge(0.0),0.0,12345.6789,tiny(0.0) )
!!    call unit_check_msg('myroutine','doubleprecision :',huge(0.0d0),0.0d0,12345.6789d0,tiny(0.0d0) )
!!    call unit_check_msg('myroutine','complex         :',cmplx(huge(0.0),tiny(0.0)) )
!!    call unit_check_done('myroutine')
!!
!!    end program demo_unit_check_msg
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine unit_check_msg(name,g1, g2, g3, g4, g5, g6, g7, g8, g9)
implicit none

! ident_1="@(#)M_verify::unit_check_msg(3f): writes a message to a string composed of any standard scalar types"

character(len=*),intent(in)   :: name
class(*),intent(in),optional  :: g1 ,g2 ,g3 ,g4 ,g5
class(*),intent(in),optional  :: g6 ,g7 ,g8 ,g9

   ! write message to standard error
   call stderr('unit_check_msg:   '//atleast(name,20)//' INFO    : '//str(g1,g2,g3,g4,g5,g6,g7,g8,g9))

end subroutine unit_check_msg
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine stderr(msg, gen0, gen1, gen2, gen3, gen4, gen5, gen6, gen7, gen8, gen9)
implicit none

! ident_2="@(#)M_verify::stderr(3f): writes a message to standard error using a standard f2003 method"

class(*),intent(in),optional :: msg
class(*),intent(in),optional :: gen0, gen1, gen2, gen3, gen4
class(*),intent(in),optional :: gen5, gen6, gen7, gen8, gen9
integer                      :: ios

   write(error_unit,'(a)',iostat=ios) str(msg, gen0, gen1, gen2, gen3, gen4, gen5, gen6, gen7, gen8, gen9)
   flush(unit=output_unit,iostat=ios)
   flush(unit=error_unit,iostat=ios)
end subroutine stderr
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    fstop(3f) - [M_verify] call stop with both a number and a message
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
!!    use M_verify, only: fstop
!!    implicit none
!!    integer :: int
!!    !*!write(*,*)'Enter stop value'
!!    !*!read(*,*) int
!!    int=25
!!    select case(int)
!!    case(10) ; call fstop(int)
!!    case(20) ; call fstop(int,stderr='error: program will now stop')
!!    case(25) ; call fstop(int,stdout='stdout message',stderr='stderr message')
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

! ident_3="@(#)M_verify::fstop(3f): calls 'STOP VALUE' passing in a value (1-32), with optional message"

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
   if(stderr.ne.'')then
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
   if(stdout.ne.'')then
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
!>
!!
!!##NAME
!!    unit_check(3f) - [M_verify] if logical expression is false, call command "goodbad NAME bad" and stop program by default
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine unit_check(name,expression,msg,msg1,msg2,msg3,msg4,msg5,msg6,msg7,msg8,msg9)
!!
!!     character(len=*),intent(in) :: name
!!     logical,intent(in) :: expression
!!     class(*),intent(in),optional :: msg,msg1,msg2,msg3,msg4,msg5,msg6,msg7,msg8,msg9
!!
!!##DESCRIPTION
!!    unit_check(3f) tests the expression and if it is false, calls the
!!    shell command
!!
!!         goodbad NAME bad
!!
!!    and stops the program.
!!##OPTIONS
!!     NAME             the unit test name passed on to the goodbad(1)
!!                      command
!!     EXPRESSION       the logical expression to evaluate
!!     MSG,MSG1...MSG9  optional message to display when performing test,
!!                      composed of any scalar intrinsics of type INTEGER,
!!                      REAL, DOUBLEPRECISION, COMPLEX, LOGICAL, or
!!                      CHARACTER, with a space placed between each value.
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!    program demo_unit_check
!!    use M_verify, only: unit_check
!!    use M_verify, only: unit_check_start
!!    use M_verify, only: unit_check_done
!!    use M_verify,  only: almost
!!
!!    !!use M_verify, only: unit_check_keep_going         ! default is unit_check_keep_going=.false.
!!    !!use M_verify, only: debug              ! default is .false.
!!    !!use M_verify, only: unit_check_command ! default is unit_check_command='goodbad'
!!
!!    implicit none
!!    integer :: i
!!    integer :: x
!!    integer,allocatable :: arr(:)
!!    real,allocatable :: arr1(:)
!!    real,allocatable :: arr2(:)
!!
!!       !!unit_check_command=''
!!       x=10
!!       arr1=[1.0,10.0,100.0]
!!       arr2=[1.0001,10.001,100.01]
!!       call unit_check_start('myroutine')
!!
!!       call unit_check('myroutine', x.gt.3 ,'test if big enough')
!!       call unit_check('myroutine', x.lt.100 ,'test if small enough')
!!
!!       do i=1,size(arr1)
!!          call unit_check('myroutine', almost(arr1(i),arr2(i),3.9,verbose=.true.) )
!!       enddo
!!
!!       arr=[10,20,30]
!!       call unit_check('myroutine', .not.any(arr.lt.0) ,'test if any negative values in array ARR')
!!       call unit_check('myroutine', all(arr.lt.100) ,'test if all values less than 100 in array ARR')
!!
!!       call unit_check_done('myroutine',msg='checks on "myroutine" all passed')
!!
!!    end program demo_unit_check
!!
!!   Sample output (varies with what goodbad(1) command is used):
!!
!!    unit_check:      myroutine        SUCCESS:test if big enough
!!    unit_check:      myroutine        SUCCESS:test if small enough
!!    unit_check:      myroutine        SUCCESS:test if any negative values in array ARR
!!    unit_check:      myroutine        SUCCESS:test if all values less than 100 in array ARR
!!     *almost* for values 1.00000000 1.00010002 agreement of 3.99997139 digits out of requested 3.90000010
!!     *almost* for values 10.0000000 10.0010004 agreement of 3.99986792 digits out of requested 3.90000010
!!     *almost* for values 100.000000 100.010002 agreement of 3.99995065 digits out of requested 3.90000010
!!    unit_check_good: myroutine        PASSED:checks on "myroutine" all passed
!!
!!
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine unit_check(name,logical_expression,msg,msg1,msg2,msg3,msg4,msg5,msg6,msg7,msg8,msg9)

! ident_4="@(#)M_verify::unit_check(3f):if .not.expression call 'goodbad NAME bad' & stop program"

character(len=*),intent(in)          :: name
logical,intent(in)                   :: logical_expression
class(*),intent(in),optional         :: msg,msg1,msg2,msg3,msg4,msg5,msg6,msg7,msg8,msg9
character(len=:),allocatable         :: msg_local
!-----------------------------------------------------------------------------------------------------------------------------------
msg_local=str(msg,msg1,msg2,msg3,msg4,msg5,msg6,msg7,msg8,msg9)
!-----------------------------------------------------------------------------------------------------------------------------------
   if(.not.logical_expression)then
      call stderr('unit_check:       '//atleast(name,20)//' FAILURE : '//trim(msg_local))  ! write message to standard error
      if(unit_check_command.ne.'')then
         call execute_command_line(unit_check_command//' '//trim(name)//' bad')
      endif
      if(.not.unit_check_keep_going) then
         call stderr('unit_check:         STOPPING PROGRAM ON FAILED TEST OF '//trim(name))    ! write to standard error
         call fstop(1)
      endif
      IFAILED_G=IFAILED_G+1
   else
      if(.not.no_news_is_good_news)then
         call stderr('unit_check:       '//atleast(name,20)//' SUCCESS : '//trim(msg_local))  ! write message to standard error
      endif
      IPASSED_G=IPASSED_G+1
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine unit_check
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    unit_check_start(3f) - [M_verify] call command "goodbad NAME start" and optionally set options
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine unit_check_start(name,options,msg)
!!
!!     character(len=*),intent(in)          :: name
!!     character(len=*),intent(in),optional :: options
!!     character(len=*),intent(in),optional :: msg
!!
!!##DESCRIPTION
!!    unit_check_start(3f) is an initialization command that by default
!!    calls the shell command
!!
!!       goodbad NAME start [options]
!!
!!    The command can be changed by setting the environment variable
!!    UNIT_CHECK_COMMAND or the global module variable UNIT_CHECK_COMMAND.
!!    The environment variable overrides the global module variable.
!!
!!    By default if a unit_check(3f) logical expression is false or the
!!    unit_check_bad(3f) procedure is called the program will be stopped.
!!
!!    This has the same effect as setting the environment
!!    variable M_verify_STOP to "FALSE" or the global module variable
!!    UNIT_CHECK_KEEP_GOING to .FALSE. . Set the value to .true. and the
!!    program will continue even when tests fail.
!!
!!##OPTIONS
!!       NAME  name of the shell command to execute. If blank, no command
!!             is executed.
!!    OPTIONS  pass additional options to the shell command
!!
!!    MSG      print message
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!     program demo_unit_check_start
!!     use M_verify, only: unit_check_start
!!     use M_verify, only: unit_check
!!     use M_verify, only: unit_check_done
!!
!!     implicit none
!!     integer :: ival
!!     call unit_check_start('myroutine')
!!     ! the goodbad(1) command called here takes many options
!!     ! used to build an SQLite3 entry
!!     call unit_check_start('myroutine_long',' &
!!       & -section        3                    &
!!       & -library        libGPF               &
!!       & -filename       `pwd`/M_verify.FF     &
!!       & -documentation  y                    &
!!       & -ufpp           y                    &
!!       & -ccall          n                    &
!!       & -archive        GPF.a                &
!!       & ')
!!
!!     ival=10
!!     call unit_check('myroutine', ival.gt.3 ,   msg='test if big enough')
!!     call unit_check('myroutine', ival.lt.100 , msg='test if small enough')
!!
!!     call unit_check_done('myroutine',msg='completed checks of "myroutine"')
!!
!!     end program demo_unit_check_start
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine unit_check_start(name,options,msg)

! ident_5="@(#)M_verify::unit_check_start(3f): call 'goodbad NAME start'"

character(len=*),intent(in)          :: name
character(len=*),intent(in),optional :: options
character(len=*),intent(in),optional :: msg
character(len=4096)                  :: var
!-----------------------------------------------------------------------------------------------------------------------------------
   call get_environment_variable('UNIT_CHECK_COMMAND',var)
   if(var.ne.'')unit_check_command=var
!-----------------------------------------------------------------------------------------------------------------------------------
   if(present(options))then
      if(unit_check_command.ne.'')then
         call execute_command_line(unit_check_command//' '//trim(name)//' start '//trim(options))
      endif
   else
      if(unit_check_command.ne.'')then
         call execute_command_line(unit_check_command//' '//trim(name)//' start')
      endif
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   call system_clock(clicks)
   duration=julian()
   if(present(msg))then
     if(msg.ne.'')then
        call stderr('unit_check_start: '//atleast(name,20)//' START   : '//trim(msg)) ! write message to standard error
     endif
   endif
   call get_environment_variable('M_verify_STOP',var)
   select case(var)
   case('FALSE','false')
         unit_check_keep_going=.false.
   case('1')
         unit_check_keep_going=.false.
   case('no','NO')
         unit_check_keep_going=.false.
   end select
!-----------------------------------------------------------------------------------------------------------------------------------
   IPASSED_G=0
   IFAILED_G=0
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine unit_check_start
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!
!!##NAME
!!    unit_check_done(3f) - [M_verify] call command "goodbad NAME good" or "goodbad NAME bad" depending on whether failures were found
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine unit_check_done(name,opts,msg)
!!
!!     character(len=*),intent(in) :: name
!!     character(len=*),intent(in),optional :: opts
!!     character(len=*),intent(in),optional :: msg
!!
!!##DESCRIPTION
!!
!!    If there have been no failures the shell command
!!
!!         goodbad NAME good [opts]
!!
!!    is executed, else the command
!!
!!         goodbad NAME bad [opts]
!!
!!    is executed and by default stops the program if their have been
!!    any failures.
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!     program demo_unit_check_done
!!     use M_verify, only: unit_check_start
!!     use M_verify, only: unit_check
!!     use M_verify, only: unit_check_good, unit_check_done, unit_check_bad
!!
!!     implicit none
!!     integer :: x
!!     x=10
!!     call unit_check_start('myroutine')
!!
!!     call unit_check('myroutine', x.gt.3 ,'test if big enough')
!!     call unit_check('myroutine', x.lt.100 ,'test if small enough')
!!
!!     if(x.ne.0)then
!!        call unit_check_done ('myroutine',msg='checks on "myroutine"' ) ! program execution stopped
!!     endif
!!
!!     end program demo_unit_check_done
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine unit_check_done(name,opts,msg)
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64

! ident_6="@(#)M_verify::unit_check_done(3f): call 'goodbad NAME bad'"

character(len=*),intent(in)          :: name
character(len=*),intent(in),optional :: opts
character(len=*),intent(in),optional :: msg
character(len=:),allocatable         :: msg_local
character(len=:),allocatable         :: opts_local
character(len=4096)                  :: out
character(len=9)                     :: pf
integer(kind=int64)                  :: milliseconds
integer                              :: clicks_now
   if(present(msg))then
      msg_local=msg
   else
      msg_local=''
   endif
   if(present(opts))then
      opts_local=opts
   else
      opts_local=''
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   if(unit_check_command.ne.'')then                           ! if system command name is not blank call system command
      if(ifailed_g.eq.0)then
         call execute_command_line(unit_check_command//' '//trim(name)//' bad '//trim(opts))
         if(.not.unit_check_keep_going) call fstop(1)            ! stop program depending on mode
      else
         call execute_command_line(unit_check_command//' '//trim(name)//' good '//trim(opts))
      endif
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   PF=merge('PASSED  :','FAILED  :',ifailed_G.eq.0)
   if(PF.eq.'PASSED  :'.and.ipassed_G.eq.0)then
      PF='UNTESTED:'
   endif
   if(duration.ne.0.0d0)then
      call system_clock(clicks_now)
      milliseconds=(julian()-duration)*1000
      milliseconds=clicks_now-clicks
      write(out,'("unit_check_done:  ",a, &
       & 1x,a,                            &
       & " DURATION:",i14.14,             &
       & " GOOD:",i0,                     &
       & 1x," BAD:",i0                    &
       & )')                              &
       & atleast(name,20),                &
       & PF,                              &
       & milliseconds,                    &
       & IPASSED_G,                       &
       & IFAILED_G
   else
      write(out,'("unit_check_done:  ",a,1x,a," GOOD:",i0,1x," BAD:",i0)') atleast(name,20),PF,IPASSED_G,IFAILED_G
   endif
   if(present(msg))then
      call stderr(trim(out)//': '//trim(msg))
   else
      call stderr(trim(out))
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   IPASSED_G=0
   IFAILED_G=0
   duration=0.0d0
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine unit_check_done
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!
!!##NAME
!!    unit_check_bad(3f) - [M_verify] call command "goodbad NAME bad" and stop program
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine unit_check_bad(name,opts,msg)
!!
!!     character(len=*),intent(in) :: name
!!     character(len=*),intent(in),optional :: opts
!!     character(len=*),intent(in),optional :: msg
!!
!!##DESCRIPTION
!!
!!    unit_check_bad(3f) calls the shell command
!!
!!         goodbad NAME bad [opts]
!!
!!    and stops the program. It is just a shortcut for calling
!!         call unit_check(name,.false.)
!!         call unit_check_done(name,opts,msg)
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!     program demo_unit_check_bad
!!     use M_verify, only: unit_check_start
!!     use M_verify, only: unit_check
!!     use M_verify, only: unit_check_good, unit_check_bad
!!
!!     implicit none
!!     integer :: x
!!     x=10
!!     call unit_check_start('myroutine')
!!
!!     call unit_check('myroutine', x.gt.3 ,'test if big enough')
!!     call unit_check('myroutine', x.lt.100 ,'test if small enough')
!!
!!     if(x.ne.0)then
!!        call unit_check_bad ('myroutine',msg='checks on "myroutine" failed') ! program execution stopped
!!     endif
!!
!!     end program demo_unit_check_bad
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
subroutine unit_check_bad(name,opts,msg)

! ident_7="@(#)M_verify::unit_check_bad(3f): call 'goodbad NAME bad'"

character(len=*),intent(in)          :: name
character(len=*),intent(in),optional :: opts
character(len=*),intent(in),optional :: msg
character(len=:),allocatable         :: msg_local
character(len=:),allocatable         :: opts_local
   if(present(msg))then
      msg_local=msg
   else
      msg_local=''
   endif
   if(present(opts))then
      opts_local=opts
   else
      opts_local=''
   endif
   call unit_check(name,.false.)
   call unit_check_done(name,opts_local,msg_local)
end subroutine unit_check_bad
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!
!!##NAME
!!    unit_check_good(3f) - [M_verify] call command "goodbad NAME good"
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine unit_check_good(name,opts,msg)
!!
!!     character(len=*),intent(in)          :: name
!!     character(len=*),intent(in),optional :: opts
!!     character(len=*),intent(in),optional :: msg
!!
!!##DESCRIPTION
!!    A shortcut for
!!
!!       call unit_check(name,.true.)
!!       call unit_check_done(name,opts,msg)
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!     program demo_unit_check_good
!!     use M_verify, only: unit_check_start, unit_check_done
!!     use M_verify, only: unit_check
!!     use M_verify, only: unit_check_good, unit_check_bad
!!
!!     implicit none
!!     integer :: x
!!     x=10
!!     call unit_check_start('myroutine')
!!
!!     call unit_check('myroutine', x.gt.3 ,'test if big enough')
!!     call unit_check('myroutine', x.lt.100 ,'test if small enough')
!!
!!     call unit_check_good('myroutine',msg='checks on "myroutine" ')
!!
!!     end program demo_unit_check_good
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine unit_check_good(name,opts,msg)

! ident_8="@(#)M_verify::unit_check_good(3f): call 'goodbad NAME good'"

character(len=*),intent(in)          :: name
character(len=*),intent(in),optional :: opts
character(len=*),intent(in),optional :: msg
character(len=:),allocatable         :: msg_local
character(len=:),allocatable         :: opts_local
   if(present(msg))then
      msg_local=msg
   else
      msg_local=''
   endif
   if(present(opts))then
      opts_local=opts
   else
      opts_local=''
   endif
   call unit_check(name,.true.,msg=msg_local)
   call unit_check_done(name,opts_local)
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine unit_check_good
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!      pdec(3f) - [M_verify] write out string with ASCII decimal equivalent vertically under it
!!      (LICENSE:PD)
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
!!    Given a string to print, PDEC() writes out the ASCII Decimal equivalent
!!    of the string directly underneath it. This can help you to locate
!!    unprintable characters or non-standard white-space such as a backspace
!!    character or tab character in input strings that your program could
!!    not interpret. On output, non-printable characters are replaced with
!!    a space, and trailing spaces are ignored.
!!
!!    You read the numbers vertically.
!!
!!    1. ignore trailing spaces
!!    2. print the character if it has an ADE of 32 on up
!!    3. print a space if it has an ADE of less than 32
!!    4. underneath each character print the ADE value vertically
!!    5. strings are assumed under 32767 characters in length.
!!       Format integer constants > 32767 are not supported on HP-UX
!!       when newer compilers are available use unlimited
!!
!!##EXAMPLES
!!
!!
!!    Sample program:
!!
!!       program demo_pdec
!!       use M_verify, only : pdec
!!       call pdec(' ABCDEFG abcdefg    ')
!!       end program demo_pdec
!!
!!    would produce (notice trailing space is trimmed):
!!
!!      > ABCDEFG abcdefg
!!      >0000000000001111
!!      >3666667739990000
!!      >2567890127890123
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine pdec(string)

! ident_9="@(#)M_verify::pdec(3f): write ASCII Decimal Equivalent (ADE) numbers vertically beneath string"

character(len=*),intent(in) :: string   ! the string to print
integer                     :: ilen     ! number of characters in string to print
integer                     :: i        ! counter used to step thru string
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
function atleast(line,length) result(strout)

! ident_10="@(#)M_verify::atleast(3fp): return string padded to at least specified length"

character(len=*),intent(in)  ::  line
integer,intent(in)           ::  length
character(len=max(length,len(trim(line)))) ::  strout
   strout=line
end function atleast
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    assert(3f) - [M_verify] print filename, linenumber, and message to stderr and stop program
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    function assert(file,linenum,expr,g1,g2g3,g4,g5,g6,g7,g8,g9)
!!
!!     character(len=*),intent(in)  :: file
!!     character(len=*),intent(in)  :: linenum
!!     logical,intent(in)           :: expr
!!     class(*),intent(in),optional :: g1,g2,g3,g4,g5,g6,g7,g8,g9
!!##DESCRIPTION
!!    assert(3f) prints strings to stderr and then stops program with exit
!!    code 1 It labels the first string as the filename, the next integer
!!    parameter as the linenumber, and then up to nine scalar values.
!!
!!    It is primarily intended for use by the ufpp(1) preprocessor $ASSERT
!!    directive
!!
!!##OPTIONS
!!
!!    filename   a string assumed to be the current filename when compiling
!!    linenum    assumed to be the line number of the source code the ASSERT(3f)
!!               procedure was called at.
!!    expr       logical value
!!    g[1-9]  optional value(s) to print as a message before stopping. May
!!            be of type INTEGER, LOGICAL, REAL, DOUBLEPRECISION, COMPLEX,
!!            or CHARACTER.
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!    program demo_assert
!!    use M_verify, only : assert
!!    implicit none
!!    real :: a, toobig=1024
!!    a=2000
!!    call assert('myroutine', 101, a.gt.toobig, 'The value is too large', a, '.gt.', toobig)
!!    end program demo_assert
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine assert(filename,linen,expr,g1, g2, g3, g4, g5, g6, g7, g8, g9)
implicit none

! ident_11="@(#)M_verify::assert(3f): writes a message to a string composed of any standard scalar types"

character(len=*),intent(in)   :: filename
integer,intent(in)            :: linen
logical,intent(in)            :: expr
class(*),intent(in),optional  :: g1 ,g2 ,g3 ,g4 ,g5
class(*),intent(in),optional  :: g6 ,g7 ,g8 ,g9

   ! write message to standard error
   if(.not.expr)then
      call stderr('ERROR:filename:',filename,':line number:',linen,':',str(g1,g2,g3,g4,g5,g6,g7,g8,g9))
      stop 1
   endif

end subroutine assert
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function julian()
! REFERENCE: From Wikipedia, the free encyclopedia 2015-12-19

! ident_12="@(#)M_verify::julian(3f): Converts proleptic Gregorian DAT date-time array to Julian Date"

real(kind=realtime)              :: julian   ! Julian Date (non-negative, but may be non-integer)
integer                          :: dat(8)   ! array like returned by DATE_AND_TIME(3f)
integer                          :: year, month, day, utc, hour, minute
real(kind=realtime)              :: second
integer                          :: A, Y, M, JDN

   call date_and_time(values=dat)
   year   = dat(1)                        ! Year
   month  = dat(2)                        ! Month
   day    = dat(3)                        ! Day
   utc    = dat(4)*60                     ! Delta from UTC, convert from minutes to seconds
   hour   = dat(5)                        ! Hour
   minute = dat(6)                        ! Minute
   second = dat(7)-utc+dat(8)/1000.0d0    ! Second   ! correction for time zone and milliseconds

!  You must compute first the number of years (Y) and months (M) since March 1st -4800 (March 1, 4801 BC)
   A=(14-month)/12 ! A will be 1 for January or February, and 0 for other months, with integer truncation
   Y=year+4800-A
   M=month+12*A-3  ! M will be 0 for March and 11 for February
!  All years in the BC era must be converted to astronomical years, so that 1BC is year 0, 2 BC is year "-1", etc.
!  Convert to a negative number, then increment towards zero
!  Staring from a Gregorian calendar date
   JDN=day + (153*M+2)/5 + 365*Y + Y/4 - Y/100 + Y/400 - 32045  !  with integer truncation
!  Finding the Julian Calendar date given the JDN (Julian day number) and time of day
   julian=JDN + dble(hour-12)/24.0d0 + dble(minute)/1440.0d0 + second/86400.0d0
end function julian
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    almost(3f) - [M_verify] return true or false if two numbers agree up to specified number of digits
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    function almost(x,y,digits)
!!
!!     class(*),intent(in)         :: x,y
!!     class(*),intent(in)         :: rdigits
!!     logical,intent(in),optional :: verbose
!!     logical                     :: almost
!!
!!##DESCRIPTION
!!    Returns true or false depending on whether the two numbers given agree
!!    to within the specified number of digits as calculated by ACCDIG(3f).
!!##OPTIONS
!!    x,y      expected and calculated values to be compared. May be of
!!             type REAL, INTEGER, or DOUBLEPRECISION.
!!    rdigits  real number representing number of digits of precision
!!             to compare
!!    verbose  optional value that specifies to print the results of the
!!             comparison when set to .TRUE..
!!##RETURNS
!!    almost   TRUE if the input values compare up to the specified number
!!             of values
!!##EXAMPLE
!!
!!   sample:
!!
!!    program demo_almost
!!    use M_verify, only : almost
!!    real    :: x, y
!!    logical :: z
!!    x=1.2345678
!!    y=1.2300000
!!    do i=1,8
!!       z=almost(x,y,real(i),verbose=.true.)
!!       write(*,*)i,z
!!    enddo
!!    end program demo_almost
!!
!!   output:
!!
!!     *almost* for values 1.23456776 1.23000002 agreement of 2.43020344 digits out of requested 1.0
!!            1   T
!!     *almost* for values 1.23456776 1.23000002 agreement of 2.43020344 digits out of requested 2.0
!!            2   T
!!     *almost* for values 1.23456776 1.23000002 agreement of 2.43020344 digits out of requested 3.0
!!            3   F
!!     *almost* for values 1.23456776 1.23000002 agreement of 2.43020344 digits out of requested 4.0
!!            4   F
!!     *almost* for values 1.23456776 1.23000002 agreement of 2.43020344 digits out of requested 5.0
!!            5   F
!!     *almost* for values 1.23456776 1.23000002 agreement of 2.43020344 digits out of requested 6.0
!!            6   F
!!     *almost* for values 1.23456776 1.23000002 agreement of 2.43020344 digits out of requested 7.0
!!            7   F
!!     *accdig* significant digit request too high= 8.00000000
!!     *almost* for values 1.23456776 1.23000002 agreement of 2.43020344 digits out of requested 8.0
!!            8   F
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
function almost(x,y,digits,verbose)
use M_journal,  only : journal

! ident_13="@(#)M_verify::almost(3f): function to compare two real numbers only up to a specified number of digits by calling DP_ACCDIG(3f)"

class(*),intent(in)         :: x,y
class(*),intent(in)         :: digits
logical,intent(in),optional :: verbose
logical                     :: almost

logical                     :: verbose_local
real                        :: acurcy
real                        :: digits_local
integer                     :: ind

   if(present(verbose))then
      verbose_local=verbose
   else
      verbose_local=.false.
   endif

   digits_local=anyscalar_to_real128(digits)
   acurcy=0.0
   select type(x)
   type is(real)
      select type(y)
      type is(real)
         call accdig(x,y,digits_local,acurcy,ind)
         if(verbose_local)then
            call journal('sc','*almost*','for values',x,y,'agreement of',acurcy,'digits out of requested',digits_local)
         endif
      class default
         call dp_accdig(x,y,digits_local,acurcy,ind)
         if(verbose_local)then
            call journal('sc','*almost*','for values',x,y,'agreement of',acurcy,'digits out of requested',digits_local)
         endif
      end select
   class default
      call dp_accdig(x,y,digits,acurcy,ind)
      if(verbose_local)then
         call journal('sc','*almost*','for values',x,y,'agreement of',acurcy,'digits out of requested',digits_local)
      endif
   end select

   if(ind.eq.0)then
      almost=.true.
   else
      almost=.false.
   endif

end function almost
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!      accdig(3f) - [M_verify] compare two real numbers only up to a specified number of digits
!!      (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!       subroutine accdig(x,y,digio,acurcy,ind)
!!
!!        real,intent(in)     :: X
!!        real,intent(in)     :: Y
!!        real,intent(in)     :: DIGI0
!!        real,intent(out)    :: acurcy
!!        integer,intent(out) :: ind
!!
!!##DESCRIPTION
!!    This procedure is used to check how closely two numbers agree.
!!
!!       call accdig(X,Y,DIGI0,ACURCY,IND)
!!
!!    The values X and Y are the numbers to compare, and DIGI0 is the
!!    threshold number of digits to consider significant in returning IND.
!!
!!    If X and Y are considered equal within DIGI0 relative tolerance,
!!
!!        IND    = 0, if tolerance is     satisfied.
!!               = 1, if tolerance is not satisfied.
!!
!!    The result ACURCY gives a measure of the number of leading digits in X
!!    which are the same as the number of leading digits in Y.
!!
!!            ACURCY=-log10((X-Y)/Y)   if X != Y and Y != 0
!!            ACURCY=-log10(X-Y)       if X != Y and Y = 0
!!            ACURCY=8                 if X=Y
!!
!!            ACURCY is never less than -8 or greater than 8
!!
!!    TOLERANCE ...
!!         X and Y are considered equal within DIGI0 relative tolerance,
!!         if ACURCY is greater than DIGI0.
!!
!!    For example, Take some numbers and compare then  to 1.2345678 ...
!!
!!       ================================================
!!       A number     |    ACURCY       |   ACURCY
!!                    |    1.2345678=Y  |   1.2345678=X
!!       ================================================
!!        1.234680    |    3.7900571    |   3.7901275
!!        1.2345378   |    4.6144510    |   4.6144404
!!        2.2234568   |    0.096367393  |   0.35188114
!!        1.2345678   |    8.0000000    |   8.0000000
!!        1.2345679   |    7.0732967    |   7.0731968
!!       -1.2345678   |   -0.30103000   |  -0.30103000
!!       76.234567    |   -1.7835463    |   0.0070906729
!!        2.4691356   |    0.0          |   0.3010300
!!        0.0         |    0.0          |  -0.91514942.
!!
!!    Due to the typical limits of the log function, the number of
!!    significant digits in the result is best considered to be three.
!!
!!    Notice that 1.2345678=Y produces different values than 1.2345678=X
!!
!!    A negative result indicates the two values being compared either do
!!    not agree in the first digit or they differ with respect to sign. An
!!    example of two numbers which do not agree in their leading digit (and
!!    actually differ in order of magnitude) is given above by X=76.234567
!!    and Y=1.2345678; the accuracy reported is -1.7835463. An example of
!!    two numbers which do not agree in sign in X=-1.2345678 and Y=1.2345678;
!!    here the accuracy reported is -0.30103000.
!!
!!##EXAMPLE
!!
!!
!!   Example program:
!!
!!    program demo_accdig ! fortran 90 example
!!    use M_verify, only : accdig
!!    implicit none
!!    integer :: digi
!!    integer :: i10, i20, i30
!!    integer :: ind, ind1, ind2
!!    real    :: acurcy, acurcy1, acurcy2
!!    real    :: a, b
!!    real    :: vals(9)
!!    data vals/ &
!!      &1.234680,   1.2345378,  2.2234568, 1.2345678, &
!!      &1.2345679, -1.2345678, 76.234567,  2.4691356, &
!!      &0.0/
!!       write(*,*)'========================='
!!       do i10=0,16
!!          a=1.0
!!          b=a+1.0/(10**i10)
!!          call accdig(a,b,8.0,acurcy,ind)
!!          write(*,*)i10,a,b,acurcy,ind
!!       enddo
!!       write(*,*)'========================='
!!       digi=16
!!       do i20=0,digi
!!          a=1.0
!!          b=a+1.0/(10**i20)
!!          call accdig(a,b,real(digi),acurcy,ind)
!!          write(*,*)i20,a,b,acurcy,ind
!!       enddo
!!       write(*,*)'========================='
!!       do i30=1,9
!!          call accdig(1.2345678,vals(i30),8.0,acurcy1,ind1)
!!          call accdig(vals(i30),1.2345678,8.0,acurcy2,ind2)
!!          write(*,*)i30,vals(i30),acurcy1,acurcy2,ind1,ind2
!!       enddo
!!    end program demo_accdig
!!
!!##REFERENCES
!!
!!   based on ...
!!
!!    NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. accdig V 7.00  2/14/90. **
!!       David Hogben,
!!       Statistical Engineering Division,
!!       Center for Computing and Applied Mathematics,
!!       A337 Administration Building,
!!       National Institute of Standards and Technology,
!!       Gaithersburg, MD 20899
!!                      TELEPHONE 301-975-2845
!!           ORIGINAL VERSION -  October, 1969.
!!            CURRENT VERSION - February, 1990.
!!            JSU     VERSION - February, 1991.
!!
!!##DEPENDENCIES
!!    o M_journal(),log10(), abs(1)
!!
!!##AUTHOR
!!    David Hogben, John S. Urban
!!
!!##LICENSE
!!    Public Domain
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE accdig(X,Y,digi0,ACURCY,IND)
use M_journal, only : journal
implicit none

! ident_14="@(#)M_verify::accdig(3f): compare two real numbers only up to a specified number of digits"

!     INPUT ...
real,intent(in) :: x           ! First  of two real numbers to be compared.
real,intent(in) :: y           ! Second of two real numbers to be compared.
real,intent(in) :: digi0       ! Number of digits to be satisfied in relative tolerance.
!     OUTPUT ...
integer,intent(out) :: ind     ! = 0, If tolerance is     satisfied.
! = 1, If tolerance is not satisfied.
real,intent(out)    :: acurcy  ! = - LOG10(ABS((X-Y)/Y)))

real     :: diff
real     :: digi
integer  :: ireal_significant_digits
!-----------------------------------------------------------------------------------------------------------------------------------
   ireal_significant_digits=int(log10(2.**digits(0.0))) ! maximum number of significant digits in a real number.
   digi=digi0
   if(digi.le.0)then
      call journal('sc','*accdig* bad number of significant digits=',digi)
      digi=ireal_significant_digits
   elseif(digi .gt. ireal_significant_digits)then
      call journal('sc','*accdig* significant digit request too high=',digi)
      digi=min(digi,real(ireal_significant_digits))
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   diff = x - y
   if(diff .eq. 0.0) then
      acurcy = digi
   elseif(y .eq. 0.0) then
      acurcy = - log10(abs(x))
   else
      acurcy = - log10(abs(diff)) + log10(abs(y))
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   if(acurcy .lt. digi ) then
      ind = 1
   else
      ind = 0
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
END SUBROUTINE accdig
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!      dp_accdig(3f) - [M_verify] compare two numbers only up to a specified number of digits
!!      (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!       subroutine dp_accdig(x,y,digio,acurcy,ind)
!!
!!        class(*),intent(in)  :: X
!!        class(*),intent(in)  :: Y
!!        class(*),intent(in)  :: DIGI0
!!        real,intent(out)     :: acurcy
!!        integer,intent(out)  :: ind
!!
!!##DESCRIPTION
!!
!!    This procedure is used to check how closely two numbers agree.
!!
!!       call dp_accdig(X,Y,DIGI0,ACURCY,IND)
!!
!!    The values X and Y are the numbers to compare, and DIGI0 is the
!!    threshold number of digits to consider significant in returning IND.
!!
!!    If X and Y are considered equal within DIGI0 relative tolerance,
!!
!!        IND    = 0, if tolerance is     satisfied.
!!               = 1, if tolerance is not satisfied.
!!
!!    The result ACURCY gives a measure of the number of leading digits in X
!!    which are the same as the number of leading digits in Y.
!!
!!         ACURCY=-log10((X-Y)/Y)   if X != Y and Y != 0
!!         ACURCY=-log10(X-Y)       if X != Y and Y = 0
!!         ACURCY=8                 if X=Y
!!
!!         ACURCY is never less than -8 or greater than 8 for REAL values
!!
!!    TOLERANCE ...
!!         X and Y are considered equal within DIGI0 relative tolerance,
!!         if ACURCY is greater than DIGI0.
!!
!!    For example, Take some numbers and compare then  to 1.2345678 ...
!!
!!       ================================================
!!       A number     |    ACURCY       |   ACURCY
!!                    |    1.2345678=Y  |   1.2345678=X
!!       ================================================
!!        1.234680    |    3.7900571    |   3.7901275
!!        1.2345378   |    4.6144510    |   4.6144404
!!        2.2234568   |    0.096367393  |   0.35188114
!!        1.2345678   |    8.0000000    |   8.0000000
!!        1.2345679   |    7.0732967    |   7.0731968
!!       -1.2345678   |   -0.30103000   |  -0.30103000
!!       76.234567    |   -1.7835463    |   0.0070906729
!!        2.4691356   |    0.0          |   0.3010300
!!        0.0         |    0.0          |  -0.91514942.
!!
!!    Due to the typical limits of the log function, the number of
!!    significant digits in the result is best considered to be three.
!!
!!    Notice that 1.2345678=Y produces different values than 1.2345678=X
!!
!!    A negative result indicates the two values being compared either do
!!    not agree in the first digit or they differ with respect to sign. An
!!    example of two numbers which do not agree in their leading digit (and
!!    actually differ in order of magnitude) is given above by X=76.234567
!!    and Y=1.2345678; the accuracy reported is -1.7835463. An example of
!!    two numbers which do not agree in sign in X=-1.2345678 and Y=1.2345678;
!!    here the accuracy reported is -0.30103000.
!!
!!##EXAMPLE
!!
!!
!!   Example program:
!!
!!    program demo_dp_accdig ! fortran 90 example
!!    use M_verify, only : dp_accdig
!!    implicit none
!!    integer         :: digi
!!    doubleprecision :: a, b
!!    integer         :: i10, i20, i30
!!    integer         :: ind, ind1, ind2
!!    real            :: acurcy, acurcy1, acurcy2
!!    doubleprecision :: vals(9)
!!    data vals/ &
!!      &1.234680d0,   1.2345378d0,  2.2234568d0, 1.2345678d0, &
!!      &1.2345679d0, -1.2345678d0, 76.234567d0,  2.4691356d0, &
!!      &0.0d0/
!!       write(*,*)'========================='
!!       do i10=0,16
!!          a=1.0d0
!!          b=a+1.0d0/(10**i10)
!!          call dp_accdig(a,b,8.0,acurcy,ind)
!!          write(*,*)i10,a,b,acurcy,ind
!!       enddo
!!       write(*,*)'========================='
!!       digi=16
!!       do i20=0,digi
!!          a=1.0d0
!!          b=a+1.0d0/(10**i20)
!!          call dp_accdig(a,b,dble(digi),acurcy,ind)
!!          write(*,*)i20,a,b,acurcy,ind
!!       enddo
!!       write(*,*)'========================='
!!       do i30=1,9
!!          call dp_accdig(1.2345678d0,vals(i30),8.0,acurcy1,ind1)
!!          call dp_accdig(vals(i30),1.2345678d0,8.0,acurcy2,ind2)
!!          write(*,*)i30,vals(i30),acurcy1,acurcy2,ind1,ind2
!!       enddo
!!    end program demo_dp_accdig
!!
!!##NOTES
!!##REFERENCES
!!
!!   based on ...
!!
!!    NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. dp_accdig V 7.00  2/14/90. **
!!       David Hogben,
!!       Statistical Engineering Division,
!!       Center for Computing and Applied Mathematics,
!!       A337 Administration Building,
!!       National Institute of Standards and Technology,
!!       Gaithersburg, MD 20899
!!                      TELEPHONE 301-975-2845
!!           ORIGINAL VERSION -  October, 1969.
!!            CURRENT VERSION - February, 1990.
!!            JSU     VERSION - February, 1991.
!!
!!##DEPENDENCIES
!!         o M_journal(), log10(), abs(1)
!!
!!##AUTHORS
!!      David Hogben, John S. Urban
!!
!!##LICENSE
!!      Public Domain
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE dp_accdig(x,y,digi0,ACURCY,IND)
use,intrinsic :: iso_fortran_env, only : real128
use M_journal,  only : journal
implicit none

! ident_15="@(#)M_verify::dp_accdig(3f): compare two values only up to a specified number of digits"

!  INPUT ...
class(*),intent(in)  :: x           ! FIRST  OF TWO NUMBERS TO BE COMPARED.
class(*),intent(in)  :: y           ! SECOND OF TWO NUMBERS TO BE COMPARED.
class(*),intent(in)  :: digi0       ! NUMBER OF DIGITS TO BE SATISFIED IN RELATIVE TOLERANCE.

real(kind=real128)   :: x_local
real(kind=real128)   :: y_local

!  OUTPUT ...
integer,intent(out)  :: ind         ! = 0, IF TOLERANCE IS     SATISFIED.
                                              ! = 1, IF TOLERANCE IS NOT SATISFIED.
real,intent(out)     :: acurcy      ! = - LOG10(ABS((x_local-y_local)/y_local)))
real(kind=real128)   :: diff
real(kind=real128)   :: digi
integer              :: idble_significant_digits
!-----------------------------------------------------------------------------------------------------------------------------------
   x_local=anyscalar_to_real128(x)
   y_local=anyscalar_to_real128(y)
   digi=anyscalar_to_real128(digi0)
!-----------------------------------------------------------------------------------------------------------------------------------
   idble_significant_digits=int(log10(2.0_real128**digits(0.0_real128))) ! MAXIMUM NUMBER OF SIGNIFICANT DIGITS IN A REAL128 NUMBER.
   if(digi.le.0)then
      call journal('sc','*dp_accdig* bad number of significant digits=',real(digi,kind=real128))
      digi=idble_significant_digits
   elseif(digi .gt. idble_significant_digits)then
      call journal('sc','*dp_accdig* significant digit request too high=',real(digi,kind=real128))
      digi=min(digi,real(idble_significant_digits,kind=real128))
   endif
   diff = x_local - y_local
   if(diff .eq. 0.0_real128) then
      acurcy = digi
   elseif(y_local .eq. 0.0_real128) then
      acurcy = - log10(abs(x_local))
   else
      acurcy = - log10(abs(diff)) + log10(abs(y_local))
   endif
   if(acurcy .lt. digi ) then
      ind = 1
   else
      ind = 0
   endif
end subroutine dp_accdig
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!   in_margin(3f) - [M_verify] check if two reals are approximately equal using a relative margin
!!
!!##SYNOPSIS
!!
!!     elemental pure function in_margin( expected_value, measured_value, allowed_margin )
!!
!!      real, intent(in)    :: expected_value
!!      real, intent(in)    :: measured_value
!!      real, intent(in)    :: allowed_margin
!!      class(*),intent(in) :: invalue
!!
!!##DESCRIPTION
!!   Compare two values to see if they are relatively equal using the
!!   specified allowed margin. That is, see if VALUE_MEASURED is in
!!   the range VALUE_EXPECTED +- ALLOWED_ERROR where the allowed error
!!   varies with the magnitude of the values, such that the allowed error
!!   is margin * average magnitude of measured and expected).
!!
!!   So the allowed error is smaller when the magnitudes are smaller.
!!
!!##OPTIONS
!!   expected_value   First value
!!   measured_value   Second value
!!   allowed_margin   Allowed relative margin
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_in_margin
!!    use :: M_verify, only : in_margin
!!    implicit none
!!    write(*,*) in_margin(4.00000,3.99999,0.000000001)
!!    write(*,*) in_margin(4.00000,3.99999,0.00000001)
!!    write(*,*) in_margin(4.00000,3.99999,0.0000001)
!!    write(*,*) in_margin(4.00000,3.99999,0.000001)
!!
!!    write(*,*) in_margin([4.0,40.0,400.0,4000.0,40000.0], [3.9,39.9,399.9,3999.9,39999.9] ,0.000001)
!!    write(*,*) in_margin([4.0,40.0,400.0,4000.0,40000.0], [3.9,39.9,399.9,3999.9,39999.9] ,0.00001)
!!
!!    write(*,*) in_margin(4.00000,3.99999,0.00001)
!!    write(*,*) in_margin(4.00000,3.99999,0.0001)
!!    write(*,*) in_margin(4.00000,3.99999,0.001)
!!    write(*,*) in_margin(4.00000,3.99999,0.01)
!!
!!    end program demo_in_margin
!!
!!   Results:
!!
!!     F
!!     F
!!     F
!!     F
!!     F F F F F
!!     F F F F T
!!     T
!!     T
!!     T
!!     T
!===================================================================================================================================
elemental pure function in_margin(expected_value, measured_value, allowed_margin)
implicit none

! ident_16="@(#)M_verify::in_margin(3f): check if two reals are approximately equal using a relative margin"

class(*),intent(in) :: expected_value, measured_value, allowed_margin
logical             :: in_margin

   doubleprecision     :: expected, measured, margin

   expected=anyscalar_to_double(expected_value)
   measured=anyscalar_to_double(measured_value)
   margin=anyscalar_to_double(allowed_margin)

   if ( abs(expected-measured) > 0.50d0 * margin * (abs(expected)+abs(measured)) ) then
      in_margin=.false.  ! values not comparable
   else
      in_margin=.true.   ! values comparable
   endif

end function in_margin
function round(val,idigits0)
implicit none

! ident_17="@(#)M_verify::round(3f): round val to specified number of significant digits"

integer,parameter          :: dp=kind(0.0d0)
real(kind=dp),intent(in)   :: val
integer,intent(in)         :: idigits0
   integer                 :: idigits,ipow
   real(kind=dp)           :: aval,rnormal
   real(kind=dp)           :: round
!  this does not work very well because of round-off errors.
!  Make a better one, probably have to use machine-dependent bit shifting
   ! make sure a reasonable number of digits has been requested
   idigits=max(1,idigits0)
   aval=abs(val)
!  select a power that will normalize the number
!  (put it in the range 1 > abs(val) <= 0)
   if(aval.ge.1)then
      ipow=int(log10(aval)+1)
   else
      ipow=int(log10(aval))
   endif
   rnormal=val/(10.0d0**ipow)
   if(rnormal.eq.1)then
      ipow=ipow+1
   endif
   !normalize, multiply by 10*idigits to an integer, and so on
   round=real(anint(val*10.d0**(idigits-ipow)))*10.d0**(ipow-idigits)
end function round
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
pure elemental function anyscalar_to_real128(valuein) result(d_out)
use, intrinsic :: iso_fortran_env, only : error_unit !! ,input_unit,output_unit
implicit none

! ident_18="@(#)M_verify::anyscalar_to_real128(3f): convert integer or real parameter of any kind to real128"

class(*),intent(in)          :: valuein
real(kind=real128)           :: d_out
character(len=3)             :: readable
   select type(valuein)
   type is (integer(kind=int8));   d_out=real(valuein,kind=real128)
   type is (integer(kind=int16));  d_out=real(valuein,kind=real128)
   type is (integer(kind=int32));  d_out=real(valuein,kind=real128)
   type is (integer(kind=int64));  d_out=real(valuein,kind=real128)
   type is (real(kind=real32));    d_out=real(valuein,kind=real128)
   type is (real(kind=real64));    d_out=real(valuein,kind=real128)
   Type is (real(kind=real128));   d_out=valuein
   type is (logical);              d_out=merge(0.0_real128,1.0_real128,valuein)
   type is (character(len=*));     read(valuein,*) d_out
   class default
    !!d_out=huge(0.0_real128)
    readable='NaN'
    read(readable,*)d_out
    !!stop '*M_verify::anyscalar_to_real128: unknown type'
   end select
end function anyscalar_to_real128
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
pure elemental function anyscalar_to_double(valuein) result(d_out)
use, intrinsic :: iso_fortran_env, only : error_unit !! ,input_unit,output_unit
implicit none

! ident_19="@(#)M_verify::anyscalar_to_double(3f): convert integer or real parameter of any kind to doubleprecision"

class(*),intent(in)       :: valuein
doubleprecision           :: d_out
doubleprecision,parameter :: big=huge(0.0d0)
   select type(valuein)
   type is (integer(kind=int8));   d_out=dble(valuein)
   type is (integer(kind=int16));  d_out=dble(valuein)
   type is (integer(kind=int32));  d_out=dble(valuein)
   type is (integer(kind=int64));  d_out=dble(valuein)
   type is (real(kind=real32));    d_out=dble(valuein)
   type is (real(kind=real64));    d_out=dble(valuein)
   Type is (real(kind=real128))
      !!if(valuein.gt.big)then
      !!   write(error_unit,*)'*anyscalar_to_double* value too large ',valuein
      !!endif
      d_out=dble(valuein)
   type is (logical);              d_out=merge(0.0d0,1.0d0,valuein)
   type is (character(len=*));      read(valuein,*) d_out
   !type is (real(kind=real128))
   !   if(valuein.gt.big)then
   !      write(error_unit,*)'*anyscalar_to_double* value too large ',valuein
   !   endif
   !   d_out=dble(valuein)
   class default
     d_out=0.0d0
     !!stop '*M_verify::anyscalar_to_double: unknown type'
   end select
end function anyscalar_to_double
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
end module M_verify
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
