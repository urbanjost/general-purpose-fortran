










!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
module M_f2kcli
!>
!!
!!##NAME
!!   M_F2KCLI(3f) - [M_f2kcli]Fortran 200x Command Line Interface for Programming Environments without it
!!##SYNOPSIS
!!
!!   use M_f2kcli, only: command_argument_count
!!   use M_f2kcli, only: get_command
!!   use M_f2kcli, only: get_command_argument
!!##DESCRIPTION
!!
!!    The M_f2kcli(3f) module contains procedures useful for reading
!!    command line arguments from Fortran programs in Fortran 90 programming
!!    environments that do not support the new Fortran 2003 standardized
!!    procedures.
!!
!!    An extensive set of such routines is available on-line from
!!    Winteractor. The M_F2KCLI(3f) module found here is based on a single
!!    routine from this collection.
!!
!!    M_F2KCLI is an implementation of the Fortran command line interface as
!!    defined in the Fortran F2003 standard:
!!
!!     COMMAND_ARGUMENT_COUNT : Returns the number of command arguments.
!!     GET_COMMAND_ARGUMENT   : Returns a command argument.
!!     GET_COMMAND            : Returns the entire command by which
!!                              the program was invoked.
!!
!!    F2KCLI(2f) is intended to provide the definitive solution to the
!!    long standing lack of standardised command line access in Fortran,
!!    until F2003 compilers become generally available.
!!
!!    For the latest version of F2KCLI go to:
!!
!!       http://www.winteracter.com/f2kcli
!!
!!##AUTHOR
!!
!!  Copyright Interactive Software Services Ltd. 2001
!!
!!  Platform     : Unix/1
!!  Compiler     : Any Fortran 9x compiler supporting IARGC/GETARG
!!                 which counts the first true command line argument
!!                 after the program name as argument number one.
!!                 (Excludes compilers which require a special USE
!!                 statement to make IARGC/GETARG available).
!!  Implementer  : Lawson B. Wakefield, I.S.S. Ltd.
!!  Date         : February 2001
!!##CONDITIONS OF USE
!!    F2KCLI has been developed by Interactive Software Services Ltd. based on
!!    the F2003 standard, on a voluntary basis. The F2KCLI source code, object
!!    code and documentation is copyright Interactive Software Services Ltd.
!!    2001-2005 (with the obvious exception of the definitions of the F2003
!!    command line interface routines themselves).
!!
!!    This software is provided 'as-is', without any express or implied
!!    warranty. In no event will the copyright owner be held liable for any
!!    damages arising from the use of this software.
!!
!!    Permission is granted to anyone to use this software for any purpose,
!!    including commercial applications, subject to the following conditions:
!!
!!    1. The origin of this software must not be misrepresented; you must not
!!       claim that you wrote the original software. If you use this software
!!       in a product, an acknowledgment in the product documentation would
!!       be appreciated but is not required.
!!
!!    2. The supplied source code may be altered, but only to correct any
!!       failure to conform to the F2003 command line interface standard or
!!       to allow its use with a previously unsupported compiler/platform
!!       combination.
!!
!!    3. Altered source versions must be plainly marked as such and must not
!!       be misrepresented as being the original software.
!!
!!    4. You may not sell F2KCLI as a product in its own right. F2KCLI is
!!       free software (as in "free lunch" or "free beer").
!!
!!    If you amend F2KCLI or develop an implementation for a previously
!!    unsupported compiler/platform combination, you are invited to contribute
!!    your implementation to the F2KCLI file set. The origin of any such
!!    contributions will be fully acknowledged.
!!##EXAMPLE
!!
!!   Sample program
!!
!!    program demo_M_f2kcli
!!    use M_f2kcli, only : command_argument_count
!!    use M_f2kcli, only : get_command
!!    use M_f2kcli, only : get_command_argument
!!    implicit none
!!    character(len=256) :: line
!!    character(len=256) :: exe
!!    character(len=40)  :: cmd
!!    integer            :: narg,iarg
!!       narg = command_argument_count()
!!       write(unit=*,fmt=*) "Arg count=", narg
!!       call get_command(line)
!!       write(unit=*,fmt=*) "Line=",trim(line)
!!       call get_command_argument(0,exe)
!!       write(unit=*,fmt=*) "Program=",trim(exe)
!!       do iarg = 1,narg
!!          call get_command_argument(iarg,cmd)
!!          WRITE(unit=*,fmt=*) "Arg ",IARG,"=",CMD
!!       enddo
!!       stop
!!    end program demo_M_f2kcli
public test_suite_M_f2kcli
contains
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
SUBROUTINE GET_COMMAND(COMMAND,LENGTH,STATUS)
!
! Description. Returns the entire command by which the program was
!   invoked.
!
! Class. Subroutine.
!
! Arguments.
! COMMAND (optional) shall be scalar and of type default character.
!   It is an INTENT(OUT) argument. It is assigned the entire command
!   by which the program was invoked. If the command cannot be
!   determined, COMMAND is assigned all blanks.
! LENGTH (optional) shall be scalar and of type default integer. It is
!   an INTENT(OUT) argument. It is assigned the significant length
!   of the command by which the program was invoked. The significant
!   length may include trailing blanks if the processor allows commands
!   with significant trailing blanks. This length does not consider any
!   possible truncation or padding in assigning the command to the
!   COMMAND argument; in fact the COMMAND argument need not even be
!   present. If the command length cannot be determined, a length of
!   0 is assigned.
! STATUS (optional) shall be scalar and of type default integer. It is
!   an INTENT(OUT) argument. It is assigned the value 0 if the
!   command retrieval is sucessful. It is assigned a processor-dependent
!   non-zero value if the command retrieval fails.
!
      CHARACTER(LEN=*), INTENT(OUT), OPTIONAL :: COMMAND
      INTEGER         , INTENT(OUT), OPTIONAL :: LENGTH
      INTEGER         , INTENT(OUT), OPTIONAL :: STATUS
!
      INTEGER                   :: IARG,NARG,IPOS
      INTEGER            , SAVE :: LENARG
      CHARACTER(LEN=2000), SAVE :: ARGSTR
      LOGICAL            , SAVE :: GETCMD = .TRUE.
!
! Under Unix we must reconstruct the command line from its constituent
! parts. This will not be the original command line. Rather it will be
! the expanded command line as generated by the shell.
!
      IF (GETCMD) THEN
         NARG = IARGC()
         IF (NARG > 0) THEN
            IPOS = 1
            DO IARG = 1,NARG
               CALL GETARG(IARG,ARGSTR(IPOS:))
               LENARG = LEN_TRIM(ARGSTR)
               IPOS   = LENARG + 2
               IF (IPOS > LEN(ARGSTR)) EXIT
            END DO
         ELSE
            ARGSTR = ' '
            LENARG = 0
         ENDIF
         GETCMD = .FALSE.
      ENDIF
      IF (PRESENT(COMMAND)) COMMAND = ARGSTR
      IF (PRESENT(LENGTH))  LENGTH  = LENARG
      IF (PRESENT(STATUS))  STATUS  = 0
      RETURN
   END SUBROUTINE GET_COMMAND
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
INTEGER FUNCTION COMMAND_ARGUMENT_COUNT()
!
! Description. Returns the number of command arguments.
!
! Class. Inquiry function
!
! Arguments. None.
!
! Result Characteristics. Scalar default integer.
!
! Result Value. The result value is equal to the number of command
!   arguments available. If there are no command arguments available
!   or if the processor does not support command arguments, then
!   the result value is 0. If the processor has a concept of a command
!   name, the command name does not count as one of the command
!   arguments.
!
COMMAND_ARGUMENT_COUNT = IARGC()
END FUNCTION COMMAND_ARGUMENT_COUNT
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
   SUBROUTINE GET_COMMAND_ARGUMENT(NUMBER,VALUE,LENGTH,STATUS)
!
! Description. Returns a command argument.
!
! Class. Subroutine.
!
! Arguments.
! NUMBER shall be scalar and of type default integer. It is an
!   INTENT(IN) argument. It specifies the number of the command
!   argument that the other arguments give information about. Useful
!   values of NUMBER are those between 0 and the argument count
!   returned by the COMMAND_ARGUMENT_COUNT intrinsic.
!   Other values are allowed, but will result in error status return
!   (see below).  Command argument 0 is defined to be the command
!   name by which the program was invoked if the processor has such
!   a concept. It is allowed to call the GET_COMMAND_ARGUMENT
!   procedure for command argument number 0, even if the processor
!   does not define command names or other command arguments.
!   The remaining command arguments are numbered consecutively from
!   1 to the argument count in an order determined by the processor.
! VALUE (optional) shall be scalar and of type default character.
!   It is an INTENT(OUT) argument. It is assigned the value of the
!   command argument specified by NUMBER. If the command argument value
!   cannot be determined, VALUE is assigned all blanks.
! LENGTH (optional) shall be scalar and of type default integer.
!   It is an INTENT(OUT) argument. It is assigned the significant length
!   of the command argument specified by NUMBER. The significant
!   length may include trailing blanks if the processor allows command
!   arguments with significant trailing blanks. This length does not
!   consider any possible truncation or padding in assigning the
!   command argument value to the VALUE argument; in fact the
!   VALUE argument need not even be present. If the command
!   argument length cannot be determined, a length of 0 is assigned.
! STATUS (optional) shall be scalar and of type default integer.
!   It is an INTENT(OUT) argument. It is assigned the value 0 if
!   the argument retrieval is sucessful. It is assigned a
!   processor-dependent non-zero value if the argument retrieval fails.
!
! NOTE
!   One possible reason for failure is that NUMBER is negative or
!   greater than COMMAND_ARGUMENT_COUNT().
!
      INTEGER         , INTENT(IN)            :: NUMBER
      CHARACTER(LEN=*), INTENT(OUT), OPTIONAL :: VALUE
      INTEGER         , INTENT(OUT), OPTIONAL :: LENGTH
      INTEGER         , INTENT(OUT), OPTIONAL :: STATUS
!
!  A temporary variable for the rare case case where LENGTH is
!  specified but VALUE is not. An arbitrary maximum argument length
!  of 1000 characters should cover virtually all situations.
!
      CHARACTER(LEN=1000) :: TMPVAL
!
! Possible error codes:
! 1 = Argument number is less than minimum
! 2 = Argument number exceeds maximum
!
      IF (NUMBER < 0) THEN
         IF (PRESENT(VALUE )) VALUE  = ' '
         IF (PRESENT(LENGTH)) LENGTH = 0
         IF (PRESENT(STATUS)) STATUS = 1
         RETURN
      ELSE IF (NUMBER > IARGC()) THEN
         IF (PRESENT(VALUE )) VALUE  = ' '
         IF (PRESENT(LENGTH)) LENGTH = 0
         IF (PRESENT(STATUS)) STATUS = 2
         RETURN
      END IF
!
! Get the argument if VALUE is present
!
      IF (PRESENT(VALUE)) CALL GETARG(NUMBER,VALUE)
!
! The LENGTH option is fairly pointless under Unix.
! Trailing spaces can only be specified using quotes.
! Since the command line has already been processed by the
! shell before the application sees it, we have no way of
! knowing the true length of any quoted arguments. LEN_TRIM
! is used to ensure at least some sort of meaningful result.
!
      IF (PRESENT(LENGTH)) THEN
         IF (PRESENT(VALUE)) THEN
            LENGTH = LEN_TRIM(VALUE)
         ELSE
            CALL GETARG(NUMBER,TMPVAL)
            LENGTH = LEN_TRIM(TMPVAL)
         END IF
      END IF
!
! Since GETARG does not return a result code, assume success
!
      IF (PRESENT(STATUS)) STATUS = 0
END SUBROUTINE GET_COMMAND_ARGUMENT
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine test_suite_M_f2kcli()
use M_verify, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg
use M_verify, only : unit_check_level

!! setup
   call test_command_argument_count()
   call test_get_command()
   call test_get_command_argument()
!! teardown
contains
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_command_argument_count()

   call unit_check_start('command_argument_count',msg='')
   !!call unit_check('command_argument_count', 0.eq.0, 'checking',100)
   call unit_check_done('command_argument_count',msg='')
end subroutine test_command_argument_count
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_get_command()

   call unit_check_start('get_command',msg='')
   !!call unit_check('get_command', 0.eq.0, 'checking',100)
   call unit_check_done('get_command',msg='')
end subroutine test_get_command
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_get_command_argument()

   call unit_check_start('get_command_argument',msg='')
   !!call unit_check('get_command_argument', 0.eq.0, 'checking',100)
   call unit_check_done('get_command_argument',msg='')
end subroutine test_get_command_argument
!===================================================================================================================================
end subroutine test_suite_M_f2kcli
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
end module M_f2kcli
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
