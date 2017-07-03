!>
!!##NAME
!!     cond(3f) - [M_logic]process input files with embedded if/else/elseif/endif commands
!!##SYNOPSIS
!!
!!
!!     subroutine cond(verb,line,ierr)
!!
!!      character(len=*),intent(in) :: verb
!!      character(len=*),intent(in) :: line
!!      integer,intent(out)         :: ierr
!!
!!     Public module variables include
!!
!!      WRITE -- flag whether next input line should be read
!!
!!##OPTIONS
!!
!!      verb      must be 'if','elseif','else','endif'
!!      line      calculator expression to reduce to an integer.
!!      ierr      indicate if an error occurred
!!
!!##DESCRIPTION
!!
!!     Many programs read input files where input lines are conditionally skipped over. It may make sense that the data file itself
!!     contains the conditionals (think of pre-processors like cpp(1), shell programs (ie. interpreted languages), and configuration
!!     files that are read from multiple programs and from different environments). Instead of having many code-specific ways of
!!     specifying which lines will be processed, this module allows you to embed if/else/elseif/endif directives in the input files.
!!
!!     If you pass the conditional lines to cond(3f) it sets the logical variable "WRITE" to indicate when non-conditional lines
!!     (blocks of lines between those logical conditions) should be skipped.
!!
!!     if ierr=0, no obvious error occurred. if ierr not equal=0, suggest stopping the calling program from processing input
!!
!!     The variable NEST_LEVEL should be zero at the end, or a nesting error occurred.
!!
!!##EXAMPLES
!!
!!
!!   The example program uses cond(3f) and the calculator module to let you have input files that the program or the input file (via
!!   the #define command) can set values for. Note that expressions on the #if and #elseif input lines are truncated to whole
!!   numbers. You need to see the calculator documentation to see all the expressions available (date and time functions, most
!!   FORTRAN 77 intrinsic functions, ...)
!!
!!   Here is an example program that will read in a file and write just lines from the file to output that are in true blocks...
!!
!!    program logic
!!    use M_journal, only : journal  ! for logging messages
!!    use M_strings, only : lower, delim,v2s ! convert character case; split string
!!    use M_logic, only : cond
!!    use M_logic, only : write ! flag whether current data lines should be written
!!    use M_logic, only : nest_level   ! nesting level for #IF/#ELSEIF/#ELSE/#ENDIF
!!    use M_calculator_plus, only : rnum0
!!    character(len=1)    :: prefix              ! directive prefix character
!!    character(len=1024) :: line                ! input line
!!    integer,parameter   :: max_words=2  ! maximum number of words allowed on line
!!    character(len=1024) :: array(max_words)    ! working copy of input line
!!    ! location where words start and end
!!    integer             :: ibegin(max_words), iterm(max_words)
!!    !----------------------------------------------------------------------------
!!    PREFIX='#'              ! for the example, assume direct lines use a # prefix
!!    !----------------------------------------------------------------------------
!!    READLINE: do                                   ! read loop to read input file
!!       read(*,'(a)',iostat=ios) line
!!       if(ios.ne.0)then
!!          if (nest_level.ne.0) then ! check to make sure all if blocks are closed
!!             call journal('sc',&
!!             &'*logic* error - #IF BLOCK NOT CLOSED WHEN READiNG FILE FINISHED.')
!!          endif
!!          stop
!!       endif
!!       ! although delim(3f) can do more
!!       ! just parsing the first word out and finding where second word starts
!!       ! make sure array is initialized for when
!!       ! icount(number of words on line) is zero
!!       array=' '
!!       call delim(lower(line),array,max_words,icount,ibegin,iterm,ilen,' ')
!!       select case(array(1))
!!       ! find conditional lines
!!       case('#if','#else','#elseif','#endif')
!!          ! process conditional directive
!!          call cond(trim(array(1)(2:)),line(iterm(1)+1:),ierr_logic)
!!       case('#define')
!!          ! evaluate expression
!!          value=rnum0(line(iterm(1)+1:))
!!       case default
!!          ! find input lines you want to use, skip others
!!          if (write) then
!!             ! for example, if last conditional was true then write line
!!             write(*,'(a)') trim(line)
!!             ! write data line
!!          endif
!!       end select
!!    enddo READLINE
!!    end program logic
!!
!!   Here is an input file for the test program
!!
!!    >#define A=10
!!    >#define B=1234.0
!!    >#define C=sin(30)
!!    >DEFINED SOME VALUES AND READY TO START
!!    >#if eq(A,20)
!!    >   SHOULD NOT BE OUTPUT
!!    >#elseif eq(A,10)
!!    >  CORRECT BRANCH WRITE THIS
!!    >   #if gt(B,A)
!!    >      CORRECT AGAIN
!!    >   #else
!!    >      SHOULD NOT BE IN OUTPUT EITHER
!!    >   #endif
!!    >#else
!!    >   SHOULD NOT BE OUTPUT ONCE AGAIN
!!    >#endif
!!    >GOT TO END
!!
!!   The output should be
!!
!!    >DEFINED SOME VALUES AND READY TO START
!!    >  CORRECT BRANCH WRITE THIS
!!    >      CORRECT AGAIN
!!    >GOT TO END
!===================================================================================================================================
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
module M_logic
!  Author: John S. Urban
!  last updated: 2013-06-11
!===================================================================================================================================
! given a verb that is one of if|elseif|else|endif and numeric expressions on if|elseif return the flag "write"
! that indicates whether the blocks of lines between these logical conditionals should be processed or not.
!
! if ierr=0, no obvious error occurred.
! if ierr not equal=0, suggest stopping the calling program from processing input
!===================================================================================================================================
use M_journal, only : journal
use M_calculator_plus, only : inum0
implicit none
private
character(len=*),parameter :: ident="@(#)M_logic(3fm): Allows if/else/elseif/endif logic to be applied to input files"
   integer,parameter   ::  max_nest_level=20                  ! maximum nesting level of conditionals
   logical,save        :: condop(0:max_nest_level)            ! flag to keep track of previous write flags
   data condop /.true.,max_nest_level*.false./
   integer             :: ierr_logic=0                        !
   logical             :: dc                                  ! flag to determine write flag
   integer,public,save,protected :: nest_level=0              ! count of if/elseif/else/endif nesting level
   logical,public,save,protected :: write=.true.              ! flag indicating whether current lines should be processed
   ! SUBROUTINES:
   public  :: cond                                            !
   private :: if                                              !
   private :: evalit                                          !
   private :: else                                            !
   private :: elseif                                          !
   private :: endif                                           !
   contains
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine cond(verb,line,ierr)
character(len=*),parameter :: ident="@(#)M_logic::cond(3f): process if/else/elseif/endif directives passed from input files"
character(len=*),intent(in)       :: verb                     ! verb must be if, elseif, else, endif
character(len=*),intent(in)       :: line                     ! line is an expression to reduce to an integer.
integer,intent(out)               :: ierr                     ! indicate if an error occurred
   logical,save                   :: eb=.false.               !
   integer,save                   :: noelse=0                 !
!-----------------------------------------------------------------------------------------------------------------------------------
   ierr_logic=0
!-----------------------------------------------------------------------------------------------------------------------------------
   select case(verb)                                          ! process directive based on first two characters
   case('if');  call if(line,noelse,eb)                       ! input is an IF directive
   case('elseif');   call elseif(line,noelse,eb)              ! input is an ELSEIF directive
   case('else');     call else(noelse,eb)                     ! input is an ELSE directive
   case('endif');    call endif(noelse,eb)                    ! input is an ENDIF directive
   case default
      call journal('sc','*logic* FATAL - UNKNOWN DIRECTIVE '//trim(verb))
   end select
   ierr=ierr_logic
end subroutine cond
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine if(line,noelse,eb)
character(len=*),parameter :: ident="@(#)M_logic::if(3fp): process IF command in input files"
character(len=*)               :: line                     ! line        -
integer                        :: noelse
logical                        :: eb
!-----------------------------------------------------------------------------------------------------------------------------------
   noelse=0
   write=.false.
   nest_level=nest_level+1                                    ! increment IF nest level
   if (nest_level.gt.max_nest_level) then
      call journal('sc','*logic* ABORT - "IF" BLOCK NESTING TOO DEEP:',nest_level)
      ierr_logic=-30
      nest_level=0
      write=.true.
      return
   endif
   call evalit(line)                                          ! evaluate line
   if (.not.dc.or..not.condop(nest_level-1).or.eb)then
      return                                                  ! check to make sure previous IF was true
   endif
   condop(nest_level)=.true.
   write=condop(nest_level)
end subroutine if
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine evalit(line)
character(len=*),parameter :: ident="@(#)M_logic::evalit(3fp): evaluate expression from an IF/ELSEIF and set write flag"
character(len=*),intent(in):: line
integer                    :: ival
integer                    :: status
   ival=inum0(line,ierr=status)
   if(ival.eq.0)then
      dc=.true.
   else
      dc=.false.
   endif
   if(status.ne.0)then
      dc=.false.
   endif
end subroutine evalit
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine else(noelse,eb)                                    ! process else
character(len=*),parameter :: ident="@(#)M_logic::else(3fp): process ELSE command in input files"
integer                    :: noelse
logical                    :: eb
!-----------------------------------------------------------------------------------------------------------------------------------
   if (noelse.eq.1.or.nest_level.eq.0) then                   ! test for else instead of elseif
      call journal('sc','*logic* FATAL - MISPLACED "ELSE" DIRECTIVE.')
      ierr_logic=-10
      nest_level=0
      write=.true.
      return
   endif
   noelse=1
   if (.not.condop(nest_level-1)) return                      ! if was true so ignore else
     eb=.false.
   if (condop(nest_level)) then
       eb=.true.
       write=.false.
   else                                                       ! else detected
     condop(nest_level)=.true.
     write=.true.
   endif
end subroutine else
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine elseif(line,noelse,eb)                             ! process elseif
character(len=*),parameter :: ident="@(#)M_logic::elseif(3fp): process ELSEIF command in input files"
logical                    :: eb
integer                    :: noelse
character(len=*)           :: line                            ! line        - integer expression to evaluate
!-----------------------------------------------------------------------------------------------------------------------------------
   if (noelse.eq.1.or.nest_level.eq.0) then                   ! test for else instead of elseif
      call journal('sc','*logic* FATAL - MISPLACED "ELSEIF" DIRECTIVE.')
      ierr_logic=-20
      nest_level=0
      write=.true.
      return
   endif
   if (.not.condop(nest_level-1)) return                      ! if was true so ignore else
     eb=.false.
   if (condop(nest_level)) then
       eb=.true.
       write=.false.
   else                                                       ! elseif detected
     nest_level=nest_level-1                                  ! decrease if level because it will be incremented in subroutine if
     call if(line,noelse,eb)
   endif
end subroutine elseif
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine endif(noelse,eb)                                   ! process ENDIF directive
character(len=*),parameter :: ident="@(#)M_logic::endif(3fp): process ENDIF command in input files"
logical                    :: eb
integer                    :: noelse
!-----------------------------------------------------------------------------------------------------------------------------------
   ! if no ELSE or ELSEIF present insert ELSE to simplify logic
   if(noelse.eq.0)then
      call else(noelse,eb)
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   nest_level=nest_level-1                                    ! decrease if level
   if(nest_level.lt.0)then
      call journal('sc','*logic* FATAL - MISPLACED "ENDIF" DIRECTIVE.')
      ierr_logic=-40
      nest_level=0
      write=.true.
      return
   endif
   noelse=0                                                   ! reset else level
   eb=.not.condop(nest_level+1)
   write=.not.eb
   condop(nest_level+1)=.false.
   if (nest_level.eq.0) then
         write=.true.
         eb=.false.
   endif
end subroutine endif
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
end module M_logic
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
