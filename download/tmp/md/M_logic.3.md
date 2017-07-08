[UP]

-----------------------------------------------------------------------------------------------------------------------------------
                                               Manual Reference Pages  - M_logic (3)
-----------------------------------------------------------------------------------------------------------------------------------
                                                                 
NAME

    cond(3f) - [M_logic]process input files with embedded if/else/elseif/endif commands

CONTENTS

    Synopsis
    Options
    Description
    Examples

SYNOPSIS



    subroutine cond(verb,line,ierr)


         character(len=*),intent(in) :: verb
         character(len=*),intent(in) :: line
         integer,intent(out)         :: ierr



    Public module variables include

         WRITE -- flag whether next input line should be read



OPTIONS

        verb must be  if , elseif , else , endif 

        line calculator expression to reduce to an integer.

        ierr indicate if an error occurred

DESCRIPTION

    Many programs read input files where input lines are conditionally skipped over. It may make sense that the data file itself
    contains the conditionals (think of pre-processors like cpp(1), shell programs (ie. interpreted languages), and configuration
    files that are read from multiple programs and from different environments). Instead of having many code-specific ways of
    specifying which lines will be processed, this module allows you to embed if/else/elseif/endif directives in the input files.

    If you pass the conditional lines to cond(3f) it sets the logical variable "WRITE" to indicate when non-conditional lines
    (blocks of lines between those logical conditions) should be skipped.

    if ierr=0, no obvious error occurred. if ierr not equal=0, suggest stopping the calling program from processing input

    The variable NEST_LEVEL should be zero at the end, or a nesting error occurred.

EXAMPLES

    The example program uses cond(3f) and the calculator module to let you have input files that the program or the input file (via
    the #define command) can set values for. Note that expressions on the #if and #elseif input lines are truncated to whole
    numbers. You need to see the calculator documentation to see all the expressions available (date and time functions, most
    FORTRAN 77 intrinsic functions, ...)

    Here is an example program that will read in a file and write just lines from the file to output that are in true blocks...

       program logic
       use M_journal, only : journal  ! for logging messages
       use M_strings, only : lower, delim,v2s ! convert character case; split string
       use M_logic, only : cond
       use M_logic, only : write ! flag whether current data lines should be written
       use M_logic, only : nest_level   ! nesting level for #IF/#ELSEIF/#ELSE/#ENDIF
       use M_calculator_plus, only : rnum0
       character(len=1)    :: prefix              ! directive prefix character
       character(len=1024) :: line                ! input line
       integer,parameter   :: max_words=2  ! maximum number of words allowed on line
       character(len=1024) :: array(max_words)    ! working copy of input line
       ! location where words start and end
       integer             :: ibegin(max_words), iterm(max_words)
       !----------------------------------------------------------------------------
       PREFIX= #               ! for the example, assume direct lines use a # prefix
       !----------------------------------------------------------------------------
       READLINE: do                                   ! read loop to read input file
          read(*, (a) ,iostat=ios) line
          if(ios.ne.0)then
             if (nest_level.ne.0) then ! check to make sure all if blocks are closed
                call journal( sc ,&
                & *logic* error - #IF BLOCK NOT CLOSED WHEN READiNG FILE FINISHED. )
             endif
             stop
          endif
          ! although delim(3f) can do more
          ! just parsing the first word out and finding where second word starts
          ! make sure array is initialized for when
          ! icount(number of words on line) is zero
          array=   
          call delim(lower(line),array,max_words,icount,ibegin,iterm,ilen,   )
          select case(array(1))
          ! find conditional lines
          case( #if , #else , #elseif , #endif )
             ! process conditional directive
             call cond(trim(array(1)(2:)),line(iterm(1)+1:),ierr_logic)
          case( #define )
             ! evaluate expression
             value=rnum0(line(iterm(1)+1:))
          case default
             ! find input lines you want to use, skip others
             if (write) then
                ! for example, if last conditional was true then write line
                write(*, (a) ) trim(line)
                ! write data line
             endif
          end select
       enddo READLINE
       end program logic



    Here is an input file for the test program

       >#define A=10
       >#define B=1234.0
       >#define C=sin(30)
       >DEFINED SOME VALUES AND READY TO START
       >#if eq(A,20)
       >   SHOULD NOT BE OUTPUT
       >#elseif eq(A,10)
       >  CORRECT BRANCH WRITE THIS
       >   #if gt(B,A)
       >      CORRECT AGAIN
       >   #else
       >      SHOULD NOT BE IN OUTPUT EITHER
       >   #endif
       >#else
       >   SHOULD NOT BE OUTPUT ONCE AGAIN
       >#endif
       >GOT TO END



    The output should be

       >DEFINED SOME VALUES AND READY TO START
       >  CORRECT BRANCH WRITE THIS
       >      CORRECT AGAIN
       >GOT TO END



-----------------------------------------------------------------------------------------------------------------------------------

                                                            M_logic (3)                                               July 02, 2017

Generated by manServer 1.08 from f363775d-0336-4d19-a2ea-ffd5514bde57 using man macros.
                                                             [M_logic]
