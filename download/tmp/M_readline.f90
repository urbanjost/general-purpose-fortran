!>
!!##NAME
!!      M_readline(3fm) - [M_readline]Calling readline(3c) from Fortran
!!##SYNOPSIS
!!
!!      Use M_readline, only : system_readline
!!##DESCRIPTION
!!
!!    The M_readline(3fm) module uses the ISO_C_BINDING module to create a
!!    binding to the GNU readline(3c) procedure from Fortran programs.
!!
!!##EXAMPLE
!!
!!
!!    The test program is basically just a read loop that prompts for
!!    lines of input read with readline(3c). You can edit the line being
!!    read with readline(3c) per it's documentation. At a minimum, you can
!!    probably move around the line with the left and right arrow keys, and
!!    insert characters by typing them whereever you moved the cursor to,
!!    and use the DEL/ RUBOUT key to delete characters and such. If you use
!!    a GNU/Linux shell with command line editing, you are probably familiar
!!    with readline(3c)'s function.
!!
!!    It quits if you enter 'q' on an input line, and it dumps the history if
!!    you enter 'h'.
!!
!!   the test program
!!
!!    program testit
!!       use m_readline
!!       implicit none
!!       character(len=256):: line
!!
!!       write(*,*)' ____________________________________________________________'
!!       write(*,*)'  Your input lines are now edittable using the GNU'
!!       write(*,*)'  readline(3C) procedure.  By default, up-arrow and'
!!       write(*,*)'  down-arrow go thru the history lines; left and right arrow'
!!       write(*,*)'  keys and delete and just typing characters let you do'
!!       write(*,*)'  simple editting. Far more input control is available.'
!!       write(*,*)'  See the browser pages and man(1) pages for readline(3c).'
!!       write(*,*)' ____________________________________________________________'
!!       write(*,*)' Enter text and then edit it. "q" quits; "h" display history:'
!!
!!       do
!!          call system_readline(line,'readline>') ! read editable input line
!!          if(line.eq.'q') stop
!!          !call system(line(:len_trim(line))) ! common extension
!!          !call execute_command_line(line(:len_trim(line))) ! f08 equivalent
!!       enddo
!!    end program testit
!===================================================================================================================================
MODULE M_readline
! @(#) Call readline(3c) from Fortran using ISO_C_BINDING
! assumes you have the GNU readline library libreadline.a available
   USE ISO_C_BINDING
   IMPLICIT NONE
   PRIVATE
   PUBLIC system_readline
!-------------------------------------------------------------------------------
! define the call to the C routine
! extern char     *Freadline(int ilen, char *buf, char prompt[]);
  PUBLIC ::  Freadline
   INTERFACE
      SUBROUTINE Freadline(ilen,buf,prompt) BIND(C,NAME='FCreadline')
         USE ISO_C_BINDING
         IMPLICIT NONE
         INTEGER(KIND=C_INT),INTENT(IN),VALUE  :: ilen
         CHARACTER(KIND=C_CHAR),intent(out)    :: buf(*)
         CHARACTER(KIND=C_CHAR),intent(in)     :: prompt(*)
      END SUBROUTINE Freadline
   END INTERFACE
!-------------------------------------------------------------------------------
contains
! the routine that calls the C routine
!>
!!##NAME
!!      system_readline(3f) - [M_readline]Call readline(3c) from Fortran
!!##SYNOPSIS
!!
!!     character(kind=c_char,len=*),intent(in)  :: prompt
!!     character(kind=c_char,len=*),intent(out) :: line
!!
!!##DESCRIPTION
!!
!!    The system_readline(3f) uses the ISO_C_BINDING module to create a
!!    binding to the GNU readline(3c) procedure from Fortran programs.
!!
!!##EXAMPLE
!!
!!
!!    The test program is basically just a read loop that prompts for
!!    lines of input read with readline(3c). You can edit the line being
!!    read with readline(3c) per it's documentation. At a minimum, you can
!!    probably move around the line with the left and right arrow keys, and
!!    insert characters by typing them whereever you moved the cursor to,
!!    and use the DEL/ RUBOUT key to delete characters and such. If you use
!!    a GNU/Linux shell with command line editing, you are probably familiar
!!    with readline(3c)'s function.
!!
!!    It quits if you enter 'q' on an input line, and it dumps the history if
!!    you enter 'h'.
!!
!!    It is presented here as a Bourne shell script that creates the neccessary
!!    files and does a "compile, load, and go"
!!
!!   the test program
!!
!!    program demo_system_readline
!!       use m_readline, only : system_readline
!!       implicit none
!!       character(len=256):: line
!!
!!       write(*,*)' ____________________________________________________________'
!!       write(*,*)'  Your input lines are now edittable using the GNU'
!!       write(*,*)'  readline(3C) procedure.  By default, up-arrow and'
!!       write(*,*)'  down-arrow go thru the history lines; left and right arrow'
!!       write(*,*)'  keys and delete and just typing characters let you do'
!!       write(*,*)'  simple editting. Far more input control is available.'
!!       write(*,*)'  See the browser pages and man(1) pages for readline(3c).'
!!       write(*,*)' ____________________________________________________________'
!!       write(*,*)' Enter text and then edit it. "q" quits; "h" display history:'
!!
!!       do
!!          call system_readline(line,'readline>') ! read editable input line
!!          if(line.eq.'q') stop
!!          call execute_command_line(line(:len_trim(line)))
!!       enddo
!!    end program demo_system_readline
!===================================================================================================================================
SUBROUTINE system_readline(line,prompt)
   USE ISO_C_BINDING
   IMPLICIT NONE
   CHARACTER(KIND=C_CHAR,LEN=*),INTENT(OUT) :: line
   CHARACTER(KIND=C_CHAR,LEN=*),INTENT(IN)  :: prompt

   ! trim to last non-blank character and append null for C
   CALL Freadline(INT(LEN(line),KIND=C_INT),line,prompt(:LEN_TRIM(prompt))//ACHAR(0))

 END SUBROUTINE system_readline
!-------------------------------------------------------------------------------
END MODULE M_readline
!-------------------------------------------------------------------------------
