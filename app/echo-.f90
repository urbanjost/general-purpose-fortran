program echo
use M_kracken, only       : kracken, sget, lget, iget
use M_strings, only       : expand
use iso_fortran_env, only : ERROR_UNIT,OUTPUT_UNIT        ! access computing environment
implicit none
character(len=:),allocatable :: line
character(len=:),allocatable :: adv
integer                      :: ios
integer                      :: iout
integer                      :: irepeats
integer                      :: i
character(len=1)             :: escape
   call kracken('echo',' -n .f. -ne .f. -E .f. -help .f. -version .f. -r 1 -x %')
   call help_usage(lget('echo_help'))
   call help_version(lget('echo_version'))
!-----------------------------------------------------------------------------------------------------------------------------------
! NEXT TIME:
! sleep between repeats, like an extended yes(1)
! execute options as a command instead of printing, with substitution like xargs(1)
! call now(3f) to allow date and time information like now(1f) or date(1)
! allow numeric formatting or a format statement like printf(1)
!-----------------------------------------------------------------------------------------------------------------------------------
   if(lget('echo_n'))then    ! use advancing or non-advancing I/O
      adv='no'
   else
      adv='yes'
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   if(lget('echo_E'))then    ! use stderr for output
      IOUT=ERROR_UNIT
   else
      IOUT=OUTPUT_UNIT
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   escape=sget('echo_x')
!-----------------------------------------------------------------------------------------------------------------------------------
   if(sget('echo_r').ne. '')then
      irepeats=iget('echo_r')   ! how many times to repeat output
   else
      irepeats=-1
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   line=sget('echo_oo')
   if(.not.lget('echo_ne'))then
      line=expand(line,escape)
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   if (irepeats.le.0)then
      INFINITE: do
         write(IOUT,'(a)',advance=ADV,iostat=ios)trim(line)
         if(ios.ne.0)exit INFINITE
      enddo INFINITE
   else
      COUNTED: do i=1,irepeats
         write(IOUT,'(a)',advance=ADV,iostat=ios)trim(line)
         if(ios.ne.0)exit COUNTED
      enddo COUNTED
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
contains
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
'NAME                                                                                                                            ',&
'         echo-(1f) - [FUNIX] display a line of text                                                                             ',&
'         (LICENSE:PD)                                                                                                           ',&
'SYNOPSIS                                                                                                                        ',&
'         echo- [OPTION]... [STRING]...                                                                                          ',&
'DESCRIPTION                                                                                                                     ',&
'         Echo the STRING(s) to standard output.                                                                                 ',&
'OPTIONS                                                                                                                         ',&
'         -n      do not output the trailing newline                                                                             ',&
'         -E      write message to stderr instead of stdout                                                                      ',&
'         -ne     disable interpretation of escape escapes                                                                       ',&
'         -r [n]  output a string repeatedly until killed if n is blank or                                                       ',&
'                 n <= 0, else repeat output "n" times. Default value is "1".                                                    ',&
'         -x      escape character. Default is %                                                                                 ',&
'         --help  display this help and exit                                                                                     ',&
'         --version                                                                                                              ',&
'                output version information and exit                                                                             ',&
'                                                                                                                                ',&
'         Escape sequences                                                                                                       ',&
'                                                                                                                                ',&
'            %%     escape character           %a     alert (BEL)                                                                ',&
'            %b     backspace                  %c     suppress further output                                                    ',&
'            %e     escape                     %E     escape                                                                     ',&
'            %f     form feed                  %n     new line                                                                   ',&
'            %r     carriage return            %t     horizontal tab                                                             ',&
'            %v     vertical tab                                                                                                 ',&
'            %oNNN  byte with octal value NNN (1 to 3 digits)                                                                    ',&
'            %dNNN  byte with decimal value NNN (1 to 3 digits)                                                                  ',&
'            %xHH   byte with hexadecimal value HH (1 to 2 digits)                                                               ',&
'EXAMPLES                                                                                                                        ',&
'                                                                                                                                ',&
'    Example invocations:                                                                                                        ',&
'                                                                                                                                ',&
'         echo- Echo this text to stderr -E                                                                                      ',&
'         echo- y -r             # repeat "y" until interrupted                                                                  ',&
'         echo- ''''"#"'''' -r 80 -n # create 80-character break line                                                            ',&
'         echo- %e[2J   # clear screen on vt102-compatible terminal or emulator                                                  ',&
'']
   WRITE(*,'(a)')(trim(help_text(i)),i=1,size(help_text))
   stop ! if --help was specified, stop
endif
end subroutine help_usage
!>
!!##NAME
!!          echo-(1f) - [FUNIX] display a line of text
!!          (LICENSE:PD)
!!##SYNOPSIS
!!
!!          echo- [OPTION]... [STRING]...
!!##DESCRIPTION
!!          Echo the STRING(s) to standard output.
!!##OPTIONS
!!          -n      do not output the trailing newline
!!          -E      write message to stderr instead of stdout
!!          -ne     disable interpretation of escape escapes
!!          -r [n]  output a string repeatedly until killed if n is blank or
!!                  n <= 0, else repeat output "n" times. Default value is "1".
!!          -x      escape character. Default is %
!!          --help  display this help and exit
!!          --version
!!                 output version information and exit
!!
!!          Escape sequences
!!
!!             %%     escape character           %a     alert (BEL)
!!             %b     backspace                  %c     suppress further output
!!             %e     escape                     %E     escape
!!             %f     form feed                  %n     new line
!!             %r     carriage return            %t     horizontal tab
!!             %v     vertical tab
!!             %oNNN  byte with octal value NNN (1 to 3 digits)
!!             %dNNN  byte with decimal value NNN (1 to 3 digits)
!!             %xHH   byte with hexadecimal value HH (1 to 2 digits)
!!##EXAMPLES
!!
!!
!!     Example invocations:
!!
!!          echo- Echo this text to stderr -E
!!          echo- y -r             # repeat "y" until interrupted
!!          echo- ''"#"'' -r 80 -n # create 80-character break line
!!          echo- %e[2J   # clear screen on vt102-compatible terminal or emulator
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
'@(#)PRODUCT:        GPF (General Purpose Fortran) utilities and examples>',&
'@(#)PROGRAM:        echo-(1)>',&
'@(#)DESCRIPTION:    display a line of text>',&
'@(#)VERSION:        1.0.0, 2016-01-17>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)REPORTING BUGS: http://www.urbanjost.altervista.org/>',&
'@(#)HOME PAGE:      http://www.urbanjost.altervista.org/index.html>',&
'@(#)LICENSE:        Public Domain. This is free software: you are free to change and redistribute it.>',&
'@(#)                There is NO WARRANTY, to the extent permitted by law.>',&
'@(#)COMPILED:       2022-12-21 19:28:38 UTC-300>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if --version was specified, stop
endif
end subroutine help_version
end program echo
!-----------------------------------------------------------------------------------------------------------------------------------
