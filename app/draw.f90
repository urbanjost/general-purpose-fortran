!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!(LICENSE:PD)
program draw
use M_drawplus, only : call_draw
use M_io, only : read_line
use M_kracken, only : kracken, lget, sget, iget                  ! add command-line parser module
use M_strings, only: split
implicit none
character(len=:),allocatable   :: line
character(len=:),allocatable   :: cmds(:) ! output array of tokens
logical                        :: found
integer                        :: iend
integer                        :: i
character(len=256)             :: delim
!-----------------------------------------------------------------------------------------------------------------------------------
   call kracken('draw','-help .false. -version .false. -delim ";"') ! define command arguments,default values and crack command line
   call help_usage(lget('draw_help'))                               ! if -help option is present, display help text and exit
   call help_version(lget('draw_version'))                          ! if -version option is present, display version text and exit
   delim=sget('draw_delim')
!-----------------------------------------------------------------------------------------------------------------------------------
   INFINITE: do while (read_line(line)==0)
      line=adjustl(line)
      !!write(*,*)'LINE=',i,trim(line)
      call split(line,cmds,delimiters=trim(delim))
      do i=1,size(cmds)
         cmds(i)=adjustl(cmds(i))
         iend=scan(cmds(i),' #')-1
         if(iend.le.0)iend=len_trim(cmds(i))
         if(iend.ne.0)then
            cmds(i)=trim(cmds(i))//' '
            if(cmds(i)(:1).eq.'#') cycle
            call call_draw(cmds(i)(:iend),cmds(i)(iend+2:),found)
            if(.not.found)then
               write(*,'(*(a))')'ERROR: ',trim(cmds(i)(:iend)),' [',trim(cmds(i)(iend+2:)),']',' not found'
            endif
         endif
      enddo
   enddo INFINITE
!-----------------------------------------------------------------------------------------------------------------------------------
contains
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
'@(#)PRODUCT:        GPF library utilities and examples>',&
'@(#)PROGRAM:        draw(1)>',&
'@(#)DESCRIPTION:    simple interpreter for M_draw(3fm) routines>',&
'@(#)VERSION:        1.0, 20180722>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)HOME PAGE:      http://www.urbanjost.altervista.org/index.html>',&
'@(#)COMPILED:       2022-01-09 10:18:01 UTC-300>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if --version was specified, stop
endif
end subroutine help_version
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
'    draw(1f) - [M_drawplus] basic interpreter for testing M_draw(3fm) routines                                                  ',&
'    (LICENSE:PD)                                                                                                                ',&
'                                                                                                                                ',&
'SYNOPSIS                                                                                                                        ',&
'    draw [delim DELIMITERS]|[ --help| --version]                                                                                ',&
'                                                                                                                                ',&
'DESCRIPTION                                                                                                                     ',&
'    draw(1) is a basic interpreter for M_draw(3f) routines. It is useful for                                                    ',&
'    making simple graphics and for testing the M_draw(3fm) module.                                                              ',&
'                                                                                                                                ',&
'OPTIONS                                                                                                                         ',&
'    -delim CHARS  delimiter character(s) used to separate commands.                                                             ',&
'                  Defaults to semi-colon (";").                                                                                 ',&
'    --help        display this help and exit                                                                                    ',&
'    --version     output version information and exit                                                                           ',&
'                                                                                                                                ',&
'EXAMPLES                                                                                                                        ',&
'    Sample commands                                                                                                             ',&
'                                                                                                                                ',&
'       draw  <<\eof                                                                                                             ',&
'       prefsize 300 300;prefposition 200 10;vinit X11;color 3;clear;color 2                                                     ',&
'       ortho2 -1 1 -1 1;                                                                                                        ',&
'       polyfill .true.;rect 0 0 98 98                                                                                           ',&
'       color 1;circle 0 0 .25                                                                                                   ',&
'       polyfill .false.;linewidth 200;color 4;circle 0 0 .2                                                                     ',&
'       # see calculator document for real expressions                                                                           ',&
'       set X=-0.25 Y=0.25                                                                                                       ',&
'       #                                                                                                                        ',&
'       polyfill .true.; color 5; circle X Y sqrt(Y)/2                                                                           ',&
'       polyfill .false.;color 5; circle X Y sqrt(2*Y)/2                                                                         ',&
'       #                                                                                                                        ',&
'       getkey;vexit                                                                                                             ',&
'       set dump                                                                                                                 ',&
'       eof                                                                                                                      ',&
'                                                                                                                                ',&
'SEE ALSO                                                                                                                        ',&
'    M_draw(3fm), M_drawplus(3fm)                                                                                                ',&
'AUTHOR                                                                                                                          ',&
'   John S. Urban                                                                                                                ',&
'LICENSE                                                                                                                         ',&
'   Public Domain                                                                                                                ',&
'']
   WRITE(*,'(a)')(trim(help_text(i)),i=1,size(help_text))
   stop ! if --help was specified, stop
endif
end subroutine help_usage
!>
!!##NAME
!!     draw(1f) - [M_drawplus] basic interpreter for testing M_draw(3fm) routines
!!     (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!     draw [delim DELIMITERS]|[ --help| --version]
!!
!!##DESCRIPTION
!!     draw(1) is a basic interpreter for M_draw(3f) routines. It is useful for
!!     making simple graphics and for testing the M_draw(3fm) module.
!!
!!##OPTIONS
!!     -delim CHARS  delimiter character(s) used to separate commands.
!!                   Defaults to semi-colon (";").
!!     --help        display this help and exit
!!     --version     output version information and exit
!!
!!##EXAMPLES
!!
!!     Sample commands
!!
!!        draw  <<\eof
!!        prefsize 300 300;prefposition 200 10;vinit X11;color 3;clear;color 2
!!        ortho2 -1 1 -1 1;
!!        polyfill .true.;rect 0 0 98 98
!!        color 1;circle 0 0 .25
!!        polyfill .false.;linewidth 200;color 4;circle 0 0 .2
!!        # see calculator document for real expressions
!!        set X=-0.25 Y=0.25
!!        #
!!        polyfill .true.; color 5; circle X Y sqrt(Y)/2
!!        polyfill .false.;color 5; circle X Y sqrt(2*Y)/2
!!        #
!!        getkey;vexit
!!        set dump
!!        eof
!!
!!##SEE ALSO
!!     M_draw(3fm), M_drawplus(3fm)
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
end program draw
