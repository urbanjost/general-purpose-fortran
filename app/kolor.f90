program prg_color
use,intrinsic :: iso_fortran_env, only : stderr=>ERROR_UNIT, stdin=>INPUT_UNIT, stdout=>OUTPUT_UNIT
use M_CLI2,                       only : set_args, sget, colors=>unnamed ! add command-line parser module
implicit none
character(len=:),allocatable :: help_text(:), version_text(:), background, foreground, cursorcolor
integer                      :: sz, ios
   call setup()
   call set_args('-bg " " -fg " " -cr " "',help_text,version_text)
   background = sget('bg')
   foreground = sget('fg')
   cursorcolor = sget('cr')
   sz = size(colors)
   if( sz.gt.0 ) background=trim(colors(1) )
   if( background.ne.'' ) write(*,'(g0)',advance='no',iostat=ios)achar(27)//']11;'//background//achar(7)
   if( sz.gt.1 ) foreground=trim(colors(2) )
   if( foreground.ne.'' ) write(*,'(g0)',advance='no',iostat=ios)achar(27)//']10;'//foreground//achar(7)
   if( sz.gt.2 ) cursorcolor=trim(colors(3) )
   if( cursorcolor.ne.'' ) write(*,'(g0)',advance='no',iostat=ios)achar(27)//']12;'//cursorcolor//achar(7)
contains
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine setup()
help_text=[ CHARACTER(LEN=128) :: &
'NAME                                                                            ',&
'   kolor(1) - [TERMINAL] set terminal background, foreground, and cursor color  ',&
'   (LICENSE:PD)                                                                 ',&
'                                                                                ',&
'SYNOPSIS                                                                        ',&
'    color [background foreground cursorcolor] [-bg background][-fg foreground][-cr cursorcolor]',&
'                                                                                ',&
'DESCRIPTION                                                                     ',&
'   Set the background, foreground, and cursor color for terminals and           ',&
'   terminal emulators that obey the ANSI in-band signaling control              ',&
'   sequences                                                                    ',&
'                                                                                ',&
'   If you use something that filters stdout such as tmux(1) or screen(1)        ',&
'   assign kolor(1) output to the initial stdout before starting the             ',&
'   program. For example:                                                        ',&
'                                                                                ',&
'      export OTHERTTY=`tty`                                                     ',&
'      tmux                                                                      ',&
'                                                                                ',&
'   Color values may be specified by name or hex value "#RRGGBB" or using        ',&
'   RGB syntax, eg. rgb:0000/0000/0000 to rgb:ffff/ffff/ffff.                    ',&
'                                                                                ',&
'OPTIONS                                                                         ',&
'   -bg CNAME  specify background color                                          ',&
'   -fg CNAME  specify foreground color                                          ',&
'   -cr CNAME  specify cursor color                                              ',&
'   --help     display help and exit                                             ',&
'   --version  display version information and exit                              ',&
'                                                                                ',&
'VARIABLES                                                                       ',&
'   By default kolor(1) writes output to the current stdout file. The            ',&
'   environment variable OTHERTTY can be used to change the default              ',&
'   file. This is commonly required before starting programs that                ',&
'   filter stdout, such as tmux(1) and screen(1).                                ',&
'                                                                                ',&
'      export OTHERTTY=/dev/pts/1                                                ',&
'                                                                                ',&
'EXAMPLE                                                                         ',&
'  Sample usage:                                                                 ',&
'                                                                                ',&
'   kolor gray30 white yellow                                                    ',&
'   kolor -bg brown -fg white -cr red                                            ',&
'   kolor -bg ''#ff00ff''                                                        ',&
'   kolor rgb:0000/8080/0000 rgb:ffff/ffff/ffff rgb:ffff/0000/0000               ',&
'                                                                                ',&
'AUTHOR                                                                          ',&
'   John S. Urban                                                                ',&
'LICENSE                                                                         ',&
'   Public Domain                                                                ',&
'']
!>
!!##NAME
!!    kolor(1) - [TERMINAL] set terminal background, foreground, and cursor color
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!     color [background foreground cursorcolor] [-bg background][-fg foreground][-cr cursorcolor]
!!
!!##DESCRIPTION
!!    Set the background, foreground, and cursor color for terminals and
!!    terminal emulators that obey the ANSI in-band signaling control
!!    sequences
!!
!!    If you use something that filters stdout such as tmux(1) or screen(1)
!!    assign kolor(1) output to the initial stdout before starting the
!!    program. For example:
!!
!!       export OTHERTTY=`tty`
!!       tmux
!!
!!    Color values may be specified by name or hex value "#RRGGBB" or using
!!    RGB syntax, eg. rgb:0000/0000/0000 to rgb:ffff/ffff/ffff.
!!
!!##OPTIONS
!!    -bg CNAME  specify background color
!!    -fg CNAME  specify foreground color
!!    -cr CNAME  specify cursor color
!!    --help     display help and exit
!!    --version  display version information and exit
!!
!!##VARIABLES
!!    By default kolor(1) writes output to the current stdout file. The
!!    environment variable OTHERTTY can be used to change the default
!!    file. This is commonly required before starting programs that
!!    filter stdout, such as tmux(1) and screen(1).
!!
!!       export OTHERTTY=/dev/pts/1
!!
!!##EXAMPLE
!!
!!   Sample usage:
!!
!!    kolor gray30 white yellow
!!    kolor -bg brown -fg white -cr red
!!    kolor -bg '#ff00ff'
!!    kolor rgb:0000/8080/0000 rgb:ffff/ffff/ffff rgb:ffff/0000/0000
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain

! Do not use the terminators ",>,\ in the strings
version_text=[ CHARACTER(LEN=128) :: &
'@(#)PRODUCT:        GPF (General Purpose Fortran) utilities and examples>       ',&
'@(#)PROGRAM:        kolor(1)>                                                   ',&
'@(#)DESCRIPTION:    set terminal(1) window foreground, background, and cursor color>',&
'@(#)VERSION:        1.0, 20180408>                                              ',&
'@(#)AUTHOR:         John S. Urban>                                              ',&
'@(#)HOME PAGE:      http://www.urbanjost.altervista.org/index.html>             ',&
'']
end subroutine setup
!-----------------------------------------------------------------------------------------------------------------------------------
end program prg_color
!-----------------------------------------------------------------------------------------------------------------------------------
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>-
!-----------------------------------------------------------------------------------------------------------------------------------
