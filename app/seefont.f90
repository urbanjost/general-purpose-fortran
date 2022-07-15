!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
program demo_seefont
use M_draw
use M_drawplus, only : seefont
use M_kracken,  only : kracken, lget, sget, iget             ! add command-line parser module
implicit none

!@(#) seefont(1) - display sample page of a font

character(len=128) :: fontname
integer            :: iwidth
integer            :: ix, iy
integer            :: ikey
   call kracken('seefont','-w 20 -list --version F --help F -x 600 -y 600') ! define command arguments and crack command line
   call help_usage(lget('seefont_help'))                              ! if -help option is present, display help text and exit
   call help_version(lget('seefont_version'))                         ! if -version option is present, display version text and exit
   fontname=sget('seefont_oo')
   iwidth=iget('seefont_w')
   ix=iget('seefont_x')
   iy=iget('seefont_y')
   call prefsize(ix,iy)
   call vinit(' ')
   call linewidth(iwidth)
   call seefont(fontname)
   ikey=getkey()
   call vexit()
contains
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
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
'   seefont(1) - [M_drawplus] show sample of font                                                                                ',&
'   (LICENSE:PD)                                                                                                                 ',&
'SYNOPSIS                                                                                                                        ',&
'   seefont [fontname[ -x window_width][ -y window_height][ -w linewidth]|[ --help| --version]                                   ',&
'DESCRIPTION                                                                                                                     ',&
'   seefont(1) displays fonts                                                                                                    ',&
'OPTIONS                                                                                                                         ',&
'    fontname   fontname to display. Defaults to interactive mode, where "n" means "next",                                       ',&
'               "p" for "previous", and "q" for quit.                                                                            ',&
'    -x         display surface width in device units (typically rasters)                                                        ',&
'    -y         display surface height in device units (typically rasters)                                                       ',&
'    -w         linewidth (default is 20) in terms of 1/10000 of display width                                                   ',&
'    --help     display this help and exit                                                                                       ',&
'    --version  output version information and exit                                                                              ',&
'EXAMPLES                                                                                                                        ',&
'  Typical usage:                                                                                                                ',&
'                                                                                                                                ',&
'    # use current output device (typically X11 Windows)                                                                         ',&
'    seefont                                                                                                                     ',&
'                                                                                                                                ',&
'    # display a specific font                                                                                                   ',&
'    seefont cursive                                                                                                             ',&
'                                                                                                                                ',&
'    # make an Adobe PDF file showing samples of each font                                                                       ',&
'    env M_DRAW_DEVICE=pdf seefont -x 3000 -y 3000 >seefont.pdf                                                                  ',&
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
!!    seefont(1) - [M_drawplus] show sample of font
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    seefont [fontname[ -x window_width][ -y window_height][ -w linewidth]|[ --help| --version]
!!##DESCRIPTION
!!    seefont(1) displays fonts
!!##OPTIONS
!!     fontname   fontname to display. Defaults to interactive mode, where "n" means "next",
!!                "p" for "previous", and "q" for quit.
!!     -x         display surface width in device units (typically rasters)
!!     -y         display surface height in device units (typically rasters)
!!     -w         linewidth (default is 20) in terms of 1/10000 of display width
!!     --help     display this help and exit
!!     --version  output version information and exit
!!##EXAMPLES
!!
!!   Typical usage:
!!
!!     # use current output device (typically X11 Windows)
!!     seefont
!!
!!     # display a specific font
!!     seefont cursive
!!
!!     # make an Adobe PDF file showing samples of each font
!!     env M_DRAW_DEVICE=pdf seefont -x 3000 -y 3000 >seefont.pdf
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
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
'@(#)PROGRAM:        seefont(1)>',&
'@(#)DESCRIPTION:    display font samples>',&
'@(#)VERSION:        1.0, 20181109>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)HOME PAGE:      http://www.urbanjost.altervista.org/index.html>',&
'@(#)COMPILED:       2022-07-15 10:01:12 UTC-240>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if --version was specified, stop
endif
end subroutine help_version
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
end program demo_seefont
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
