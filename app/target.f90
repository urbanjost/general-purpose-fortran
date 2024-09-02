program target
use M_drawplus, only : draw_interpret
use M_kracken, only  : kracken, lget
character(len=:),allocatable ::  f50_instinct_pistol(:)
character(len=:),allocatable ::  f50_slow_fire_pistol(:)
character(len=:),allocatable ::  f50_small_bore_rifle(:)
character(len=:),allocatable ::  f50_timed_and_rapid_fire_pistol(:)
call kracken('target','-help .false. -version .false. -repeat -1 ') ! define command arguments,default values and crack command line
call help_usage(lget('target_help'))                                ! if -help option is present, display help text and exit
call help_version(lget('target_version'))                           ! if -version option is present, display version text and exit
f50_instinct_pistol=[ CHARACTER(LEN=128) :: &
'voutput f50_instinct_pistol.pdf',&
'set FACTOR=2.4;prefsize 3600*FACTOR 3150*FACTOR',&
'prefposition 0 0                               ',&
'vinit pdf                                      ',&
'page -6 6 -5.25 5.25                           ',&
'set R1=.9/2 R2=1.8/2 R3=3.0/2 R4=4.45/2 R5=6.1/2 R6=8.3/2 R7=10/2',&
'font times.r                                                     ',&
'set WHITE=7 BLACK=0                                              ',&
'move2 -6 -5.25                                                   ',&
'draw2  6 -5.25                                                   ',&
'draw2  6  5.25                                                   ',&
'draw2 -6  5.25                                                   ',&
'draw2 -6 -5.25                                                   ',&
'rotate 90 "z"                                                    ',&
'color WHITE                                                      ',&
'circleprecision 300                                              ',&
'polyfill .true.;linewidth 10                                     ',&
'makepoly ; circle 0 0 R7; closepoly                              ',&
'color BLACK                                                      ',&
'makepoly; circle 0 0 R1; closepoly                               ',&
'centertext .true.                                                ',&
'set K=0.125/.65 ;textsize K K*5/7                                ',&
'color WHITE                                                      ',&
'move2 0 0;drawstr "X"                                            ',&
'set K=0.150/.65 ;textsize K K*5/7;color WHITE                    ',&
'move2 0 5.25 ; drawstr "50 FT. INSTINCT PISTOL TARGET"           ',&
'vflush                                                           ',&
'vexit                                                            ',&
'']
f50_slow_fire_pistol=[ CHARACTER(LEN=128) :: &
'voutput f50_slow_fire_pistol.pdf',&
'set FACTOR=2.4;prefsize 3600*FACTOR 3150*FACTOR',&
'prefposition 0 0                               ',&
'vinit pdf                                      ',&
'page -6 6 -5.25 5.25                           ',&
'circleprecision 300                            ',&
'set R1=.9/2 R2=1.5/2 R3=2.2/2 R4=3.1/2 R5=4.15/2 R6=5.55/2 R7=7.3/2',&
'font times.r                                                       ',&
'set WHITE=7 BLACK=0                                                ',&
'move2 -6 -5.25                                                     ',&
'draw2  6 -5.25                                                     ',&
'draw2  6  5.25                                                     ',&
'draw2 -6  5.25                                                     ',&
'draw2 -6 -5.25                                                     ',&
'rotate 90 "z"                                                      ',&
'polyfill .true.;linewidth 10;color WHITE                           ',&
'#makepoly; circle 0 0 R4; closepoly                                ',&
'# note: circles are polygons in vogle                              ',&
'circle 0 0 R4                                                      ',&
'polyfill .false.;                                                  ',&
'color BLACK                                                        ',&
'circle 0 0 R1                                                      ',&
'circle 0 0 R2                                                      ',&
'circle 0 0 R3                                                      ',&
'color WHITE                                                        ',&
'circle 0 0 R5                                                      ',&
'circle 0 0 R6                                                      ',&
'circle 0 0 R7                                                      ',&
'centertext .true.                                                  ',&
'set K=0.125/.65 ;textsize K K*5/7                                  ',&
'color BLACK                                                        ',&
'move2 0  0               ;drawstr 10                               ',&
'move2   -(R1+R2)/2  0    ;drawstr 9                                ',&
'move2   -(R2+R3)/2  0    ;drawstr 8                                ',&
'move2   -(R3+R4)/2  0    ;drawstr 7                                ',&
'color WHITE                                                        ',&
'move2   -(R4+R5)/2  0    ;drawstr 6                                ',&
'move2   -(R5+R6)/2  0    ;drawstr 5                                ',&
'move2   -(R6+R7)/2  0    ;drawstr 4                                ',&
'set K=0.150/.65 ;textsize K K*5/7                                  ',&
'move2 0 5.25;drawstr 50 FT. SLOW FIRE PISTOL TARGET                ',&
'vflush                                                             ',&
'vexit                                                              ',&
'']
f50_small_bore_rifle=[ CHARACTER(LEN=128) :: &
'voutput f50_small_bore_rifle.pdf',&
'set FACTOR=2.4;prefsize 3600*FACTOR 3150*FACTOR',&
'prefposition 0 0                               ',&
'vinit pdf                                      ',&
'page 0 12 0 10.5 # SETS DIMENSIONS TO X,Y=(12,10.5)',&
'set R1=.14/2 R2=.48/2 R3=.80/2 R4=1.13/2 R5=1.50/2 R6=(1+13/16)/2 R7=(3+1/16)/2',&
'set WHITE=7 BLACK=0 RED=1                                                      ',&
'color RED                                                                      ',&
'linewidth 15                                                                   ',&
'rect 0 0 12 10.5 # outline drawing surface                                     ',&
'circleprecision 300                                                            ',&
'font times.r                                                                   ',&
'set WHITE=7 BLACK=0                                                            ',&
'# BULLSEYE                                                                     ',&
'genobj BULLSEYE;makeobj BULLSEYE                                               ',&
'polyfill .true.                                                                ',&
'linewidth 15;color WHITE                                                       ',&
'makepoly; circle 0 0 R5; closepoly                                             ',&
'polyfill .false.                                                               ',&
'color BLACK; circle 0 0 R1; circle 0 0 R2; circle 0 0 R3; circle 0 0 R4        ',&
'color WHITE; circle 0 0 R5; circle 0 0 R6                                      ',&
'closeobj BULLSEYE                                                              ',&
'#                                                                              ',&
'genobj SHEET;makeobj SHEET                                                     ',&
'set XD=3.5 YD=(7+13.5/16)/3 X2=.85 Y2=.75                                      ',&
'set K=0.1/.65 ;textsize K K*5/7                                                ',&
'invokeobj X1=1.75+0*XD Y1=2.15+0*YD 0 1 1 1 0 0 0 BULLSEYE;move2 X1+X2 Y1-Y2;drawstr 8',&
'invokeobj X1=1.75+1*XD Y1=2.15+0*YD 0 1 1 1 0 0 0 BULLSEYE;move2 X1+X2 Y1-Y2;drawstr 9',&
'invokeobj X1=1.75+2*XD Y1=2.15+0*YD 0 1 1 1 0 0 0 BULLSEYE;move2 X1+X2 Y1-Y2;drawstr 10',&
'invokeobj X1=1.75+0*XD Y1=2.15+1*YD 0 1 1 1 0 0 0 BULLSEYE;move2 X1+X2 Y1-Y2;drawstr 6 ',&
'invokeobj X1=1.75+2*XD Y1=2.15+1*YD 0 1 1 1 0 0 0 BULLSEYE;move2 X1+X2 Y1-Y2;drawstr 7 ',&
'invokeobj X1=1.75+0*XD Y1=2.15+2*YD 0 1 1 1 0 0 0 BULLSEYE;move2 X1+X2 Y1-Y2;drawstr 4 ',&
'invokeobj X1=1.75+2*XD Y1=2.15+2*YD 0 1 1 1 0 0 0 BULLSEYE;move2 X1+X2 Y1-Y2;drawstr 5 ',&
'invokeobj X1=1.75+0*XD Y1=2.15+3*YD 0 1 1 1 0 0 0 BULLSEYE;move2 X1+X2 Y1-Y2;drawstr 1 ',&
'invokeobj X1=1.75+1*XD Y1=2.15+3*YD 0 1 1 1 0 0 0 BULLSEYE;move2 X1+X2 Y1-Y2;drawstr 2 ',&
'invokeobj X1=1.75+2*XD Y1=2.15+3*YD 0 1 1 1 0 0 0 BULLSEYE;move2 X1+X2 Y1-Y2;drawstr 3 ',&
'invokeobj X1=1.75+XD Y1=2.15+1.5*YD 0 1 1 1 0 0 0 BULLSEYE                             ',&
'circle X1 Y1 R7;move2 X1+X2 Y1-Y2 ;drawstr SS;move2 X1-X2 Y1-Y2;drawstr SS             ',&
'#                                                                                      ',&
'color BLACK;set X=X1     Y=Y1                                                          ',&
'centertext .true.                                                                      ',&
'move2            0+X    Y    ;drawstr X                                                ',&
'move2   -(R1+R2)/2+X    Y    ;drawstr 9                                                ',&
'move2   -(R2+R3)/2+X    Y    ;drawstr 8                                                ',&
'move2   -(R3+R4)/2+X    Y    ;drawstr 7                                                ',&
'move2   -(R4+R5)/2+X    Y    ;drawstr 6                                                ',&
'color WHITE                                                                            ',&
'move2   -(R5+R6)/2+X    Y    ;drawstr 5                                                ',&
'set K=0.150/.65                                                                        ',&
'textsize K K*5/7                                                                       ',&
'move2 5.25 0+2*K  ;drawstr 50 FT. SMALL BORE RIFLE TARGET                              ',&
'closeobj SHEET                                                                         ',&
'invokeobj 12 0 0 1 1 1 0 0 90 SHEET                                                    ',&
'vflush                                                                                 ',&
'vexit                                                                                  ',&
'']
f50_timed_and_rapid_fire_pistol=[ CHARACTER(LEN=128) :: &
'voutput f50_timed_and_rapid_fire_pistol.pdf',&
'set FACTOR=2.4;prefsize 3600*FACTOR 3150*FACTOR',&
'prefposition 0 0                               ',&
'vinit pdf                                      ',&
'circleprecision 300                            ',&
'page -6 6 -5.25 5.25                           ',&
'set R1=.9/2 R2=1.8/2 R3=3.0/2 R4=4.45/2 R5=6.1/2 R6=8.3/2',&
'set WHITE=7 BLACK=0 RED=7                                ',&
'move2 -6 -5.25                                           ',&
'draw2  6 -5.25                                           ',&
'draw2  6  5.25                                           ',&
'draw2 -6  5.25                                           ',&
'draw2 -6 -5.25                                           ',&
'rotate 90 "z"                                            ',&
'polyfill .true.                                          ',&
'font times.r;linewidth 10;                               ',&
'color RED                                                ',&
'makepoly; circle 0 0 R3; closepoly                       ',&
'polyfill .false.                                         ',&
'color BLACK                                              ',&
'circle 0 0 R1                                            ',&
'circle 0 0 R2                                            ',&
'color WHITE                                              ',&
'circle 0 0 R4                                            ',&
'circle 0 0 R5                                            ',&
'circle 0 0 R6                                            ',&
'centertext .true.                                        ',&
'set K=0.125/.65 ;textsize K K*5/7                        ',&
'color BLACK                                              ',&
'move2            0  0;drawstr X                          ',&
'move2   -(R1+R2)/2  0;drawstr 10                         ',&
'move2   -(R2+R3)/2  0;drawstr 9                          ',&
'color WHITE                                              ',&
'move2   -(R3+R4)/2  0;drawstr 8                          ',&
'move2   -(R4+R5)/2  0;drawstr 7                          ',&
'move2   -(R5+R6)/2  0;drawstr 6                          ',&
'set K=0.150/.65 ;textsize K K*5/7                        ',&
'move2 0 5.25;drawstr 50 FT. TIMED AND RAPID FIRE PISTOL TARGET',&
'vflush                                                        ',&
'vexit                                                         ',&
'']
call draw_interpret(f50_instinct_pistol,delimiters=';')
call draw_interpret(f50_slow_fire_pistol,delimiters=';')
call draw_interpret(f50_small_bore_rifle,delimiters=';')
call draw_interpret(f50_timed_and_rapid_fire_pistol,delimiters=';')
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
'NAME                                                                            ',&
'   target(1f) - [M_drawplus] draw targets on Adobe PDF files                    ',&
'   (LICENSE:PD)                                                                 ',&
'SYNOPSIS                                                                        ',&
'   target [ --help| --version]                                                  ',&
'DESCRIPTION                                                                     ',&
'   Use the M_draw(3fm) and M_drawplus(3fm) modules to create various            ',&
'   targets. Requires large paper (at least 12x10.5 inches) to be printed        ',&
'   to scale.                                                                    ',&
'OPTIONS                                                                         ',&
'   --help     display this help and exit                                        ',&
'   --version  output version information and exit                               ',&
'                                                                                ',&
'EXAMPLE                                                                         ',&
'  Sample usage:                                                                 ',&
'                                                                                ',&
'   target                                                                       ',&
'   ls *.pdf                                                                     ',&
'   f50_instinct_pistol.pdf                                                      ',&
'   f50_slow_fire_pistol.pdf                                                     ',&
'   f50_small_bore_rifle.pdf                                                     ',&
'   f50_timed_and_rapid_fire_pistol.pdf                                          ',&
'AUTHOR                                                                          ',&
'   John S. Urban                                                                ',&
'LICENSE                                                                         ',&
'   Public Domain                                                                ',&
'']
   WRITE(*,'(a)')(trim(help_text(i)),i=1,size(help_text))
   stop ! if --help was specified, stop
endif
end subroutine help_usage
!>
!!##NAME
!!    target(1f) - [M_drawplus] draw targets on Adobe PDF files
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    target [ --help| --version]
!!##DESCRIPTION
!!    Use the M_draw(3fm) and M_drawplus(3fm) modules to create various
!!    targets. Requires large paper (at least 12x10.5 inches) to be printed
!!    to scale.
!!##OPTIONS
!!    --help     display this help and exit
!!    --version  output version information and exit
!!
!!##EXAMPLE
!!
!!   Sample usage:
!!
!!    target
!!    ls *.pdf
!!    f50_instinct_pistol.pdf
!!    f50_slow_fire_pistol.pdf
!!    f50_small_bore_rifle.pdf
!!    f50_timed_and_rapid_fire_pistol.pdf
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
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
'@(#)PROGRAM:        target(1)>',&
'@(#)DESCRIPTION:    create printable targets>',&
'@(#)VERSION:        1.0, 20190108>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)COMPILED:       2024-06-29 21:59:20 UTC-240>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if --version was specified, stop
endif
end subroutine help_version
end program target
