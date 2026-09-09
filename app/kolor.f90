program kolor_exe
use,intrinsic :: iso_fortran_env, only : stderr=>ERROR_UNIT, stdin=>INPUT_UNIT, stdout=>OUTPUT_UNIT
use M_CLI2,                       only : set_args, sget, colors=>unnamed ! add command-line parser module
use M_strings,                    only : lower
use M_io,                         only : get_env
use M_color,                      only : color_name2rgb
implicit none
character(len=:),allocatable :: help_text(:), version_text(:), background, foreground, cursorcolor, output, prompt
integer                      :: sz, ios, lun, c, itype
character(len=1),parameter   :: esc=achar(27), bel=achar(7)
character(len=*),parameter   :: gen='(*(g0))', genx='(*(g0,1x))'
   c=1
   itype=1
   call setup() ! set the help text and version text
   call set_args('-bg " " -fg " " -cr " "',help_text,version_text)

   output=get_env('OTHERTTY') ! explicitly set the output pathname for tmux(1), screen(1), ...
   if(output == '')then
      lun=stdout
   else
      open(file=output,iostat=ios,newunit=lun)
      if(ios /= 0)lun=stdout
   endif

   background = sget('bg')
   foreground = sget('fg')
   cursorcolor = sget('cr')
   sz = size(colors)
   if( sz.gt.0 ) background=trim(colors(1) )
   if( sz.gt.1 ) foreground=trim(colors(2) )
   if( sz.gt.2 ) cursorcolor=trim(colors(3) )
   if( background.ne.'' ) write(lun,gen,advance='no',iostat=ios)esc//']11;'//background//bel
   if( foreground.ne.'' ) write(lun,gen,advance='no',iostat=ios)esc//']10;'//foreground//bel
   if( cursorcolor.ne.'' ) write(lun,gen,advance='no',iostat=ios)esc//']12;'//cursorcolor//bel
   ! if no color was specified on the command line go into interactive mode
   if( background//foreground//cursorcolor == ' ') call listallnames()
contains
!-----------------------------------------------------------------------------------------------------------------------------------
function paws()
integer :: iostat
character(len=100) :: line
character(len=:),allocatable :: paws
   paws=' '
   prompt=''
   if(c==1)prompt='(enter "Next", "Previous", "Top", "Quit". Defaults to "Next")'
   write(*,'(1x,"| ",i0,1x,a)',advance='no')c,prompt
   read(*,'(a)',iostat=iostat)line
   if(iostat.eq.0)then
      paws=trim(line)//' '
   else
      paws=' '
   endif
end function paws
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine listallnames()
    ! list colors known to colorname2rgb(3f) & corresponding RGB values
    character(len=20) :: name
    character(len=20) :: echoname
    real              :: red,green,blue
    integer           :: r,g,b, iostat
    character(len=:),allocatable :: returned
    TRYALL: do
       ! color names may be numeric strings from 1 to N
       write(name,'(i0)')c
       ! get the RGB values and English name of the color
       call color_name2rgb(name,red,green,blue,echoname)
       r=nint(2.5*red)
       g=nint(2.5*green)
       b=nint(2.5*blue)
       ! the last color name is "Unknown" so the loop should exit or recover
       !if(echoname == 'Unknown')exit TRYALL
       if(echoname == 'Unknown')then
          write(*,gen)'End of color list. Restarting. Enter (q)uit to exit,'
          c=1
          cycle TRYALL
       endif
       ! display the English name and RGB values for the name
       ! printf "\x1b[38;2;40;177;249mTRUECOLOR\x1b[0m\n"
       ! write(*,gen,advance='no') esc, '[38;5;', r, ';', g, ';', b, 'm_________', esc, '[0m'
       write(lun,gen,advance='no',iostat=ios)esc//']',itype+10,';'//trim(echoname)//bel
       if(itype+10.eq.11)then
          ! set foreground color to contrast new background color
          write(lun,'(g0,g0,z0,z0,z0,g0)',advance='no',iostat=ios)esc,']10;#',255-r,255-g,255-b,bel
       endif
       write(*,'(a,1x,i3,1x,i3,1x,i3)',advance='no')echoname,int([red,green,blue])
         returned=lower(paws())//' '
         select case(returned(1:1))
         case('t')             ; write(*,'(a)')'back to top'    ;c=1 ! top
         case('b','p')         ; c=max(1,c-1)                        ! backward, previous
         case('f','n',' ')     ; c=c+1                               ! forward,  next
         case('q','e','s')     ; exit TRYALL                         ! quit
         case('>')             ; itype=modulo(itype+1,3)             ! shift bg,fg,cr
         case('<')             ; itype=modulo(itype-1,3)             ! shift bg,fg,cr
         case('0':'9')
             read(returned,'(i9)',iostat=iostat)c
             c=max(c,1) ! make sure at least 1
         case('h','?','i')
             write(*,gen)' '
             write(*,gen)'Enter command(default is "n"): '
             write(*,gen)'    (n)ext,(p)revious or (f)orward, (b)ackward.'
             write(*,gen)'    (t)op,NNN'
             write(*,gen)'    > and < shift between foreground, background, and cursor color'
             write(*,gen)'    (h)elp,?'
             write(*,gen)'    (q)uit'
             write(*,gen)'If number is past range program returns to top'
             write(*,gen)' '
         case default
             write(*,gen)'Unknown command ',returned,' . Enter "h" for help'
       end select
    enddo TRYALL
    !write(*,*)'Number of colors found is ',c-1
end subroutine listallnames
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine setup()
help_text=[ CHARACTER(LEN=128) :: &
'NAME',&
'    kolor(1) - [TERMINAL] set background, foreground, and cursor color',&
'    of most terminal emulators                                        ',&
'    (LICENSE:PD)                                                      ',&
'                                                                      ',&
'SYNOPSIS                                                              ',&
'                                                                      ',&
'     kolor [background foreground cursorcolor]|                       ',&
'           [-bg background][-fg foreground][-cr cursorcolor]          ',&
'           [--verbose]                                                ',&
'                                                                      ',&
'     kolor --help| --usage| --version                                 ',&
'                                                                      ',&
'DESCRIPTION                                                           ',&
'    Set the background, foreground, and cursor color for terminals and',&
'    terminal emulators that obey the ANSI in-band signaling control   ',&
'    sequences, such as xterm(1).                                      ',&
'                                                                      ',&
'    If no options are supplied, steps through known color names changing',&
'    the background kolor. Obeys commands such as "back", "top", "next", ',&
'    "quit". Defaults to "next". Enter "help" at the prompt for a full   ',&
'    list.                                                               ',&
'                                                                        ',&
'    If you use something that filters stdout such as tmux(1) or screen(1)',&
'    assign kolor(1) output to the initial stdout before starting the     ',&
'    program by setting the environment variable OTHERTTY to the output   ',&
'    device path. For example:                                            ',&
'                                                                         ',&
'       export OTHERTTY=`tty`                                             ',&
'       tmux                                                              ',&
'                                                                         ',&
'    Color values may be specified by name or hex value "#RRGGBB" or      ',&
'    using RGB syntax, eg. rgb:0000/0000/0000 to rgb:ffff/ffff/ffff.      ',&
'                                                                         ',&
'OPTIONS                                                                  ',&
'    -bg CNAME  specify background color                                  ',&
'    -fg CNAME  specify foreground color                                  ',&
'    -cr CNAME  specify cursor color                                      ',&
'    --verbose  display additional information                            ',&
'                                                                         ',&
'    --help     display help and exit                                     ',&
'    --version  display version information and exit                      ',&
'    --usage    display usage table                                       ',&
'                                                                         ',&
'VARIABLES                                                                ',&
'    By default kolor(1) writes output to the current stdout file. The    ',&
'    environment variable OTHERTTY can be used to change the default      ',&
'    file. This is commonly required before starting programs that        ',&
'    filter stdout, such as tmux(1) and screen(1).                        ',&
'                                                                         ',&
'       export OTHERTTY=$(realpath /proc/$$/fd/1)                         ',&
'                                                                         ',&
'EXAMPLE                                                                  ',&
'                                                                         ',&
'   Sample usage:                                                         ',&
'                                                                         ',&
'    kolor gray30 white yellow  # list of colors                          ',&
'    kolor -bg brown -fg white -cr red  # using keywords                  ',&
'    kolor -bg ''#ff00ff'' #RGB in hexadecimal                            ',&
'    kolor rgb:0000/8080/0000 rgb:ffff/ffff/ffff rgb:ffff/0000/0000       ',&
'    kolor # step through background colors                               ',&
'                                                                         ',&
'AUTHOR                                                                   ',&
'    John S. Urban                                                        ',&
'LICENSE                                                                  ',&
'    Public Domain                                                        ',&
'']

version_text=[ CHARACTER(LEN=128) :: &
'PRODUCT:        GPF (General Purpose Fortran) utilities and examples',&
'PROGRAM:        kolor(1)                                            ',&
'DESCRIPTION:    set terminal(1) window foreground, background, and cursor color',&
'VERSION:        1.0, 20180408                                                  ',&
'AUTHOR:         John S. Urban                                                  ',&
'HOME PAGE:      http://www.urbanjost.altervista.org/index.html                 ',&
'']
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine setup
end program kolor_exe
!-----------------------------------------------------------------------------------------------------------------------------------
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>-
!-----------------------------------------------------------------------------------------------------------------------------------
