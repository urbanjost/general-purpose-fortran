!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
program testCLI
!
! Exercise libjust4 and libscreen
!
!-----------------------------------------------------------------------------------------------------------------------------------
use :: M_journal,         only : journal

use :: M_calculator,      only : jucalc,iclen_calc
use :: M_calculator,      only : stuff    ! [M_calculator] directly store value into calculator directory for efficiency
use :: M_calculator_plus, only : strgar2

use :: M_history,         only : redo

use :: M_kracken,         only : kracken, IPvalue
use :: M_kracken,         only : dissect,sget,rget,lget,iget,parse
use :: M_kracken,         only : rgets,igets,sgets

use :: M_system, only : system_chdir

use :: M_pixel, only : P_pixel, P_ColorMap

use :: M_logic,           only : &
   ! procedure for processing if/else/elseif/endif commands
   & cond,               &
   ! logical value to flag whether current data lines should be processed
   & write,              &
   ! integer value of nesting level for IF/ELSEIF/ELSE/ENDIF
   & nest_level
use M_time, only            : now, fmtdate_usage
use M_calculator_plus, only : dnum0
use m_readline, only : system_readline
!-----------------------------------------------------------------------------------------------------------------------------------
   implicit NONE
character(len=*),parameter::ident="&
&@(#)line mode calculator program (that calls jucalc(3f),cond(3f),M_history(3f),M_kracken(3f),M_pixel(3f))"

   integer,parameter             :: dp=kind(0.0d0)         ! calculator returns double precision values
   character(len=:),allocatable  :: scratch                ! numeric value as a string returned by calculator

   character(len=iclen_calc)     :: line0                  ! input line
   character(len=iclen_calc)     :: linet                  ! input line trimmed of leading spaces

   integer                       :: ierr
   integer                       :: ierr_logic             ! error status from if/else/elseif/endif processing
   integer                       :: ii                     ! location of end of verb on input lines
   integer                       :: iin=5                  ! I/O unit to read input from
   integer                       :: ios                    ! status returned by last READ
   logical,save                  :: set_mode=.false.
   integer,parameter             :: number_of_args=100
   character(len=4096)           :: message
   real                          :: values(number_of_args)
   integer                       :: numbers(number_of_args)
   character(len=IPvalue)        :: strings(number_of_args)
   integer                       :: igot
   real                          :: x,y
   character(len=IPvalue),allocatable :: stringsa(:)
   character(len=IPvalue),allocatable :: oldread
   integer                       :: ifound
   oldread=''
!-----------------------------------------------------------------------------------------------------------------------------------
   call expression('ownmode(1)')      ! activate user-defined function interface in calculator
!-----------------------------------------------------------------------------------------------------------------------------------
!  process command line arguments
   call kracken('cmd','-oo -help F -version F -read -replay')  ! parse command line arguments
   call help_usage(lget('cmd_help'))                           ! display version name at startup and exit
   call help_version(lget('cmd_version'))                      ! display help when first starting up and exit
   if(sget('cmd_oo').ne.' ')then                               ! found an expression on command line
      call journal('sc','',dnum0(sget('cmd_oo')))              ! evaluate it assuming it is a numeric expression and write return
      stop                                                     ! stop
   endif
   if(sget('cmd_read').ne.' ')then                         ! found an initial file to read
      iin=iin+10
      open(unit=iin,file=trim(sget('cmd_read')),iostat=ios)
      if(ios.ne.0)iin=iin-10                               ! if open failed fall back to previous I/O unit
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   call journal('O',sget('cmd_replay'))                    ! turn on journal file
!-----------------------------------------------------------------------------------------------------------------------------------
   call parse('MODE','LEAVEQUOTES','YES')                  ! leave double-quotes alone now that past parsing command line
!-----------------------------------------------------------------------------------------------------------------------------------
   INFINITE: do                                            ! start loop to read input lines
      if(iin.eq.5)then                                     ! only show the prompt when reading from stdin
         write(*,'(i0,a)',advance='no')nest_level,'>'      ! write prompt showing nesting level in if/else/endif
      elseif(iin.lt.5)then
         call journal('sc','*testCLI* error: internal value for input unit is ',iin)
         stop
      endif
      if(iin.eq.5)then
         !!---------------------------------
         !!read(iin,'(a)',iostat=ios)line0                 ! read next line of input
         ios=0
         call system_readline(line0,'>')                      ! read editable input line using readline(3c)
         !!---------------------------------
      else
         read(iin,'(a)',iostat=ios)line0                   ! read next line of input
      endif
      if(ios.ne.0)then                                     ! process an I/O error
         if(iin.eq.5)exit INFINITE                         ! if reading from stdin exit
         close(iin,iostat=ios)                             ! if reading from an alternate input file close it
         iin=iin-10                                        ! drop back to previous input file
         cycle INFINITE
      else
         !call journal('T',trim(line0))
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      call redo(line0) ! store into history if not "r".    ! if "r", edit and return a line from the history editor
!-----------------------------------------------------------------------------------------------------------------------------------
      if(line0.eq.'.')then
         call journal('sc','Exiting calculator mode')
         set_mode=.false.
         cycle INFINITE
      elseif(set_mode)then
         call expression(line0)
         cycle INFINITE
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      linet=adjustl(line0)                                  ! trim leading whitespace
      ii=index(linet,' ')                                   ! find end of first word
      if(ii.eq.0)ii=len(linet)                              ! if input line is completely full first word is entire line
!-----------------------------------------------------------------------------------------------------------------------------------
      if(linet(:1).eq.'#')then                              ! ignore comment lines
         cycle INFINITE
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      if(index(linet(:ii),'=').ne.0)then                    ! if first word has an equal in it assume it is an expression
         call expression(linet)
         cycle INFINITE
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      select case(linet(:ii))                               ! using verb (first word) select an action
!-----------------------------------------------------------------------------------------------------------------------------------
      case('if','elseif','else','endif')
         call cond(linet(:ii),linet(ii+1:),ierr_logic)      ! process conditional directive assumed to be in SOURCE "$verb..."
      case default
!-----------------------------------------------------------------------------------------------------------------------------------
      if(.not.write)cycle INFINITE                         ! skip line if reading a conditional block not to be executed
!-----------------------------------------------------------------------------------------------------------------------------------
         ifound=0
         call pixel(ifound)
         if(ifound.eq.0) cycle INFINITE
!-----------------------------------------------------------------------------------------------------------------------------------
         select case(linet(:ii))                            ! handle all the other verbs
         case('.','quit')                                  ! COMMAND: quit program
            call journal('*testCLI*: that''s all folks ...')
            stop
!-----------------------------------------------------------------------------------------------------------------------------------
         case(' ')                                         ! ignore blank lines
!-----------------------------------------------------------------------------------------------------------------------------------
         case('help')                                      ! display help
            call execute_command_line('shell --help')      ! see if shell will execute it
!-----------------------------------------------------------------------------------------------------------------------------------
         case('version')
            call execute_command_line('shell --version')   ! see if shell will execute it
!-----------------------------------------------------------------------------------------------------------------------------------
         case('test1')                                                                    ! test M_KRACKEN(3f) procedure DISSECT(3f)
            call dissect(linet(:ii),'-x 1 -y 1 -help .F. -version .F.',linet(ii+1:),ierr)   ! define and parse command
            call journal('sc','test1 -x=',rget('test1_x'))                                        ! display command arguments
            call journal('sc','test1 -y=',rget('test1_y'))
            call journal('sc','test1 --help=',lget('test1_help'))
            call journal('sc','test1 --version=',lget('test1_version'))
!-----------------------------------------------------------------------------------------------------------------------------------
         case('read')                                              ! read from alternate input file
             call dissect(linet(:ii),'-oo -q .f.',linet(ii+1:),ierr) ! define and parse command
             iin=iin+10                                            ! increment I/O unit used for input
             if(sget('read_oo').eq.'')then
                open(unit=iin,file=oldread,iostat=ios,iomsg=message,status='old')
             else
                oldread=sget('read_oo')
                open(unit=iin,file=oldread,iostat=ios,iomsg=message,status='old')
             endif
             if(ios.ne.0)then
                iin=iin-10                                ! if open failed fall back to previous I/O unit
                write(*,*)'*read* ERROR:'//trim(message)
             endif
!-----------------------------------------------------------------------------------------------------------------------------------
         case('today')
             call dissect(linet(:ii),'-help .f.',linet(ii+1:),ierr)  ! define and parse command
             if(lget('today_help'))then
                call fmtdate_usage(3)
             endif
             scratch=now(sget('today_oo'))
             write(*,'(a)')scratch
!-----------------------------------------------------------------------------------------------------------------------------------
         case('print')                                          ! print expression word by word
             call dissect(linet(:ii),'-oo',linet(ii+1:),ierr)   ! define and parse command (note explicit -oo when no other options)
             call journal(trim(sget('read_oo')))
!-----------------------------------------------------------------------------------------------------------------------------------
         case('chdir')                                          ! change working directory
             call dissect(linet(:ii),'-oo',linet(ii+1:),ierr)   ! define and parse command (note explicit -oo when no other options)
             call system_chdir(trim(sget('read_oo')),ierr)
!-----------------------------------------------------------------------------------------------------------------------------------
         case('set')                                            ! calculator expression
                                                                ! define and parse command (note explicit -oo when no other options)
             call dissect(linet(:ii),'-oo -q .false.',linet(ii+1:),ierr)
             scratch=sget('set_oo')
             if(scratch.eq. ' ' )then
                set_mode=.true.
             else
                call expression(scratch)
             endif
!-----------------------------------------------------------------------------------------------------------------------------------
         case default
            call execute_command_line(linet(:len_trim(linet)))   ! see if shell will execute it
         end select
      end select
!-----------------------------------------------------------------------------------------------------------------------------------
   enddo INFINITE
!-----------------------------------------------------------------------------------------------------------------------------------
   if (nest_level.ne.0) then                               ! check to make sure all if blocks are closed
      call journal('sc','*logic* error - IF block not closed.')
      stop
   endif
   call journal('*testCLI* exiting')
!-----------------------------------------------------------------------------------------------------------------------------------
contains
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine pixel(ifound)
use :: M_pixel, only : P_pixel, P_ColorMap
 use :: M_pixel, only : circle          ! [M_pixel] draw a circle using current line width and color
 use :: M_pixel, only : clear           ! [M_pixel] clear background to current color or specified color index
 use :: M_pixel, only : color           ! [M_pixel] set current color index
 use :: M_pixel, only : draw2           ! [M_pixel] draw from current position to given point
 use :: M_pixel, only : line            ! [M_pixel] draw line between two points
 use :: M_pixel, only : linewidth       ! [M_pixel] set linewidth
 use :: M_pixel, only : move2           ! [M_pixel] change current position
 use :: M_pixel, only : ortho2          ! [M_pixel] define the area of the virtual world coordinates to map to the viewport
 use :: M_pixel, only : prefsize        ! [M_pixel] specify size of pixel array
 use :: M_pixel, only : rdraw2          ! [M_pixel] draw from current position to given point
 use :: M_pixel, only : rect            ! [M_pixel] draw rectangle given two corners
 use :: M_pixel, only : rmove2          ! [M_pixel] relative move
 use :: M_pixel, only : vexit           ! [M_pixel] exit pixel graphics mode
 use :: M_pixel, only : viewport        ! [M_pixel] Specify which part of the screen to draw in.
 use :: M_pixel, only : vinit           ! [M_pixel] initialize pixel graphics module
 use :: M_pixel, only : drawstr         ! [M_pixel] Draw the text string at the current position
 use :: M_pixel, only : centertext      ! [M_pixel]
 use :: M_pixel, only : xcentertext     ! [M_pixel]
 use :: M_pixel, only : ycentertext     ! [M_pixel]
 use :: M_pixel, only : textang         ! [M_pixel] set text angle
 use :: M_pixel, only : textsize        ! [M_pixel] set text size in world units
 use :: M_pixel, only : circleprecision ! [M_pixel] set number of line segments used to approximate a circle
 use :: M_pixel, only : point2          ! [M_pixel] Draw a point at x, y
 use :: M_pixel, only : print_ascii     ! [M_pixel] print small pixel array as ASCII text
 use :: M_pixel, only : print_ppm       ! [M_pixel] print pixel array as a ppm p3 file
 use :: M_pixel, only : mapcolor        ! [M_pixel] set a color index using RGB values
 use :: M_pixel, only : arc             ! [M_pixel] draw an arc using current line width and color
 use :: M_pixel, only : getgp2          ! [M_pixel] Gets the current graphics position in world coords.
 use :: M_pixel, only : getdisplaysize  ! [M_pixel] Returns the width and height of the device in pixels
 use :: M_pixel, only : state           ! [M_pixel] print graphics state
 use :: M_pixel, only : makepoly        ! [M_pixel] start polygon
 use :: M_pixel, only : closepoly       ! [M_pixel] close and render polygon
 use :: M_pixel, only : font            ! [M_pixel] select font style

use :: M_pixel, only : biggest_ortho2  ! [M_pixel] define the area of the virtual world coordinates to map to the viewport
use :: M_pixel, only : hershey         ! [M_pixel] draw text string as Hershey software vector fonts
use :: M_pixel, only : justfy          ! [M_pixel] return lengths used to justify a string when calling hershey
use :: M_pixel, only : polyline        ! [M_pixel] connect points with lines

use :: M_writegif,        only : writegif
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=*),parameter::ident="@(#)pixel(3fp):parse M_pixel(3fm) module routines in shell(1)"
integer :: ifound
!-----------------------------------------------------------------------------------------------------------------------------------
      select case(linet(:ii))                               ! using verb (first word) select an action
         case('clear')
             call return_values(igot,iwant=-1,ierr=ierr)
             if(ierr.eq.0)then
                if(igot.eq.0)then
                   call clear()
                elseif(igot.eq.0)then
                   numbers(:igot)=int(values(:igot))
                   call clear(numbers(1))
                else
                   write(*,*)'*clear* incorrect number of parameters=',igot
                endif
             endif
         case('getgp2')                                  ! [M_pixel] Gets the current graphics position in world coords.
             call dissect(linet(:ii),'-oo ',linet(ii+1:),ierr)
             stringsa=sgets('getgp2_oo')
             if(size(stringsa).eq.2)then
                call getgp2(x,y)
                if(stringsa(1).ne.'')call stuff(trim(stringsa(1)),x)
                if(stringsa(2).ne.'')call stuff(trim(stringsa(2)),y)
             endif
         case('getdisplaysize')                          ! [M_pixel] Returns the width and height of the device in pixels
             call dissect(linet(:ii),'-oo ',linet(ii+1:),ierr)
             stringsa=sgets('getdisplaysize_oo')
             if(size(stringsa).eq.2)then
                call getdisplaysize(x,y)
                if(stringsa(1).ne.'')call stuff(trim(stringsa(1)),x)
                if(stringsa(2).ne.'')call stuff(trim(stringsa(2)),y)
             endif
         case('color')
             call return_values(igot,iwant=1,ierr=ierr)
             if(ierr.eq.0) call color(int(values(1)))
         case('point2')
             call return_values(igot,iwant=2,ierr=ierr)
             if(ierr.eq.0) call point2(values(1),values(2))
         case('draw2')
             call return_values(igot,iwant=2,ierr=ierr)
             if(ierr.eq.0) call draw2(values(1),values(2))
         case('drawstr')         ! [M_pixel] Draw the text string at the current position
             call dissect(linet(:ii),'-oo ',linet(ii+1:),ierr)
             call drawstr(sget('drawstr_oo'))
         case('textang')         ! [M_pixel] set text angle
             call return_values(igot,iwant=1,ierr=ierr)
             if(ierr.eq.0) call textang(values(1))
         case('textsize')        ! [M_pixel] set text size in world units
             call return_values(igot,iwant=2,ierr=ierr)
             if(ierr.eq.0) call textsize(values(1),values(2))
         case('centertext')        ! [M_pixel] turn text centering mode on or off
             call dissect(linet(:ii),'-oo ',linet(ii+1:),ierr)
             call centertext( lget('centertext_oo'))
         case('xcentertext')
             call dissect(linet(:ii),'-oo ',linet(ii+1:),ierr)
             call xcentertext()
         case('ycentertext')
             call dissect(linet(:ii),'-oo ',linet(ii+1:),ierr)
             call ycentertext()
         case('move2')
             call return_values(igot,iwant=2,ierr=ierr)
             if(ierr.eq.0) call move2(values(1),values(2))
         case('rdraw2')
             call return_values(igot,iwant=2,ierr=ierr)
             if(ierr.eq.0) call rdraw2(values(1),values(2))
         case('rmove2')
             call return_values(igot,iwant=2,ierr=ierr)
             if(ierr.eq.0) call rmove2(values(1),values(2))
         case('line')
             call return_values(igot,iwant=4,ierr=ierr)
             if(ierr.eq.0) call line(values(1),values(2),values(3),values(4))
         case('rect')
             call return_values(igot,iwant=4,ierr=ierr)
             if(ierr.eq.0) call rect(values(1),values(2),values(3),values(4))
         case('circle')
             call return_values(igot,iwant=3,ierr=ierr)
             if(ierr.eq.0) call circle(values(1),values(2),values(3))
         case('arc')
             call return_values(igot,iwant=5,ierr=ierr)
             if(ierr.eq.0) call arc(values(1),values(2),values(3),values(4),values(5))
         case('polyline')
             call dissect(linet(:ii),'-oo ',linet(ii+1:),ierr)

         case('mapcolor')
             call return_values(igot,iwant=4,ierr=ierr)
             numbers(1:4)=int(values(1:4))
             if(ierr.eq.0) call mapcolor(numbers(1),numbers(2),numbers(3),numbers(4))
         case('circleprecision')
             call return_values(igot,iwant=1,ierr=ierr)
             numbers(1)=int(values(1))
             if(ierr.eq.0) call circleprecision(numbers(1))
         case('linewidth')
             call dissect(linet(:ii),'-oo ',linet(ii+1:),ierr)
             call linewidth(iget('linewidth_oo'))

         case('prefsize')
             call return_values(igot,iwant=2,ierr=ierr)
             if(ierr.eq.0)then
                numbers(:2)=int(values(:2))
                call prefsize(numbers(1),numbers(2))
             endif
         case('vinit')
             call dissect(linet(:ii),'-oo ',linet(ii+1:),ierr)
             call vinit()
         case('viewport')
             call return_values(igot,iwant=4,ierr=ierr)
             if(ierr.eq.0) call viewport(values(1),values(2),values(3),values(4))
         case('ortho2')
             call return_values(igot,iwant=4,ierr=ierr)
             if(ierr.eq.0) call ortho2(values(1),values(2),values(3),values(4))
         case('vexit')
             call dissect(linet(:ii),'-oo ',linet(ii+1:),ierr)
             call vexit()

         case('hershey')
             call dissect(linet(:ii),'-oo ',linet(ii+1:),ierr)
         case('justfy')
             call dissect(linet(:ii),'-oo ',linet(ii+1:),ierr)
         case('font')
             call dissect(linet(:ii),'-oo ',linet(ii+1:),ierr)
             call font(sget('font_oo'))

         case('writegif')
             call dissect(linet(:ii),'-oo ',linet(ii+1:),ierr)
             call writegif(sget('writegif_oo'),P_pixel,P_colormap)
         case('show')
             call dissect(linet(:ii),'-oo ',linet(ii+1:),ierr)
             if(sget('').eq.'')then
                call writegif('display.gif',P_pixel,P_colormap)
                call execute_command_line('display display.gif')
             else
                call writegif(sget('writegif_oo'),P_pixel,P_colormap)
                call execute_command_line('display '//sget('writegif_oo'))
             endif
         case('print_ascii')     ! [M_pixel] print small pixel array as ASCII text
             call dissect(linet(:ii),'-oo ',linet(ii+1:),ierr)
             call print_ascii(sget('print_ascii_oo'))
         case('print_ppm')       ! [M_pixel] print pixel array as a ppm p3 file
             call dissect(linet(:ii),'-oo ',linet(ii+1:),ierr)
             call print_ppm(sget('print_ppm_oo'))
         case('state')     ! [M_pixel] print graphics state
             call dissect(linet(:ii),'-oo ',linet(ii+1:),ierr)
             call state(sget('state_oo'))
         case('makepoly')     ! [M_pixel] start polygon
             call dissect(linet(:ii),'-oo ',linet(ii+1:),ierr)
             call makepoly()
         case('closepoly')     ! [M_pixel] close and render polygon
             call dissect(linet(:ii),'-oo ',linet(ii+1:),ierr)
             call closepoly()
!-----------------------------------------------------------------------------------------------------------------------------------
         case default
            ifound=1 ! indicate none of the cases were selected
         end select
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine pixel
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine return_values(igot,iwant,ierr)
integer,intent(out)           :: igot
integer,intent(in)            :: iwant
integer,intent(out)           :: ierr
   character(len=*),parameter :: delims=' '

   igot=0
   call dissect(linet(:II),'-oo ',linet(II+1:),ierr)
   call strgar2(sget(trim(linet(:II))//'_oo'),NUMBER_OF_ARGS,VALUES,igot,delims,ierr)

   if(ierr.ne.0)then
      write(*,*)'*return_values* ERROR: could not convert to numeric values:'//trim(linet)
   else
      if(igot.ne.iwant.and.iwant.ne.-1)then
         write(*,*)'return_values* ERROR: wanted ',iwant,' and got ',igot,' in '//trim(linet)
         ierr=-1
      endif
   endif

end subroutine return_values
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine expression(inline)
character(len=*),intent(in)   :: inline                 ! expression to convert to a value
integer,parameter             :: dp=kind(0.0d0)         ! calculator returns double precision values
character(len=iclen_calc)     :: event                  ! string value or message returned by calculator
character(len=iclen_calc)     :: outlin                 ! numeric value as a string returned by calculator
real(kind=dp)                 :: rvalue                 ! numeric value returned by calculator
integer                       :: ierr
   call jucalc(inline,outlin,event,rvalue,ierr)
   if(.not.lget('set_q'))then
      select case (ierr)                                        ! handle different status flag values returned by calculator
      case(0);  call journal(trim(outlin)//' = '//trim(inline)) ! a numeric value was returned without error
      case(2);  call journal(event(:int(rvalue)))               ! a string value was returned without error
      case(1);  call journal('message===>'//trim(event))        ! a request for a message has been returned (from DUMP or FUNC)
      case(-1); call journal('error===>'//trim(event))          ! an error has occurred
      case default; !                                              this should not occur
      call journal('sc','*expression* UNEXPECTED IERR VALUE ',IERR)
      end select
   endif
end subroutine expression
end program testCLI
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!2345678901234567890123456789012345678901234567890123456789012345678901234567890
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
'       shell(1f) - shell for demonstrating major modules in libjust4.a          ',&
'                                                                                ',&
'SYNOPSIS                                                                        ',&
'       shell expression|-help|-version|[-replay][-read FILENAME]                ',&
'                                                                                ',&
'DESCRIPTION                                                                     ',&
'    Example command line interface with command line history, numeric           ',&
'    expressions, if/else/elseif/endif logic, and Unix-like command              ',&
'    parsing.                                                                    ',&
'                                                                                ',&
'OPTIONS                                                                         ',&
'   --help      display this help and exit                                       ',&
'   --version   output version information and exit                              ',&
'                                                                                ',&
'USAGE                                                                           ',&
'                                                                                ',&
' At the command prompt the following example commands may be used:              ',&
'                                                                                ',&
'  #----------------------------------------------------------------------------#',&
'  | command                     |exercises       | description                 |',&
'  #----------------------------------------------------------------------------#',&
'  | r                           | M_HISTORY      | enter history editor;       |',&
'  |                             |                | ? will produce help         |',&
'  #----------------------------------------------------------------------------#',&
'  | today format [-help]        | M_TIME         | display current time using  |',&
'  |                             |                | fmtdate(3f)                 |',&
'  #----------------------------------------------------------------------------#',&
'  | if/else/elseif expression   | M_LOGIC        | conditionally process input |',&
'  |                             |                | lines                       |',&
'  | endif                       | M_LOGIC        | end IF block                |',&
'  #----------------------------------------------------------------------------#',&
'  | set dump                    | M_CALCULATOR   | dump currently defined      |',&
'  |                             |                | variable names              |',&
'  | set funcs                   | M_CALCULATOR   | describe functions available|',&
'  |                             |                | in expressions              |',&
'  | set nc()                    | JUOWN1/CALC_NC | display list of ncurses()   |',&
'  |                             |                | functions                   |',&
'  | set expression [-q]         | M_CALCULATOR   | anything else is assumed to |',&
'  |                             |                | be a numeric expression     |',&
'  | set                         | M_CALCULATOR   | If no expression switch to  |',&
'  |                             |                | calculator mode till "." is |',&
'  |                             |                | entered.                    |',&
'  #----------------------------------------------------------------------------#',&
'  | read file [-q]              | M_KRACKEN      | read from another input file|',&
'  | test1 -x -y --help --version| M_KRACKEN      | test of DISSECT(3f)         |',&
'  #----------------------------------------------------------------------------#',&
'  |                             | M_PIXEL        | pixel graphics              |',&
'  |prefsize:        specify size of pixel array                                 ',&
'  |vinit:           initialize pixel array drawing module                       ',&
'  |                                                                             ',&
'  |rect:            draw line rectangle given two opposite corners              ',&
'  |line:            draw line between two points applying line width and color  ',&
'  |polyline:        draw lines between points                                   ',&
'  |rmove2:          relative move                                               ',&
'  |move2:           move current position                                       ',&
'  |rdraw2:          relative draw                                               ',&
'  |draw2:           draw a line from current position to specified point        ',&
'  |                                                                             ',&
'  |point2:          Draw a point at x, y                                        ',&
'  |                                                                             ',&
'  |arc:             draw a arc using current line width and color               ',&
'  |circle:          draw a circle using current line width and color            ',&
'  |circleprecision: set number of line segments making up a circle              ',&
'  |                                                                             ',&
'  |drawchar:        draw text at the current position                           ',&
'  |drawstr:         draw text at the current position                           ',&
'  |                                                                             ',&
'  |linewidth:       set line width for lines drawn in pixel image               ',&
'  |color:           set current color for lines drawn in pixel image            ',&
'  |textsize:        set text size in world units                                ',&
'  |ycentertext:     set text centering mode on for drawstr(3f) and drawc(3f) in Y direction',&
'  |xcentertext:     set text centering mode for drawstr(3f) and drawc(3f) in X direction',&
'  |centertext:      set text centering mode for drawstr(3f) and drawc(3f)       ',&
'  |textang:         set angle in degrees to draw text at using drawstr(3f)      ',&
'  |                                                                             ',&
'  |mapcolor:        set a color index using RGB values                          ',&
'  |                                                                             ',&
'  |strlength:       length of string using current font size                    ',&
'  |getviewport:     return viewport in screen pixel coordinates                 ',&
'  |getdisplaysize:  Returns the width and height of the device in pixels        ',&
'  |state:           print graphics state of M_pixel graphics module             ',&
'  |getgp2:          get current graphics position                               ',&
'  |                                                                             ',&
'  |viewport:        Specify which part of the screen to draw in.                ',&
'  |ortho2:          define the area of the virtual world coordinates to map to the viewport',&
'  |ortho2_biggest:  given a window size, find and set to largest accommodating viewport',&
'  |                                                                             ',&
'  |print_ppm:       print pixel array as a P3 PPM file                          ',&
'  |print_ascii:     print pixel array as an ASCII block of text                 ',&
'  |                                                                             ',&
'  |clear:           set background color all to specified color index           ',&
'  |                                                                             ',&
'  |vexit:           exit pixel array drawing module                             ',&
'  |                             |                |                             |',&
'  #----------------------------------------------------------------------------#',&
'  | help                        |                | display this information    |',&
'  |                             |                | this information            |',&
'  | quit|.                      |                | exit program                |',&
'  #----------------------------------------------------------------------------#',&
'  | anything_else               | execute as system command                    |',&
'  #----------------------------------------------------------------------------#',&
'                                                                                ',&
' It can be confusing if you make variables that are command names.              ',&
' An expression on the command line is evaluated and then the program exits.     ',&
'                                                                                ',&
'']
   WRITE(*,'(a)')(trim(help_text(i)),i=1,size(help_text))
   stop ! if -help was specified, stop
endif
end subroutine help_usage
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!        shell(1f) - shell for demonstrating major modules in libjust4.a
!!
!!##SYNOPSIS
!!
!!        shell expression|-help|-version|[-replay][-read FILENAME]
!!
!!##DESCRIPTION
!!     Example command line interface with command line history, numeric
!!     expressions, if/else/elseif/endif logic, and Unix-like command
!!     parsing.
!!
!!##OPTIONS
!!    --help      display this help and exit
!!    --version   output version information and exit
!!
!!##USAGE
!!
!!  At the command prompt the following example commands may be used:
!!
!!   #----------------------------------------------------------------------------#
!!   | command                     |exercises       | description                 |
!!   #----------------------------------------------------------------------------#
!!   | r                           | M_HISTORY      | enter history editor;       |
!!   |                             |                | ? will produce help         |
!!   #----------------------------------------------------------------------------#
!!   | today format [-help]        | M_TIME         | display current time using  |
!!   |                             |                | fmtdate(3f)                 |
!!   #----------------------------------------------------------------------------#
!!   | if/else/elseif expression   | M_LOGIC        | conditionally process input |
!!   |                             |                | lines                       |
!!   | endif                       | M_LOGIC        | end IF block                |
!!   #----------------------------------------------------------------------------#
!!   | set dump                    | M_CALCULATOR   | dump currently defined      |
!!   |                             |                | variable names              |
!!   | set funcs                   | M_CALCULATOR   | describe functions available|
!!   |                             |                | in expressions              |
!!   | set nc()                    | JUOWN1/CALC_NC | display list of ncurses()   |
!!   |                             |                | functions                   |
!!   | set expression [-q]         | M_CALCULATOR   | anything else is assumed to |
!!   |                             |                | be a numeric expression     |
!!   | set                         | M_CALCULATOR   | If no expression switch to  |
!!   |                             |                | calculator mode till "." is |
!!   |                             |                | entered.                    |
!!   #----------------------------------------------------------------------------#
!!   | read file [-q]              | M_KRACKEN      | read from another input file|
!!   | test1 -x -y --help --version| M_KRACKEN      | test of DISSECT(3f)         |
!!   #----------------------------------------------------------------------------#
!!   |                             | M_PIXEL        | pixel graphics              |
!!   |prefsize:        specify size of pixel array
!!   |vinit:           initialize pixel array drawing module
!!   |
!!   |rect:            draw line rectangle given two opposite corners
!!   |line:            draw line between two points applying line width and color
!!   |polyline:        draw lines between points
!!   |rmove2:          relative move
!!   |move2:           move current position
!!   |rdraw2:          relative draw
!!   |draw2:           draw a line from current position to specified point
!!   |
!!   |point2:          Draw a point at x, y
!!   |
!!   |arc:             draw a arc using current line width and color
!!   |circle:          draw a circle using current line width and color
!!   |circleprecision: set number of line segments making up a circle
!!   |
!!   |drawchar:        draw text at the current position
!!   |drawstr:         draw text at the current position
!!   |
!!   |linewidth:       set line width for lines drawn in pixel image
!!   |color:           set current color for lines drawn in pixel image
!!   |textsize:        set text size in world units
!!   |ycentertext:     set text centering mode on for drawstr(3f) and drawc(3f) in Y direction
!!   |xcentertext:     set text centering mode for drawstr(3f) and drawc(3f) in X direction
!!   |centertext:      set text centering mode for drawstr(3f) and drawc(3f)
!!   |textang:         set angle in degrees to draw text at using drawstr(3f)
!!   |
!!   |mapcolor:        set a color index using RGB values
!!   |
!!   |strlength:       length of string using current font size
!!   |getviewport:     return viewport in screen pixel coordinates
!!   |getdisplaysize:  Returns the width and height of the device in pixels
!!   |state:           print graphics state of M_pixel graphics module
!!   |getgp2:          get current graphics position
!!   |
!!   |viewport:        Specify which part of the screen to draw in.
!!   |ortho2:          define the area of the virtual world coordinates to map to the viewport
!!   |ortho2_biggest:  given a window size, find and set to largest accommodating viewport
!!   |
!!   |print_ppm:       print pixel array as a P3 PPM file
!!   |print_ascii:     print pixel array as an ASCII block of text
!!   |
!!   |clear:           set background color all to specified color index
!!   |
!!   |vexit:           exit pixel array drawing module
!!   |                             |                |                             |
!!   #----------------------------------------------------------------------------#
!!   | help                        |                | display this information    |
!!   |                             |                | this information            |
!!   | quit|.                      |                | exit program                |
!!   #----------------------------------------------------------------------------#
!!   | anything_else               | execute as system command                    |
!!   #----------------------------------------------------------------------------#
!!
!!  It can be confusing if you make variables that are command names.
!!  An expression on the command line is evaluated and then the program exits.
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
'@(#)PRODUCT:        CLI library utilities and examples>',&
'@(#)PROGRAM:        shell(1f)>',&
'@(#)DESCRIPTION:    shell for demonstrating major modules in libjust4.a>',&
'@(#)VERSION:        1.2, 20160624>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)REPORTING BUGS: http://www.urbanjost.altervista.org/>',&
'@(#)HOME PAGE:      http://www.urbanjost.altervista.org/index.html>',&
'@(#)LICENSE:        Public Domain. This is free software: you are free to change and redistribute it.>',&
'@(#)                There is NO WARRANTY, to the extent permitted by law.>',&
'@(#)COMPILED:       Thu, Jun 29th, 2017 6:14:27 AM>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if -version was specified, stop
endif
end subroutine help_version
!-----------------------------------------------------------------------------------------------------------------------------------
