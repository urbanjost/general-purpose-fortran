program clockit
use M_kracken,  only : kracken, iget, sget, lget
use M_draw
use M_time,     only : system_sleep, d2o, fmtdate
use M_color,    only : hue
implicit none
call clockit_main()
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
'   clock(1f) - [M_draw] display a clock using the M_draw(3f) graphics module                                                    ',&
'   (LICENSE:PD)                                                                                                                 ',&
'                                                                                                                                ',&
'SYNOPSIS                                                                                                                        ',&
'   clock [SECONDS] [ -sz PIXELS ]                                                                                               ',&
'                                                                                                                                ',&
'DESCRIPTION                                                                                                                     ',&
'                                                                                                                                ',&
'   clock(1) displays a simple clock for the specified number of seconds                                                         ',&
'   or until the "q" character is entered in the graphic display area                                                            ',&
'   using the M_draw(3f) graphics module.                                                                                        ',&
'                                                                                                                                ',&
'OPTIONS                                                                                                                         ',&
'                                                                                                                                ',&
' SECONDS  The clock runs the specified number of seconds before                                                                 ',&
'          stopping. A time of -1 means to run until terminated. Enter                                                           ',&
'          the letter "q" in the graphics area to stop the clock before                                                          ',&
'          the requested time has been exhausted.                                                                                ',&
'                                                                                                                                ',&
' -sz         Size of display window in terms of device rasters                                                                  ',&
' -d          Size of edge of clock. Default is "X11". The units are                                                             ',&
'             somewhat device-dependent. For "X11" the units are rasters.                                                        ',&
' --help      display help text and exit                                                                                         ',&
' --version   display version text and exit                                                                                      ',&
'                                                                                                                                ',&
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
!!    clock(1f) - [M_draw] display a clock using the M_draw(3f) graphics module
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    clock [SECONDS] [ -sz PIXELS ]
!!
!!##DESCRIPTION
!!
!!    clock(1) displays a simple clock for the specified number of seconds
!!    or until the "q" character is entered in the graphic display area
!!    using the M_draw(3f) graphics module.
!!
!!##OPTIONS
!!
!!  SECONDS  The clock runs the specified number of seconds before
!!           stopping. A time of -1 means to run until terminated. Enter
!!           the letter "q" in the graphics area to stop the clock before
!!           the requested time has been exhausted.
!!
!!  -sz         Size of display window in terms of device rasters
!!  -d          Size of edge of clock. Default is "X11". The units are
!!              somewhat device-dependent. For "X11" the units are rasters.
!!  --help      display help text and exit
!!  --version   display version text and exit
!!
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
'@(#)PRODUCT:        GPF library utilities and examples>',&
'@(#)PROGRAM:        clock(1)>',&
'@(#)DESCRIPTION:    draw a clock>',&
'@(#)VERSION:        1.0, 20180616>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)COMPILED:       2021-08-21 22:05:29 UTC-240>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if --version was specified, stop
endif
end subroutine help_version
subroutine clockit_main()

! ident_1="@(#)M_DRAW-based clock"

character(len=20) :: device
integer           :: ix, iy
integer           :: iseconds

!  define command arguments, default values and crack command line
   call kracken('clock','-1 -sz 500 -d X11 -help .F. -version .F.') ! define command and default values and crack command line
   call help_usage(lget('clock_help'))                              ! if -help option is present, display help text and exit
   call help_version(lget('clock_version'))                         ! if -version option is present, display version text and exit

   iseconds = iget('clock_oo')                                      ! get -s value from command line for seconds to run
   ix = max(1,iget('clock_sz'))                                     ! get -x value from command line for window size
   ix=max(ix,20)
   iy=ix
   device = sget('clock_d')                                         ! get -d value from command line to select output device
   call prefsize(ix,iy)                                             ! set display size
   call vinit(device)                                               ! set display device and initialize graphics
   call vclock(iseconds)                                            ! draw clock
   call vexit()                                                     ! exit graphics

end subroutine clockit_main
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine vclock(isec) ! draw a M_DRAW clock
!@(#) draw a M_DRAW clock that quits on 'q'
!     make this into a calculator
character(len=10) :: line
REAL              :: HH,MM,SS
integer           :: dat(8)
integer           :: ihh12
equivalence(dat(1),iye)  !  Year since 1900
equivalence(dat(2),imo)  !  Months since January [0-12]
equivalence(dat(3),ida)  !  Day of the month [1-31]
equivalence(dat(5),ihh)  !  Hours since midnight [0-23]
equivalence(dat(6),imm)  !  Minutes after the hour [0-59]
equivalence(dat(7),iss)  !  Seconds after the minute [0-60]
equivalence(dat(4),idow) !  Days since Sunday [0-6]
equivalence(dat(8),ijul) !  Days since January 1 [0-365]
integer           :: iye
integer           :: imo
integer           :: ida
integer           :: iHH,iMM,iSS
integer           :: idow
integer           :: isec ! number of seconds to run
integer           :: CURRENT_COLOR
integer           :: ijul
integer           :: ir,ig,ib
integer           :: i10
integer           :: i40
integer           :: icount
integer           :: idum
integer           :: istatus
integer           :: letter
real              :: a
real              :: anext
real              :: r,g,b
real              :: hue_val
real              :: rlightness
real              :: saturation
!==================================================================================================================================!
   icount=0
   call pushattributes()
   call pushmatrix()
   call pushviewport()
   call circleprecision(260)
   call page(-50.0,50.0,-50.0,50.0)
   call linewidth(80)
!==================================================================================================================================!
INFINITE: do
   call date_and_time(values=dat) ! initialize hour, minute, and second hands
   ihh12=mod(ihh,12)
   ! GET TIME
   MM=real(IMM)*(-6.0)+90.0
   HH=(real(ihh12-12)+real(IMM)/60.0)*(-30.0)+90.0
   SS=real(ISS)*(-6.0)+90.0
   idum=backbuffer()
   call color(6)  ! background rectangle color
   call clear()
   A=3.1416/3.0
   call circle(0.0,0.0,50.0)
   call color(0)  ! outer circle color
   call move2(-50.0,-50.0)
   call polyfill(.true.)
   call circle(0.0,0.0,50.0)

   CURRENT_COLOR=4
   RLIGHTNESS=real(IHH)/12.0/2.0*100.0
   SATURATION=real(IMM+40)
   HUE_VAL=real(ISS*6)
   call hue("hls",hue_val,RLIGHTNESS,saturation,"rgb",r,g,b,istatus)
   ir=int(r*255.0/100.0+0.50)
   ig=int(g*255.0/100.0+0.50)
   ib=int(b*255.0/100.0+0.50)
   call mapcolor(CURRENT_COLOR,ir,ig,ib)
   call color(CURRENT_COLOR)
   call color(CURRENT_COLOR)  ! inner circle color
   call circle(0.0,0.0,40.0)
   DO I10=1,12
      if(ihh12.eq.i10.and..false.)then
         call color(2)  ! number circle color
         call circle(COS(A)*45.0,SIN(A)*45.0,4.0)
         call color(1)  ! number circle color
         call circle(COS(A)*45.0,SIN(A)*45.0,4.0*real(imm)/60.0)
      else
         call color(7)  ! number circle color
         call circle(COS(A)*45.0,SIN(A)*45.0,4.0)
      endif
      A=A-3.1416/6.0
      ANEXT=A
   enddo
   call color(0) ! hour text color
   call font('futura.l')         ! put hour numbers onto clock face
   call textsize(5.5,5.5)
   call centertext(.true.)
   DO I40=1,12
      call move2(COS(A)*45.0,SIN(A)*45.0)
      if(i40.le.9)then
         write(line,'(i1)')i40
      else
         write(line,'(i2)')i40
      endif
      call drawstr(line)
      A=A-3.1416/6.0
   enddo
   call centertext(.false.)
!==================================================================================================================================!
   ! DRAW HANDS
   call polyfill(.true.)
   call color(0)              ! set to color of hands
   call draw_hands(hh,mm,ss)  ! draw hands to be seen
   call polyfill(.false.)
   call draw_hands(hh,mm,ss)  ! draw hands to be seen

   call polyfill(.false.)
   call draw_date(dat)
   call draw_dow(dat)

   ! DRAW BUTTON
   call polyfill(.true.)
   call color(7)              ! draw button in center of clock
   call circle(0.0,0.0,2.0)
   call color(0)

   call vflush()              ! flush hands
   call swapbuffers()
   letter=checkkey()          ! see if a character was pressed in graphic window
   !write(*,*)icount,' Ordinal=',letter

   ! CHECK TO QUIT
   if(letter.eq.113.or.letter.lt.0)exit INFINITE  ! quit on letter q
   if(icount.gt.isec.and.isec.gt.0)exit INFINITE ! quit from here so hands are drawn

   ! PAUSE
   call system_sleep(1)            ! pause for one second
   icount=icount+1            ! increment number of seconds since started

enddo INFINITE
!==================================================================================================================================!
   ! HOUSECLEANING AND RESTORE PREVIOUS STATE
   call vflush()
   call popattributes()
   call popmatrix()
   call popviewport()
end subroutine vclock
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine draw_date(dat)
integer,intent(in) :: dat(*)
   call color(6)
   call centertext(.true.)
   call font('futura.l')
   !call rect(-20.0,8.0,20.0,28.0)
   call color(0)

   call textsize(4.0,4.5)
   call move2(36.0,46.0)
   call drawstr(fmtdate(dat,'%Y-%M-%D'))  ! YYYY-MM-DD

   call textsize(5.5,5.5)
   call move2(36.0,-46.0)
   call drawstr(fmtdate(dat,'%O'))   ! ordinal day

   call centertext(.false.)
end subroutine draw_date
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine draw_dow(dat)
integer,intent(in) :: dat(*)
   call color(6)
   call centertext(.true.)
   call font('futura.l')
   call textsize(5.5,5.5)
   call color(0)

   call move2(-36.0,46.0)
   call drawstr(fmtdate(dat,'%W'))  ! day of week

   call move2(-36.0,-46.0)
   call drawstr(fmtdate(dat,'%h:%m:%s'))   ! HH:MM:SS

   call centertext(.false.)
end subroutine draw_dow
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine draw_hands(hh,mm,ss)
real,intent(in) :: hh,mm,ss
   call draw_hand(HH,0.0,0.0,27.0,2.50)
   call draw_hand(MM,0.0,0.0,37.0,2.15)
   call draw_hand(SS,0.0,0.0,40.0,1.0)
end subroutine draw_hands
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
SUBROUTINE draw_hand(AA,X,Y,L,W)
REAL,intent(in) ::  AA, X, Y, L, W
   call move2(X,Y)
   call rotate(AA,'z')
   call makepoly()
   call draw2(X+0.7*L,Y+W)
   call draw2(X+L,Y)
   call draw2(X+0.7*L,Y-W)
   call draw2(X,Y)
   call closepoly()
   call pushattributes()
   call color(7)
   call draw2(X+0.7*L,Y+W)
   call draw2(X+L,Y)
   call draw2(X+0.7*L,Y-W)
   call draw2(X,Y)
   call popattributes()
   call rotate(-AA,'z')
END SUBROUTINE draw_hand
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
end program clockit
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
