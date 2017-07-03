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
'   huegif(1) - generate color wheels as GIF pixmap files                        ',&
'                                                                                ',&
'SYNOPSIS                                                                        ',&
'   huegif                                                                       ',&
'                                                                                ',&
'DESCRIPTION                                                                     ',&
'                                                                                ',&
'    Generates color wheels using the HSL (Hue Lightness, Saturation)            ',&
'    model as GIF pixel files.                                                   ',&
'                                                                                ',&
'OPTIONS                                                                         ',&
'    --help      display this help and exit                                      ',&
'                                                                                ',&
'    --version   output version information and exit                             ',&
'                                                                                ',&
'']
   WRITE(*,'(a)')(trim(help_text(i)),i=1,size(help_text))
   stop ! if -help was specified, stop
endif
end subroutine help_usage
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!    huegif(1) - generate color wheels as GIF pixmap files
!!
!!##SYNOPSIS
!!
!!    huegif
!!
!!##DESCRIPTION
!!
!!     Generates color wheels using the HSL (Hue Lightness, Saturation)
!!     model as GIF pixel files.
!!
!!##OPTIONS
!!     --help      display this help and exit
!!
!!     --version   output version information and exit
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
'@(#)PROGRAM:        huegif(1f)>',&
'@(#)DESCRIPTION:    generate GIF pixel files with color wheels at different lightness>',&
'@(#)VERSION:        1.0 20170604>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)HOME PAGE:      http://www.urbanjost.altervista.org/index.html>',&
'@(#)COMPILED:       Wed, Jun 14th, 2017 10:01:07 AM>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if -version was specified, stop
endif
end subroutine help_version
!-----------------------------------------------------------------------------------------------------------------------------------
program demo_mapcolor
use M_pixel
use m_color,    only : hue
use M_writegif, only : writegif
use M_units,    only : cosd, sind
use M_kracken,  only : kracken, sget, IPvalue, lget, rget
implicit none
   character(len=4096)  :: filename
   real                 :: lightstep
   integer              :: ii,iframe
   integer,parameter    :: SLICES=30
   integer,parameter    :: RINGS=  8
   real                 :: LIGHTNESS
   integer,parameter    :: BOX=1200

   ! define command arguments, default values and crack command line
   call kracken('huegif','-oo -help .f. -version .f.')
   call help_usage(lget('huegif_help'))
   call help_version(lget('huegif_version'))

   call prefsize(BOX,BOX)
   call vinit(' ')
   call color(0)
   call clear()
   call color(7)
   call biggest_ortho2(-110./2.,85./2.,-110./2.,110./2.)

   LIGHTNESS=100.0
   lightstep=-5
   do ii=1,19
      iframe=ii

      call color(0)
      call clear()
      call color(7)
      call wheel()

      write(filename,'("hue.3_",i3.3,".gif")')int(LIGHTNESS)
      call writegif(filename,P_pixel,P_colormap)

      LIGHTNESS=LIGHTNESS+LIGHTSTEP
   enddo
   call vexit()
contains
!=======================================================================--------
subroutine wheel() ! draw an entire wheel
   character(len=40) :: inline
   real              :: hue_val
   integer           :: ii
   call textang(0.0)
   call color(7)
   call textsize(5.0,6.0)
   call font('times.r')
   call move2(0.0,103.0/2.0)
   call centertext(.true.)
   call linewidth(30)
   call drawstr('COLOR WHEEL')
   call linewidth(0)
   call textsize( 2.5,2.5)
   call font('futura.l')
   call move2(0.0,90.0/2.0)
   write(inline,'("lightness=",f6.2)')LIGHTNESS
   call linewidth(30)
   call drawstr(inline)
   call linewidth(0)
   call textsize(1.5,1.5)
   hue_val=0
   do ii=SLICES, 1,-1
      call slice(hue_val)
   enddo
   call centertext(.false.)
end subroutine wheel
!=======================================================================--------
subroutine slice(hue_val) ! draw a slice
   integer           :: buffer
   real              :: hue_val, ang_inc
   character(len=40) :: inline
   real              :: step
   real              :: X1, X2, X3, X4
   real              :: Y1, Y2, Y3, Y4

   integer           :: maxcolors, current_color
   integer           :: ir, ig, ib
   real              :: r,g,b
   real              :: saturation
   integer           :: j
   integer           :: status
   integer           :: icount
   real              :: angle1, angle2
   real              :: radius1, radius2, radius3, radius4

   integer,save      :: color_count=0

   buffer=8
   ANG_INC=360.0/SLICES
   angle1=hue_val-ANG_INC/2
   angle2=angle1+ANG_INC
   saturation=100
   radius1=32
   radius3=radius1+4
   radius4=radius1+7
   ! draw tic from wheel to start of angle label
   call color(7)
   call linewidth(40)

   do j=0,1
      call move2( radius1*cosd(hue_val+J*ANG_INC/2.0), radius1*sind(hue_val+J*ANG_INC/2.0) )
      call draw2( radius3*cosd(hue_val+J*ANG_INC/2.0), radius3*sind(hue_val+J*ANG_INC/2.0) )
   enddo
   ! draw degree label at tic
   call textang(hue_val)
   call linewidth(20)

   do j=0,1
      call move2( radius4*cosd(hue_val+J*ANG_INC/2.0), radius4*sind(hue_val+J*ANG_INC/2.0) )
      write(inline,'(i0)')nint(hue_val+J*ANG_INC/2.0)
      call drawstr(inline)
   enddo

   call linewidth(0)
   step=radius1/(RINGS)
   radius2=radius1-step
   ! draw a chunk in a slice
   MAXCOLORS=(256)-buffer
   do icount=RINGS+1,2,-1
      CURRENT_COLOR=MOD(color_count,MAXCOLORS)+buffer  ! add buffer to leave base colors alone
      color_count=color_count+1
      ! fancy mapcolor
      call hue("hls",hue_val,LIGHTNESS,saturation,"rgb",r,g,b,status)
      ir=int(r*255.0/100.0+0.50)
      ig=int(g*255.0/100.0+0.50)
      ib=int(b*255.0/100.0+0.50)
      call mapcolor(CURRENT_COLOR,ir,ig,ib)
      call color(CURRENT_COLOR)

      X1=cosd(angle1)*radius2
      Y1=sind(angle1)*radius2
      X2=cosd(angle1)*radius1
      Y2=sind(angle1)*radius1

      X3=cosd(angle2)*radius2
      Y3=sind(angle2)*radius2
      X4=cosd(angle2)*radius1
      Y4=sind(angle2)*radius1

      call makepoly()
      call move2(X1,Y1)
      call draw2(X2,Y2)
      call draw2(X4,Y4)
      call draw2(X3,Y3)
      call closepoly()

      saturation=saturation-100.0/RINGS
      radius1=radius2
      radius2=radius1-step
   enddo
   hue_val=hue_val+ANG_INC
end subroutine slice
end program demo_mapcolor
