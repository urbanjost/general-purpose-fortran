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
'    slice(1f) - [M_slices] display a grid of Z values with a 3D view                                                            ',&
'    (LICENSE:PD)                                                                                                                ',&
'                                                                                                                                ',&
'SYNOPSIS                                                                                                                        ',&
'    slice -f FILENAME -d X11 -w 20 -x 680 -y 680 -a 30 -b 40                                                                    ',&
'                                                                                                                                ',&
'DESCRIPTION                                                                                                                     ',&
'   Given a file that defines a grid of Z values display each row as a                                                           ',&
'   curve with the DL_SLICES(3f) routine with a 3D view.                                                                         ',&
'                                                                                                                                ',&
'   On interactive devices the viewing angles may be adjusted by entering                                                        ',&
'   from the set of letters "lrud" (left, right, up, down)..                                                                     ',&
'                                                                                                                                ',&
'   Enter "h" in graphics window for further help on controlling the                                                             ',&
'   displayed curves''                                                                                                           ',&
'                                                                                                                                ',&
'   On batch devices the output is placed in filename                                                                            ',&
'                                                                                                                                ',&
'      "FILENAME_AxB.DEVICE"                                                                                                     ',&
'                                                                                                                                ',&
'OPTIONS                                                                                                                         ',&
'   -f FILENAME   filename with contents of form                                                                                 ',&
'                                                                                                                                ',&
'                  z1 z2 z3 z4 z5 ...                                                                                            ',&
'                  z1 z2 z3 z4 z5 ...                                                                                            ',&
'                  z1 z2 z3 z4 z5 ...                                                                                            ',&
'                  z1 z2 z3 z4 z5 ...                                                                                            ',&
'   -a XANGLE     angle (in degrees) of x axis (NX) from horizontal                                                              ',&
'                 from 0 to 80.                                                                                                  ',&
'   -b YANGLE     angle (in degrees) of z axis (NZ) from horizontal                                                              ',&
'                 from 5 to 80.                                                                                                  ',&
'   -d DEVICE     device type. Defaults to "X11". Enter an invalid                                                               ',&
'                 device name such as "help" to see a list of                                                                    ',&
'                 available devices.                                                                                             ',&
'   -w LINEWIDTH  line width in 1/10 000, of x-axis                                                                              ',&
'   -x XSIZE      X size in device units of output                                                                               ',&
'   -y YSIZE      Y size in device units of output                                                                               ',&
'EXAMPLES                                                                                                                        ',&
'  Sample commands:                                                                                                              ',&
'                                                                                                                                ',&
'   $ slice -f in1                                                                                                               ',&
'                                                                                                                                ',&
'   # you can adjust line width and size for different devices                                                                   ',&
'   $ slice -f in1 -d svg -w 20 -x 14000 -y 14000                                                                                ',&
'                                                                                                                                ',&
'AUTHOR                                                                                                                          ',&
'   John S. Urban; calls M_slice(3f) module (which is based on                                                                   ',&
'   PD routines by D. Long, 1984, JPL).                                                                                          ',&
'LICENSE                                                                                                                         ',&
'   Public License                                                                                                               ',&
'']
   WRITE(*,'(a)')(trim(help_text(i)),i=1,size(help_text))
   stop ! if --help was specified, stop
endif
end subroutine help_usage
!>
!!##NAME
!!     slice(1f) - [M_slices] display a grid of Z values with a 3D view
!!     (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!     slice -f FILENAME -d X11 -w 20 -x 680 -y 680 -a 30 -b 40
!!
!!##DESCRIPTION
!!    Given a file that defines a grid of Z values display each row as a
!!    curve with the DL_SLICES(3f) routine with a 3D view.
!!
!!    On interactive devices the viewing angles may be adjusted by entering
!!    from the set of letters "lrud" (left, right, up, down)..
!!
!!    Enter "h" in graphics window for further help on controlling the
!!    displayed curves'
!!
!!    On batch devices the output is placed in filename
!!
!!       "FILENAME_AxB.DEVICE"
!!
!!##OPTIONS
!!    -f FILENAME   filename with contents of form
!!
!!                   z1 z2 z3 z4 z5 ...
!!                   z1 z2 z3 z4 z5 ...
!!                   z1 z2 z3 z4 z5 ...
!!                   z1 z2 z3 z4 z5 ...
!!    -a XANGLE     angle (in degrees) of x axis (NX) from horizontal
!!                  from 0 to 80.
!!    -b YANGLE     angle (in degrees) of z axis (NZ) from horizontal
!!                  from 5 to 80.
!!    -d DEVICE     device type. Defaults to "X11". Enter an invalid
!!                  device name such as "help" to see a list of
!!                  available devices.
!!    -w LINEWIDTH  line width in 1/10 000, of x-axis
!!    -x XSIZE      X size in device units of output
!!    -y YSIZE      Y size in device units of output
!!##EXAMPLES
!!
!!   Sample commands:
!!
!!    $ slice -f in1
!!
!!    # you can adjust line width and size for different devices
!!    $ slice -f in1 -d svg -w 20 -x 14000 -y 14000
!!
!!##AUTHOR
!!    John S. Urban; calls M_slice(3f) module (which is based on
!!    PD routines by D. Long, 1984, JPL).
!!##LICENSE
!!    Public License
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
'@(#)PROGRAM:        slice(1)>',&
'@(#)DESCRIPTION:    display a set of curves as slices with a 3D view>',&
'@(#)VERSION:        1.1, 20190326>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)HOME PAGE:      http://www.urbanjost.altervista.org/index.html>',&
'@(#)COMPILED:       2022-07-15 10:00:24 UTC-240>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if --version was specified, stop
endif
end subroutine help_version
program slice
use M_io, only        : slurp
use M_strings,   only : s2vs
use M_kracken95, only : kracken,iget,lget,rget,sget
use M_draw
use M_msg,       only : str
implicit none
character(len=1),allocatable :: text(:) ! array to hold file in memory
integer                      :: length
integer                      :: irows
integer                      :: icols
integer                      :: irows2
integer                      :: icols2
integer                      :: nchars
integer                      :: i
integer                      :: j
integer                      :: k
integer                      :: istart
real,allocatable             :: array(:,:)
character(len=:),allocatable :: line

character(len=:),allocatable :: filename
character(len=20)            :: device

real                         :: a
real                         :: b
integer                      :: ixsize,iysize
integer                      :: w
!-----------------------------------------------------------------------------------------------------------------------------------
!  define command arguments, default values and crack command line
   call kracken('slice',' -f -help .f. -version .f. -d x11 -a 30 -b 40 -x 680 -y 680 -w 20')
   call help_usage(lget('slice_help'))                                ! if -help option is present, display help text and exit
   call help_version(lget('slice_version'))                           ! if -version option is present, display version text and exit
!  get values
   filename=sget('slice_f') ! get ‐f FILENAME
   filename=filename//' '//sget('slice_oo') ! get FILENAME
   filename=trim(adjustl(filename))
   write(*,*)'FILENAME=['//filename//']'
   device=sget('slice_d')   ! get ‐d DEVICE
   a=rget('slice_a')
   b=rget('slice_b')
   w=iget('slice_w')
   ixsize=iget('slice_x')
   iysize=iget('slice_y')
!-----------------------------------------------------------------------------------------------------------------------------------
   if(device.eq.'x11')then
      call prefposition(0,0)
      call prefsize(ixsize,iysize)
   else
      call prefsize(ixsize,iysize)
      call voutput(str(filename,'_',int(a),'x',int(b),'.',device,sep=''))
   endif
   call vinit(device)
   call vsetflush(.false.)
   call vflush()
   call linewidth(w)
!-----------------------------------------------------------------------------------------------------------------------------------
   if(FILENAME.ne.' ')then
      call slurp(FILENAME,text,lines=irows,length=length) ! allocate character array and copy file into it
      nchars=size(text)

      if(.not.allocated(text).or.irows.eq.0)then
         write(*,*)'*slice* failed to load file '//FILENAME
      else
         allocate(character(len=length) :: line)
         ! find number of values on first line and assume this is constant
         line(:)=''
         do i=1,nchars
            if(text(i).eq.NEW_LINE('A'))then
               exit
            endif
            line(i:i)=text(i)
         enddo
         !!write(*,*)'FIRST LINE=',trim(line)
         icols=size(s2vs(line))
         !!write(*,*)'COLUMNS=',icols
         allocate(array(irows,icols))

         array=0.0
         istart=1
         do j=1,irows
            k=0
            !!write(*,*)'ISTART=',istart
            line(:)=''
            do i=istart,nchars
               if(text(i).eq.NEW_LINE('A').or.i.eq.nchars)then
                  exit
               endif
               k=k+1
               !!write(*,*)'I=',i
               !!write(*,*)'K=',k
               !!write(*,*)'T=',text(i)
               line(k:k)=text(i)
            enddo
            istart=i+1
            array(j,:)=s2vs(line)
         enddo

         !!do i=1,size(array,dim=1)
         !!   write(*,'(*(g0,1x))')array(i,:)
         !!enddo

         deallocate(text)  ! release memory
         a=30.0
         b=40.0
         write(*,'(a,i0,a,i0,a,i0)')'ARRAY SIZE=',size(array), ' where ROWS=',irows, ' and COLS=',icols
         if(irows.gt.0.and.icols.gt.0)then
            irows2=irows
            icols2=icols
            call slices(array,irows,icols,irows2,icols2,a,b,filename)
            write(*,*)'last axis angles were ',a,b
         endif
      endif
   else
      DEMO: block
         integer,parameter  :: ix=35
         integer,parameter  :: iz=45
         real               :: surfdat(ix,iz) ! array of y values
         real,parameter     :: pi=3.141592654
         integer :: i
         integer :: j
         write(*,*)'WARNING: NO FILENAME GIVEN. Using demonstration data'
         write(*,*)'         enter "h" in the graphics area for help'
         ! fill some arrays with data we can plot
         do j=1,ix
            do i=1,iz
               surfdat(j,i)=cos(pi*real(j-1)/12.0)*cos(pi*real(i-1)/12.0)
            enddo
         enddo
         call slices(surfdat,ix,iz,ix,iz,a,b,'DEMO')
      endblock DEMO
   endif
   CALL VFLUSH()              ! flush graphics buffers
   CALL VEXIT()               ! close up plot package
!-----------------------------------------------------------------------------------------------------------------------------------
end program slice
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine slices(surfdat,kix,kiz,nx,nz,a0,b0,title)
   use M_draw
   use :: M_slices, only : dl_slices, dl_init, dl_symbol
!-----------------------------------------------------------------------------------------------------------------------------------
   real,intent(in)              :: SURFDAT(kix,kiz) ! array of y values
   integer,intent(in)           :: kix              ! x dimension of surfdat array
   integer,intent(in)           :: kiz
   integer,intent(in)           :: nx               ! x size of surface to plot
   integer,intent(in)           :: nz
   real,intent(in)              :: a0               ! initial angle of x axis from horizontal 0-80 degrees
   real,intent(in)              :: b0               ! initial angle of z axis from horizontal 5-80 degrees
   character(len=*),intent(in)  :: title
!-----------------------------------------------------------------------------------------------------------------------------------
   integer           :: ICOL(255)
   character(len=80) :: XT,YT,ZT                    ! axis titles
   character(len=1)  :: key                         ! pressed key in graphics window
!-----------------------------------------------------------------------------------------------------------------------------------
   a=a0                                             ! mutable copy of parameter A
   b=b0                                             ! mutable copy of parameter B
!-----------------------------------------------------------------------------------------------------------------------------------
!  initialize the color array
   DO I=1,255
      ICOL(I)=MOD(I,7)
   enddo
!-----------------------------------------------------------------------------------------------------------------------------------
   CALL DL_INIT(12.5,12.5,1.5,1.5,1.0)
!-----------------------------------------------------------------------------------------------------------------------------------
!     now plot 3-d surface using slices with axis
!-----------------------------------------------------------------------------------------------------------------------------------
! surface data
IX=KIX ! IX,IZ    (i): x and z dimensions of SURFDAT array
IZ=KIZ
! NX,NZ    (i): x and z sizes of surface to plot SURFDAT array
!-----------------------------------------------------------------------------------------------------------------------------------
! view angles
! A        (R): angle of x axis from horizontal 0-80 degrees
! B        (R): angle of z axis from horizontal 5-80 degrees
!               note: origin (1,1) is in lower-left corner
!                     x axis runs left to right on screen
!                     y axis runs up to down on screen
!                     z axis appears to run into the screen but is angled to the right
!-----------------------------------------------------------------------------------------------------------------------------------
! length of axis in window units
XH=6.0 ! xh,yh,zh (R): length of each axis
YH=3.8
ZH=5.0
!-----------------------------------------------------------------------------------------------------------------------------------
IFLAG=012
IFLAG=002
IFLAG=000
! iflag    (i): option flag
!               (1's digit) =2: use color array (need all parameters)
!                           =1: do not use color array
!               (10's digit)=0: Plot sides
!                           =1: Do not plot sides
!-----------------------------------------------------------------------------------------------------------------------------------
IAX=-11
IAX= 01
! SIGN:
! iax   (i): axis format control
!            < 0 : plot axes, use input scale factors dm and dx
!            = 0 : no axes plotted, optional parameters (xt...dx)
!                  not used, scaling computed from input array
!            > 0 : plot axes, use scaling computed from input array
!                  only axis parameters xt through smz accessed.
! DIGITS:
!  (1's digit)  = 1 : Plot actual max/min or input values for Y axis
!               = 2 : Plot smoothed values for Y axis
!  (10's digit) = 0 : Use default axis type
!               = 1 : Use input DL_AXISB-type axis parameters
!                      (nmx, nnx, mlx, tsx, ndx, etc.)
!-----------------------------------------------------------------------------------------------------------------------------------
!          (NOTE: the following optional parameters are accessed only if
!                 iax < 0 or mod(iflag,10)=1)
DM=-1.0   ! dm,dx (R): minimum and maximum values of SURFDAT array
DX=1.0
!-----------------------------------------------------------------------------------------------------------------------------------
! (NOTE: the following optional parameters are used if iax < 0 or mod(iflag,10)=1)
!        (see DL_AXISB for detailed description of axis parameters)
!-----------------------------------------------------------------------------------------------------------------------------------
! XAXIS:
XS=1.0                 ! xs,xe (R): starting and ending values displayed on x axis
XE=NX
!-----------------------
NMX=4                  ! nmx   (i): number of minor ticks between major ticks on x axis
NNX=0                  ! nnx   (i): highlight length of nnx-th minor tick on x axis
MLX=4                  ! mlx   (i): number of major tick marks on x axis
TSX=-0.15              ! tsx   (R): size of title and numbers on x axis
!                         < 0 auto exponent scaling (x10 to power) disabled
!                         > 0 auto exponent scaling (x10 to power) enabled
NDX=1                  ! (i): number of digits to right of decimal point on x axis
SMX=0.1                ! (R): major tick length on x axis
!-----------------------
XT='X Axis'            ! xt    (C): title of x axis (width)
NXT=len_trim(xt)       ! nxt   (i): number of characters in xt ;nxt = 0 : no axis plotted ; nxt > 0 : normal
!-----------------------------------------------------------------------------------------------------------------------------------
! YAXIS:
YS=-10.0               ! ys,ye (R): starting and ending values displayed on y axis
YE=10.0
!-----------------------
NMY=1                  ! (i): number of minor ticks between major ticks on y axis
NNY=0                  ! (i): highlight length of nny-th minor tick on y axis
MLY=3                  ! (i): number of major tick marks on y axis
TSY=-0.15              ! (R): size of title and numbers on y axis
                       !      < 0 auto exponent scaling (x10 to power) disabled
                       !      > 0 auto exponent scaling (x10 to power) enabled
NDY=1                  ! ndy   (i): number of digits to right of decimal point on y axis
SMY=0.10               ! smy   (R): major tick length on y axis
!-----------------------
YT='Y Axis'            ! yt    (C): title of y axis (width)
NYT=len_trim(yt)       ! nyt   (i): number of characters in xt ;nyt = 0 : no axis plotted ; nyt > 0 : normal
!-----------------------------------------------------------------------------------------------------------------------------------
! ZAXIS:
ZS=1.0
ZE=NZ                  ! zs,ze (R): starting and ending value displayed on z axis
!-----------------------
NMZ=0                  ! nmz   (i): number of minor ticks between major ticks on z axis
NNZ=2                  ! nnz   (i): highlight length of nnz-th minor tick on z axis
MLZ=5                  ! mlz   (i): number of major tick marks on z axis
TSZ=-0.15              ! tsz   (R): size of title and numbers on z axis
!                         < 0 auto exponent scaling (x10 to power) disabled
!                         > 0 auto exponent scaling (x10 to power) enabled
NDZ=0                  ! ndz   (i): number of digits to right of decimal point on z axis
SMZ=0.1                ! smz   (R): major tick length on z axis
!-----------------------
ZT='SLICES'            ! zt    (C): title of z axis (width)
NZT=len_trim(zt)       ! nzt   (i): number of characters in xt ;nzt = 0 : no axis plotted ; nzt > 0 : normal
!-----------------------------------------------------------------------------------------------------------------------------------
! (NOTE: color array accessed only if mod(iflag,10)=1)
! ic    (i): color list
!            ic(1) : color for axis lines
!            ic(2) : color for axis numbers
!            ic(3) : color for axis titles
!            ic(4) : color for axis exponents
!            ic(5) : color index for lower plot surface (return)
!            ic(6) : color index for upper plot surface (return)
!-----------------------------------------------------------------------------------------------------------------------------------
   call frontbuffer()

   A_OLD=A+1.0
   B_OLD=B+1.0

   DRAWPLOT: DO
      IBUF=BACKBUFFER()
      CALL VFLUSH()              ! flush graphics buffers
      CALL COLOR(0)
      CALL CLEAR()
      CALL COLOR(7)
      CALL VFLUSH()              ! flush graphics buffers
      if(A_OLD.ne.A.or.B_OLD.ne.B)then
         CALL dl_slices(SURFDAT,IX,IZ,NX,NZ,A,B,XH,YH,ZH,IFLAG,IAX, &
     &    XT,NXT,                                                   &
     &    XS,XE,NMX,NNX,MLX,TSX,NDX,SMX,                            &
     &    YT,NYT,                                                   &
     &    NMY,NNY,MLY,TSY,NDY,SMY,                                  &
     &    ZT,NZT,                                                   &
     &    ZS,ZE,NMZ,NNZ,MLZ,TSZ,NDZ,SMZ,                            &
     &    DM,DX,ICOL)
!        add a label after master routine call
         CALL LINEWIDTH(30)
         CALL COLOR(4)
         CALL DL_SYMBOL(0.0,0.0,0.25,trim(title),0.0,len_trim(title),-1)
         call swapbuffers()
         CALL VFLUSH()
      else
         !!write(*,*)'DUPLICATE ',a,b
      endif
      A_OLD=A
      B_OLD=B
      !IF(CHECKKEY().EQ.0)GOTO 111
      IVALUE=GETKEY()     ! wait till keypress is read in graphic window
      if(ivalue.eq.-1)then
         exit
      endif
      KEY=char(ivalue)
      SELECTCASE (KEY)
      CASE ('l')
         a=a-1
         a=max(0.0,a)
         !!write(*,*)'l: a=',a
      CASE ('r')
         a=a+1
         a=min(80.0,a)
         !!write(*,*)'r: a=',a
      CASE ('u')
         b=b+1.0
         b=min(80.0,b)
         !!write(*,*)'u: b=',b
      CASE ('d')
         b=b-1
         b=max(5.0,b)
         !!write(*,*)'d: b=',b
      CASE ('h')
         write(*,*)'================================================================================'
         write(*,*)' l,r -- change viewing angle of x axis from horizontal (0 - 85 degrees'
         write(*,*)' u,d -- change viewing angle of z axis from horizontal (0 - 90 degrees'
         write(*,*)'        note: origin (1,1) is in lower-left corner '
         write(*,*)'              x axis runs left to right on screen '
         write(*,*)'              y axis runs up to down on screen '
         write(*,*)'              z axis appears to run into the screen but '
         write(*,*)'                is angled to the right '
         write(*,*)' h   -- help'
         write(*,*)' q   -- quit '
         write(*,*)' '
         write(*,*)'================================================================================'
      CASE ('q')
         write(*,*)' x-axis angle=',A,' y-axis angle=',B
         exit
      CASE DEFAULT
         write(*,*)'================================================================================'
         write(*,*)'unknown key. ADE=',ivalue,merge(key,' ',ivalue>31.and.ivalue<127)
         write(*,*)'x-axis angle=',A,' from 0 to 80'
         write(*,*)'z-axis angle=',B,' from 5 to 80'
         write(*,*)'enter "h" for additional help'
         write(*,*)'================================================================================'
      END SELECT
      write(*,*)' x-axis angle=',A,' z-axis angle=',B
!-----------------------------------------------------------------------------------------------------------------------------------
      call frontbuffer()
   enddo DRAWPLOT
END subroutine slices
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
