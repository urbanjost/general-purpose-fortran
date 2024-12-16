










! ==================================================================================================================================
module M_pixel__slices
!
! This module intentionally duplicates graphics routines used in other high-level routines so that the DL_SLICES routine
! is essentially self-contained except for very low-level external graphics functions. This is so this routine can be
! developed and ported to other underlying graphics libraries.
!
!>
!!##NAME
!!    dl_slices(3f) - [M_pixel__slices] basic 3-d surface plotting routine
!!
!!##SYNOPSIS
!!
!!  subroutine dl_slices(d,ndx,ndz,nx,nz,a,b,xh,yh,zh,iflag,iax,
!!
!!                     & xt,nxt,xs,xe,nmx,nnx,mlx,tsx,ndx,smx,
!!                     & yt,nyt,      nmy,nny,mly,tsy,ndy,smy,
!!                     & zt,nzt,zs,ze,nmz,nnz,mlz,tsz,ndz,smz,
!!                     & dm,dx,ic,maxsize)
!!
!!##DESCRIPTION
!!    dl_slices is a simple 3-d surface plotting routine. A 3-d surface
!!    is plotted by plotting slices through the volume which are parallel
!!    to the x-y plane. The x,y values of the surface at the intersection
!!    of the slice plane and the fixed z value are plotted. Hidden lines
!!    are suppressed, giving the illusion of a 3 dimensional surface.
!!    The height of the plotted surface relative to the y axis value is
!!    calibrated to the x and z axes. No perspective is used. Options exist
!!    to vary the plotting angle and to plot axes.
!!
!!    The origin of the plot is in the lower-left corner. The x axis runs
!!    left to right along the plot bottom. The y axis is plotted as a
!!    vertical displacement offset by the z axis value. The z axis appears
!!    to point into the screen. This, with the hidden line removal, gives
!!    the illusion of depth.
!!
!!    dl_slices contains an internal working storage array dimensioned
!!    sufficiently large for most surfaces. However, for very complex
!!    surfaces, the working storage buffer length may be exceeded. In this
!!    case an error message is written to the terminal and the routine
!!    terminated.
!!
!!##OPTIONS
!!     d         (R): array of y values dimensioned d(ndx,ndz)
!!     ndx,ndz   (i): x and z dimensions of d array
!!     nx,nz     (i): x and z sizes of surface to plot d array
!!     a         (R): angle of x axis from horizontal 0-85 degrees
!!     b         (R): angle of z axis from horizontal 0-90 degrees
!!                    note: origin (1,1) is in lower-left corner
!!
!!                      x  axis runs left to right on screen
!!                      y  axis runs up to down on screen
!!                      z  axis appears to run into the screen but
!!                         is angled to the right
!!     xh,yh,zh  (R): length of each axis
!!     iflag     (i): option flag
!!                   (1's digit) =2: use color array (need all parameters)
!!                               =1: do not use color array
!!                   (10's digit)=0: Plot sides
!!                               =1: Do not plot sides
!!     iax   (i): axis format control
!!                < 0 : plot axes, use input scale factors dm and dx
!!                = 0 : no axes plotted, optional parameters (xt...dx)
!!                      not used, scaling computed from input array
!!                > 0 : plot axes, use scaling computed from input array
!!                      only axis parameters xt through smz accessed.
!!      (1's digit)  = 1 : Plot actual max/min or input values for Y axis
!!                   = 2 : Plot smoothed values for Y axis
!!      (10's digit) = 0 : Use default axis type
!!                   = 1 : Use input DL_AXISB-type axis parameters
!!                          (nmx, nnx, mlx, tsx, ndx, etc.)
!!
!!     (NOTE: the following optional parameters are used if iax < 0
!!            or mod(iflag,10)=1)
!!
!!     X-AXIS
!!        xt     (C): title of x axis (width)
!!        nxt    (i): number of characters in xt
!!                   = 0 : no axis plotted
!!                   > 0 : normal
!!        xs,xe  (R): starting and ending values displayed on x axis
!!
!!        (see DL_AXISB for detailed description of axis parameters)
!!
!!        nmx    (i): number of minor ticks between major ticks on x axis
!!        nnx    (i): highlight length of nnx-th minor tick on x axis
!!        mlx    (i): number of major tick marks on x axis
!!        tsx    (R): size of title and numbers on x axis
!!                   < 0 auto exponent scaling (x10 to power) disabled
!!                   > 0 auto exponent scaling (x10 to power) enabled
!!        ndx    (i): number of digits to right of decimal point on x axis
!!        smx    (R): major tick length on x axis
!!     Y-AXIS
!!        yt     (C): title of y axis (depth)
!!        nyt    (i): number of characters in yt
!!
!!                   = 0 : no y axis plotted
!!                   > 0 : normal
!!        nmy    (i): number of minor ticks between major ticks on y axis
!!        nny    (i): highlight length of nny-th minor tick on y axis
!!        mly    (i): number of major tick marks on y axis
!!        tsy    (R): size of title and numbers on y axis
!!
!!                    < 0 auto exponent scaling (x10 to power) disabled
!!                    > 0 auto exponent scaling (x10 to power) enabled
!!        ndy    (i): number of digits to right of decimal point on y axis
!!        smy    (R): major tick length on y axis
!!     Z-AXIS
!!        zt     (C): title of z axis (height)
!!        nzt    (i): number of characters in zt
!!
!!                   = 0 : no z axis plotted
!!                   > 0 : normal
!!        zs,ze  (R): starting and ending value displayed on z axis
!!        nmz    (i): number of minor ticks between major ticks on z axis
!!        nnz    (i): highlight length of nnz-th minor tick on z axis
!!        mlz    (i): number of major tick marks on z axis
!!        tsz    (R): size of title and numbers on z axis
!!
!!                   < 0 auto exponent scaling (x10 to power) disabled
!!                   > 0 auto exponent scaling (x10 to power) enabled
!!        ndz    (i): number of digits to right of decimal point on z axis
!!        smz    (R): major tick length on z axis
!!
!!     (NOTE: the following optional parameters are accessed only if
!!            iax < 0 or mod(iflag,10)=1)
!!
!!        dm,dx  (R): minimum and maximum values of d array
!!
!!        (NOTE: color array accessed only if mod(iflag,10)=1)
!!
!!        ic     (i): color list
!!
!!                    ic(1):  color for axis lines
!!                    ic(2):  color for axis numbers
!!                    ic(3):  color for axis titles
!!                    ic(4):  color for axis exponents
!!                    ic(5):  color index for lower plot surface (return)
!!                    ic(6):  color index for upper plot surface (return)
!!
!!        maxsize     optional maximum number of points.
!!
!!##EXAMPLES
!!
!!  Sample program:
!!
!!    program demo_dl_slices
!!    !     WRITTEN BY: DGL, LAST REVISED ON  5-JAN-1994 10:31:18.86
!!    !                 JSU,                 19-JUL-2005
!!     use M_pixel
!!     use M_pixel__writegif_animated, only : write_animated_gif
!!     use :: M_pixel__slices, only : dl_slices, dl_init, dl_symbol
!!     implicit none
!!       integer,parameter  :: ix=35
!!       integer,parameter  :: iz=45
!!       real               :: surfdat(ix,iz)              ! array of y values
!!       integer            :: movie(85+90+90,0:500,0:500) ! array of y values
!!       real,save          :: tpi=3.141592654
!!       integer            :: icol(255)
!!       character(len=80)  :: xt,yt,zt                    ! axis titles
!!       real :: a,b,dm,dx
!!       real :: smx,smy,smz
!!       real :: tsx,tsy,tsz
!!       real :: xe,xh,xs
!!       real :: ye,yh,ys
!!       real :: ze,zh,zs
!!    !
!!       integer :: i
!!       integer :: i10,i20,i40
!!       integer :: iax
!!       integer :: iflag
!!       integer :: iframe
!!       integer :: ii
!!       integer :: j
!!       integer :: mlx,mly,mlz
!!       integer :: ndx,ndy,ndz
!!       integer :: nmx,nmy,nmz
!!       integer :: nnx,nny,nnz
!!       integer :: nx,nxt
!!       integer :: nyt
!!       integer :: nz,nzt
!!    !
!!    ! (NOTE: color array accessed only if mod(iflag,10)=1)
!!    ! icol  (i): color list
!!    !            icol(1) : color for axis lines
!!    !            icol(2) : color for axis numbers
!!    !            icol(3) : color for axis titles
!!    !            icol(4) : color for axis exponents
!!    !            icol(5) : color index for lower plot surface (return)
!!    !            icol(6) : color index for upper plot surface (return)
!!    !     initialize the color array
!!          do i=1,255
!!             icol(i)=mod(i,7)
!!          enddo
!!    !     fill some arrays with data we can plot
!!          do j=1,ix
!!           do i=1,iz
!!            surfdat(j,i)=cos(tpi*real(j-1)/12.0)*cos(tpi*real(i-1)/12.0)
!!           enddo
!!          enddo
!!    !
!!          call prefsize(501,501)
!!          call vinit()
!!    !
!!          call dl_init(12.5,12.5,1.5,1.5,1.0)   ! set up plotting surface scale
!!          call linewidth(3)
!!          call color(4)
!!    !     now plot 3-d surface using slices with axis
!!          nx=ix
!!          nz=iz
!!    !
!!    ! length of axis in window units
!!    xh=6.0 ! xh,yh,zh (R): length of each axis
!!    yh=3.8
!!    zh=5.0
!!    !
!!    iflag=012
!!    iflag=000
!!    iflag=002
!!    ! iflag    (i): option flag
!!    !               (1's digit) =2: use color array (need all parameters)
!!    !                           =1: do not use color array
!!    !               (10's digit)=0: Plot sides
!!    !                           =1: Do not plot sides
!!    !
!!    iax= 01
!!    iax=-11
!!    ! SIGN:
!!    ! iax   (i): axis format control
!!    !            < 0 : plot axes, use input scale factors dm and dx
!!    !            = 0 : no axes plotted, optional parameters (xt...dx)
!!    !                  not used, scaling computed from input array
!!    !            > 0 : plot axes, use scaling computed from input array
!!    !                  only axis parameters xt through smz accessed.
!!    ! DIGITS:
!!    !  (1's digit)  = 1 : Plot actual max/min or input values for Y axis
!!    !               = 2 : Plot smoothed values for Y axis
!!    !  (10's digit) = 0 : Use default axis type
!!    !               = 1 : Use input DL_AXISB-type axis parameters
!!    !                      (nmx, nnx, mlx, tsx, ndx, etc.)
!!    !
!!    ! NOTE: the following optional parameters are used if iax < 0 or
!!    ! mod(iflag,10)=1
!!    !
!!    ! (see DL_AXISB for detailed description of axis parameters)
!!    !
!!    ! XAXIS:
!!          xs=-10.0   ! xs,xe (R): starting and ending values displayed on x axis
!!          xe=10.0
!!    !-----------------------
!!          nmx=4      ! (i): number of minor ticks between major ticks on x axis
!!          nnx=0      ! (i): highlight length of nnx-th minor tick on x axis
!!          mlx=4      ! (i): number of major tick marks on x axis
!!          tsx=-0.15  ! (R): size of title and numbers on x axis
!!                     !      < 0 auto exponent scaling (x10 to power) disabled
!!                     !      > 0 auto exponent scaling (x10 to power) enabled
!!          ndx=1      ! (i): number of digits to right of decimal point on x axis
!!          smx=0.1    ! (R): major tick length on x axis
!!    !-----------------------
!!          xt='dl_slices X TITLE' ! (C): title of x axis (width)
!!          nxt=len_trim(xt)       ! (i): number of characters in xt ;
!!                                 ! nxt = 0 : no axis plotted ; nxt > 0 : normal
!!    !
!!    ! YAXIS:
!!          ys=-10.0               ! ys,ye (R): starting and ending values
!!                                 ! displayed on y axis
!!          ye=10.0
!!    !-----------------------
!!          nmy=1      ! (i): number of minor ticks between major ticks on y axis
!!          nny=0      ! (i): highlight length of nny-th minor tick on y axis
!!          mly=3      ! (i): number of major tick marks on y axis
!!          tsy=-0.15  ! (R): size of title and numbers on y axis
!!                     !      < 0 auto exponent scaling (x10 to power) disabled
!!                     !      > 0 auto exponent scaling (x10 to power) enabled
!!          ndy=1      ! ndy   (i): number of digits to right of decimal point
!!                     !      on y axis
!!          smy=0.10   ! smy  (R): major tick length on y axis
!!    !-----------------------
!!          yt='dl_slices Y TITLE' ! (C): title of y axis (width)
!!          nyt=len_trim(yt)       ! (i): number of characters in xt ;
!!                                 ! nyt = 0 : no axis plotted ; nyt > 0 : normal
!!    !
!!    ! ZAXIS:
!!          zs=1.0
!!          ze=1.0      ! (R): starting and ending value displayed on z axis
!!    !-----------------------
!!          nmz=3       ! (i): number of minor ticks between major ticks on z axis
!!          nnz=2       ! (i): highlight length of nnz-th minor tick on z axis
!!          mlz=2       ! (i): number of major tick marks on z axis
!!          tsz=-0.15   ! (R): size of title and numbers on z axis
!!                      !      < 0 auto exponent scaling (x10 to power) disabled
!!                      !      > 0 auto exponent scaling (x10 to power) enabled
!!          ndz=1       ! (i): number of digits to right of decimal point on z axis
!!          smz=0.1     ! (R): major tick length on z axis
!!    !-----------------------
!!          zt='SLICE'         ! (C): title of z axis (width)
!!          nzt=len_trim(zt)   ! (i): number of characters in xt ;nzt = 0 :
!!                             !      no axis plotted ; nzt > 0 : normal
!!    !
!!    !          (NOTE: the following optional parameters are accessed only if
!!    !                 iax < 0 or mod(iflag,10)=1)
!!          dx=1.0
!!          dm=-1.0   ! dm,dx (R): minimum and maximum values of SURFDAT array
!!    ! view angles
!!    ! A    (R): angle of x axis from horizontal 0-80 degrees
!!    ! B    (R): angle of z axis from horizontal 5-80 degrees
!!    !           note: origin (1,1) is in lower-left corner
!!    !                 x axis runs left to right on screen
!!    !                 y axis runs up to down on screen
!!    !                 z axis appears to run into the screen
!!    !                   but is angled to the right
!!          iframe=1
!!          b=15.0
!!          do i10=1,85   ! Animate cycling thru angle A
!!           a=i10
!!           call color(7)
!!           call clear()
!!           call color(0)
!!           call dl_slices(surfdat,ix,iz,nx,nz,a,b,xh,yh,zh,iflag,iax,  &
!!         & xt,nxt,  &
!!         & xs,xe,nmx,nnx,mlx,tsx,ndx,smx,  &
!!         & yt,nyt,  &
!!         & nmy,nny,mly,tsy,ndy,smy,  &
!!         & zt,nzt,  &
!!         & zs,ze,nmz,nnz,mlz,tsz,ndz,smz,dm,dx,icol)
!!    !
!!    !      add a label after master routine call
!!           call color(1)
!!           call linewidth(1)
!!           call dl_symbol(0.0,0.0,0.25,'VAX3DX',0.0,6,-1)
!!           movie(iframe,:,:)=p_pixel(:,:)
!!           iframe=iframe+1
!!          enddo
!!    !
!!          a=25
!!          do i20=1,90   ! Animate cycling thru angle B
!!           b=i20
!!           call color(7)
!!           call clear()
!!           call color(0)
!!           call dl_slices(surfdat,ix,iz,nx,nz,a,b,xh,yh,zh,iflag,iax,  &
!!         & xt,nxt,  &
!!         & xs,xe,nmx,nnx,mlx,tsx,ndx,smx,  &
!!         & yt,nyt,  &
!!         & nmy,nny,mly,tsy,ndy,smy,  &
!!         & zt,nzt,  &
!!         & zs,ze,nmz,nnz,mlz,tsz,ndz,smz,dm,dx,icol)
!!           movie(iframe,:,:)=p_pixel(:,:)
!!           iframe=iframe+1
!!          enddo
!!    !
!!    iax=01
!!    iflag=012
!!    ii=1
!!    !
!!    do i40=1,90*ii  ! Animate cycling thru angles A and B
!!       a=real(i40)/ii/2.0 ! should get warning when this exceeds 85
!!       b=real(i40)/ii/2.0
!!       call color(7)
!!       call clear()
!!       call color(0)
!!       call dl_slices(surfdat,ix,iz,nx,nz,a,b,xh,yh,zh,iflag,iax,  &
!!     & xt,nxt,  &
!!     & xs,xe,nmx,nnx,mlx,tsx,ndx,smx,  &
!!     & yt,nyt,  &
!!     & nmy,nny,mly,tsy,ndy,smy,  &
!!     & zt,nzt,  &
!!     & zs,ze,nmz,nnz,mlz,tsz,ndz,smz,dm,dx,icol)
!!       movie(iframe,:,:)=p_pixel(:,:)
!!       iframe=iframe+1
!!    enddo
!!    !
!!    call vexit()    ! close up plot package
!!    call write_animated_gif('dl_slices.3m_pixel.gif',movie,p_colormap,delay=5)
!!    !call execute_system_command('display dl_slices.3m_pixel.gif')
!!    end program demo_dl_slices
!!    !
implicit none
private
!-----------------------------------------------------------------------------------------------------------------------------------
public  :: dl_init           ! (XMAX0,YMAX0,VPX,VPY,ZOM)
public  :: dl_symbol         ! (X,Y,S,T,A,NN,IS)
public  :: dl_slices
                             ! DL_SLICES(A,INX,INZ,NX,NZ,ALPHA,BETA,XH,YH,ZH,IFLAG,IAXIS,
                             !           XT,NXT,XASTART,XAEND,NMX,NNX,MLX,TSX,NDX,SMX,
                             !           YT,NYT,              NMY,NNY,MLY,TSY,NDY,SMY,
                             !           ZT,NZT,ZASTART,ZAEND,NMZ,NNZ,MLZ,TSZ,NDZ,SMZ,
                             !           AMININ,AMAXIN,ICOL)
!-----------------------------------------------------------------------------------------------------------------------------------
private :: dl_vxpt3d         ! (X,Y,AVAL,IX,IZ,NX)
private :: dl_intersect      ! (FLAG,X,Y,AX1,AY1,AX2,AY2,BX1,BY1,BX2,BY2,A)
private :: dl_clipit         ! (IVTB,XV2,YV2,AV1,AV2,XM,YM,XX,YX)
private :: dl_trs            ! (XIN,YIN,XCON,YCON)
private :: dl_inbox          ! (X,Y,X_BOTTOM_LEFT,Y_BOTTOM_LEFT,X_top_right,Y_top_right)
!------------------------
private :: dl_axisb          ! (X0,Y0,A0,N0,S0,T0,C0,D0,NM,NN,ML,TS,ND,SM,ICOL)
private :: dl_axisa          ! (X0,Y0,A0,N0,S0,T0,C0,D0,NM,ML,ICOL)
!------------------------
private :: dl_number         ! (X,Y,HGHT,Z,T,F0,IPF)
private :: dl_range          ! (X,S,N,K,IX,XMIN,DX)
!------------------------
private :: dl_translate      ! (XA,YA)
private :: dl_viewport       ! (XMIN,XMAX,YMIN,YMAX)
private :: dl_color          ! (IC)
private :: dl_width          ! (IC)
private :: dl_draw           ! (XA,YA)
private :: dl_move           ! (XA,YA)
private :: dl_plot           ! (XPLOT0,YPLOT0,ISELECT0)
!-----------------------------------------------------------------------------------------------------------------------------------
! plot coordinate system
real,save    :: translatexq                ! TRANSLATEXQ  SCALED ROTATED ORIGIN X VALUE
real,save    :: translateyq                ! TRANSLATEYQ  SCALED ROTATED ORIGIN Y VALUE
real,save    :: xminq,yminq,xmaxq,ymaxq
real,save    :: scaleq                     ! ZOOM SCALE FACTOR
real,save    :: xlastscaleq                ! LAST SCALED, SHIFTED X VALUE
real,save    :: ylastscaleq                ! LAST SCALED, SHIFTED Y VALUE
real,save    :: angleq                     ! PLOTTING ANGLE
real,save    :: viewportq(4)               ! VIEWPORTQ(4) VIEWPORT PARAMETERS
!-----------------------------------------------------------------------------------------------------------------------------------
real,save    :: xscaleq,yscaleq,zscaleq,aminq,alphq,betq
!-----------------------------------------------------------------------------------------------------------------------------------
contains
! ==================================================================================================================================
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine dl_vxpt3d(x,y,aval,ix,iz,nx)
!
!     CREATED BY DAVID LONG    AUG, 1982 AT JPL; revised 1993
!     SUBPROGRAM OF DL_slices
!
!     ROUTINE TO DETERMINE X,Y VALUE OF A POINT ON 3-D SURFACE
!     FOR dl_slices
!
implicit none
real :: x
real :: y
real :: aval
integer :: ix
integer :: iz
integer :: nx
   x=xscaleq*float(ix-1)*cos(alphq)+float(iz-1)*cos(betq)*zscaleq
   y=yscaleq*(aval-aminq)+float(nx-ix+1)*sin(alphq)*xscaleq+float(iz-1)*sin(betq)*zscaleq
end subroutine dl_vxpt3d
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine dl_intersect(flag,x,y,ax1,ay1,ax2,ay2,bx1,by1,bx2,by2,a)
!
!     CREATED BY D. LONG     AUG, 1983 AT JPL; revised 19931208
!     SUBPROGRAM OF dl_slices
!
!     Determine if two segments intersect and the point of intersection
!     if starting points of segments are the same segments are not
!     considered to be intersecting
!
implicit none
real :: ax1
real :: ax2
real :: ay1
real :: ay2
real :: bx1
real :: bx2
real :: by1
real :: by2
real :: ca
real :: cb
real :: da
real :: da1
real :: da2
real :: db
real :: db1
real :: db2
real :: denom
real :: x
real :: y
real ma,mb
logical flag,vert,a
!
      vert=.false.
      flag=.false.
!
      if (ax1.eq.bx1.and.ay1.eq.by1) return !SAME START POINT
      if (ax2.eq.bx2.and.ay2.eq.by2) then !SAME END POINT
         flag=.true.
         x=ax2
         y=ay2
         return
      endif
!
      denom=ax1-ax2
      if (denom.eq.0.0) then  !VERTICAL LINE
         ma=1.e10
         vert=.true.
      else
         ma=(ay1-ay2)/denom !SLOPE OF SEGMENT A
      endif
      denom=bx1-bx2
      if (denom.eq.0.0) then  !VERTICAL LINE
         mb=1.e10
         vert=.true.
      else
         mb=(by1-by2)/denom !SLOPE OF SEGMENT B
      endif
      if (ma.eq.mb) return  !PARALLEL
      ca=ay1-ma*ax1
      cb=by1-mb*bx1
      if (vert) then
         if (ma.eq.1.e10) then
            x=ax1
            y=x*mb+cb
         endif
         if (mb.eq.1.e10) then
            x=bx1
            y=x*ma+ca
         endif
      else
         x=(ca-cb)/(mb-ma)
         y=ma*x+ca
      endif
!     INTERSECTION OF LINES THROUGH POINTS IS AT X,Y
      da=(ax1-ax2)**2+(ay1-ay2)**2
      da1=(ax1-x)**2+(ay1-y)**2
      if (da1.gt.da) return
         da2=(ax2-x)**2+(ay2-y)**2
         if (da2.gt.da) return
            db=(bx1-bx2)**2+(by1-by2)**2
            db1=(bx1-x)**2+(by1-y)**2
            if (db1.gt.db) return
               db2=(bx2-x)**2+(by2-y)**2
               if (db2.gt.db) return
                  if (ma.gt.mb) then
                     a=.true.
                  else
                     a=.false.
                  endif
                  flag=.true.
                  return
end subroutine dl_intersect
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!*==dl_axisb.f90 processed by SPAG 8.01RF 02:04 13 Dec 2024
SUBROUTINE dl_axisb(X0,Y0,A0,N0,S0,T0,C0,D0,Nm,Nn,Ml,Ts,Nd,Sm,Icol)
!
!     WRITTEN BY D.LONG   17-OCT-1983  AT JPL
!     revised 19870813 DGL + IMPROVED APPEARANCE OF EXPONENT, ADDED EXPONENT COLOR
!
!     THIS VERSION OF AXIS INCLUDES MINOR TICKS ON AXIS AND A MORE
!     FLEXIBLE METHOD OF SPECIFYING AXIS PARAMETERS.
!
!     X0  (R)  X COORDINATE OF START OF AXIS
!     Y0  (R)  Y COORDINATE OF START OF AXIS
!     A0  (C)  CHARACTER STRING TO DESCRIBE AXIS
!     N0  (I)  NUMBER OF CHARACTERS IN STRING
!        - ON CLOCKWISE SIDE OF AXIS (NORMAL FOR X)
!        + ON COUNTER CLOCKWISE SIDE OF AXIS (NORMAL FOR Y)
!        HUNDREDS DIGIT = 1 NO LABELING OF AXIS--TICKS AND LINE ONLY
!        THOUSANDS DIGIT = 1 HORIZONTAL NUMBERS IN Y AXIS LABEL
!        10 THOUSANDS DIGIT=1 USE NM,ML,ETC. PARAMETERS
!        100 THOUSANDS DIGIT=1 USE COLOR PARAMETERS
!     S0  (R)  LENGTH OF AXIS
!        < 0   TICKS PLACED ON OPPOSITE SIDE OF AXIS LABEL
!        = 0   NO ACTION
!        > 0   NORMAL
!     T0  (R)  ANGLE OF AXIS TO X AXIS OF PAPER
!        0.0 FOR X-AXIS
!        90.0 FOR Y-AXIS
!     C0  (R)  COORDINATE OF MINIMUM TICK ON AXIS
!     D0  (R)  SCALING DISTANCE BETWEEN  1" TICKS
!        NOTE:  THE FOLLOWING ARE ACCESSED ONLY IF ABS(NO)>10000
!     NM  (I)  NUMBER OF MINOR AXIS TICKS BETWEEN MAJOR TICKS (DEFAULT=0)
!     NN  (I)  NNth MINOR TICK FROM MAJOR IS HIGHLIGHTED
!        (DEFAULT=0 INDICATES NO MINOR TICKS HIGHLIGHTED)
!     ML  (I)  NUMBER OF MAJOR AXIS TICKS (DEFAULT= 1 TICK/INCH)
!        < 0 THEN USE FOLLOWING VARIABLES
!     TS  (R)  CHARACTER SIZE OF TITLE AND NUMBER (DEFAULT=.15)
!        < 0 THEN DO NOT AUTO SCALE BY (x10 TO POWER)
!     ND  (I)  NUMBER OF DIGITS TO RIGHT OF DECIMAL POINTS (DEFAULT=1)
!     SM  (R)  MAJOR TICK LENGTH (DEFAULT= .1) (MINOR TICK=1/2 MAJOR TICK)
!     ICOL (I) COLOR ARRAY (ACCESSED ONLY IF ABS(NO)>100000
!         ICOL(1) AXIS COLOR
!         ICOL(2) NUMBER COLOR
!         ICOL(3) LABEL COLOR
!         ICOL(4) EXPONENT COLOR
!        PEN COLOR ON RETURN DEPENDS ON LAST ITEM PLOTTED
!        IN THE SEQUENCE INDICATED
   IMPLICIT NONE
   REAL :: b1
   REAL :: b2
   REAL :: b3
   REAL :: b4
   REAL :: b6
   REAL :: b7
   REAL :: b8
   REAL :: C0
   REAL :: c1
   REAL :: c2
   REAL :: cs
   REAL :: D0
   REAL :: d1
   REAL :: e1
   REAL :: hmt
   REAL :: hor
   INTEGER :: i
   INTEGER :: ic
   INTEGER :: ic2
   INTEGER :: k
   INTEGER :: Ml
   INTEGER :: N0
   INTEGER :: n1
   INTEGER :: n2
   INTEGER :: nc1
   INTEGER :: Nd
   INTEGER :: ndd
   INTEGER :: nddd
   INTEGER :: Nm
   INTEGER :: nm1
   INTEGER :: Nn
   REAL :: S0
   REAL :: s1
   REAL :: Sm
   REAL :: T0
   REAL :: t1
   REAL :: t2
   REAL :: t3
   REAL :: t4
   REAL :: t5
   REAL :: t6
   REAL :: Ts
   REAL :: X0
   REAL :: x1
   REAL :: x2
   REAL :: x3
   REAL :: xl
   REAL :: xm
   REAL :: Y0
   REAL :: y1
   REAL :: y2
   REAL :: y3
   CHARACTER*(*) A0
   INTEGER Icol(4)
   LOGICAL vert , ticks , color , scale
!
   cs = .15          ! CHARACTER SIZE
   IF ( S0/=0.0 ) THEN        ! ZERO LENGTH AXIS
      vert = .FALSE.    ! NO VERTICAL NUMBERS ON HORIZONTAL AXIS
      ticks = .TRUE.    ! PUT ON TICKS
      scale = .TRUE.    ! (x10 TO POWER SCALING)
      hor = T0
      ndd = 1     ! NUMBER OF DIGITS TO RIGHT OF DECIMAL
      t5 = 0.1       ! TICK LENGTH
      b7 = t5 + .08  ! NUMBER DISTANCE FROM AXIS
      b6 = b7
      b8 = 0.0
      nm1 = 0        ! NUMBER MINOR TICKS
      n2 = (abs(S0)+0.5) ! NUMBER OF MAJOR TICKS
      s1 = n2
      xl = 1.        ! INCREMENT BETWEEN MAJOR TICKS
      n1 = iabs(N0)
      color = .FALSE.
      IF ( n1>100000 ) THEN
         n1 = mod(n1,100000)
                           ! USE COLOR ARRAY
         color = .TRUE.
      ENDIF
      IF ( n1>10000 ) THEN
         n1 = mod(n1,10000)
         n2 = iabs(Ml)  ! NUMBER MAJOR TICKS
         s1 = abs(S0)
         IF ( n2==0 ) n2 = 1
         xl = abs(S0)/float(n2)
                              ! SPACING MAJOR TICKS
         nm1 = iabs(Nm) + 1
                         ! NUMBER MINOR TICKS
         IF ( Ml<0 ) THEN
            cs = abs(Ts)
                        ! DIFFERENT TITLE SIZE
            IF ( cs==0. ) cs = .15
            ndd = iabs(Nd)
            IF ( Ts<0 ) scale = .FALSE.
                                       ! DO NOT SCALE
            t5 = abs(Sm)  ! NEW TICK LENGTH
            IF ( t5==0. ) t5 = .1
         ENDIF
      ENDIF
      IF ( n1>1000 ) THEN
         n1 = mod(n1,1000)
                          ! VERTICAL NUMBERS ON HORIZONTAL AXIS
         vert = .TRUE.
         hor = 0.0
         b4 = (abs(t5)*(1.+sign(1.,S0))/2.+.1)*sign(1.,float(N0))
         b6 = .49*cs
      ENDIF
      IF ( n1>100 ) THEN
         n1 = mod(n1,100)
                         ! NO TICKS
         ticks = .FALSE.
      ENDIF
      IF ( N0<0 ) THEN
         b3 = (-cs)*(3.+ndd)
                           ! CLOCKWISE LABELING
         b4 = -t5 - cs
         t2 = T0
         t5 = -t5
      ELSE
         b3 = cs*(2.8+ndd)
                         ! COUNTER-CLOCKWISE LABELING
         b4 = cs + t5
         t2 = T0
      ENDIF
      t5 = t5*sign(1.,S0)
      t1 = T0*0.017453294
      t3 = cos(t1)
      t4 = sin(t1)
!
      t6 = t5*t3
      t5 = t5*t4
      x1 = X0
      y1 = Y0
      IF ( color ) CALL dl_color(Icol(1))
                                        ! COLOR
      DO i = 1 , n2   ! MAJOR TICKS
         IF ( nm1/=0 ) THEN
            xm = xl/float(nm1)
                          ! SPACING MINOR TICKS
            DO k = 1 , nm1
                      ! DO MINOR TICKS
               x2 = x1 + t3*float(k-1)*xm
               y2 = y1 + t4*float(k-1)*xm
               IF ( k-1==Nn .AND. Nn/=0 ) THEN
                  hmt = 0.8
               ELSE
                  hmt = 0.5
               ENDIF
               x3 = x2 - t5*hmt
               y3 = y2 + t6*hmt
               CALL dl_move(x2,y2)
            ENDDO
            CALL dl_draw(x3,y3)
         ENDIF
         x2 = x1 - t5
         y2 = y1 + t6
         CALL dl_move(x2,y2)
         CALL dl_draw(x1,y1)
         x1 = x1 + t3*xl
         y1 = y1 + t4*xl
         IF ( T0==90.0 ) x1 = X0
      ENDDO
      CALL dl_draw(x1,y1)
      x2 = x1 - t5
      y2 = y1 + t6
      CALL dl_draw(x2,y2)   ! FINISH LAST MAJOR TICK
!     CHECK FOR EXPONENT VALUE
      d1 = D0           ! SCALING FACTOR
      c1 = C0 + d1*s1    ! STARTING VALUE
      e1 = 0.0           ! EXPONENT
      IF ( scale ) THEN
         IF ( d1/=0.0 ) THEN
            DO WHILE ( abs(d1)>=10.0 )
               d1 = d1*0.1
               c1 = c1*0.1
               e1 = e1 + 1.0
            ENDDO
            DO WHILE ( abs(d1)<0.5 )
               d1 = d1*10.0
               c1 = c1*10.0
               e1 = e1 - 1.0
            ENDDO
         ENDIF
      ENDIF
      IF ( .NOT.ticks ) THEN
         IF ( color ) CALL dl_color(Icol(3))
                                           ! COLOR
         RETURN
      ENDIF
      IF ( vert ) THEN
         c2 = c1 - n2*d1
                        ! MAKE SPACE FOR VERTICAL NUMBERS
         ic = 1      ! ON HORIZONTAL AXIS
         IF ( abs(c2)>=1.0 ) ic = ifix(alog10(abs(c2)))
         ic2 = 1
         IF ( abs(c1)>=1.0 ) ic2 = ifix(alog10(abs(c1)))
         nc1 = max(ic,ic2) + 2
         IF ( c2<0.0 .OR. C0<0.0 ) nc1 = nc1 + 1
         IF ( N0>0.0 ) b4 = b4 + float(nc1+ndd)*cs
         b3 = 0.0
         b8 = (.25+abs(t5)*(sign(1.,S0)+1.)/2.+float(nc1+ndd)*cs)*sign(1.,float(N0))
      ENDIF
      x2 = x1 - b4*t4 - b7*t3
                         ! LOCATE CENTER NUMBER LABELS
      y2 = y1 + b4*t3 - b6*t4
      n2 = n2 + 1
      IF ( color ) CALL dl_color(Icol(2))
                                        ! COLOR
      nddd = ndd
      IF ( ndd==0 ) nddd = -1
      DO i = 1 , n2  ! LABEL MAJOR TICKS
         CALL dl_number(x2,y2,cs,c1,hor,float(nddd)/100.,-1)
         c1 = c1 - d1*s1/float(n2-1)
         x2 = x2 - t3*xl
         y2 = y2 - t4*xl
      ENDDO
      IF ( n1/=0 ) THEN
         c2 = 0.0
         y2 = 0.0
         CALL dl_symbol(c2,y2,cs,A0,0.,n1,-3)
         b1 = 0.5*(abs(S0)-c2)
                              ! CENTER TITLE
         IF ( e1/=0.0 ) b1 = b1 - cs*3.
                                    ! PUT ON EXPONENT SPACE
         x2 = X0 + b1*t3 - b3*t4 - b8*t4
         y2 = Y0 + b1*t4 + b3*t3
         IF ( color ) CALL dl_color(Icol(3))
                                           ! COLOR
         CALL dl_symbol(x2,y2,cs,A0,t2,n1,-1)
      ELSE
         c2 = 0.0
         b1 = 0.5*abs(S0)
         x2 = X0 + b1*t3 - b3*t4 - b8*t4
         y2 = Y0 + b1*t4 + b3*t3
      ENDIF
      IF ( e1/=0.0 ) THEN      ! NO EXPONENT
         IF ( color ) CALL dl_color(Icol(4))
                                        ! COLOR
         c2 = c2 + cs
         x2 = x2 + c2*t3
         y2 = y2 + c2*t4
         CALL dl_symbol(x2,y2,cs,'(X10',t2,4,-1)
         x2 = x2 + 3.75*cs*t3 - cs*t4*0.4
         y2 = y2 + 3.75*cs*t4 + cs*t3*0.4
         CALL dl_number(x2,y2,cs,e1,t2,0.0,-1)
         b2 = 0.8 + aint(alog10(abs(e1)))
         IF ( e1<0.0 ) b2 = b2 + 1
         x2 = x2 + b2*cs*t3 + cs*t4*0.4
         y2 = y2 + b2*cs*t4 - cs*t3*0.4
         CALL dl_symbol(x2,y2,cs,')',t2,1,-1)
      ENDIF
   ENDIF
END SUBROUTINE dl_axisb
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!*==dl_range.f90 processed by SPAG 8.01RF 02:00 13 Dec 2024
SUBROUTINE dl_range(X,S,N,K,Ix,Xmin,Dx)
!
!     CREATES SMOOTHED LINEAR SCALE FACTORS FROM INPUT DATA
!
!     X  (R) ARRAY OF DATA TO BE SCANNED FOR MAXIMUM AND MINIMUM
!            VALUES.
!     S  (R) LENGTH OVER WHICH THIS DATA IS TO BE PLOTTED.
!     N  (I) NUMBER OF DATA POINTS IN THE ARRAY X.
!     K  (I) REPEAT CYCLE OF MIXED ARRAY(NORMALLY 1).
!     IX (I) FIRST RELEVANT DATA POINT IN X
!     XMIN  (R) SMOOTHED MINIMUM AFTER CALL
!     DX (R) SMOOTHED INCREMENT AFTER CALL
!
!     TO USE SMOOTHED VALUES: XPLOTTED=(XVALUE-XM)/DX
IMPLICIT NONE
REAL X(*) , q(6)
REAL :: Dx
INTEGER :: i
INTEGER :: idx
INTEGER :: Ix
INTEGER :: K
INTEGER :: N
INTEGER :: np
REAL :: S
REAL :: si
REAL :: sj
REAL :: xi
REAL :: xmax
REAL :: Xmin
REAL :: xmm
DATA q/1.0 , 2.0 , 4.0 , 5.0 , 8.0 , 10.0/
   np = N*K
   xmax = X(1)
   Xmin = xmax
   DO i = Ix , np , K
      xi = X(i)
      xmax = amax1(xmax,xi)
      Xmin = amin1(Xmin,xi)
   ENDDO
   xmm = Xmin
   IF ( S>0.0 ) THEN
      Dx = (xmax-Xmin)/S
      IF ( Dx>0.0 ) THEN
         sj = 0.0
         IF ( Dx<1.0 ) sj = -1.0
         idx = alog10(Dx) + sj
         Dx = Dx/(10.0**idx)
         SPAG_Loop_1_1: DO i = 1 , 6
            xi = q(i)
            IF ( xi>=Dx ) EXIT SPAG_Loop_1_1
         ENDDO SPAG_Loop_1_1
         Dx = xi*(10.0**idx)
         si = 1.0
         sj = 0.0
         IF ( Xmin<0 ) THEN
            si = -1.0
            sj = -0.99999
            Xmin = -Xmin
         ELSEIF ( Xmin==0 ) THEN
            CALL spag_block_1
            RETURN
         ENDIF
         idx = alog10(Xmin) + sj
         Xmin = Xmin/(10.0**idx)
         Xmin = Xmin - sj
         Xmin = ifix(Xmin)*si*(10.0**idx)
         CALL spag_block_1
         RETURN
      ENDIF
   ENDIF
   Dx = 1.0
   Xmin = Xmin - 0.5
   CALL spag_block_1
contains
subroutine spag_block_1
!
!     BEFORE EXIT, CHECK TO BE SURE THAT DATA IS CONTAINED WITHIN
!     THE LIMITS XMIN AND XMIN+DX*S. IF NOT, RESET DX
!
      if ( xmm<xmin ) xmin = xmm
      if ( xmax>xmin+dx*s ) then
         if ( s>0.0 ) dx = (xmax-xmin)/s
         if ( dx<=0.0 ) dx = 1.0
      endif
end subroutine spag_block_1
END SUBROUTINE dl_range
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine dl_color(ic)
use M_pixel
implicit none
integer :: ic
   if (ic.ge.0) then
      call color(ic)           ! change color
   endif
end subroutine dl_color
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine dl_draw(xa,ya)
implicit none
real :: xa
real :: ya
   call dl_plot(xa,ya,2)
end subroutine dl_draw
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine dl_move(xa,ya)
implicit none
real :: xa
real :: ya
   call dl_plot(xa,ya,3)
end subroutine dl_move
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine dl_translate(xa,ya)
implicit none
real :: xa
real :: ya
   call dl_plot(xa,ya,-3)
end subroutine dl_translate
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine dl_viewport(xmin,xmax,ymin,ymax)
implicit none
real,intent(in) :: xmax
real,intent(in) :: xmin
real,intent(in) :: ymax
real,intent(in) :: ymin
real :: xconmax
real :: xconmin
real :: yconmax
real :: yconmin

   ! note that new viewport is in terms of current coordinate system
   ! SET upper right CORNER OF VIEW PORT
   call dl_trs(xmax,ymax,xconmax,yconmax) ! convert call numbers to current plot coordinate system
   viewportq(3)=xconmax
   viewportq(4)=yconmax
   ! SET lower left CORNER OF VIEW PORT
   call dl_trs(xmin,ymin,xconmin,yconmin) ! convert call numbers to current plot coordinate system
   viewportq(1)=xconmin
   viewportq(2)=yconmin
end subroutine dl_viewport
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine dl_width(ic)
use M_pixel
implicit none
integer :: ic
   call linewidth(ic)
end subroutine dl_width
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine dl_trs(xin,yin,xcon,ycon)
implicit none
real :: tang
real :: xcon
real :: xin
real :: ycon
real :: yin
!     convert call numbers to current plot coordinate system

      tang=angleq*.0174532              ! convert degrees to radians

      xcon=xin*cos(tang)-yin*sin(tang)  ! rotate coordinates
      ycon=xin*sin(tang)+yin*cos(tang)  ! rotate coordinates

      xcon=scaleq*xcon+translatexq      ! scale and translate
      ycon=scaleq*ycon+translateyq      ! scale and translate

end subroutine dl_trs
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine dl_init(xmax0,ymax0,vpx,vpy,zom)
!
!     ROUTINE TO INITIALIZE THE LONGLIB GRAPHICS PLOT PACKAGE
!
!     FORTRAN-77 VERSION:   DGL JULY, 1987
!     xmin,xmax,ymin,ymax -- size in inches that the library  will simulate as the display size
!
!     VPX,VPY -------------- COORDINATES OF BOTTOM LEFT ORIGIN
!     ZOM   ---------------- ZOOM FACTOR
!
use M_pixel
implicit none
real :: vpx
real :: vpy
real :: xmax
real :: xmax0
real :: ymax
real :: ymax0
real :: z
real :: zom

   z=zom
   xmax=xmax0
   ymax=ymax0
   call page(0.0,xmax,0.0,ymax)
   xminq=0.0
   yminq=0.0
   xmaxq=xmax
   ymaxq=ymax

   call color(0)
   call clear()
   call color(7)

   translatexq=vpx         ! ORIGIN X
   translateyq=vpy         ! ORIGIN Y

   scaleq=abs(z)      ! SCALE FACTOR
   if (scaleq.le.0.0) scaleq=1.0

   angleq=0.0       ! PLOTTING ANGLE ROTATION

   xlastscaleq=0.0         ! LAST POINT PLOTTED
   ylastscaleq=0.0

   ! set the VIEWPORTQ() ARRAY
   call dl_viewport(-999.0,999.0,-999.0,999.0)
   call dl_color(7) ! INITIALIZE LINE COLOR
end subroutine dl_init
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine dl_clipit(ivtb,xv2,yv2,av1,av2,xm,ym,xx,yx)
implicit none
real     :: av1
real     :: av2
integer  :: ivtb
real     :: xm
real     :: xv2
real     :: xx
real     :: ym
real     :: yv2
real     :: yx
!
!     CLIPS A LINE SEGMENT PARTIALLY VISIBLE
!
   if (iand(ivtb,1).ne.0) then ! LEFT EDGE
      if (av1.ne.xv2) yv2=yv2+(av2-yv2)*(xm-xv2)/(av1-xv2)
      xv2=xm
      ivtb=dl_inbox(xv2,yv2,xm,ym,xx,yx)
   endif
   if (iand(ivtb,2).ne.0) then ! RIGHT EDGE
      if (av1.ne.xv2) yv2=yv2+(av2-yv2)*(xx-xv2)/(av1-xv2)
      xv2=xx
      ivtb=dl_inbox(xv2,yv2,xm,ym,xx,yx)
   endif
   if (iand(ivtb,4).ne.0) then ! BOTTOM EDGE
      if (av2.ne.yv2) xv2=xv2+(av1-xv2)*(ym-yv2)/(av2-yv2)
      yv2=ym
      ivtb=dl_inbox(xv2,yv2,xm,ym,xx,yx)
   endif
   if (iand(ivtb,8).ne.0) then ! TOP EDGE
      if (av2.ne.yv2) xv2=xv2+(av1-xv2)*(yx-yv2)/(av2-yv2)
      yv2=yx
      ivtb=dl_inbox(xv2,yv2,xm,ym,xx,yx)
   endif
end subroutine dl_clipit
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
integer function dl_inbox(x,y,x_bottom_left,y_bottom_left,x_top_right,y_top_right)
!
!     FORTRAN-77 VERSION:   DGL JULY, 1987
!     CHECKS TO SEE IF POINT X,Y IS IN RECTANGLE
!     RETURNS ZERO IF IT IS
!     DL_INBOX tests a point to determine if it lies in a rectangle defined
!     by <x1,y1>,<x2,y2> and returns an integer value indicating where the point
!     is in relation to the rectangle. The value can easily be decoded by
!     ANDing the return value with the binary values of 1, 2, 4, 8.
!
!           |       |
!     1+8=9 | 0+8=8 | 2+8=10
!           |       |
!           |       |<x2,y2>
!     - - - #-------# - - - Add 8 if above this line
!           |  BOX  |
!         1 |   0   | 2
!           |       |
!     - - - #-------# - - - Add 4 if below this line
!    <x1,y1>|       |
!           |       |
!     1+4=5 | 0+4=4 | 2+4=6
!           |       |
!
      implicit none
      real,intent(in)    :: x,y                         ! coordinates of point
      real,intent(in)    :: x_bottom_left,y_bottom_left ! coordinates of bottom left box corner
      real,intent(in)    :: x_top_right,y_top_right     ! coordinates of upper right box corner

      integer            :: cd

      cd=0  ! start off assuming <x,y> is in or on the box

      ! check x range, and assign CD=1 if to left of box, 0 if in range, and 2 if to right of box
      if (x .lt. x_bottom_left) then
         cd=1
      elseif(x .gt. x_top_right)then
         cd=2
      endif

      ! check y range, and add 4 to CD if below box and add 8 if above box
      if (y .lt. y_bottom_left) then
         cd=cd+4
      elseif (y .gt. y_top_right)then
         cd=cd+8
      endif

      ! now CD=0 only if <x,y> is in or on the box
      dl_inbox=cd

end function dl_inbox
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!*==dl_axisa.f90 processed by SPAG 8.01RF 02:19 13 Dec 2024
SUBROUTINE dl_axisa(X0,Y0,A0,N0,S0,T0,C0,D0,Nm,Ml,Icol)
!
!     EXTENSIVELY MODIFIED BY D.LONG   7-OCT-83 AT JPL
!     REVISED 13-AUG-1987 DGL + IMPROVED APPEARANCE OF EXPONENT, ADDED EXPONENT COLOR
!     LAST REVISED ON 10-AUG-1990
!
!     X0  (R)  X COORDINATE OF START OF AXIS
!     Y0  (R)  Y COORDINATE OF START OF AXIS
!     A0  (C)  CHARACTER STRING TO DESCRIBE AXIS
!     N0  (I)  NUMBER OF CHARACTERS IN STRING
!              - ON CLOCKWISE SIDE OF AXIS (NORMAL FOR X)
!              + ON COUNTER CLOCKWISE SIDE OF AXIS (NORMAL FOR Y)
!                   HUNDREDS DIGIT    = 1 NO LABELING OF AXIS--TICKS AND LINE ONLY
!                   THOUSANDS DIGIT   = 1 HORIZONTAL NUMBERS IN Y AXIS LABEL
!                10 THOUSANDS DIGIT=1 USE NM,ML PARAMETERS
!               100 THOUSANDS DIGIT=1 USE COLOR PARAMETERS
!     S0  (I)  LENGTH OF AXIS (SHOULD BE INTEGER-VALUED)
!              < 0   TICKS PLACED ON OPPOSITE SIDE OF AXIS LABEL
!              = 0   NO ACTION
!              > 0   NORMAL
!     T0  (R)  ANGLE OF AXIS TO X AXIS OF PLOT COORDINATE SYSTEM
!                0.0 FOR X-AXIS
!               90.0 FOR Y-AXIS
!     C0  (R)  COORDINATE OF MINIMUM TICK ON AXIS
!     D0  (R)  SCALING DISTANCE BETWEEN "1" TICKS
!        NOTE:  THE FOLLOWING ARE ACCESSED ONLY IF ABS(NO)>10000
!     NM  (I)  NUMBER OF MINOR AXIS TICKS BETWEEN MAJOR TICKS (DEFAULT=0)
!     ML  (I)  NUMBER OF MAJOR AXIS TICKS (DEFAULT= 1 TICK/INCH)
!    ICOL (I)  COLOR ARRAY (ACCESSED ONLY IF ABS(NO)>100000
!               ICOL(1) AXIS COLOR
!               ICOL(2) NUMBER COLOR
!               ICOL(3) LABEL COLOR
!               ICOL(4) EXPONENT COLOR
!             PEN COLOR ON RETURN DEPENDS ON LAST ITEM PLOTTED
!             IN THE SEQUENCE INDICATED
! *************************************************************************
   IMPLICIT NONE
   REAL :: b1
   REAL :: b2
   REAL :: b3
   REAL :: b4
   REAL :: b6
   REAL :: b7
   REAL :: b8
   REAL :: C0
   REAL :: c1
   REAL :: c2
   REAL :: cs
   REAL :: D0
   REAL :: d1
   REAL :: e1
   REAL :: hor
   INTEGER :: i
   INTEGER :: ic
   INTEGER :: ic2
   INTEGER :: k
   INTEGER :: Ml
   INTEGER :: N0
   INTEGER :: n1
   INTEGER :: n2
   INTEGER :: nc1
   INTEGER :: Nm
   INTEGER :: nm1
   REAL :: S0
   REAL :: s1
   REAL :: T0
   REAL :: t1
   REAL :: t2
   REAL :: t3
   REAL :: t4
   REAL :: t5
   REAL :: t6
   REAL :: X0
   REAL :: x1
   REAL :: x2
   REAL :: x3
   REAL :: xl
   REAL :: xm
   REAL :: Y0
   REAL :: y1
   REAL :: y2
   REAL :: y3
   CHARACTER A0*(*)
   INTEGER Icol(4)
   LOGICAL vert , ticks , color
!
   cs = 0.15         ! CHARACTER SIZE
   IF ( S0/=0.0 ) THEN        ! ZERO LENGTH AXIS
      vert = .FALSE.    ! NO VERTICAL NUMBERS ON HORIZONTAL AXIS
      ticks = .TRUE.    ! PUT ON TICKS
      hor = T0
      t5 = 0.1       ! TICK LENGTH
      b7 = t5 + .08  ! NUMBER DISTANCE FROM AXIS
      b6 = b7
      b8 = 0.0
      nm1 = 0        ! NUMBER MINOR TICKS
      n2 = (abs(S0)+0.5) ! NUMBER OF MAJOR TICKS
      s1 = float(n2)
      xl = 1.0       ! INCREMENT BETWEEN MAJOR TICKS
      n1 = iabs(N0)
      color = .FALSE.
      IF ( n1>=100000 ) THEN
         n1 = mod(n1,100000)
                           ! USE COLOR ARRAY
         color = .TRUE.
      ENDIF
      IF ( n1>=10000 ) THEN
         n1 = mod(n1,10000)
         n2 = iabs(Ml)  ! NUMBER MAJOR TICKS
         IF ( n2==0 ) n2 = 1
         s1 = abs(S0)
         xl = abs(S0)/float(n2)
                              ! SPACING MAJOR TICKS
         nm1 = iabs(Nm) + 1
                         ! NUMBER MINOR TICKS
      ENDIF
      IF ( N0<0 ) THEN
!-----------------------------------------------------------------------------------------------------------------------------------
         b3 = (-cs)*4.0   ! CLOCKWISE LABELING
         b4 = -t5 - cs - .05
         t2 = T0
         t5 = -t5
      ELSE
         b3 = cs*3.8    ! COUNTER-CLOCKWISE LABELING
         b4 = cs + 0.08
         t2 = T0
      ENDIF
!-----------------------------------------------------------------------------------------------------------------------------------
      IF ( n1>=1000 ) THEN
         n1 = mod(n1,1000)
                          ! VERTICAL NUMBERS ON HORIZONTAL AXIS
         vert = .TRUE.
         hor = 0.0
         b4 = (abs(t5)*(1.+sign(1.,S0))/2.+.1)*sign(1.,float(N0))
         b6 = .49*cs
      ENDIF
      IF ( n1>=100 ) THEN
         n1 = mod(n1,100)
                         ! NO TICKS
         ticks = .FALSE.
      ENDIF
      t5 = t5*sign(1.,S0)
      t1 = T0*0.017453294
      t3 = cos(t1)
      t4 = sin(t1)
!
      t6 = t5*t3
      t5 = t5*t4
      x1 = X0
      y1 = Y0
      IF ( color ) CALL dl_color(Icol(1))
                                        ! COLOR
      DO i = 1 , n2   ! MAJOR TICKS
         IF ( nm1/=0 ) THEN
            xm = xl/float(nm1)
                          ! SPACING MINOR TICKS
            DO k = 1 , nm1
                      ! DO MINOR TICKS
               x2 = x1 + t3*float(k-1)*xm
               y2 = y1 + t4*float(k-1)*xm
               x3 = x2 - t5*.5
               y3 = y2 + t6*.5
               CALL dl_move(x2,y2)
            ENDDO
            CALL dl_draw(x3,y3)
         ENDIF
         x2 = x1 - t5
         y2 = y1 + t6
         CALL dl_move(x2,y2)
         CALL dl_draw(x1,y1)
         x1 = x1 + t3*xl
         y1 = y1 + t4*xl
         IF ( T0==90. ) x1 = X0
         CALL dl_draw(x1,y1)
      ENDDO
      x2 = x1 - t5
      y2 = y1 + t6
      CALL dl_draw(x2,y2)   ! FINISH LAST MAJOR TICK
!     CHECK FOR EXPONENT VALUE
      d1 = D0           ! SCALING FACTOR
      c1 = C0 + s1*d1    ! STARTING VALUE
      e1 = 0.0           ! EXPONENT
      IF ( d1/=0.0 ) THEN
         DO WHILE ( abs(d1)>=10.0 )
            d1 = d1*0.1
            c1 = c1*0.1
            e1 = e1 + 1.0
         ENDDO
         DO WHILE ( abs(d1)<0.5 )
            d1 = d1*10.0
            c1 = c1*10.0
            e1 = e1 - 1.0
         ENDDO
      ENDIF
      IF ( .NOT.ticks ) THEN
         IF ( color ) CALL dl_color(Icol(3))
                                           ! COLOR
         RETURN
      ENDIF
      IF ( vert ) THEN
         c2 = c1 - n2*d1
                        ! MAKE SPACE FOR VERTICAL NUMBERS
         ic = 1      ! ON HORIZONTAL AXIS
         IF ( abs(c2)>=1.0 ) ic = ifix(alog10(abs(c2)))
         ic2 = 1
         IF ( abs(c1)>=1.0 ) ic2 = ifix(alog10(abs(c1)))
         nc1 = max(ic,ic2) + 2
         IF ( c2<0.0 .OR. C0<0.0 ) nc1 = nc1 + 1
         IF ( N0>0.0 ) b4 = b4 + float(nc1)*cs
         b3 = 0.0
         b8 = (.25+abs(t5)*(sign(1.,S0)+1.)/2.+float(nc1)*cs)*sign(1.,float(N0))
      ENDIF
      x2 = x1 - b4*t4 - b7*t3
                         ! LOCATE CENTER NUMBER LABELS
      y2 = y1 + b4*t3 - b6*t4
      n2 = n2 + 1
      IF ( color ) CALL dl_color(Icol(2))
                                        ! COLOR
      DO i = 1 , n2  ! LABEL MAJOR TICKS
         CALL dl_number(x2,y2,cs,c1,hor,0.01,-1)
         c1 = c1 - d1*s1/float(n2-1)
         x2 = x2 - t3*xl
         y2 = y2 - t4*xl
      ENDDO
      IF ( color ) CALL dl_color(Icol(3))
                                        ! COLOR
      IF ( n1>0 ) THEN   ! ADD TITLE
         c2 = 0.0
         y2 = 0.0
         CALL dl_symbol(c2,y2,cs,A0,0.0,n1,-3) ! TITLE LENGTH
         b1 = 0.5*(abs(S0)-c2)
                              ! CENTER TITLE
         IF ( e1/=0. ) b1 = b1 - cs*3.0
                                    ! PUT ON EXPONENT
         x2 = X0 + b1*t3 - b3*t4 - b8*t4
         y2 = Y0 + b1*t4 + b3*t3
         CALL dl_symbol(x2,y2,cs,A0,t2,n1,-1)
      ELSE
         c2 = 0.0
         b1 = 0.5*abs(S0)
         x2 = X0 + b1*t3 - b3*t4 - b8*t4
         y2 = Y0 + b1*t4 + b3*t3
      ENDIF
      IF ( e1/=0.0 ) THEN      ! NO EXPONENT
         IF ( color ) CALL dl_color(Icol(4))
                                        ! COLOR
         c2 = c2 + cs
         x2 = x2 + c2*t3
         y2 = y2 + c2*t4
         CALL dl_symbol(x2,y2,cs,'(X10',t2,4,-1)
         x2 = x2 + cs*3.75*t3 - cs*t4*0.4
         y2 = y2 + cs*3.75*t4 + cs*t3*0.4
         CALL dl_number(x2,y2,cs,e1,t2,0.0,-1)
         b2 = 0.8 + aint(alog10(abs(e1)))
         IF ( e1<0.0 ) b2 = b2 + 1
         x2 = x2 + b2*cs*t3 + cs*t4*0.4
         y2 = y2 + b2*cs*t4 - cs*t3*0.4
         CALL dl_symbol(x2,y2,cs,')',t2,1,-1)
      ENDIF
   ENDIF
END SUBROUTINE dl_axisa
!*==dl_number.f90 processed by SPAG 8.01RF 02:19 13 Dec 2024
SUBROUTINE dl_number(X,Y,Hght,Z,T,F0,Ipf)
!
!     WRITTEN BY D. LONG    AUG, 1983 AT JPL
!     REVISED: JUNE 1990
!
!     PLOTS THE FLOATING POINT NUMBER Z (CAN PLOT AS AN INTEGER)
!
!     X,Y   (R) COORDINATES OF STRING
!            (999.,999.) CONTINUE FROM LAST POINT
!     HGHT  (R) HEIGHT OF THE PLOTTED NUMBER
!     Z  (R) FLOATING POINT NUMBER TO BE PLOTTED
!     T  (R) ORIENTATION ANGLE
!     F0 (R) PLOTTING FORMAT (Fn.j)
!          n = TOTAL NUMBER OF SPACES TO USE (INCLUDING SIGN AND D.P.)
!           [MAX 18 CHARACTERS WIDE]
!          j = DIGITS TO RIGHT OF DECIMAL POINT (TWO DIGITS EXPECTED)
!           (F4.2 SHOULD BE WRITTEN F4.02)
!          IF F0 < 0 USE EXPONENTIAL NOTATION (I.E., En,j)
!          F0 = 1 PLOT IN FLOATING POINT FREE FORMAT
!          F0 = 0 PLOT INTEGER PORTION WITH NO D.P. (FREE FORMAT)
!          F0 =-1 PLOT IN EXPONENTIAL FREE FORMAT
!            NOTE: FREE FORMATS HAVE LEADING SPACES SUPPRESSED
!          F0 > 1000 PLOT INTEGER PORTION IN FIXED FORMAT WITH
!           n DIGITS AND WITHOUT D.P.
!          IF n=0 THEN PLOT INTEGER PORTION, DECIMAL POINT, AND
!            j DIGITS TO RIGHT OF DECIMAL POINT
!          WHEN Z OVERFLOWS THIS FORMAT, SPACE IS FILLED WITH ASTERISKS
!     IPF   (I) NUMBER CENTERING FLAG (SEE SYMBOL)
!        =-3 X,Y ARE LOWER LEFT CORNER, END OF STRING RETURNED IN X,Y
!           BUT NUMBER IS NOT PLOTTED
!        =-2 X,Y ARE LOWER LEFT CORNER, END OF STRING RETURNED IN X,Y
!        =-1 X,Y ARE LOWER LEFT CORNER
!        = 0 X,Y ARE STRING CENTER
!        =+1 X,Y ARE LOWER RIGHT CORNER
!        =+2 NO PLOT OUTPUT
!
   IMPLICIT NONE
   REAL :: alg
   REAL :: f
   REAL :: F0
   REAL :: fa
   REAL :: hg
   REAL :: Hght
   INTEGER :: i
   INTEGER :: iff
   INTEGER :: Ipf
   INTEGER :: nd
   INTEGER :: nn
   REAL :: T
   REAL :: t1
   REAL :: X
   REAL :: Y
   REAL :: Z
   CHARACTER b*18 , fb*8 , fb1*8   ! WORKING BUFFERS
   INTEGER :: spag_nextblock_1
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         iff = 0
         hg = Hght
         IF ( hg==0.0 ) hg = 0.15
         t1 = T
         nd = 0
         nn = 0
         fa = F0
         IF ( abs(fa)>1022.0 ) fa = 0.0
         IF ( fa/=0.0 ) THEN  ! INTEGER FORMAT
            IF ( fa>999.0 ) THEN
                             ! PLOT FORMATTED INTEGER
               nn = amod(fa,1000.)
               fa = 0.0
            ELSE     ! PLOT FLOAT OR EXPON NUMBER
               f = abs(fa)*1.000002
               nn = f
               f = (f-nn)*100.
               nd = f
            ENDIF
         ENDIF
         IF ( nd>17 ) nd = nd/10
                              ! CORRECT SIMPLE INPUT ERRORS
         IF ( nn==0 ) THEN
                         ! DIGITS TO LEFT OF DECIMAL POINT
            nn = nd + 2
            IF ( Z==0 .AND. fa==0.0 ) nn = 1
            IF ( Z/=0.0 ) THEN
               alg = alog10(abs(Z))
               IF ( alg<0.0 ) alg = 0.0
               nn = nd + 2 + alg
               IF ( fa==0.0 ) nn = 1 + alg
            ENDIF
            IF ( Z<0.0 ) nn = nn + 1
            IF ( fa<0.0 ) nn = nn + 4
         ENDIF
         IF ( nd>nn ) GOTO 20
                             ! FORMAT ERROR
         IF ( nn>18 ) nn = 18
                           ! MAX CHARACTERS
         IF ( fa==0.0 ) THEN
                           ! INTEGER
            i = Z
            fb = char(nn-10*(nn/10)+48)//')'
            fb1 = fb
            IF ( nn/10>0 ) fb = char(nn/10+48)//fb1
            fb1 = '(I'//fb
            WRITE (b,fb1,ERR=20) i
         ELSE        ! FLOATING POINT OR EXPONENTIAL
            IF ( nn>1 ) THEN
               fb = char(nd-10*(nd/10)+48)//')'
               fb1 = fb
               IF ( nd/10>0 ) fb = char(nd/10+48)//fb1
               fb1 = char(nn-10*(nn/10)+48)//'.'//fb
               fb = fb1
               IF ( nn/10>0 ) fb = char(nn/10+48)//fb1
               IF ( fa>0.0 ) THEN
                  fb1 = '(F'//fb
               ELSE
                  fb1 = '(E'//fb
               ENDIF
            ELSE
               IF ( fa>0.0 ) THEN
                  fb1 = '(F)'
               ELSE
                  fb1 = '(E)'
               ENDIF
               nn = 16
               iff = 1
            ENDIF
            WRITE (b,fb1,ERR=20) Z
            IF ( iff==1 ) b = adjustl(b)
                             ! REMOVE LEADING SPACES
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
         CALL dl_symbol(X,Y,hg,b,t1,nn,Ipf)
         RETURN
      END SELECT
      cycle SPAG_DispatchLoop_1
 20   continue
      DO i = 1 , 18
         b(i:i) = '*'
         IF ( i==nn-nd ) b(i:i) = '.'
      ENDDO
      spag_nextblock_1 = 2
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE dl_number
!*==dl_plot.f90 processed by SPAG 8.01RF 02:19 13 Dec 2024
SUBROUTINE dl_plot(Xplot0,Yplot0,Iselect0)
!
!     PLOT is the central routine for controlling the plotting of lines.
!     Any call to PLOT when graphics mode is not initialized is a dummy call.
!     By default the viewport will be the "usable size" of the display device
!     as set by "DL_INIT".
!     By default the lower left corner is (0,0)
!     Only non-blank page strips are output to the device.
!
!     The transformation of an input point (x,y) in plot units
!     to an output point (nx,ny) in plot surface units (ignoring clipping) is:
!         nx = (z * (x * cos( a ) - y * sin( a )) + ox) / rx
!         nx = (z * (x * sin( a ) + y * cos( a )) + oy) / ry
!
!     where
!
!         a is the relative plotting angle (expressed in radians)
!         z is the zoom scale factor
!         ox,oy is the scaled, rotated relative origin
!         rx,ry is the metafile resolution for each axis (inches/res unit)
!     (see DL_QUERY)
!
!     The relative plotting angle a is updated according to:
!         anew = aold + ain
!
!     and the relative origin (ox,oy) is updated according to:
!         oxnew = z * (xin * cos( a ) - yin * sin( a )) + oxold
!         oynew = z * (xin * sin( a ) + yin * cos( a )) + oyold
!
!     CALL DL_PLOT (XPLOT0,YPLOT0,ISELECT0)
!
!     XPLOT0,YPLOT0   (R): coordinate values
!     ISELECT0        (I): plot function selector
!
!     XPLOT0 YPLOT0 ISELECT0 COMMAND INTERPRETATION
!
!
!      X      Y       2: draw to (x,y) with 'pen down'
!      X      Y      -2: same as iselect0=2; but (x,y) becomes new origin
!      X      Y       3: move to (x,y) with 'pen up'
!      X      Y      -3: same as iselect0=3. (x,y) becomes new origin
!      -      -       5: pick pen up at last point and flush
!                  ????: ANY OTHER VALUE OF ISELECT0 IS TREATED AS A NOP
! *************************************************************************
   USE m_pixel
   IMPLICIT NONE
   INTEGER :: Iselect0
   INTEGER :: ivta
   INTEGER :: ivtb
   REAL :: xcon
   REAL :: Xplot0
   REAL :: xtemp
   REAL :: xtemp1
   REAL :: ycon
   REAL :: Yplot0
   REAL :: ytemp
   REAL :: ytemp1
!#######################################################################
!     DECODE COMMAND
   SELECT CASE (Iselect0)
   CASE (2,3)
   CASE (-2,-3)
      translatexq = xcon     ! make scaled rotated input coordinates the new origin
      translateyq = ycon
   CASE DEFAULT
      WRITE (*,*) '# *PLOT* UNEXPECTED SELECTION ' , Iselect0
   END SELECT
   CALL dl_trs(Xplot0,Yplot0,xcon,ycon)     ! convert call numbers to current plot coordinate system
!     DRAW LINE SEGMENT  ISELECT0=2,3 (and -2,-3)
      ! check if point (xcon,ycon) is in viewport rectangle
   ivta = dl_inbox(xcon,ycon,viewportq(1),viewportq(2),viewportq(3),viewportq(4))
      ! check if point (xlastscaleq,ylastscaleq) is in viewport rectangle
   ivtb = dl_inbox(xlastscaleq,ylastscaleq,viewportq(1),viewportq(2),viewportq(3),viewportq(4))
   IF ( ior(ivta,ivtb)/=0 ) THEN        ! LINE ENTIRELY VISIBLE
      IF ( iand(ivta,ivtb)/=0 ) THEN  ! LINE ENTIRELY INVISIBLE
         xlastscaleq = xcon
         ylastscaleq = ycon
         RETURN
      ENDIF
      IF ( ivtb/=0 ) THEN   ! OLD POINT IS OUTSIDE WINDOW
         xtemp1 = xlastscaleq
         ytemp1 = ylastscaleq
         CALL dl_clipit(ivtb,xtemp1,ytemp1,xcon,ycon,viewportq(1),viewportq(2),viewportq(3),viewportq(4))
         IF ( ivtb/=0 ) THEN  ! VECTOR DOES NOT INTERSECT
            xlastscaleq = xcon
            ylastscaleq = ycon
            RETURN
         ENDIF

      ENDIF
      xtemp = xcon
      ytemp = ycon
      ! clips a partially visible line segment
      IF ( ivta/=0 ) CALL dl_clipit(ivta,xtemp,ytemp,xlastscaleq,ylastscaleq,viewportq(1),viewportq(2),viewportq(3),viewportq(4))
      xlastscaleq = xcon
      ylastscaleq = ycon
      IF ( ivta/=0 ) RETURN
      xcon = xtemp
      ycon = ytemp
   ENDIF
! ----------------------------------------------------------------------------------------------------------------------------------
   IF ( Iselect0==2 ) THEN
      CALL draw2(xcon,ycon)
   ELSEIF ( Iselect0==3 ) THEN
      CALL move2(xcon,ycon)
   ELSE
      WRITE (*,*) '*dl_plot* 2,3 internal error' , xcon , ycon , Iselect0
   ENDIF
   xlastscaleq = xcon
   ylastscaleq = ycon
END SUBROUTINE dl_plot
!*==dl_slices.f90 processed by SPAG 8.01RF 02:19 13 Dec 2024
SUBROUTINE dl_slices(A,Inx,Inz,Nx,Nz,Alpha,Beta,Xh,Yh,Zh,Iflag,Iaxis,Xt,Nxt,Xastart,Xaend,Nmx,Nnx,Mlx,Tsx,Ndx,Smx,Yt,Nyt,Nmy,Nny,&
         & Mly,Tsy,Ndy,Smy,Zt,Nzt,Zastart,Zaend,Nmz,Nnz,Mlz,Tsz,Ndz,Smz,Aminin,Amaxin,Icol,Maxsize)
!
!     CREATED BY D. LONG     APR, 1984 AT JPL
!     REVISED BY D. LONG     MAY, 1986
!     +REDUCED REDUNDANT PEN MOTIONS AND CORRECTED SOME MINOR BUGS
!
!     ROUTINE TO PLOT DATA IN 3-D OVERLAY FORM
!
!     COORDINATE SYSTEM IS:  Y  Z
!              NAMES:   |/
!                  \X
!
!     A REAL ARRAY A(INX,INZ) CONTAINING VERTICAL HEIGHT DATA
!     INX,INZ INTEGERS DIMENSION OF A ARRAY
!     NX,NZ INTEGERS INDICATING SIZE OF A ARRAY TO PLOT
!     ALPHA REAL ANGLE (IN DEGREES) OF X AXIS (NX) FROM HORIZONTAL
!     BETA  REAL ANGLE (IN DEGREES) OF Z AXIS (NZ) FROM HORIZONTAL
!     XH,YH,ZH REAL LENGTH OF EACH AXIS
!     IFLAG INTEGER
!     (ONE'S DIGIT)      = 2 USE PEN COLOR CONTROL ARRAY
!                        = 1 DO NOT USE PEN COLOR ARRAY
!     (TEN'S DIGIT)      = 0 PLOT SIDE PLATES
!                        = 1 DO NOT PLOT SIDE PLATES
!     IAXIS INTEGER AXIS OPTION FLAG
!           = 0 DO NOT PLOT AXIS--FOLLOWING VARIABLES NOT ACCESSED
!           < 0 PLOT AXIS, USE INPUT Y AXIS SCALE--FOLLOWING VARIABLES ACCESSED
!           > 0 PLOT AXIS, USE COMPUTED Y AXIS SCALE--FOLLOWING VARIABLES ACCESSED
!     (ONE'S DIGIT)  = 1 PLOT AXIS, Y AXIS SCALE--VARIABLES ACCESSED
!           = 2 PLOT AXIS, AUTO SCALE Y AXIS--VARIABLES ACCESSED
!     (TEN'S DIGIT)  = 0 DEFAULT AXIS PARAMETERS
!           = 1 SPECIALIZED DL_AXISB PARAMETERS
!     XT,YT,ZT CHAR STRINGS FOR AXIS TITLES
!     NXT,NYT,NZT INT  LENGTH OF AXIS TITLES
!                IF ZERO THEN THAT AXIS NOT PLOTTED
!     XASTART,ZASTART   REAL AXIS START VALUES
!     XAEND,ZAEND REAL AXIS END VALUES
!
! FOLLOWING ONLY ACCESSED IF TEN'S DIGIT OF IFLAG=1
!     NMX,NMY,NMZ INT NUMBER OF MINOR TICKS BETWEEN MAJOR TICKS
!     NNX,NNY,NNZ INT HIGHLIGHT LENGTH OF NNX-TH MINOR TICK ON AXIS
!     MLX,MLY,MLZ INT NUMBER OF MAJOR TICK MARKS ON AXIS
!     TSX,TSY,TSZ REAL SIZE OF TITLE AND NUMBERS OF AXIS
!             IF LESS THAN ZERO DO NOT AUTO-SCALE BY (x10^POWER)
!     NDX,NDY,NDZ INT NUMBER OF DIGITS TO RIGHT OF DECIMAL POINT
!     SMX,SMY,SMZ REAL MAJOR TICK LENGTH
!     AMININ,AMAXIN  REAL YAXIS SCALING FACTORS (ONLY NEEDED IF IAXIS < 0)
!     ICOL    INTEGER COLOR CONTROL (ACCESSED IF MAG(IFLAG)=2)
!              ICOL(1) AXIS LINE
!              ICOL(2) AXIS NUMBERS
!              ICOL(3) AXIS TITLE
!              ICOL(4) AXIS EXPONENT
!              ICOL(5) PLOT
!
   IMPLICIT NONE
   REAL :: A
   REAL :: Alpha
   REAL :: amax
   REAL :: Amaxin
   REAL :: amh
   REAL :: Aminin
   REAL :: ang
   REAL :: as
   REAL :: Beta
   REAL :: bh
   REAL :: daa
   REAL :: dx
   REAL :: dx1
   REAL :: dx2
   REAL :: dy
   REAL :: dy1
   REAL :: dy2
   REAL :: dz
   REAL :: hx1
   REAL :: hx2
   REAL :: hy1
   REAL :: hy2
   INTEGER :: i
   INTEGER :: iaf
   INTEGER :: Iaxis
   INTEGER :: ic
   INTEGER :: Icol
   INTEGER :: idct
   INTEGER :: Iflag
   INTEGER :: iflag1
   INTEGER :: iflag10
   INTEGER :: ihct
   INTEGER :: ihold
   INTEGER :: ip
   INTEGER :: ipct
   INTEGER :: ipen
   INTEGER :: ix
   INTEGER :: iz
   INTEGER :: Mlx
   INTEGER :: Mly
   INTEGER :: Mlz
   INTEGER :: n1
   INTEGER :: n2
   INTEGER :: nadd
   INTEGER :: Ndx
   INTEGER :: Ndy
   INTEGER :: Ndz
   INTEGER :: Nmx
   INTEGER :: Nmy
   INTEGER :: Nmz
   INTEGER :: Nnx
   INTEGER :: Nny
   INTEGER :: Nnz
   INTEGER :: Nx
   INTEGER :: Nxt
   INTEGER :: Nyt
   INTEGER :: Nz
   INTEGER :: Nzt
   REAL :: Smx
   REAL :: Smy
   REAL :: Smz
   REAL :: Tsx
   REAL :: Tsy
   REAL :: Tsz
   REAL :: x
   REAL :: x0
   REAL :: Xaend
   REAL :: Xastart
   REAL :: Xh
   REAL :: xlen
   REAL :: xp
   REAL :: xp1
   REAL :: xp2
   REAL :: y
   REAL :: y0
   REAL :: Yh
   REAL :: ylen
   REAL :: yp
   REAL :: yp1
   REAL :: yp2
   REAL :: Zaend
   REAL :: Zastart
   REAL :: Zh
   REAL :: zlen
   INTEGER :: Inx , Inz
   DIMENSION A(Inx,Inz) , as(2) , Icol(*) , ic(4)
   INTEGER , INTENT(IN) , OPTIONAL :: Maxsize
   INTEGER :: maxsize_local
! PARAMETER (maxsize=204800)
! DIMENSION H(maxsize,2),P(maxsize_local,2)
   REAL , ALLOCATABLE :: h(:,:)
   REAL , ALLOCATABLE :: p(:,:)
   CHARACTER*(*) Xt , Yt , Zt
   LOGICAL flag , hhigh
   REAL , PARAMETER :: TPI = 3.141592654
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
         IF ( present(Maxsize) ) THEN
            maxsize_local = Maxsize
         ELSE
            maxsize_local = 204800
         ENDIF

         IF ( allocated(h) ) DEALLOCATE (h)
         IF ( allocated(p) ) DEALLOCATE (p)
         ALLOCATE (h(maxsize_local,2))
         ALLOCATE (p(maxsize_local,2))
!
         alphq = Alpha*TPI/180.0
                             ! X-AXIS INCLINATION 0-80 DEGS
         betq = Beta*TPI/180.0
                             ! Z-AXIS ANGLE 5-80 DEGS
!
         IF ( Iaxis<0 ) THEN
            amax = Amaxin
            aminq = Aminin
         ELSE
            amax = A(1,1)
            aminq = A(1,1)
            DO iz = 1 , Nz
                         ! DETERMINE MAX,MIN ARRAY VALUES
               DO ix = 1 , Nx
                  amax = amax1(amax,A(ix,iz))
                  aminq = amin1(aminq,A(ix,iz))
               ENDDO
            ENDDO
         ENDIF
         IF ( Alpha<0. .OR. Alpha>88. .OR. Beta<1. .OR. Beta>90. ) THEN
            WRITE (*,'(*(g0))') '(" *** dl_slices INPUT ANGLE ERROR ***")ALPHA=' , Alpha , '(allowed 0 to 88) BETA=' , Beta , &
                               &'(allowed 1 to 90)'
            RETURN
         ENDIF
         IF ( amax==aminq ) THEN
            WRITE (*,'(" *** dl_slices SCALE ERROR *** MAX=MIN")')
            amax = aminq + 1.0
         ENDIF
!
         xlen = abs(Xh)
         xscaleq = xlen/float(Nx-1)
         zlen = abs(Zh)
         zscaleq = zlen/float(Nz-1)
         ylen = abs(Yh)
         IF ( mod(iabs(Iaxis),10)==2 ) THEN
                                         ! SMOOTH SCALE FACTORS
            as(1) = amax
            as(2) = aminq
            CALL dl_range(as,ylen,2,1,1,aminq,daa)
            amax = ylen*daa + aminq
         ENDIF
         yscaleq = 1.0
         IF ( amax-aminq/=0.0 ) yscaleq = ylen/(amax-aminq)
!
!     INITIALIZE PLOT PACKAGE
!
         iaf = iabs(Iaxis)/10

         iflag1 = iabs(Iflag)
         iflag10 = mod(iflag1,100)/10
         iflag1 = mod(iflag1,10)

         IF ( Iaxis/=0 ) THEN
                            ! PLOT AXIS LABELS
            nadd = 0
            IF ( iflag1==2 ) THEN
               ic(1) = Icol(2)
               ic(2) = Icol(3)
               ic(3) = Icol(4)
               ic(4) = Icol(5)
               nadd = 100000
                        ! PEN COLOR
            ENDIF
            CALL dl_vxpt3d(xp,yp,aminq,1,1,Nx)
                                            ! DETERMINE X,Y VALUE OF A POINT ON 3-D SURFACE
            dy = (amax-aminq)/ylen
            IF ( Nyt>0 ) THEN
                             ! PLOT Y AXIS
               IF ( iaf==1 ) THEN
                  CALL dl_axisb(xp,yp,Yt,Nyt+11000+nadd,ylen,90.,aminq,dy,Nmy,Nny,-iabs(Mly),Tsy,Ndy,Smy,ic)
               ELSE
                  CALL dl_axisa(xp,yp,Yt,Nyt+1000+nadd,ylen,90.,aminq,dy,n1,n2,ic)
               ENDIF
            ENDIF
            CALL dl_vxpt3d(xp1,yp1,aminq,Nx,1,Nx)
                                               ! DETERMINE X,Y VALUE OF A POINT ON 3-D SURFACE
            dx = (Xaend-Xastart)/xlen
            ang = atan2(yp1-yp,xp1-xp)*180./TPI
            IF ( Nxt>0 ) THEN
               IF ( iaf==1 ) THEN
                  CALL dl_axisb(xp,yp,Xt,-Nxt-nadd-10000,xlen,ang,Xastart,dx,Nmx,Nnx,-iabs(Mlx),Tsx,Ndx,Smx,ic)
               ELSE
                  CALL dl_axisa(xp,yp,Xt,-Nxt-nadd,xlen,ang,Xastart,dx,n1,n2,ic)
               ENDIF
            ENDIF
            dz = (Zaend-Zastart)/zlen
            IF ( Nzt>0 ) THEN
               IF ( iaf==1 ) THEN
                  CALL dl_axisb(xp1,yp1,Zt,-Nzt-nadd-10000,zlen,Beta,Zastart,dz,Nmz,Nnz,-iabs(Mlz),Tsz,Ndz,Smz,ic)
               ELSE
                  CALL dl_axisa(xp1,yp1,Zt,-Nzt-nadd,zlen,Beta,Zastart,dz,n1,n2,ic)
               ENDIF
            ENDIF
         ENDIF
         IF ( iflag1==2 ) CALL dl_color(Icol(5))
                                              ! PEN COLOR
!
!     PLOT FRONT PLATE
!
         ipen = 3
         DO i = 1 , Nx
            IF ( i>maxsize_local ) THEN
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            CALL dl_vxpt3d(h(i,1),h(i,2),A(i,1),i,1,Nx)
                                                     ! INITIALIZE HISTORY ! DETERMINE X,Y VALUE OF A POINT ON 3-D SURFACE
            CALL dl_plot(h(i,1),h(i,2),ipen)
                                            ! PLOT SIDE LINE
            ipen = 2
         ENDDO
         ihold = Nx
         IF ( Beta/=90.0 ) THEN

            IF ( iflag10/=1 ) THEN
                                  ! DON'T PLOT SIDE PLATES
               CALL dl_vxpt3d(xp,yp,aminq,Nx,1,Nx)
                                          ! DETERMINE X,Y VALUE OF A POINT ON 3-D SURFACE
               CALL dl_draw(xp,yp)
               DO i = 1 , Nx - 1
                       ! ADD SIDE LINES
                  CALL dl_move(h(i,1),h(i,2))
                  CALL dl_vxpt3d(xp,yp,aminq,i,1,Nx)
                                            ! DETERMINE X,Y VALUE OF A POINT ON 3-D SURFACE
                  CALL dl_draw(xp,yp)
                  CALL dl_vxpt3d(xp,yp,aminq,i+1,1,Nx)
                                              ! DETERMINE X,Y VALUE OF A POINT ON 3-D SURFACE
                  CALL dl_draw(xp,yp)
               ENDDO
            ENDIF
!
!     PLOT SIDE PLATE
!
            CALL dl_move(h(Nx,1),h(Nx,2))
            DO i = 1 , Nz
                       ! PLOT RIGHT SIDE CURVE
               IF ( Nx+i>maxsize_local ) THEN
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               CALL dl_vxpt3d(xp,yp,A(Nx,i),Nx,i,Nx)
                                               ! DETERMINE X,Y VALUE OF A POINT ON 3-D SURFACE
               h(Nx+i,1) = xp
               h(Nx+i,2) = yp
               CALL dl_draw(xp,yp)
            ENDDO
            CALL dl_vxpt3d(xp,yp,aminq,Nx,1,Nx)
                                          ! DETERMINE X,Y VALUE OF A POINT ON 3-D SURFACE
            CALL dl_move(xp,yp)
            ihold = Nx + Nz
                         ! NUMBER OF H VALUES
            IF ( iflag10/=1 ) THEN
                            ! DON'T PLOT SIDE PLATES
               DO i = 2 , Nz
                          ! ADD SIDE LINES
                  CALL dl_vxpt3d(xp2,yp2,aminq,Nx,i,Nx)
                                                   ! DETERMINE X,Y VALUE OF A POINT ON 3-D SURFACE
                  CALL dl_draw(xp2,yp2)
                  CALL dl_vxpt3d(xp,yp,A(Nx,i),Nx,i,Nx)
                                                  ! DETERMINE X,Y VALUE OF A POINT ON 3-D SURFACE
                  CALL dl_draw(xp,yp)
                  CALL dl_move(xp,yp2)
               ENDDO
            ENDIF
!
!     BEGIN MAIN LOOP
            ip = 3
         ENDIF
         mainloop: DO iz = 2 , Nz
                                ! OVER Z DIMENSION TOWARD REAR
            spag_nextblock_2 = 1
            SPAG_DispatchLoop_2: DO
               SELECT CASE (spag_nextblock_2)
               CASE (1)
                  ipct = 1
                  idct = 1
                  ihct = 1
!        DETERMINE START POINT LOCATION
                  CALL dl_vxpt3d(xp1,yp1,A(idct,iz),1,iz,Nx)
                                                    ! LEFT-MOST DATA POINT ! DETERMINE X,Y VALUE OF A POINT ON 3-D SURFACE
                  IF ( xp1<h(1,1) ) THEN
                                  ! DATA TO LEFT OF HISTORY ARRAY
!           IF (IPCT.GT.maxsize_local) GOTO 999
!           P(IPCT,1)=XP1
!           P(IPCT,2)=YP1
!           IPCT=IPCT+1
                     CALL dl_move(xp1,yp1)
                     DO i = 1 , Nx
                       ! (VERY RARE)
                        CALL dl_vxpt3d(xp1,yp1,A(i,iz),i,iz,Nx)
                                                       ! DETERMINE X,Y VALUE OF A POINT ON 3-D SURFACE
                        IF ( xp1>h(1,1) ) THEN
                           idct = i - 1
                           CALL dl_vxpt3d(dx1,dy1,A(idct,iz),idct,iz,Nx)
                                                                ! DETERMINE X,Y VALUE OF A POINT ON 3-D SURFACE
                           hhigh = .FALSE.
                           hx1 = h(1,1)
                           hy1 = h(1,2)
                           hx2 = h(2,1)
                           hy2 = h(2,2)
                           idct = idct + 1
                           ihct = ihct + 2
                           CALL dl_vxpt3d(dx2,dy2,A(idct,iz),idct,iz,Nx)
                                                                ! DETERMINE X,Y VALUE OF A POINT ON 3-D SURFACE
                           idct = idct + 1
                           spag_nextblock_2 = 2
                           CYCLE SPAG_DispatchLoop_2
                        ENDIF
                        IF ( ipct>maxsize_local ) THEN
                           spag_nextblock_1 = 3
                           CYCLE SPAG_DispatchLoop_1
                        ENDIF
                        p(ipct,1) = xp1
                        p(ipct,2) = yp1
                        ipct = ipct + 1
                        CALL dl_draw(xp1,yp1)
                     ENDDO
                  ENDIF
                  idct = 2
                  CALL dl_vxpt3d(dx1,dy1,A(1,iz-1),1,iz-1,Nx)
                                                     ! DETERMINE X,Y VALUE OF A POINT ON 3-D SURFACE
                  CALL dl_vxpt3d(dx2,dy2,A(1,iz),1,iz,Nx)
                                                     ! DETERMINE X,Y VALUE OF A POINT ON 3-D SURFACE
!C       CALL dl_move(H(1,1),H(1,2))
                  x0 = h(1,1)
                  y0 = h(1,2)
                  ip = 3
                  IF ( ipct>maxsize_local ) THEN
                     spag_nextblock_1 = 3
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  p(ipct,1) = h(1,1)
                  p(ipct,2) = h(1,2)
                  ipct = ipct + 1
                  SPAG_Loop_2_1: DO i = 2 , ihold
                     IF ( h(i,1)>dx1 ) EXIT SPAG_Loop_2_1
                     IF ( ipct>maxsize_local ) THEN
                        spag_nextblock_1 = 3
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     p(ipct,1) = h(i,1)
                     p(ipct,2) = h(i,2)
                     ipct = ipct + 1
!C             CALL dl_draw(H(I,1),H(I),2)
                     x0 = h(i,1)
                     y0 = h(i,2)
                  ENDDO SPAG_Loop_2_1
                  SPAG_Loop_2_2: DO
                     ihct = i - 1
                     hx1 = h(ihct,1)
                     hy1 = h(ihct,2)
                     hx2 = h(ihct+1,1)
                     hy2 = h(ihct+1,2)
                     ihct = ihct + 2
                     hhigh = .TRUE.
                     IF ( hx1==hx2 ) THEN
                        IF ( ihct==ihold ) EXIT SPAG_Loop_2_2
                        ihct = ihct + 1
                        CYCLE
                     ENDIF
                     amh = (hy2-hy1)/(hx2-hx1)
                     bh = hy1 - hx1*amh
                     yp = amh*dx1 + bh
                     IF ( yp<=dy1 ) hhigh = .FALSE.
                     IF ( hy1==dy1 .AND. hx1==dx1 ) THEN
                        hhigh = .TRUE.
                        yp = amh*dx2 + bh
                        IF ( yp<dy2 ) hhigh = .FALSE.
                     ENDIF
                     EXIT SPAG_Loop_2_2
                  ENDDO SPAG_Loop_2_2
                  spag_nextblock_2 = 2
               CASE (2)
                  SPAG_Loop_2_3: DO
!
!     TOP OF INNER LOOP
!
                     CALL dl_intersect(flag,x,y,hx1,hy1,hx2,hy2,dx1,dy1,dx2,dy2,hhigh)
                     IF ( flag ) THEN
                            ! SEGMENTS INTERSECT
                        hx1 = x
                        ! DRAW SEGMENT WITH
                        hy1 = y
                        ! HIGHEST START POINT
                        dx1 = x
                        ! TO THE INTERSECTION
                        dy1 = y
                        IF ( ipct>maxsize_local ) THEN
                           spag_nextblock_1 = 3
                           CYCLE SPAG_DispatchLoop_1
                        ENDIF
                        p(ipct,1) = x
                        p(ipct,2) = y
                        ipct = ipct + 1
                        IF ( ip==2 ) CALL dl_draw(x,y)
                        x0 = x
                        y0 = y
                        CYCLE
                     ENDIF
!
                     IF ( hx2<=dx2 ) THEN
                                 ! CHECKED ALL H SEGS OVER D SEGS
                        IF ( hhigh ) THEN
                               ! DRAW HIGHEST SEGMENT
                           IF ( ipct>maxsize_local ) THEN
                              spag_nextblock_1 = 3
                              CYCLE SPAG_DispatchLoop_1
                           ENDIF
                           p(ipct,1) = hx2
                           p(ipct,2) = hy2
                           ipct = ipct + 1
                           IF ( ip==3 ) CALL dl_move(x0,y0)
                           CALL dl_draw(hx2,hy2)
                           x0 = hx2
                           y0 = hy2
                           ip = 2
                        ENDIF
                        hx1 = hx2
                        hy1 = hy2
                        hx2 = h(ihct,1)
                        hy2 = h(ihct,2)
                        ihct = ihct + 1
                        IF ( ihct>ihold+1 ) THEN
                           DO
                              IF ( idct<=Nx+1 ) THEN
                                 CALL dl_vxpt3d(x,y,A(idct-1,iz),idct-1,iz,Nx)
                                                                   ! DETERMINE X,Y VALUE OF A POINT ON 3-D SURFACE
                                 IF ( ipct>maxsize_local ) THEN
                                    spag_nextblock_1 = 3
                                    CYCLE SPAG_DispatchLoop_1
                                 ENDIF
                                 p(ipct,1) = x
                                 p(ipct,2) = y
                                 ipct = ipct + 1
                                 IF ( ip==3 ) CALL dl_move(x0,y0)
                                 ip = 2
                                 CALL dl_draw(x,y)
                                 idct = idct + 1
                                 CYCLE
                              ENDIF
                              spag_nextblock_2 = 3
                              CYCLE SPAG_DispatchLoop_2
                           ENDDO
                        ENDIF
                        IF ( hx1==dx2 ) THEN
                           dx1 = dx2
                           ! NEXT DATA POINT
                           dy1 = dy2
                           x0 = dx1
                           y0 = dy1
!C                IF (.NOT.HHIGH)CALL dl_draw(DX1,DY1)
                  !write(*,*)' I IDCT,IZ=',idct,iz,inx,inz,nx,nz
                           IF ( idct>Nx ) THEN
                              dx2 = dx1
                              dy2 = aminq
                           ELSE
                              CALL dl_vxpt3d(dx2,dy2,A(idct,iz),idct,iz,Nx)
                                                                   ! DETERMINE X,Y VALUE OF A POINT ON 3-D SURFACE
                           ENDIF
                           idct = idct + 1
                           IF ( idct>Nx+2 ) THEN
                                             ! DONE WITH DATA
                              ihct = ihct - 1
                              EXIT SPAG_Loop_2_3
                           ELSE
                              hhigh = .TRUE.
                              IF ( dy1>hy1 ) hhigh = .FALSE.
                           ENDIF
                        ENDIF
                     ELSE
                        IF ( .NOT.hhigh ) THEN
                                    ! PLOT DATA THAT IS HIGHEST
                           IF ( ipct>maxsize_local ) THEN
                              spag_nextblock_1 = 3
                              CYCLE SPAG_DispatchLoop_1
                           ENDIF
                           p(ipct,1) = dx2
                           p(ipct,2) = dy2
                           ipct = ipct + 1
                           IF ( ip==3 ) CALL dl_move(x0,y0)
                           CALL dl_draw(dx2,dy2)
                           ip = 2
                           x0 = dx2
                           y0 = dy2
                        ENDIF
                        dx1 = dx2
                        ! NEXT DATA POINT
                        dy1 = dy2
               !write(*,*)'II IDCT,IZ=',idct,iz,inx,inz,nx,nz
                        IF ( idct>Nx ) THEN
                           dx2 = dx1
                           dy2 = aminq
                        ELSE
                           CALL dl_vxpt3d(dx2,dy2,A(idct,iz),idct,iz,Nx)
                                                                ! DETERMINE X,Y VALUE OF A POINT ON 3-D SURFACE
                        ENDIF
                        idct = idct + 1
                        IF ( idct>Nx+2 ) THEN
                                          ! DONE WITH DATA
                           ihct = ihct - 1
                           EXIT SPAG_Loop_2_3
                        ENDIF
!
!     DONE WITH INNER LOOP
!
                     ENDIF
                  ENDDO SPAG_Loop_2_3
                  DO WHILE ( ihct<=ihold )
                     x = h(ihct,1)
                     y = h(ihct,2)
                     ihct = ihct + 1
                     IF ( ipct>maxsize_local ) THEN
                        spag_nextblock_1 = 3
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     p(ipct,1) = x
                     p(ipct,2) = y
                     ipct = ipct + 1
!C       CALL dl_draw(X,Y)
                     idct = idct + 1
                  ENDDO
                  spag_nextblock_2 = 3
               CASE (3)
!
                  ihold = ipct - 1
                          ! STORE NEW HISTORY
                  DO i = 1 , ipct
                     h(i,1) = p(i,1)
                     h(i,2) = p(i,2)
                  ENDDO
                  EXIT SPAG_DispatchLoop_2
               END SELECT
            ENDDO SPAG_DispatchLoop_2
!
         ENDDO mainloop
         spag_nextblock_1 = 2
      CASE (2)
!
         CALL dl_move(0.,0.)
                            ! PEN UP
         RETURN
      CASE (3)
         WRITE (*,99001)
99001    FORMAT (' *** dl_slices INTERNAL MEMORY OVERFLOW ERROR ***')
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE dl_slices
!*==dl_symbol.f90 processed by SPAG 8.01RF 02:19 13 Dec 2024
SUBROUTINE dl_symbol(X,Y,S,T,A,Nn,Is)
!
!     ROUTINE TO PLOT CHARACTERS AND SYMBOLS
!
!     WRITTEN BY: D. LONG  JAN 1991,1995   BYU
!     THIS ROUTINE IS FORTRAN-77 COMPATIBLE WITH THE FOLLOWING
!     EXCEPTIONS:
!        1. INTEGER*2 ARRAYS ARE USED TO SAVE SPACE. THEY MAY
!           BE REPLACED WITH INTEGER.
!
!     MACHINE DEPENDENT NOTES:
!        1. THE FUNCTION IBITS(I,J,K) RETURNS THE VALUE OF THE BITS
!           IN I STARTING AT J FOR K BITS.
!
!     X,Y   (R): string position
!                If x>998 or y>998 then plotting of the string is continued
!           from the last DL_SYMBOL call
!     S     (R): height of the string to be printed
!     T     (C): character variable containing the ascii text to be plotted
!     A     (R): angle at which the string is to be plotted
!           counter-clockwise from x axis
!     N     (I): number of characters to use from T
!           note: plotting will terminate if an ASCII zero is
!           encountered at any other position than the first character.
!                  If N<0, a plot(x,y,2) will be executed prior to plotting
!           the first character and ABS(N) characters will be plotted.
!           For N<2, the plot pen is left at the 1st character origin
!           point; otherwise it is at the end of the last plotted
!           vector in the last plotted character.
!     IS    (I): centering option flag
!           = -3 end coordinates of string (if it were to  be
!                            plotted will be returned in x,y where the input
!                (x,y) are the lower left corner of string. This
!                permits computation of the plotted length.
!                However, no plotting is done and the last position
!                            variables are not changed.
!           = -2 end coordinates of string are returned in x,y.
!                Initial (x,y) to be lower left corner of plotted string. String is plotted.
!           = -1 (x,y) to be lower left corner of plotted string
!                            (x and y not altered)  String is plotted.
!           = 0  (x,y) to be center of plotted string
!                            (x and y not altered)  String is plotted.
!                       = 1  (x,y) to be lower right corner of plotted string
!                            (x and y not altered)  String is plotted.
!
! DL_SYMBOL plots an ASCII string in a CHARACTER array. Each character (or string
! of characters) can be imagined as a square box with the origin at the lower
! left corner. The routine determines the initial position of the lower
! left of the first character than plots each character relative to this
! position. As each character is plotted the "current position" is moved
! to the right (along the string baseline) a fixed amount S. When the
! string centering option is selected, the length of the plotted string is
! determined and, based on the character height, the lower left corner is
! computed from the input (x,y) position. The special plot symbols (ASCII
! 0-31) are always centered about the current position.
! **********************************************************************
   IMPLICIT NONE
   REAL :: A
   REAL :: aa
   REAL :: al
   REAL :: co
   INTEGER :: i
   INTEGER :: icc
   INTEGER :: il
   INTEGER :: ip
   INTEGER :: ipen
   INTEGER :: ipenlast
   INTEGER :: Is
   INTEGER :: iss
   INTEGER :: iw
   INTEGER :: ix
   INTEGER :: ixoff
   INTEGER :: iy
   INTEGER :: iyoff
   INTEGER :: n
   INTEGER :: Nn
   REAL :: oldx
   REAL :: oldy
   REAL :: ox
   REAL :: oy
   REAL :: S
   REAL :: si
   REAL :: ss
   REAL :: X
   REAL :: x0
   REAL :: x1
   REAL :: xx
   REAL :: Y
   REAL :: y0
   REAL :: y1
   CHARACTER*(*) T
   LOGICAL length
   SAVE oldx , oldy
      !INTEGER, PARAMETER :: Short   = SELECTED_INT_KIND(4)   ! Short integer
   INTEGER , PARAMETER :: SHORT = selected_int_kind(8)       ! Long integer
!
   INTEGER(KIND=SHORT) :: ifnt(968) , ipnt(176)

! ----------------------------------------------------------------------------------------------------------------------------------
INTEGER(KIND=SHORT) :: if001(88) , if002(88) , if003(88) , if004(88) , if005(88) , if006(88) , if007(88) , if008(88) , if009(88) &
& , if010(88) , if011(88)
! ----------------------------------------------------------------------------------------------------------------------------------

   INTEGER(KIND=SHORT) :: ipt001(88) , ipt002(88)
! ----------------------------------------------------------------------------------------------------------------------------------
EQUIVALENCE (ifnt(1),if001(1)) , (ifnt(89),if002(1)) , (ifnt(177),if003(1)) , (ifnt(265),if004(1)) , (ifnt(353),if005(1)) ,      &
& (ifnt(441),if006(1)) , (ifnt(529),if007(1)) , (ifnt(617),if008(1)) , (ifnt(705),if009(1)) , (ifnt(793),if010(1)) ,            &
& (ifnt(881),if011(1))
! ----------------------------------------------------------------------------------------------------------------------------------
   EQUIVALENCE (ipnt(1),ipt001(1)) , (ipnt(89),ipt002(1))
   INTEGER :: spag_nextblock_1
! ----------------------------------------------------------------------------------------------------------------------------------
DATA if001/6186 , 6826 , 6806 , 5526 , 5546 , 6186 , 2080 , 6176 , 10282 , 10538 , 10916 , 10908 , 10518 , 10006 , 9628 , 9636 , &
& 5930 , 2090 , 6176 , 6186 , 6747 , 5595 , 2090 , 6186 , 6816 , 6166 , 5536 , 6186 , 2080 , 6176 , 6688 , 5672 , 5656 ,      &
& 2592 , 6501 , 6491 , 5851 , 5861 , 14693 , 5546 , 14053 , 5526 , 14043 , 6806 , 14683 , 6176 , 2730 , 6166 , 14378 , 5536 , &
& 2720 , 5721 , 14823 , 5735 , 2521 , 5536 , 15008 , 6166 , 14378 , 5721 , 14823 , 6617 , 1639 , 5735 , 6176 , 14358 , 6176 , &
& 2535 , 6166 , 6186 , 6816 , 5536 , 2090 , 5526 , 6826 , 5546 , 2710 , 13844 , 5672 , 6696 , 5656 , 14872 , 5920 , 2336 ,    &
& 13612 , 5672 , 6680 , 6696/
! ----------------------------------------------------------------------------------------------------------------------------------
DATA if002/1560 , 6176 , 14872 , 6696 , 6176 , 1568 , 5672 , 6696 , 5656 , 6680 , 1576 , 6176 , 6680 , 6696 , 5656 , 5672 ,      &
& 2080 , 6176 , 6186 , 6747 , 5595 , 14378 , 6757 , 6166 , 5605 , 2661 , 15124 , 6696 , 5672 , 6680 , 13848 , 6432 , 1824 ,   &
& 6696 , 6680 , 5672 , 5656 , 14888 , 6180 , 2075 , 5656 , 6696 , 5672 , 6680 , 13848 , 6432 , 1824 , 5536 , 6186 , 6816 ,    &
& 6166 , 5536 , 15008 , 6186 , 2070 , 5656 , 5672 , 6696 , 6680 , 5656 , 14888 , 5672 , 2584 , 6176 , 5928 , 6440 , 6176 ,    &
& 6688 , 6424 , 6176 , 5912 , 5664 , 2080 , 10204 , 10077 , 10015 , 10017 , 10083 , 10212 , 10340 , 10467 , 10529 , 10527 ,   &
& 10461 , 10332 , 14300 , 5983 , 5985/
!-----------------------------------------------------------------------------------------------------------------------------------
DATA if003/14177 , 6046 , 14242 , 6109 , 14307 , 6173 , 14371 , 6237 , 14435 , 6302 , 14498 , 6367 , 2273 , 5916 , 5924 , 6436 , &
& 6428 , 14108 , 5981 , 14179 , 6045 , 14243 , 6109 , 14307 , 6173 , 14371 , 6237 , 14435 , 6301 , 14499 , 6365 , 2275 ,      &
& 6170 , 5859 , 6499 , 14362 , 5986 , 6173 , 14562 , 6176 , 6114 , 6176 , 2146 , 10273 , 10208 , 10271 , 10336 , 2081 ,       &
& 10204 , 10077 , 10015 , 10017 , 10083 , 10212 , 10340 , 10467 , 10529 , 10527 , 10461 , 10332 , 2012 , 10133 , 9942 , 9752 ,&
& 9627 , 9566 , 9570 , 9637 , 9768 , 9962 , 10155 , 10411 , 10602 , 10792 , 10917 , 10978 , 10974 , 10907 , 10776 , 10582 ,   &
& 10389 , 1941 , 10122 , 9803 , 9549 , 9359 , 9170 , 9045/
!-----------------------------------------------------------------------------------------------------------------------------------
DATA if004/8921 , 8862 , 8866 , 8935 , 9067 , 9198 , 9393 , 9587 , 9845 , 10166 , 10422 , 10741 , 10995 , 11185 , 11374 , 11499 ,&
& 11623 , 11682 , 11678 , 11609 , 11477 , 11346 , 11151 , 10957 , 10699 , 10378 , 1930 , 6186 , 5527 , 6743 , 2090 , 1931 ,   &
& 9163 , 9355 , 9359 , 9167 , 13259 , 5138 , 5088 , 5280 , 1042 , 5081 , 13280 , 5472 , 1369 , 5067 , 13280 , 5472 , 13643 ,  &
& 4825 , 13913 , 5714 , 722 , 4815 , 9551 , 9810 , 9557 , 9173 , 4825 , 5084 , 13916 , 5280 , 1163 , 4815 , 13916 , 9180 ,    &
& 8924 , 8928 , 9184 , 13276 , 9803 , 9547 , 9551 , 9807 , 1611 , 9810 , 9355 , 9163 , 8911 , 8914 , 9561 , 9564 , 9376 ,     &
& 9180 , 9173 , 1611 , 9177 , 9372/
!-----------------------------------------------------------------------------------------------------------------------------------
DATA if005/1184 , 9568 , 9177 , 9170 , 1355 , 9184 , 9561 , 9554 , 971 , 5263 , 13468 , 5721 , 13010 , 4825 , 1618 , 5263 ,      &
& 13468 , 4821 , 1621 , 8905 , 9165 , 975 , 4821 , 1621 , 5068 , 971 , 4811 , 1632 , 8911 , 8924 , 9184 , 9568 , 5724 , 9807 ,&
& 9547 , 5067 , 4815 , 1628 , 5067 , 13643 , 5259 , 5280 , 988 , 8924 , 9184 , 9568 , 9820 , 9817 , 4815 , 4811 , 1611 ,      &
& 4832 , 9824 , 5724 , 9365 , 9557 , 9810 , 9807 , 9547 , 9163 , 719 , 5451 , 5472 , 4821 , 4818 , 1618 , 8911 , 9163 , 9547 ,&
& 9807 , 9813 , 5465 , 4825 , 4832 , 1632 , 8917 , 9557 , 9810 , 9807 , 9547 , 9163 , 8911 , 8921 , 9376 , 1632 , 4832 ,      &
& 5728 , 9820/
!-----------------------------------------------------------------------------------------------------------------------------------
DATA if006/9170 , 971 , 9163 , 9547 , 9807 , 9810 , 9557 , 9173 , 8921 , 8924 , 9184 , 9568 , 9820 , 9817 , 13653 , 9173 , 8914 ,&
& 8911 , 971 , 8907 , 9355 , 9810 , 9820 , 9568 , 9184 , 8924 , 8921 , 9173 , 1621 , 5068 , 13259 , 5073 , 978 , 13003 ,      &
& 9163 , 9359 , 13458 , 5272 , 1177 , 5451 , 4821 , 1376 , 4818 , 13906 , 5721 , 729 , 13003 , 5067 , 5717 , 992 , 5260 ,     &
& 13451 , 9362 , 9817 , 9820 , 9568 , 9184 , 732 , 9547 , 9163 , 8911 , 8924 , 9184 , 9568 , 9820 , 9810 , 9362 , 9365 ,      &
& 1628 , 4811 , 8921 , 5280 , 5721 , 13899 , 4818 , 1618 , 4811 , 4832 , 9568 , 9820 , 5721 , 5461 , 13013 , 9557 , 9810 ,    &
& 5711 , 5451 , 715/
!-----------------------------------------------------------------------------------------------------------------------------------
DATA if007/9820 , 9568 , 9184 , 8924 , 8911 , 9163 , 9547 , 1615 , 4811 , 9547 , 9807 , 5724 , 5472 , 13024 , 5088 , 971 , 4811 ,&
& 4832 , 13920 , 5461 , 13013 , 4811 , 1611 , 4811 , 4832 , 13920 , 5269 , 725 , 9820 , 9568 , 9184 , 8924 , 8911 , 9163 ,    &
& 5707 , 5714 , 1362 , 4811 , 13024 , 4821 , 13909 , 5728 , 1611 , 5067 , 13643 , 5259 , 13472 , 5088 , 1376 , 8911 , 9163 ,  &
& 5451 , 5711 , 1632 , 4811 , 13024 , 5728 , 4821 , 1611 , 4832 , 4811 , 1611 , 4811 , 4832 , 5269 , 5728 , 1611 , 4811 ,     &
& 13024 , 4828 , 13903 , 5728 , 1611 , 8911 , 9163 , 9547 , 9807 , 9820 , 9568 , 9184 , 8924 , 719 , 4811 , 4832 , 9568 ,     &
& 9820 , 5721 , 5461/
!-----------------------------------------------------------------------------------------------------------------------------------
DATA if008/725 , 8911 , 9163 , 9547 , 9807 , 9820 , 9568 , 9184 , 8924 , 13007 , 5266 , 1611 , 4811 , 4832 , 9568 , 9820 , 5721 ,&
& 5461 , 13013 , 5269 , 1611 , 8911 , 9163 , 9547 , 9807 , 9810 , 9557 , 9173 , 8921 , 8924 , 9184 , 9568 , 1628 , 4832 ,     &
& 13920 , 5280 , 1163 , 4832 , 8911 , 9163 , 5451 , 5711 , 1632 , 4832 , 5259 , 1632 , 4832 , 5067 , 5269 , 5451 , 1632 ,     &
& 4811 , 4815 , 5724 , 13920 , 4832 , 4828 , 5711 , 1611 , 4832 , 4828 , 5269 , 13451 , 5728 , 5724 , 1173 , 4832 , 5728 ,    &
& 5724 , 4815 , 4811 , 1611 , 5280 , 4832 , 4811 , 1163 , 5707 , 736 , 4811 , 5259 , 5280 , 736 , 4821 , 5276 , 13909 , 5276 ,&
& 1167 , 5263/
!-----------------------------------------------------------------------------------------------------------------------------------
DATA if009/4821 , 13468 , 4821 , 1621 , 4832 , 1365 , 8911 , 8917 , 9177 , 9369 , 9810 , 9355 , 9163 , 13007 , 5721 , 1611 ,     &
& 4811 , 13024 , 8914 , 9369 , 9561 , 9813 , 9807 , 9547 , 9355 , 722 , 5721 , 9177 , 8917 , 4815 , 5067 , 1611 , 5728 ,      &
& 13899 , 9810 , 9369 , 9177 , 8917 , 8911 , 9163 , 9355 , 1618 , 4818 , 9810 , 9813 , 9561 , 9177 , 8917 , 8911 , 9163 ,     &
& 1355 , 5259 , 5276 , 13664 , 5077 , 1365 , 9156 , 5444 , 5704 , 13913 , 9810 , 9369 , 9177 , 8917 , 8911 , 9163 , 9355 ,    &
& 1618 , 4811 , 13024 , 8921 , 5465 , 5717 , 1611 , 5067 , 13643 , 5259 , 5269 , 13269 , 5275 , 1180 , 5061 , 5253 , 5449 ,   &
& 13657 , 5471 , 1376 , 4811/
!-----------------------------------------------------------------------------------------------------------------------------------
DATA if010/13024 , 5724 , 13007 , 5269 , 1611 , 5067 , 13643 , 5259 , 5280 , 992 , 4811 , 13017 , 8917 , 5081 , 5269 , 13451 ,   &
& 9365 , 5465 , 5717 , 1611 , 4811 , 13017 , 8917 , 9177 , 5465 , 5717 , 1611 , 8911 , 8917 , 9177 , 9561 , 9813 , 9807 ,     &
& 9547 , 9163 , 719 , 4804 , 13017 , 8914 , 9369 , 9561 , 9813 , 9807 , 9547 , 9355 , 722 , 5700 , 13913 , 9810 , 9369 ,      &
& 9177 , 8917 , 8911 , 9163 , 9355 , 1618 , 4811 , 13017 , 8914 , 5273 , 1625 , 8907 , 9547 , 9807 , 9554 , 9170 , 8917 ,     &
& 9177 , 1625 , 5081 , 13657 , 5280 , 5263 , 1355 , 4825 , 8911 , 9163 , 9547 , 13903 , 5721 , 1611 , 4825 , 5259 , 1625 ,    &
& 4825 , 5067 , 5266 , 5451/
!-----------------------------------------------------------------------------------------------------------------------------------
DATA if011/1625 , 4811 , 13913 , 4825 , 1611 , 9156 , 5444 , 5704 , 13913 , 4825 , 8911 , 9163 , 1611 , 4825 , 5721 , 4811 ,     &
& 1611 , 5259 , 5007 , 5075 , 4885 , 5080 , 5020 , 1184 , 5259 , 13458 , 5273 , 1184 , 5067 , 5327 , 5267 , 5461 , 5272 ,     &
& 5340 , 992 , 4819 , 5079 , 5459 , 1623 , 131 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 ,  &
& 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0/
!-----------------------------------------------------------------------------------------------------------------------------------
DATA ipt001/1 , 8 , 19 , 24 , 30 , 35 , 48 , 52 , 56 , 64 , 69 , 74 , 78 , 85 , 90 , 95 , 100 , 106 , 115 , 122 , 129 , 136 ,    &
& 144 , 152 , 162 , 190 , 209 , 220 , 225 , 238 , 259 , 292 , 296 , 297 , 306 , 310 , 318 , 328 , 340 , 351 , 354 , 358 ,     &
& 362 , 368 , 372 , 375 , 377 , 379 , 381 , 391 , 396 , 404 , 414 , 419 , 428 , 438 , 443 , 460 , 470 , 474 , 480 , 483 ,     &
& 487 , 491 , 499 , 510 , 517 , 529 , 537 , 545 , 552 , 557 , 566 , 572 , 578 , 583 , 588 , 591 , 596 , 602 , 611 , 618 ,     &
& 629 , 638 , 650 , 654 , 660 , 663/
!-----------------------------------------------------------------------------------------------------------------------------------
DATA ipt002/668 , 676 , 683 , 689 , 693 , 695 , 699 , 704 , 709 , 711 , 721 , 731 , 737 , 747 , 756 , 761 , 773 , 779 , 786 ,    &
& 792 , 798 , 803 , 813 , 820 , 829 , 839 , 849 , 854 , 862 , 867 , 874 , 877 , 882 , 886 , 894 , 898 , 905 , 909 , 916 ,     &
& 920 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 ,   &
& 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!-----------------------------------------------------------------------------------------------------------------------------------
         n = Nn      ! NUMBER OF CHARACTERS
         aa = A      ! PLOTTING ANGLE
         si = sin(aa*0.0174532)
         co = cos(aa*0.0174532)
         length = .TRUE. ! PLOT (TRUE) OR LENGTH ONLY
         al = 0.0       ! PLOTTED LENGTH
         iss = Is       ! CENTERING FLAG
         IF ( iss==-3 ) length = .FALSE.
         IF ( iss>-1 ) length = .FALSE.
         ox = oldx      ! SAVE CURRENT POSITION
         oy = oldy
         spag_nextblock_1 = 2
      CASE (2)
         al = 0.0       ! LENGTH OF PLOTTED STRING ACCUMULATOR
         x1 = X      ! LOWER LEFT CORNER
         y1 = Y
         IF ( iss==0 ) THEN
                          ! CENTERED
            x1 = X - al/2.*co + S/2.*si
            y1 = Y - S/2.*co - al/2.*si
         ENDIF
         IF ( iss==1 ) THEN
                          ! LOWER RIGHT CORNER
            x1 = X - al*co
            y1 = Y - al*si
         ENDIF
         IF ( X>998.0 .OR. Y>998.0 ) THEN
            IF ( X<998.0 ) oldx = oldx + x1
            IF ( Y<998.0 ) oldy = oldy + y1
         ELSE
            oldx = x1
            oldy = y1
         ENDIF
         x0 = oldx
         y0 = oldy
         IF ( length .AND. n<0 ) CALL dl_draw(oldx,oldy)
                                                     ! PLOT TO START
         ss = S/21.
                  ! SCALE FACTOR
         i = 0    ! CHARACTER COUNTER
         SPAG_Loop_1_1: DO
            i = i + 1
            IF ( i>iabs(n) ) EXIT SPAG_Loop_1_1
                                   ! END OF STRING COUNT
            icc = ichar(T(i:i))
                            ! GET ITH ASCII CHARACTER
            IF ( icc<=127 ) THEN  ! CODE TO LARGE
               IF ( icc==0 .AND. i>1 ) EXIT SPAG_Loop_1_1
                                            ! END OF STRING REACHED
               ixoff = 11
                        ! OFFSET
               iyoff = 11
               IF ( icc<32 ) THEN
                              ! DIFFERENT SYMBOL OFFSET
                  ixoff = 32
                  iyoff = 32
               ENDIF
               il = ipnt(icc+1)
                          ! STARTING INDEX
               iw = 21  ! CHARACTER WIDTH
               IF ( il/=0 ) THEN
                               ! NO PLOTTING INFO
                  ipenlast = 3
                  SPAG_Loop_2_2: DO
                     iy = ibits(ifnt(il),0,6)
                     ix = ibits(ifnt(il),6,6)
                     ipen = ibits(ifnt(il),12,2)
                     ip = ipenlast
                     ipenlast = ipen
                     xx = ss*(ix-ixoff)
!c       Y1=SS*(IY-IYOFF+ISUB)
                     y1 = ss*(iy-iyoff)
                     x1 = xx*co - y1*si + oldx
                     y1 = xx*si + y1*co + oldy
                     IF ( ip==0 ) ip = 2
                     IF ( ip==1 ) ip = 2
                     IF ( length ) CALL dl_plot(x1,y1,ip)
                     il = il + 1
                     IF ( ipen==0 ) EXIT SPAG_Loop_2_2
                  ENDDO SPAG_Loop_2_2
               ENDIF
               xx = ss*iw
                        ! END OF CHARACTER
               al = al + ss*iw
               oldx = xx*co + oldx
               oldy = xx*si + oldy
            ENDIF
         ENDDO SPAG_Loop_1_1
         IF ( .NOT.length ) THEN
                            ! FINISHED LENGTH-ONLY PASS
            length = .TRUE.
            IF ( iss==-3 ) THEN
                             ! RETURN END POSITION
               X = oldx
               Y = oldy
            ENDIF
            oldx = ox
                     ! RESTORE OLD POSITION
            oldy = oy
            IF ( iss==0 .OR. iss==1 ) THEN
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ELSE
            IF ( n<=1 ) CALL dl_move(x0,y0)
                                         ! LEAVE PEN AT START
            IF ( iss==-2 ) THEN
                             ! RETURN END POSITION
               X = oldx
               Y = oldy
            ENDIF
         ENDIF
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE dl_symbol
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
end module M_pixel__slices
