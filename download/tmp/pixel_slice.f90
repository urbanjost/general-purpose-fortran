!>
!!##NAME
!!    pixel_slice(3f) - [M_pixel] simplified call of DL_SLICES(3f) to make a plot of a 3-d surface
!!
!!##SYNOPSIS
!!
!!   subroutine pixel_SLICE(surfdat,kix,kiz,nx,nz,a0,b0)
!!
!!    real,intent(in)     :: SURFDAT(kix,kiz)
!!    integer,intent(in)  :: kix
!!    integer,intent(in)  :: kiz
!!    integer,intent(in)  :: nx
!!    integer,intent(in)  :: nz
!!    real,intent(in)     :: a0
!!    real,intent(in)     :: b0
!!
!!##DESCRIPTION
!!    Simplified call to DL_SLICES(3f) to make a plot of a 3-d surface
!!
!!##OPTIONS
!!    SURFDAT(kix,kiz)  array of y values
!!    KIX,KIZ           dimension of surfdat array
!!    NX,NZ             size of surface to plot
!!    A0                initial angle of x axis from horizontal 0-80 degrees
!!    B0                initial angle of z axis from horizontal 5-80 degrees
!!
!!##EXAMPLE
!!
!===================================================================================================================================
subroutine pixel_SLICE(surfdat,kix,kiz,nx,nz,a0,b0)
   use M_pixel
   use :: M_pixel_slices, only : dl_slices, dl_init, dl_symbol
character(len=*),parameter::ident="@(#)M_pixel::pixel_slice(3f): simplified call of dl_slices to plot a 3d-surface"
!-----------------------------------------------------------------------------------------------------------------------------------
   real,intent(in)              :: SURFDAT(kix,kiz) ! array of y values
   integer,intent(in)           :: kix              ! x dimension of surfdat array
   integer,intent(in)           :: kiz
   integer,intent(in)           :: nx               ! x size of surface to plot
   integer,intent(in)           :: nz
   real,intent(in)              :: a0               ! initial angle of x axis from horizontal 0-80 degrees
   real,intent(in)              :: b0               ! initial angle of z axis from horizontal 5-80 degrees
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
XS=-10.0               ! xs,xe (R): starting and ending values displayed on x axis
XE=10.0
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
XT='dl_slices X TITLE' ! xt    (C): title of x axis (width)
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
YT='dl_slices Y TITLE' ! yt    (C): title of y axis (width)
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
ZT='SLICE'             ! zt    (C): title of z axis (width)
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
   A=30.0
   B=40.0
   A_OLD=A+1.0
   B_OLD=B+1.0

   DRAWPLOT: DO
      CALL COLOR(7)
      CALL CLEAR()
      CALL COLOR(0)
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
         CALL LINEWIDTH(3)
         CALL COLOR(4)
         CALL DL_SYMBOL(0.0,0.0,0.25,'dl_slices',0.0,9,-1)
      else
         write(*,*)'DUPLICATE ',a,b
      endif
      A_OLD=A
      B_OLD=B
      write(*,'(a)',advance='no')'Enter (lrud h q):'
      read(*,'(a)',iostat=ios)KEY ! wait till keypress is read in graphic window
      SELECTCASE (KEY)
      CASE ('l')
         a=a-1
         a=max(0.0,a)
         write(*,*)'l: a=',a
      CASE ('r')
         a=a+1
         a=min(80.0,a)
         write(*,*)'r: a=',a
      CASE ('u')
         b=b+1.0
         b=min(80.0,b)
         write(*,*)'u: b=',b
      CASE ('d')
         b=b-1
         b=max(5.0,b)
         write(*,*)'d: b=',b
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
         write(*,*)'x-axis angle=',A,' from 0 to 80'
         write(*,*)'z-axis angle=',B,' from 5 to 80'
      END SELECT
      write(*,*)' x-axis angle=',A,' z-axis angle=',B
!-----------------------------------------------------------------------------------------------------------------------------------
   enddo DRAWPLOT
END subroutine pixel_slice
