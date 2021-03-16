! ==================================================================================================================================
module m_slices
!
! This module intentionally duplicates graphics routines used in other high-level routines so that the DL_SLICES routine
! is essentially self-contained except for very low-level external graphics functions. This is so this routine can be
! developed and ported to other underlying graphics libraries.
!
implicit none
PRIVATE
!-----------------------------------------------------------------------------------------------------------------------------------
PUBLIC  :: DL_INIT           ! (XMAX0,YMAX0,VPX,VPY,ZOM)
PUBLIC  :: DL_SYMBOL         ! (X,Y,S,T,A,NN,IS)
PUBLIC  :: DL_SLICES         ! DL_SLICES(A,INX,INZ,NX,NZ,ALPHA,BETA,XH,YH,ZH,IFLAG,IAXIS,
                             !           XT,NXT,XASTART,XAEND,NMX,NNX,MLX,TSX,NDX,SMX,
                             !           YT,NYT,              NMY,NNY,MLY,TSY,NDY,SMY,
                             !           ZT,NZT,ZASTART,ZAEND,NMZ,NNZ,MLZ,TSZ,NDZ,SMZ,
                             !           AMININ,AMAXIN,ICOL)
public test_suite_M_slices
!-----------------------------------------------------------------------------------------------------------------------------------
PRIVATE :: vxpt3_          ! (X,Y,AVAL,IX,IZ,NX)
PRIVATE :: intersect_      ! (FLAG,X,Y,AX1,AY1,AX2,AY2,BX1,BY1,BX2,BY2,A)
PRIVATE :: clipit_         ! (IVTB,XV2,YV2,AV1,AV2,XM,YM,XX,YX)
PRIVATE :: trs_            ! (XIN,YIN,XCON,YCON)
PRIVATE :: inbox_          ! (X,Y,X_BOTTOM_LEFT,Y_BOTTOM_LEFT,X_top_right,Y_top_right)
!------------------------
PRIVATE :: axisb_          ! (X0,Y0,A0,N0,S0,T0,C0,D0,NM,NN,ML,TS,ND,SM,ICOL)
PRIVATE :: axisa_          ! (X0,Y0,A0,N0,S0,T0,C0,D0,NM,ML,ICOL)
!------------------------
PRIVATE :: number_         ! (X,Y,HGHT,Z,T,F0,IPF)
PRIVATE :: range_          ! (X,S,N,K,IX,XMIN,DX)
!------------------------
PRIVATE :: translate_      ! (XA,YA)
PRIVATE :: viewport_       ! (XMIN,XMAX,YMIN,YMAX)
PRIVATE :: color_          ! (IC)
PRIVATE :: width_          ! (IC)
PRIVATE :: draw_           ! (XA,YA)
PRIVATE :: move_           ! (XA,YA)
PRIVATE :: plot_           ! (XPLOT0,YPLOT0,ISELECT0)
!-----------------------------------------------------------------------------------------------------------------------------------
! plot coordinate system
real,save    :: TRANSLATEXQ                ! TRANSLATEXQ  SCALED ROTATED ORIGIN X VALUE
real,save    :: TRANSLATEYQ                ! TRANSLATEYQ  SCALED ROTATED ORIGIN Y VALUE
real,save    :: XMINQ,YMINQ,XMAXQ,YMAXQ
real,save    :: SCALEQ                     ! ZOOM SCALE FACTOR
real,save    :: XLASTSCALEQ                ! LAST SCALED, SHIFTED X VALUE
real,save    :: YLASTSCALEQ                ! LAST SCALED, SHIFTED Y VALUE
real,save    :: ANGLEQ                     ! PLOTTING ANGLE
real,save    :: VIEWPORTQ(4)               ! VIEWPORTQ(4) VIEWPORT PARAMETERS
!-----------------------------------------------------------------------------------------------------------------------------------
real,save    :: XSCALEQ,YSCALEQ,ZSCALEQ,AMINQ,ALPHQ,BETQ
!-----------------------------------------------------------------------------------------------------------------------------------
contains
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
! NAME
!    dl_slices(3f) - [M_slices] plot data in 3-D overlay form
!    (LICENSE:PD)
! SYNOPSIS
!    subroutine dl_slices(a,inx,inz,nx,nz,alpha,beta,xh,yh,zh,iflag,iaxis, &
! 
!                          & xt,nxt,xastart,xaend,nmx,nnx,mlx,tsx,ndx,smx, &
!                          & yt,nyt,              nmy,nny,mly,tsy,ndy,smy, &
!                          & zt,nzt,zastart,zaend,nmz,nnz,mlz,tsz,ndz,smz, &
!                          & aminin,amaxin,icol,maxsize)
! 
! DESCRIPTION
!    Routine to plot data in 3-D overlay form.
! 
! OPTIONS
! 
!    coordinate system is:  Y  Z
! 
!      A           array a(inx,inz) containing vertical height data
!      INX,INZ     integers dimension of A array
!      NX,NZ       integers indicating size of A array to plot
!      ALPHA       real angle (in degrees) of x axis (NX) from horizontal
!      BETA        real angle (in degrees) of z axis (NZ) from horizontal
!      XH,YH,ZH    real length of each axis
!      IFLAG       integer
!                     (one's digit)  = 2 use pen color control array
!                                    = 1 do not use pen color array
!                     (ten's digit)  = 0 plot side plates
!                                    = 1 do not plot side plates
!      IAXIS       integer axis option flag
!                     = 0 do not plot axis
!                         --following variables not accessed
!                     < 0 plot axis, use input y axis scale
!                         --following variables accessed
!                     > 0 plot axis, use computed y axis scale
!                         --following variables accessed
!                     (one's digit)  = 1 plot axis, y axis scale
!                                      --variables accessed
!                                    = 2 plot axis, auto scale y axis
!                                      --variables accessed
!                     (ten's digit)  = 0 default axis parameters
!                                    = 1 specialized axisb_ parameters
!      XT,YT,ZT          char strings for axis titles
!      NXT,NYT,NZT       int  length of axis titles.
!                        if zero then that axis not plotted
!      XASTART,ZASTART   real axis start values
!      XAEND,ZAEND       real axis end values
! 
!  following only accessed if ten's digit of iflag=1
! 
!      NMX,NMY,NMZ     int number of minor ticks between major ticks
!      NNX,NNY,NNZ     int highlight length of nnx-th minor tick on axis
!      MLX,MLY,MLZ     int number of major tick marks on axis
!      TSX,TSY,TSZ     real size of title and numbers of axis.
!                      if less than zero do not auto-scale by (x10^power)
!      NDX,NDY,NDZ     int number of digits to right of decimal point
!      SMX,SMY,SMZ     real major tick length
!      AMININ,AMAXIN   real yaxis scaling factors (only needed if iaxis < 0)
!      ICOL            integer color control (accessed if mag(iflag)=2)
! 
!                        icol(1) axis line
!                        icol(2) axis numbers
!                        icol(3) axis title
!                        icol(4) axis exponent
!                        icol(5) plot
!      maxsize         size for working array
SUBROUTINE dl_slices(a,INX,INZ,NX,NZ,ALPHA,BETA,XH,YH,ZH,IFLAG,IAXIS,XT,NXT, &
                    & XASTART,XAEND,NMX,NNX,MLX,TSX,NDX,SMX,&
                    & YT,NYT,NMY,NNY,MLY,TSY,NDY,SMY, ZT,NZT,ZASTART,ZAEND, &
                    & NMZ,NNZ,MLZ,TSZ,NDZ,SMZ, AMININ,AMAXIN,ICOL,maxsize)
!
!     CREATED BY D. LONG     APR, 1984 AT JPL
!     REVISED BY D. LONG     MAY, 1986
!     +REDUCED REDUNDANT PEN MOTIONS AND CORRECTED SOME MINOR BUGS
!
!     COORDINATE SYSTEM IS:  Y  Z  X
!
implicit none

! ident_1="@(#)M_slices::dl_slices(3f): Routine to plot data in 3-D overlay form."

integer                     :: inx
integer                     :: inz
real,intent(in)             :: A(INX,INZ)
integer,intent(in),optional :: maxsize
integer,intent(in)          :: ICOL(*)

integer                     :: maxsize_local
real                        :: AS(2)
integer                     :: IC(4)
real,allocatable            :: h(:,:)
real,allocatable            :: p(:,:)
CHARACTER(len=*)            :: XT,YT,ZT
LOGICAL                     :: FLAG,HHIGH
real,parameter              :: TPI= 3.141592654
!!PARAMETER (MAXSIZE=204800)
!!DIMENSION H(MAXSIZE,2),P(MAXSIZE,2)
integer :: i
integer :: iaf
integer :: iaxis
integer :: idct
integer :: iflag
integer :: iflag1
integer :: iflag10
integer :: ihct
integer :: ihold
integer :: ip
integer :: ipct
integer :: ipen
integer :: ix
integer :: iz
integer :: mlx
integer :: mly
integer :: mlz
integer :: n1
integer :: n2
integer :: nadd
integer :: ndx
integer :: ndy
integer :: ndz
integer :: nmx
integer :: nmy
integer :: nmz
integer :: nnx
integer :: nny
integer :: nnz
integer :: nx
integer :: nxt
integer :: nyt
integer :: nz
integer :: nzt
real :: alpha
real :: amax
real :: amaxin
real :: amh
real :: aminin
real :: ang
real :: beta
real :: bh
real :: daa
real :: dx
real :: dx1
real :: dx2
real :: dy
real :: dy1
real :: dy2
real :: dz
real :: hx1
real :: hx2
real :: hy1
real :: hy2
real :: smx
real :: smy
real :: smz
real :: tsx
real :: tsy
real :: tsz
real :: x
real :: x0
real :: xaend
real :: xastart
real :: xh
real :: xlen
real :: xp
real :: xp1
real :: xp2
real :: y
real :: y0
real :: yh
real :: ylen
real :: yp
real :: yp1
real :: yp2
real :: zaend
real :: zastart
real :: zh
real :: zlen
   if(present(maxsize))then
      maxsize_local=maxsize
   else
      maxsize_local=204800
   endif
   allocate(h(maxsize_local,2))
   allocate(p(maxsize_local,2))
!
   ALPHQ=ALPHA*TPI/180.0  ! X-AXIS INCLINATION 0-80 DEGS
   BETQ=BETA*TPI/180.0    ! Z-AXIS ANGLE 5-80 DEGS
!
   IF (IAXIS.LT.0) THEN
      AMAX=AMAXIN
      AMINQ=AMININ
   ELSE
      AMAX=A(1,1)
      AMINQ=A(1,1)
      DO IZ=1,NZ      ! DETERMINE MAX,MIN ARRAY VALUES
         DO IX=1,NX
            AMAX=AMAX1(AMAX,A(IX,IZ))
            AMINQ=AMIN1(AMINQ,A(IX,IZ))
         enddo
      enddo
   ENDIF
   IF(ALPHA.LT.0..OR.ALPHA.GT.88..OR.BETA.LT.1..OR.BETA.GT.90.)THEN
      WRITE(*,*)'("*** dl_slices INPUT ANGLE ERROR ***") ALPHA=',ALPHA,'(allowed 0 to 88) BETA=',BETA,'(allowed 1 to 90)'
      RETURN
   ENDIF
   IF (AMAX.EQ.AMINQ) THEN
      WRITE(*,'(" *** dl_slices SCALE ERROR *** MAX=MIN")')
      AMAX=AMINQ+1.0
   ENDIF
!
   XLEN=ABS(XH)
   XSCALEQ=XLEN/FLOAT(NX-1)
   ZLEN=ABS(ZH)
   ZSCALEQ=ZLEN/FLOAT(NZ-1)
   YLEN=ABS(YH)
   IF (MOD(IABS(IAXIS),10).EQ.2) THEN ! SMOOTH SCALE FACTORS
      AS(1)=AMAX
      AS(2)=AMINQ
      CALL range_(AS,YLEN,2,1,1,AMINQ,DAA)
      AMAX=YLEN*DAA+AMINQ
   ENDIF
   YSCALEQ=1.0
   IF (AMAX-AMINQ.NE.0.0) YSCALEQ=YLEN/(AMAX-AMINQ)
!
!     INITIALIZE PLOT PACKAGE
!
   IAF=IABS(IAXIS)/10

   iflag1=IABS(IFLAG)
   iflag10=MOD(iflag1,100)/10
   iflag1=MOD(iflag1,10)

   IF (IAXIS.NE.0) THEN  ! PLOT AXIS LABELS
      NADD=0
      IF (iflag1.EQ.2) THEN
         IC(1)=ICOL(2)
         IC(2)=ICOL(3)
         IC(3)=ICOL(4)
         IC(4)=ICOL(5)
         NADD=100000 ! PEN COLOR
      ENDIF
      CALL vxpt3_(XP,YP,AMINQ,1,1,NX) ! DETERMINE X,Y VALUE OF A POINT ON 3-D SURFACE
      DY=(AMAX-AMINQ)/YLEN
      IF (NYT.GT.0) THEN  ! PLOT Y AXIS
         IF (IAF.EQ.1) THEN
            CALL axisb_(XP,YP,YT,NYT+11000+NADD, YLEN,90.,AMINQ,DY,NMY,NNY,-IABS(MLY), TSY,NDY,SMY,IC)
         ELSE
            CALL axisa_(XP,YP,YT,NYT+1000+NADD, YLEN,90.,AMINQ,DY,N1,N2,IC)
         ENDIF
      ENDIF
      CALL vxpt3_(XP1,YP1,AMINQ,NX,1,NX) ! DETERMINE X,Y VALUE OF A POINT ON 3-D SURFACE
      DX=(XAEND-XASTART)/XLEN
      ANG=ATAN2(YP1-YP,XP1-XP)*180./TPI
      IF (NXT.GT.0) THEN
         IF (IAF.EQ.1) THEN
            CALL axisb_(XP,YP,XT,-NXT-NADD-10000,XLEN,ANG,XASTART,DX,NMX,NNX,-IABS(MLX),TSX,NDX,SMX,IC)
         ELSE
            CALL axisa_(XP,YP,XT,-NXT-NADD,XLEN,ANG,XASTART,DX,N1,N2,IC)
         ENDIF
      ENDIF
      DZ=(ZAEND-ZASTART)/ZLEN
      IF (NZT.GT.0) THEN
         IF (IAF.EQ.1) THEN
            CALL axisb_(XP1,YP1,ZT,-NZT-NADD-10000 ,ZLEN,BETA,ZASTART,DZ,NMZ,NNZ, -IABS(MLZ),TSZ,NDZ,SMZ,IC)
         ELSE
            CALL axisa_(XP1,YP1,ZT,-NZT-NADD, ZLEN,BETA,ZASTART,DZ,N1,N2,IC)
         ENDIF
      ENDIF
   ENDIF
   IF (iflag1.EQ.2) CALL color_(ICOL(5)) ! PEN COLOR
!
!     PLOT FRONT PLATE
!
   IPEN=3
   DO I=1,NX
      IF (I.GT.MAXSIZE_local) GOTO 999
      CALL vxpt3_(H(I,1),H(I,2),A(I,1),I,1,NX) ! INITIALIZE HISTORY ! DETERMINE X,Y VALUE OF A POINT ON 3-D SURFACE
      CALL plot_(H(I,1),H(I,2),IPEN)   ! PLOT SIDE LINE
      IPEN=2
   enddo
   IHOLD=NX
   IF (BETA.EQ.90.0) GOTO 5

   IF (iflag10.EQ.1) GOTO 71   ! DON'T PLOT SIDE PLATES
   CALL vxpt3_(XP,YP,AMINQ,NX,1,NX) ! DETERMINE X,Y VALUE OF A POINT ON 3-D SURFACE
   CALL draw_(XP,YP)
   DO I=1,NX-1      ! ADD SIDE LINES
      CALL move_(H(I,1),H(I,2))
      CALL vxpt3_(XP,YP,AMINQ,I,1,NX) ! DETERMINE X,Y VALUE OF A POINT ON 3-D SURFACE
      CALL draw_(XP,YP)
      CALL vxpt3_(XP,YP,AMINQ,I+1,1,NX) ! DETERMINE X,Y VALUE OF A POINT ON 3-D SURFACE
      CALL draw_(XP,YP)
   enddo
!
!     PLOT SIDE PLATE
!
71 continue
   CALL move_(H(NX,1),H(NX,2))
   DO I=1,NZ        ! PLOT RIGHT SIDE CURVE
      IF (NX+I.GT.MAXSIZE_local) GOTO 999
      CALL vxpt3_(XP,YP,A(NX,I),NX,I,NX) ! DETERMINE X,Y VALUE OF A POINT ON 3-D SURFACE
      H(NX+I,1)=XP
      H(NX+I,2)=YP
      CALL draw_(XP,YP)
   enddo
   CALL vxpt3_(XP,YP,AMINQ,NX,1,NX) ! DETERMINE X,Y VALUE OF A POINT ON 3-D SURFACE
   CALL move_(XP,YP)
   IHOLD=NX+NZ        ! NUMBER OF H VALUES
   IF (iflag10.NE.1) then! DON'T PLOT SIDE PLATES
      DO I=2,NZ        ! ADD SIDE LINES
         CALL vxpt3_(XP2,YP2,AMINQ,NX,I,NX)  ! DETERMINE X,Y VALUE OF A POINT ON 3-D SURFACE
         CALL draw_(XP2,YP2)
         CALL vxpt3_(XP,YP,A(NX,I),NX,I,NX) ! DETERMINE X,Y VALUE OF A POINT ON 3-D SURFACE
         CALL draw_(XP,YP)
         CALL move_(XP,YP2)
      enddo
   endif
!
!     BEGIN MAIN LOOP
5  continue
   MAINLOOP: DO IZ=2,NZ      ! OVER Z DIMENSION TOWARD REAR
      IPCT=1
      IDCT=1
      IHCT=1
!        DETERMINE START POINT LOCATION
      CALL vxpt3_(XP1,YP1,A(IDCT,IZ),1,IZ,NX) ! LEFT-MOST DATA POINT ! DETERMINE X,Y VALUE OF A POINT ON 3-D SURFACE
      IF (XP1.LT.H(1,1)) THEN  ! DATA TO LEFT OF HISTORY ARRAY
!           IF (IPCT.GT.MAXSIZE_local) GOTO 999
!           P(IPCT,1)=XP1
!           P(IPCT,2)=YP1
!           IPCT=IPCT+1
         CALL move_(XP1,YP1)
         DO I=1,NX  ! (VERY RARE)
            CALL vxpt3_(XP1,YP1,A(I,IZ),I,IZ,NX) ! DETERMINE X,Y VALUE OF A POINT ON 3-D SURFACE
            IF (XP1.GT.H(1,1)) THEN
               IDCT=I-1
               CALL vxpt3_(DX1,DY1,A(IDCT,IZ),IDCT,IZ,NX) ! DETERMINE X,Y VALUE OF A POINT ON 3-D SURFACE
               HHIGH=.FALSE.
               HX1=H(1,1)
               HY1=H(1,2)
               HX2=H(2,1)
               HY2=H(2,2)
               IDCT=IDCT+1
               IHCT=IHCT+2
               CALL vxpt3_(DX2,DY2,A(IDCT,IZ),IDCT,IZ,NX) ! DETERMINE X,Y VALUE OF A POINT ON 3-D SURFACE
               IDCT=IDCT+1
               GOTO 100
            ENDIF
            IF (IPCT.GT.MAXSIZE_local) GOTO 999
            P(IPCT,1)=XP1
            P(IPCT,2)=YP1
            IPCT=IPCT+1
            CALL draw_(XP1,YP1)
         enddo
      ENDIF
      IDCT=2
      CALL vxpt3_(DX1,DY1,A(1,IZ-1),1,IZ-1,NX) ! DETERMINE X,Y VALUE OF A POINT ON 3-D SURFACE
      CALL vxpt3_(DX2,DY2,A(1,IZ),1,IZ,NX)     ! DETERMINE X,Y VALUE OF A POINT ON 3-D SURFACE
!C       CALL move_(H(1,1),H(1,2))
      X0=H(1,1)
      Y0=H(1,2)
      IP=3
      IF (IPCT.GT.MAXSIZE_local) GOTO 999
      P(IPCT,1)=H(1,1)
      P(IPCT,2)=H(1,2)
      IPCT=IPCT+1
      DO I=2,IHOLD
         IF (H(I,1).GT.DX1) exit
         IF (IPCT.GT.MAXSIZE_local) GOTO 999
         P(IPCT,1)=H(I,1)
         P(IPCT,2)=H(I,2)
         IPCT=IPCT+1
!C             CALL draw_(H(I,1),H(I),2)
         X0=H(I,1)
         Y0=H(I,2)
      enddo
8     continue
      IHCT=I-1
      HX1=H(IHCT,1)
      HY1=H(IHCT,2)
      HX2=H(IHCT+1,1)
      HY2=H(IHCT+1,2)
      IHCT=IHCT+2
      HHIGH=.TRUE.
      IF (HX1.EQ.HX2) THEN
         IF (IHCT.EQ.IHOLD) GOTO 100
         IHCT=IHCT+1
         GOTO 8
      ENDIF
      AMH=(HY2-HY1)/(HX2-HX1)
      BH=HY1-HX1*AMH
      YP=AMH*DX1+BH
      IF (YP.LE.DY1) HHIGH=.FALSE.
      IF (HY1.EQ.DY1.AND.HX1.EQ.DX1) THEN
         HHIGH=.TRUE.
         YP=AMH*DX2+BH
         IF (YP.LT.DY2) HHIGH=.FALSE.
      ENDIF
!
!     TOP OF INNER LOOP
!
100   CONTINUE
      CALL intersect_(FLAG,X,Y,HX1,HY1,HX2,HY2,DX1,DY1,DX2,DY2,HHIGH)
      IF (FLAG) THEN  ! SEGMENTS INTERSECT
         HX1=X    ! DRAW SEGMENT WITH
         HY1=Y    ! HIGHEST START POINT
         DX1=X    ! TO THE INTERSECTION
         DY1=Y
         IF (IPCT.GT.MAXSIZE_local) GOTO 999
         P(IPCT,1)=X
         P(IPCT,2)=Y
         IPCT=IPCT+1
         IF (IP.EQ.2) CALL draw_(X,Y)
         X0=X
         Y0=Y
         GOTO 100
      ENDIF
!
      IF (HX2.LE.DX2) THEN ! CHECKED ALL H SEGS OVER D SEGS
         IF (HHIGH) THEN ! DRAW HIGHEST SEGMENT
            IF (IPCT.GT.MAXSIZE_local) GOTO 999
            P(IPCT,1)=HX2
            P(IPCT,2)=HY2
            IPCT=IPCT+1
            IF (IP.EQ.3) CALL move_(X0,Y0)
            CALL draw_(HX2,HY2)
            X0=HX2
            Y0=HY2
            IP=2
         ENDIF
         HX1=HX2
         HY1=HY2
         HX2=H(IHCT,1)
         HY2=H(IHCT,2)
         IHCT=IHCT+1
         IF (IHCT.GT.IHOLD+1) THEN
34          CONTINUE
            IF (IDCT.LE.NX+1) THEN
               CALL vxpt3_(X,Y,A(IDCT-1,IZ),IDCT-1,IZ,NX) ! DETERMINE X,Y VALUE OF A POINT ON 3-D SURFACE
               IF(IPCT.GT.MAXSIZE_local)GOTO 999
               P(IPCT,1)=X
               P(IPCT,2)=Y
               IPCT=IPCT+1
               IF (IP.EQ.3) CALL move_(X0,Y0)
               IP=2
               CALL draw_(X,Y)
               IDCT=IDCT+1
               GOTO 34
            ENDIF
            GOTO 200 ! DONE WITH H'S
         ENDIF
         IF (HX1.EQ.DX2) THEN
            DX1=DX2  ! NEXT DATA POINT
            DY1=DY2
            X0=DX1
            Y0=DY1
!C                IF (.NOT.HHIGH)CALL draw_(DX1,DY1)
            !write(*,*)' I IDCT,IZ=',idct,iz,inx,inz,nx,nz
            if(idct.gt.nx)then
               DX2=DX1
               DY2=AMINQ
            else
               CALL vxpt3_(DX2,DY2,A(IDCT,IZ),IDCT,IZ,NX) ! DETERMINE X,Y VALUE OF A POINT ON 3-D SURFACE
            endif
            IDCT=IDCT+1
            IF (IDCT.GT.NX+2) GOTO 235 ! DONE WITH DATA
            HHIGH=.TRUE.
            IF (DY1.GT.HY1) HHIGH=.FALSE.
         ENDIF
         GOTO 100
      ELSE
         IF (.NOT.HHIGH) THEN ! PLOT DATA THAT IS HIGHEST
            IF (IPCT.GT.MAXSIZE_local) GOTO 999
            P(IPCT,1)=DX2
            P(IPCT,2)=DY2
            IPCT=IPCT+1
            IF (IP.EQ.3) CALL move_(X0,Y0)
            CALL draw_(DX2,DY2)
            IP=2
            X0=DX2
            Y0=DY2
         ENDIF
         DX1=DX2  ! NEXT DATA POINT
         DY1=DY2
         !write(*,*)'II IDCT,IZ=',idct,iz,inx,inz,nx,nz
         if(idct.gt.nx)then
            DX2=DX1
            DY2=AMINQ
         else
            CALL vxpt3_(DX2,DY2,A(IDCT,IZ),IDCT,IZ,NX) ! DETERMINE X,Y VALUE OF A POINT ON 3-D SURFACE
         endif
         IDCT=IDCT+1
         IF (IDCT.GT.NX+2) GOTO 235 ! DONE WITH DATA
      ENDIF
!
!     DONE WITH INNER LOOP
!
      GOTO 100
235   CONTINUE          ! FINISH H CURVE WHEN OUT OF DATA
      IHCT=IHCT-1
236   CONTINUE
      IF (IHCT.GT.IHOLD) GOTO 200
      X=H(IHCT,1)
      Y=H(IHCT,2)
      IHCT=IHCT+1
      IF (IPCT.GT.MAXSIZE_local) GOTO 999
      P(IPCT,1)=X
      P(IPCT,2)=Y
      IPCT=IPCT+1
!C       CALL draw_(X,Y)
      IDCT=IDCT+1
      GOTO 236
!
200   continue
      IHOLD=IPCT-1     ! STORE NEW HISTORY
      DO I=1,IPCT
         H(I,1)=P(I,1)
         H(I,2)=P(I,2)
      enddo
!
   enddo MAINLOOP
!
520 CALL move_(0.,0.)   ! PEN UP
   RETURN
999 CONTINUE
   WRITE(*,3002)
3002 FORMAT(' *** dl_slices INTERNAL MEMORY OVERFLOW ERROR ***')
   GOTO 520
END SUBROUTINE dl_slices
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
SUBROUTINE vxpt3_(X,Y,AVAL,IX,IZ,NX)
implicit none
!
!  CREATED BY DAVID LONG    AUG, 1982 AT JPL; revised 1993
!  SUBPROGRAM OF DL_slices

! ident_2="@(#)M_slices::vxpt3_(3fp) :: routine to determine x,y value of a point on 3-d surface for dl_slices(3f)"

real,intent(out)    :: x
real,intent(out)    :: y
real,intent(in)     :: aval
integer,intent(in)  :: ix
integer,intent(in)  :: iz
integer,intent(in)  :: nx
   X=XSCALEQ*FLOAT(IX-1)*COS(ALPHQ)+FLOAT(IZ-1)*COS(BETQ)*ZSCALEQ
   Y=YSCALEQ*(AVAL-AMINQ)+FLOAT(NX-IX+1)*SIN(ALPHQ)*XSCALEQ+FLOAT(IZ-1)*SIN(BETQ)*ZSCALEQ
END SUBROUTINE vxpt3_
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
      SUBROUTINE intersect_(FLAG,X,Y,AX1,AY1,AX2,AY2,BX1,BY1,BX2,BY2,A)
!
!     CREATED BY D. LONG     AUG, 1983 AT JPL; revised 19931208
!     SUBPROGRAM OF dl_slices
!
!     DETERMINE IF TWO SEGMENTS INTERSECT AND THE POINT OF INTERSECTION
!     IF STARTING POINTS OF SEGMENTS ARE THE SAME SEGMENTS ARE NOT
!     CONSIDERED TO BE INTERSECTING
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
REAL :: MA
REAL :: MB
LOGICAL :: FLAG
LOGICAL :: VERT
LOGICAL :: A
!
      VERT=.FALSE.
      FLAG=.FALSE.
!
      IF (AX1.EQ.BX1.AND.AY1.EQ.BY1) RETURN !SAME START POINT
      IF (AX2.EQ.BX2.AND.AY2.EQ.BY2) THEN !SAME END POINT
         FLAG=.TRUE.
         X=AX2
         Y=AY2
         RETURN
      ENDIF
!
      DENOM=AX1-AX2
      IF (DENOM.EQ.0.0) THEN  !VERTICAL LINE
         MA=1.E10
         VERT=.TRUE.
      ELSE
         MA=(AY1-AY2)/DENOM !SLOPE OF SEGMENT A
      ENDIF
      DENOM=BX1-BX2
      IF (DENOM.EQ.0.0) THEN  !VERTICAL LINE
         MB=1.E10
         VERT=.TRUE.
      ELSE
         MB=(BY1-BY2)/DENOM !SLOPE OF SEGMENT B
      ENDIF
      IF (MA.EQ.MB) RETURN  !PARALLEL
      CA=AY1-MA*AX1
      CB=BY1-MB*BX1
      IF (VERT) THEN
         IF (MA.EQ.1.E10) THEN
            X=AX1
            Y=X*MB+CB
         ENDIF
         IF (MB.EQ.1.E10) THEN
            X=BX1
            Y=X*MA+CA
         ENDIF
      ELSE
         X=(CA-CB)/(MB-MA)
         Y=MA*X+CA
      ENDIF
!     INTERSECTION OF LINES THROUGH POINTS IS AT X,Y
      DA=(AX1-AX2)**2+(AY1-AY2)**2
      DA1=(AX1-X)**2+(AY1-Y)**2
      IF (DA1.GT.DA) RETURN
         DA2=(AX2-X)**2+(AY2-Y)**2
         IF (DA2.GT.DA) RETURN
            DB=(BX1-BX2)**2+(BY1-BY2)**2
            DB1=(BX1-X)**2+(BY1-Y)**2
            IF (DB1.GT.DB) RETURN
               DB2=(BX2-X)**2+(BY2-Y)**2
               IF (DB2.GT.DB) RETURN
                  IF (MA.GT.MB) THEN
                     A=.TRUE.
                  ELSE
                     A=.FALSE.
                  ENDIF
                  FLAG=.TRUE.
                  RETURN
      END SUBROUTINE intersect_
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
      SUBROUTINE axisb_(X0,Y0,A0,N0,S0,T0,C0,D0,NM,NN,ML,TS,ND,SM,ICOL)
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
!
implicit none
integer :: i
integer :: ic
integer :: ic2
integer :: k
integer :: ml
integer :: n0
integer :: n1
integer :: n2
integer :: nc1
integer :: nd
integer :: ndd
integer :: nddd
integer :: nm
integer :: nm1
integer :: nn
real    :: b1
real    :: b2
real    :: b3
real    :: b4
real    :: b6
real    :: b7
real    :: b8
real    :: c0
real    :: c1
real    :: c2
real    :: cs
real    :: d0
real    :: d1
real    :: e1
real    :: hmt
real    :: hor
real    :: s0
real    :: s1
real    :: sm
real    :: t0
real    :: t1
real    :: t2
real    :: t3
real    :: t4
real    :: t5
real    :: t6
real    :: ts
real    :: x0
real    :: x1
real    :: x2
real    :: x3
real    :: xl
real    :: xm
real    :: y0
real    :: y1
real    :: y2
real    :: y3
CHARACTER*(*) A0
INTEGER ICOL(4)
LOGICAL VERT,TICKS,COLOR,SCALE
!
      CS=.15         ! CHARACTER SIZE
      IF (S0.EQ.0.0) GOTO 200 ! ZERO LENGTH AXIS
      VERT=.FALSE.      ! NO VERTICAL NUMBERS ON HORIZONTAL AXIS
      TICKS=.TRUE.      ! PUT ON TICKS
      SCALE=.TRUE.      ! (x10 TO POWER SCALING)
      HOR=T0
      NDD=1       ! NUMBER OF DIGITS TO RIGHT OF DECIMAL
      T5=0.1         ! TICK LENGTH
      B7=T5+.08      ! NUMBER DISTANCE FROM AXIS
      B6=B7
      B8=0.0
      NM1=0          ! NUMBER MINOR TICKS
      N2=(ABS(S0)+0.5)   ! NUMBER OF MAJOR TICKS
      S1=N2
      XL=1.          ! INCREMENT BETWEEN MAJOR TICKS
      N1=IABS(N0)
      COLOR=.FALSE.
      IF (N1.GT.100000) THEN
         N1=MOD(N1,100000) ! USE COLOR ARRAY
         COLOR=.TRUE.
      ENDIF
      IF (N1.GT.10000) THEN
         N1=MOD(N1,10000)
         N2=IABS(ML)    ! NUMBER MAJOR TICKS
         S1=ABS(S0)
         IF (N2.EQ.0) N2=1
         XL=ABS(S0)/FLOAT(N2) ! SPACING MAJOR TICKS
         NM1=IABS(NM)+1  ! NUMBER MINOR TICKS
         IF (ML.LT.0) THEN
            CS=ABS(TS)  ! DIFFERENT TITLE SIZE
            IF (CS.EQ.0.) CS=.15
            NDD=IABS(ND)
            IF (TS.LT.0) SCALE=.FALSE. ! DO NOT SCALE
            T5=ABS(SM)    ! NEW TICK LENGTH
            IF (T5.EQ.0.) T5=.1
         ENDIF
      ENDIF
      IF (N1.GT.1000)THEN
         N1=MOD(N1,1000)  ! VERTICAL NUMBERS ON HORIZONTAL AXIS
         VERT=.TRUE.
         HOR=0.0
         B4=(ABS(T5)*(1.+SIGN(1.,S0))/2.+.1)*SIGN(1.,FLOAT(N0))
         B6=.49*CS
      ENDIF
      IF (N1.GT.100) THEN
         N1=MOD(N1,100)  ! NO TICKS
         TICKS=.FALSE.
      ENDIF
      IF(N0.LT.0)GOTO 10
      B3=CS*(2.8+NDD)    ! COUNTER-CLOCKWISE LABELING
      B4=CS+T5
      T2=T0
      GOTO 20
10    continue
      B3=(-CS)*(3.+NDD)    ! CLOCKWISE LABELING
      B4=-T5-CS
      T2=T0
      T5=-T5
20    CONTINUE
      T5=T5*SIGN(1.,S0)
      T1=T0*0.017453294
      T3=COS(T1)
      T4=SIN(T1)
!
      T6=T5*T3
      T5=T5*T4
      X1=X0
      Y1=Y0
      IF (COLOR) CALL color_(ICOL(1)) ! COLOR
      DO I=1,N2       ! MAJOR TICKS
         IF (NM1.EQ.0) GOTO 106
         XM=XL/FLOAT(NM1) ! SPACING MINOR TICKS
         DO K=1,NM1   ! DO MINOR TICKS
            X2=X1+T3*FLOAT(K-1)*XM
            Y2=Y1+T4*FLOAT(K-1)*XM
            IF (K-1.EQ.NN.AND.NN.NE.0) THEN
               HMT=0.8
            ELSE
               HMT=0.5
            ENDIF
            X3=X2-T5*HMT
            Y3=Y2+T6*HMT
            CALL move_(X2,Y2)
         enddo
         CALL draw_(X3,Y3)
106      continue
         X2=X1-T5
         Y2=Y1+T6
         CALL move_(X2,Y2)
         CALL draw_(X1,Y1)
         X1=X1+T3*XL
         Y1=Y1+T4*XL
         IF (T0.EQ.90.0) X1=X0
      enddo
      CALL draw_(X1,Y1)
      X2=X1-T5
      Y2=Y1+T6
      CALL draw_(X2,Y2)   ! FINISH LAST MAJOR TICK
!     CHECK FOR EXPONENT VALUE
      D1=D0             ! SCALING FACTOR
      C1=C0+D1*S1        ! STARTING VALUE
      E1=0.0             ! EXPONENT
      IF (.NOT.SCALE) GOTO 140
      IF(D1.EQ.0.0) GOTO 140
110   continue
      IF(ABS(D1).LT.10.0)GOTO 130
      D1=D1*0.1
      C1=C1*0.1
      E1=E1+1.0
      GOTO 110
120   continue
      D1=D1*10.0
      C1=C1*10.0
      E1=E1-1.0
130   continue
      IF(ABS(D1).LT.0.5)GOTO 120
140   CONTINUE       ! PEN AT END OF AXIS
      IF (.NOT.TICKS) THEN
         IF (COLOR) CALL color_(ICOL(3)) ! COLOR
         GOTO 200
      ENDIF
      IF (VERT) THEN
         C2=C1-N2*D1    ! MAKE SPACE FOR VERTICAL NUMBERS
         IC=1        ! ON HORIZONTAL AXIS
         IF (ABS(C2).GE.1.0) IC=IFIX(ALOG10(ABS(C2)))
         IC2=1
         IF (ABS(C1).GE.1.0) IC2=IFIX(ALOG10(ABS(C1)))
         NC1=MAX(IC,IC2)+2
         IF (C2.LT.0.0.OR.C0.LT.0.0) NC1=NC1+1
         IF (N0.GT.0.0) B4=B4+FLOAT(NC1+NDD)*CS
         B3=0.0
         B8=(.25+ABS(T5)*(SIGN(1.,S0)+1.)/2.+FLOAT(NC1+NDD)*CS)*SIGN(1.,FLOAT(N0))
      ENDIF
      X2=X1-B4*T4-B7*T3  ! LOCATE CENTER NUMBER LABELS
      Y2=Y1+B4*T3-B6*T4
      N2=N2+1
      IF (COLOR) CALL color_(ICOL(2)) ! COLOR
      NDDD=NDD
      IF (NDD.EQ.0) NDDD=-1
      DO I=1,N2      ! LABEL MAJOR TICKS
         CALL number_(X2,Y2,CS,C1,HOR,FLOAT(NDDD)/100.,-1)
         C1=C1-D1*S1/FLOAT(N2-1)
         X2=X2-T3*XL
         Y2=Y2-T4*XL
      enddo
      IF (N1.NE.0) THEN
         C2=0.0
         Y2=0.0
         call dl_symbol(C2,Y2,CS,A0,0.,N1,-3)
         B1=0.5*(ABS(S0)-C2)  ! CENTER TITLE
         IF (E1.NE.0.0) B1=B1-CS*3. ! PUT ON EXPONENT SPACE
         X2=X0+B1*T3-B3*T4-B8*T4
         Y2=Y0+B1*T4+B3*T3
         IF (COLOR) CALL color_(ICOL(3)) ! COLOR
         call dl_symbol(X2,Y2,CS,A0,T2,N1,-1)
      ELSE
         C2=0.0
         B1=0.5*ABS(S0)
         X2=X0+B1*T3-B3*T4-B8*T4
         Y2=Y0+B1*T4+B3*T3
      ENDIF
      IF (E1.EQ.0.0) GOTO 200  ! NO EXPONENT
      IF (COLOR) CALL color_(ICOL(4)) ! COLOR
      C2=C2+CS
      X2=X2+C2*T3
      Y2=Y2+C2*T4
      call dl_symbol(X2,Y2,CS,'(X10',T2,4,-1)
      X2=X2+3.75*CS*T3-CS*T4*0.4
      Y2=Y2+3.75*CS*T4+CS*T3*0.4
      CALL number_(X2,Y2,CS,E1,T2,0.0,-1)
      B2=0.8+AINT(ALOG10(ABS(E1)))
      IF (E1.LT.0.0) B2=B2+1
      X2=X2+B2*CS*T3+CS*T4*0.4
      Y2=Y2+B2*CS*T4-CS*T3*0.4
      call dl_symbol(X2,Y2,CS,')',T2,1,-1)
200   continue
      END SUBROUTINE axisb_
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
      SUBROUTINE axisa_(X0,Y0,A0,N0,S0,T0,C0,D0,NM,ML,ICOL)
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
implicit none
real      :: b1
real      :: b2
real      :: b3
real      :: b4
real      :: b6
real      :: b7
real      :: b8
real      :: c0
real      :: c1
real      :: c2
real      :: cs
real      :: d0
real      :: d1
real      :: e1
real      :: hor
integer   :: i
integer   :: ic
integer   :: ic2
integer   :: k
integer   :: ml
integer   :: n0
integer   :: n1
integer   :: n2
integer   :: nc1
integer   :: nm
integer   :: nm1
real      :: s0
real      :: s1
real      :: t0
real      :: t1
real      :: t2
real      :: t3
real      :: t4
real      :: t5
real      :: t6
real      :: x0
real      :: x1
real      :: x2
real      :: x3
real      :: xl
real      :: xm
real      :: y0
real      :: y1
real      :: y2
real      :: y3
CHARACTER A0*(*)
INTEGER ICOL(4)
LOGICAL VERT,TICKS,COLOR
!
      CS=0.15        ! CHARACTER SIZE
      IF (S0.EQ.0.0) GOTO 200 ! ZERO LENGTH AXIS
      VERT=.FALSE.      ! NO VERTICAL NUMBERS ON HORIZONTAL AXIS
      TICKS=.TRUE.      ! PUT ON TICKS
      HOR=T0
      T5=0.1         ! TICK LENGTH
      B7=T5+.08      ! NUMBER DISTANCE FROM AXIS
      B6=B7
      B8=0.0
      NM1=0          ! NUMBER MINOR TICKS
      N2=(ABS(S0)+0.5)   ! NUMBER OF MAJOR TICKS
      S1=FLOAT(N2)
      XL=1.0         ! INCREMENT BETWEEN MAJOR TICKS
      N1=IABS(N0)
      COLOR=.FALSE.
      IF (N1.GE.100000) THEN
         N1=MOD(N1,100000) ! USE COLOR ARRAY
         COLOR=.TRUE.
      ENDIF
      IF (N1.GE.10000) THEN
         N1=MOD(N1,10000)
         N2=IABS(ML)    ! NUMBER MAJOR TICKS
         IF (N2.EQ.0) N2=1
         S1=ABS(S0)
         XL=ABS(S0)/FLOAT(N2) ! SPACING MAJOR TICKS
         NM1=IABS(NM)+1  ! NUMBER MINOR TICKS
      ENDIF
      IF(N0.LT.0)GOTO 10
      B3=CS*3.8         ! COUNTER-CLOCKWISE LABELING
      B4=CS+0.08
      T2=T0
      GOTO 20
!-----------------------------------------------------------------------------------------------------------------------------------
10    CONTINUE
      B3=(-CS)*4.0        ! CLOCKWISE LABELING
      B4=-T5-CS-.05
      T2=T0
      T5=-T5
!-----------------------------------------------------------------------------------------------------------------------------------
20    CONTINUE
      IF (N1.GE.1000)THEN
         N1=MOD(N1,1000)  ! VERTICAL NUMBERS ON HORIZONTAL AXIS
         VERT=.TRUE.
         HOR=0.0
         B4=(ABS(T5)*(1.+SIGN(1.,S0))/2.+.1)*SIGN(1.,FLOAT(N0))
         B6=.49*CS
      ENDIF
      IF (N1.GE.100) THEN
         N1=MOD(N1,100)  ! NO TICKS
         TICKS=.FALSE.
      ENDIF
      T5=T5*SIGN(1.,S0)
      T1=T0*0.017453294
      T3=COS(T1)
      T4=SIN(T1)
!
      T6=T5*T3
      T5=T5*T4
      X1=X0
      Y1=Y0
      IF (COLOR) CALL color_(ICOL(1)) ! COLOR
      DO I=1,N2       ! MAJOR TICKS
      IF (NM1.EQ.0) GOTO 106
         XM=XL/FLOAT(NM1) ! SPACING MINOR TICKS
         DO K=1,NM1   ! DO MINOR TICKS
            X2=X1+T3*FLOAT(K-1)*XM
            Y2=Y1+T4*FLOAT(K-1)*XM
            X3=X2-T5*.5
            Y3=Y2+T6*.5
            CALL move_(X2,Y2)
         enddo
         CALL draw_(X3,Y3)
106      CONTINUE
         X2=X1-T5
         Y2=Y1+T6
         CALL move_(X2,Y2)
         CALL draw_(X1,Y1)
         X1=X1+T3*XL
         Y1=Y1+T4*XL
         IF (T0.EQ.90.) X1=X0
         CALL draw_(X1,Y1)
      enddo
      X2=X1-T5
      Y2=Y1+T6
      CALL draw_(X2,Y2)   ! FINISH LAST MAJOR TICK
!     CHECK FOR EXPONENT VALUE
      D1=D0             ! SCALING FACTOR
      C1=C0+S1*D1        ! STARTING VALUE
      E1=0.0             ! EXPONENT
      IF(D1.EQ.0.0)GOTO 140
110   CONTINUE
      IF(ABS(D1).LT.10.0)GOTO 130
      D1=D1*0.1
      C1=C1*0.1
      E1=E1+1.0
      GOTO 110
120   CONTINUE
      D1=D1*10.0
      C1=C1*10.0
      E1=E1-1.0
130   CONTINUE
      IF(ABS(D1).LT.0.5)GOTO 120
140   CONTINUE       ! PEN AT END OF AXIS
      IF (.NOT.TICKS) THEN
         IF (COLOR) CALL color_(ICOL(3)) ! COLOR
         GOTO 200
      ENDIF
      IF (VERT) THEN
         C2=C1-N2*D1    ! MAKE SPACE FOR VERTICAL NUMBERS
         IC=1        ! ON HORIZONTAL AXIS
         IF (ABS(C2).GE.1.0) IC=IFIX(ALOG10(ABS(C2)))
         IC2=1
         IF (ABS(C1).GE.1.0) IC2=IFIX(ALOG10(ABS(C1)))
         NC1=MAX(IC,IC2)+2
         IF (C2.LT.0.0.OR.C0.LT.0.0) NC1=NC1+1
         IF (N0.GT.0.0) B4=B4+FLOAT(NC1)*CS
         B3=0.0
         B8=(.25+ABS(T5)*(SIGN(1.,S0)+1.)/2.+FLOAT(NC1)*CS)*SIGN(1.,FLOAT(N0))
      ENDIF
      X2=X1-B4*T4-B7*T3  ! LOCATE CENTER NUMBER LABELS
      Y2=Y1+B4*T3-B6*T4
      N2=N2+1
      IF (COLOR) CALL color_(ICOL(2)) ! COLOR
      DO I=1,N2      ! LABEL MAJOR TICKS
         CALL number_(X2,Y2,CS,C1,HOR,0.01,-1)
         C1=C1-D1*S1/FLOAT(N2-1)
         X2=X2-T3*XL
         Y2=Y2-T4*XL
      enddo
      IF (COLOR) CALL color_(ICOL(3)) ! COLOR
      IF (N1.GT.0) THEN  ! ADD TITLE
         C2=0.0
         Y2=0.0
         call dl_symbol(C2,Y2,CS,A0,0.0,N1,-3) ! TITLE LENGTH
         B1=0.5*(ABS(S0)-C2)  ! CENTER TITLE
         IF (E1.NE.0.) B1=B1-CS*3.0 ! PUT ON EXPONENT
         X2=X0+B1*T3-B3*T4-B8*T4
         Y2=Y0+B1*T4+B3*T3
         call dl_symbol(X2,Y2,CS,A0,T2,N1,-1)
      ELSE
         C2=0.0
         B1=0.5*ABS(S0)
         X2=X0+B1*T3-B3*T4-B8*T4
         Y2=Y0+B1*T4+B3*T3
      ENDIF
      IF (E1.EQ.0.0) GOTO 200  ! NO EXPONENT
      IF (COLOR) CALL color_(ICOL(4)) ! COLOR
      C2=C2+CS
      X2=X2+C2*T3
      Y2=Y2+C2*T4
      call dl_symbol(X2,Y2,CS,'(X10',T2,4,-1)
      X2=X2+CS*3.75*T3-CS*T4*0.4
      Y2=Y2+CS*3.75*T4+CS*T3*0.4
      CALL number_(X2,Y2,CS,E1,T2,0.0,-1)
      B2=0.8+AINT(ALOG10(ABS(E1)))
      IF (E1.LT.0.0) B2=B2+1
      X2=X2+B2*CS*T3+CS*T4*0.4
      Y2=Y2+B2*CS*T4-CS*T3*0.4
      call dl_symbol(X2,Y2,CS,')',T2,1,-1)
200   CONTINUE
      END SUBROUTINE axisa_
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
SUBROUTINE number_(X,Y,HGHT,Z,T,F0,IPF)
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
implicit none
real    :: alg
real    :: f
real    :: f0
real    :: fa
real    :: hg
real    :: hght
integer :: i
integer :: iff
integer :: ipf
integer :: nd
integer :: nn
real    :: t
real    :: t1
real    :: x
real    :: y
real    :: z
CHARACTER(len=18) :: B   ! WORKING BUFFERS
CHARACTER(len=8)  :: FB  ! WORKING BUFFERS
CHARACTER(len=8)  :: FB1 ! WORKING BUFFERS
!
   IFF=0
   HG=HGHT
   IF (HG.EQ.0.0) HG=0.15
   T1=T
   ND=0
   NN=0
   FA=F0
   IF (ABS(FA).GT.1022.0) FA=0.0
   IF (FA.EQ.0.0) GOTO 10  ! INTEGER FORMAT
   IF (FA.GT.999.0) THEN  ! PLOT FORMATTED INTEGER
      NN=AMOD(FA,1000.)
      FA=0.0
   ELSE           ! PLOT FLOAT OR EXPON NUMBER
      F=ABS(FA)*1.000002
      NN=F
      F=(F-NN)*100.
      ND=F
   ENDIF
10 continue
   IF (ND.GT.17) ND=ND/10  ! CORRECT SIMPLE INPUT ERRORS
   IF (NN.EQ.0) THEN  ! DIGITS TO LEFT OF DECIMAL POINT
      NN=ND+2
      IF (Z.EQ.0.AND.FA.EQ.0.0) NN=1
      IF (Z.NE.0.0) THEN
         ALG=ALOG10(ABS(Z))
         IF (ALG.LT.0.0) ALG=0.0
         NN=ND+2+ALG
         IF (FA.EQ.0.0) NN=1+ALG
      ENDIF
      IF (Z.LT.0.0) NN=NN+1
      IF (FA.LT.0.0) NN=NN+4
   ENDIF
   IF (ND.GT.NN) GOTO 90  ! FORMAT ERROR
   IF (NN.GT.18) NN=18  ! MAX CHARACTERS
   IF (FA.EQ.0.0) THEN  ! INTEGER
      I=Z
      FB=CHAR(NN-10*(NN/10)+48)//')'
      FB1=FB
      IF (NN/10.GT.0) FB=CHAR(NN/10+48)//FB1
      FB1='(I'//FB
      WRITE(B,FB1,ERR=90) I
   ELSE           ! FLOATING POINT OR EXPONENTIAL
      IF (NN.GT.1) THEN
         FB=CHAR(ND-10*(ND/10)+48)//')'
         FB1=FB
         IF (ND/10.GT.0) FB=CHAR(ND/10+48)//FB1
         FB1=CHAR(NN-10*(NN/10)+48)//'.'//FB
         FB=FB1
         IF (NN/10.GT.0) FB=CHAR(NN/10+48)//FB1
         IF (FA.GT.0.0) THEN
            FB1='(F'//FB
         ELSE
            FB1='(E'//FB
         ENDIF
      ELSE
         IF (FA.GT.0.0) THEN
            FB1='(F)'
         ELSE
            FB1='(E)'
         ENDIF
         NN=16
         IFF=1
      ENDIF
      WRITE(B,FB1,ERR=90) Z
      IF (IFF.EQ.1) THEN  ! REMOVE LEADING SPACES
         DO I=1,18
            IF (B(1:1).EQ.' ') B=B(2:18)
         enddo
      ENDIF
   ENDIF
50 continue
   call dl_symbol(X,Y,HG,B,T1,NN,IPF)
   RETURN
90 continue
   DO I=1,18
      B(I:I)='*'
      IF (I.EQ.NN-ND) B(I:I)='.'
   enddo
   GOTO 50
END SUBROUTINE number_
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
SUBROUTINE range_(X,S,N,K,IX,XMIN,DX)
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
!
implicit none
integer :: i
integer :: idx
integer :: ix
integer :: k
integer :: n
integer :: np
real    :: dx
real    :: s
real    :: si
real    :: sj
real    :: xi
real    :: xmax
real    :: xmin
real    :: xmm
REAL X(*),Q(6)
DATA Q/1.0,2.0,4.0,5.0,8.0,10.0/
   NP=N*K
   XMAX=X(1)
   XMIN=XMAX
   DO I=IX,NP,K
      XI=X(I)
      XMAX=AMAX1(XMAX,XI)
      XMIN=AMIN1(XMIN,XI)
   enddo
   XMM=XMIN
   IF (S.LE.0.0) GOTO 160
   DX=(XMAX-XMIN)/S
   IF (DX.LE.0.0) GOTO 160
   SJ=0.0
   IF (DX.LT.1.0) SJ=-1.0
   IDX=ALOG10(DX)+SJ
   DX=DX/(10.0**IDX)
   DO I=1,6
      XI=Q(I)
      IF (XI.GE.DX) GOTO 120
   enddo
120 continue
   DX=XI*(10.0**IDX)
   SI=1.0
   SJ=0.0
   IF (XMIN) 130,170,140
130 continue
   SI=-1.0
   SJ=-0.99999
   XMIN=-XMIN
140 continue
   IDX=ALOG10(XMIN)+SJ
   XMIN=XMIN/(10.0**IDX)
   XMIN=XMIN-SJ
   XMIN=IFIX(XMIN)*SI*(10.0**IDX)
   GOTO 170
160 continue
   DX=1.0
   XMIN=XMIN-0.5
170 CONTINUE
!
!     BEFORE EXIT, CHECK TO BE SURE THAT DATA IS CONTAINED WITHIN
!     THE LIMITS XMIN AND XMIN+DX*S.  IF NOT, RESET DX
!
   IF (XMM.LT.XMIN) XMIN=XMM
   IF (XMAX.GT.XMIN+DX*S) THEN
      IF (S.GT.0.0) DX=(XMAX-XMIN)/S
      IF (DX.LE.0.0) DX=1.0
   ENDIF
END SUBROUTINE range_
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine color_(ic)
use M_draw
integer :: ic
   IF (ic.GE.0) THEN
      CALL COLOR(IC)           ! change color
   ENDIF
end subroutine color_
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine draw_(xa,ya)
implicit none
real :: xa
real :: ya
   call plot_(xa,ya,2)
end subroutine draw_
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine move_(xa,ya)
implicit none
real :: xa
real :: ya
   call plot_(xa,ya,3)
end subroutine move_
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine translate_(xa,ya)
implicit none
real :: xa
real :: ya
   call plot_(xa,ya,-3)
end subroutine translate_
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine viewport_(xmin,xmax,ymin,ymax)
implicit none
real :: xmin
real :: xmax
real :: ymin
real :: ymax
real :: xconmax
real :: yconmax
real :: xconmin
real :: yconmin
! note that new viewport is in terms of current coordinate system
! SET upper right CORNER OF VIEW PORT
   CALL trs_(xmax,ymax,XCONmax,YCONmax) ! convert call numbers to current plot coordinate system
   VIEWPORTQ(3)=XCONmax
   VIEWPORTQ(4)=YCONmax
! SET lower left CORNER OF VIEW PORT
   CALL trs_(xmin,ymin,XCONmin,YCONmin) ! convert call numbers to current plot coordinate system
   VIEWPORTQ(1)=XCONmin
   VIEWPORTQ(2)=YCONmin
end subroutine viewport_
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine width_(ic)
use M_draw
implicit none
integer :: ic
   CALL LINEWIDTH(IC)
end subroutine width_
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
SUBROUTINE trs_(XIN,YIN,XCON,YCON)
implicit none
real :: xin
real :: yin
real :: xcon
real :: ycon
real :: tang
! convert call numbers to current plot coordinate system

   TANG=ANGLEQ*.0174532              ! convert degrees to radians

   XCON=XIN*COS(TANG)-YIN*SIN(TANG)  ! rotate coordinates
   YCON=XIN*SIN(TANG)+YIN*COS(TANG)  ! rotate coordinates

   XCON=SCALEQ*XCON+TRANSLATEXQ      ! scale and translate
   YCON=SCALEQ*YCON+TRANSLATEYQ      ! scale and translate

end SUBROUTINE trs_
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
SUBROUTINE plot_(XPLOT0,YPLOT0,ISELECT0)
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
!     CALL plot_ (XPLOT0,YPLOT0,ISELECT0)
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
use M_draw
implicit none
integer :: iselect0
integer :: ivta
integer :: ivtb
real    :: xcon
real    :: xplot0
real    :: xtemp
real    :: xtemp1
real    :: ycon
real    :: yplot0
real    :: ytemp
real    :: ytemp1
!#######################################################################
!     DECODE COMMAND
       select case (ISELECT0)
       case (2,3)
       case (-2,-3)
         TRANSLATEXQ=XCON    ! make scaled rotated input coordinates the new origin
         TRANSLATEYQ=YCON
       case default
          WRITE(*,*)'# *PLOT* UNEXPECTED SELECTION ',ISELECT0
       end select
       CALL trs_(XPLOT0,YPLOT0,XCON,YCON) ! convert call numbers to current plot coordinate system
!     DRAW LINE SEGMENT  ISELECT0=2,3 (and -2,-3)
      ! check if point (xcon,ycon) is in viewport rectangle
      IVTA=inbox_(XCON,YCON, VIEWPORTQ(1),VIEWPORTQ(2),VIEWPORTQ(3),VIEWPORTQ(4))
      ! check if point (xlastscaleq,ylastscaleq) is in viewport rectangle
      IVTB=inbox_(XLASTSCALEQ,YLASTSCALEQ, VIEWPORTQ(1),VIEWPORTQ(2),VIEWPORTQ(3),VIEWPORTQ(4))
      IF (IOR(IVTA,IVTB).EQ.0) GOTO 333 ! LINE ENTIRELY VISIBLE
      IF (IAND(IVTA,IVTB).NE.0) THEN  ! LINE ENTIRELY INVISIBLE
         XLASTSCALEQ=XCON
         YLASTSCALEQ=YCON
         RETURN
      ENDIF
      IF (IVTB.NE.0) THEN   ! OLD POINT IS OUTSIDE WINDOW
         XTEMP1=XLASTSCALEQ
         YTEMP1=YLASTSCALEQ
         CALL clipit_(IVTB,XTEMP1,YTEMP1,XCON,YCON, VIEWPORTQ(1),VIEWPORTQ(2),VIEWPORTQ(3),VIEWPORTQ(4))
         IF (IVTB.NE.0) THEN  ! VECTOR DOES NOT INTERSECT
            XLASTSCALEQ=XCON
            YLASTSCALEQ=YCON
            RETURN
         ENDIF

      ENDIF
      XTEMP=XCON
      YTEMP=YCON
      ! clips a partially visible line segment
      IF (IVTA.NE.0)then
         CALL clipit_(IVTA,XTEMP,YTEMP,XLASTSCALEQ,YLASTSCALEQ,VIEWPORTQ(1),VIEWPORTQ(2),VIEWPORTQ(3),VIEWPORTQ(4))
       endif
      XLASTSCALEQ=XCON
      YLASTSCALEQ=YCON
      IF (IVTA.NE.0) THEN
         RETURN
      ENDIF
      XCON=XTEMP
      YCON=YTEMP
! ----------------------------------------------------------------------------------------------------------------------------------
333   CONTINUE  ! draw clipped vector
      IF(ISELECT0.EQ.2)THEN
        CALL DRAW2(XCON,YCON)
      ELSEIF(ISELECT0.EQ.3)THEN
        CALL MOVE2(XCON,YCON)
      ELSE
        write(*,*)'*plot_* 2,3 internal error',xcon,ycon,iselect0
      ENDIF
      XLASTSCALEQ=XCON
      YLASTSCALEQ=YCON
      END SUBROUTINE plot_
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
! NAME
!      dl_init(3f) - [M_slices] initialize the longlib graphics plot package
!      (LICENSE:PD)
! SYNOPSIS
!   subroutine dl_init(xmax0,ymax0,vpx,vpy,zom)
! 
!    real,intent(in) :: xmax0
!    real,intent(in) :: ymax0
!    real,intent(in) :: vpx
!    real,intent(in) :: vpy
!    real,intent(in) :: zom
! 
! DESCRIPTION
! 
!    Routine to initialize the longlib graphics plot package
! 
!    XMAX0,YMAX0  size in inches M_DRAW will simulate as the
!                 display size for this library
! 
!    VPX,VPY      coordinates of bottom left origin
!    ZOM          zoom factor
!
subroutine dl_init(xmax0,ymax0,vpx,vpy,zom)
!     FORTRAN-77 VERSION:   DGL JULY, 1987
use M_draw
implicit none

! ident_3="@(#)M_slices::dl_init(3f): initialize the longlib graphics plot package"

real,intent(in)   :: xmax0
real,intent(in)   :: ymax0
real,intent(in)   :: vpx
real,intent(in)   :: vpy
real,intent(in)   :: zom
real              :: xmax
real              :: ymax
real              :: z
   Z=ZOM
   xmax=xmax0
   ymax=ymax0
   call page(0.0,xmax,0.0,ymax)
   XMINQ=0.0
   YMINQ=0.0
   XMAXQ=xmax
   YMAXQ=ymax

   call color(0)
   call clear()
   call color(7)
   call vflush()

   TRANSLATEXQ=VPX         ! ORIGIN X
   TRANSLATEYQ=VPY         ! ORIGIN Y

   SCALEQ=ABS(Z)      ! SCALE FACTOR
   IF (SCALEQ.LE.0.0) SCALEQ=1.0

   ANGLEQ=0.0       ! PLOTTING ANGLE ROTATION

   XLASTSCALEQ=0.0         ! LAST POINT PLOTTED
   YLASTSCALEQ=0.0

   ! set the VIEWPORTQ() ARRAY
   call viewport_(-999.0,999.0,-999.0,999.0)
   CALL color_(7) ! INITIALIZE LINE COLOR
END SUBROUTINE DL_INIT
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
SUBROUTINE clipit_(IVTB,XV2,YV2,AV1,AV2,XM,YM,XX,YX)
implicit none
!
!     CLIPS A LINE SEGMENT PARTIALLY VISIBLE
!
real    :: av1
real    :: av2
integer :: ivtb
real    :: xm
real    :: xv2
real    :: xx
real    :: ym
real    :: yv2
real    :: yx
   IF (IAND(IVTB,1).NE.0) THEN ! LEFT EDGE
      if (av1.ne.xv2) YV2=YV2+(AV2-YV2)*(XM-XV2)/(AV1-XV2)
      XV2=XM
      IVTB=inbox_(XV2,YV2,XM,YM,XX,YX)
   ENDIF
   IF (IAND(IVTB,2).NE.0) THEN ! RIGHT EDGE
      if (av1.ne.xv2) YV2=YV2+(AV2-YV2)*(XX-XV2)/(AV1-XV2)
      XV2=XX
      IVTB=inbox_(XV2,YV2,XM,YM,XX,YX)
   ENDIF
   IF (IAND(IVTB,4).NE.0) THEN ! BOTTOM EDGE
      if (av2.ne.yv2) XV2=XV2+(AV1-XV2)*(YM-YV2)/(AV2-YV2)
      YV2=YM
      IVTB=inbox_(XV2,YV2,XM,YM,XX,YX)
   ENDIF
   IF (IAND(IVTB,8).NE.0) THEN ! TOP EDGE
      if (av2.ne.yv2) XV2=XV2+(AV1-XV2)*(YX-YV2)/(AV2-YV2)
      YV2=YX
      IVTB=inbox_(XV2,YV2,XM,YM,XX,YX)
   ENDIF
END SUBROUTINE clipit_
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
! NAME
!      dl_symbol(3f) - [M_slices] routine to plot characters and symbols
!      (LICENSE:PD)
! SYNOPSIS
!       SUBROUTINE DL_SYMBOL(X,Y,S,T,A,NN,IS)
! DESCRIPTION
! 
!      Routine to plot characters and symbols
! 
!      X,Y   string position. If x>998 or y>998 then plotting
!            of the string is continued from the last DL_SYMBOL(3f) call
! 
!      S     height of the string to be printed
! 
!      T     character variable containing the ascii text to be plotted
! 
!      A     angle at which the string is to be plotted
!            counter-clockwise from x axis
! 
!      N     number of characters to use from T
! 
!            note: plotting will terminate if an ASCII zero is
!            encountered at any other position than the first character.
! 
!            If N<0, a plot(x,y,2) will be executed prior to plotting
!            the first character and ABS(N) characters will be plotted.
! 
!            For N<2, the plot pen is left at the 1st character origin
!            point; otherwise it is at the end of the last plotted
!            vector in the last plotted character.
! 
!      IS    centering option flag.
! 
!            = -3  end coordinates of string (if it were to be
!                  plotted will be returned in x,y where the input
!                  (x,y) are the lower left corner of string. This
!                  permits computation of the plotted length.
!                  However, no plotting is done and the last position
!                  variables are not changed.
!            = -2  end coordinates of string are returned in x,y.
!                  Initial (x,y) to be lower left corner of plotted
!                  string. String is plotted.
!            = -1  (x,y) to be lower left corner of plotted string
!                  (x and y not altered) String is plotted.
!            = 0   (x,y) to be center of plotted string
!                  (x and y not altered) String is plotted.
!            = 1   (x,y) to be lower right corner of plotted string
!                  (x and y not altered) String is plotted.
! 
!    DL_SYMBOL plots an ASCII string in a CHARACTER array. Each character
!    (or string of characters) can be imagined as a square box with the
!    origin at the lower left corner. The routine determines the initial
!    position of the lower left of the first character than plots each
!    character relative to this position. As each character is plotted the
!    "current position" is moved to the right (along the string baseline)
!    a fixed amount S. When the string centering option is selected,
!    the length of the plotted string is determined and, based on the
!    character height, the lower left corner is computed from the input
!    (x,y) position. The special plot symbols (ASCII 0-31) are always
!    centered about the current position.
!
SUBROUTINE DL_SYMBOL(X,Y,S,T,A,NN,IS)
!     WRITTEN BY: D. LONG  JAN 1991,1995   BYU
!     THIS ROUTINE IS FORTRAN-77 COMPATIBLE WITH THE FOLLOWING
!     EXCEPTIONS:
!        1. INTEGER*2 ARRAYS ARE USED TO SAVE SPACE.  THEY MAY
!           BE REPLACED WITH INTEGER.
!
!     MACHINE DEPENDENT NOTES:
!        1. THE FUNCTION IBITS(I,J,K) RETURNS THE VALUE OF THE BITS
!           IN I STARTING AT J FOR K BITS.
implicit none

! ident_4="@(#)M_slices::dl_symbol(3f): routine to plot characters and symbols"

real                :: X
real                :: Y
character(len=*)    :: T
integer             :: i
integer             :: icc
integer             :: il
integer             :: ip
integer             :: ipen
integer             :: ipenlast
integer             :: is
integer             :: iss
integer             :: iw
integer             :: ix
integer             :: ixoff
integer             :: iy
integer             :: iyoff
integer             :: n
integer             :: nn
real                :: a
real                :: aa
real                :: al
real                :: co
real                :: ox
real                :: oy
real                :: s
real                :: si
real                :: ss
real                :: x0
real                :: x1
real                :: xx
real                :: y0
real                :: y1
logical             :: LENGTH
real,save           :: OLDX,OLDY
!INTEGER, PARAMETER :: Short   = SELECTED_INT_KIND(4)   ! Short integer
INTEGER, PARAMETER  :: Short   = SELECTED_INT_KIND(8)   ! Long integer
!
INTEGER(kind=short) :: IFNT(  968),IPNT( 176)

! ----------------------------------------------------------------------------------------------------------------------------------
INTEGER(kind=short) :: IF001( 88),IF002( 88),IF003( 88),         &
     & IF004( 88),IF005( 88),IF006( 88),IF007( 88),       &
     & IF008( 88),IF009( 88),IF010( 88),IF011( 88)
! ----------------------------------------------------------------------------------------------------------------------------------

INTEGER(kind=short) :: IPT001( 88),IPT002( 88)
! ----------------------------------------------------------------------------------------------------------------------------------
EQUIVALENCE (IFNT(    1),IF001(1)),(IFNT(   89),IF002(1)),  &
     & (IFNT(  177),IF003(1)),(IFNT(  265),IF004(1)),             &
     & (IFNT(  353),IF005(1)),(IFNT(  441),IF006(1)),             &
     & (IFNT(  529),IF007(1)),(IFNT(  617),IF008(1)),             &
     & (IFNT(  705),IF009(1)),(IFNT(  793),IF010(1)),             &
     & (IFNT(  881),IF011(1))
! ----------------------------------------------------------------------------------------------------------------------------------
EQUIVALENCE (IPNT(    1),IPT001(1)),(IPNT(   89),IPT002(1))
! ----------------------------------------------------------------------------------------------------------------------------------
DATA IF001/  6186,  6826,  6806,  5526,  5546,  6186,  2080,          &
     &   6176, 10282, 10538, 10916, 10908, 10518, 10006,  9628,  9636,          &
     &   5930,  2090,  6176,  6186,  6747,  5595,  2090,  6186,  6816,          &
     &   6166,  5536,  6186,  2080,  6176,  6688,  5672,  5656,  2592,          &
     &   6501,  6491,  5851,  5861, 14693,  5546, 14053,  5526, 14043,          &
     &   6806, 14683,  6176,  2730,  6166, 14378,  5536,  2720,  5721,          &
     &  14823,  5735,  2521,  5536, 15008,  6166, 14378,  5721, 14823,          &
     &   6617,  1639,  5735,  6176, 14358,  6176,  2535,  6166,  6186,          &
     &   6816,  5536,  2090,  5526,  6826,  5546,  2710, 13844,  5672,          &
     &   6696,  5656, 14872,  5920,  2336, 13612,  5672,  6680,  6696/
! ----------------------------------------------------------------------------------------------------------------------------------
DATA IF002/  1560,  6176, 14872,  6696,  6176,  1568,  5672,            &
     &   6696,  5656,  6680,  1576,  6176,  6680,  6696,  5656,  5672,            &
     &   2080,  6176,  6186,  6747,  5595, 14378,  6757,  6166,  5605,            &
     &   2661, 15124,  6696,  5672,  6680, 13848,  6432,  1824,  6696,            &
     &   6680,  5672,  5656, 14888,  6180,  2075,  5656,  6696,  5672,            &
     &   6680, 13848,  6432,  1824,  5536,  6186,  6816,  6166,  5536,            &
     &  15008,  6186,  2070,  5656,  5672,  6696,  6680,  5656, 14888,            &
     &   5672,  2584,  6176,  5928,  6440,  6176,  6688,  6424,  6176,            &
     &   5912,  5664,  2080, 10204, 10077, 10015, 10017, 10083, 10212,            &
     &  10340, 10467, 10529, 10527, 10461, 10332, 14300,  5983,  5985/
!-----------------------------------------------------------------------------------------------------------------------------------
DATA IF003/ 14177,  6046, 14242,  6109, 14307,  6173, 14371,            &
     &   6237, 14435,  6302, 14498,  6367,  2273,  5916,  5924,  6436,            &
     &   6428, 14108,  5981, 14179,  6045, 14243,  6109, 14307,  6173,            &
     &  14371,  6237, 14435,  6301, 14499,  6365,  2275,  6170,  5859,            &
     &   6499, 14362,  5986,  6173, 14562,  6176,  6114,  6176,  2146,            &
     &  10273, 10208, 10271, 10336,  2081, 10204, 10077, 10015, 10017,            &
     &  10083, 10212, 10340, 10467, 10529, 10527, 10461, 10332,  2012,            &
     &  10133,  9942,  9752,  9627,  9566,  9570,  9637,  9768,  9962,            &
     &  10155, 10411, 10602, 10792, 10917, 10978, 10974, 10907, 10776,            &
     &  10582, 10389,  1941, 10122,  9803,  9549,  9359,  9170,  9045/
!-----------------------------------------------------------------------------------------------------------------------------------
DATA IF004/  8921,  8862,  8866,  8935,  9067,  9198,  9393,            &
     &   9587,  9845, 10166, 10422, 10741, 10995, 11185, 11374, 11499,            &
     &  11623, 11682, 11678, 11609, 11477, 11346, 11151, 10957, 10699,            &
     &  10378,  1930,  6186,  5527,  6743,  2090,  1931,  9163,  9355,            &
     &   9359,  9167, 13259,  5138,  5088,  5280,  1042,  5081, 13280,            &
     &   5472,  1369,  5067, 13280,  5472, 13643,  4825, 13913,  5714,            &
     &    722,  4815,  9551,  9810,  9557,  9173,  4825,  5084, 13916,            &
     &   5280,  1163,  4815, 13916,  9180,  8924,  8928,  9184, 13276,            &
     &   9803,  9547,  9551,  9807,  1611,  9810,  9355,  9163,  8911,            &
     &   8914,  9561,  9564,  9376,  9180,  9173,  1611,  9177,  9372/
!-----------------------------------------------------------------------------------------------------------------------------------
DATA IF005/  1184,  9568,  9177,  9170,  1355,  9184,  9561,            &
     &   9554,   971,  5263, 13468,  5721, 13010,  4825,  1618,  5263,            &
     &  13468,  4821,  1621,  8905,  9165,   975,  4821,  1621,  5068,            &
     &    971,  4811,  1632,  8911,  8924,  9184,  9568,  5724,  9807,            &
     &   9547,  5067,  4815,  1628,  5067, 13643,  5259,  5280,   988,            &
     &   8924,  9184,  9568,  9820,  9817,  4815,  4811,  1611,  4832,            &
     &   9824,  5724,  9365,  9557,  9810,  9807,  9547,  9163,   719,            &
     &   5451,  5472,  4821,  4818,  1618,  8911,  9163,  9547,  9807,            &
     &   9813,  5465,  4825,  4832,  1632,  8917,  9557,  9810,  9807,            &
     &   9547,  9163,  8911,  8921,  9376,  1632,  4832,  5728,  9820/
!-----------------------------------------------------------------------------------------------------------------------------------
DATA IF006/  9170,   971,  9163,  9547,  9807,  9810,  9557,            &
     &   9173,  8921,  8924,  9184,  9568,  9820,  9817, 13653,  9173,            &
     &   8914,  8911,   971,  8907,  9355,  9810,  9820,  9568,  9184,            &
     &   8924,  8921,  9173,  1621,  5068, 13259,  5073,   978, 13003,            &
     &   9163,  9359, 13458,  5272,  1177,  5451,  4821,  1376,  4818,            &
     &  13906,  5721,   729, 13003,  5067,  5717,   992,  5260, 13451,            &
     &   9362,  9817,  9820,  9568,  9184,   732,  9547,  9163,  8911,            &
     &   8924,  9184,  9568,  9820,  9810,  9362,  9365,  1628,  4811,            &
     &   8921,  5280,  5721, 13899,  4818,  1618,  4811,  4832,  9568,            &
     &   9820,  5721,  5461, 13013,  9557,  9810,  5711,  5451,   715/
!-----------------------------------------------------------------------------------------------------------------------------------
DATA IF007/  9820,  9568,  9184,  8924,  8911,  9163,  9547,            &
     &   1615,  4811,  9547,  9807,  5724,  5472, 13024,  5088,   971,            &
     &   4811,  4832, 13920,  5461, 13013,  4811,  1611,  4811,  4832,            &
     &  13920,  5269,   725,  9820,  9568,  9184,  8924,  8911,  9163,            &
     &   5707,  5714,  1362,  4811, 13024,  4821, 13909,  5728,  1611,            &
     &   5067, 13643,  5259, 13472,  5088,  1376,  8911,  9163,  5451,            &
     &   5711,  1632,  4811, 13024,  5728,  4821,  1611,  4832,  4811,            &
     &   1611,  4811,  4832,  5269,  5728,  1611,  4811, 13024,  4828,            &
     &  13903,  5728,  1611,  8911,  9163,  9547,  9807,  9820,  9568,            &
     &   9184,  8924,   719,  4811,  4832,  9568,  9820,  5721,  5461/
!-----------------------------------------------------------------------------------------------------------------------------------
DATA IF008/   725,  8911,  9163,  9547,  9807,  9820,  9568,            &
     &   9184,  8924, 13007,  5266,  1611,  4811,  4832,  9568,  9820,            &
     &   5721,  5461, 13013,  5269,  1611,  8911,  9163,  9547,  9807,            &
     &   9810,  9557,  9173,  8921,  8924,  9184,  9568,  1628,  4832,            &
     &  13920,  5280,  1163,  4832,  8911,  9163,  5451,  5711,  1632,            &
     &   4832,  5259,  1632,  4832,  5067,  5269,  5451,  1632,  4811,            &
     &   4815,  5724, 13920,  4832,  4828,  5711,  1611,  4832,  4828,            &
     &   5269, 13451,  5728,  5724,  1173,  4832,  5728,  5724,  4815,            &
     &   4811,  1611,  5280,  4832,  4811,  1163,  5707,   736,  4811,            &
     &   5259,  5280,   736,  4821,  5276, 13909,  5276,  1167,  5263/
!-----------------------------------------------------------------------------------------------------------------------------------
DATA IF009/  4821, 13468,  4821,  1621,  4832,  1365,  8911,            &
     &   8917,  9177,  9369,  9810,  9355,  9163, 13007,  5721,  1611,            &
     &   4811, 13024,  8914,  9369,  9561,  9813,  9807,  9547,  9355,            &
     &    722,  5721,  9177,  8917,  4815,  5067,  1611,  5728, 13899,            &
     &   9810,  9369,  9177,  8917,  8911,  9163,  9355,  1618,  4818,            &
     &   9810,  9813,  9561,  9177,  8917,  8911,  9163,  1355,  5259,            &
     &   5276, 13664,  5077,  1365,  9156,  5444,  5704, 13913,  9810,            &
     &   9369,  9177,  8917,  8911,  9163,  9355,  1618,  4811, 13024,            &
     &   8921,  5465,  5717,  1611,  5067, 13643,  5259,  5269, 13269,            &
     &   5275,  1180,  5061,  5253,  5449, 13657,  5471,  1376,  4811/
!-----------------------------------------------------------------------------------------------------------------------------------
DATA IF010/ 13024,  5724, 13007,  5269,  1611,  5067, 13643,            &
     &   5259,  5280,   992,  4811, 13017,  8917,  5081,  5269, 13451,            &
     &   9365,  5465,  5717,  1611,  4811, 13017,  8917,  9177,  5465,            &
     &   5717,  1611,  8911,  8917,  9177,  9561,  9813,  9807,  9547,            &
     &   9163,   719,  4804, 13017,  8914,  9369,  9561,  9813,  9807,            &
     &   9547,  9355,   722,  5700, 13913,  9810,  9369,  9177,  8917,            &
     &   8911,  9163,  9355,  1618,  4811, 13017,  8914,  5273,  1625,            &
     &   8907,  9547,  9807,  9554,  9170,  8917,  9177,  1625,  5081,            &
     &  13657,  5280,  5263,  1355,  4825,  8911,  9163,  9547, 13903,            &
     &   5721,  1611,  4825,  5259,  1625,  4825,  5067,  5266,  5451/
!-----------------------------------------------------------------------------------------------------------------------------------
DATA IF011/  1625,  4811, 13913,  4825,  1611,  9156,  5444,            &
     &   5704, 13913,  4825,  8911,  9163,  1611,  4825,  5721,  4811,            &
     &   1611,  5259,  5007,  5075,  4885,  5080,  5020,  1184,  5259,            &
     &  13458,  5273,  1184,  5067,  5327,  5267,  5461,  5272,  5340,            &
     &    992,  4819,  5079,  5459,  1623,   131,     0,     0,     0,            &
     &      0,     0,     0,     0,     0,     0,     0,     0,     0,            &
     &      0,     0,     0,     0,     0,     0,     0,     0,     0,            &
     &      0,     0,     0,     0,     0,     0,     0,     0,     0,            &
     &      0,     0,     0,     0,     0,     0,     0,     0,     0,            &
     &      0,     0,     0,     0,     0,     0,     0,     0,     0/
!-----------------------------------------------------------------------------------------------------------------------------------
DATA IPT001/     1,     8,    19,    24,    30,    35,    48,            &
     &     52,    56,    64,    69,    74,    78,    85,    90,    95,            &
     &    100,   106,   115,   122,   129,   136,   144,   152,   162,            &
     &    190,   209,   220,   225,   238,   259,   292,   296,   297,            &
     &    306,   310,   318,   328,   340,   351,   354,   358,   362,            &
     &    368,   372,   375,   377,   379,   381,   391,   396,   404,            &
     &    414,   419,   428,   438,   443,   460,   470,   474,   480,            &
     &    483,   487,   491,   499,   510,   517,   529,   537,   545,            &
     &    552,   557,   566,   572,   578,   583,   588,   591,   596,            &
     &    602,   611,   618,   629,   638,   650,   654,   660,   663/
!-----------------------------------------------------------------------------------------------------------------------------------
DATA IPT002/   668,   676,   683,   689,   693,   695,   699,            &
     &    704,   709,   711,   721,   731,   737,   747,   756,   761,            &
     &    773,   779,   786,   792,   798,   803,   813,   820,   829,            &
     &    839,   849,   854,   862,   867,   874,   877,   882,   886,            &
     &    894,   898,   905,   909,   916,   920,     0,     0,     0,            &
     &      0,     0,     0,     0,     0,     0,     0,     0,     0,            &
     &      0,     0,     0,     0,     0,     0,     0,     0,     0,            &
     &      0,     0,     0,     0,     0,     0,     0,     0,     0,            &
     &      0,     0,     0,     0,     0,     0,     0,     0,     0,            &
     &      0,     0,     0,     0,     0,     0,     0,     0,     0/
!-----------------------------------------------------------------------------------------------------------------------------------
      N=NN           ! NUMBER OF CHARACTERS
      AA=A           ! PLOTTING ANGLE
      SI=SIN(AA*0.0174532)
      CO=COS(AA*0.0174532)
      LENGTH=.TRUE.      ! PLOT (TRUE) OR LENGTH ONLY
      AL=0.0            ! PLOTTED LENGTH
      ISS=IS            ! CENTERING FLAG
      IF (ISS.EQ.-3) LENGTH=.FALSE.
      IF (ISS.GT.-1) LENGTH=.FALSE.
      OX=OLDX           ! SAVE CURRENT POSITION
      OY=OLDY
1100  CONTINUE       ! TOP OF LENGTH COMPUTATION
      AL=0.0            ! LENGTH OF PLOTTED STRING ACCUMULATOR
      X1=X           ! LOWER LEFT CORNER
      Y1=Y
      IF (ISS.EQ.0) THEN  ! CENTERED
         X1=X-AL/2.*CO+S/2.*SI
         Y1=Y-S/2.*CO-AL/2.*SI
      ENDIF
      IF (ISS.EQ.1) THEN  ! LOWER RIGHT CORNER
         X1=X-AL*CO
         Y1=Y-AL*SI
      ENDIF
      IF (X.GT.998.0.OR.Y.GT.998.0) THEN
         IF (X.LT.998.0) OLDX=OLDX+X1
         IF (Y.LT.998.0) OLDY=OLDY+Y1
      ELSE
         OLDX=X1
         OLDY=Y1
      ENDIF
      X0=OLDX
      Y0=OLDY
      IF (LENGTH.AND.N.LT.0) CALL draw_(OLDX,OLDY) ! PLOT TO START
      SS=S/21.    ! SCALE FACTOR
      I=0         ! CHARACTER COUNTER
50    continue
      I=I+1
      IF (I.GT.IABS(N)) GOTO 1000  ! END OF STRING COUNT
         ICC=ICHAR(T(I:I))  ! GET ITH ASCII CHARACTER
         IF (ICC.GT.127) GOTO 50  ! CODE TO LARGE
         IF (ICC.EQ.0.AND.I.GT.1) GOTO 1000 ! END OF STRING REACHED
         IXOFF=11       ! OFFSET
         IYOFF=11
         IF (ICC.LT.32) THEN  ! DIFFERENT SYMBOL OFFSET
            IXOFF=32
            IYOFF=32
         ENDIF
         IL=IPNT(ICC+1)   ! STARTING INDEX
         IW=21          ! CHARACTER WIDTH
         IF (IL.EQ.0) GOTO 90  ! NO PLOTTING INFO
         IPENLAST=3
70       CONTINUE
         IY=IBITS(IFNT(IL),0,6)
         IX=IBITS(IFNT(IL),6,6)
         IPEN=IBITS(IFNT(IL),12,2)
         IP=IPENLAST
         IPENLAST=IPEN
         XX=SS*(IX-IXOFF)
!c       Y1=SS*(IY-IYOFF+ISUB)
         Y1=SS*(IY-IYOFF)
         X1=XX*CO-Y1*SI+OLDX
         Y1=XX*SI+Y1*CO+OLDY
         IF (IP.EQ.0) IP=2
         IF (IP.EQ.1) IP=2
         IF (LENGTH) CALL plot_(X1,Y1,IP)
         IL=IL+1
         IF (IPEN.NE.0) GOTO 70
90       continue
         XX=SS*IW       ! END OF CHARACTER
         AL=AL+SS*IW
         OLDX=XX*CO+OLDX
         OLDY=XX*SI+OLDY
         GOTO 50
1000  continue
      IF (.NOT.LENGTH) THEN ! FINISHED LENGTH-ONLY PASS
         LENGTH=.TRUE.
         IF (ISS.EQ.-3) THEN ! RETURN END POSITION
            X=OLDX
            Y=OLDY
         ENDIF
         OLDX=OX     ! RESTORE OLD POSITION
         OLDY=OY
         IF (ISS.EQ.0.OR.ISS.EQ.1) GOTO 1100
      ELSE
         IF (N.LE.1) CALL move_(X0,Y0) ! LEAVE PEN AT START
         IF (ISS.EQ.-2) THEN ! RETURN END POSITION
            X=OLDX
            Y=OLDY
         ENDIF
      ENDIF
END SUBROUTINE DL_SYMBOL
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
INTEGER FUNCTION inbox_(X,Y,X_BOTTOM_LEFT,Y_BOTTOM_LEFT,X_top_right,Y_top_right)
!
!     FORTRAN-77 VERSION:   DGL JULY, 1987
!     CHECKS TO SEE IF POINT X,Y IS IN RECTANGLE
!     RETURNS ZERO IF IT IS
!     inbox_ tests a point to determine if it lies in a rectangle defined
!     by <x1,y1>,<x2,y2> and returns an integer value indicating where the point
!     is in relation to the rectangle.  The value can easily be decoded by
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

      INTEGER            :: CD

      CD=0  ! start off assuming <x,y> is in or on the box

      ! check x range, and assign CD=1 if to left of box, 0 if in range, and 2 if to right of box
      IF (X .LT. x_bottom_left) THEN
         CD=1
      ELSEIF(X .GT. x_top_right)THEN
         CD=2
      ENDIF

      ! check y range, and add 4 to CD if below box and add 8 if above box
      IF (Y .LT. Y_bottom_left) THEN
         CD=CD+4
      ELSEIF (Y .GT. y_top_right)THEN
         CD=CD+8
      ENDIF

      ! now CD=0 only if <x,y> is in or on the box
      inbox_=CD

END FUNCTION inbox_
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine test_suite_M_slices()
use M_verify, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg
use M_verify, only : unit_check_level

!! setup
   call test_dl_init()
   call test_dl_slices()
   call test_dl_symbol()
!! teardown
contains
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_dl_init()

   call unit_check_start('dl_init',msg='')
   !!call unit_check('dl_init', 0.eq.0, 'checking',100)
   call unit_check_done('dl_init',msg='')
end subroutine test_dl_init
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_dl_slices()

   call unit_check_start('dl_slices',msg='')
   !!call unit_check('dl_slices', 0.eq.0, 'checking',100)
   call unit_check_done('dl_slices',msg='')
end subroutine test_dl_slices
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_dl_symbol()

   call unit_check_start('dl_symbol',msg='')
   !!call unit_check('dl_symbol', 0.eq.0, 'checking',100)
   call unit_check_done('dl_symbol',msg='')
end subroutine test_dl_symbol
!===================================================================================================================================
end subroutine test_suite_M_slices
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
end module m_slices
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
