     program demo_dl_slices
     !     WRITTEN BY: DGL, LAST REVISED ON  5-JAN-1994 10:31:18.86
     !                 JSU,                 19-JUL-2005
      use M_pixel
      use M_pixel__writegif_animated, only : write_animated_gif
      use :: M_pixel__slices, only : dl_slices, dl_init, dl_symbol
      implicit none
        integer,parameter  :: ix=35
        integer,parameter  :: iz=45
        real               :: surfdat(ix,iz)              ! array of y values
        integer            :: movie(85+90+90,0:500,0:500) ! array of y values
        real,save          :: tpi=3.141592654
        integer            :: icol(255)
        character(len=80)  :: xt,yt,zt                    ! axis titles
        real :: a,b,dm,dx
        real :: smx,smy,smz
        real :: tsx,tsy,tsz
        real :: xe,xh,xs
        real :: ye,yh,ys
        real :: ze,zh,zs
     !
        integer :: i
        integer :: i10,i20,i40
        integer :: iax
        integer :: iflag
        integer :: iframe
        integer :: ii
        integer :: j
        integer :: mlx,mly,mlz
        integer :: ndx,ndy,ndz
        integer :: nmx,nmy,nmz
        integer :: nnx,nny,nnz
        integer :: nx,nxt
        integer :: nyt
        integer :: nz,nzt
     !
     ! (NOTE: color array accessed only if mod(iflag,10)=1)
     ! icol  (i): color list
     !            icol(1) : color for axis lines
     !            icol(2) : color for axis numbers
     !            icol(3) : color for axis titles
     !            icol(4) : color for axis exponents
     !            icol(5) : color index for lower plot surface (return)
     !            icol(6) : color index for upper plot surface (return)
     !     initialize the color array
           do i=1,255
              icol(i)=mod(i,7)
           enddo
     !     fill some arrays with data we can plot
           do j=1,ix
            do i=1,iz
             surfdat(j,i)=cos(tpi*real(j-1)/12.0)*cos(tpi*real(i-1)/12.0)
            enddo
           enddo
     !
           call prefsize(501,501)
           call vinit()
     !
           call dl_init(12.5,12.5,1.5,1.5,1.0)   ! set up plotting surface scale
           call linewidth(3)
           call color(4)
     !     now plot 3-d surface using slices with axis
           nx=ix
           nz=iz
     !
     ! length of axis in window units
     xh=6.0 ! xh,yh,zh (R): length of each axis
     yh=3.8
     zh=5.0
     !
     iflag=012
     iflag=000
     iflag=002
     ! iflag    (i): option flag
     !               (1's digit) =2: use color array (need all parameters)
     !                           =1: do not use color array
     !               (10's digit)=0: Plot sides
     !                           =1: Do not plot sides
     !
     iax= 01
     iax=-11
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
     !
     ! NOTE: the following optional parameters are used if iax < 0 or
     ! mod(iflag,10)=1
     !
     ! (see DL_AXISB for detailed description of axis parameters)
     !
     ! XAXIS:
           xs=-10.0   ! xs,xe (R): starting and ending values displayed on x axis
           xe=10.0
     !-----------------------
           nmx=4      ! (i): number of minor ticks between major ticks on x axis
           nnx=0      ! (i): highlight length of nnx-th minor tick on x axis
           mlx=4      ! (i): number of major tick marks on x axis
           tsx=-0.15  ! (R): size of title and numbers on x axis
                      !      < 0 auto exponent scaling (x10 to power) disabled
                      !      > 0 auto exponent scaling (x10 to power) enabled
           ndx=1      ! (i): number of digits to right of decimal point on x axis
           smx=0.1    ! (R): major tick length on x axis
     !-----------------------
           xt='dl_slices X TITLE' ! (C): title of x axis (width)
           nxt=len_trim(xt)       ! (i): number of characters in xt ;
                                  ! nxt = 0 : no axis plotted ; nxt > 0 : normal
     !
     ! YAXIS:
           ys=-10.0               ! ys,ye (R): starting and ending values
                                  ! displayed on y axis
           ye=10.0
     !-----------------------
           nmy=1      ! (i): number of minor ticks between major ticks on y axis
           nny=0      ! (i): highlight length of nny-th minor tick on y axis
           mly=3      ! (i): number of major tick marks on y axis
           tsy=-0.15  ! (R): size of title and numbers on y axis
                      !      < 0 auto exponent scaling (x10 to power) disabled
                      !      > 0 auto exponent scaling (x10 to power) enabled
           ndy=1      ! ndy   (i): number of digits to right of decimal point
                      !      on y axis
           smy=0.10   ! smy  (R): major tick length on y axis
     !-----------------------
           yt='dl_slices Y TITLE' ! (C): title of y axis (width)
           nyt=len_trim(yt)       ! (i): number of characters in xt ;
                                  ! nyt = 0 : no axis plotted ; nyt > 0 : normal
     !
     ! ZAXIS:
           zs=1.0
           ze=1.0      ! (R): starting and ending value displayed on z axis
     !-----------------------
           nmz=3       ! (i): number of minor ticks between major ticks on z axis
           nnz=2       ! (i): highlight length of nnz-th minor tick on z axis
           mlz=2       ! (i): number of major tick marks on z axis
           tsz=-0.15   ! (R): size of title and numbers on z axis
                       !      < 0 auto exponent scaling (x10 to power) disabled
                       !      > 0 auto exponent scaling (x10 to power) enabled
           ndz=1       ! (i): number of digits to right of decimal point on z axis
           smz=0.1     ! (R): major tick length on z axis
     !-----------------------
           zt='SLICE'         ! (C): title of z axis (width)
           nzt=len_trim(zt)   ! (i): number of characters in xt ;nzt = 0 :
                              !      no axis plotted ; nzt > 0 : normal
     !
     !          (NOTE: the following optional parameters are accessed only if
     !                 iax < 0 or mod(iflag,10)=1)
           dx=1.0
           dm=-1.0   ! dm,dx (R): minimum and maximum values of SURFDAT array
     ! view angles
     ! A    (R): angle of x axis from horizontal 0-80 degrees
     ! B    (R): angle of z axis from horizontal 5-80 degrees
     !           note: origin (1,1) is in lower-left corner
     !                 x axis runs left to right on screen
     !                 y axis runs up to down on screen
     !                 z axis appears to run into the screen
     !                   but is angled to the right
           iframe=1
           b=15.0
           do i10=1,85   ! Animate cycling thru angle A
            a=i10
            call color(7)
            call clear()
            call color(0)
            call dl_slices(surfdat,ix,iz,nx,nz,a,b,xh,yh,zh,iflag,iax,  &
          & xt,nxt,  &
          & xs,xe,nmx,nnx,mlx,tsx,ndx,smx,  &
          & yt,nyt,  &
          & nmy,nny,mly,tsy,ndy,smy,  &
          & zt,nzt,  &
          & zs,ze,nmz,nnz,mlz,tsz,ndz,smz,dm,dx,icol)
     !
     !      add a label after master routine call
            call color(1)
            call linewidth(1)
            call dl_symbol(0.0,0.0,0.25,'VAX3DX',0.0,6,-1)
            movie(iframe,:,:)=p_pixel(:,:)
            iframe=iframe+1
           enddo
     !
           a=25
           do i20=1,90   ! Animate cycling thru angle B
            b=i20
            call color(7)
            call clear()
            call color(0)
            call dl_slices(surfdat,ix,iz,nx,nz,a,b,xh,yh,zh,iflag,iax,  &
          & xt,nxt,  &
          & xs,xe,nmx,nnx,mlx,tsx,ndx,smx,  &
          & yt,nyt,  &
          & nmy,nny,mly,tsy,ndy,smy,  &
          & zt,nzt,  &
          & zs,ze,nmz,nnz,mlz,tsz,ndz,smz,dm,dx,icol)
            movie(iframe,:,:)=p_pixel(:,:)
            iframe=iframe+1
           enddo
     !
     iax=01
     iflag=012
     ii=1
     !
     do i40=1,90*ii  ! Animate cycling thru angles A and B
        a=real(i40)/ii/2.0 ! should get warning when this exceeds 85
        b=real(i40)/ii/2.0
        call color(7)
        call clear()
        call color(0)
        call dl_slices(surfdat,ix,iz,nx,nz,a,b,xh,yh,zh,iflag,iax,  &
      & xt,nxt,  &
      & xs,xe,nmx,nnx,mlx,tsx,ndx,smx,  &
      & yt,nyt,  &
      & nmy,nny,mly,tsy,ndy,smy,  &
      & zt,nzt,  &
      & zs,ze,nmz,nnz,mlz,tsz,ndz,smz,dm,dx,icol)
        movie(iframe,:,:)=p_pixel(:,:)
        iframe=iframe+1
     enddo
     !
     call vexit()    ! close up plot package
     call write_animated_gif('dl_slices.3m_pixel.gif',movie,p_colormap,delay=5)
     !call execute_system_command('display dl_slices.3m_pixel.gif')
     end program demo_dl_slices
