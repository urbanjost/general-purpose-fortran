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
'   cn(1f) - [M_xyplot] Draw a basic contour plot                                                                                ',&
'   (LICENSE:PD)                                                                                                                 ',&
'                                                                                                                                ',&
'SYNOPSIS                                                                                                                        ',&
'   cn [ -f] FILE -xlabel STR -ylabel STR -d DEVICE -fn FILENAME                                                                 ',&
'                                                                                                                                ',&
'DESCRIPTION                                                                                                                     ',&
'   Create a basic contour plot from a simple ASCII table of numeric                                                             ',&
'   values. The file is assumed to be of the form x,y,z. The data                                                                ',&
'   is not required to be on a grid.                                                                                             ',&
'                                                                                                                                ',&
'OPTIONS                                                                                                                         ',&
'   -d         M_draw(3fm) device name (X11,pdf,svg, ...). Enter the                                                             ',&
'              device name "list" for a list of available devices on                                                             ',&
'              an otherwise valid command.                                                                                       ',&
'   -f         filename of format "X Y Z"                                                                                        ',&
'   -fn FNAME  For file output, the default output name is cn.$DEVICE.                                                           ',&
'              If a name is specified containing a period it is used                                                             ',&
'              as-is. Otherwise the output file will be FNAME.$DEVICE                                                            ',&
'                                                                                                                                ',&
'   -xlabel    X-axis label. Default is "X"                                                                                      ',&
'   -ylabel    Y-axis label. Default is "Y"                                                                                      ',&
'   -title     plot title. Defaults to input filename                                                                            ',&
'                                                                                                                                ',&
'   -verbose   display plot information to stdout                                                                                ',&
'   -help      display help text to stdout and exit                                                                              ',&
'   -version   display version to stdout and exit                                                                                ',&
'                                                                                                                                ',&
'EXAMPLE                                                                                                                         ',&
'  Create a simple file with X,Y,Z values and draw plot                                                                          ',&
'                                                                                                                                ',&
'   program demo_cn                                                                                                              ',&
'   use M_ContourPlot, only : contourlines                                                                                       ',&
'   implicit none                                                                                                                ',&
'   integer,parameter    :: NPTS=121                                                                                             ',&
'   integer              :: errCode                                                                                              ',&
'   integer,parameter    :: DBG=2                                                                                                ',&
'   integer              :: i,j,k                                                                                                ',&
'   real,dimension(NPTS) :: x,y,z                                                                                                ',&
'   character(len=:),allocatable :: cmd                                                                                          ',&
'   !                                                                                                                            ',&
'   k=0                                                                                                                          ',&
'   do j=0,10                                                                                                                    ',&
'      do i=0,10                                                                                                                 ',&
'         k=k+1                                                                                                                  ',&
'         x(k)=0.1*real(i)                                                                                                       ',&
'         y(k)=0.1*real(j)                                                                                                       ',&
'      end do                                                                                                                    ',&
'   end do                                                                                                                       ',&
'   !                                                                                                                            ',&
'   z=(x-0.5)**2 + (y-0.5)**2                                                                                                    ',&
'   z(:)=16.0*z(:)                                                                                                               ',&
'   ! write out the input values for plotting                                                                                    ',&
'   open(unit=dbg, file=''test.dat'', status=''replace'', &                                                                      ',&
'      iostat=errCode, action=''write'', position=''rewind'')                                                                    ',&
'   !!write(DBG,''(I4,3F12.6)'') (k,x(k),y(k),z(k),k=1,npts)                                                                     ',&
'   write(DBG,''(3F12.6)'') (x(k),y(k),z(k),k=1,npts)                                                                            ',&
'   flush(unit=dbg)                                                                                                              ',&
'   ! call plotting program                                                                                                      ',&
'   cmd=''cn test.dat -xlabel X-axis -ylabel Y-axis &                                                                            ',&
'   & -levels 0.1 0.2 0.5 1.0 2.0 4.0 6.0 8.0 ''                                                                                 ',&
'   call execute_command_line(cmd)                                                                                               ',&
'   call execute_command_line(cmd//'' -d pdf'')                                                                                  ',&
'   call execute_command_line(cmd//'' -d svg'')                                                                                  ',&
'   !                                                                                                                            ',&
'   close(DBG,status=''delete'')                                                                                                 ',&
'   end program demo_cn                                                                                                          ',&
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
!!    cn(1f) - [M_xyplot] Draw a basic contour plot
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    cn [ -f] FILE -xlabel STR -ylabel STR -d DEVICE -fn FILENAME
!!
!!##DESCRIPTION
!!    Create a basic contour plot from a simple ASCII table of numeric
!!    values. The file is assumed to be of the form x,y,z. The data
!!    is not required to be on a grid.
!!
!!##OPTIONS
!!    -d         M_draw(3fm) device name (X11,pdf,svg, ...). Enter the
!!               device name "list" for a list of available devices on
!!               an otherwise valid command.
!!    -f         filename of format "X Y Z"
!!    -fn FNAME  For file output, the default output name is cn.$DEVICE.
!!               If a name is specified containing a period it is used
!!               as-is. Otherwise the output file will be FNAME.$DEVICE
!!
!!    -xlabel    X-axis label. Default is "X"
!!    -ylabel    Y-axis label. Default is "Y"
!!    -title     plot title. Defaults to input filename
!!
!!    -verbose   display plot information to stdout
!!    -help      display help text to stdout and exit
!!    -version   display version to stdout and exit
!!
!!##EXAMPLE
!!
!!   Create a simple file with X,Y,Z values and draw plot
!!
!!    program demo_cn
!!    use M_ContourPlot, only : contourlines
!!    implicit none
!!    integer,parameter    :: NPTS=121
!!    integer              :: errCode
!!    integer,parameter    :: DBG=2
!!    integer              :: i,j,k
!!    real,dimension(NPTS) :: x,y,z
!!    character(len=:),allocatable :: cmd
!!    !
!!    k=0
!!    do j=0,10
!!       do i=0,10
!!          k=k+1
!!          x(k)=0.1*real(i)
!!          y(k)=0.1*real(j)
!!       end do
!!    end do
!!    !
!!    z=(x-0.5)**2 + (y-0.5)**2
!!    z(:)=16.0*z(:)
!!    ! write out the input values for plotting
!!    open(unit=dbg, file='test.dat', status='replace', &
!!       iostat=errCode, action='write', position='rewind')
!!    !!write(DBG,'(I4,3F12.6)') (k,x(k),y(k),z(k),k=1,npts)
!!    write(DBG,'(3F12.6)') (x(k),y(k),z(k),k=1,npts)
!!    flush(unit=dbg)
!!    ! call plotting program
!!    cmd='cn test.dat -xlabel X-axis -ylabel Y-axis &
!!    & -levels 0.1 0.2 0.5 1.0 2.0 4.0 6.0 8.0 '
!!    call execute_command_line(cmd)
!!    call execute_command_line(cmd//' -d pdf')
!!    call execute_command_line(cmd//' -d svg')
!!    !
!!    close(DBG,status='delete')
!!    end program demo_cn
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
'@(#)PROGRAM:        cn(1)>',&
'@(#)DESCRIPTION:    create a basic contour plot>',&
'@(#)VERSION:        1.0, 2018-07-06>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)HOME PAGE:      http://www.urbanjost.altervista.org/index.html>',&
'@(#)COMPILED:       2022-08-14 13:36:09 UTC-240>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if --version was specified, stop
endif
end subroutine help_version
module pass_levels
REAL,ALLOCATABLE               :: zContour(:)
character(len=20),allocatable  :: sContour(:)
end module pass_levels
program draw_a_contour
use M_draw,        only : clear, color, prefsize, vexit, vinit, vflush, vsetflush, voutput, getkey
use M_draw,        only : centertext, drawstr, move2, textsize,strlength, rect, circle
use M_xyplot,      only : plot_set_plot_area, plot_set_nice_range, plot_init_globals
use M_xyplot,      only : plot_axes, plot_label, plot_line, plot_page, plot_resetplot
use M_xyplot,      only : plot_ids, plot_axis
use M_kracken,     only : sget, lget, iget, kracken, rgets, sgets, rget
use M_strings,     only : merge_str
use M_math,        only : bds
use M_io,          only : read_table
USE M_contourplot, only : contourlines
use pass_levels
implicit none

! for read_table(3f)
real,allocatable             :: cn_array(:,:)
integer                      :: irows, icols
real                         :: xlow, xhigh, ylow, yhigh
logical                      :: verbose

integer                      :: ipen
integer                      :: ilets
real                         :: biggest
real                         :: xsmall, xlarge, ysmall, ylarge
real                         :: text_width, text_height
integer                      :: key
integer                      :: marker_frequency
real                         :: marker_size
character(len=:),allocatable :: device
character(len=:),allocatable :: xlabel
character(len=:),allocatable :: ylabel
character(len=:),allocatable :: title
character(len=:),allocatable :: filename
character(len=:),allocatable :: fn
integer                      :: ierr
integer                      :: istart,iend
integer                      :: iexp, jexp, ism
logical                      :: smooth
real                         :: eps

   call kracken('plt',' -help .F. -version .F. -f -xlabel X -ylabel Y -d X11 -m 0 -fn cn -sz 2 -title "#N#" -verbose .F. &
   & -smooth .F. -iexp 0 -jexp 0 -levels 0')
   call help_usage(lget('plt_help'))                                ! if -help option is present, display help text and exit
   call help_version(lget('plt_version'))                           ! if -version option is present, display version text and exit

   device=trim(sget('plt_d'))
   title=trim(sget('plt_title'))
   xlabel=trim(sget('plt_xlabel'))//'    '
   ylabel=trim(sget('plt_ylabel'))//'    '
   marker_frequency=iget('plt_m')
   marker_size=rget('plt_sz')
   verbose=lget('plt_verbose')
   iexp=iget('plt_iexp')
   jexp=iget('plt_jexp')
   smooth=lget('plt_smooth')
   if(smooth)then
      ism=1
   else
      ism=0
   endif
   zContour=rgets('plt_levels')
   write(*,*)'NUMERIC contour levels=',zContour
   scontour=sgets('plt_levels')
!-----------------------------------------------------------------------------------------------------------------------------------
   allocate(cn_array(0,0))
   filename=trim(adjustl(sget('plt_f')//' '//sget('plt_oo')))
   if(filename.eq.'')then
      write(*,*)'*cn* error: filename of form X Y Z ... required'
      stop 1
   endif
   call read_table(filename,cn_array,ierr)                ! read data assuming column 1 is X values and at least two columns
   irows=size(cn_array,dim=1)
   icols=size(cn_array,dim=2)
   if(ierr.ne.0)then
      write(*,*)'*cn* ERROR: datafile',filename,' contains errors '
      stop 3
   endif

   title=merge_str(filename,title, title.eq.'#N#' )
   if(irows.eq.0.or.icols.ne.3)then
      write(*,*)'*cn* ERROR: datafile must have at least one row and three columns'
      stop 2
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   if(device.eq.'')device='X11'                    ! determine output filename
   if(device.eq.'x11'.or.device.eq.'X11')then
      call prefsize(640,480)
   elseif(device.eq.'xtek'.or.device.eq.'tek')then
   else
      fn=trim(adjustl(sget('plt_fn')))
      if(index(fn,'.').eq.0)then
         call voutput(fn//'.'//device)
      else
         call voutput('cn.'//device)
      endif
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   xsmall=0.0
   xlarge=6.5
   ysmall=0.0
   ylarge=4.8
!-----------------------------------------------------------------------------------------------------------------------------------
   call vinit(device)
   call plot_init_globals()
   call plot_resetplot()
!-----------------------------------------------------------------------------------------------------------------------------------
   plot_axis%background=0        ! set background color of axis area
   plot_axis%grid_style(1)=0 !major x line  on
   plot_axis%grid_style(2)=0 !major y line  on
   plot_axis%grid_style(3)=0 !minor x tic   on
   plot_axis%grid_style(4)=0 !minor y tic   on
   !! plot_ids(-1)%color=6     ! set grid line color
   !! plot_ids(-2)%color=5     ! set grid line color
   !! plot_ids(0)%color=2      ! change color of pen 0 for drawing box around axis area
   !! plot_ids(-1)%dashcode=0  ! change grid dash pattern
   !! plot_ids(0)%width=30     ! change width of pen 0 for drawing box around axis area
   !! IPEN=1
   plot_ids(1:)%width=10  ! change width of pen IPEN
   if(marker_frequency.lt.0)then          ! scatter plot
      plot_ids(1:)%dashcode=-1
      marker_frequency=-marker_frequency
   endif
   plot_ids(1:)%marker_frequency=marker_frequency  ! how often to put out marker on pen
   plot_ids(1:)%marker_size=marker_size
   !!block
   !!integer :: i
   !!write(*,*)'SIZE=',size(plot_ids)
   !!write(*,*)'color fill_style width dashcode marker marker_frequency marker_size marker_font legend'
   !!write(*,*)(plot_ids(i),new_line('A'),i=1,500)
   !!endblock

   istart=2
   iend=icols
   xlow=minval(cn_array(:,1))                      ! find extremes in an array
   xhigh=maxval(cn_array(:,1))
   ! first pass plot all columns
   call drawone()
   key=getkey()
!-----------------------------------------------------------------------------------------------------------------------------------
   ! WARNING: NOTE RECALLING plot_set_nice_range WITH LOG10(). THIS WILL PROBABLY NOT BE REQUIRED IN THE NEXT VERSION
   !!plot_axis%xlogmode=1   ! logarithmic x-axis
   !!call plot_set_nice_range(log10(xlow),log10(xhigh),ylow,yhigh)
!-----------------------------------------------------------------------------------------------------------------------------------
   call vexit()
   stop
contains
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()(
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine drawtitle()
   ilets=len_trim(title)
   biggest=16.0
   if(real(ilets).le.biggest)then
      text_width=((xlarge-xsmall)*0.95)/biggest
   else
      text_width=((xlarge-xsmall)*0.95)/real(ilets)
   endif
   call centertext(.true.)
   text_height=text_width*1.16
   call textsize(text_width,text_height)
   !!text_width=((xlarge-xsmall)/strlength(title))*text_width
   text_height=text_width*1.16
   call move2((xlarge-xsmall)/2.0,ylarge-0.7*text_height)
   call drawstr(title)
   call centertext(.false.)
end subroutine drawtitle
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()(
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine printdata()
   write(*,*)'FILE=',filename
   write(*,*)'TITLE=',title
   write(*,*)'XLABEL=',xlabel
   write(*,*)'YLABEL=',ylabel
   write(*,'(*(a,i0))')' ARRAY SIZE=',size(cn_array), ' where ROWS=',irows, ' and COLS=',icols
   write(*,*)'XLOW= ',xlow
   write(*,*)'XHIGH=',xhigh
   write(*,*)'YLOW= ',ylow
   write(*,*)'YHIGH=',yhigh
end subroutine printdata
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()(
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine drawone()
real                   :: delta
real                   :: ystep
real                   :: xl, xr
real                   :: y
real                   :: ylarge2
external my_cntcrv
!-----------------------------------------------------------------------------------------------------------------------------------
   ylow=minval(cn_array(:,istart:istart))
   yhigh=maxval(cn_array(:,istart:istart))
   call plot_set_nice_range(xlow,xhigh,ylow,yhigh) ! find nice ranges for axes
!-----------------------------------------------------------------------------------------------------------------------------------
   call plot_page(xsmall,xlarge,ysmall,ylarge)     ! lay out a virtual surface like an 8.5x11 piece of paper
   call color(15)
   call clear()
   call color(7)
   call vsetflush(.false.)
!-----------------------------------------------------------------------------------------------------------------------------------
   call drawtitle()
!-----------------------------------------------------------------------------------------------------------------------------------
   if(verbose)then
      call printdata()
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   call plot_label(1,xlabel)
   call plot_label(2,ylabel)
!-----------------------------------------------------------------------------------------------------------------------------------
   if(title.ne.'')then
      ylarge2=ylarge-0.5*text_height
      call plot_set_plot_area(xsmall,xlarge,ysmall,ylarge2)
   else
      ylarge2=ylarge
   endif
   delta=(xlarge-xsmall)/5.0
   xl=0.10*delta
   xr=0.90*delta
   ystep=(ylarge2-ysmall)/2.0/real(max(14,size(zContour)))
   y=ylarge2-1.5*ystep
   call textsize(ystep,ystep)
   do ipen=1,size(zContour)
      y=y-0.5*ystep
      call plot_line(ipen,2,'toframe',[xl,xr],[y,y])
      y=y-0.5*ystep
      y=y-ystep
      call move2(xl,y)
      call drawstr(sContour(ipen))
   enddo
   call plot_set_plot_area(xsmall+delta,xlarge,ysmall,ylarge2)
   ! draw the axes. MANY OPTIONS CAN BE SET THROUGH MODULE VARIABLES AND CALLS
   call plot_axes()
   ! draw data scaled to last left axis drawn with requested PEN style
   ! CAUTION; X AND Y MAY BE CHANGED (IF LOG PLOTS ARE BEING DRAWN)
   open(3,file='cn.gnu')
   CALL ContourLines(cn_array(:,1),cn_array(:,2),cn_array(:,3), ism,iexp,jexp, zContour, eps,ierr,my_cntcrv)
   call vflush()
end subroutine drawone
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()(
!-----------------------------------------------------------------------------------------------------------------------------------
end program draw_a_contour
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine my_cntcrv(x,y,n,z)
! user-supplied routine for drawing a contour line
use M_xyplot,    only : plot_line
use pass_levels, only : zContour
implicit none
integer,intent(in)           :: n
real,intent(in),dimension(n) :: x,y
real,intent(in)              :: z
integer                      :: gnu=3
integer                      :: k
integer                      :: ipen
integer                      :: i

   write(gnu,'("# level ",g0," values=",i0)')z,n
   write(gnu,'(2f12.5)') (x(k),y(k),k=1,n)
   write(gnu,'(a)') " "

   !!ipen=findloc(zcontour,value=z,dim=1)
   ipen=0
   do i=1,size(zcontour)
      if(zcontour(i).eq.z)ipen=i
   enddo

   call plot_line(ipen,n,'toleft',x,y)

end subroutine my_cntcrv
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()(
!-----------------------------------------------------------------------------------------------------------------------------------
