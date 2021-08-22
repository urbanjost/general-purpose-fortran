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
'   xy(1f) - [M_xyplot] Draw a basic XY plot                                                                                     ',&
'   (LICENSE:PD)                                                                                                                 ',&
'                                                                                                                                ',&
'SYNOPSIS                                                                                                                        ',&
'   xy [ -f] FILE -xlabel STR -ylabel STR -d DEVICE -m NNNN -fn FILENAME -sz MARKER_SIZE                                         ',&
'                                                                                                                                ',&
'DESCRIPTION                                                                                                                     ',&
'   Create a basic XY plot from a simple ASCII table of numeric                                                                  ',&
'   values. The first column is assumed to be the shared X values for                                                            ',&
'   the other columns.                                                                                                           ',&
'                                                                                                                                ',&
'   A plot of all Y columns is followed by a plot of each Y column                                                               ',&
'   by default.                                                                                                                  ',&
'                                                                                                                                ',&
'   If an interactive device is selected (X11, x11, xtek, tek, PC)                                                               ',&
'   then a pause occurs after each plot. Single character commands are                                                           ',&
'    o q quit                                                                                                                    ',&
'    o n next                                                                                                                    ',&
'    o p previous                                                                                                                ',&
'    o v toggle verbose mode                                                                                                     ',&
'    o 0 redisplay plot of all curves                                                                                            ',&
'                                                                                                                                ',&
'OPTIONS                                                                                                                         ',&
'   -d         M_draw(3fm) device name (X11,pdf,svg, ...). Enter the                                                             ',&
'              device name "list" for a list of available devices on                                                             ',&
'              an otherwise valid command.                                                                                       ',&
'   -f         filename of format "X Y1 Y2 Y3 ..."                                                                               ',&
'   -fn FNAME  For file output, the default output name is xy.$DEVICE.                                                           ',&
'              If a name is specified containing a period it is used                                                             ',&
'              as-is. Otherwise the output file will be FNAME.$DEVICE                                                            ',&
'                                                                                                                                ',&
'   -xlabel    X-axis label. Default is "X"                                                                                      ',&
'   -ylabel    Y-axis label. Default is "Y"                                                                                      ',&
'   -title     plot title. Defaults to input filename                                                                            ',&
'   -m NNN     marker frequency. Place a marker at every nth point.                                                              ',&
'              The default is zero. Negative values suppress drawing                                                             ',&
'              the interconnecting lines.                                                                                        ',&
'   -sz        marker size as a percent of display width. Default is 2.0 .                                                       ',&
'                                                                                                                                ',&
'   -verbose   display plot information to stdout                                                                                ',&
'   -help      display help text to stdout and exit                                                                              ',&
'   -version   display version to stdout and exit                                                                                ',&
'                                                                                                                                ',&
'EXAMPLE                                                                                                                         ',&
'   Create a simple file with X and Y values and draw plot                                                                       ',&
'                                                                                                                                ',&
'      program demo_xy                                                                                                           ',&
'      integer :: i10                                                                                                            ',&
'      real :: x, y                                                                                                              ',&
'         ! set up the data file                                                                                                 ',&
'         open(unit=10,file=''xy.dat'')                                                                                          ',&
'         do i10=1,200                                                                                                           ',&
'            x=i10*0.50                                                                                                          ',&
'            y=x*sin(x)*100.0+400.0                                                                                              ',&
'            write(10,*)x,y,y+40.0,cos(y)*120.0+380.0                                                                            ',&
'         enddo                                                                                                                  ',&
'         flush(10)                                                                                                              ',&
'         call execute_command_line(''xy xy.dat -xlabel X-axis -ylabel Y-axis'')                                                 ',&
'         call execute_command_line(''xy xy.dat -xlabel X-axis -ylabel Y-axis -d pdf'')                                          ',&
'         call execute_command_line(''xy xy.dat -xlabel X-axis -ylabel Y-axis -d svg'')                                          ',&
'         close(10,status=''delete'')                                                                                            ',&
'      end program demo_xy                                                                                                       ',&
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
!!    xy(1f) - [M_xyplot] Draw a basic XY plot
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    xy [ -f] FILE -xlabel STR -ylabel STR -d DEVICE -m NNNN -fn FILENAME -sz MARKER_SIZE
!!
!!##DESCRIPTION
!!    Create a basic XY plot from a simple ASCII table of numeric
!!    values. The first column is assumed to be the shared X values for
!!    the other columns.
!!
!!    A plot of all Y columns is followed by a plot of each Y column
!!    by default.
!!
!!    If an interactive device is selected (X11, x11, xtek, tek, PC)
!!    then a pause occurs after each plot. Single character commands are
!!     o q quit
!!     o n next
!!     o p previous
!!     o v toggle verbose mode
!!     o 0 redisplay plot of all curves
!!
!!##OPTIONS
!!    -d         M_draw(3fm) device name (X11,pdf,svg, ...). Enter the
!!               device name "list" for a list of available devices on
!!               an otherwise valid command.
!!    -f         filename of format "X Y1 Y2 Y3 ..."
!!    -fn FNAME  For file output, the default output name is xy.$DEVICE.
!!               If a name is specified containing a period it is used
!!               as-is. Otherwise the output file will be FNAME.$DEVICE
!!
!!    -xlabel    X-axis label. Default is "X"
!!    -ylabel    Y-axis label. Default is "Y"
!!    -title     plot title. Defaults to input filename
!!    -m NNN     marker frequency. Place a marker at every nth point.
!!               The default is zero. Negative values suppress drawing
!!               the interconnecting lines.
!!    -sz        marker size as a percent of display width. Default is 2.0 .
!!
!!    -verbose   display plot information to stdout
!!    -help      display help text to stdout and exit
!!    -version   display version to stdout and exit
!!
!!##EXAMPLE
!!
!!    Create a simple file with X and Y values and draw plot
!!
!!       program demo_xy
!!       integer :: i10
!!       real :: x, y
!!          ! set up the data file
!!          open(unit=10,file='xy.dat')
!!          do i10=1,200
!!             x=i10*0.50
!!             y=x*sin(x)*100.0+400.0
!!             write(10,*)x,y,y+40.0,cos(y)*120.0+380.0
!!          enddo
!!          flush(10)
!!          call execute_command_line('xy xy.dat -xlabel X-axis -ylabel Y-axis')
!!          call execute_command_line('xy xy.dat -xlabel X-axis -ylabel Y-axis -d pdf')
!!          call execute_command_line('xy xy.dat -xlabel X-axis -ylabel Y-axis -d svg')
!!          close(10,status='delete')
!!       end program demo_xy
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
'@(#)PROGRAM:        xy(1)>',&
'@(#)DESCRIPTION:    create a basic XY plot>',&
'@(#)VERSION:        1.0, 20180706>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)HOME PAGE:      http://www.urbanjost.altervista.org/index.html>',&
'@(#)COMPILED:       2021-08-21 22:19:14 UTC-240>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if --version was specified, stop
endif
end subroutine help_version
program draw_a_plot
use M_draw,    only : clear, color, prefsize, vexit, vinit, vflush, vsetflush, voutput, getkey
use M_draw,    only : centertext, drawstr, move2, textsize,strlength, rect, circle
use M_xyplot,  only : plot_set_plot_area, plot_set_nice_range, plot_init_globals
use M_xyplot,  only : plot_axes, plot_label, plot_line, plot_page, plot_resetplot
use M_xyplot,  only : plot_ids, plot_axis
use M_kracken, only : sget, lget, iget, kracken, rget
use M_strings, only : merge_str, v2s
use M_math,    only : bds
use M_io,      only : read_table
implicit none

! for read_table(3f)
real,allocatable             :: xy_array(:,:)
integer                      :: irows, icols
real                         :: xlow, xhigh, ylow, yhigh
integer                      :: i
logical                      :: verbose
real                         :: statistics(13)

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

   call kracken('plt',' -help .F. -version .F. -f -xlabel X -ylabel Y -d X11 -m 0 -fn xy -sz 2 -title "#N#" -verbose .F.')
   call help_usage(lget('plt_help'))                                ! if -help option is present, display help text and exit
   call help_version(lget('plt_version'))                           ! if -version option is present, display version text and exit

   device=trim(sget('plt_d'))
   xlabel=trim(sget('plt_xlabel'))
   ylabel=trim(sget('plt_ylabel'))
   marker_frequency=iget('plt_m')
   marker_size=rget('plt_sz')
   verbose=lget('plt_verbose')
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
         call voutput('xy.'//device)
      endif
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   xsmall=0.0
   xlarge=6.5
   ysmall=0.0
   ylarge=4.8
!-----------------------------------------------------------------------------------------------------------------------------------
   call vinit(device)
!-----------------------------------------------------------------------------------------------------------------------------------
   allocate(xy_array(0,0))
   filename=trim(adjustl(sget('plt_f')//' '//sget('plt_oo')))
   if(filename.eq.'')then
      write(*,*)'*xy* error: filename of form X Y1 Y2 Y3 ... required'
      stop 1
   endif
   call read_table(filename,xy_array,ierr)                ! read data assuming column 1 is X values and at least two columns
   irows=size(xy_array,dim=1)
   icols=size(xy_array,dim=2)
   if(ierr.ne.0)then
      write(*,*)'*xy* ERROR: datafile',filename,' contains errors '
      stop 3
   endif

   if(irows.eq.0.or.icols.lt.2)then
      write(*,*)'*xy* ERROR: datafile must have at least one row and at least two columns'
      stop 2
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
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
   !!write(*,*)'SIZE=',size(plot_ids)
   !!write(*,*)'color fill_style width dashcode marker marker_frequency marker_size marker_font legend'
   !!write(*,*)(plot_ids(i),new_line('A'),i=1,500)

   ! first pass plot all columns
   istart=2
   iend=icols
   xlow=minval(xy_array(:,1))                      ! find extremes in an array
   xhigh=maxval(xy_array(:,1))
   ! first pass plot all columns
   call drawone()
   key=getkey()
   ! if more than one Y column print each curve
   if(icols.gt.2)then
      if(device.eq.'x11'.or.device.eq.'X11'.or.device.eq.'PC'.or.device.eq.'tek'.or.device.eq.'xtek')then
         i=2
         INFINITE: do
            istart=i
            iend=i
            call drawone()
            key=getkey()
            select case(char(key))
             case('0')
               istart=2
               iend=icols
               call drawone()
               key=getkey()
             case('v')
               verbose=merge(.false.,.true.,verbose)
             case('p')
               i=max(2,i-1)
             case('h')
               write(*,'(a)')[character(len=80) :: &
               & ' 0   plot all curves and wait for a return', &
               & ' h   help                                 ', &
               & ' n   previous frame                       ', &
               & ' p   previous frame                       ', &
               & ' q   quit                                 ', &
               & ' v   toggle verbose mode                  ', &
               & ' ']
             case('n')
               i=min(icols,i+1)
             case('q')
               exit INFINITE
             case default
               i=min(icols,i+1)
               write(*,*)'q to quit, h for help'
            end select
         enddo INFINITE
      else
         do i=2,icols
            istart=i
            iend=i
            call drawone()
         enddo
      endif
   endif
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
      title=trim(sget('plt_title'))
      title=merge_str(filename,title, title.eq.'#N#' )//"("//v2s(istart)//"-"//v2s(iend)//")"
      ilets=len_trim(title)
      biggest=16.0
      if(ilets.le.biggest)then
         text_width=((xlarge-xsmall)*0.95)/biggest
      else
         text_width=((xlarge-xsmall)*0.95)/ilets
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
   integer :: i
      write(*,*)'FILE=',filename
      write(*,*)'TITLE=',title
      write(*,*)'XLABEL=',xlabel
      write(*,*)'YLABEL=',ylabel
      write(*,'(*(a,i0))')' ARRAY SIZE=',size(xy_array), ' where ROWS=',irows, ' and COLS=',icols
      write(*,*)'XLOW= ',xlow
      write(*,*)'XHIGH=',xhigh
      write(*,*)'YLOW= ',ylow
      write(*,*)'YHIGH=',yhigh
      do i=istart,iend
         write(*,'(a,i0)')' CURVE=',i
         call bds(xy_array(:,i),irows,statistics)
         write(*,*)' mean                         ',statistics(1)
         write(*,*)' second moment about the mean ',statistics(2)
         write(*,*)' third moment about the mean  ',statistics(3)
         write(*,*)' fourth moment about the mean ',statistics(4)
         write(*,*)' variance                     ',statistics(5)
         write(*,*)' standard deviation           ',statistics(6)
         write(*,*)' skewness                     ',statistics(7)
         write(*,*)' kurtosis                     ',statistics(8)
         write(*,*)' sum                          ',statistics(9)
         write(*,*)' largest value                ',statistics(10)
         write(*,*)' smallest value               ',statistics(11)
         write(*,*)' location of largest value    ',statistics(12)
         write(*,*)' location of smallest value   ',statistics(13)
      enddo
   end subroutine printdata
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()(
!-----------------------------------------------------------------------------------------------------------------------------------
   subroutine drawone()
!-----------------------------------------------------------------------------------------------------------------------------------
      ylow=minval(xy_array(:,istart:iend))
      yhigh=maxval(xy_array(:,istart:iend))
      call plot_set_nice_range(xlow,xhigh,ylow,yhigh) ! find nice ranges for axes
!-----------------------------------------------------------------------------------------------------------------------------------
      call plot_page(xsmall,xlarge,ysmall,ylarge)     ! lay out a virtual surface like an 8.5x11 piece of paper
      call color(4)
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
         call plot_set_plot_area(xsmall,xlarge,ysmall,ylarge-0.5*text_height)
      endif
      ! draw the axes. MANY OPTIONS CAN BE SET THROUGH MODULE VARIABLES AND CALLS
      call plot_axes()
      ! draw data scaled to last left axis drawn with requested PEN style
      do ipen=istart,iend
         ! CAUTION; X AND Y MAY BE CHANGED (IF LOG PLOTS ARE BEING DRAWN)
         call plot_line(ipen,irows,'toleft',xy_array(:,1),xy_array(:,ipen))
      enddo
      call vflush()
   end subroutine drawone
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()(
!-----------------------------------------------------------------------------------------------------------------------------------
end program draw_a_plot
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()(
!-----------------------------------------------------------------------------------------------------------------------------------
