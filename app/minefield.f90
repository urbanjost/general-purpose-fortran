program sweepit
use M_draw
use M_time, only : system_sleep, ordinal_seconds
use M_kracken, only: kracken, iget, sget, lget
implicit none

! ident_1="@(#) minefield(1f) [M_draw] minefield sweeper game"

logical :: switch
integer :: irows
integer :: icols
integer :: iordinal
integer :: istat
real    :: xx, yy

!  define command arguments, default values and crack command line
   call kracken('mine','-c 30 -r 16 -d X11 -help .F. -version .F. -switch .F. -x  1240 -y 700')
   call help_usage(lget('mine_help'))                   ! if -help option is present, display help text and exit
   call help_version(lget('mine_version'))              ! if -version option is present, display version text and exit
   call prefsize(iget('mine_x'),iget('mine_y'))
   call vinit('X11')
   call system_sleep(2)
   irows=iget('mine_r')
   icols=iget('mine_c')
   switch=lget('mine_switch')

   INFINITE: do
      call minefield(irows,icols,switch)
      write(*,*)'"q" to quit; any other character to start new game'
      LOOP : do
         iordinal=checkkey()                ! see if a character was pressed in graphic window, get ordinal of letter last pressed
         istat=locator(xx,yy)               ! get location and mouse key pressed
         call system_sleep(0.04)            ! pause
         if(iordinal.eq.113) exit INFINITE  ! quit if letter q
         if(istat.ne.0)exit LOOP            ! if mouse clicked start new session
      enddo LOOP
   enddo INFINITE

   call vexit()

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
'NAME                                                                            ',&
'   minefield(1f) - [M_draw] minefield game                                      ',&
'   (LICENSE:PD)                                                                 ',&
'                                                                                ',&
'SYNOPSIS                                                                        ',&
'   minefield [[ -r rows] [ -c columns]] |[ --help --version ]                   ',&
'                                                                                ',&
'DESCRIPTION                                                                     ',&
'   minefield(1f) is a minefield sweeper game. The game tests many M_DRAW(3fm)   ',&
'   functions.                                                                   ',&
'                                                                                ',&
'   The object of the game is to mark all the mines with mouse 2 and expose      ',&
'   all the squares that are not mines with mouse 1 as quickly as possible.      ',&
'   The number in a square indicates how many mines are adjacent to it.          ',&
'                                                                                ',&
'   The game can go up to 99 rows or columns.                                    ',&
'                                                                                ',&
'   MOUSE 1   Use mouse 1 to expose a square                                     ',&
'   MOUSE 2   Use mouse 2 to mark a mine                                         ',&
'   MOUSE 3   Use mouse 3 to take a mine marker back off                         ',&
'                                                                                ',&
'   To cheat use mouse 2 and 3 together to expose all squares that have          ',&
'   no mines adjacent to them. Use mouse 1, 2 and 3 to expose unexposed          ',&
'   squares with no risk of a bomb going off, use mouse 1 and 3 to solve         ',&
'   all unexposed squares.                                                       ',&
'                                                                                ',&
'OPTIONS                                                                         ',&
'   -r          number of rows                                                   ',&
'   -c          number of columns                                                ',&
'   -switch     switch mouse 2 and mouse 3 buttons; typically for                ',&
'               two-button mouses                                                ',&
'   -x          width of window in rasters. Defaults to 800                      ',&
'   -y          height of window in rasters. Defaults to 500                     ',&
'   --help      display help text and exit                                       ',&
'   --version   display version text and exit                                    ',&
'AUTHOR                                                                          ',&
'   John S. Urban                                                                ',&
'LICENSE                                                                         ',&
'   Public Domain                                                                ',&
'']
   WRITE(*,'(a)')(trim(help_text(i)),i=1,size(help_text))
   stop ! if --help was specified, stop
endif
end subroutine help_usage
!>
!!##NAME
!!    minefield(1f) - [M_draw] minefield game
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    minefield [[ -r rows] [ -c columns]] |[ --help --version ]
!!
!!##DESCRIPTION
!!    minefield(1f) is a minefield sweeper game. The game tests many M_DRAW(3fm)
!!    functions.
!!
!!    The object of the game is to mark all the mines with mouse 2 and expose
!!    all the squares that are not mines with mouse 1 as quickly as possible.
!!    The number in a square indicates how many mines are adjacent to it.
!!
!!    The game can go up to 99 rows or columns.
!!
!!    MOUSE 1   Use mouse 1 to expose a square
!!    MOUSE 2   Use mouse 2 to mark a mine
!!    MOUSE 3   Use mouse 3 to take a mine marker back off
!!
!!    To cheat use mouse 2 and 3 together to expose all squares that have
!!    no mines adjacent to them. Use mouse 1, 2 and 3 to expose unexposed
!!    squares with no risk of a bomb going off, use mouse 1 and 3 to solve
!!    all unexposed squares.
!!
!!##OPTIONS
!!    -r          number of rows
!!    -c          number of columns
!!    -switch     switch mouse 2 and mouse 3 buttons; typically for
!!                two-button mouses
!!    -x          width of window in rasters. Defaults to 800
!!    -y          height of window in rasters. Defaults to 500
!!    --help      display help text and exit
!!    --version   display version text and exit
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
'@(#)PROGRAM:        minefield(1)>',&
'@(#)DESCRIPTION:    minefield game>',&
'@(#)VERSION:        4.0, 20180616>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)COMPILED:       2024-06-29 21:59:04 UTC-240>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if --version was specified, stop
endif
end subroutine help_version
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine minefield(irows0,icols0,switch) ! draw a minesweep program
! Created: 19971231
use M_draw
! ident_2="@(#) draw a minesweep game that quits on 'q'"
!      storage
!        -1 to -9 for unexposed and 0 to 8 adjacent bombs
!        -10 for unexposed and a bomb
!        0 to 8 for exposed good values
!        9 for an exposed bomb
!        10 to 18 for marked as bomb and not one

!     prototype showing functions used in calculators, toggle buttons, and menus
integer storage(0:100,0:100)  ! storage for the minesweep game
integer,intent(in) :: irows0
integer,intent(in) :: icols0
integer            :: irows
integer            :: icols
integer            :: istart
integer            :: icount
integer            :: isecs
integer            :: icovered
integer            :: ibadbomb
integer            :: letter
integer            :: i,j
intrinsic min,max
logical switch
logical iwon

iwon=.false.

   istart=ordinal_seconds()
   irows=min(max(1,irows0),99)                                  ! make sure OK values for rows and columns
   icols=min(max(1,icols0),99)

   call pushattributes()                                        ! save graphics environment
   call pushmatrix()
   call pushviewport()
   call circleprecision(100)

   ! lay out a window such that boxes are 10x10, with room above for a 20x100 bomb counter
   ! The window value 0,0 is in the middle of the bomb field
   !
   ! set window and viewport so boxes are 10 units on a side
   call page(real(-icols)*5.0,real(icols)*5.0,real(-irows)*5.0,real(irows)*5.0+20.0)
!----------------------------------------------------------------------------------------------------------------------------------!
   ! count bombs
   call bombs(irows,icols,storage,icount) ! fill storage with -1 to -9 for 0 to 8 adjacent bombs, and -10 for a bomb
!----------------------------------------------------------------------------------------------------------------------------------!
   call color(2)                                                ! background color
   call clear()                                                 ! clear display area to background color
   if(irows0.lt.13.and.icols0.lt.13)then
      call color(-4)                                            ! set line thickness
   elseif(irows0.lt.24.and.icols0.lt.24)then
      call color(-3)                                            ! set line thickness
   elseif(irows0.lt.40.and.icols0.lt.40)then
      call color(-2)                                            ! set line thickness
   else
      call color(-1)                                            ! set line thickness
   endif
   call font('futura.m')                                        ! select text font for numbers

   INFINITE : do
      isecs=ordinal_seconds()-istart
      call draw_board(icols,irows,storage,.false.,icount,iwon,isecs)  ! draw the gameboard
      ! if it looks like you won check to see
      if(icount.eq.0)then                                         ! see if actually won or if badly marked bombs
         icovered=0
         ibadbomb=0
         do i=1,irows
            do j=1,icols
               if( storage(i,j).lt.0)then
                  icovered=icovered+1
               elseif(storage(i,j).gt.9)then
                  ibadbomb=ibadbomb+1
               endif
            enddo
         enddo
         if(icovered+ibadbomb.eq.0)then                           ! YOU WON
            iwon=.true.
            exit INFINITE
         endif
      endif
      call check_value(icols,irows,storage,letter,icount,switch)  ! change values to 0 to 8 (9 for bomb marker)
      if(letter.eq.113)exit INFINITE                              ! if not the letter q, continue
   enddo INFINITE
   call draw_board(icols,irows,storage,.true.,icount,iwon,isecs)  ! display all values while quitting
   call popattributes()                                           ! restore graphics environment
   call popmatrix()
   call popviewport()
end subroutine minefield
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine draw_board(icols,irows,storage,all,icount,iwon,isecs)     ! draw the gameboard
use M_draw
character line*80
integer,intent(in) :: icols
integer,intent(in) :: irows
integer storage(0:100,0:100)
logical :: all
integer :: icount
logical :: iwon
integer :: idum

real    :: xmin
real    :: xmax
real    :: ymin
real    :: ymax
integer :: i, j
real    :: tsize
integer :: isecs
integer :: icolor

   idum=backbuffer()
   call color(6)             ! rectangle color
   call clear()
   call centertext(.true.)   ! all text should be centered

   ! draw count of markers
   xmin=real(-icols)*5.0
   xmax=xmin+(real(icols)*5.0)
   ymin=real(irows)*5.0
   ymax=ymin+20.0
   write(line,'(i5.5)')abs(icount)
   do i=1,2
      call color(0)
      call polyfill(.true.)
      call rect(xmin,ymin,xmax,ymax)
      call polyfill(.false.)
      call color(7)
      if(xmax-xmin.lt.60)then
         tsize=(xmax-xmin)/6.0
      else
         tsize=16.0
      endif
      call textsize(tsize,tsize)
      call move2((xmin+xmax)/2.0,(ymin+ymax)/2.0)
      call drawstr(line)
      if(icount.lt.0)then  ! must have marked some as mines that are not
         call move2(xmin,ymin)
         call draw2(xmax,ymax)
         call move2(xmin,ymax)
         call draw2(xmax,ymin)
      endif
      xmin=xmax
      write(line,'(i5.5)')isecs
      xmax=xmin+(real(icols)*5.0)
   enddo

   call color(6)  ! rectangle color
   call textsize(5.5,5.5)
   do i=1,irows
      do j=1,icols
         icolor=storage(i,j)
         xmin=real(j-1)*10.0-real(icols)*5.0
         xmax=xmin+10.0
         ymax=real(irows)*5.0-real(i-1)*10.0
         ymin=ymax-10.0
         if(icolor.ge.0.and.icolor.le.8)then   ! if an exposed piece
            write(line,'(i3)')icolor
            call color(0)
            call polyfill(.true.)
            call rect(xmin,ymin,xmax,ymax)
            call color(7)
            call polyfill(.false.)
            call rect(xmin,ymin,xmax,ymax)
            call color(icolor)
            write(line,'(i1.1)')icolor
            call move2(xmin+5.0,ymin+5.0)
            call drawstr(line)
         elseif(icolor.eq.9)then               ! exposed bomb
            if(iwon)then
               call smiley(xmin,xmax,ymin,ymax)
            else
               call color(1)
               call polyfill(.true.)
               call rect(xmin,ymin,xmax,ymax)
               call polyfill(.false.)
               call color(7)
               call rect(xmin,ymin,xmax,ymax)
            endif
            ! unexposed pieces and mistakenly marked bombs
         elseif(all)then   ! end of game, expose all pieces
            call color(7)
            call polyfill(.true.)
            call rect(xmin,ymin,xmax,ymax)
            call color(0)
            call circle((xmax+xmin)/2.0,(ymax+ymin)/2.0,5.0)   ! circle unexposed pieces
            call polyfill(.false.)
            call color(7)
            call rect(xmin,ymin,xmax,ymax)
            if(icolor.eq.-10)then         ! unexposed bomb. Put green square out over black square
               icolor=2
               call color(icolor)
               call polyfill(.true.)
               call rect(xmin,ymin,xmax,ymax)
               call polyfill(.false.)
            elseif(icolor.gt.9)then   ! marked as a bomb but not one
               call color(1)
               call circle((xmax+xmin)/2.0,(ymax+ymin)/2.0,5.0)
               icolor=icolor-10
            elseif(icolor.lt.0)then       ! unexposed
               icolor=-icolor-1
            endif
            call color(icolor)
            write(line,'(i1.1)')icolor
            call move2(xmin+5.0,ymin+5.0)
            call drawstr(line)
         elseif(icolor.gt.9)then               ! mistaken bomb marker
            call color(1)
            call polyfill(.true.)
            call rect(xmin,ymin,xmax,ymax)
            call polyfill(.false.)
            call color(7)
            call rect(xmin,ymin,xmax,ymax)
         else                                  ! unexposed piece
            call color(7)
            call rect(xmin,ymin,xmax,ymax)
         endif
      enddo
   enddo
   call swapbuffers()
   call vflush()
end subroutine draw_board
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine check_value(ic,ir,st,letter,icount,switch)
!   -1 to -9  for unexposed and 0 to 8 adjacent bombs
!   -10       for unexposed and a bomb
!   0 to 8    for exposed good values
!   9         for an exposed bomb
!   10 to 18  for marked as bomb and not one
use M_draw
use M_time, only : system_sleep
real    :: xx, yy  ! locator location in window coordinates
integer :: istat   ! istat is mouse combination as binary values for three key mouse
integer :: letter  ! letter is ordinal of a character key
integer,intent(in) :: ic
integer,intent(in) :: ir
integer st(0:100,0:100)
integer :: icount
logical :: switch

integer :: EXPOSED
integer :: mouse1, mouse2, mouse3
real    :: xdelta, ydelta
integer :: ixdelta, iydelta
integer :: ii
integer :: i, j

   if(switch)then
      mouse1=1
      mouse2=4
      mouse3=2
   else
      mouse1=1
      mouse2=2
      mouse3=4
   endif
   INFINITE : do
      letter=checkkey()          ! see if a character was pressed in graphic window, get ordinal of letter last pressed
      istat=locator(xx,yy)                                     ! get location and mouse key pressed
      call system_sleep(0.04)                                  ! pause
      if(letter.eq.113)goto 999                                ! quit if letter q
      if(istat.eq.0)cycle INFINITE                             ! wait until a mouse key is clicked
      xdelta=xx-real(ic*(-5))                                  ! distance from left edge
      ydelta=yy-real(ir*(-5))                                  ! distance from top edge
      ! row and column number to point into into st array
      ixdelta=int((xdelta+5.0)/10.0+0.5)
      iydelta=int((ydelta+5.0)/10.0+0.5)
      ! if pick location is a valid box location
      if(iydelta.ge.1.and.iydelta.le.ir .and.ixdelta.ge.1.and.ixdelta.le.ic) then
         ii=st(abs(iydelta-ir)+1,ixdelta)                      ! value in selected box
         ! if ii is less than 0 figure new values
         if(ii.lt.0)then
            EXPOSED=-(ii)-1
         else
            EXPOSED=ii
         endif
         !MOUSE 1
         if(ii.lt.0.and.istat.eq.mouse1)then                   ! this one has not been exposed
            !EXPOSING UNEXPOSED (M1)
            st(abs(iydelta-ir)+1,ixdelta)=EXPOSED              ! expose the piece marked
            ! EXPOSED BOMB BY MISTAKE
            if(EXPOSED.eq.9)then                               ! exposed a bomb with mouse 1
               call move2(xx,yy)                               ! get current position to point selected for boom routine
               call draw2(xx,yy)
               call boom(ic,ir)
               call system_sleep(0.1)                          ! pause for a second for effect
               letter=113
               goto 999
               ! EXPOSED A ZERO; BE NICE AND EXPOSE EVERYTHING OBVIOUS (ADJACENT TO A ZERO)
            elseif(EXPOSED.eq.0)then
               call find_zeros(ir,ic,st)                       ! if exposed a zero, clear around it pseudo-recursively
            endif
            !MOUSE 2
         elseif(ii.lt.0.and.istat.eq.mouse2)then
            !MARKING UNEXPOSED PIECE AS A BOMB (M2)
            icount=icount-1                                    ! mouse two is for marking bombs
            if(EXPOSED.eq.9)then
               st(abs(iydelta-ir)+1,ixdelta)=EXPOSED           ! correctly found a bomb
            else
               st(abs(iydelta-ir)+1,ixdelta)=EXPOSED+10        ! something marked as a bomb that is not; add 10
                                                               ! (so 0 is 10, 1 is 11, 8 is 18)
            endif
            !MOUSE 3
         elseif(istat.eq.mouse3.and.ii.gt.9)then                    ! something already marked as a bomb that is not
            st(abs(iydelta-ir)+1,ixdelta)=-(ii-10)-1           ! put it back to an unmarked value and expose it (BACK DOOR)
            icount=icount+1
         elseif(istat.eq.mouse3.and.ii.eq.9)then               ! something already marked as a bomb that is
            st(abs(iydelta-ir)+1,ixdelta)=-10                  ! put it back to an unmarked value
            icount=icount+1
         elseif(istat.eq.mouse3.and.ii.ge.0)then               ! something exposed already. Why cover it back up?
            ! use other keys for testing
         elseif(ii.lt.0.and.ii.ge.-9.and.istat.eq.(mouse1+mouse2+mouse3))then       ! CHEAT AND EXPOSE NON-BOMBS
            st(abs(iydelta-ir)+1,ixdelta)=EXPOSED
         elseif(istat.eq.(mouse3+mouse2))then                  ! CHEAT AND EXPOSE ALL ZERO REGIONS
            do i=1,99
               do j=1,99
                  if( st(i,j).eq.-1)then
                     st(i,j)=0
                  endif
               enddo
            enddo
            call find_zeros(ir,ic,st)                       ! if exposed a zero, clear around it pseudo-recursively
         elseif(istat.eq.mouse3+mouse1)then                    ! CHEAT AND EXPOSE ALL UNEXPOSED REGIONS
            do i=1,99
               do j=1,99
                  if( st(i,j).lt.0)then
                     if(st(i,j).eq.-10) icount=icount-1
                     st(i,j)=-st(i,j)-1
                  endif
               enddo
            enddo
            call find_zeros(ir,ic,st)                          ! if exposed a zero, clear around it pseudo-recursively
         else
            !write(*,*)'took no action ',istat,ii
         endif
      else
         cycle INFINITE                                        ! picked a location out of boxes
      endif
      exit INFINITE
   enddo INFINITE

999   continue
end subroutine check_value
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine find_zeros(irows,icols,s)
integer,intent(in) :: irows
integer,intent(in) :: icols
! brute force approach
! search for 0 boxes and pop all adjacent boxes
! loop until no changes made
integer :: s(0:100,0:100)
integer :: ilimit
integer :: ichange
integer :: i1,i2,i3,i4

   ilimit=0
   do
      ichange=0
      do i1=1,irows           ! figure out value for each box
         do i2=1,icols
            if(s(i1,i2).eq.0)then
               do i3=-1,1
                  do i4=-1,1
                     if(i3.eq.0.and.i4.eq.0)then
                        ! on the center
                     elseif(s(i1+i3,i2+i4).lt.0)then
                        s(i1+i3,i2+i4)=abs(s(i1+i3,i2+i4))-1
                        ichange=ichange+1
                     endif
                  enddo
               enddo
            endif
         enddo
      enddo
      ilimit=ilimit+1
      if(ilimit.ge.irows*icols)then  ! do not permit an infinite loop
         !write(*,*)'too many times in find_zeros'
         exit
      endif
      if(ichange.eq.0)exit
   enddo
end subroutine find_zeros
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine bombs(irows,icols,storage,icount)
use M_draw
use M_random, only : init_random_seed
! fill storage with -1 to -9 for 0 to 8 adjacent bombs, and -10 for a bomb
integer storage(0:100,0:100)
logical switch(0:100,0:100)        ! array to fill with random bombs
integer,intent(in) :: irows
integer,intent(in) :: icols
integer            :: icount
integer            :: i,j
integer            :: irand
integer            :: isum
real               :: fval

   call init_random_seed(ordinal_seconds())    ! set the seed for random_number

   switch=.true. ! fill storage with 9 and bomb map with no bombs

   icount=0
   do i=1,irows           ! place bombs
      do j=1,icols
         ! CHANGING THE VALUE 9 BELOW CHANGES THE NUMBER OF BOMBS
         call random_number(harvest=fval)
         irand=int(fval*9.0) ! returns a REAL number from 0.0 to 1.0;
         if(irand.le.1)then
            switch(i,j)=.false.
            icount=icount+1
         endif
      enddo
   enddo

   do i=1,irows           ! figure out value for each box
      do j=1,icols
         if(switch(i,j).neqv..false.)then
            isum=0
            if( .not.switch(i-1,j-1) )isum=isum+1
            if( .not.switch(i+1,j+1) )isum=isum+1
            if( .not.switch(i-1,j+1) )isum=isum+1
            if( .not.switch(i+1,j-1) )isum=isum+1
            if( .not.switch(i-1,j+0) )isum=isum+1
            if( .not.switch(i+1,j+0) )isum=isum+1
            if( .not.switch(i-0,j+1) )isum=isum+1
            if( .not.switch(i-0,j-1) )isum=isum+1
            storage(i,j)=-(isum+1)  ! values of -1 to -9 for 0 to 8 adjacent bombs
         else
            storage(i,j)=-10  ! bomb value
         endif
      enddo
   enddo
end subroutine bombs
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine boom(icols,irows)
use M_draw
integer,intent(in) :: icols
integer,intent(in) :: irows
real               :: x0,y0
real               :: rad
integer            :: istart, iend, istep
integer            :: i,j,k,m
real               :: x,y
real               :: x1,y1
real               :: radius
integer,parameter  :: it=380
real               :: rand

   call polyfill(.true.)
   call frontbuffer()
   call getgp2(x0,y0)
   rad=real(max(icols,irows)*10*2)
   istart=200
   iend=1
   istep=-1
   x1=x0
   y1=y0
   do j=1,2
      do k=istart,iend,istep
         do i=1,3,2
            call color(i)
            radius=rad/real(k)
            call makepoly()
            do m=1,it
               call random_number(harvest=rand)
               x=cos(2*3.141592*real(m)/real(it))*radius *(rand+0.1)+x0
               y=sin(2*3.141592*real(m)/real(it))*radius *(rand+0.1)+y0
               if(m.eq.1)then
                  x1=x
                  y1=y
                  call move2(x,y)
               else
                  call draw2(x,y)
               endif
            enddo
            call draw2(x1,y1)
            call closepoly()
            call vflush()
         enddo
      enddo
      ! implode
      iend=20
      istart=1
      istep=1
   enddo
   call polyfill(.false.)
end subroutine boom
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine smiley(xmin,xmax,ymin,ymax)
! draw a smiley face in the box defined by xmin,xmax,ymin,ymax
use M_draw
use M_drawplus, only : uconic
real,intent(in) :: xmin, xmax
real,intent(in) :: ymin, ymax
real            :: x,y
real            :: radius
real            :: xc,yc,long,tall,a,b,e,ae,pf,ph,xq1,yq1,h
integer         :: i,j
real,parameter  :: xx(*)=[ &
-3.249,   -3.225,       -3.176,   -2.692,  -2.039,  -0.9985,  +0.5985,  +1.445, &
+2.438,   +2.825,       +2.921,   +3.551,  +2.534,  +2.849,   +2.317,   +1.566, &
+0.7437,  +0.4197E-01,  -0.9743,  -1.942,  -2.547,  -2.886,   -3.152,   -3.152, &
-3.176,   -2.838,       -3.515,   -3.249]
real,parameter  :: yy(*)=[ &
-0.6698,  -0.8426,  -1.448,   -2.269,   -2.895,   -3.457,   -3.414,  -2.873, &
-1.901,   -1.123,   -0.7778,  -0.5185,  -0.3673,  -0.7994,  -1.448,  -2.204, &
-2.830,   -3.003,   -2.938,   -2.528,   -1.923,   -1.469,   -1.102,  -0.8210, &
-0.7346,  -0.4105,  -0.2377,  -0.6698]

   X=(xmax+xmin)/2.0
   Y=(ymax+ymin)/2.0
   radius=abs(xmax-xmin)/2.0
   call color(1)                                     ! fill the rectangular area in red
   call polyfill(.true.)
   call rect(xmin,ymin,xmax,ymax)
   call polyfill(.false.)
   call color(7)                                     ! draw an outline around the rectangle
   call rect(xmin,ymin,xmax,ymax)

   call polyfill(.true.)
   call color(3)                                     ! fill the yellow circular face
   call circle(X,Y,radius)
   call polyfill(.false.)
   call color(0)
   call circle(X,Y,radius)                           ! outline the circular face
   XC=3.0/5.0*radius                                 ! draw an elliptical eye
   YC=1.0/5.0*radius
   LONG=3.0/5.0*radius
   TALL=2.0/5.0*radius
   B=TALL/2.0
   A=LONG/2.0
   E=SQRT(1-(B/A)**2)
   AE=SQRT(A**2-B**2)
   PF=A-AE
   PH=PF/E
   XQ1=XC-AE
   YQ1=YC
   H=PH+PF
   call polyfill(.true.)
   !call makepoly()
   !CALL UCONIC (X,Y,P,E,THETA1,THETA2,ORIENTATION)
   call UCONIC(X+XQ1,Y+YQ1,H,E,0.0,360.0,90.0)
   !call closepoly()
   call circle(X+XQ1,Y+1.5*YQ1,H/2.0)                 ! draw the pupil
   XC=(-0.5)/5.0*radius                               ! draw the other eye
   YC=+1.0/5.0*radius
   XQ1=XC-AE
   YQ1=YC
   !call  makepoly()
   call UCONIC(X+XQ1,Y+YQ1,H,E,0.0,360.0,90.0)
   !call closepoly()
   call circle(X+XQ1,Y+1.5*YQ1,H/2.0)                ! fill the other pupil
   do i=1,2                                          ! fill and outline the smile
      call makepoly()
      call move2(x+xx(1)/5.0*radius,Y+yy(1)/5.0*radius)
      do j=2,size(xx)
         call draw2(x+xx(j)/5.0*radius,y+yy(j)/5.0*radius)
      enddo
      call closepoly()
      call polyfill(.false.)
   enddo
end subroutine smiley
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
end program sweepit
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
