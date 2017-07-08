!-----------------------------------------------------------------------------------------------------------------------------------
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>-
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine nc_printunicode(win,filename)
! @(#) plain text print reading window from ncurses
!! write as HTML or ANSI escape codes per ansi2html(1) script
!! maybe look in original page data instead like loaddata(3f)
!! what about alternate characters like box characters ?
!! could newterm(3c) be used to set to a file instead of a terminal to capture escape sequences and ensure they are VT102 commands?
   use M_ncurses
   implicit none
!-----------------------------------------------------------------------------------------------------------------------------------
   type(C_PTR),intent(in)       :: win          ! window to print
   character(len=*),intent(in)  :: filename     ! filename to print to
!-----------------------------------------------------------------------------------------------------------------------------------
   integer                      :: my,mx        ! size of the specified window
   integer(C_LONG)              :: ch           ! long character in cell (attributes, color pair, and character)
   character(len=1)             :: let          ! the character in the cell
   integer                      :: ilet         ! decimal value of character in the cell
   integer(C_LONG)              :: attr         ! attributes of the cell
   integer                      :: ios          ! status from open(3f)
   character(len=256)           :: msg          ! message from open(3f)
   integer,save                 :: iout=11      ! unit to open(3f) for writing
   integer                      :: i,j          ! loop counters
   integer                      :: ierr
   integer(C_SHORT)             :: pair         ! color pair used by the cell
   integer(C_SHORT)             :: rf,gf,bf     ! color components of cell foreground
   integer(C_SHORT)             :: rb,gb,bb     ! color components of cell background
   integer(C_SHORT)             :: fg,bg        ! foreground and background color numbers of cell
   logical                      :: alt, bold, reverse
!-----------------------------------------------------------------------------------------------------------------------------------
   OPEN(UNIT=iout,FILE=trim(filename),ACTION='write',ACCESS='stream',FORM='unformatted',IOSTAT=ios,IOMSG=msg,STATUS='unknown')
   if(ios.ne.0)then
      call nc_errmessage("failed to open print file "//trim(filename)//':'//trim(msg))
      return
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   call getmaxyx(win,my,mx)                     ! size window size as defined (all of it, even if subsection being displayed)
!-----------------------------------------------------------------------------------------------------------------------------------
   do i=0,my-1
      do j=0,mx-1
         ch=mvwinch(win,i,j)                    ! retrieve cell value
         ilet=iand(ch,A_CHARTEXT)               ! get decimal letter from cell using bit-mask
         let=char(ilet)                         ! get character from decimal
         pair=int(PAIR_NUMBER(ch),C_INT)        ! the color pair used to draw the cell
         ierr=pair_content(pair,fg,bg)          ! find out how a given color-pair is currently defined
         ierr=color_content(fg,rf,gf,bf)        ! extract red, green, and blue components in an initialized color
         ierr=color_content(bg,rb,gb,bb)        ! extract red, green, and blue components in an initialized color
         attr=iand(ch,A_ATTRIBUTES)             !! the attributes of the cell
         alt=.false.
         bold=.false.
         reverse=.false.
         if(iand(attr,A_BLINK).eq. A_BLINK)then           !  blink
         endif
         if(iand(attr,A_INVIS).eq. A_INVIS)then           !  invisible
         endif
         if(iand(attr,A_NORMAL).eq. A_NORMAL)then
         endif
         if(iand(attr,A_PROTECT).eq. A_PROTECT)then
         endif
         if(iand(attr,A_UNDERLINE).eq. A_UNDERLINE)then
         endif
         if(iand(attr,A_DIM).eq.A_DIM)then
         endif
         if(iand(attr,A_HORIZONTAL).eq.A_HORIZONTAL)then
         endif
         if(iand(attr,A_LEFT).eq.A_LEFT)then
         endif
         if(iand(attr,A_LOW).eq.A_LOW)then
         endif
         if(iand(attr,A_RIGHT).eq.A_RIGHT)then
         endif
         if(iand(attr,A_STANDOUT).eq.A_STANDOUT)then
         endif
         if(iand(attr,A_TOP).eq.A_TOP)then
         endif
         if(iand(attr,A_VERTICAL).eq.A_VERTICAL)then
         endif
         if(iand(attr,A_ALTCHARSET).eq.A_ALTCHARSET)then
            alt=.true.
            select case(let)
            case('l') ! ACS_ULCORNER  upper left corner
               let='#'
            case('m') ! ACS_LLCORNER  lower left corner
               let='#'
            case('k') ! ACS_URCORNER  upper right corner
               let='#'
            case('j') ! ACS_LRCORNER  lower right corner
               let='#'
            case('t') ! ACS_LTEE      tee pointing right
               let='+'
            case('u') ! ACS_RTEE      tee pointing left
               let='+'
            case('v') ! ACS_BTEE      tee pointing up
               let='+'
            case('w') ! ACS_TTEE      tee pointing down
               let='+'
            case('q') ! ACS_HLINE     horizontal line
               let='-'
            case('x') ! ACS_VLINE     vertical line
               let='|'
            case('n') ! ACS_PLUS      large plus or crossover
               let='+'
            case('`') ! ACS_DIAMOND
               let='o'
            end select
         endif
         if(iand(attr,A_BOLD).eq.A_BOLD)then
            bold=.true.
         endif
         if(iand(attr,A_REVERSE).eq.A_REVERSE)then
            reverse=.true.
         endif
         write(iout)let                         !! use stream position or buffer lines and print to remove trailing white space
      enddo
      write(iout)NEW_LINE('a')
   enddo
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine nc_printunicode
