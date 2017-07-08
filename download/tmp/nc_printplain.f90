!-----------------------------------------------------------------------------------------------------------------------------------
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>-
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine nc_printplain(win,filename) ! @(#) plain text print reading window from ncurses
   use M_ncurses
   implicit none
!-----------------------------------------------------------------------------------------------------------------------------------
   type(C_PTR),intent(in)       :: win          ! window to print
   character(len=*),intent(in)  :: filename     ! filename to print to
!-----------------------------------------------------------------------------------------------------------------------------------
   integer                      :: my,mx        ! size of the specified window
   integer(C_LONG)              :: cell         ! cell information (attributes, color pair, and character)
   character(len=1)             :: let          ! the character in the cell
   integer(C_LONG)              :: attr         ! attributes of the cell
   integer                      :: ios          ! status from open(3f)
   character(len=256)           :: msg          ! message from open(3f)
   integer,save                 :: iout=11      ! unit to open(3f) for writing
   integer                      :: i,j          ! loop counters
!-----------------------------------------------------------------------------------------------------------------------------------
   OPEN(UNIT=iout,FILE=trim(filename),ACTION='write',ACCESS='stream',FORM='unformatted',IOSTAT=ios,IOMSG=msg,STATUS='unknown')
   if(ios.ne.0)then
      call nc_errmessage("failed to open print file "//trim(filename)//':'//trim(msg))
      return
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   call getmaxyx(win,my,mx)                     ! window size as defined (all of it, even if subsection being displayed)
!-----------------------------------------------------------------------------------------------------------------------------------
   do i=0,my-1                                  ! scan window cell by cell
      do j=0,mx-1
         cell=mvwinch(win,i,j)                  ! retrieve cell value
         let=char(iand(cell,A_CHARTEXT))        ! get letter from cell using bit-mask
         attr=iand(cell,A_ATTRIBUTES)           ! the attributes of the cell
         if(iand(attr,A_ALTCHARSET).eq.A_ALTCHARSET)then  ! if an alternate character pick a standard character
            select case(let)
            case('l');let='#' ! ACS_ULCORNER  upper left corner
            case('m');let='#' ! ACS_LLCORNER  lower left corner
            case('k');let='#' ! ACS_URCORNER  upper right corner
            case('j');let='#' ! ACS_LRCORNER  lower right corner
            case('t');let='+' ! ACS_LTEE      tee pointing right
            case('u');let='+' ! ACS_RTEE      tee pointing left
            case('v');let='+' ! ACS_BTEE      tee pointing up
            case('w');let='+' ! ACS_TTEE      tee pointing down
            case('q');let='-' ! ACS_HLINE     horizontal line
            case('x');let='|' ! ACS_VLINE     vertical line
            case('n');let='+' ! ACS_PLUS      large plus or crossover
            case('`');let='o' ! ACS_DIAMOND
               ! for the fixedform(1) program change diamond if "selected" so can tell the difference in output
               if(iand(attr,A_STANDOUT).eq.A_STANDOUT)then  ! using reverse or standout on a diamond to select it as a menu item
                  let='X'
               endif
               if(iand(attr,A_REVERSE).eq.A_REVERSE)then
                  let='X'
               endif
            end select
         endif
         write(iout)let          !! use stream position or buffer lines and print to remove trailing white space
      enddo
      write(iout)NEW_LINE('a')   ! write system-appropriate end-of-line
   enddo
end subroutine nc_printplain
