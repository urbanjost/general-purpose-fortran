!-----------------------------------------------------------------------------------------------------------------------------------
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>-
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine nc_printhtml(win,filename) ! @(#) print ncurses(3c) window as HTML
   use M_ncurses
   implicit none
!-----------------------------------------------------------------------------------------------------------------------------------
   type(C_PTR),intent(in)       :: win          ! window to print
   character(len=*),intent(in)  :: filename     ! filename to print to
!-----------------------------------------------------------------------------------------------------------------------------------
   integer                      :: my,mx        ! size of the specified window
   integer(C_LONG)              :: cell         ! long character in cell (attributes, color pair, and character)
   character(len=1)             :: let          ! the character in the cell
   character(len=128)           :: lets         ! the characters that need printed in HTML to represent the cell
   integer                      :: ilet         ! decimal value of character in the cell
   integer(C_LONG)              :: attr         ! attributes of the cell
   integer                      :: ios          ! status from open(3f)
   character(len=256)           :: msg          ! message from open(3f)
   integer                      :: iout         ! unit to open(3f) for writing
   integer                      :: i,j          ! loop counters
   integer                      :: ierr
   integer(C_SHORT)             :: pair         ! color pair used by the cell
   integer(C_SHORT)             :: rf,gf,bf     ! color components of cell foreground
   integer(C_SHORT)             :: rb,gb,bb     ! color components of cell background
   integer(C_SHORT)             :: fg,bg        ! foreground and background color numbers of cell
   logical                      :: span=.false. !! started a span region or not. Might actually use it later
   character(len=100)           :: colordef
   character(len=100)           :: class
   character(len=100)           :: lastclass
!-----------------------------------------------------------------------------------------------------------------------------------
   if(filename.ne.'')then
      iout=11
      OPEN(UNIT=iout,FILE=trim(filename),ACTION='write',ACCESS='stream',FORM='unformatted',IOSTAT=ios,IOMSG=msg,STATUS='unknown')
      if(ios.ne.0)then
         call nc_errmessage("failed to open print file "//trim(filename)//':'//trim(msg))
         return
      endif
   else       !! Actually an error for now:: So far, no way to write a stream to stdout using Fortran
      iout=6
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
call ln('<html>')
call ln('<meta http-equiv="Content-Type" content="text/html; charset=utf-8"/>')
call ln('<head>')
call ln('<title></title>')
!-----------------------------------------------------------------------------------------------------------------------------------
call ln('<style type="text/css">')
call ln('.BO { font-weight:     bold;         }')
call ln('.U  { text-decoration: underline;    }')
call ln('.S  { text-decoration: line-through; }')
call ln('.BL { text-decoration: blink;        }')
call ln('.I  { font-style:      italic;       }')
call ln('.N  { font-style:      normal;       }')
!-----------------------------------------------------------------------------------------------------------------------------------
call ln('body{ color: #FFFFFF; background-color: #000000; }')
! write out defined color pairs
! CSS does not seem to support the idea of reverse video, so make a "PAIRnn" and a "RPAIRnn" for each pair
!! might be worth it to scan window and only print color definitions that are used
! always define foreground and background

call ln('.PAIR0{ color: #FFFFFF; background-color: #000000; }')
call ln('.RPAIR0{ color: #000000; background-color: #FFFFFF; }')

call getcolor(COLORS,COLOR_PAIRS) !! extension to get these set

do pair=0_C_SHORT,COLOR_PAIRS
   !! even when color not defined did not get an error value back
   ierr=pair_content(pair,fg,bg)            ! find out how a given color-pair is currently defined
   ierr=color_content(fg,rf,gf,bf)          ! extract red, green, and blue components in an initialized color
   ierr=color_content(bg,rb,gb,bb)          ! extract red, green, and blue components in an initialized color
   if(fg+bg+rf+gf+bf+bg+rb+gb+bb.ne.0)then     !! assume this means not defined
      rf=rf*255/1000   ! the colors are in the range 0 to 1000 inclusive, want a two-digit hex value
      gf=gf*255/1000
      bf=bf*255/1000
      rb=rb*255/1000
      gb=gb*255/1000
      bb=bb*255/1000
      write(colordef,'(".PAIR",i0,"{ color: #",z2.2,z2.2,z2.2,"; background-color: #",z2.2,z2.2,z2.2,"; }")')pair,rf,gf,bf,rb,gb,bb
      write(iout)colordef
      write(iout)new_line('a')
      write(colordef,'(".RPAIR",i0,"{ color: #",z2.2,z2.2,z2.2,"; background-color: #",z2.2,z2.2,z2.2,"; }")')pair,rb,gb,bb,rf,gf,bf
      write(iout)colordef
      write(iout)new_line('a')
   endif
enddo
call ln('</style>')
!-----------------------------------------------------------------------------------------------------------------------------------
call ln('</head>')
call ln('<body>')
call ln('<pre>')
!-----------------------------------------------------------------------------------------------------------------------------------
   call getmaxyx(win,my,mx)                     ! size window size as defined (all of it, even if subsection being displayed)
!-----------------------------------------------------------------------------------------------------------------------------------
   lastclass=''
   do i=0,my-1
      do j=0,mx-1
         class=''
         cell=mvwinch(win,i,j)                    ! retrieve cell value
         ilet=iand(cell,A_CHARTEXT)               ! get decimal letter from cell using bit-mask
         let=char(ilet)                           ! get character from decimal
         lets=let                                 ! what to print for the character
         pair=int(PAIR_NUMBER(cell),C_INT)        ! the color pair used to draw the cell
         attr=iand(cell,A_ATTRIBUTES)             !! the attributes of the cell
         if(iand(attr,A_BLINK).eq. A_BLINK)          class=trim(class)//' BL'
         if(iand(attr,A_INVIS).eq. A_INVIS)          class=trim(class)//' IN'
         if(iand(attr,A_NORMAL).eq. A_NORMAL)        class=class !class=' '
         if(iand(attr,A_PROTECT).eq. A_PROTECT)      class=trim(class)//' '
         if(iand(attr,A_UNDERLINE).eq. A_UNDERLINE)  class=trim(class)//' U'
         if(iand(attr,A_DIM).eq.A_DIM)               class=trim(class)//' '
         if(iand(attr,A_HORIZONTAL).eq.A_HORIZONTAL) class=trim(class)//' '
         if(iand(attr,A_LEFT).eq.A_LEFT)             class=trim(class)//' '
         if(iand(attr,A_LOW).eq.A_LOW)               class=trim(class)//' '
         if(iand(attr,A_RIGHT).eq.A_RIGHT)           class=trim(class)//' '
         if(iand(attr,A_STANDOUT).eq.A_STANDOUT)     class=trim(class)//' '
         if(iand(attr,A_TOP).eq.A_TOP)               class=trim(class)//' '
         if(iand(attr,A_VERTICAL).eq.A_VERTICAL)     class=trim(class)//' '
         if(iand(attr,A_BOLD).eq.A_BOLD)             class=trim(class)//' BO'
         if(iand(attr,A_ITALIC).eq.A_ITALIC)         class=trim(class)//' I'
!-----------------------------------------------------------------------------------------------------------------------------------
         ! The Alternate Character Set ( includes lines & boxes)
         if(iand(attr,A_ALTCHARSET).eq.A_ALTCHARSET)then
            select case(let)
            case('l'); lets="&#9484;"   ! ACS_ULCORNER upper left corner        ┌
            case('m'); lets="&#9492;"   ! ACS_LLCORNER lower left corner        └
            case('k'); lets="&#9488;"   ! ACS_URCORNER upper right corner       ┐
            case('j'); lets="&#9496;"   ! ACS_LRCORNER lower right corner       ┘
            case('t'); lets="&#9500;"   ! ACS_LTEE     tee pointing right       ├
            case('u'); lets="&#9508;"   ! ACS_RTEE     tee pointing left        ┤
            case('v'); lets="&#9524;"   ! ACS_BTEE     tee pointing up          ┴
            case('w'); lets="&#9516;"   ! ACS_TTEE     tee pointing down        ┬
            case('q'); lets="&#9472;"   ! ACS_HLINE    horizontal line          ─
            case('x'); lets="&#9474;"   ! ACS_VLINE    vertical line            !
            case('n'); lets="&#9532;"   ! ACS_PLUS     large plus or crossover  ┼
            case('o'); lets="&#9146;"   ! ACS_S1       macron, overline, scan line 1 (above) ⎺
               lets="&macr;"
               lets="&#9146;"
            case('s'); lets="&#9149;"   ! ACS_S9       scan line 9 (below)      ⎽
            case('`'); lets="&#9670;"   ! ACS_DIAMOND  diamond                  ◆
               lets="&loz;"
               lets="&diams;"
            case('a'); lets="&#2591;"   ! ACS_CKBOARD  checker board (stipple)  ▒
               lets="&#9618;"
            case('f'); lets="&deg;"     ! ACS_DEGREE   degree symbol            °
            case('g'); lets="&plusmn;"  ! ACS_PLMINUS  plus/minus               ±
            case('~'); lets="&bull;"    ! ACS_BULLET   bullet                   · •
            case(','); lets="&larr;"    ! ACS_LARROW   arrow pointing left      ←
            case('+'); lets="&rarr;"    ! ACS_RARROW   arrow pointing right     →
            case('.'); lets="&darr;"    ! ACS_DARROW   arrow pointing down      ↓
            case('-'); lets="&uarr;"    ! ACS_UARROW   arrow pointing up        ↑
            case('h'); lets="&#9626;"   ! ACS_BOARD    board of squares         ▚
            case('i'); lets="&#9227;"   ! ACS_LANTERN  lantern symbol           ␋
            case('0'); lets="&block;"   ! ACS_BLOCK    solid square block       █
            case('p'); lets="&#9147;"   ! ACS_S3       scan line 3(at top)      ⎻
            case('r'); lets="&#9148;"   ! ACS_S7       scan line 7 (at bottom)  ⎼
            case('y'); lets="&le;"      ! ACS_LEQUAL   less/equal               ≤
            case('z'); lets="&ge;"      ! ACS_GEQUAL   greater/equal            ≥
            case('{'); lets="&pi;"      ! ACS_PI       Pi                       π
            case('|'); lets="&ne;"      ! ACS_NEQUAL   not equal                ≠
            case('}'); lets="&pound;"   ! ACS_STERLING UK pound sign            £
            end select
         else                           ! Regular Characters that are special in HTML
            select case(let)
            case('&'); lets='&amp;'     !   &  &#38;  {ampersand}
            case('<'); lets='&lt;'      !   <  &#60;  {less than}
            case('>'); lets='&gt;'      !   >  &#62;  {greater than}
            case('"'); lets='&quot;'    !   "  &#34;  {quotation mark}
            !case(' '); lets='&nbsp;'   !      &#160; {Non-breaking space}
            end select
         endif
!-----------------------------------------------------------------------------------------------------------------------------------
         if(iand(attr,A_REVERSE).eq.A_REVERSE)then
            write(class,'(a," RPAIR",i0)') trim(class),pair
         else
            write(class,'(a," PAIR",i0)') trim(class),pair
         endif
!-----------------------------------------------------------------------------------------------------------------------------------
         if(iand(attr,A_STANDOUT).eq.A_STANDOUT)then
            write(class,'(a," RPAIR",i0)') trim(class),pair
         else
            write(class,'(a," PAIR",i0)') trim(class),pair
         endif
!-----------------------------------------------------------------------------------------------------------------------------------
         if(class.ne.lastclass)then
            if(span)then
               write(iout)'</span>'
               span=.false.
            endif
            if(class.eq.' ')then
               write(iout)'<span class="N">'
            else
               write(iout)'<span class="'//trim(class)//'">'
            endif
            span=.true.
         endif
!-----------------------------------------------------------------------------------------------------------------------------------
         write(iout)lets(:max(1,len_trim(lets))) ! print a space even if string is blank
         lastclass=class
      enddo
      write(iout)NEW_LINE('a')
   enddo
!-----------------------------------------------------------------------------------------------------------------------------------
   if(span)call ln('</span>')
   call ln('</pre>')
   call ln('</body>')
   call ln('</html>')
   endfile(unit=iout,iostat=ios,iomsg=msg) ! make sure file is truncated or longer old files may leave data in file
contains
subroutine ln(string)
   implicit none
   character(len=*) :: string
   write(iout)string
   write(iout)new_line('a')
   end subroutine ln
end subroutine nc_printhtml
