program init_func_example
!(LICENSE:PD)
! @(#) Capture all key strokes and identify what keys were pressed

   use M_ncurses
   implicit none
   integer           :: ch
   integer           :: ierr
   integer           :: cursor_state
   integer           :: maxrow,maxcol,irow
   character(len=80) :: msg
   stdscr=initscr()                      ! Start curses mode
   ierr=raw()                            ! Line buffering disabled
   ierr=keypad(stdscr,TRUE)              ! We get function keys such as F1, F2 etc..
   ierr=noecho()                         ! Don't echo() while we do getch(3c)
   cursor_state=curs_set(0)              ! The curs_set routine sets the cursor state to invisible, normal,
                                         ! or very visible for visibility equal to 0, 1, or 2 respectively. If
                                         ! the terminal supports the visibility requested, the previous cursor
                                         ! state is returned; otherwise, ERR is returned
   call getmaxyx(stdscr,maxrow,maxcol)   ! get the number of rows and columns
   ierr=mvprintw(0,0,"Type any character to see it in bold ('q' to quit)"//C_NULL_CHAR)
   ierr=mvprintw(1,0,"Note on some keyboards you hit [FN][Function Key] to press a 'function key'"//C_NULL_CHAR)
   irow=2
   INFINITE: do
      ch = getch()                       ! If raw() hadn't been called we have to press enter before it gets to the program

      if(irow.gt.maxrow-1)irow=2
      ierr=move(irow,0)                  ! move to beginning of line from the top
      ierr=clrtoeol()                    ! make sure the line is cleared

      if(ch >= KEY_F(1).and. ch <= KEY_F(64) )then   ! Without keypad enabled this will not get to us either
         ierr=printw("pressed key with value = %d which is "//C_NULL_CHAR,ch)
         ierr=printw("Function Key %d "//C_NULL_CHAR,ch-KEY_F0)
         ! Note that without noecho() some ugly escape characters might have been printed  on screen
      elseif(ch>=33 .and. ch<=126)then   ! regular printable character
         ierr=printw("The pressed key value is %d, the regular character "//C_NULL_CHAR,ch)
         ierr=attron(A_BOLD)
         ierr=printw("%c"//C_NULL_CHAR, ch)
         ierr=attroff(A_BOLD)
    if(char(ch)=='q')exit INFINITE
      else
         msg=""
         SELECT CASE (ch)
         case (key_break ) ; msg=" break: Not on PC KBD"
         case (key_down ) ; msg=" down: Down arrow key"
         case (key_up ) ; msg=" up: Up arrow key"
         case (key_left ) ; msg=" left: Left arrow key"
         case (key_right ) ; msg=" right: Right arrow key"
         case (key_home ) ; msg=" home: home key"
         case (key_backspace ) ; msg=" backspace: not on all PC KBD"
         case (key_f0 ) ; msg=" f0: function keys; 64 reserved"
         case (key_dl ) ; msg=" dl: delete line"
         case (key_il ) ; msg=" il: insert line"
         case (key_dc ) ; msg=" dc: delete character"
         case (key_ic ) ; msg=" ic: insert char or enter ins mode"
         case (key_eic ) ; msg=" eic: exit insert char mode"
         case (key_clear ) ; msg=" clear: clear screen"
         case (key_eos ) ; msg=" eos: clear to end of screen"
         case (key_eol ) ; msg=" eol: clear to end of line"
         case (key_sf ) ; msg=" sf: scroll 1 line forward"
         case (key_sr ) ; msg=" sr: scroll 1 line back (reverse)"
         case (key_npage ) ; msg=" npage: next page"
         case (key_ppage ) ; msg=" ppage: previous page"
         case (key_stab ) ; msg=" stab: set tab"
         case (key_ctab ) ; msg=" ctab: clear tab"
         case (key_catab ) ; msg=" catab: clear all tabs"
         case (key_enter ) ; msg=" enter: enter or send (unreliable)"
         case (key_sreset ) ; msg=" sreset: soft/reset (partial/unreliable)"
         case (key_reset ) ; msg=" reset: reset/hard reset (unreliable)"
         case (key_print ) ; msg=" print: print/copy"
         case (key_ll ) ; msg=" ll: home down/bottom (lower left)"
         case (key_a1 ) ; msg=" a1:"
         case (key_a3 ) ; msg=" a3:"
         case (key_b2 ) ; msg=" b2:"
         case (key_c1 ) ; msg=" c1:"
         case (key_c3 ) ; msg=" c3:"
         case (key_btab ) ; msg=" btab: Back tab key"
         case (key_beg ) ; msg=" beg: beginning key"
         case (key_cancel ) ; msg=" cancel: cancel key"
         case (key_close ) ; msg=" close: close key"
         case (key_command ) ; msg="command: command key"
         case (key_copy ) ; msg=" copy: copy key"
         case (key_create ) ; msg=" create: create key"
         case (key_end ) ; msg=" end: end key"
         case (key_exit ) ; msg=" exit: exit key"
         case (key_find ) ; msg=" find: find key"
         case (key_help ) ; msg=" help: help key"
         case (key_mark ) ; msg=" mark: mark key"
         case (key_message ) ; msg=" message: message key"
         case (key_move ) ; msg=" move: move key"
         case (key_next ) ; msg=" next: next object key"
         case (key_open ) ; msg=" open: open key"
         case (key_options ) ; msg=" options: options key"
         case (key_previous ) ; msg=" previous: previous object key"
         case (key_redo ) ; msg=" redo: redo key"
         case (key_reference ) ; msg=" reference: reference key"
         case (key_refresh ) ; msg=" refresh: refresh key"
         case (key_replace ) ; msg=" replace: replace key"
         case (key_restart ) ; msg=" restart: restart key"
         case (key_resume ) ; msg=" resume: resume key"
         case (key_save ) ; msg=" save: save key"
         case (key_sbeg ) ; msg=" sbeg: shifted beginning key"
         case (key_scancel ) ; msg=" scancel: shifted cancel key"
         case (key_scommand ) ; msg=" scommand: shifted command key"
         case (key_scopy ) ; msg=" scopy: shifted copy key"
         case (key_screate ) ; msg=" screate: shifted create key"
         case (key_sdc ) ; msg=" sdc: shifted delete char key"
         case (key_sdl ) ; msg=" sdl: shifted delete line key"
         case (key_select ) ; msg=" select: select key"
         case (key_send ) ; msg=" send: shifted end key"
         case (key_seol ) ; msg=" seol: shifted clear line key"
         case (key_sexit ) ; msg=" sexit: shifted exit key"
         case (key_sfind ) ; msg=" sfind: shifted find key"
         case (key_shelp ) ; msg=" shelp: shifted help key"
         case (key_shome ) ; msg=" shome: shifted home key"
         case (key_sic ) ; msg=" sic: shifted input key"
         case (key_sleft ) ; msg=" sleft: shifted left arrow key"
         case (key_smessage ) ; msg=" smessage: shifted message key"
         case (key_smove ) ; msg=" smove: shifted move key"
         case (key_snext ) ; msg=" snext: shifted next key"
         case (key_soptions ) ; msg=" soptions: shifted options key"
         case (key_sprevious ) ; msg=" sprevious: shifted prev key"
         case (key_sprint ) ; msg=" sprint: shifted print key"
         case (key_sredo ) ; msg=" sredo: shifted redo key"
         case (key_sreplace ) ; msg=" sreplace: shifted replace key"
         case (key_sright ) ; msg=" sright: shifted right arrow"
         case (key_srsume ) ; msg=" srsume: shifted resume key"
         case (key_ssave ) ; msg=" ssave: shifted save key"
         case (key_ssuspend ) ; msg=" ssuspend: shifted suspend key"
         case (key_sundo ) ; msg=" sundo: shifted undo key"
         case (key_suspend ) ; msg=" suspend: suspend key"
         case (key_undo ) ; msg=" undo: undo key"
         case (key_mouse ) ; msg=" mouse: mouse key"
         case (key_resize ) ; msg=" resize: window resize"
         case (key_event ) ; msg=" event: event key"
         case (key_max ) ; msg=" max: undo key"
         CASE DEFAULT
           msg=""
         END SELECT
    if(msg /= "" )then
            ierr=printw("The pressed key value is %d, named function "//C_NULL_CHAR,ch)
            ierr=attron(A_BOLD)
            ierr=printw("%s"//C_NULL_CHAR, trim(msg)//C_NULL_CHAR)
            ierr=attroff(A_BOLD)
    else
            SELECT CASE (ch)
               CASE(0) ; msg="NUL '\0'"
               CASE(1) ; msg="SOH"
               CASE(2) ; msg="STX"
               CASE(3) ; msg="ETX"
               CASE(4) ; msg="EOT"
               CASE(5) ; msg="ENQ"
               CASE(6) ; msg="ACK"
               CASE(7) ; msg="BEL '\a'"
               CASE(8) ; msg="BS '\b'"
               CASE(9) ; msg="HT '\t'"
               CASE(10) ; msg="LF '\n'"
               CASE(11) ; msg="VT '\v'"
               CASE(12) ; msg="FF '\f'"
               CASE(13) ; msg="CR '\r'"
               CASE(14) ; msg="SO"
               CASE(15) ; msg="SI"
               CASE(16) ; msg="DLE"
               CASE(17) ; msg="DC1"
               CASE(18) ; msg="DC2"
               CASE(19) ; msg="DC3"
               CASE(20) ; msg="DC4"
               CASE(21) ; msg="NAK"
               CASE(22) ; msg="SYN"
               CASE(23) ; msg="ETB"
               CASE(24) ; msg="CAN"
               CASE(25) ; msg="EM"
               CASE(26) ; msg="SUB"
               CASE(27) ; msg="ESC"
               CASE(28) ; msg="FS"
               CASE(29) ; msg="GS"
               CASE(30) ; msg="RS"
               CASE(31) ; msg="US"
               CASE(32) ; msg="SPACE"
               CASE(127) ; msg="DEL"
            CASE DEFAULT
                 msg=""
            END SELECT
          if(msg /= "" )then
             ierr=printw("The key value is %d, a normal 'non-printable' called "//C_NULL_CHAR,ch)
             ierr=attron(A_BOLD)
             ierr=printw(" %s which prints as"//C_NULL_CHAR, trim(msg)//C_NULL_CHAR)
             ierr=printw("%c"//C_NULL_CHAR,ch)
             ierr=attroff(A_BOLD)
          else
             ierr=printw("The pressed key value is %d and is UNKNOWN to this program "//C_NULL_CHAR,ch)
             ierr=attron(A_BOLD)
             ierr=printw("%s"//C_NULL_CHAR, trim(msg)//C_NULL_CHAR)
             ierr=attroff(A_BOLD)
          endif
    endif
      endif
      ierr=refresh()                     ! Print it onto the real screen
      irow=irow+1
   enddo INFINITE
   ierr=getch()
   if(cursor_state >= 0)then             ! restore cursor state so cursor (probably) shows at end of following output
      ierr=curs_set(cursor_state)
   endif
   ierr=endwin()                         ! End curses mode
end program init_func_example
