     program demo_system_readline
     use M_readline, only : system_readline
     implicit none
     character(len=4096) :: line
     integer             :: cstat
     character(len=256)  :: sstat
        call intro()

        do
           call system_readline(line,'readline>') ! read editable input line
           if(line.eq.'q') then
              stop
           elseif(line.eq.'?') then
              call intro()
           else
              call execute_command_line(trim(line),cmdstat=cstat,cmdmsg=sstat)
           endif
        enddo

     contains
     subroutine intro()
     integer             :: i
     character(len=*),parameter :: help_text(*)=[character(len=80) :: &
     '--------------------------------------------------------------------------------' ,&
     '  Your input lines are now editable using the GNU readline(3c) procedure.'        ,&
     '  By default, up-arrow and down-arrow go through the history lines; left and '    ,&
     '  and right arrow keys and the delete key and just typing characters let you do'  ,&
     '  simple editing. Far more input control is available. See the mon(1) page for'   ,&
     '  readline(3c) for more information.'                                             ,&
     ' --------------------------------------------------------------------------------',&
     ' "q" quits; "h" display history, "?" displays this help text'                     ,&
     ' Enter commands and then edit them...'                                            ,&
     ' ' ]
        write(*,'(a)')(trim(help_text(i)),i=1,size(help_text))
     end subroutine intro
     end program demo_system_readline
