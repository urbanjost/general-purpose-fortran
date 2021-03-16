program redoit
!(LICENSE:PD)
use M_history, only : redo
implicit none

character(len=*),parameter :: ident="@(#) redo(1f): simple history shell allows using redo(3f) for command history"

character(len=1024)        :: line
integer                    :: ios
integer                    :: cstat
character(len=256)         :: sstat

   call instructions()
   do
      write(*,'(a)',advance='no')'>->'    ! write prompt
      read(*,'(a)',iostat=ios) line       ! read new input line

      ! if "r", edit and return a line from the history editor
      call redo(line) ! store into history if not "r".

      if(line.eq.'quit')stop ! exit program if user enters "quit"

      ! now call user code to process new line of data
      ! As an example, call the system shell 
      call execute_command_line(trim(line),cmdstat=cstat,cmdmsg=sstat) 
   enddo
   contains
      subroutine instructions
      write(*,'(a)')'_______________________________________________________________________________'
      write(*,'(a)')'| REDO(3f) COMMAND INPUT EDITOR                                                |'
      write(*,'(a)')'|                                                                              |'
      write(*,'(a)')'| Enter commands to be passed to the shell and then enter "r" or "r r_command" |'
      write(*,'(a)')'| on the input line to go into history edit mode. Once in history edit mode    |'
      write(*,'(a)')'| you may enter "?" to get some help. Enter "quit" to exit the program         |'
      write(*,'(a)')'|______________________________________________________________________________|'
      end subroutine instructions
end program redoit
