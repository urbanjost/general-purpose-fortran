          program demo_M_f2kcli
          use M_f2kcli, only : command_argument_count
          use M_f2kcli, only : get_command
          use M_f2kcli, only : get_command_argument
          implicit none
          character(len=256) :: line
          character(len=256) :: exe
          character(len=40)  :: cmd
          integer            :: narg,iarg
             narg = command_argument_count()
             write(unit=*,fmt=*) "Arg count=", narg
             call get_command(line)
             write(unit=*,fmt=*) "Line=",trim(line)
             call get_command_argument(0,exe)
             write(unit=*,fmt=*) "Program=",trim(exe)
             do iarg = 1,narg
                call get_command_argument(iarg,cmd)
                WRITE(unit=*,fmt=*) "Arg ",IARG,"=",CMD
             enddo
             stop
          end program demo_M_f2kcli
