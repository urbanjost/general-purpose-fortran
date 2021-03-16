          program demo_exec
          implicit none
            integer :: i

            call execute_command_line("external_prog.exe", exitstat=i)
            print *, "Exit status of external_prog.exe was ", i

            call execute_command_line("reindex_files.exe", wait=.false.)
            print *, "Now reindexing files in the background"
          end program demo_exec
