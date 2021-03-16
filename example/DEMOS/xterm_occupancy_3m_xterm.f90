          program demo_xterm_occupancy
          use M_xterm, only : xterm_occupancy
          call xterm_occupancy("all")
          call xterm_occupancy("1")
          call xterm_occupancy("Project A")
          end program demo_xterm_occupancy
