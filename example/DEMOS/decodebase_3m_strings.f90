          program demo_decodebase
          use M_strings, only : codebase, decodebase
          implicit none
          integer           :: ba,bd
          character(len=40) :: x,y
          integer           :: r

          print *,' BASE CONVERSION'
          write(*,'("Start   Base (2 to 36): ")',advance='no'); read *, bd
          write(*,'("Arrival Base (2 to 36): ")',advance='no'); read *, ba
          INFINITE: do
             print *,''
             write(*,'("Enter number in start base: ")',advance='no'); read *, x
             if(x.eq.'0') exit INFINITE
             if(decodebase(x,bd,r)) then
                if(codebase(r,ba,y)) then
                  write(*,'("In base ",I2,": ",A20)')  ba, y
                else
                  print *,'Error in coding number.'
                endif
             else
                print *,'Error in decoding number.'
             endif
          enddo INFINITE

          end program demo_decodebase
