          program demo_base
          use M_strings, only : base
          implicit none
          integer           :: ba,bd
          character(len=40) :: x,y

          print *,' BASE CONVERSION'
          write(*,'("Start   Base (2 to 36): ")',advance='no'); read *, bd
          write(*,'("Arrival Base (2 to 36): ")',advance='no'); read *, ba
          INFINITE: do
             write(*,'("Enter number in start base (0 to quit): ")',advance='no')
             read *, x
             if(x.eq.'0') exit INFINITE
             if(base(x,bd,y,ba))then
                  write(*,'("In base ",I2,": ",A20)')  ba, y
              else
                print *,'Error in decoding/encoding number.'
              endif
           enddo INFINITE

           end program demo_base
