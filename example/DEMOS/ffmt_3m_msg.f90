           program demo_fmt
           use :: M_msg, only : ffmt
           implicit none
           character(len=:),allocatable :: output

              output=ffmt(10,"'[',i0,']'")
              write(*,*)'result is ',output

              output=ffmt(10.0/3.0,"'[',g0.5,']'")
              write(*,*)'result is ',output

              output=ffmt(.true.,"'The final answer is [',g0,']'")
              write(*,*)'result is ',output

              end program demo_fmt
