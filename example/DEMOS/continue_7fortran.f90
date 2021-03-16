          program demo_continue
                I=10
                J=5
                if(I.lt.5)goto 100
                J=3
          100   continue
                write(*,*)'J=',J

          end program demo_continue
