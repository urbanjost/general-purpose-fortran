          program demo_rotate13
          use M_strings, only : rotate13
          implicit none
          character(len=256) :: line
          integer            :: ios
          do
             read(*,'(a)',iostat=ios)line
             if(ios.ne.0)exit
             write(*,'(a)')rotate13(line)
          enddo
          end program demo_rotate13
