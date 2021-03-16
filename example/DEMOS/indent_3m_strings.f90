          program demo_indent
          !  test filter to count leading spaces in a character variable
          !  might want to call notabs(3f) to expand tab characters
          use M_strings, only : indent
          implicit none
          character(len=1024) :: in
          integer             :: ios
             READFILE: do
                read(*,'(A)',iostat=ios)in
                if(ios /= 0) exit READFILE
                write(*,'(i3,"",a)')indent(in),trim(in)
             enddo READFILE
          end program demo_indent
