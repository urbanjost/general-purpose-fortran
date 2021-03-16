           program demo_expand
           !  test filter to expand escape sequences in input lines
           use M_strings, only : expand
           character(len=1024) :: line
           integer             :: ios
              READFILE: block
                 do
                    read(*,'(A)',iostat=ios)line
                    if(ios /= 0) exit READFILE
                    write(*,'(a)')trim(expand(line))
                 enddo
              endblock READFILE
           end program demo_expand
