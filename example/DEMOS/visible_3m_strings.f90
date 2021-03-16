           program demo_visible
           use M_strings, only : visible
           integer :: i
              do i=0,255
                 write(*,'(i0,1x,a)')i,visible(char(i))
              enddo
           end program demo_visible
