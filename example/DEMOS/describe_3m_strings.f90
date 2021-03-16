          program demo_describe
           use M_strings, only : describe
           implicit none
           integer :: i
              do i=1,128  ! fill variable with base ASCII character set
                 write(*,*)describe(char(i-1))
              enddo
          end program demo_describe
