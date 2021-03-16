          program demo_h_close
          use M_html
          implicit none
          real    :: arr(10,20)=0.0
          integer :: io=20
          integer :: i,j
          do i=1,10
             do j=1,20
                arr(i,j)=(i-1)*20+j
             enddo
          enddo
          call h_open(io,'table.html')
          call h_array(io,arr)
          call h_close(io)

          end program demo_h_close
