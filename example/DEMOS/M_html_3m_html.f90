          program demo_M_html
             use M_html
             implicit none
             integer :: i,j
             real    :: arr(3,4)=reshape(         &
             & [(((i-1)*3.0+j*2.0,i=1,3),j=1,4)], &
             & shape(arr),order=[1,2])
             integer :: io=6
             call h_open(io,'table.html')
             call h_array(io,arr)
             call h_close(io)
          end program demo_M_html
