           program demo_storage_size
           implicit none
              write(*,*)'size of integer ',storage_size(0)
              write(*,*)'size of real    ',storage_size(0.0)
              write(*,*)'size of logical ',storage_size(.true.)
              write(*,*)'size of complex ',storage_size((0.0,0.0))
              write(*,*)'size of integer array ',storage_size([0,1,2,3,4,5,6,7,8,9])
           end program demo_storage_size
