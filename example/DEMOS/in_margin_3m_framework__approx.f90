     program demo_in_margin
     use :: M_framework__approx, only : in_margin
     implicit none
     write(*,*) in_margin(4.00000,3.99999,0.000000001)
     write(*,*) in_margin(4.00000,3.99999,0.00000001)
     write(*,*) in_margin(4.00000,3.99999,0.0000001)
     write(*,*) in_margin(4.00000,3.99999,0.000001)

     write(*,*) in_margin([4.0,40.0,400.0,4000.0,40000.0], &
             & [3.9,39.9,399.9,3999.9,39999.9] ,0.000001)
     write(*,*) in_margin([4.0,40.0,400.0,4000.0,40000.0], &
             & [3.9,39.9,399.9,3999.9,39999.9] ,0.00001)

     write(*,*) in_margin(4.00000,3.99999,0.00001)
     write(*,*) in_margin(4.00000,3.99999,0.0001)
     write(*,*) in_margin(4.00000,3.99999,0.001)
     write(*,*) in_margin(4.00000,3.99999,0.01)

     end program demo_in_margin
