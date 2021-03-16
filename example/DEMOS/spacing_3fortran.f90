           program demo_spacing
           implicit none
             integer, parameter :: sgl = selected_real_kind(p=6, r=37)
             integer, parameter :: dbl = selected_real_kind(p=13, r=200)

             write(*,*) spacing(1.0_sgl)      ! "1.1920929e-07"          on i686
             write(*,*) spacing(1.0_dbl)      ! "2.220446049250313e-016" on i686
           end program demo_spacing
