           program demo_trim
           implicit none
             character(len=10), parameter :: s = "gfortran  "
             write(*,*) len(s), len(trim(s))  ! "10 8", with/without trailing blanks
           end program demo_trim
