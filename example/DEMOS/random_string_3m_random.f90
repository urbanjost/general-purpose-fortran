      program demo_random_string
      use M_random, only : random_string, init_random_seed_by_dat
         character(len=64) :: hexstring
         ! use date and time to create a seed for calling random_seed(3f)
         call init_random_seed_by_dat()
         hexstring=random_string('0123456789abcdef',len(hexstring))
         ! write random hexadecimal value for use
         ! as something like an X11 authorization key
         write(*,'(a)')hexstring
      end program demo_random_string
