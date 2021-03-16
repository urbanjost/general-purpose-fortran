           program demo_random_hex
           use M_random, only : random_hex, init_random_seed_by_dat
              character(len=64) :: hexstring
              ! use date and time to create a seed for calling random_seed(3f)
              call init_random_seed_by_dat()
              ! write random hexadecimal value for use
              ! as something like an X11 authorization key
              hexstring=random_hex(len(hexstring))
              write(*,'(a)')hexstring
           end program demo_random_hex
