     program demo_random_int
     use M_random, only : random_int, init_random_seed_by_system_clock
     call init_random_seed_by_system_clock()
     write(*,'(*(i0:,1x))')(random_int(1,10),i=1,20)
     write(*,'(*(i0:,1x))')(random_int(-5,5),i=1,20)
     end program demo_random_int
