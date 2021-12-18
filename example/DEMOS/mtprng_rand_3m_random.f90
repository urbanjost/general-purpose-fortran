     program demo_mtprng_rand
     use M_random, only : mtprng_state, mtprng_init, mtprng_rand
     use, intrinsic :: iso_fortran_env, only : int32
     implicit none
     integer(INT32) :: seed
     type(mtprng_state) :: state
       GET_SEED: block
       integer :: count
       integer :: count_rate
          call system_clock(count, count_rate)
          seed = count
       endblock GET_SEED
       call mtprng_init(seed, state)
       ! returns a INT32 integer with a range in 0 .. 2^31-1
       write(*,*) mtprng_rand(state)
     end program demo_mtprng_rand
