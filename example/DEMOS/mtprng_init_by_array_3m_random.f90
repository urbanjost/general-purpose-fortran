     program demo_mtprng_init_by_array
     use M_random, only : mtprng_state, mtprng_init_by_array
     use M_random, only : mtprng_rand64, mtprng_rand_real1
     use, intrinsic :: iso_fortran_env, only : int32, int64
     implicit none
     integer(INT32)     :: init_key(3)
     type(mtprng_state) :: state
       GET_SEED: block
       integer :: count
       integer :: count_rate
          call system_clock(count, count_rate)
          init_key(1) = 11*count
          init_key(2) = 37*count
          init_key(3) = 97*count
       endblock GET_SEED
       call mtprng_init_by_array(init_key, state )
       ! returns a INT64 integer with a range in 0 .. 2^32-1
       write(*,*) mtprng_rand64(state)
       ! returns a IEEE64 real, may be used as double precision
       write(*,*) mtprng_rand_real1(state)
     end program demo_mtprng_init_by_array
