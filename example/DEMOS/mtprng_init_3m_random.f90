          program demo_mtprng_init
          use M_random, only : mtprng_state, mtprng_init, mtprng_rand64
          use, intrinsic :: iso_fortran_env, only : int32, int64
          implicit none
          integer(INT32) :: seed
          type(mtprng_state) :: state
             GET_SEED: block
             integer :: count
             integer :: count_rate
                call system_clock(count, count_rate)
                seed=count
             endblock GET_SEED
            call mtprng_init(seed, state)
            ! returns a INT64 integer with a range in 0 .. 2^32-1
            write(*,*) mtprng_rand64(state)
          end program demo_mtprng_init
