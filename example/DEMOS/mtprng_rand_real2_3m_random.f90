          program demo_mtprng_real2
          use M_random, only : mtprng_state, mtprng_init, mtprng_rand_real2
          use, intrinsic :: iso_fortran_env, only : int32
          implicit none
          integer(INT32)     :: seed
          type(mtprng_state) :: state
            GET_SEED: block
            integer :: count
            integer :: count_rate
               call system_clock(count, count_rate)
               seed = count
            endblock GET_SEED
            call mtprng_init(seed, state)
            ! returns a IEEE64 real, may be used as double precision
            write(*,*) mtprng_rand_real2(state)
          end program demo_mtprng_real2
