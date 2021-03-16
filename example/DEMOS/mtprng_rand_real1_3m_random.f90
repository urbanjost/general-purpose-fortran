          program demo_mtprng_real1
          use M_random, only : mtprng_init
          use M_random, only : mtprng_state
          use M_random, only : mtprng_rand_real1
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
            write(*,*) mtprng_rand_real1(state)
          end program demo_mtprng_real1
