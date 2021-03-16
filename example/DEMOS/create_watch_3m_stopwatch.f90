          program demo_creat_watch
          ! example program starts a watch W1, stops it, and prints the results
          use,intrinsic :: iso_fortran_env, only : &
             ERROR_UNIT, INPUT_UNIT, OUTPUT_UNIT ! access computing environment
          use M_stopwatch, only : watchtype
          use M_stopwatch, only : option_stopwatch, create_watch, start_watch
          use M_stopwatch, only : stop_watch, print_watch, destroy_watch
          implicit none
          type (watchtype) w1
          character(len=:),allocatable :: cmd
          integer errcode

             cmd='hostname;sleep 3;date;pwd'

             call option_stopwatch(                                            &
                default_clock=[character(len=4) :: 'cpu','wall','user','sys'], &
                io_unit_print=ERROR_UNIT,                                      &
                io_unit_error=ERROR_UNIT)

             call create_watch(watch=w1, name='times')
             call start_watch(watch=w1)

             call execute_command_line(cmd) ! do something that takes some time

             call stop_watch(watch=w1)
             call print_watch(watch=w1, title='COMMAND:'//cmd, err=errcode)
             call destroy_watch(w1)

          end program demo_creat_watch
