      program demo_unit_test_start
      use M_verify, only: unit_test_start
      use M_verify, only: unit_test
      use M_verify, only: unit_test_done

      implicit none
      integer :: ival
      call unit_test_start('myroutine')
      ! the goodbad(1) command called here takes many options
      ! used to build an SQLite3 entry
      call unit_test_start('myroutine_long',' &
        & -section        3                    &
        & -library        libGPF               &
        & -filename       `pwd`/M_verify.FF     &
        & -documentation  y                    &
        & -prep           y                    &
        & -ccall          n                    &
        & -archive        GPF.a                &
        & ')

      ival=10
      call unit_test('myroutine', ival > 3 ,   msg='test if big enough')
      call unit_test('myroutine', ival < 100 , msg='test if small enough')

      call unit_test_done('myroutine',msg='completed checks of "myroutine"')

      end program demo_unit_test_start
