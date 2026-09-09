      !! program demo_M_framework__verify
      module M_framework__verify_demo
      private
      public one ! some regular routine
      public two ! some regular routine
      contains

      subroutine one(array)
      integer,intent(out),allocatable :: array(:)
         array=[21,51,14,45]
      end subroutine one

      subroutine two(array)
      integer,intent(inout),allocatable :: array(:)
         array=2*array
      end subroutine two

      end module M_framework__verify_demo

      program demo_M_framework__verify
      use M_framework, only: unit_test_start, unit_test,   &
          & unit_test_end, unit_test_msg, unit_test_stop, &
          & unit_test_system, unit_test_mode
      use M_framework__verify_demo,   only: one, two
      ! set-up
      call unit_test_mode(command='',flags=[0],keep_going=.true.)
      ! call a test procedure for each routine to test
         call test_one()
         call test_two()
      ! tear-down
      call unit_test_stop()
      contains

      subroutine test_one()
      integer,allocatable :: results(:)
      integer,parameter   :: expected(*)=[21,51,14,45]
      call unit_test_start('one')
      call one(results)
      call unit_test('one',all(expected>0), &
         & 'testing if everyone greater than zero')
      call unit_test('one',all(expected==results), &
         & 'testing if all values are expected')
      call unit_test_end('one','checks on "one" ended')
      end subroutine test_one

      subroutine test_two
      integer,allocatable :: results(:)
      integer,parameter   :: expected(*)=[2,20,200]
      results=[1,10,100]
      call two(results)
      call unit_test_start('two','check procedure "two" ')
      call unit_test('two', all(expected == results) .and. &
         & all(expected > 0) .and. maxval(expected) <201,msg='long expression')
      call unit_test_end('two','checks on "two" ended')
      end subroutine test_two

      end program demo_M_framework__verify
