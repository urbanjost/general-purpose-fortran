      !!program demo_unit_tests
      module M_msg__demo
      private
      public one !! regular routines
      public two !! regular routines
      public test_suite_M_demo !! special name for use with test_suite(1bash).
      contains

      !!  regular routines
      subroutine one()
      end subroutine one

      subroutine two()
      end subroutine two

      !! unit test
      subroutine test_suite_M_demo
      use M_verify, only: unit_test_start, unit_test
      use M_verify, only: unit_test_good, unit_test_bad, unit_test_done
      use M_verify, only: unit_test_msg, unit_test_stop
      implicit none
      integer :: i, j, k
      integer,allocatable :: array(:)
      integer :: arr(4)=[21,51,14,45]
      integer :: a=21, b=51, c=14, d=45
      ! TEST-DRIVEN DEVELOPMENT
      ! optional set-up       perform initialization operations common to all tests within a module
         i=1
         j=2
         k=3
         array=[10,20,30,40,50,60,70]
         call test_one()
         call test_two()
      ! optional tear-down    perform finalization operations common to all tests within a module
      contains

      subroutine test_one()
      !  register an entry for specified name ("one") in database with status of zero (0)
      call unit_test_start('one')

      !  if mask test fails, can
      !  * produce a SUCCESS: or FAIL: message and stop program
      !  * change database status for specified entry to -1 and stop program, else continue
      !  * produce a SUCCESS: or FAIL: message and keep going
      !  * produce a FAIL: message if test fails but no SUCCESS: message if test passes
      call unit_test('one',i > 0,msg='I > 0')

      ! using ANY(3f) and ALL(3f)
      call unit_test('one',all([i,j,k] > 0),      'testing if everyone greater than zero')
      ! display message built of scalars as well
      call unit_test('one',all(.not.[i,j,k] == 4),'for set ',i,j,k,'testing if no one is equal to four')

      ! for tests that are hard to reduce to a logical test just call unit_test_bad(3f) if fail
      if(i+j+k < 1)then
         call unit_test_bad('one')
      endif

      call unit_test_done('one','checks on "one" ended')
      end subroutine test_one

      subroutine test_two
      ! use of all(3f), any(3f), merge(3f) can be useful
      ! if you know what these would produce
      ! write(*,*)['A','X','X','X','X','B'] == 'B'      ! this would return an array, the last element having the value T, else F
      ! write(*,*)all(['A','X','X','X','X','X'] == 'X') ! this would return F
      ! write(*,*)any(['A','X','X','X','X','X'] == 'B') ! this would return F
      ! write(*,*)any(['A','X','X','X','X','B'] == 'B') ! this would return T
      ! write(*,*).not.all(array < 100)
      ! write(*,*)all(array < 100)
      ! write(*,*)all([a,b,c,d] == [21,51,14,45]) ! compare a list. This would return T
      ! write(*,*)all(arr == [21,51,14,45])       ! compare an array. This would return T
      ! you know how valuable ANY(3f) and ALL(3f) will be
      call unit_test_start('two','check on "two" passed')
      call unit_test('two', 1 > 0 .and. abs(10.10000-10.10001) < 0.0001,msg='two looks good')
      call unit_test_done('two','checks on "two" ended')
      end subroutine test_two

      end subroutine test_suite_M_demo

      end module M_msg__demo

      program demo_M_verify
      use M_msg__demo,  only: test_suite_M_demo
      use M_verify, only: unit_test_command, unit_test_keep_going,unit_test_level
      unit_test_command=''
      unit_test_keep_going=.true.
      unit_test_level=0
        call test_suite_M_demo
      end program demo_M_verify
