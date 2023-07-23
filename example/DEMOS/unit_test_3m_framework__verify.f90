        program demo_unit_test
        use M_framework, only: &
           & unit_test_mode,     &
           & unit_test_start,    &
           & unit_test,          &
           & unit_test_end,      &
           & unit_test_stop
        use M_framework, only: almost

        implicit none
        integer :: i
        integer :: x
        integer,allocatable :: arr(:)
        real,allocatable :: arr1(:)
        real,allocatable :: arr2(:)

           call unit_test_mode(keep_going=.true.,debug=.false.,command='')

           x=10
           arr1=[1.0,10.0,100.0]
           arr2=[1.0001,10.001,100.01]
           call unit_test_start('myroutine')

           call unit_test('myroutine', x > 3 ,' if big enough')
           call unit_test('myroutine', x < 100 ,' if small enough')

           do i=1,size(arr1)
              call unit_test('myroutine', &
              & almost(arr1(i),arr2(i),3.9,verbose=.true.) )
           enddo

           arr=[10,20,30]
           call unit_test('myroutine', .not.any(arr < 0) , &
           & 'fail if any negative values in array ARR')
           call unit_test('myroutine', all(arr < 100) , &
           & 'fail unless all values are less than 100 in array ARR')

           call unit_test_end('myroutine', &
           & msg='checks on "myroutine" all passed')

           call unit_test_stop()

        end program demo_unit_test
