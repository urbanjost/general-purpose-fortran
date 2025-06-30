     program demo_unit_test
     use M_verify, only: unit_test
     use M_verify, only: unit_test_start
     use M_verify, only: unit_test_done
     use M_verify,  only: almost

     !!use M_verify, only: unit_test_keep_going         ! default is unit_test_keep_going=.false.
     !!use M_verify, only: debug              ! default is .false.
     !!use M_verify, only: unit_test_command ! default is unit_test_command=''; was 'goodbad'

     implicit none
     integer :: i
     integer :: x
     integer,allocatable :: arr(:)
     real,allocatable :: arr1(:)
     real,allocatable :: arr2(:)

        !!unit_test_command=''
        x=10
        arr1=[1.0,10.0,100.0]
        arr2=[1.0001,10.001,100.01]
        call unit_test_start('myroutine')

        call unit_test('myroutine', x > 3 ,'test if big enough')
        call unit_test('myroutine', x < 100 ,'test if small enough')

        do i=1,size(arr1)
           call unit_test('myroutine', almost(arr1(i),arr2(i),3.9,verbose=.true.) )
        enddo

        arr=[10,20,30]
        call unit_test('myroutine', .not.any(arr < 0) ,'test if any negative values in array ARR')
        call unit_test('myroutine', all(arr < 100) ,'test if all values less than 100 in array ARR')

        call unit_test_done('myroutine',msg='checks on "myroutine" all passed')

     end program demo_unit_test
