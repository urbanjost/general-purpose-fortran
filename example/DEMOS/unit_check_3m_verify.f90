     program demo_unit_check
     use M_verify, only: unit_check
     use M_verify, only: unit_check_start
     use M_verify, only: unit_check_done
     use M_verify,  only: almost

     !!use M_verify, only: unit_check_keep_going         ! default is unit_check_keep_going=.false.
     !!use M_verify, only: debug              ! default is .false.
     !!use M_verify, only: unit_check_command ! default is unit_check_command=''; was 'goodbad'

     implicit none
     integer :: i
     integer :: x
     integer,allocatable :: arr(:)
     real,allocatable :: arr1(:)
     real,allocatable :: arr2(:)

        !!unit_check_command=''
        x=10
        arr1=[1.0,10.0,100.0]
        arr2=[1.0001,10.001,100.01]
        call unit_check_start('myroutine')

        call unit_check('myroutine', x > 3 ,'test if big enough')
        call unit_check('myroutine', x < 100 ,'test if small enough')

        do i=1,size(arr1)
           call unit_check('myroutine', almost(arr1(i),arr2(i),3.9,verbose=.true.) )
        enddo

        arr=[10,20,30]
        call unit_check('myroutine', .not.any(arr < 0) ,'test if any negative values in array ARR')
        call unit_check('myroutine', all(arr < 100) ,'test if all values less than 100 in array ARR')

        call unit_check_done('myroutine',msg='checks on "myroutine" all passed')

     end program demo_unit_check
