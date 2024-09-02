     program demo_unit_test_msg
     use M_framework, only : unit_test_start,unit_test_msg, &
             & unit_test_end
     implicit none

     call unit_test_start('myroutine')
     call unit_test_msg('myroutine','HUGE(3f) integers', &
             & huge(0),'and real',huge(0.0),'and double',huge(0.0d0))
     call unit_test_msg('myroutine','real            :', &
             & huge(0.0),0.0,12345.6789,tiny(0.0) )
     call unit_test_msg('myroutine','doubleprecision :', &
             & huge(0.0d0),0.0d0,12345.6789d0,tiny(0.0d0) )
     call unit_test_msg('myroutine','complex         :', &
             & cmplx(huge(0.0),tiny(0.0)) )
     call unit_test_end('myroutine')

     end program demo_unit_test_msg
