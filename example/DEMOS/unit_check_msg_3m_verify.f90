          program demo_unit_check_msg
          use M_verify, only : unit_check_start,unit_check_msg,unit_check_done
          implicit none

          call unit_check_start('myroutine')
          call unit_check_msg('myroutine','HUGE(3f) integers',huge(0),'and real',huge(0.0),'and double',huge(0.0d0))
          call unit_check_msg('myroutine','real            :',huge(0.0),0.0,12345.6789,tiny(0.0) )
          call unit_check_msg('myroutine','doubleprecision :',huge(0.0d0),0.0d0,12345.6789d0,tiny(0.0d0) )
          call unit_check_msg('myroutine','complex         :',cmplx(huge(0.0),tiny(0.0)) )
          call unit_check_done('myroutine')

          end program demo_unit_check_msg
