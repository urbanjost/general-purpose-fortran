          program demo_lget
          use M_kracken, only: kracken, lget
          implicit none
          logical  :: val
            ! define command arguments and parse user command
            call kracken('demo','-truth .F.' )
            ! get any values specified on command line for -truth
            val=lget('demo_truth')
            write(*,'("The truth is ",l1)')val
          end program demo_lget
