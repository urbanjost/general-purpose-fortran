          program demo_lgets
          use M_kracken, only: kracken, lgets
          implicit none
          logical,allocatable  :: vals(:)
            ! define command arguments and parse user command
            call kracken('demo','-truths .F. .T. .F. .F. .T. .T.' )
            ! get any values specified on command line for -truth
            vals=lgets('demo_truths')
            write(*,*)vals
          end program demo_lgets
