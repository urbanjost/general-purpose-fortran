          program demo_iget
          use M_kracken, only: kracken, iget
          implicit none
          integer :: val
            ! define command arguments and parse user command
            call kracken('demo','-val 31416' )
            val=iget('demo_val') ! get any values specified on -val option
            write(*,*)val        ! print the value
          end program demo_iget
