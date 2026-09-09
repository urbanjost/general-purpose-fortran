     program demo_rget
     use M_kracken, only: kracken, rget
     implicit none
     real :: val
       ! define command arguments and parse user command
       call kracken('demo','-val 3.1416' )
       val=rget('demo_val') ! get any values specified on -val option
       write(*,*)val        ! print the value
     end program demo_rget
