     program demo_dget
     use M_kracken, only: kracken, dget
     implicit none
     doubleprecision :: val
       ! define command arguments and parse user command
       call kracken('demo','-val 3.1416' )
       val=dget('demo_val') ! get any values specified on -val option
       write(*,*)val         ! print the value
     end program demo_dget
