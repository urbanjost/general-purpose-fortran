     program demo_dgets
     use M_kracken, only: kracken, dgets
     implicit none
     doubleprecision,allocatable  :: vals(:)
     integer :: i
     ! define command arguments and parse user command
     call kracken('demo','-nums 1 2 3 1000 100,000 11.11111 77.77777 -77.7777' )
     vals=dgets('demo_nums') ! get any values specified for -nums
     write(*,'(*(g0:,","))')( vals(i),i=1,size(vals)) ! print the values
     end program demo_dgets
