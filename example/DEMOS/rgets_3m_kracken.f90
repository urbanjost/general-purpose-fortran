          program demo_rgets
          use M_kracken, only: kracken, rgets
          implicit none
          real,allocatable  :: val(:)
          integer           :: i
            ! define command arguments and parse user command
            call kracken('fc','-F -C' )

            ! get any values specified on -C option
            val=rgets('fc_C')
            ! test if have something to print in C ==> F table
            if(size(val).gt.0)then
               ! print the requested values
               write(*,'(a,t14,a)')'celsius','fahrenheit'
               write(*,'(f5.1,t14,f5.1)')( val(i),(val(i)+40.0)*9.0/5.0 - 40.0,i=1,size(val))
            endif

            val=rgets('fc_F')
            ! check for values on -F
            if(size(val).gt.0)then
               write(*,'(a,t14,a)') 'fahrenheit', 'celsius'
               write(*,'(f5.1,t14,f5.1)')(val(i),(val(i)+40.0)*5.0/9.0 - 40.0,i=1,size(val))
            endif
          end program demo_rgets
