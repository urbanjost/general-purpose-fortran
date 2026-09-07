     program demo_oz
     use M_overload, only: bool, zo, lt, le, eq, ne, gt, ge
     implicit none
        write (*, *) 'is 10 < 20 ?', bool(10 < 20)
        write (*, *) 'elemental', bool([2 > 1, 3 == 4, 10 < 5, 100 > 50])
        if (sum(bool([2 > 1, 3 == 4, 10 < 5, 100 > 50])) >= 2) then
           write (*, *) 'two or more are true'
        endif
     end program demo_oz
