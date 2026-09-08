     program demo_bool
     use M_sets, only: bool
     implicit none
        write (*, *) 'is 10 < 20 ?', bool(10 < 20)
        write (*, *) 'elemental', bool([2 > 1, 3 == 4, 10 < 5, 100 > 50])
        if (sum(bool([2 > 1, 3 == 4, 10 < 5, 100 > 50])) >= 2) then
           write (*, *) 'two or more are true'
        endif
     end program demo_bool
