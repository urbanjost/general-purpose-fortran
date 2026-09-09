     program demo_zo
     use M_overload, only: zo, zo, lt, le, eq, ne, gt, ge
     implicit none
     write (*, *) zo(10 < 20)
     if (sum(zo([1 > 2, 3 == 4, 10 < 5, 100 > 50])) > 2) then
        write (*, *) 'two or more are not true'
     endif
     end program demo_zo
