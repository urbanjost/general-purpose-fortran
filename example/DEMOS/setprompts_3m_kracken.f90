           program demo_setprompts
           use M_kracken, only : kracken,iget,rget,sget,setprompts
           implicit none

           call setprompts('demo', ' -int My INTEGER value  -float My REAL value  -str My CHARACTER value')
           call kracken(   'demo', ' -int 100 -float 123.456 -str DEFAULT')
           write(*,'(a,i0)')'INTEGER IS ',iget('demo_int')
           write(*,'(a,g0)')'REAL IS ',rget('demo_float')
           write(*,'(a,a)')'STRING IS '//trim(sget('demo_str'))

           end program demo_setprompts
