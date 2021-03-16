           program demo_rnum0
           use M_calculator, only : rnum0
           x=rnum0('20/3.4')
           y=rnum0('CI = 10 * sin(3.1416/4)')
           z=rnum0('CI')
           write(*,*)x,y,z
           end program demo_rnum0
