     program demo_merge
     ! allow strings of different length on merge
     use M_overload, only : merge
     implicit none
     character(len=*), parameter :: gen='(*("[",g0,"]":,","))'

        write(*,gen)merge('a','bbbbb',1.eq.1)
        write(*,gen)merge('a','bbbbb',1.eq.2)
        write(*,gen)merge(['a','b'],['bbbbb','ccccc'],1.eq.2)

     end program demo_merge
