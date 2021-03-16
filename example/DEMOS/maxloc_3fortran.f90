           program demo_maxloc
           implicit none
           integer,save :: ints(3,5)= reshape([&
              1,  2,  3,  4,  5, &
             10, 20, 30, 40, 50, &
             11, 22, 33, 44, 55  &
           ],shape(ints),order=[2,1])
           write(*,*) maxloc(ints)
           write(*,*) maxloc(ints,dim=1)
           write(*,*) maxloc(ints,dim=2)
           end program demo_maxloc
