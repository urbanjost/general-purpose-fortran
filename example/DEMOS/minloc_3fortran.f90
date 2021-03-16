           program demo_minloc
           implicit none
           integer,save :: ints(3,5)= reshape([&
              1,  2,  3,  4,  5, &
             10, 20, 30, 40, 50, &
             11, 22, 33, 44, 55  &
           ],shape(ints),order=[2,1])
           write(*,*) minloc(ints)
           write(*,*) minloc(ints,dim=1)
           write(*,*) minloc(ints,dim=2)
           write(*,*) minloc(pack(ints,.true.),dim=1)
           end program demo_minloc
