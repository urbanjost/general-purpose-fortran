           program demo_maxval
           implicit none
           integer,save :: ints(3,5)= reshape([&
              1,  2,  3,  4,  5, &
             10, 20, 30, 40, 50, &
             11, 22, 33, 44, 55  &
           ],shape(ints),order=[2,1])
           write(*,*) maxval(ints)
           write(*,*) maxval(ints,dim=1)
           write(*,*) maxval(ints,dim=2)
           ! find biggest number less than 30 with mask
           write(*,*) maxval(ints,mask=ints.lt.30)
           end program demo_maxval
