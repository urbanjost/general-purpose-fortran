      program demo_minloc
      implicit none
      integer,save :: ints(3,5)= reshape([&
         4, 10,  1,  7, 13, &
         9, 15,  6, 12,  3, &
        14,  5, 11,  2,  8  &
      ],shape(ints),order=[2,1])
         write(*,*) minloc(ints)
         write(*,*) minloc(ints,dim=1)
         write(*,*) minloc(ints,dim=2)
         ! where in each column is the smallest number .gt. 10 ?
         write(*,*) minloc(ints,dim=2,mask=ints.gt.10)
         ! a one-dimensional array with dim=1 explicitly listed returns a scalar
         write(*,*) minloc(pack(ints,.true.),dim=1) ! scalar
      end program demo_minloc
