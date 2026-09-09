      program demo_maxloc
      implicit none
      integer      :: ii
      integer,save :: i(-3:3)=[(abs(abs(ii)-50),ii=-3,3)]
      integer,save :: ints(3,5)= reshape([&
         1,  2,  3,  4,  5, &
         10, 20, 30, 40, 50, &
         11, 22, 33, 44, 55  &
      ],shape(ints),order=[2,1])

          write(*,*) maxloc(ints)
          write(*,*) maxloc(ints,dim=1)
          write(*,*) maxloc(ints,dim=2)
          ! when array bounds do not start with one remember MAXLOC(3) returns
          ! the offset relative to the lower bound-1 of the location of the
          ! maximum value, not the subscript of the maximum value. When the
          ! lower bound of the array is one, these values are the same. In
          ! other words, MAXLOC(3) returns the subscript of the value assuming
          ! the first subscript of the array is one no matter what the lower
          ! bound of the subscript actually is.
          write(*,'(g0,1x,g0)') (ii,i(ii),ii=lbound(i,dim=1),ubound(i,dim=1))
          write(*,*)maxloc(i)

      end program demo_maxloc
