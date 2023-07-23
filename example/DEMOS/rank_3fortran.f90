      program demo_rank
      implicit none

      ! a bunch of data objects to query
      integer           :: a
      real, allocatable :: b(:,:)
      real, pointer     :: c(:)
      complex           :: d

      ! make up a type
      type mytype
         integer :: int
         real :: float
         character :: char
      end type mytype
      type(mytype) :: any_thing(1,2,3,4,5)

        ! basics
         print *, 'rank of scalar a=',rank(a)
         ! you can query this array even though it is not allocated
         print *, 'rank of matrix b=',rank(b)
         print *, 'rank of vector pointer c=',rank(c)
         print *, 'rank of complex scalar d=',rank(d)

        ! you can query any type, not just intrinsics
         print *, 'rank of any arbitrary type=',rank(any_thing)

        ! an assumed-rank object may be queried
         call query_int(10)
         call query_int([20,30])
         call query_int( reshape([40,50,60,70],[2,2]) )

        ! you can even query an unlimited polymorphic entity
         call query_anything(10.0)
         call query_anything([.true.,.false.])
         call query_anything( reshape([40.0,50.0,60.0,70.0],[2,2]) )

      contains

      subroutine query_int(data_object)
      ! It is hard to do much with something dimensioned
      ! name(..) if not calling C except inside of a
      ! SELECT_RANK construct but one thing you can
      ! do is call the inquiry functions ...
      integer,intent(in) :: data_object(..)
      character(len=*),parameter :: all='(*(g0,1x))'

         if(rank(data_object).eq.0)then
            print all,&
            & 'passed a scalar to an assumed rank,  &
            & rank=',rank(data_object)
         else
            print all,&
            & 'passed an array to an assumed rank,  &
            & rank=',rank(data_object)
         endif

      end subroutine query_int

      subroutine query_anything(data_object)
      class(*),intent(in) ::data_object(..)
      character(len=*),parameter :: all='(*(g0,1x))'
        if(rank(data_object).eq.0)then
          print all,&
          &'passed a scalar to an unlimited polymorphic rank=', &
          & rank(data_object)
        else
          print all,&
          & 'passed an array to an unlimited polymorphic, rank=', &
          & rank(data_object)
        endif
      end subroutine query_anything

      end program demo_rank
