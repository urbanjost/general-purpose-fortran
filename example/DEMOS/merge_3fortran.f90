      program demo_merge
      implicit none
      integer :: tvals(2,3), fvals(2,3), answer(2,3)
      logical :: mask(2,3)
      integer :: i
      integer :: k
      logical :: chooseleft
      logical :: maybe

         ! Works with scalars
         k=5
         write(*,*)merge (1.0, 0.0, k > 0)
         k=-2
         write(*,*)merge (1.0, 0.0, k > 0)

         ! note for scalar logicals calls such as
         maybe = merge (.true.,.false., k > 0)
         ! are simply the same as
         if (k > 0)then
            maybe=.true.
         else
            maybe=.false.
         endif
         ! but even more succinctly, and array-compatible, is
         maybe = k > 0

         ! set up some simple arrays that all conform to the
         ! same shape
         tvals(1,:)=[  10, -60,  50 ]
         tvals(2,:)=[ -20,  40, -60 ]

         fvals(1,:)=[ 0, 3, 2 ]
         fvals(2,:)=[ 7, 4, 8 ]

         mask(1,:)=[ .true.,  .false., .true. ]
         mask(2,:)=[ .false., .false., .true. ]

         ! lets use the mask of specific values
         write(*,*)'mask of logicals'
         answer=merge( tvals, fvals, mask )
         call printme()

         ! more typically the mask is an expression
         write(*, *)'highest values'
         answer=merge( tvals, fvals, tvals > fvals )
         call printme()

         write(*, *)'lowest values'
         answer=merge( tvals, fvals, tvals < fvals )
         call printme()

         write(*, *)'zero out negative values'
         answer=merge( 0, tvals, tvals < 0)
         call printme()

         write(*, *)'binary choice'
         chooseleft=.false.
         write(*, '(3i4)')merge([1,2,3],[10,20,30],chooseleft)
         chooseleft=.true.
         write(*, '(3i4)')merge([1,2,3],[10,20,30],chooseleft)

      contains

      subroutine printme()
            write(*, '(3i4)')(answer(i, :), i=1, size(answer, dim=1))
      end subroutine printme

      end program demo_merge
