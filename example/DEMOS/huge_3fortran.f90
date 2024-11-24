      program demo_huge
      implicit none
      character(len=*),parameter :: f='(i2,1x,2(i11,1x),f14.0:,1x,l1,1x,a)'
      integer                    :: i, j, k, biggest
      real                       :: v, w
      doubleprecision            :: tally
         ! basic
         print *, huge(0), huge(0.0), huge(0.0d0)
         print *, tiny(0.0), tiny(0.0d0)

         tally=0.0d0
         ! note subtracting one because counter is the end value+1 on exit
         do i=0,huge(0)-1
            tally=tally+i
         enddo
         write(*,*)'tally=',tally

         ! advanced
         biggest=huge(0)
         ! be careful of overflow when using integers in computation
         do i=1,14
            j=6**i   ! Danger, Danger
            w=6**i   ! Danger, Danger
            v=6.0**i
            k=v      ! Danger, Danger

            if(v.gt.biggest)then
               write(*,f) i, j, k, v, v.eq.w, 'wrong j and k and w'
            else
               write(*,f) i, j, k, v, v.eq.w
            endif
         enddo
         ! a simple check of the product of two 32-bit integers
         print *,checkprod([2,4,5,8],[10000,20000,3000000,400000000])

      contains
      impure elemental function checkprod(i,j) result(ij32)
      ! checkprod(3f) - check for overflow when multiplying 32-bit integers
      use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64
      integer(kind=int32),intent(in)  :: i, j
      integer(kind=int64)             :: ij64
      integer(kind=int32)             :: ij32
      integer,parameter               :: toobig=huge(0_int32)
      character(len=80)               :: message
         ij64=int(i,kind=int64)*int(j,kind=int64)
         if(ij64.gt.toobig)then
            write(message,'(*(g0))')&
            & '<ERROR>checkprod(3f):',i,'*',j,'=',ij64,'>',toobig
            stop message
         else
            ij32=ij64
         endif
      end function checkprod
      end program demo_huge
