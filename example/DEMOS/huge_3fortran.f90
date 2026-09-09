      program demo_huge
      use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64
      use,intrinsic :: iso_fortran_env, only : real32,real64,real128
      implicit none
      character(len=*),parameter :: f='(i2,1x,2(i11,1x),f14.0:,1x,l1,1x,a)'
      integer                    :: i, j, k, biggest
      real                       :: v, w
      integer,allocatable        :: undef(:,:,:)

         print *,'basics:'

         print *, huge(0), huge(0.0), huge(0.0d0)
         print *, tiny(0.0), tiny(0.0d0)
         print *, 'an array argument returns a scalar'
         print *, huge([10_int8,20_int8,30_int8])
         print *, 'the value of the argument does not matter, it does not'
         print *, 'even need to be allocated, just the type and kind are'
         print *, 'used',huge(1000),huge(-654321),huge(undef)

         print *, 'dusty corners:'

         print *, 'Perhaps instead of an "infinite" loop you want to make'
         print *, 'a very large one so you have a counter handy.'
         do i=1,huge(0)-1
            call random_number(w)
            if(w > 0.9999999)exit
         enddo
         write(*,*)'exited with counter=',i
         ! use huge(0)-1 not huge(0) because when a loop terminates normally
         ! the counter is set to the last value + 1. If the loop reached
         ! i=huge(0) adding 1 would cause an overflow!

         ! Can HUGE(1.d0) be accurately formatted?
         print '(E330.320)', huge(1.d0)
         print *, huge(1.d0)
         print '(g0)', huge(1.d0)

         print *,'ranges for signed numbers  are symmetrical so if HUGE(0.0)'
         print *,'is a valid number  so is -HUGE(0.0).'
         print *, huge(0.0),-huge(0.0), huge(0.0)-huge(0.0)
         print *,'but for 2''s-complement whole numbers -1-huge(0)='
         print *,  -1-huge(0)
         print *,'is a valid number too, but huge(0)+1 will cause an overflow!'
         print *,'Almost all computers use 2''s-complement integers now-adays.'
         print *,'so -huge(0)-1 is often used as a "magic number" to designate'
         print *,'invalid whole numbers, as INTEGER types do not have a Nan'
         print *,'or Infinite value like floats do if it is not a "possible"'
         print *,'value for a computation.'
         print *
         print *,'for a single byte a value can be from -128 to 127 so maybe'
         print *,'-128 is not unlikely to be used though, for example:'
         print *,'range of a 2''scomplement one-byte kind is',-huge(0_int8)-1,&
               & 'to',huge(0_int8)
         print *,'so there is no "perfect" integer value to represent an '
         print *,'invalid number except on a case-by-case basis.'

         print *,'advanced:'

         print *,'be careful of overflow; Fortran is not required to report it'
         print *,'See OUT_OF_RANGE(3) for information on detecting overflows.'

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
