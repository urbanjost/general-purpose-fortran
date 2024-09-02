      program demo_allocated
      use,intrinsic :: iso_fortran_env, only : dp=>real64,sp=>real32
      implicit none
      real(kind=sp), allocatable :: x(:)
      character(len=256) :: message
      integer :: istat
        ! basics
         if( allocated(x)) then
             write(*,*)'do things if allocated'
         else
             write(*,*)'do things if not allocated'
         endif

         ! if already allocated, deallocate
         if ( allocated(x) ) deallocate(x,STAT=istat, ERRMSG=message )
         if(istat.ne.0)then
            write(*,*)trim(message)
            stop
         endif

         ! only if not allocated, allocate
         if ( .not. allocated(x) ) allocate(x(20))

        ! allocation and intent(out)
         call intentout(x)
         write(*,*)'note it is deallocated!',allocated(x)

         contains

         subroutine intentout(arr)
         ! note that if arr has intent(out) and is allocatable,
         ! arr is deallocated on entry
         real(kind=sp),intent(out),allocatable :: arr(:)
             write(*,*)'note it was allocated in calling program',allocated(arr)
         end subroutine intentout

      end program demo_allocated
