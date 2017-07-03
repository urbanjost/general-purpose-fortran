!-------------------------------------------------------------------------------
SUBROUTINE init_random_seed_by_clock()
   INTEGER :: i, n, clock
   INTEGER, DIMENSION(:), ALLOCATABLE :: seed
   CALL RANDOM_SEED(size = n)
   ALLOCATE(seed(n))
   CALL SYSTEM_CLOCK(COUNT=clock)
   seed = clock + 37 * (/ (i - 1, i = 1, n) /)
!   write(*,*)seed
!   write(*,*)(/ (i - 1, i = 1, n) /)
   CALL RANDOM_SEED(PUT = seed)

   DEALLOCATE(seed)
END SUBROUTINE init_random_seed_by_clock
!-------------------------------------------------------------------------------
! This subroutine is from a post on comp.lang.fortran.
! it also uses the time to initialize the sequence
subroutine init_seed()
  integer :: n, ival(8), v(3), i
  integer, allocatable :: seed(:)
  call date_and_time(values=ival)
  v(1) = ival(8) + 2048*ival(7)
  v(2) = ival(6) + 64*ival(5)     ! value(4) isn't really 'random'
  v(3) = ival(3) + 32*ival(2) + 32*8*ival(1)
  call random_seed(size=n)
  allocate(seed(n))
  call random_seed()   ! Give the seed an implementation-dependent kick
  call random_seed(get=seed)
  do i=1, n
     seed(i) = seed(i) + v(mod(i-1, 3) + 1)
  enddo
  call random_seed(put=seed)
  deallocate(seed)
end subroutine
!-------------------------------------------------------------------------------
! to make this start with a single number like srand(3c) take the seed and
! use the value to fill the seed array, adding 37 to each subsequent value
! till the array is filled.
SUBROUTINE init_random_seed(mine)
   implicit none
   integer,intent(in) :: mine
   INTEGER :: i, n
   INTEGER, DIMENSION(:), ALLOCATABLE :: seed
   CALL RANDOM_SEED(size = n)
   ALLOCATE(seed(n))
   seed = mine + 37 * (/ (i - 1, i = 1, n) /)
!   write(*,*)seed
!   write(*,*)(/ (i - 1, i = 1, n) /)
   CALL RANDOM_SEED(PUT = seed)
   DEALLOCATE(seed)
END SUBROUTINE init_random_seed
!-------------------------------------------------------------------------------
