module M_test_suite_M_random
use M_framework__msg
use M_framework__verify, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg
use M_framework__verify, only : unit_check_level
use M_random
private
public test_suite
contains
subroutine test_suite()

!*! setup

   call test_init_random_seed()
   call test_init_random_seed_by_dat()
   call test_init_random_seed_by_system_clock()

   call test_random_kiss64()

   call test_random_hex()
   call test_random_string()
   call test_random_int()

   call test_scramble()

   call test_mtprng_init()
   call test_mtprng_init_by_array()
   call test_mtprng_rand()
   call test_mtprng_rand64()
   call test_mtprng_rand_range()
   call test_mtprng_rand_real1()
   call test_mtprng_rand_real2()
   call test_mtprng_rand_real3()

!*! teardown
contains
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_init_random_seed()

integer :: i
real    :: x
intrinsic random_number
   call unit_check_start('init_random_seed',msg='')
   call init_random_seed_by_dat()
   do i=1,10
      ! assigned pseudo-random numbers from the uniform distribution in the interval 0 <= x < 1.
      call random_number(x)
      if(unit_check_level.gt.0)then
         write(*,'(g0,1x)',advance='no')x
      endif
   enddo
   !*!call unit_check('init_random_seed', 0.eq.0, 'checking',100)
   call unit_check_done('init_random_seed',msg='')
end subroutine test_init_random_seed
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_init_random_seed_by_dat()

integer :: i
real    :: x
intrinsic random_number
   call unit_check_start('init_random_seed_by_dat',msg='')
   call init_random_seed_by_dat()
   do i=1,10
      ! assigned pseudo-random numbers from the uniform distribution in the interval 0 <= x < 1.
      call random_number(x)
      if(unit_check_level.gt.0)then
         write(*,'(g0,1x)',advance='no')x
      endif
   enddo
   !*!call unit_check('init_random_seed_by_dat', 0.eq.0, 'checking',100)
   call unit_check_done('init_random_seed_by_dat',msg='')
end subroutine test_init_random_seed_by_dat
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_init_random_seed_by_system_clock()

integer :: i
real    :: x
intrinsic random_number
   call unit_check_start('init_random_seed_by_system_clock',msg='')
   call init_random_seed_by_system_clock()
   do i=1,10
      ! assigned pseudo-random numbers from the uniform distribution in the interval 0 <= x < 1.
      call random_number(x)
      if(unit_check_level.gt.0)then
         write(*,'(g0,1x)',advance='no')x
      endif
   enddo
   !*!call unit_check('init_random_seed_by_system_clock', 0.eq.0, 'checking',100)
   call unit_check_done('init_random_seed_by_system_clock',msg='')
end subroutine test_init_random_seed_by_system_clock
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mtprng_init()

   call unit_check_start('mtprng_init',msg='')
   !*!call unit_check('mtprng_init', 0.eq.0, 'checking',100)
   call unit_check_done('mtprng_init',msg='')
end subroutine test_mtprng_init
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mtprng_init_by_array()

   call unit_check_start('mtprng_init_by_array',msg='')
   !*!call unit_check('mtprng_init_by_array', 0.eq.0, 'checking',100)
   call unit_check_done('mtprng_init_by_array',msg='')
end subroutine test_mtprng_init_by_array
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mtprng_rand()

   call unit_check_start('mtprng_rand',msg='')
   !*!call unit_check('mtprng_rand', 0.eq.0, 'checking',100)
   call unit_check_done('mtprng_rand',msg='')
end subroutine test_mtprng_rand
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mtprng_rand64()

!use M_random, only : mtprng_state, mtprng_init, mtprng_rand64, mtprng_rand_real1
use, intrinsic :: iso_fortran_env, only : int8, int16, int32, int64  !  1 2 4 8
implicit none

integer(INT32)     :: seed
type(mtprng_state) :: state
integer            :: i, misses
integer(INT64)     :: known(100), hop
double precision   :: known_double
logical            :: success, success_double

known = (/ &
   1062284148_INT64  ,  1840134109_INT64  ,  3636226446_INT64  ,   887288380_INT64  ,  3889928508_INT64  ,  &
    631642018_INT64  ,   914101460_INT64  ,  3162600344_INT64  ,  4279173875_INT64  ,  3888840898_INT64  ,  &
   2663878183_INT64  ,  2252856790_INT64  ,  3463027370_INT64  ,  1097057726_INT64  ,  3184139039_INT64  ,  &
   1434652676_INT64  ,  3403152620_INT64  ,   833869435_INT64  ,   612581734_INT64  ,   576299959_INT64  ,  &
   1474687423_INT64  ,  3466262180_INT64  ,  3133945242_INT64  ,  2677293441_INT64  ,  3505646298_INT64  ,  &
   1339531483_INT64  ,  3669763788_INT64  ,   510452911_INT64  ,   559989931_INT64  ,  2308431220_INT64  ,  &
   3603446463_INT64  ,   667203617_INT64  ,  1127428958_INT64  ,  1290717533_INT64  ,  3513405708_INT64  ,  &
    927031876_INT64  ,  4032539667_INT64  ,  3825851657_INT64  ,  2014122148_INT64  ,  3946341433_INT64  ,  &
   2489727744_INT64  ,  3391425033_INT64  ,  1283015508_INT64  ,   913542342_INT64  ,   764165363_INT64  ,  &
   3945892851_INT64  ,  2442690715_INT64  ,  1067765245_INT64  ,  2537181202_INT64  ,  3064480631_INT64  ,  &
   3101723380_INT64  ,   802526031_INT64  ,  3157511722_INT64  ,  3269291235_INT64  ,  4215855479_INT64  ,  &
    181260300_INT64  ,  3744061959_INT64  ,  1143883471_INT64  ,    53781702_INT64  ,   882249490_INT64  ,  &
    321985400_INT64  ,   577208133_INT64  ,   885895688_INT64  ,  2725767908_INT64  ,  2231810848_INT64  ,  &
   1980748708_INT64  ,  3886306640_INT64  ,   202325230_INT64  ,  3043114780_INT64  ,  2021505937_INT64  ,  &
   1074135674_INT64  ,   753749191_INT64  ,  3155169167_INT64  ,  2997245061_INT64  ,  1203929572_INT64  ,  &
   2247630318_INT64  ,  1571401504_INT64  ,  4114645852_INT64  ,  3048726454_INT64  ,  3912566191_INT64  ,  &
    546592897_INT64  ,  3352654755_INT64  ,  3632644786_INT64  ,  3691031383_INT64  ,  1267283456_INT64  ,  &
    900236841_INT64  ,    47733538_INT64  ,  2049551875_INT64  ,   668453652_INT64  ,  2379406100_INT64  ,  &
    689049113_INT64  ,   759943852_INT64  ,   623176051_INT64  ,  1348915232_INT64  ,  1345348561_INT64  ,  &
   2055844659_INT64  ,  3678167085_INT64  ,  1856143658_INT64  ,  2464849813_INT64  ,    28304277_INT64    /)  

  known_double = 0.96673825731657869742d0
  seed = 6928

  call unit_check_start('mtprng_rand64',msg='')
  call mtprng_init(seed, state)

  misses = 0
  success=.true.
  do i=1,100
     hop = mtprng_rand64(state)
     if (hop.ne.known(i)) misses=misses+1
  enddo
  if (misses>0) success=.false.

! retourne un reel IEEE64, peut  tre utilis  en double precision
  if ( abs(mtprng_rand_real1(state)-known_double) > 5d-16 ) then
     success_double = .false.
  else
     success_double = .true.
  endif

  call unit_check('mtprng_rand64', success,        'integer misses',misses)
  call unit_check('mtprng_rand64', success_double, 'double precision')

  call unit_check_done('mtprng_rand64',msg='')
end subroutine test_mtprng_rand64
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mtprng_rand_range()

   call unit_check_start('mtprng_rand_range',msg='')
   !*!call unit_check('mtprng_rand_range', 0.eq.0, 'checking',100)
   call unit_check_done('mtprng_rand_range',msg='')
end subroutine test_mtprng_rand_range
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mtprng_rand_real1()

   call unit_check_start('mtprng_rand_real1',msg='')
   !*!call unit_check('mtprng_rand_real1', 0.eq.0, 'checking',100)
   call unit_check_done('mtprng_rand_real1',msg='')
end subroutine test_mtprng_rand_real1
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mtprng_rand_real2()

   call unit_check_start('mtprng_rand_real2',msg='')
   !*!call unit_check('mtprng_rand_real2', 0.eq.0, 'checking',100)
   call unit_check_done('mtprng_rand_real2',msg='')
end subroutine test_mtprng_rand_real2
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mtprng_rand_real3()

   call unit_check_start('mtprng_rand_real3',msg='')
   !*!call unit_check('mtprng_rand_real3', 0.eq.0, 'checking',100)
   call unit_check_done('mtprng_rand_real3',msg='')
end subroutine test_mtprng_rand_real3
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_random_int()

   call unit_check_start('random_int',msg='')
   !*!call unit_check('random_int', 0.eq.0, 'checking',100)
   call unit_check_done('random_int',msg='')
end subroutine test_random_int
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_random_hex()

integer :: i
intrinsic random_number
   call unit_check_start('random_hex',msg='')
   call init_random_seed(218595421)
   do i=1,2
      write(*,*)random_hex(32)
      write(*,*)random_hex(64)
      write(*,*)random_hex(128)
      write(*,*)random_hex(256)
      write(*,*)random_hex(512)
   enddo
   !*!call unit_check('random_hex', 0.eq.0, 'checking',100)
   call unit_check_done('random_hex',msg='')
end subroutine test_random_hex
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_random_kiss64()

integer, parameter    :: i8b = selected_int_kind(18)  ! eight-byte integer
integer(i8b)          :: i, t
   call unit_check_start('random_kiss64',msg='')
   do i = 1, 100000000
      t = random_kiss64()
      if(unit_check_level.gt.0)then
         if(mod(i,1000000_i8b+1_i8b)==1000000_i8b)write(*,*)i,' T=',T
      endif
   enddo
 !*!call unit_check('random_kiss64',t.eq.1666297717051644203_i8b,'100 million calls to KILL',t,16662977170511644203_i8b)
   call unit_check('random_kiss64',t.eq.1666297717051644203_i8b,msg='100 million calls to KILL')
   call unit_check_done('random_kiss64',msg='')
end subroutine test_random_kiss64
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_scramble()

   call unit_check_start('scramble',msg='')
   !*!call unit_check('scramble', 0.eq.0, 'checking',100)
   call unit_check_done('scramble',msg='')
end subroutine test_scramble
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_random_string()

intrinsic random_number
integer           :: i
integer,parameter :: tests=100
character(len=8)  :: alpha(tests)
integer           :: repeats
   repeats=0
   call unit_check_start('random_string',msg='')
   call init_random_seed(218595421)
   do i=1,tests
      alpha(i)=random_string('ABCDEFGHIJKLMNOPQRSTUVWXYZ',8)
   enddo
   do i=1,tests
   if(.not.random_string('ABCDEFGHIJKLMNOPQRSTUVWXYZ',8).eq.alpha(i))then
      repeats=repeats+1
   endif
   enddo
   call unit_check('random_string', repeats.eq.tests,'test if repeats with same seed',tests)
   call unit_check_done('random_string',msg='')
end subroutine test_random_string
!===================================================================================================================================
end subroutine test_suite
!==================================================================================================================================!
end module M_test_suite_M_random
!==================================================================================================================================!
program runtest
use M_framework__msg
use M_framework__verify, only : unit_check_level, unit_check_stop
use M_test_suite_M_random
implicit none
   unit_check_level=0
   call test_suite()
   call unit_check_stop()
end program runtest
!==================================================================================================================================!
