module M_test_suite_M_overload
use, intrinsic :: iso_fortran_env, only : integer_kinds, int8, int16, int32, int64
use, intrinsic :: iso_fortran_env, only : real32, real64, real128
use M_msg
use M_verify, only : unit_check_command, unit_check_keep_going, unit_check_level
use M_overload
private
public test_suite_m_overload
contains
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
! BUG REPORTS: https://gcc.gnu.org/bugzilla/show_bug.cgi?id=103782
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine test_suite_M_overload()
use M_verify,                 only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg
use M_verify,                 only : unit_check_level
use M_verify,                 only : almost
!use M_compare_float_numbers, only : operator(.EqualTo.)
implicit none
character(len=:),allocatable :: cmd

!! setup
   unit_check_level=5
   if(command_argument_count().eq.0)then
      call test_boolean_equal()
      call test_boolean_notequal()
      call test_dble_s2v()
      call test_dbles_s2v()
      call test_int_s2v()
      call test_ints_s2v()
      call test_real_s2v()
      call test_reals_s2v()
      call test_sign()
!$!      call get_command(cmd,realloc=.true.)
!$!      cmd=cmd//' -x -y "hello there" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
!$!      call execute_command_line(cmd)
!$!   else
!$!      call test_get_command()
!$!      call test_get_command_argument()
   endif
!! teardown
contains
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_sign()
!use M_overload,              only : operator(==)
   call unit_check_start('sign',' &
         & -description "overload SIGN to take a single argument" &
         & -section 3  &
         & -library libGPF  &
         & -filename `pwd`/M_overload.FF &
         & -documentation y &
         &  -prep         y &
         &  -ccall        n &
         &  -archive      GPF.a &
         & ')
   ! add 0+ to avoid gfortran-11 bug
   call unit_check('sign',sign(10_int8).eq.1.and.sign(-10_int8).eq.-1,'sign(+-10_int8)',0+sign(10_int8),0+sign(-10_int8))
   call unit_check('sign',sign(10_int16).eq.1.and.sign(-10_int16).eq.-1,'sign(+-10_int16)',0+sign(10_int16),0+sign(-10_int16))
   call unit_check('sign',sign(10_int32).eq.1.and.sign(-10_int32).eq.-1,'sign(+-10_int32)',0+sign(10_int32),0+sign(-10_int32))
   call unit_check('sign',sign(10_int64).eq.1.and.sign(-10_int64).eq.-1,'sign(+-10_int64)',0+sign(10_int64),0+sign(-10_int64))
   call unit_check('sign',sign(10.0_real32).eq.1.and.sign(-10.0_real32).eq.-1,&
   & 'sign(+-10_real32)',0+sign(10.0_real32),0+sign(-10.0_real32))
   call unit_check('sign',0+sign(10.0_real64).eq.1.and.sign(-10.0_real64).eq.-1,&
   & 'sign(+-10_real64)',0+sign(10.0_real64),0+sign(-10.0_real64))
   call unit_check('sign',0+sign(10.0_real128).eq.1.and.0+sign(-10.0_real128).eq.-1,&
   & 'sign(+-10_real128)',0+sign(10.0_real128),0+sign(-10.0_real128))
   call unit_check_done('sign',msg='')
end subroutine test_sign
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_boolean_equal()
!use M_overload,              only : operator(==)
   call unit_check_start('boolean_equal',' &
         & -description "overload == to take LOGICAL arguments" &
         & -section 3  &
         & -library libGPF  &
         & -filename `pwd`/M_overload.FF &
         & -documentation y &
         &  -prep         y &
         &  -ccall        n &
         &  -archive      GPF.a &
         & ')
   call unit_check('boolean_equal',.true. == .true. ,'== works like .eqv. for LOGICAL values')
   call unit_check_done('boolean_equal',msg='')
end subroutine test_boolean_equal
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_boolean_notequal()
!use M_overload,              only : operator(/=)
   call unit_check_start('boolean_notequal',' &
         & -description "overload /= to take LOGICAL arguments" &
         & -section 3  &
         & -library libGPF  &
         & -filename `pwd`/M_overload.FF &
         & -documentation y &
         &  -prep         y &
         &  -ccall        n &
         &  -archive      GPF.a &
         & ')
   call unit_check('boolean_notequal', (.true. /= .false. ),'/= works like .neqv. for LOGICAL values')
   call unit_check_done('boolean_notequal',msg='')
end subroutine test_boolean_notequal
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_dble_s2v()
!use M_overload,              only : int, real, dble
   call unit_check_start('dble_s2v',' &
         & -description "overload dble() to take string arguments" &
         & -section 3  &
         & -library libGPF  &
         & -filename `pwd`/M_overload.FF &
         & -documentation y &
         &  -prep         y &
         &  -ccall        n &
         &  -archive      GPF.a &
         & ')
   if(dble('0.3570726221234567').eq. 0.3570726221234567d0)then
      call unit_check_good('dble_s2v')                                             ! string passed to dble
!   elseif(dble('0.3570726221234567') .EqualTo. 0.3570726221234567d0 )then
   elseif(abs(dble('0.3570726221234567') - 0.3570726221234567d0 ).lt.2*epsilon(0.0d0))then
      call unit_check_good('dble_s2v')                                             ! string passed to real but not exactly
   else
      call unit_check_bad('dble_s2v')                                              ! returned value not equal to expected value
   endif
end subroutine test_dble_s2v
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_dbles_s2v()
doubleprecision,allocatable :: vals(:)
   call unit_check_start('dbles_s2v',' &
         & -description "overload dble() to take string arguments" &
         & -section 3  &
         & -library libGPF  &
         & -filename `pwd`/M_overload.FF &
         & -documentation y &
         &  -prep         y &
         &  -ccall        n &
         &  -archive      GPF.a &
         & ')

   vals=dble(['10.0d0','20.0d0'])
   vals=vals-[10.0d0,20.0d0]
   
   if(all(vals.eq.0.0d0))then
      call unit_check_good('dbles_s2v')                                             ! string passed to dble
   elseif(all(vals.lt.2*epsilon(0.0d0)))then
      call unit_check_good('dbles_s2v')                                             ! string passed to real but not exactly
   else
      call unit_check_bad('dbles_s2v')                                              ! returned value not equal to expected value
   endif
!!   ENCOUNTERS GFORTRAN 10.3.0 BUG
!!   if(all(abs(dble(['10.0d0','20.0d0']).eq.[10.0d0,20.0d0])))then
!!      call unit_check_good('dbles_s2v')                                             ! string passed to dble
!!!   elseif(all(dble(['10.0d0','20.0d0']) .EqualTo. [10.0d0,20.0d0]))then
!!   elseif(all(abs(dble(['10.0d0','20.0d0']) - [10.0d0,20.0d0]).lt.2*epsilon(0.0d0)))then
!!      call unit_check_good('dbles_s2v')                                             ! string passed to real but not exactly
!!   else
!!      call unit_check_bad('dbles_s2v')                                              ! returned value not equal to expected value
!!   endif
end subroutine test_dbles_s2v
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_int_s2v()
   call unit_check_start('int_s2v',' &
         & -description "overload INT() to take string arguments" &
         & -section 3  &
         & -library libGPF  &
         & -filename `pwd`/M_overload.FF &
         & -documentation y &
         &  -prep         y &
         &  -ccall        n &
         &  -archive      GPF.a &
         & ')
   call unit_check('int_s2v',int('1234').eq.1234,'string passed to int')
   call unit_check_done('int_s2v',msg='')
end subroutine test_int_s2v
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_ints_s2v()
integer,allocatable :: ibug(:)
   call unit_check_start('ints_s2v',' &
         & -description "overload INT() to take string arguments" &
         & -section 3  &
         & -library libGPF  &
         & -filename `pwd`/M_overload.FF &
         & -documentation y &
         &  -prep         y &
         &  -ccall        n &
         &  -archive      GPF.a &
         & ')
   !!if(all(int(['100','200']).eq. [100,200]))then
   ibug=int(['100','200'])
   if(all(ibug.eq. [100,200]))then
      call unit_check_good('ints_s2v')                                             ! string passed to int
   else
      call unit_check_bad('ints_s2v')                                              ! returned value not equal to expected value
   endif
end subroutine test_ints_s2v
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_real_s2v()
   call unit_check_start('real_s2v','&
         & -description "overload REAL() to take string arguments" &
         & -section 3                    &
         & -library libGPF               &
         & -filename `pwd`/M_overload.FF &
         & -documentation y              &
         &  -prep         y              &
         &  -ccall        n              &
         &  -archive      GPF.a          &
         & ')
   if(REAL('0.357072622').eq. 0.357072622)then
      call unit_check_good('real_s2v')
   elseif(almost(real('0.357072622'), 0.357072622,7.0) )then
      call unit_check_good('real_s2v')                                          ! string passed to real but not exactly
   else
      call unit_check_bad('real_s2v')                                           ! returned value not equal to expected value
   endif
end subroutine test_real_s2v
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_reals_s2v()
real,allocatable :: rbug(:)
   call unit_check_start('reals_s2v','&
         & -description "overload REAL() to take string arguments" &
         & -section 3                    &
         & -library libGPF               &
         & -filename `pwd`/M_overload.FF &
         & -documentation y              &
         &  -prep         y              &
         &  -ccall        n              &
         &  -archive      GPF.a          &
         & ')
   rbug=real(['0.357072622','200.0      '])
   if(all(rbug.eq. [0.357072622,200.0]))then
      call unit_check_good('reals_s2v')                                             ! string passed to int
!   elseif(all(real(['0.357072622','200.0      ']) .EqualTo. [0.357072622,200.0]))then
   elseif(all(abs(real(['0.357072622','200.0      ']) - [0.357072622,200.0]).lt.2*epsilon(0.0d0)))then
      call unit_check_good('reals_s2v')                                             ! string passed to real but not exactly
   else
      call unit_check_bad('reals_s2v')                                              ! returned value not equal to expected value
   endif

end subroutine test_reals_s2v
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
!$!subroutine test_get_command()
!$!   call unit_check_start('get_command','&
!$!         & -description "overload GET_COMMAND() to take allocatable string" &
!$!         & -section 3                    &
!$!         & -library libGPF               &
!$!         & -filename `pwd`/M_overload.FF &
!$!         & -documentation y              &
!$!         &  -prep         y              &
!$!         &  -ccall        n              &
!$!         &  -archive      GPF.a          &
!$!         & ')
!$!   call unit_check_done('get_command',msg='')
!$!end subroutine test_get_command
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
!$!subroutine test_get_command_argument()
!$!integer                      :: istat, ilen, i
!$!character(len=:),allocatable :: argument, arguments
!$!character(len=10)            :: regular
!$!integer,allocatable          :: istats(:), ilens(:)
!$!! assuming called with cmd//' -x -y "hello there" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
!$!   call unit_check_start('get_command_argument','&
!$!    & -description "overload GET_command_argument() to take allocatable string" &
!$!    & -section 3                    &
!$!    & -library libGPF               &
!$!    & -filename `pwd`/M_overload.FF &
!$!    & -documentation y              &
!$!    &  -prep         y              &
!$!    &  -ccall        n              &
!$!    &  -archive      GPF.a          &
!$!    & ')
!$!   
!$!   ! unallocated allocatable variable and realloc=.true.
!$!   allocate(integer :: istats(0),ilens(0))
!$!   arguments=''
!$!   do i=1, command_argument_count()
!$!      call get_command_argument(i, argument, length=ilen, status=istat, realloc=.true.)
!$!      istats=[istats,istat]
!$!      ilens=[ilens,ilen]
!$!      arguments=arguments//argument//' '
!$!   enddo
!$!   write(*,*)'ISTATS=',istats
!$!   write(*,*)'ILENS=',ilens
!$!   write(*,*)'ARGUMENTS=',arguments
!$!   call unit_check('get_command_argument',all(istats.eq.0),'check if all status returns are zero')
!$!   call unit_check('get_command_argument',all(ilens.eq.[2,2,11,36]),'check lengths')
!$!   call unit_check('get_command_argument',arguments.eq.'-x -y hello there xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx')
!$!
!$!   ! allocated allocatable variable and realloc=.true.
!$!   if(allocated(istats))deallocate(istats)
!$!   if(allocated(ilens))deallocate(ilens)
!$!   if(allocated(argument))deallocate(argument)
!$!   allocate(integer :: istats(0),ilens(0))
!$!   argument='12345678901234'
!$!   arguments=''
!$!   do i=1, command_argument_count()
!$!      call get_command_argument(i, argument, length=ilen, status=istat, realloc=.true.)
!$!      istats=[istats,istat]
!$!      ilens=[ilens,ilen]
!$!      arguments=arguments//argument//' '
!$!   enddo
!$!   write(*,*)'ISTATS=',istats
!$!   write(*,*)'ILENS=',ilens
!$!   write(*,*)'ARGUMENTS=',arguments
!$!   call unit_check('get_command_argument',all(istats.eq.0),'check if all status returns are zero')
!$!   call unit_check('get_command_argument',all(ilens.eq.[2,2,11,36]),'check lengths')
!$!   call unit_check('get_command_argument',arguments.eq.'-x -y hello there xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx')
!$!
!$!   ! allocatable variable already allocated and no realloc=.true.
!$!   deallocate(istats,ilens,argument)
!$!   allocate(integer :: istats(0),ilens(0))
!$!   arguments=''
!$!   allocate(character(len=10) :: argument)
!$!   do i=1, command_argument_count()
!$!      call get_command_argument(i, argument, length=ilen, status=istat)
!$!      istats=[istats,istat]
!$!      ilens=[ilens,ilen]
!$!      arguments=arguments//argument//' '
!$!   enddo
!$!   write(*,*)'ISTATS=',istats
!$!   write(*,*)'ILENS=',ilens
!$!   write(*,*)'ARGUMENTS=',arguments
!$!   call unit_check('get_command_argument',all(istats.eq.[0,0,-1,-1]),'check if get truncations as expected')
!$!   call unit_check('get_command_argument',all(ilens.eq.[2,2,11,36]),'check lengths')
!$!   call unit_check('get_command_argument',arguments.eq.'-x         -y         hello ther xxxxxxxxxx')
!$!
!$!   ! regular
!$!   deallocate(istats,ilens)
!$!   allocate(integer :: istats(0),ilens(0))
!$!   arguments=''
!$!   do i=1, command_argument_count()
!$!      call get_command_argument(i, regular, length=ilen, status=istat)
!$!      istats=[istats,istat]
!$!      ilens=[ilens,ilen]
!$!      arguments=arguments//regular//' '
!$!   enddo
!$!   write(*,*)'ISTATS=',istats
!$!   write(*,*)'ILENS=',ilens
!$!   write(*,*)'ARGUMENTS=',arguments
!$!   call unit_check('get_command_argument',all(istats.eq.[0,0,-1,-1]),'check if get truncations as expected')
!$!   call unit_check('get_command_argument',all(ilens.eq.[2,2,11,36]),'check lengths')
!$!   call unit_check('get_command_argument',arguments.eq.'-x         -y         hello ther xxxxxxxxxx')
!$!
!$!   call unit_check_done('get_command_argument',msg='')
!$!end subroutine test_get_command_argument
!===================================================================================================================================
end subroutine test_suite_M_overload
end module M_test_suite_M_overload

program runtest
use M_msg
use M_verify, only : unit_check_command, unit_check_keep_going, unit_check_level, unit_check_stop
use M_test_suite_M_overload
implicit none
   unit_check_command=''
   unit_check_keep_going=.true.
   unit_check_level=0
   call test_suite_M_overload()
   call unit_check_stop()
end program runtest
