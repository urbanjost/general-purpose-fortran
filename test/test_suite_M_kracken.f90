!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
program test_suite_M_kracken
use, intrinsic :: iso_fortran_env, only : ERROR_UNIT
use,intrinsic :: IEEE_ARITHMETIC, only : IEEE_IS_NAN       ! Determine if value is IEEE Not-a-Number.
!!use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64, real32, real64, real128
use :: M_verify,   only : unit_check, unit_check_good, unit_check_bad, unit_check_done, unit_check_start, unit_check_level
use :: M_verify,   only : unit_check_command, unit_check_keep_going, unit_check_level
use :: M_verify,   only : almost
use :: M_kracken
implicit none
integer,parameter :: HT=9
unit_check_command=''
unit_check_keep_going=.true.
unit_check_level=0
call unit_check_start('M_kracken')
!! setup
call test_dget()
call test_dgets()
call test_iget()
call test_igets()
call test_lget()
call test_lgets()
call test_rget()
call test_rgets()
call test_sget()
call test_sgets()
call test_store()
call test_retrev()
call test_parse()
call test_dissect()
   call test_kracken()
   call test_setprompts()
   call test_show()
!! teardown
contains
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_dget()
integer         :: ier
doubleprecision :: dd=huge(0.0d0)*0.999999999999d0
   call unit_check_start('dget',msg=' direct tests of dget(3f)')
   call store('MY_DOUBLE1',dd,'define',ier)
   call store('MY_DOUBLE2',-1234.0d-20,'define',ier)
   call store('BAD','nN3.3','define',ier)
   call store('BLANK',' ','define',ier)
   call unit_check('dget', almost(dget('MY_DOUBLE1'),dd,15),  'MY_DOUBLE1',dget('MY_DOUBLE1'),'versus',dd)
   call unit_check('dget', dget('MY_DOUBLE2').eq.-1234.0d-20, 'MY_DOUBLE2',dget('MY_DOUBLE2'))
   call unit_check('dget', dget('NOTTHERE').eq.0,             'NOTTHERE',dget('NOTTHERE'))
   call unit_check('dget', ieee_is_nan(dget('BAD')),          'BAD',dget('BAD'))
   call unit_check('dget', dget('BLANK').eq.0,                'BLANK',dget('BLANK'))
   call unit_check_done('dget',msg='')
end subroutine test_dget
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_dgets()
integer                     :: ier
doubleprecision,allocatable :: d(:)
   call unit_check_start('dgets',msg=' direct tests of dgets(3f)')
   call store('MY_DOUBLE1','100.0d0 0.0d0 300.33333d2','define',ier)
   call store('MY_DOUBLE2',-1234.0d-20,'define',ier)
   call store('BAD','sfs','define',ier)
   call store('BLANK',' ','define',ier)
   call unit_check('dgets', all(dgets('MY_DOUBLE1').eq.[100.0d0,0.0d0,300.33333d2]),'MY_DOUBLE1')
   call unit_check('dgets', all(dgets('MY_DOUBLE2').eq.[-1234.0d-20]),              'MY_DOUBLE2')
   call unit_check('dgets', size(dgets('NOTTHERE')).eq.0,                           'NOTTHERE')
   d=dgets('BAD')
   if(size(d).gt.0)then
      call unit_check('dgets', size(d).eq.1.and.ieee_is_nan(d(1)),                  'BAD IS NAN',size(d))
   else
      call unit_check('dgets', size(d).eq.1,                                        'BAD HAS SIZE 1',size(d))
   endif
   call unit_check('dgets', size(dgets('BLANK')).eq.0,                              'BLANK')
   call unit_check_done('dgets',msg='')
end subroutine test_dgets
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_dissect()
integer :: ierr
   call dissect('demo',' -int 1000 -float 1234.567 -str CHARACTER value','-int 456 -float 50.00 ',ierr)
   call unit_check_start('dissect',msg='')
   call unit_check('dissect', iget('demo_int').eq.456, 'demo_int',iget('demo_int'))
   call unit_check('dissect', rget('demo_float').eq.50.0, 'demo_float',rget('demo_float'))
   call unit_check('dissect', sget('demo_str').eq.'CHARACTER value', 'demo_str',sget('demo_str'))
   call unit_check_done('dissect',msg='')
end subroutine test_dissect
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_iget()
integer        :: ier
   call unit_check_start('iget',msg=' direct tests of iget(3f)')
   call store('MY_INTEGER1',1234,'define',ier)
   call store('MY_INTEGER2',-1234,'define',ier)
   call store('BAD','3z4j','define',ier)
   call store('BLANK',' ','define',ier)
   call unit_check('iget', iget('MY_INTEGER1').eq.1234,  'MY_INTEGER1',iget('MY_INTEGER1'))
   call unit_check('iget', iget('MY_INTEGER2').eq.-1234, 'MY_INTEGER2',iget('MY_INTEGER2'))
   call unit_check('iget', iget('NOTTHERE').eq.0,        'NOTTHERE',iget('NOTTHERE'))
   call unit_check('iget', iget('BLANK').eq.0,           'BLANK',iget('BLANK'))
   call unit_check_done('iget',msg='')
end subroutine test_iget
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_igets()
integer        :: ier
real,allocatable :: i(:)
   call unit_check_start('igets',msg=' direct tests of igets(3f)')
   call store('MY_INTEGER1','100 0 -321','define',ier)
   call store('MY_INTEGER2',-1234,'define',ier)
   call store('BAD','sfs','define',ier)
   call store('BLANK',' ','define',ier)
   call unit_check('igets', all(igets('MY_INTEGER1').eq.[100,0,-321]), 'MY_INTEGER1')
   call unit_check('igets', all(igets('MY_INTEGER2').eq.[-1234]),      'MY_INTEGER2')
   call unit_check('igets', size(igets('NOTTHERE')).eq.0,              'NOTTHERE')
   i=igets('BAD')
   call unit_check('igets', size(i).eq.1.and.i(1).eq.-huge(0),         'BAD')
   call unit_check('igets', size(igets('BLANK')).eq.0,                 'BLANK')
   call unit_check_done('igets',msg='')
end subroutine test_igets
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_kracken()
   call unit_check_start('kracken',msg='')
   !!call unit_check('kracken', 0.eq.0, 'checking',100)
   call unit_check_done('kracken',msg='')
end subroutine test_kracken
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_lget()
integer        :: ier
   call unit_check_start('lget',msg=' direct tests of LGET(3f)')
   call store('MY_LOGICAL_TRUE',.true.,'define',ier)
   call store('MY_LOGICAL_FALSE',.false.,'define',ier)
   call store('BAD','.bad.','define',ier)
   call store('BLANK',' ','define',ier)
   call unit_check('lget', lget('MY_LOGICAL_TRUE'),       'MY_LOGICAL_TRUE',lget('MY_LOGICAL_TRUE'))
   call unit_check('lget', .not.lget('MY_LOGICAL_FALSE'), 'MY_LOGICAL_FALSE',lget('MY_LOGICAL_FALSE'))
   call unit_check('lget', .not.lget('NOTTHERE'),         'NOTTHERE',lget('NOTTHERE'))
   call unit_check('lget', .not.lget('BAD'),              'BAD',lget('BAD'))
   call unit_check('lget', lget('BLANK'),                 'BLANK',lget('BLANK'))
   call unit_check_done('lget',msg='')
end subroutine test_lget
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_lgets()
integer        :: ier
   call unit_check_start('lgets',msg=' direct tests of lgets(3f)')
   call store('MY_LOGICAL_TRUE','.true. .true.','define',ier)
   call store('MY_LOGICAL_FALSE','.false.','define',ier)
   call store('MY_LOGICAL3','.true. .false. .false. .true.','define',ier)
   call store('BAD','.bad.','define',ier)
   call store('BLANK',' ','define',ier)
   call unit_check('lgets',all(lgets('MY_LOGICAL_TRUE').eqv.[.true.,.true.]),             'MY_LOGICAL_TRUE')
   call unit_check('lgets',all(lgets('MY_LOGICAL_FALSE').eqv.[.false.]),                  'MY_LOGICAL_FALSE')
   call unit_check('lgets',all(lgets('MY_LOGICAL3').eqv.[.true.,.false.,.false.,.true.]), 'MY_LOGICAL3')
   call unit_check('lgets',all(lgets('NOTTHERE').eqv.[.false.]),                          'NOTTHERE')
   call unit_check('lgets',all(lgets('BAD').eqv.[.false.]),                               'BAD')
   call unit_check('lgets',all(lgets('BLANK').eqv.[.true.]),                              'BLANK')
   call unit_check_done('lgets',msg='')
end subroutine test_lgets
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_parse()
character(len=:),allocatable  :: verb
integer     :: i
integer     :: ierr
character(len=132) :: line
character(len=132), parameter :: commands(5,2)= reshape([character(len=132) :: &
  'start', ' -i 10 -message this is a message', &
  'end'  , ' -i 20 -j 30 -k 55.55 -l .false.', &
  'list' , ' ', &
  'help' , ' -oo', &
  'where', ' -i 44.44 '],shape(commands),order=[2,1])
   call unit_check_start('parse',msg='')
   do i=1,size(commands,dim=1)
      line=commands(i,2) ! need mutable line
      verb=commands(i,1)
      call parse(verb,line,'define',ierr)
      select case(verb)
      case('start')
         call unit_check('parse',sget('start_i').eq.'10','start_i')
         call unit_check('parse',sget('start_message').eq.'this is a message','start_message')
      case('end')
         call unit_check('parse',iget('end_i').eq.20,'end_i')
         call unit_check('parse',iget('end_j').eq.30,'end_j')
         call unit_check('parse',rget('end_k').eq.55.55,'end_k')
      case('list')
         call unit_check('parse',sget('list_oo').eq.'','list_oo')
      case('help')
         call unit_check('parse',sget('help_oo').eq.'','help_oo')
      endselect
   enddo
   call unit_check_done('parse',msg='')
end subroutine test_parse
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_retrev()
character(len=IPvalue) :: val
integer                :: len, ier
   call unit_check_start('retrev',msg='')
   call kracken('demo', ' -value my default string -x one -x two -A 12345')
   call retrev('demo_value',val,len,ier)
   call unit_check('retrev', val.eq.'my default string', 'value',val)
   call retrev('demo_x',val,len,ier)
   call unit_check('retrev', val.eq.'one two', 'x',val)
   call retrev('demo_A',val,len,ier)
   call unit_check('retrev', val.eq.'12345', 'A',val)
   call unit_check_done('retrev',msg='')
end subroutine test_retrev
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_rget()
integer        :: ier
   call unit_check_start('rget',msg=' direct tests of rget(3f)')
   call store('MY_REAL1',1234.567,'define',ier)
   call store('MY_REAL2',-1234.567e3,'define',ier)
   call store('BLANK',' ','define',ier)
   call unit_check('rget', rget('MY_REAL1').eq.1234.567,    'MY_REAL1',rget('MY_REAL1'))
   call unit_check('rget', rget('MY_REAL2').eq.-1234.567e3, 'MY_REAL2',rget('MY_REAL2'))
   call unit_check('rget', rget('NOTTHERE').eq.0,           'NOTTHERE',rget('NOTTHERE'))
   call unit_check('dget', ieee_is_nan(dget('BAD')),        'BAD',dget('BAD'))
   call unit_check('rget', rget('BLANK').eq.0,              'BLANK',rget('BLANK'))
   call unit_check_done('rget',msg='')
end subroutine test_rget
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_rgets()
integer        :: ier
real,allocatable :: r(:)
   call unit_check_start('rgets',msg=' direct tests of rgets(3f)')
   call store('MY_REAL1','100.0e0 0.0e0 300.33333e2','define',ier)
   call store('MY_REAL2',-1234.0e-20,'define',ier)
   call store('BAD','sfs','define',ier)
   call store('BLANK',' ','define',ier)
   call unit_check('rgets', all(rgets('MY_REAL1').eq.[100.0e0,0.0e0,300.33333e2]),'MY_REAL1')
   call unit_check('rgets', all(rgets('MY_REAL2').eq.[-1234.0e-20]),              'MY_REAL2')
   call unit_check('rgets', size(rgets('NOTTHERE')).eq.0,                         'NOTTHERE')
   r=rgets('BAD')
   if(size(r).gt.0)then
      call unit_check('rgets', size(r).eq.1.and.ieee_is_nan(r(1)),                'BAD')
   else
      call unit_check('rgets', size(r).eq.1,                                      'BAD')
   endif
   call unit_check('rgets', size(rgets('BLANK')).eq.0,                            'BLANK')
   call unit_check_done('rgets',msg='')
end subroutine test_rgets
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_setprompts()
   call unit_check_start('setprompts',msg='')
   !!call unit_check('setprompts', 0.eq.0, 'checking',100)
   call unit_check_done('setprompts',msg='')
end subroutine test_setprompts
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_sget()
integer        :: ier
   call unit_check_start('sget',msg=' direct tests of sget(3f)')
   call store('MY_STRING1','100 0 -321','define',ier)
   call store('MY_STRING2',-1234,'define',ier)
   call store('BLANK',' ','define',ier)
   call unit_check('sget', sget('MY_STRING1').eq.'100 0 -321',               'MY_STRING1')
   call unit_check('sget', sget('MY_STRING2').eq.'-1234',                    'MY_STRING2')
   call unit_check('sget', len(sget('NOTTHERE')).eq.0,                       'NOTTHERE')
   call unit_check('sget', len(sget('BLANK')).ne.0.and.sget('BLANK').eq.' ', 'BLANK')
   call unit_check_done('sget',msg='')
end subroutine test_sget
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_sgets()
integer        :: ier
   call unit_check_start('sgets',msg=' direct tests of sgets(3f)')
   call store('MY_STRING1','100 0 -321','define',ier)
   call store('MY_STRING2',-1234,       'define',ier)
   call store('BLANK',' ',              'define',ier)
   call unit_check('sgets', all(sgets('MY_STRING1').eq. ['100 ','0   ','-321']),      'MY_STRING1')
   call unit_check('sgets', all(sgets('MY_STRING2').eq.['-1234']),                    'MY_STRING2')
   call unit_check('sgets', all(sgets('NOTTHERE_SGETS').eq.[char(0)]),                'NOTTHERE_SGETS')
   call unit_check('sgets', size(sgets('BLANK')).eq.0.and.len(sgets('BLANK')).eq.0,   'BLANK')
   call unit_check_done('sgets',msg='')
end subroutine test_sgets
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_show()
   call unit_check_start('show',msg='')
   !!call unit_check('show', 0.eq.0, 'checking',100)
   call unit_check_done('show',msg='')
end subroutine test_show
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_store()
integer :: ier
 call unit_check_start('store',msg='')

 call store('MY_STRING','My string value','add',ier)
 if(ier.ne.0)write(*,*)'ERROR: could not store MY_STRING ier=',ier
 call store('MY_INTEGER',12345678,'no_add',ier)    ! this will be an error because MY does not have the -INTEGER keyword defined
 call store('MY_INTEGER',1234,'add',ier)           ! now define MY_INTEGER
 call store('MY_INTEGER',987654321,'add',ier)      ! if 'no_add' it will APPEND to current string
 call store('MY_REAL',1234.5678,'add',ier)
 call store('MY_DOUBLE',123.4567d8,'add',ier)
 call store('MY_LOGICAL',.true.,'add',ier)
 call store('MY_INTEGER',987654321,'replace',ier)  ! if 'replace' is used REPLACE instead of APPEND to current value
 call store('MY_UNKNOWN',987654321,'replace',ier)  ! 'replace' can only replace an existing entry, not add one

 call unit_check('store',sget('MY_STRING')  == 'My string value',    'MY_STRING',sget('MY_STRING'),'My string value')
 call unit_check('store',rget('MY_REAL')    == 1234.5677,            'MY_REAL',rget('MY_REAL'),1234.5677)
 call unit_check('store',lget('MY_LOGICAL'),                         'MY_LOGICAL',lget('MY_LOGICAL'),.true.)
 call unit_check('store',iget('MY_INTEGER') == 987654321,            'MY_INTEGER',iget('MY_INTEGER'),987654321)
 call unit_check('store',dget('MY_DOUBLE')  == 12345670000.000000d0, 'MY_DOUBLE',dget('MY_DOUBLE'),12345670000.000000d0)

 call unit_check_done('store',msg='')
end subroutine test_store
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
end program test_suite_M_kracken
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
