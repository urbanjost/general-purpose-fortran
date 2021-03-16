program runtest
use M_msg
use M_verify, only : unit_check_command, unit_check_keep_going, unit_check_level
implicit none
   unit_check_command=''
   unit_check_keep_going=.true.
   unit_check_level=0
   call test_suite_M_calculator()
contains

end program runtest
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine test_suite_M_calculator
use M_verify, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg
use M_verify, only : unit_check_level
use M_verify,  only : almost
!*REGULAR VERSION, NOT GITHUB VERSION!use M_calculator, only : calculator, getvalue, igetvalue, rgetvalue, stuff, stuffa
!*REGULAR VERSION, NOT GITHUB VERSION!use M_calculator, only : calculator, stuff, stuffa
!*REGULAR VERSION, NOT GITHUB VERSION!use M_calculator, only : inum0, rnum0, dnum0, snum0, expression, strgarr, strgar2
use M_calculator, only : calculator
use M_calculator, only : iclen_calc, ixy_calc, icname_calc, x, y, values_len, values
! convenience routines
use M_calculator, only : inum0, rnum0, dnum0, snum0, expression
!!use M_calculator, only : read_config
call test_calculator()
!!call test_jucals()
!!call test_squeeze_()

!!call test_a_to_d_()

call test_getvalue()
call test_igetvalue()
call test_rgetvalue()

call test_stuffa()
call test_stuff()

call test_dnum0()
call test_inum0()
call test_expression()
call test_rnum0()
call test_snum0()
call test_strgar2()
call test_strgarr()

call test_c()
call test_juown1()

contains
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_stuff()
doubleprecision :: value
!*!   call stuff('A',10.0)
!*!   call stuff('PI',3.141592653589793238462643383279502884197169399375105820974944592307d0)
   value=dnum0('A*PI')
!*!   call unit_check_start('stuff',msg='')
!*!   call unit_check('stuff', value.eq. 31.415926535897931d0, 'check PI*10',value,31.415926535897931d0)
   call unit_check_done('stuff',msg='')
end subroutine test_stuff
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_getvalue()
doubleprecision :: value
!*!   call stuff('PI',3.141592653589793238462643383279502884197169399375105820974944592307d0)
!*!   value=getvalue('PI')
   call unit_check_start('getvalue',msg='')
!*!   call unit_check('getvalue', value.eq. 3.1415926535897931d0, 'check PI',value)
   call unit_check_done('getvalue',msg='')
end subroutine test_getvalue
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_igetvalue()
!*!   call stuff('ten',10)
   call unit_check_start('igetvalue',msg='')
!*!   call unit_check('igetvalue', inum0('ten').eq.10, 'checking integer',inum0('ten'))
   call unit_check_done('igetvalue',msg='')
end subroutine test_igetvalue
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_calculator()

   call unit_check_start('calculator',msg='')
   !!call unit_check('calculator', 0.eq.0, 'checking',100)
   call unit_check_done('calculator',msg='')
end subroutine test_calculator
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_rgetvalue()

   call unit_check_start('rgetvalue',msg='')
   !!call unit_check('rgetvalue', 0.eq.0, 'checking',100)
   call unit_check_done('rgetvalue',msg='')
end subroutine test_rgetvalue
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_stuffa()
character(len=:),allocatable :: string
   call unit_check_start('stuffa',msg='')
!*!   call stuffa('$A','')
!*!   string='this is the value of the string'
!*!   call stuffa('$mystring',string)
!*!   call unit_check('stuffa', snum0('$mystring').eq.string, 'string check',snum0('$mystring'))
!*!   string='this is the new value of the string'
!*!   call stuffa('$mystring',string)
!*!   call unit_check('stuffa', snum0('$mystring').eq.string, 'string check',snum0('$mystring'))
   call unit_check_done('stuffa',msg='')
end subroutine test_stuffa
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_c()

   call unit_check_start('c',msg='')
   !!call unit_check('c', 0.eq.0, 'checking',100)
   call unit_check_done('c',msg='')
end subroutine test_c
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_juown1()

   call unit_check_start('juown1',msg='')
   !!call unit_check('juown1', 0.eq.0, 'checking',100)
   call unit_check_done('juown1',msg='')
end subroutine test_juown1
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_dnum0()
doubleprecision :: y, z
   Y=DNUM0('CI = 10 * sin(3.1416/4)')
   Z=DNUM0('CI')
   call unit_check_start('dnum0',msg='')
   !!JUST PRINT VALUES!!if(almost(z,10*sin(3.1416d0/4.0d0),35,verbose=.true.))continue
   call unit_check('dnum0', y.eq.z.and.almost(y,10*sin(3.1416d0/4d0),15),&
           & 'checking CI',dnum0('CI'),dnum0('10*sin(3.1416/4)'),10*sin(3.1416d0/4.0d0))
   call unit_check_done('dnum0',msg='')
end subroutine test_dnum0
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_inum0()
character(len=:),allocatable :: string
   call unit_check_start('inum0',msg='')
   string='10/3'
   call unit_check('inum0', inum0(string).eq.3, 'checking',string,'==>',inum0(string),'expected',3)
   string='(444/111+1)*10-5.0'
   call unit_check('inum0', inum0(string).eq.45, 'checking',string,'==>',inum0(string),'expected',45)
   call unit_check_done('inum0',msg='')
end subroutine test_inum0
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_expression()

   call unit_check_start('expression',msg='')
   !!call unit_check('expression', 0.eq.0, 'checking',100)
   call unit_check_done('expression',msg='')
end subroutine test_expression
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_rnum0()

   call unit_check_start('rnum0',msg='')
   !!call unit_check('rnum0', 0.eq.0, 'checking',100)
   call unit_check_done('rnum0',msg='')
end subroutine test_rnum0
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_snum0()

   call unit_check_start('snum0',msg='')
   !!call unit_check('snum0', 0.eq.0, 'checking',100)
   call unit_check_done('snum0',msg='')
end subroutine test_snum0
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_strgar2()

   call unit_check_start('strgar2',msg='')
   !!call unit_check('strgar2', 0.eq.0, 'checking',100)
   call unit_check_done('strgar2',msg='')
end subroutine test_strgar2
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_strgarr()

   call unit_check_start('strgarr',msg='')
   !!call unit_check('strgarr', 0.eq.0, 'checking',100)
   call unit_check_done('strgarr',msg='')
end subroutine test_strgarr
end subroutine test_suite_M_calculator
