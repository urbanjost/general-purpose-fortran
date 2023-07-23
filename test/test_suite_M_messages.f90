module M_test_suite_M_messages
use, intrinsic :: ISO_FORTRAN_ENV, only : INT8, INT16, INT32, INT64       !  1           2           4           8
use, intrinsic :: ISO_FORTRAN_ENV, only : REAL32, REAL64, REAL128         !  4           8          10
use M_framework__msg
private
public test_suite_M_messages
contains
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine test_suite_M_messages()
use M_framework__verify, only : unit_test_start,unit_test,unit_test_done,unit_test_good,unit_test_bad,unit_test_msg
!*! setup
   call test_blocks()
   call test_junbad()
   call test_junbat()
   call test_junbuster()
   call test_jundragon()
   call test_junroach()
   call test_junsun()
   call test_juntrolls()
   call test_percent_done()
   call test_signs()
   call test_tabgraph()
!*! teardown
contains
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_blocks()
use M_messages, only : blocks
   call unit_test_start('blocks',msg='')
   call blocks('NOTICE',6)
   !*!call unit_test('blocks', 0.eq.0, 'checking',100)
   call unit_test_done('blocks',msg='')
end subroutine test_blocks
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_junbad()
use M_messages, only : junbad
   call unit_test_start('junbad',msg='')
   call junbad('s')
   !*!call unit_test('junbad', 0.eq.0, 'checking',100)
   call unit_test_done('junbad',msg='')
end subroutine test_junbad
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_junbat()
use M_messages, only : junbat
   call unit_test_start('junbat',msg='')
   call junbat('s')
   !*!call unit_test('junbat', 0.eq.0, 'checking',100)
   call unit_test_done('junbat',msg='')
end subroutine test_junbat
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_junbuster()
use M_messages, only : junbuster
   call unit_test_start('junbuster',msg='')
   call junbuster('s')
   !*!call unit_test('junbuster', 0.eq.0, 'checking',100)
   call unit_test_done('junbuster',msg='')
end subroutine test_junbuster
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_jundragon()
use M_messages, only : jundragon
character(len=32) :: a(8)
   call unit_test_start('jundragon',msg='')
   a(1)='Puff, the magic dragon----------'
   a(2)='lived by the sea----------------'
   a(3)='and frolicked in the Autumn mist'
   a(4)='in a land called----------------'
   a(5)='Honiley-------------------------'
   a(6)='--------------------------------'
   a(7)='--------------------------------'
   a(8)='--------------------------------'
   call jundragon('s',a)
   !*!call unit_test('jundragon', 0.eq.0, 'checking',100)
   call unit_test_done('jundragon',msg='')
end subroutine test_jundragon
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_junroach()
use M_messages, only : junroach
   call unit_test_start('junroach',msg='')
   call junroach('s')
   !*!call unit_test('junroach', 0.eq.0, 'checking',100)
   call unit_test_done('junroach',msg='')
end subroutine test_junroach
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_junsun()
use M_messages, only : junsun
   call unit_test_start('junsun',msg='')
   call junsun('s')
   !*!call unit_test('junsun', 0.eq.0, 'checking',100)
   call unit_test_done('junsun',msg='')
end subroutine test_junsun
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_juntrolls()
use M_messages, only : juntrolls
   call unit_test_start('juntrolls',msg='')
   call juntrolls('s',[         &
    'Please ...           ',  &
    "   don't feed        ",  &
    '   the               ',  &
    '   TROLLS!           '   &
    ])
   !*!call unit_test('juntrolls', 0.eq.0, 'checking',100)
   call unit_test_done('juntrolls',msg='')
end subroutine test_juntrolls
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_percent_done()
use m_messages, only : percent_done
integer :: i, nr=10
   call unit_test_start('percent_done',msg='')
   do i=1,nr
      call percent_done(i,nr)
   enddo
   !*!call unit_test('percent_done', 0.eq.0, 'checking',100)
   call unit_test_done('percent_done',msg='')
end subroutine test_percent_done
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_signs()
use M_messages, only : signs
   call unit_test_start('signs',msg='')
   call signs('NOTICE',6)
   !*!call unit_test('signs', 0.eq.0, 'checking',100)
   call unit_test_done('signs',msg='')
end subroutine test_signs
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_tabgraph()
use M_messages, only : tabgraph
       implicit none
       character(len=500) :: ctmp
       integer            :: i10, i20
       real               :: array(4)
       integer            :: ilen
   call unit_test_start('tabgraph',msg='')
   do i20=1,4
      do i10=1,400
         array(1)=i10*12/100.0
         array(2)=sin(array(1))
         array(3)=cos(array(1))
         if(i20.eq.1)then
         ! fixed width of 50 for scale
            call tabgraph(ctmp,array,3,-1.0,1.0,' ',50)
            ! ctmp  --> CTMP string to fill
            ! array --> ARRAY data
            ! 3     --> IVALS
            !-1     --> RMIN
            ! 1     --> RMAX
            !' '    --> CFILL
            !50     --> ILEN
         elseif(i20.eq.2)then
         ! fixed width of 90 for scale with a non-blank fill character
            call tabgraph(ctmp,array,3,-1.0,1.0,'.',90)
         elseif(i20.eq.3)then
         ! 0 len auto-sizes scale region
            call tabgraph(ctmp,array,3,-1.0,1.0,' ',0)
         elseif(i20.eq.4)then
         ! number of values less than or equal to 1
            call tabgraph(ctmp,array,1,0.0,48.0,' ',0)
         endif
         if(i10.eq.1)then
            ilen=len_trim(ctmp)
            ilen=max(ilen,1)
         endif
         write(*,'(a)')ctmp(1:ilen) ! tabgraph test program
         ! write(*,'(i5,a)')i10,ctmp(1:ilen) write with a number count
      enddo
   enddo
   !*!call unit_test('tabgraph', 0.eq.0, 'checking',100)
   call unit_test_done('tabgraph',msg='')
end subroutine test_tabgraph
!===================================================================================================================================
end subroutine test_suite_M_messages
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
end module M_test_suite_M_messages
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
program runtest
use M_messages
use M_framework__msg
use M_framework__verify, only : unit_test_stop
use M_test_suite_M_messages
implicit none
   call test_suite_M_messages()
   call unit_test_stop()
end program runtest
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
