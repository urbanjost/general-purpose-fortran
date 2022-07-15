program runtest
use M_LA
use M_verify
!use M_msg
implicit none
   unit_check_command=''
   unit_check_keep_going=.true.
   unit_check_level=0
!! setup
   call test_matx_waxpy_()
   call test_elementcopy()
   call test_linspace()
   call test_mat_flop()
   call test_mat_inverse_hilbert()
   call test_mat_iwamax()
   call test_mat_magic()
   call test_mat_pythag()
   call test_mat_rat()
   call test_mat_round()
   call test_mat_rref()
   call test_mat_rrot()
   call test_mat_rrotg()
   call test_mat_rset()
   call test_mat_rswap()
   call test_mat_urand()
   call test_mat_wasum()
   call test_mat_watan()
   call test_mat_wcopy()
   call test_mat_wdiv()
   call test_mat_wdotci()
   call test_mat_wdotcr()
   call test_mat_wdotui()
   call test_mat_wdotur()
   call test_mat_wlog()
   call test_mat_wmul()
   call test_mat_wnrm2()
   call test_mat_wpofa()
   call test_mat_wrscal()
   call test_mat_wscal()
   call test_mat_wset()
   call test_mat_wsign()
   call test_mat_wsqrt()
   call test_mat_wswap()
   call test_ml_comqr3_()
   call test_ml_corth_()
   call test_ml_htribk_()
   call test_ml_htridi_()
   call test_ml_imtql2_()
   call test_ml_wgeco_()
   call test_ml_wgedi_()
   call test_ml_wgefa_()
   call test_ml_wgesl_()
   call test_ml_wqrdc_()
   call test_ml_wqrsl_()
   call test_ml_wsvdc_()
!! teardown
   call unit_check_stop()
contains
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_matx_waxpy_()
use M_verify, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg
use M_verify, only : unit_check_level
implicit none
   call unit_check_start('matx_waxpy_',msg='')
   !!call unit_check('matx_waxpy_', 0.eq.0, 'checking',100)
   call unit_check_done('matx_waxpy_',msg='')
end subroutine test_matx_waxpy_
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_elementcopy()
use M_verify, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg
use M_verify, only : unit_check_level
implicit none
   call unit_check_start('elementcopy',msg='')
   !!call unit_check('elementcopy_int16', 0.eq.0, 'checking',100)
   call unit_check_done('elementcopy',msg='')
end subroutine test_elementcopy
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_linspace()
use M_verify, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg
use M_verify, only : unit_check_level
implicit none
   call unit_check_start('linspace',msg='')
   !!call unit_check('linspace_int16', 0.eq.0, 'checking',100)
   call unit_check_done('linspace',msg='')
end subroutine test_linspace
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_flop()
use M_verify, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg
use M_verify, only : unit_check_level
implicit none
   call unit_check_start('mat_flop',msg='')
   !!call unit_check('mat_flop', 0.eq.0, 'checking',100)
   call unit_check_done('mat_flop',msg='')
end subroutine test_mat_flop
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_inverse_hilbert()
use M_verify, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg
use M_verify, only : unit_check_level
implicit none
   call unit_check_start('mat_inverse_hilbert',msg='')
   !!call unit_check('mat_inverse_hilbert', 0.eq.0, 'checking',100)
   call unit_check_done('mat_inverse_hilbert',msg='')
end subroutine test_mat_inverse_hilbert
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_iwamax()
use M_verify, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg
use M_verify, only : unit_check_level
implicit none
   call unit_check_start('mat_iwamax',msg='')
   !!call unit_check('mat_iwamax', 0.eq.0, 'checking',100)
   call unit_check_done('mat_iwamax',msg='')
end subroutine test_mat_iwamax
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_magic()
use M_verify, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg
use M_verify, only : unit_check_level
implicit none
   call unit_check_start('mat_magic',msg='')
   !!call unit_check('mat_magic', 0.eq.0, 'checking',100)
   call unit_check_done('mat_magic',msg='')
end subroutine test_mat_magic
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_pythag()
use M_verify, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg
use M_verify, only : unit_check_level
implicit none
   call unit_check_start('mat_pythag',msg='')
   !!call unit_check('mat_pythag', 0.eq.0, 'checking',100)
   call unit_check_done('mat_pythag',msg='')
end subroutine test_mat_pythag
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_rat()
use M_verify, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg
use M_verify, only : unit_check_level
implicit none
   call unit_check_start('mat_rat',msg='')
   !!call unit_check('mat_rat', 0.eq.0, 'checking',100)
   call unit_check_done('mat_rat',msg='')
end subroutine test_mat_rat
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_round()
use M_verify, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg
use M_verify, only : unit_check_level
implicit none
   call unit_check_start('mat_round',msg='')
   !!call unit_check('mat_round', 0.eq.0, 'checking',100)
   call unit_check_done('mat_round',msg='')
end subroutine test_mat_round
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_rref()
use M_verify, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg
use M_verify, only : unit_check_level
implicit none
   call unit_check_start('mat_rref',msg='')
   !!call unit_check('mat_rref', 0.eq.0, 'checking',100)
   call unit_check_done('mat_rref',msg='')
end subroutine test_mat_rref
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_rrot()
use M_verify, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg
use M_verify, only : unit_check_level
implicit none
   call unit_check_start('mat_rrot',msg='')
   !!call unit_check('mat_rrot', 0.eq.0, 'checking',100)
   call unit_check_done('mat_rrot',msg='')
end subroutine test_mat_rrot
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_rrotg()
use M_verify, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg
use M_verify, only : unit_check_level
implicit none
   call unit_check_start('mat_rrotg',msg='')
   !!call unit_check('mat_rrotg', 0.eq.0, 'checking',100)
   call unit_check_done('mat_rrotg',msg='')
end subroutine test_mat_rrotg
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_rset()
use M_verify, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg
use M_verify, only : unit_check_level
implicit none
   call unit_check_start('mat_rset',msg='')
   !!call unit_check('mat_rset', 0.eq.0, 'checking',100)
   call unit_check_done('mat_rset',msg='')
end subroutine test_mat_rset
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_rswap()
use M_verify, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg
use M_verify, only : unit_check_level
implicit none
   call unit_check_start('mat_rswap',msg='')
   !!call unit_check('mat_rswap', 0.eq.0, 'checking',100)
   call unit_check_done('mat_rswap',msg='')
end subroutine test_mat_rswap
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_urand()
use M_verify, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg
use M_verify, only : unit_check_level
implicit none
   call unit_check_start('mat_urand',msg='')
   !!call unit_check('mat_urand', 0.eq.0, 'checking',100)
   call unit_check_done('mat_urand',msg='')
end subroutine test_mat_urand
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_wasum()
use M_verify, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg
use M_verify, only : unit_check_level
implicit none
   call unit_check_start('mat_wasum',msg='')
   !!call unit_check('mat_wasum', 0.eq.0, 'checking',100)
   call unit_check_done('mat_wasum',msg='')
end subroutine test_mat_wasum
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_watan()
use M_verify, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg
use M_verify, only : unit_check_level
implicit none
   call unit_check_start('mat_watan',msg='')
   !!call unit_check('mat_watan', 0.eq.0, 'checking',100)
   call unit_check_done('mat_watan',msg='')
end subroutine test_mat_watan
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_wcopy()
use M_verify, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg
use M_verify, only : unit_check_level
implicit none
   call unit_check_start('mat_wcopy',msg='')
   !!call unit_check('mat_wcopy', 0.eq.0, 'checking',100)
   call unit_check_done('mat_wcopy',msg='')
end subroutine test_mat_wcopy
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_wdiv()
use M_verify, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg
use M_verify, only : unit_check_level
implicit none
   call unit_check_start('mat_wdiv',msg='')
   !!call unit_check('mat_wdiv', 0.eq.0, 'checking',100)
   call unit_check_done('mat_wdiv',msg='')
end subroutine test_mat_wdiv
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_wdotci()
use M_verify, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg
use M_verify, only : unit_check_level
implicit none
   call unit_check_start('mat_wdotci',msg='')
   !!call unit_check('mat_wdotci', 0.eq.0, 'checking',100)
   call unit_check_done('mat_wdotci',msg='')
end subroutine test_mat_wdotci
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_wdotcr()
use M_verify, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg
use M_verify, only : unit_check_level
implicit none
   call unit_check_start('mat_wdotcr',msg='')
   !!call unit_check('mat_wdotcr', 0.eq.0, 'checking',100)
   call unit_check_done('mat_wdotcr',msg='')
end subroutine test_mat_wdotcr
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_wdotui()
use M_verify, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg
use M_verify, only : unit_check_level
implicit none
   call unit_check_start('mat_wdotui',msg='')
   !!call unit_check('mat_wdotui', 0.eq.0, 'checking',100)
   call unit_check_done('mat_wdotui',msg='')
end subroutine test_mat_wdotui
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_wdotur()
use M_verify, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg
use M_verify, only : unit_check_level
implicit none
   call unit_check_start('mat_wdotur',msg='')
   !!call unit_check('mat_wdotur', 0.eq.0, 'checking',100)
   call unit_check_done('mat_wdotur',msg='')
end subroutine test_mat_wdotur
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_wlog()
use M_verify, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg
use M_verify, only : unit_check_level
implicit none
   call unit_check_start('mat_wlog',msg='')
   !!call unit_check('mat_wlog', 0.eq.0, 'checking',100)
   call unit_check_done('mat_wlog',msg='')
end subroutine test_mat_wlog
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_wmul()
use M_verify, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg
use M_verify, only : unit_check_level
implicit none
   call unit_check_start('mat_wmul',msg='')
   !!call unit_check('mat_wmul', 0.eq.0, 'checking',100)
   call unit_check_done('mat_wmul',msg='')
end subroutine test_mat_wmul
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_wnrm2()
use M_verify, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg
use M_verify, only : unit_check_level
implicit none
   call unit_check_start('mat_wnrm2',msg='')
   !!call unit_check('mat_wnrm2', 0.eq.0, 'checking',100)
   call unit_check_done('mat_wnrm2',msg='')
end subroutine test_mat_wnrm2
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_wpofa()
use M_verify, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg
use M_verify, only : unit_check_level
implicit none
   call unit_check_start('mat_wpofa',msg='')
   !!call unit_check('mat_wpofa', 0.eq.0, 'checking',100)
   call unit_check_done('mat_wpofa',msg='')
end subroutine test_mat_wpofa
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_wrscal()
use M_verify, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg
use M_verify, only : unit_check_level
implicit none
   call unit_check_start('mat_wrscal',msg='')
   !!call unit_check('mat_wrscal', 0.eq.0, 'checking',100)
   call unit_check_done('mat_wrscal',msg='')
end subroutine test_mat_wrscal
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_wscal()
use M_verify, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg
use M_verify, only : unit_check_level
implicit none
   call unit_check_start('mat_wscal',msg='')
   !!call unit_check('mat_wscal', 0.eq.0, 'checking',100)
   call unit_check_done('mat_wscal',msg='')
end subroutine test_mat_wscal
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_wset()
use M_verify, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg
use M_verify, only : unit_check_level
implicit none
   call unit_check_start('mat_wset',msg='')
   !!call unit_check('mat_wset', 0.eq.0, 'checking',100)
   call unit_check_done('mat_wset',msg='')
end subroutine test_mat_wset
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_wsign()
use M_verify, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg
use M_verify, only : unit_check_level
implicit none
   call unit_check_start('mat_wsign',msg='')
   !!call unit_check('mat_wsign', 0.eq.0, 'checking',100)
   call unit_check_done('mat_wsign',msg='')
end subroutine test_mat_wsign
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_wsqrt()
use M_verify, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg
use M_verify, only : unit_check_level
implicit none
   call unit_check_start('mat_wsqrt',msg='')
   !!call unit_check('mat_wsqrt', 0.eq.0, 'checking',100)
   call unit_check_done('mat_wsqrt',msg='')
end subroutine test_mat_wsqrt
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_wswap()
use M_verify, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg
use M_verify, only : unit_check_level
implicit none
   call unit_check_start('mat_wswap',msg='')
   !!call unit_check('mat_wswap', 0.eq.0, 'checking',100)
   call unit_check_done('mat_wswap',msg='')
end subroutine test_mat_wswap
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_ml_comqr3_()
use M_verify, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg
use M_verify, only : unit_check_level
implicit none
   call unit_check_start('ml_comqr3_',msg='')
   !!call unit_check('ml_comqr3_', 0.eq.0, 'checking',100)
   call unit_check_done('ml_comqr3_',msg='')
end subroutine test_ml_comqr3_
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_ml_corth_()
use M_verify, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg
use M_verify, only : unit_check_level
implicit none
   call unit_check_start('ml_corth_',msg='')
   !!call unit_check('ml_corth_', 0.eq.0, 'checking',100)
   call unit_check_done('ml_corth_',msg='')
end subroutine test_ml_corth_
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_ml_htribk_()
use M_verify, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg
use M_verify, only : unit_check_level
implicit none
   call unit_check_start('ml_htribk_',msg='')
   !!call unit_check('ml_htribk_', 0.eq.0, 'checking',100)
   call unit_check_done('ml_htribk_',msg='')
end subroutine test_ml_htribk_
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_ml_htridi_()
use M_verify, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg
use M_verify, only : unit_check_level
implicit none
   call unit_check_start('ml_htridi_',msg='')
   !!call unit_check('ml_htridi_', 0.eq.0, 'checking',100)
   call unit_check_done('ml_htridi_',msg='')
end subroutine test_ml_htridi_
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_ml_imtql2_()
use M_verify, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg
use M_verify, only : unit_check_level
implicit none
   call unit_check_start('ml_imtql2_',msg='')
   !!call unit_check('ml_imtql2_', 0.eq.0, 'checking',100)
   call unit_check_done('ml_imtql2_',msg='')
end subroutine test_ml_imtql2_
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_ml_wgeco_()
use M_verify, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg
use M_verify, only : unit_check_level
implicit none
   call unit_check_start('ml_wgeco_',msg='')
   !!call unit_check('ml_wgeco_', 0.eq.0, 'checking',100)
   call unit_check_done('ml_wgeco_',msg='')
end subroutine test_ml_wgeco_
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_ml_wgedi_()
use M_verify, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg
use M_verify, only : unit_check_level
implicit none
   call unit_check_start('ml_wgedi_',msg='')
   !!call unit_check('ml_wgedi_', 0.eq.0, 'checking',100)
   call unit_check_done('ml_wgedi_',msg='')
end subroutine test_ml_wgedi_
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_ml_wgefa_()
use M_verify, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg
use M_verify, only : unit_check_level
implicit none
   call unit_check_start('ml_wgefa_',msg='')
   !!call unit_check('ml_wgefa_', 0.eq.0, 'checking',100)
   call unit_check_done('ml_wgefa_',msg='')
end subroutine test_ml_wgefa_
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_ml_wgesl_()
use M_verify, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg
use M_verify, only : unit_check_level
implicit none
   call unit_check_start('ml_wgesl_',msg='')
   !!call unit_check('ml_wgesl_', 0.eq.0, 'checking',100)
   call unit_check_done('ml_wgesl_',msg='')
end subroutine test_ml_wgesl_
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_ml_wqrdc_()
use M_verify, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg
use M_verify, only : unit_check_level
implicit none
   call unit_check_start('ml_wqrdc_',msg='')
   !!call unit_check('ml_wqrdc_', 0.eq.0, 'checking',100)
   call unit_check_done('ml_wqrdc_',msg='')
end subroutine test_ml_wqrdc_
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_ml_wqrsl_()
use M_verify, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg
use M_verify, only : unit_check_level
implicit none
   call unit_check_start('ml_wqrsl_',msg='')
   !!call unit_check('ml_wqrsl_', 0.eq.0, 'checking',100)
   call unit_check_done('ml_wqrsl_',msg='')
end subroutine test_ml_wqrsl_
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_ml_wsvdc_()
use M_verify, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg
use M_verify, only : unit_check_level
implicit none
   call unit_check_start('ml_wsvdc_',msg='')
   !!call unit_check('ml_wsvdc_', 0.eq.0, 'checking',100)
   call unit_check_done('ml_wsvdc_',msg='')
end subroutine test_ml_wsvdc_
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
end program runtest
