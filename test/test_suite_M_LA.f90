program runtest
use M_framework__verify, only : unit_test_start,unit_test,unit_test_done,unit_test_good,unit_test_bad,unit_test_msg
use M_framework__verify
use M_LA
!use M_framework__msg
implicit none
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
   call unit_test_stop()
contains
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_matx_waxpy_()
   call unit_test_start('matx_waxpy_',msg='')
   !!call unit_test('matx_waxpy_', 0.eq.0, 'checking',100)
   call unit_test_done('matx_waxpy_',msg='')
end subroutine test_matx_waxpy_
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_elementcopy()
   call unit_test_start('elementcopy',msg='')
   !!call unit_test('elementcopy_int16', 0.eq.0, 'checking',100)
   call unit_test_done('elementcopy',msg='')
end subroutine test_elementcopy
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_linspace()
   call unit_test_start('linspace',msg='')
   !!call unit_test('linspace_int16', 0.eq.0, 'checking',100)
   call unit_test_done('linspace',msg='')
end subroutine test_linspace
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_flop()
   call unit_test_start('mat_flop',msg='')
   !!call unit_test('mat_flop', 0.eq.0, 'checking',100)
   call unit_test_done('mat_flop',msg='')
end subroutine test_mat_flop
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_inverse_hilbert()
   call unit_test_start('mat_inverse_hilbert',msg='')
   !!call unit_test('mat_inverse_hilbert', 0.eq.0, 'checking',100)
   call unit_test_done('mat_inverse_hilbert',msg='')
end subroutine test_mat_inverse_hilbert
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_iwamax()
   call unit_test_start('mat_iwamax',msg='')
   !!call unit_test('mat_iwamax', 0.eq.0, 'checking',100)
   call unit_test_done('mat_iwamax',msg='')
end subroutine test_mat_iwamax
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_magic()
   call unit_test_start('mat_magic',msg='')
   !!call unit_test('mat_magic', 0.eq.0, 'checking',100)
   call unit_test_done('mat_magic',msg='')
end subroutine test_mat_magic
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_pythag()
   call unit_test_start('mat_pythag',msg='')
   !!call unit_test('mat_pythag', 0.eq.0, 'checking',100)
   call unit_test_done('mat_pythag',msg='')
end subroutine test_mat_pythag
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_rat()
   call unit_test_start('mat_rat',msg='')
   !!call unit_test('mat_rat', 0.eq.0, 'checking',100)
   call unit_test_done('mat_rat',msg='')
end subroutine test_mat_rat
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_round()
   call unit_test_start('mat_round',msg='')
   !!call unit_test('mat_round', 0.eq.0, 'checking',100)
   call unit_test_done('mat_round',msg='')
end subroutine test_mat_round
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_rref()
   call unit_test_start('mat_rref',msg='')
   !!call unit_test('mat_rref', 0.eq.0, 'checking',100)
   call unit_test_done('mat_rref',msg='')
end subroutine test_mat_rref
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_rrot()
   call unit_test_start('mat_rrot',msg='')
   !!call unit_test('mat_rrot', 0.eq.0, 'checking',100)
   call unit_test_done('mat_rrot',msg='')
end subroutine test_mat_rrot
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_rrotg()
   call unit_test_start('mat_rrotg',msg='')
   !!call unit_test('mat_rrotg', 0.eq.0, 'checking',100)
   call unit_test_done('mat_rrotg',msg='')
end subroutine test_mat_rrotg
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_rset()
   call unit_test_start('mat_rset',msg='')
   !!call unit_test('mat_rset', 0.eq.0, 'checking',100)
   call unit_test_done('mat_rset',msg='')
end subroutine test_mat_rset
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_rswap()
   call unit_test_start('mat_rswap',msg='')
   !!call unit_test('mat_rswap', 0.eq.0, 'checking',100)
   call unit_test_done('mat_rswap',msg='')
end subroutine test_mat_rswap
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_urand()
   call unit_test_start('mat_urand',msg='')
   !!call unit_test('mat_urand', 0.eq.0, 'checking',100)
   call unit_test_done('mat_urand',msg='')
end subroutine test_mat_urand
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_wasum()
   call unit_test_start('mat_wasum',msg='')
   !!call unit_test('mat_wasum', 0.eq.0, 'checking',100)
   call unit_test_done('mat_wasum',msg='')
end subroutine test_mat_wasum
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_watan()
   call unit_test_start('mat_watan',msg='')
   !!call unit_test('mat_watan', 0.eq.0, 'checking',100)
   call unit_test_done('mat_watan',msg='')
end subroutine test_mat_watan
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_wcopy()
   call unit_test_start('mat_wcopy',msg='')
   !!call unit_test('mat_wcopy', 0.eq.0, 'checking',100)
   call unit_test_done('mat_wcopy',msg='')
end subroutine test_mat_wcopy
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_wdiv()
   call unit_test_start('mat_wdiv',msg='')
   !!call unit_test('mat_wdiv', 0.eq.0, 'checking',100)
   call unit_test_done('mat_wdiv',msg='')
end subroutine test_mat_wdiv
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_wdotci()
   call unit_test_start('mat_wdotci',msg='')
   !!call unit_test('mat_wdotci', 0.eq.0, 'checking',100)
   call unit_test_done('mat_wdotci',msg='')
end subroutine test_mat_wdotci
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_wdotcr()
   call unit_test_start('mat_wdotcr',msg='')
   !!call unit_test('mat_wdotcr', 0.eq.0, 'checking',100)
   call unit_test_done('mat_wdotcr',msg='')
end subroutine test_mat_wdotcr
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_wdotui()
   call unit_test_start('mat_wdotui',msg='')
   !!call unit_test('mat_wdotui', 0.eq.0, 'checking',100)
   call unit_test_done('mat_wdotui',msg='')
end subroutine test_mat_wdotui
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_wdotur()
   call unit_test_start('mat_wdotur',msg='')
   !!call unit_test('mat_wdotur', 0.eq.0, 'checking',100)
   call unit_test_done('mat_wdotur',msg='')
end subroutine test_mat_wdotur
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_wlog()
   call unit_test_start('mat_wlog',msg='')
   !!call unit_test('mat_wlog', 0.eq.0, 'checking',100)
   call unit_test_done('mat_wlog',msg='')
end subroutine test_mat_wlog
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_wmul()
   call unit_test_start('mat_wmul',msg='')
   !!call unit_test('mat_wmul', 0.eq.0, 'checking',100)
   call unit_test_done('mat_wmul',msg='')
end subroutine test_mat_wmul
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_wnrm2()
   call unit_test_start('mat_wnrm2',msg='')
   !!call unit_test('mat_wnrm2', 0.eq.0, 'checking',100)
   call unit_test_done('mat_wnrm2',msg='')
end subroutine test_mat_wnrm2
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_wpofa()
   call unit_test_start('mat_wpofa',msg='')
   !!call unit_test('mat_wpofa', 0.eq.0, 'checking',100)
   call unit_test_done('mat_wpofa',msg='')
end subroutine test_mat_wpofa
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_wrscal()
   call unit_test_start('mat_wrscal',msg='')
   !!call unit_test('mat_wrscal', 0.eq.0, 'checking',100)
   call unit_test_done('mat_wrscal',msg='')
end subroutine test_mat_wrscal
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_wscal()
   call unit_test_start('mat_wscal',msg='')
   !!call unit_test('mat_wscal', 0.eq.0, 'checking',100)
   call unit_test_done('mat_wscal',msg='')
end subroutine test_mat_wscal
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_wset()
   call unit_test_start('mat_wset',msg='')
   !!call unit_test('mat_wset', 0.eq.0, 'checking',100)
   call unit_test_done('mat_wset',msg='')
end subroutine test_mat_wset
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_wsign()
   call unit_test_start('mat_wsign',msg='')
   !!call unit_test('mat_wsign', 0.eq.0, 'checking',100)
   call unit_test_done('mat_wsign',msg='')
end subroutine test_mat_wsign
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_wsqrt()
   call unit_test_start('mat_wsqrt',msg='')
   !!call unit_test('mat_wsqrt', 0.eq.0, 'checking',100)
   call unit_test_done('mat_wsqrt',msg='')
end subroutine test_mat_wsqrt
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_wswap()
   call unit_test_start('mat_wswap',msg='')
   !!call unit_test('mat_wswap', 0.eq.0, 'checking',100)
   call unit_test_done('mat_wswap',msg='')
end subroutine test_mat_wswap
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_ml_comqr3_()
   call unit_test_start('ml_comqr3_',msg='')
   !!call unit_test('ml_comqr3_', 0.eq.0, 'checking',100)
   call unit_test_done('ml_comqr3_',msg='')
end subroutine test_ml_comqr3_
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_ml_corth_()
   call unit_test_start('ml_corth_',msg='')
   !!call unit_test('ml_corth_', 0.eq.0, 'checking',100)
   call unit_test_done('ml_corth_',msg='')
end subroutine test_ml_corth_
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_ml_htribk_()
   call unit_test_start('ml_htribk_',msg='')
   !!call unit_test('ml_htribk_', 0.eq.0, 'checking',100)
   call unit_test_done('ml_htribk_',msg='')
end subroutine test_ml_htribk_
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_ml_htridi_()
   call unit_test_start('ml_htridi_',msg='')
   !!call unit_test('ml_htridi_', 0.eq.0, 'checking',100)
   call unit_test_done('ml_htridi_',msg='')
end subroutine test_ml_htridi_
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_ml_imtql2_()
   call unit_test_start('ml_imtql2_',msg='')
   !!call unit_test('ml_imtql2_', 0.eq.0, 'checking',100)
   call unit_test_done('ml_imtql2_',msg='')
end subroutine test_ml_imtql2_
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_ml_wgeco_()
   call unit_test_start('ml_wgeco_',msg='')
   !!call unit_test('ml_wgeco_', 0.eq.0, 'checking',100)
   call unit_test_done('ml_wgeco_',msg='')
end subroutine test_ml_wgeco_
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_ml_wgedi_()
   call unit_test_start('ml_wgedi_',msg='')
   !!call unit_test('ml_wgedi_', 0.eq.0, 'checking',100)
   call unit_test_done('ml_wgedi_',msg='')
end subroutine test_ml_wgedi_
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_ml_wgefa_()
   call unit_test_start('ml_wgefa_',msg='')
   !!call unit_test('ml_wgefa_', 0.eq.0, 'checking',100)
   call unit_test_done('ml_wgefa_',msg='')
end subroutine test_ml_wgefa_
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_ml_wgesl_()
   !!call unit_test('ml_wgesl_', 0.eq.0, 'checking',100)
   call unit_test_done('ml_wgesl_',msg='')
end subroutine test_ml_wgesl_
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_ml_wqrdc_()
   call unit_test_start('ml_wqrdc_',msg='')
   !!call unit_test('ml_wqrdc_', 0.eq.0, 'checking',100)
   call unit_test_done('ml_wqrdc_',msg='')
end subroutine test_ml_wqrdc_
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_ml_wqrsl_()
   call unit_test_start('ml_wqrsl_',msg='')
   !!call unit_test('ml_wqrsl_', 0.eq.0, 'checking',100)
   call unit_test_done('ml_wqrsl_',msg='')
end subroutine test_ml_wqrsl_
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_ml_wsvdc_()
   call unit_test_start('ml_wsvdc_',msg='')
   !!call unit_test('ml_wsvdc_', 0.eq.0, 'checking',100)
   call unit_test_done('ml_wsvdc_',msg='')
end subroutine test_ml_wsvdc_
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
end program runtest
