!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
program test_suite_M_datapac
use, intrinsic :: ISO_FORTRAN_ENV, only : INT8, INT16, INT32, INT64       !  1           2           4           8
use, intrinsic :: ISO_FORTRAN_ENV, only : REAL32, REAL64, REAL128         !  4           8          10
use M_msg
use M_verify, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg
use M_verify, only : unit_check_level
use M_verify, only : unit_check_command, unit_check_keep_going, unit_check_level, unit_check_stop
use M_datapac
!use M_test_suite_M_anything
!use M_anything, only : anyinteger_to_string, anyscalar_to_int64
!use M_anything, only : anyscalar_to_real, anyscalar_to_double, anyscalar_to_real128
!use M_anything, only : anything_to_bytes, bytes_to_anything
!use M_anything, only : empty, assignment(=)

implicit none
!! setup
   unit_check_command=''
   unit_check_keep_going=.true.
   unit_check_level=0
!! test
   call test_autoco()
   call test_betran()
   call test_bincdf()
   call test_binppf()
   call test_binran()
   call test_caucdf()
   call test_caupdf()
   call test_cauplt()
   call test_cauppf()
   call test_cauran()
   call test_causf()
   call test_chscdf()
   call test_chsplt()
   call test_chsppf()
   call test_chsran()
   call test_code()
   call test_corr()
   call test_count()
   call test_decomp()
   call test_define()
   call test_delete()
   call test_demod()
   call test_dexcdf()
   call test_dexpdf()
   call test_dexplt()
   call test_dexppf()
   call test_dexran()
   call test_dexsf()
   call test_discr2()
   call test_discr3()
   call test_discre()
   call test_ev1cdf()
   call test_ev1plt()
   call test_ev1ppf()
   call test_ev1ran()
   call test_ev2cdf()
   call test_ev2plt()
   call test_ev2ppf()
   call test_ev2ran()
   call test_expcdf()
   call test_exppdf()
   call test_expplt()
   call test_expppf()
   call test_expran()
   call test_expsf()
   call test_extrem()
   call test_fcdf()
   call test_fourie()
   call test_fran()
   call test_freq()
   call test_gamcdf()
   call test_gamplt()
   call test_gamppf()
   call test_gamran()
   call test_geocdf()
   call test_geoplt()
   call test_geoppf()
   call test_georan()
   call test_hfncdf()
   call test_hfnplt()
   call test_hfnppf()
   call test_hfnran()
   call test_hist()
   call test_invxwx()
   call test_lamcdf()
   call test_lampdf()
   call test_lamplt()
   call test_lamppf()
   call test_lamran()
   call test_lamsf()
   call test_lgncdf()
   call test_lgnplt()
   call test_lgnppf()
   call test_lgnran()
   call test_loc()
   call test_logcdf()
   call test_logpdf()
   call test_logplt()
   call test_logppf()
   call test_logran()
   call test_logsf()
   call test_median()
   call test_move()
   call test_nbcdf()
   call test_nbppf()
   call test_nbran()
   call test_norcdf()
   call test_norout()
   call test_norpdf()
   call test_norplt()
   call test_norppf()
   call test_norran()
   call test_norsf()
   call test_parcdf()
   call test_parplt()
   call test_parppf()
   call test_parran()
   call test_plot()
   call test_plot10()
   call test_plot6()
   call test_plot7()
   call test_plot8()
   call test_plot9()
   call test_plotc()
   call test_plotco()
   call test_plotct()
   call test_plots()
   call test_plotsc()
   call test_plotsp()
   call test_plotst()
   call test_plott()
   call test_plotu()
   call test_plotx()
   call test_plotxt()
   call test_plotxx()
   call test_pltsct()
   call test_pltxxt()
   call test_poicdf()
   call test_poiplt()
   call test_poippf()
   call test_poiran()
   call test_poly()
   call test_propor()
   call test_range()
   call test_rank()
   call test_ranper()
   call test_read()
   call test_readg()
   call test_relsd()
   call test_replac()
   call test_retain()
   call test_runs()
   call test_sampp()
   call test_scale()
   call test_sd()
   call test_skipr()
   call test_sortp()
   call test_spcorr()
   call test_stmom3()
   call test_stmom4()
   call test_subse1()
   call test_subse2()
   call test_subset()
   call test_tail()
   call test_tcdf()
   call test_time()
   call test_tol()
   call test_tplt()
   call test_tppf()
   call test_tran()
   call test_trim()
   call test_unimed()
   call test_unipdf()
   call test_uniplt()
   call test_unippf()
   call test_uniran()
   call test_unisf()
   call test_weib()
   call test_weicdf()
   call test_weiplt()
   call test_weippf()
   call test_weiran()
   call test_wind()
   call test_write()

   call test_copy()
   call test_dot()
   call test_max()
   call test_mean()
   call test_midm()
   call test_midr()
   call test_min()
   call test_sort()
   call test_sortc()
   call test_unicdf()
   call test_var()
!! teardown
   call unit_check_stop()
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
contains
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_autoco()
   call unit_check_start('autoco',msg='')
   !!call unit_check('autoco', 0.eq.0, 'checking',100)
   call unit_check_done('autoco',msg='')
end subroutine test_autoco
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_betran()
   call unit_check_start('betran',msg='')
   !!call unit_check('betran', 0.eq.0, 'checking',100)
   call unit_check_done('betran',msg='')
end subroutine test_betran
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_bincdf()
   call unit_check_start('bincdf',msg='')
   !!call unit_check('bincdf', 0.eq.0, 'checking',100)
   call unit_check_done('bincdf',msg='')
end subroutine test_bincdf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_binppf()
   call unit_check_start('binppf',msg='')
   !!call unit_check('binppf', 0.eq.0, 'checking',100)
   call unit_check_done('binppf',msg='')
end subroutine test_binppf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_binran()
   call unit_check_start('binran',msg='')
   !!call unit_check('binran', 0.eq.0, 'checking',100)
   call unit_check_done('binran',msg='')
end subroutine test_binran
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_caucdf()
   call unit_check_start('caucdf',msg='')
   !!call unit_check('caucdf', 0.eq.0, 'checking',100)
   call unit_check_done('caucdf',msg='')
end subroutine test_caucdf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_caupdf()
   call unit_check_start('caupdf',msg='')
   !!call unit_check('caupdf', 0.eq.0, 'checking',100)
   call unit_check_done('caupdf',msg='')
end subroutine test_caupdf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_cauplt()
   call unit_check_start('cauplt',msg='')
   !!call unit_check('cauplt', 0.eq.0, 'checking',100)
   call unit_check_done('cauplt',msg='')
end subroutine test_cauplt
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_cauppf()
   call unit_check_start('cauppf',msg='')
   !!call unit_check('cauppf', 0.eq.0, 'checking',100)
   call unit_check_done('cauppf',msg='')
end subroutine test_cauppf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_cauran()
   call unit_check_start('cauran',msg='')
   !!call unit_check('cauran', 0.eq.0, 'checking',100)
   call unit_check_done('cauran',msg='')
end subroutine test_cauran
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_causf()
   call unit_check_start('causf',msg='')
   !!call unit_check('causf', 0.eq.0, 'checking',100)
   call unit_check_done('causf',msg='')
end subroutine test_causf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_chscdf()
   call unit_check_start('chscdf',msg='')
   !!call unit_check('chscdf', 0.eq.0, 'checking',100)
   call unit_check_done('chscdf',msg='')
end subroutine test_chscdf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_chsplt()
   call unit_check_start('chsplt',msg='')
   !!call unit_check('chsplt', 0.eq.0, 'checking',100)
   call unit_check_done('chsplt',msg='')
end subroutine test_chsplt
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_chsppf()
   call unit_check_start('chsppf',msg='')
   !!call unit_check('chsppf', 0.eq.0, 'checking',100)
   call unit_check_done('chsppf',msg='')
end subroutine test_chsppf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_chsran()
   call unit_check_start('chsran',msg='')
   !!call unit_check('chsran', 0.eq.0, 'checking',100)
   call unit_check_done('chsran',msg='')
end subroutine test_chsran
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_code()
   call unit_check_start('code',msg='')
   !!call unit_check('code', 0.eq.0, 'checking',100)
   call unit_check_done('code',msg='')
end subroutine test_code
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_copy()
real,allocatable :: from(:), to(:)
   call unit_check_start('copy',msg='')

   from=[1.0,2.0,3.0,4.0,5.0]
   to=[-1.0,-1.0,-1.0,-1.0,-1.0,-1.0]

   call copy(from,3,to)
   call unit_check('copy',all(to==[1.00,2.00,3.00,-1.00,-1.00,-1.00]),'simple')

   call copy([10.0,20.0,30.0],3,to(3:5))
   call unit_check('copy',all(to==[1.00,2.00,10.00,20.00,30.00,-1.00]),'subvector')

   call unit_check_done('copy',msg='')
end subroutine test_copy
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_corr()
   call unit_check_start('corr',msg='')
   !!call unit_check('corr', 0.eq.0, 'checking',100)
   call unit_check_done('corr',msg='')
end subroutine test_corr
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_count()
   call unit_check_start('count',msg='')
   !!call unit_check('count', 0.eq.0, 'checking',100)
   call unit_check_done('count',msg='')
end subroutine test_count
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_decomp()
   call unit_check_start('decomp',msg='')
   !!call unit_check('decomp', 0.eq.0, 'checking',100)
   call unit_check_done('decomp',msg='')
end subroutine test_decomp
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_define()
   call unit_check_start('define',msg='')
   !!call unit_check('define', 0.eq.0, 'checking',100)
   call unit_check_done('define',msg='')
end subroutine test_define
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_delete()
   call unit_check_start('delete',msg='')
   !!call unit_check('delete', 0.eq.0, 'checking',100)
   call unit_check_done('delete',msg='')
end subroutine test_delete
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_demod()
   call unit_check_start('demod',msg='')
   !!call unit_check('demod', 0.eq.0, 'checking',100)
   call unit_check_done('demod',msg='')
end subroutine test_demod
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_dexcdf()
   call unit_check_start('dexcdf',msg='')
   !!call unit_check('dexcdf', 0.eq.0, 'checking',100)
   call unit_check_done('dexcdf',msg='')
end subroutine test_dexcdf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_dexpdf()
   call unit_check_start('dexpdf',msg='')
   !!call unit_check('dexpdf', 0.eq.0, 'checking',100)
   call unit_check_done('dexpdf',msg='')
end subroutine test_dexpdf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_dexplt()
   call unit_check_start('dexplt',msg='')
   !!call unit_check('dexplt', 0.eq.0, 'checking',100)
   call unit_check_done('dexplt',msg='')
end subroutine test_dexplt
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_dexppf()
   call unit_check_start('dexppf',msg='')
   !!call unit_check('dexppf', 0.eq.0, 'checking',100)
   call unit_check_done('dexppf',msg='')
end subroutine test_dexppf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_dexran()
   call unit_check_start('dexran',msg='')
   !!call unit_check('dexran', 0.eq.0, 'checking',100)
   call unit_check_done('dexran',msg='')
end subroutine test_dexran
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_dexsf()
   call unit_check_start('dexsf',msg='')
   !!call unit_check('dexsf', 0.eq.0, 'checking',100)
   call unit_check_done('dexsf',msg='')
end subroutine test_dexsf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_discr2()
   call unit_check_start('discr2',msg='')
   !!call unit_check('discr2', 0.eq.0, 'checking',100)
   call unit_check_done('discr2',msg='')
end subroutine test_discr2
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_discr3()
   call unit_check_start('discr3',msg='')
   !!call unit_check('discr3', 0.eq.0, 'checking',100)
   call unit_check_done('discr3',msg='')
end subroutine test_discr3
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_discre()
   call unit_check_start('discre',msg='')
   !!call unit_check('discre', 0.eq.0, 'checking',100)
   call unit_check_done('discre',msg='')
end subroutine test_discre
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_dot()
real, dimension(3) :: a, b
real :: dotpro , parpro
integer i , imax , imin
   call unit_check_start('dot',msg='')
   a = [ 1.0, 2.0, 3.0 ]
   b = [ 4.0, 5.0, 6.0 ]
   imin=1
   imax=size(a)
   parpro=0.0
   call dot(a,b,imin,imax,parpro,dotpro)
   call unit_check('dot', dotpro == 32.0 ,'comparing',dotpro,dot_product(a,b))
   call unit_check_done('dot',msg='')
end subroutine test_dot
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_ev1cdf()
   call unit_check_start('ev1cdf',msg='')
   !!call unit_check('ev1cdf', 0.eq.0, 'checking',100)
   call unit_check_done('ev1cdf',msg='')
end subroutine test_ev1cdf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_ev1plt()
   call unit_check_start('ev1plt',msg='')
   !!call unit_check('ev1plt', 0.eq.0, 'checking',100)
   call unit_check_done('ev1plt',msg='')
end subroutine test_ev1plt
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_ev1ppf()
   call unit_check_start('ev1ppf',msg='')
   !!call unit_check('ev1ppf', 0.eq.0, 'checking',100)
   call unit_check_done('ev1ppf',msg='')
end subroutine test_ev1ppf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_ev1ran()
   call unit_check_start('ev1ran',msg='')
   !!call unit_check('ev1ran', 0.eq.0, 'checking',100)
   call unit_check_done('ev1ran',msg='')
end subroutine test_ev1ran
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_ev2cdf()
   call unit_check_start('ev2cdf',msg='')
   !!call unit_check('ev2cdf', 0.eq.0, 'checking',100)
   call unit_check_done('ev2cdf',msg='')
end subroutine test_ev2cdf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_ev2plt()
   call unit_check_start('ev2plt',msg='')
   !!call unit_check('ev2plt', 0.eq.0, 'checking',100)
   call unit_check_done('ev2plt',msg='')
end subroutine test_ev2plt
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_ev2ppf()
   call unit_check_start('ev2ppf',msg='')
   !!call unit_check('ev2ppf', 0.eq.0, 'checking',100)
   call unit_check_done('ev2ppf',msg='')
end subroutine test_ev2ppf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_ev2ran()
   call unit_check_start('ev2ran',msg='')
   !!call unit_check('ev2ran', 0.eq.0, 'checking',100)
   call unit_check_done('ev2ran',msg='')
end subroutine test_ev2ran
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_expcdf()
   call unit_check_start('expcdf',msg='')
   !!call unit_check('expcdf', 0.eq.0, 'checking',100)
   call unit_check_done('expcdf',msg='')
end subroutine test_expcdf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_exppdf()
   call unit_check_start('exppdf',msg='')
   !!call unit_check('exppdf', 0.eq.0, 'checking',100)
   call unit_check_done('exppdf',msg='')
end subroutine test_exppdf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_expplt()
   call unit_check_start('expplt',msg='')
   !!call unit_check('expplt', 0.eq.0, 'checking',100)
   call unit_check_done('expplt',msg='')
end subroutine test_expplt
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_expppf()
   call unit_check_start('expppf',msg='')
   !!call unit_check('expppf', 0.eq.0, 'checking',100)
   call unit_check_done('expppf',msg='')
end subroutine test_expppf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_expran()
   call unit_check_start('expran',msg='')
   !!call unit_check('expran', 0.eq.0, 'checking',100)
   call unit_check_done('expran',msg='')
end subroutine test_expran
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_expsf()
   call unit_check_start('expsf',msg='')
   !!call unit_check('expsf', 0.eq.0, 'checking',100)
   call unit_check_done('expsf',msg='')
end subroutine test_expsf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_extrem()
   call unit_check_start('extrem',msg='')
   !!call unit_check('extrem', 0.eq.0, 'checking',100)
   call unit_check_done('extrem',msg='')
end subroutine test_extrem
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_fcdf()
   call unit_check_start('fcdf',msg='')
   !!call unit_check('fcdf', 0.eq.0, 'checking',100)
   call unit_check_done('fcdf',msg='')
end subroutine test_fcdf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_fourie()
   call unit_check_start('fourie',msg='')
   !!call unit_check('fourie', 0.eq.0, 'checking',100)
   call unit_check_done('fourie',msg='')
end subroutine test_fourie
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_fran()
   call unit_check_start('fran',msg='')
   !!call unit_check('fran', 0.eq.0, 'checking',100)
   call unit_check_done('fran',msg='')
end subroutine test_fran
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_freq()
   call unit_check_start('freq',msg='')
   !!call unit_check('freq', 0.eq.0, 'checking',100)
   call unit_check_done('freq',msg='')
end subroutine test_freq
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_gamcdf()
   call unit_check_start('gamcdf',msg='')
   !!call unit_check('gamcdf', 0.eq.0, 'checking',100)
   call unit_check_done('gamcdf',msg='')
end subroutine test_gamcdf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_gamplt()
   call unit_check_start('gamplt',msg='')
   !!call unit_check('gamplt', 0.eq.0, 'checking',100)
   call unit_check_done('gamplt',msg='')
end subroutine test_gamplt
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_gamppf()
   call unit_check_start('gamppf',msg='')
   !!call unit_check('gamppf', 0.eq.0, 'checking',100)
   call unit_check_done('gamppf',msg='')
end subroutine test_gamppf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_gamran()
   call unit_check_start('gamran',msg='')
   !!call unit_check('gamran', 0.eq.0, 'checking',100)
   call unit_check_done('gamran',msg='')
end subroutine test_gamran
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_geocdf()
   call unit_check_start('geocdf',msg='')
   !!call unit_check('geocdf', 0.eq.0, 'checking',100)
   call unit_check_done('geocdf',msg='')
end subroutine test_geocdf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_geoplt()
   call unit_check_start('geoplt',msg='')
   !!call unit_check('geoplt', 0.eq.0, 'checking',100)
   call unit_check_done('geoplt',msg='')
end subroutine test_geoplt
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_geoppf()
   call unit_check_start('geoppf',msg='')
   !!call unit_check('geoppf', 0.eq.0, 'checking',100)
   call unit_check_done('geoppf',msg='')
end subroutine test_geoppf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_georan()
   call unit_check_start('georan',msg='')
   !!call unit_check('georan', 0.eq.0, 'checking',100)
   call unit_check_done('georan',msg='')
end subroutine test_georan
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_hfncdf()
   call unit_check_start('hfncdf',msg='')
   !!call unit_check('hfncdf', 0.eq.0, 'checking',100)
   call unit_check_done('hfncdf',msg='')
end subroutine test_hfncdf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_hfnplt()
   call unit_check_start('hfnplt',msg='')
   !!call unit_check('hfnplt', 0.eq.0, 'checking',100)
   call unit_check_done('hfnplt',msg='')
end subroutine test_hfnplt
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_hfnppf()
   call unit_check_start('hfnppf',msg='')
   !!call unit_check('hfnppf', 0.eq.0, 'checking',100)
   call unit_check_done('hfnppf',msg='')
end subroutine test_hfnppf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_hfnran()
   call unit_check_start('hfnran',msg='')
   !!call unit_check('hfnran', 0.eq.0, 'checking',100)
   call unit_check_done('hfnran',msg='')
end subroutine test_hfnran
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_hist()
   call unit_check_start('hist',msg='')
   !!call unit_check('hist', 0.eq.0, 'checking',100)
   call unit_check_done('hist',msg='')
end subroutine test_hist
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_invxwx()
   call unit_check_start('invxwx',msg='')
   !!call unit_check('invxwx', 0.eq.0, 'checking',100)
   call unit_check_done('invxwx',msg='')
end subroutine test_invxwx
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_lamcdf()
   call unit_check_start('lamcdf',msg='')
   !!call unit_check('lamcdf', 0.eq.0, 'checking',100)
   call unit_check_done('lamcdf',msg='')
end subroutine test_lamcdf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_lampdf()
   call unit_check_start('lampdf',msg='')
   !!call unit_check('lampdf', 0.eq.0, 'checking',100)
   call unit_check_done('lampdf',msg='')
end subroutine test_lampdf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_lamplt()
   call unit_check_start('lamplt',msg='')
   !!call unit_check('lamplt', 0.eq.0, 'checking',100)
   call unit_check_done('lamplt',msg='')
end subroutine test_lamplt
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_lamppf()
   call unit_check_start('lamppf',msg='')
   !!call unit_check('lamppf', 0.eq.0, 'checking',100)
   call unit_check_done('lamppf',msg='')
end subroutine test_lamppf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_lamran()
   call unit_check_start('lamran',msg='')
   !!call unit_check('lamran', 0.eq.0, 'checking',100)
   call unit_check_done('lamran',msg='')
end subroutine test_lamran
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_lamsf()
   call unit_check_start('lamsf',msg='')
   !!call unit_check('lamsf', 0.eq.0, 'checking',100)
   call unit_check_done('lamsf',msg='')
end subroutine test_lamsf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_lgncdf()
   call unit_check_start('lgncdf',msg='')
   !!call unit_check('lgncdf', 0.eq.0, 'checking',100)
   call unit_check_done('lgncdf',msg='')
end subroutine test_lgncdf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_lgnplt()
   call unit_check_start('lgnplt',msg='')
   !!call unit_check('lgnplt', 0.eq.0, 'checking',100)
   call unit_check_done('lgnplt',msg='')
end subroutine test_lgnplt
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_lgnppf()
   call unit_check_start('lgnppf',msg='')
   !!call unit_check('lgnppf', 0.eq.0, 'checking',100)
   call unit_check_done('lgnppf',msg='')
end subroutine test_lgnppf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_lgnran()
   call unit_check_start('lgnran',msg='')
   !!call unit_check('lgnran', 0.eq.0, 'checking',100)
   call unit_check_done('lgnran',msg='')
end subroutine test_lgnran
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_loc()
   call unit_check_start('loc',msg='')
   !!call unit_check('loc', 0.eq.0, 'checking',100)
   call unit_check_done('loc',msg='')
end subroutine test_loc
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_logcdf()
   call unit_check_start('logcdf',msg='')
   !!call unit_check('logcdf', 0.eq.0, 'checking',100)
   call unit_check_done('logcdf',msg='')
end subroutine test_logcdf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_logpdf()
   call unit_check_start('logpdf',msg='')
   !!call unit_check('logpdf', 0.eq.0, 'checking',100)
   call unit_check_done('logpdf',msg='')
end subroutine test_logpdf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_logplt()
   call unit_check_start('logplt',msg='')
   !!call unit_check('logplt', 0.eq.0, 'checking',100)
   call unit_check_done('logplt',msg='')
end subroutine test_logplt
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_logppf()
   call unit_check_start('logppf',msg='')
   !!call unit_check('logppf', 0.eq.0, 'checking',100)
   call unit_check_done('logppf',msg='')
end subroutine test_logppf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_logran()
   call unit_check_start('logran',msg='')
   !!call unit_check('logran', 0.eq.0, 'checking',100)
   call unit_check_done('logran',msg='')
end subroutine test_logran
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_logsf()
   call unit_check_start('logsf',msg='')
   !!call unit_check('logsf', 0.eq.0, 'checking',100)
   call unit_check_done('logsf',msg='')
end subroutine test_logsf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_max()
use M_datapac, only : intel_max=>max
real :: xmax
real(kind=real64) :: dmax

   call unit_check_start('max',msg='')

   call intel_max([-100.0, 200.0, 0.0, 400.0, -200.0],5,1,xmax)
   call unit_check('max', xmax.eq.400.0, 'checking',xmax,400.0)

   call intel_max([-100.0d0, 200.0d0, 0.0d0, 400.0d0, -200.0d0],5,1,dmax)
   call unit_check('max', dmax.eq.400.0d0, 'checking',dmax,400.0d0)

   call unit_check_done('max',msg='')

end subroutine test_max
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mean()
real :: sp_mean
double precision :: dp_mean
   call unit_check_start('mean',msg='')
   call mean([4.0, 36.0, 45.0, 50.0, 75.0], 5, 1, sp_mean)
   call unit_check('mean', sp_mean.eq.42.0, 'checking',sp_mean,42.0)
   call mean([4.0d0, 36.0d0, 45.0d0, 50.0d0, 75.0d0], 5, 1, dp_mean)
   call unit_check('mean', dp_mean.eq.42.0d0, 'checking',dp_mean,42.0d0)
   call mean([4.0d0], 1, 1, dp_mean)
   call unit_check('mean', dp_mean.eq.4.0d0, 'checking',dp_mean,4.0d0)
   call unit_check_done('mean',msg='')
end subroutine test_mean
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_median()
   call unit_check_start('median',msg='')
   !!call unit_check('median', 0.eq.0, 'checking',100)
   call unit_check_done('median',msg='')
end subroutine test_median
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_midm()
   integer :: i
   real :: xmidm
   call unit_check_start('midm',msg='')

   call midm([real :: (i,i=0,100) ],101,1,xmidm)
   call unit_check('midm', xmidm==50.0, 'expecting',50.0,'got',xmidm)

   call midm([real :: (i,i=0,101) ],102,1,xmidm)
   call unit_check('midm', xmidm==50.5, 'expecting',50.5,'got',xmidm)

   call unit_check_done('midm',msg='')
end subroutine test_midm
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_midr()
   integer :: i
   real :: xmidr
   call unit_check_start('midr',msg='')

   call midr([real :: (i,i=0,100) ],101,1,xmidr)
   call unit_check('midr', xmidr==50.0, 'expecting',50.0,'got',xmidr)

   call midr([real :: (i,i=0,101) ],102,1,xmidr)
   call unit_check('midr', xmidr==50.5, 'expecting',50.5,'got',xmidr)

   call unit_check_done('midr',msg='')
end subroutine test_midr
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_min()
real :: xmin
real(kind=real64) :: dmin

   call unit_check_start('min',msg='')

   call min([-100.0, 200.0, 0.0, 400.0, -200.0],5,1,xmin)
   call unit_check('min', xmin.eq.-200.0, 'checking',xmin,-200.0)

   call min([-100.0d0, 200.0d0, 0.0d0, 400.0d0, -200.0d0],5,1,dmin)
   call unit_check('min', dmin.eq.-200.0d0, 'checking',dmin,-200.0d0)

   call unit_check_done('min',msg='')

end subroutine test_min
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_move()
   call unit_check_start('move',msg='')
   !!call unit_check('move', 0.eq.0, 'checking',100)
   call unit_check_done('move',msg='')
end subroutine test_move
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_nbcdf()
   call unit_check_start('nbcdf',msg='')
   !!call unit_check('nbcdf', 0.eq.0, 'checking',100)
   call unit_check_done('nbcdf',msg='')
end subroutine test_nbcdf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_nbppf()
   call unit_check_start('nbppf',msg='')
   !!call unit_check('nbppf', 0.eq.0, 'checking',100)
   call unit_check_done('nbppf',msg='')
end subroutine test_nbppf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_nbran()
   call unit_check_start('nbran',msg='')
   !!call unit_check('nbran', 0.eq.0, 'checking',100)
   call unit_check_done('nbran',msg='')
end subroutine test_nbran
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_norcdf()
   call unit_check_start('norcdf',msg='')
   !!call unit_check('norcdf', 0.eq.0, 'checking',100)
   call unit_check_done('norcdf',msg='')
end subroutine test_norcdf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_norout()
   call unit_check_start('norout',msg='')
   !!call unit_check('norout', 0.eq.0, 'checking',100)
   call unit_check_done('norout',msg='')
end subroutine test_norout
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_norpdf()
   call unit_check_start('norpdf',msg='')
   !!call unit_check('norpdf', 0.eq.0, 'checking',100)
   call unit_check_done('norpdf',msg='')
end subroutine test_norpdf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_norplt()
   call unit_check_start('norplt',msg='')
   !!call unit_check('norplt', 0.eq.0, 'checking',100)
   call unit_check_done('norplt',msg='')
end subroutine test_norplt
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_norppf()
   call unit_check_start('norppf',msg='')
   !!call unit_check('norppf', 0.eq.0, 'checking',100)
   call unit_check_done('norppf',msg='')
end subroutine test_norppf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_norran()
   call unit_check_start('norran',msg='')
   !!call unit_check('norran', 0.eq.0, 'checking',100)
   call unit_check_done('norran',msg='')
end subroutine test_norran
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_norsf()
   call unit_check_start('norsf',msg='')
   !!call unit_check('norsf', 0.eq.0, 'checking',100)
   call unit_check_done('norsf',msg='')
end subroutine test_norsf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_parcdf()
   call unit_check_start('parcdf',msg='')
   !!call unit_check('parcdf', 0.eq.0, 'checking',100)
   call unit_check_done('parcdf',msg='')
end subroutine test_parcdf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_parplt()
   call unit_check_start('parplt',msg='')
   !!call unit_check('parplt', 0.eq.0, 'checking',100)
   call unit_check_done('parplt',msg='')
end subroutine test_parplt
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_parppf()
   call unit_check_start('parppf',msg='')
   !!call unit_check('parppf', 0.eq.0, 'checking',100)
   call unit_check_done('parppf',msg='')
end subroutine test_parppf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_parran()
   call unit_check_start('parran',msg='')
   !!call unit_check('parran', 0.eq.0, 'checking',100)
   call unit_check_done('parran',msg='')
end subroutine test_parran
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_plot()
   call unit_check_start('plot',msg='')
   !!call unit_check('plot', 0.eq.0, 'checking',100)
   call unit_check_done('plot',msg='')
end subroutine test_plot
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_plot10()
   call unit_check_start('plot10',msg='')
   !!call unit_check('plot10', 0.eq.0, 'checking',100)
   call unit_check_done('plot10',msg='')
end subroutine test_plot10
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_plot6()
   call unit_check_start('plot6',msg='')
   !!call unit_check('plot6', 0.eq.0, 'checking',100)
   call unit_check_done('plot6',msg='')
end subroutine test_plot6
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_plot7()
   call unit_check_start('plot7',msg='')
   !!call unit_check('plot7', 0.eq.0, 'checking',100)
   call unit_check_done('plot7',msg='')
end subroutine test_plot7
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_plot8()
   call unit_check_start('plot8',msg='')
   !!call unit_check('plot8', 0.eq.0, 'checking',100)
   call unit_check_done('plot8',msg='')
end subroutine test_plot8
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_plot9()
   call unit_check_start('plot9',msg='')
   !!call unit_check('plot9', 0.eq.0, 'checking',100)
   call unit_check_done('plot9',msg='')
end subroutine test_plot9
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_plotc()
   call unit_check_start('plotc',msg='')
   !!call unit_check('plotc', 0.eq.0, 'checking',100)
   call unit_check_done('plotc',msg='')
end subroutine test_plotc
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_plotco()
   call unit_check_start('plotco',msg='')
   !!call unit_check('plotco', 0.eq.0, 'checking',100)
   call unit_check_done('plotco',msg='')
end subroutine test_plotco
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_plotct()
   call unit_check_start('plotct',msg='')
   !!call unit_check('plotct', 0.eq.0, 'checking',100)
   call unit_check_done('plotct',msg='')
end subroutine test_plotct
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_plots()
   call unit_check_start('plots',msg='')
   !!call unit_check('plots', 0.eq.0, 'checking',100)
   call unit_check_done('plots',msg='')
end subroutine test_plots
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_plotsc()
   call unit_check_start('plotsc',msg='')
   !!call unit_check('plotsc', 0.eq.0, 'checking',100)
   call unit_check_done('plotsc',msg='')
end subroutine test_plotsc
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_plotsp()
   call unit_check_start('plotsp',msg='')
   !!call unit_check('plotsp', 0.eq.0, 'checking',100)
   call unit_check_done('plotsp',msg='')
end subroutine test_plotsp
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_plotst()
   call unit_check_start('plotst',msg='')
   !!call unit_check('plotst', 0.eq.0, 'checking',100)
   call unit_check_done('plotst',msg='')
end subroutine test_plotst
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_plott()
   call unit_check_start('plott',msg='')
   !!call unit_check('plott', 0.eq.0, 'checking',100)
   call unit_check_done('plott',msg='')
end subroutine test_plott
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_plotu()
   call unit_check_start('plotu',msg='')
   !!call unit_check('plotu', 0.eq.0, 'checking',100)
   call unit_check_done('plotu',msg='')
end subroutine test_plotu
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_plotx()
   call unit_check_start('plotx',msg='')
   !!call unit_check('plotx', 0.eq.0, 'checking',100)
   call unit_check_done('plotx',msg='')
end subroutine test_plotx
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_plotxt()
   call unit_check_start('plotxt',msg='')
   !!call unit_check('plotxt', 0.eq.0, 'checking',100)
   call unit_check_done('plotxt',msg='')
end subroutine test_plotxt
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_plotxx()
   call unit_check_start('plotxx',msg='')
   !!call unit_check('plotxx', 0.eq.0, 'checking',100)
   call unit_check_done('plotxx',msg='')
end subroutine test_plotxx
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_pltsct()
   call unit_check_start('pltsct',msg='')
   !!call unit_check('pltsct', 0.eq.0, 'checking',100)
   call unit_check_done('pltsct',msg='')
end subroutine test_pltsct
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_pltxxt()
   call unit_check_start('pltxxt',msg='')
   !!call unit_check('pltxxt', 0.eq.0, 'checking',100)
   call unit_check_done('pltxxt',msg='')
end subroutine test_pltxxt
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_poicdf()
   call unit_check_start('poicdf',msg='')
   !!call unit_check('poicdf', 0.eq.0, 'checking',100)
   call unit_check_done('poicdf',msg='')
end subroutine test_poicdf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_poiplt()
   call unit_check_start('poiplt',msg='')
   !!call unit_check('poiplt', 0.eq.0, 'checking',100)
   call unit_check_done('poiplt',msg='')
end subroutine test_poiplt
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_poippf()
   call unit_check_start('poippf',msg='')
   !!call unit_check('poippf', 0.eq.0, 'checking',100)
   call unit_check_done('poippf',msg='')
end subroutine test_poippf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_poiran()
   call unit_check_start('poiran',msg='')
   !!call unit_check('poiran', 0.eq.0, 'checking',100)
   call unit_check_done('poiran',msg='')
end subroutine test_poiran
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_poly()
   call unit_check_start('poly',msg='')
   !!call unit_check('poly', 0.eq.0, 'checking',100)
   call unit_check_done('poly',msg='')
end subroutine test_poly
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_propor()
   call unit_check_start('propor',msg='')
   !!call unit_check('propor', 0.eq.0, 'checking',100)
   call unit_check_done('propor',msg='')
end subroutine test_propor
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_range()
   call unit_check_start('range',msg='')
   !!call unit_check('range', 0.eq.0, 'checking',100)
   call unit_check_done('range',msg='')
end subroutine test_range
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_rank()
   call unit_check_start('rank',msg='')
   !!call unit_check('rank', 0.eq.0, 'checking',100)
   call unit_check_done('rank',msg='')
end subroutine test_rank
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_ranper()
   call unit_check_start('ranper',msg='')
   !!call unit_check('ranper', 0.eq.0, 'checking',100)
   call unit_check_done('ranper',msg='')
end subroutine test_ranper
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_read()
   call unit_check_start('read',msg='')
   !!call unit_check('read', 0.eq.0, 'checking',100)
   call unit_check_done('read',msg='')
end subroutine test_read
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_readg()
   call unit_check_start('readg',msg='')
   !!call unit_check('readg', 0.eq.0, 'checking',100)
   call unit_check_done('readg',msg='')
end subroutine test_readg
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_relsd()
   call unit_check_start('relsd',msg='')
   !!call unit_check('relsd', 0.eq.0, 'checking',100)
   call unit_check_done('relsd',msg='')
end subroutine test_relsd
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_replac()
   call unit_check_start('replac',msg='')
   !!call unit_check('replac', 0.eq.0, 'checking',100)
   call unit_check_done('replac',msg='')
end subroutine test_replac
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_retain()
   call unit_check_start('retain',msg='')
   !!call unit_check('retain', 0.eq.0, 'checking',100)
   call unit_check_done('retain',msg='')
end subroutine test_retain
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_runs()
   call unit_check_start('runs',msg='')
   !!call unit_check('runs', 0.eq.0, 'checking',100)
   call unit_check_done('runs',msg='')
end subroutine test_runs
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_sampp()
   call unit_check_start('sampp',msg='')
   !!call unit_check('sampp', 0.eq.0, 'checking',100)
   call unit_check_done('sampp',msg='')
end subroutine test_sampp
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_scale()
   call unit_check_start('scale',msg='')
   !!call unit_check('scale', 0.eq.0, 'checking',100)
   call unit_check_done('scale',msg='')
end subroutine test_scale
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_sd()
   call unit_check_start('sd',msg='')
   !!call unit_check('sd', 0.eq.0, 'checking',100)
   call unit_check_done('sd',msg='')
end subroutine test_sd
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_skipr()
   call unit_check_start('skipr',msg='')
   !!call unit_check('skipr', 0.eq.0, 'checking',100)
   call unit_check_done('skipr',msg='')
end subroutine test_skipr
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_sort()
integer,parameter            :: isz=20   
real                         :: aa(isz)
real                         :: bb(isz)
integer                      :: i
integer                      :: ibad
   ibad=0
   call unit_check_start('sort',msg='')
   call random_seed()
   CALL RANDOM_NUMBER(aa)
   aa=aa*450000.0
   bb=real([(i,i=1,isz)])
   call sort(aa,isz,bb)
   do i=1,isz-1
      if(bb(i).gt.bb(i+1))then
         write(*,*)'Error in sorting reals small to large ',i,bb(i),bb(i+1)
         ibad=ibad+1
      endif
   enddo
   call unit_check('sort', ibad.eq.0, 'checking',100)
   call unit_check_done('sort',msg='')
end subroutine test_sort
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_sortc()
integer,parameter            :: isz=2000
real                         :: aa(isz)
real                         :: bb(isz)
real                         :: cc(isz)
real                         :: dd(isz)
real                         :: cc2(isz)
real                         :: dd2(isz)
integer                      :: i
integer                      :: ibad
   ibad=0
   call unit_check_start('sortc',msg='')
   call random_seed()
   CALL RANDOM_NUMBER(aa)
   aa=aa*450000.0
   bb=real([(i,i=1,isz)])
   call sortc(aa,bb,size(aa),cc,dd)
   do i=1,isz-1 ! checking if real values are sorted
      if(cc(i).gt.cc(i+1))then
         write(*,*)'Error in sorting reals small to large ',i,cc(i),cc(i+1)
         ibad=ibad+1
      endif
   enddo
   call unit_check('sortc', ibad.eq.0, 'checking ascending')
   call sortc(dd,cc,isz,dd2,cc2) ! put dd and cc back in original order
   call unit_check('sortc', all(cc2.eq.aa), 'checking reversed')
   call unit_check('sortc', all(dd2.eq.bb), 'checking reversed')
   call unit_check_done('sortc',msg='')
end subroutine test_sortc
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_sortp()
   call unit_check_start('sortp',msg='')
   !!call unit_check('sortp', 0.eq.0, 'checking',100)
   call unit_check_done('sortp',msg='')
end subroutine test_sortp
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_spcorr()
   call unit_check_start('spcorr',msg='')
   !!call unit_check('spcorr', 0.eq.0, 'checking',100)
   call unit_check_done('spcorr',msg='')
end subroutine test_spcorr
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_stmom3()
   call unit_check_start('stmom3',msg='')
   !!call unit_check('stmom3', 0.eq.0, 'checking',100)
   call unit_check_done('stmom3',msg='')
end subroutine test_stmom3
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_stmom4()
   call unit_check_start('stmom4',msg='')
   !!call unit_check('stmom4', 0.eq.0, 'checking',100)
   call unit_check_done('stmom4',msg='')
end subroutine test_stmom4
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_subse1()
   call unit_check_start('subse1',msg='')
   !!call unit_check('subse1', 0.eq.0, 'checking',100)
   call unit_check_done('subse1',msg='')
end subroutine test_subse1
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_subse2()
   call unit_check_start('subse2',msg='')
   !!call unit_check('subse2', 0.eq.0, 'checking',100)
   call unit_check_done('subse2',msg='')
end subroutine test_subse2
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_subset()
   call unit_check_start('subset',msg='')
   !!call unit_check('subset', 0.eq.0, 'checking',100)
   call unit_check_done('subset',msg='')
end subroutine test_subset
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_tail()
   call unit_check_start('tail',msg='')
   !!call unit_check('tail', 0.eq.0, 'checking',100)
   call unit_check_done('tail',msg='')
end subroutine test_tail
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_tcdf()
   call unit_check_start('tcdf',msg='')
   !!call unit_check('tcdf', 0.eq.0, 'checking',100)
   call unit_check_done('tcdf',msg='')
end subroutine test_tcdf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_time()
   call unit_check_start('time',msg='')
   !!call unit_check('time', 0.eq.0, 'checking',100)
   call unit_check_done('time',msg='')
end subroutine test_time
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_tol()
   call unit_check_start('tol',msg='')
   !!call unit_check('tol', 0.eq.0, 'checking',100)
   call unit_check_done('tol',msg='')
end subroutine test_tol
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_tplt()
   call unit_check_start('tplt',msg='')
   !!call unit_check('tplt', 0.eq.0, 'checking',100)
   call unit_check_done('tplt',msg='')
end subroutine test_tplt
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_tppf()
   call unit_check_start('tppf',msg='')
   !!call unit_check('tppf', 0.eq.0, 'checking',100)
   call unit_check_done('tppf',msg='')
end subroutine test_tppf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_tran()
   call unit_check_start('tran',msg='')
   !!call unit_check('tran', 0.eq.0, 'checking',100)
   call unit_check_done('tran',msg='')
end subroutine test_tran
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_trim()
   call unit_check_start('trim',msg='')
   !!call unit_check('trim', 0.eq.0, 'checking',100)
   call unit_check_done('trim',msg='')
end subroutine test_trim
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_unicdf()
integer,parameter :: n=40
real              :: x(0:n), y(0:n)
integer           :: i
   call unit_check_start('unicdf',msg='')
   x=[(real(i)/real(n),i=0,n)]
   do i=0,n
         call unicdf(x(i),y(i))
   enddo
   call unit_check('unicdf', all(x.eq.y), 'checking in equals out')
   call unit_check_done('unicdf',msg='')
end subroutine test_unicdf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_unimed()
   call unit_check_start('unimed',msg='')
   !!call unit_check('unimed', 0.eq.0, 'checking',100)
   call unit_check_done('unimed',msg='')
end subroutine test_unimed
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_unipdf()
   call unit_check_start('unipdf',msg='')
   !!call unit_check('unipdf', 0.eq.0, 'checking',100)
   call unit_check_done('unipdf',msg='')
end subroutine test_unipdf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_uniplt()
   call unit_check_start('uniplt',msg='')
   !!call unit_check('uniplt', 0.eq.0, 'checking',100)
   call unit_check_done('uniplt',msg='')
end subroutine test_uniplt
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_unippf()
   call unit_check_start('unippf',msg='')
   !!call unit_check('unippf', 0.eq.0, 'checking',100)
   call unit_check_done('unippf',msg='')
end subroutine test_unippf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_uniran()
   call unit_check_start('uniran',msg='')
   !!call unit_check('uniran', 0.eq.0, 'checking',100)
   call unit_check_done('uniran',msg='')
end subroutine test_uniran
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_unisf()
   call unit_check_start('unisf',msg='')
   !!call unit_check('unisf', 0.eq.0, 'checking',100)
   call unit_check_done('unisf',msg='')
end subroutine test_unisf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_var()
   real,allocatable :: x(:)
   real :: Xvar
   call unit_check_start('var',msg='')
   x = [46.0, 69.0, 32.0, 60.0, 52.0, 41.0]
   call VAR(X,size(x),1,Xvar)
   call unit_check('var', Xvar == 177.2 ,'got',Xvar,'expecting',177.2)
   call unit_check_done('var',msg='')
end subroutine test_var
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_weib()
   call unit_check_start('weib',msg='')
   !!call unit_check('weib', 0.eq.0, 'checking',100)
   call unit_check_done('weib',msg='')
end subroutine test_weib
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_weicdf()
   call unit_check_start('weicdf',msg='')
   !!call unit_check('weicdf', 0.eq.0, 'checking',100)
   call unit_check_done('weicdf',msg='')
end subroutine test_weicdf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_weiplt()
   call unit_check_start('weiplt',msg='')
   !!call unit_check('weiplt', 0.eq.0, 'checking',100)
   call unit_check_done('weiplt',msg='')
end subroutine test_weiplt
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_weippf()
   call unit_check_start('weippf',msg='')
   !!call unit_check('weippf', 0.eq.0, 'checking',100)
   call unit_check_done('weippf',msg='')
end subroutine test_weippf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_weiran()
   call unit_check_start('weiran',msg='')
   !!call unit_check('weiran', 0.eq.0, 'checking',100)
   call unit_check_done('weiran',msg='')
end subroutine test_weiran
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_wind()
   call unit_check_start('wind',msg='')
   !!call unit_check('wind', 0.eq.0, 'checking',100)
   call unit_check_done('wind',msg='')
end subroutine test_wind
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_write()
   call unit_check_start('write',msg='')
   !!call unit_check('write', 0.eq.0, 'checking',100)
   call unit_check_done('write',msg='')
end subroutine test_write
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
end program test_suite_M_datapac
