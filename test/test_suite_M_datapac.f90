!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
program test_suite_M_datapac
use, intrinsic :: ISO_FORTRAN_ENV, only : INT8, INT16, INT32, INT64       !  1           2           4           8
use, intrinsic :: ISO_FORTRAN_ENV, only : REAL32, REAL64, REAL128         !  4           8          10
use M_framework__msg
use M_framework__verify, only : unit_test_start,unit_test,unit_test_done,unit_test_good,unit_test_bad,unit_test_msg
use M_framework__verify, only : unit_test_stop
use M_datapac
!use M_test_suite_M_anything
!use M_anything, only : anyinteger_to_string, anyscalar_to_int64
!use M_anything, only : anyscalar_to_real, anyscalar_to_double, anyscalar_to_real128
!use M_anything, only : anything_to_bytes, bytes_to_anything
!use M_anything, only : empty, assignment(=)

implicit none
!! setup
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
   call unit_test_stop()
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
contains
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_autoco()
   call unit_test_start('autoco',msg='')
   !!call unit_test('autoco', 0.eq.0, 'checking',100)
   call unit_test_done('autoco',msg='')
end subroutine test_autoco
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_betran()
   call unit_test_start('betran',msg='')
   !!call unit_test('betran', 0.eq.0, 'checking',100)
   call unit_test_done('betran',msg='')
end subroutine test_betran
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_bincdf()
   call unit_test_start('bincdf',msg='')
   !!call unit_test('bincdf', 0.eq.0, 'checking',100)
   call unit_test_done('bincdf',msg='')
end subroutine test_bincdf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_binppf()
   call unit_test_start('binppf',msg='')
   !!call unit_test('binppf', 0.eq.0, 'checking',100)
   call unit_test_done('binppf',msg='')
end subroutine test_binppf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_binran()
   call unit_test_start('binran',msg='')
   !!call unit_test('binran', 0.eq.0, 'checking',100)
   call unit_test_done('binran',msg='')
end subroutine test_binran
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_caucdf()
   call unit_test_start('caucdf',msg='')
   !!call unit_test('caucdf', 0.eq.0, 'checking',100)
   call unit_test_done('caucdf',msg='')
end subroutine test_caucdf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_caupdf()
   call unit_test_start('caupdf',msg='')
   !!call unit_test('caupdf', 0.eq.0, 'checking',100)
   call unit_test_done('caupdf',msg='')
end subroutine test_caupdf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_cauplt()
   call unit_test_start('cauplt',msg='')
   !!call unit_test('cauplt', 0.eq.0, 'checking',100)
   call unit_test_done('cauplt',msg='')
end subroutine test_cauplt
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_cauppf()
   call unit_test_start('cauppf',msg='')
   !!call unit_test('cauppf', 0.eq.0, 'checking',100)
   call unit_test_done('cauppf',msg='')
end subroutine test_cauppf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_cauran()
   call unit_test_start('cauran',msg='')
   !!call unit_test('cauran', 0.eq.0, 'checking',100)
   call unit_test_done('cauran',msg='')
end subroutine test_cauran
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_causf()
   call unit_test_start('causf',msg='')
   !!call unit_test('causf', 0.eq.0, 'checking',100)
   call unit_test_done('causf',msg='')
end subroutine test_causf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_chscdf()
   call unit_test_start('chscdf',msg='')
   !!call unit_test('chscdf', 0.eq.0, 'checking',100)
   call unit_test_done('chscdf',msg='')
end subroutine test_chscdf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_chsplt()
   call unit_test_start('chsplt',msg='')
   !!call unit_test('chsplt', 0.eq.0, 'checking',100)
   call unit_test_done('chsplt',msg='')
end subroutine test_chsplt
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_chsppf()
   call unit_test_start('chsppf',msg='')
   !!call unit_test('chsppf', 0.eq.0, 'checking',100)
   call unit_test_done('chsppf',msg='')
end subroutine test_chsppf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_chsran()
   call unit_test_start('chsran',msg='')
   !!call unit_test('chsran', 0.eq.0, 'checking',100)
   call unit_test_done('chsran',msg='')
end subroutine test_chsran
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_code()
   call unit_test_start('code',msg='')
   !!call unit_test('code', 0.eq.0, 'checking',100)
   call unit_test_done('code',msg='')
end subroutine test_code
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_copy()
real,allocatable :: from(:), to(:)
   call unit_test_start('copy',msg='')

   from=[1.0,2.0,3.0,4.0,5.0]
   to=[-1.0,-1.0,-1.0,-1.0,-1.0,-1.0]

   call copy(from,3,to)
   call unit_test('copy',all(to==[1.00,2.00,3.00,-1.00,-1.00,-1.00]),'simple')

   call copy([10.0,20.0,30.0],3,to(3:5))
   call unit_test('copy',all(to==[1.00,2.00,10.00,20.00,30.00,-1.00]),'subvector')

   call unit_test_done('copy',msg='')
end subroutine test_copy
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_corr()
   call unit_test_start('corr',msg='')
   !!call unit_test('corr', 0.eq.0, 'checking',100)
   call unit_test_done('corr',msg='')
end subroutine test_corr
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_count()
   call unit_test_start('count',msg='')
   !!call unit_test('count', 0.eq.0, 'checking',100)
   call unit_test_done('count',msg='')
end subroutine test_count
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_decomp()
   call unit_test_start('decomp',msg='')
   !!call unit_test('decomp', 0.eq.0, 'checking',100)
   call unit_test_done('decomp',msg='')
end subroutine test_decomp
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_define()
   call unit_test_start('define',msg='')
   !!call unit_test('define', 0.eq.0, 'checking',100)
   call unit_test_done('define',msg='')
end subroutine test_define
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_delete()
   call unit_test_start('delete',msg='')
   !!call unit_test('delete', 0.eq.0, 'checking',100)
   call unit_test_done('delete',msg='')
end subroutine test_delete
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_demod()
   call unit_test_start('demod',msg='')
   !!call unit_test('demod', 0.eq.0, 'checking',100)
   call unit_test_done('demod',msg='')
end subroutine test_demod
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_dexcdf()
   call unit_test_start('dexcdf',msg='')
   !!call unit_test('dexcdf', 0.eq.0, 'checking',100)
   call unit_test_done('dexcdf',msg='')
end subroutine test_dexcdf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_dexpdf()
   call unit_test_start('dexpdf',msg='')
   !!call unit_test('dexpdf', 0.eq.0, 'checking',100)
   call unit_test_done('dexpdf',msg='')
end subroutine test_dexpdf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_dexplt()
   call unit_test_start('dexplt',msg='')
   !!call unit_test('dexplt', 0.eq.0, 'checking',100)
   call unit_test_done('dexplt',msg='')
end subroutine test_dexplt
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_dexppf()
   call unit_test_start('dexppf',msg='')
   !!call unit_test('dexppf', 0.eq.0, 'checking',100)
   call unit_test_done('dexppf',msg='')
end subroutine test_dexppf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_dexran()
   call unit_test_start('dexran',msg='')
   !!call unit_test('dexran', 0.eq.0, 'checking',100)
   call unit_test_done('dexran',msg='')
end subroutine test_dexran
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_dexsf()
   call unit_test_start('dexsf',msg='')
   !!call unit_test('dexsf', 0.eq.0, 'checking',100)
   call unit_test_done('dexsf',msg='')
end subroutine test_dexsf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_discr2()
   call unit_test_start('discr2',msg='')
   !!call unit_test('discr2', 0.eq.0, 'checking',100)
   call unit_test_done('discr2',msg='')
end subroutine test_discr2
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_discr3()
   call unit_test_start('discr3',msg='')
   !!call unit_test('discr3', 0.eq.0, 'checking',100)
   call unit_test_done('discr3',msg='')
end subroutine test_discr3
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_discre()
   call unit_test_start('discre',msg='')
   !!call unit_test('discre', 0.eq.0, 'checking',100)
   call unit_test_done('discre',msg='')
end subroutine test_discre
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_dot()
real, dimension(3) :: a, b
real :: dotpro , parpro
integer imax , imin
   call unit_test_start('dot',msg='')
   a = [ 1.0, 2.0, 3.0 ]
   b = [ 4.0, 5.0, 6.0 ]
   imin=1
   imax=size(a)
   parpro=0.0
   call dot(a,b,imin,imax,parpro,dotpro)
   ! multiply by 1 to avoid gfortran-11 bug
   call unit_test('dot', dotpro == 32.0 ,'comparing',dotpro,1*dot_product(a,b))
   call unit_test_done('dot',msg='')
end subroutine test_dot
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_ev1cdf()
   call unit_test_start('ev1cdf',msg='')
   !!call unit_test('ev1cdf', 0.eq.0, 'checking',100)
   call unit_test_done('ev1cdf',msg='')
end subroutine test_ev1cdf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_ev1plt()
   call unit_test_start('ev1plt',msg='')
   !!call unit_test('ev1plt', 0.eq.0, 'checking',100)
   call unit_test_done('ev1plt',msg='')
end subroutine test_ev1plt
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_ev1ppf()
   call unit_test_start('ev1ppf',msg='')
   !!call unit_test('ev1ppf', 0.eq.0, 'checking',100)
   call unit_test_done('ev1ppf',msg='')
end subroutine test_ev1ppf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_ev1ran()
   call unit_test_start('ev1ran',msg='')
   !!call unit_test('ev1ran', 0.eq.0, 'checking',100)
   call unit_test_done('ev1ran',msg='')
end subroutine test_ev1ran
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_ev2cdf()
   call unit_test_start('ev2cdf',msg='')
   !!call unit_test('ev2cdf', 0.eq.0, 'checking',100)
   call unit_test_done('ev2cdf',msg='')
end subroutine test_ev2cdf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_ev2plt()
   call unit_test_start('ev2plt',msg='')
   !!call unit_test('ev2plt', 0.eq.0, 'checking',100)
   call unit_test_done('ev2plt',msg='')
end subroutine test_ev2plt
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_ev2ppf()
   call unit_test_start('ev2ppf',msg='')
   !!call unit_test('ev2ppf', 0.eq.0, 'checking',100)
   call unit_test_done('ev2ppf',msg='')
end subroutine test_ev2ppf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_ev2ran()
   call unit_test_start('ev2ran',msg='')
   !!call unit_test('ev2ran', 0.eq.0, 'checking',100)
   call unit_test_done('ev2ran',msg='')
end subroutine test_ev2ran
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_expcdf()
   call unit_test_start('expcdf',msg='')
   !!call unit_test('expcdf', 0.eq.0, 'checking',100)
   call unit_test_done('expcdf',msg='')
end subroutine test_expcdf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_exppdf()
   call unit_test_start('exppdf',msg='')
   !!call unit_test('exppdf', 0.eq.0, 'checking',100)
   call unit_test_done('exppdf',msg='')
end subroutine test_exppdf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_expplt()
   call unit_test_start('expplt',msg='')
   !!call unit_test('expplt', 0.eq.0, 'checking',100)
   call unit_test_done('expplt',msg='')
end subroutine test_expplt
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_expppf()
   call unit_test_start('expppf',msg='')
   !!call unit_test('expppf', 0.eq.0, 'checking',100)
   call unit_test_done('expppf',msg='')
end subroutine test_expppf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_expran()
   call unit_test_start('expran',msg='')
   !!call unit_test('expran', 0.eq.0, 'checking',100)
   call unit_test_done('expran',msg='')
end subroutine test_expran
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_expsf()
   call unit_test_start('expsf',msg='')
   !!call unit_test('expsf', 0.eq.0, 'checking',100)
   call unit_test_done('expsf',msg='')
end subroutine test_expsf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_extrem()
   call unit_test_start('extrem',msg='')
   !!call unit_test('extrem', 0.eq.0, 'checking',100)
   call unit_test_done('extrem',msg='')
end subroutine test_extrem
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_fcdf()
   call unit_test_start('fcdf',msg='')
   !!call unit_test('fcdf', 0.eq.0, 'checking',100)
   call unit_test_done('fcdf',msg='')
end subroutine test_fcdf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_fourie()
   call unit_test_start('fourie',msg='')
   !!call unit_test('fourie', 0.eq.0, 'checking',100)
   call unit_test_done('fourie',msg='')
end subroutine test_fourie
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_fran()
   call unit_test_start('fran',msg='')
   !!call unit_test('fran', 0.eq.0, 'checking',100)
   call unit_test_done('fran',msg='')
end subroutine test_fran
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_freq()
   call unit_test_start('freq',msg='')
   !!call unit_test('freq', 0.eq.0, 'checking',100)
   call unit_test_done('freq',msg='')
end subroutine test_freq
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_gamcdf()
   call unit_test_start('gamcdf',msg='')
   !!call unit_test('gamcdf', 0.eq.0, 'checking',100)
   call unit_test_done('gamcdf',msg='')
end subroutine test_gamcdf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_gamplt()
   call unit_test_start('gamplt',msg='')
   !!call unit_test('gamplt', 0.eq.0, 'checking',100)
   call unit_test_done('gamplt',msg='')
end subroutine test_gamplt
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_gamppf()
   call unit_test_start('gamppf',msg='')
   !!call unit_test('gamppf', 0.eq.0, 'checking',100)
   call unit_test_done('gamppf',msg='')
end subroutine test_gamppf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_gamran()
   call unit_test_start('gamran',msg='')
   !!call unit_test('gamran', 0.eq.0, 'checking',100)
   call unit_test_done('gamran',msg='')
end subroutine test_gamran
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_geocdf()
   call unit_test_start('geocdf',msg='')
   !!call unit_test('geocdf', 0.eq.0, 'checking',100)
   call unit_test_done('geocdf',msg='')
end subroutine test_geocdf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_geoplt()
   call unit_test_start('geoplt',msg='')
   !!call unit_test('geoplt', 0.eq.0, 'checking',100)
   call unit_test_done('geoplt',msg='')
end subroutine test_geoplt
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_geoppf()
   call unit_test_start('geoppf',msg='')
   !!call unit_test('geoppf', 0.eq.0, 'checking',100)
   call unit_test_done('geoppf',msg='')
end subroutine test_geoppf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_georan()
   call unit_test_start('georan',msg='')
   !!call unit_test('georan', 0.eq.0, 'checking',100)
   call unit_test_done('georan',msg='')
end subroutine test_georan
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_hfncdf()
   call unit_test_start('hfncdf',msg='')
   !!call unit_test('hfncdf', 0.eq.0, 'checking',100)
   call unit_test_done('hfncdf',msg='')
end subroutine test_hfncdf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_hfnplt()
   call unit_test_start('hfnplt',msg='')
   !!call unit_test('hfnplt', 0.eq.0, 'checking',100)
   call unit_test_done('hfnplt',msg='')
end subroutine test_hfnplt
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_hfnppf()
   call unit_test_start('hfnppf',msg='')
   !!call unit_test('hfnppf', 0.eq.0, 'checking',100)
   call unit_test_done('hfnppf',msg='')
end subroutine test_hfnppf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_hfnran()
   call unit_test_start('hfnran',msg='')
   !!call unit_test('hfnran', 0.eq.0, 'checking',100)
   call unit_test_done('hfnran',msg='')
end subroutine test_hfnran
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_hist()
   call unit_test_start('hist',msg='')
   !!call unit_test('hist', 0.eq.0, 'checking',100)
   call unit_test_done('hist',msg='')
end subroutine test_hist
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_invxwx()
   call unit_test_start('invxwx',msg='')
   !!call unit_test('invxwx', 0.eq.0, 'checking',100)
   call unit_test_done('invxwx',msg='')
end subroutine test_invxwx
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_lamcdf()
   call unit_test_start('lamcdf',msg='')
   !!call unit_test('lamcdf', 0.eq.0, 'checking',100)
   call unit_test_done('lamcdf',msg='')
end subroutine test_lamcdf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_lampdf()
   call unit_test_start('lampdf',msg='')
   !!call unit_test('lampdf', 0.eq.0, 'checking',100)
   call unit_test_done('lampdf',msg='')
end subroutine test_lampdf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_lamplt()
   call unit_test_start('lamplt',msg='')
   !!call unit_test('lamplt', 0.eq.0, 'checking',100)
   call unit_test_done('lamplt',msg='')
end subroutine test_lamplt
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_lamppf()
   call unit_test_start('lamppf',msg='')
   !!call unit_test('lamppf', 0.eq.0, 'checking',100)
   call unit_test_done('lamppf',msg='')
end subroutine test_lamppf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_lamran()
   call unit_test_start('lamran',msg='')
   !!call unit_test('lamran', 0.eq.0, 'checking',100)
   call unit_test_done('lamran',msg='')
end subroutine test_lamran
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_lamsf()
   call unit_test_start('lamsf',msg='')
   !!call unit_test('lamsf', 0.eq.0, 'checking',100)
   call unit_test_done('lamsf',msg='')
end subroutine test_lamsf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_lgncdf()
   call unit_test_start('lgncdf',msg='')
   !!call unit_test('lgncdf', 0.eq.0, 'checking',100)
   call unit_test_done('lgncdf',msg='')
end subroutine test_lgncdf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_lgnplt()
   call unit_test_start('lgnplt',msg='')
   !!call unit_test('lgnplt', 0.eq.0, 'checking',100)
   call unit_test_done('lgnplt',msg='')
end subroutine test_lgnplt
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_lgnppf()
   call unit_test_start('lgnppf',msg='')
   !!call unit_test('lgnppf', 0.eq.0, 'checking',100)
   call unit_test_done('lgnppf',msg='')
end subroutine test_lgnppf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_lgnran()
   call unit_test_start('lgnran',msg='')
   !!call unit_test('lgnran', 0.eq.0, 'checking',100)
   call unit_test_done('lgnran',msg='')
end subroutine test_lgnran
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_loc()
   call unit_test_start('loc',msg='')
   !!call unit_test('loc', 0.eq.0, 'checking',100)
   call unit_test_done('loc',msg='')
end subroutine test_loc
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_logcdf()
   call unit_test_start('logcdf',msg='')
   !!call unit_test('logcdf', 0.eq.0, 'checking',100)
   call unit_test_done('logcdf',msg='')
end subroutine test_logcdf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_logpdf()
   call unit_test_start('logpdf',msg='')
   !!call unit_test('logpdf', 0.eq.0, 'checking',100)
   call unit_test_done('logpdf',msg='')
end subroutine test_logpdf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_logplt()
   call unit_test_start('logplt',msg='')
   !!call unit_test('logplt', 0.eq.0, 'checking',100)
   call unit_test_done('logplt',msg='')
end subroutine test_logplt
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_logppf()
   call unit_test_start('logppf',msg='')
   !!call unit_test('logppf', 0.eq.0, 'checking',100)
   call unit_test_done('logppf',msg='')
end subroutine test_logppf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_logran()
   call unit_test_start('logran',msg='')
   !!call unit_test('logran', 0.eq.0, 'checking',100)
   call unit_test_done('logran',msg='')
end subroutine test_logran
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_logsf()
   call unit_test_start('logsf',msg='')
   !!call unit_test('logsf', 0.eq.0, 'checking',100)
   call unit_test_done('logsf',msg='')
end subroutine test_logsf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_max()
use M_datapac, only : intel_max=>max
real :: xmax
real(kind=real64) :: dmax

   call unit_test_start('max',msg='')

   call intel_max([-100.0, 200.0, 0.0, 400.0, -200.0],5,1,xmax)
   call unit_test('max', xmax.eq.400.0, 'checking',xmax,400.0)

   call intel_max([-100.0d0, 200.0d0, 0.0d0, 400.0d0, -200.0d0],5,1,dmax)
   call unit_test('max', dmax.eq.400.0d0, 'checking',dmax,400.0d0)

   call unit_test_done('max',msg='')

end subroutine test_max
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mean()
real :: sp_mean
double precision :: dp_mean
   call unit_test_start('mean',msg='')
   call mean([4.0, 36.0, 45.0, 50.0, 75.0], 5, 1, sp_mean)
   call unit_test('mean', sp_mean.eq.42.0, 'checking',sp_mean,42.0)
   call mean([4.0d0, 36.0d0, 45.0d0, 50.0d0, 75.0d0], 5, 1, dp_mean)
   call unit_test('mean', dp_mean.eq.42.0d0, 'checking',dp_mean,42.0d0)
   call mean([4.0d0], 1, 1, dp_mean)
   call unit_test('mean', dp_mean.eq.4.0d0, 'checking',dp_mean,4.0d0)
   call unit_test_done('mean',msg='')
end subroutine test_mean
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_median()
   call unit_test_start('median',msg='')
   !!call unit_test('median', 0.eq.0, 'checking',100)
   call unit_test_done('median',msg='')
end subroutine test_median
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_midm()
   integer :: i
   real :: xmidm
   call unit_test_start('midm',msg='')

   call midm([real :: (i,i=0,100) ],101,1,xmidm)
   call unit_test('midm', xmidm==50.0, 'expecting',50.0,'got',xmidm)

   call midm([real :: (i,i=0,101) ],102,1,xmidm)
   call unit_test('midm', xmidm==50.5, 'expecting',50.5,'got',xmidm)

   call unit_test_done('midm',msg='')
end subroutine test_midm
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_midr()
   integer :: i
   real :: xmidr
   call unit_test_start('midr',msg='')

   call midr([real :: (i,i=0,100) ],101,1,xmidr)
   call unit_test('midr', xmidr==50.0, 'expecting',50.0,'got',xmidr)

   call midr([real :: (i,i=0,101) ],102,1,xmidr)
   call unit_test('midr', xmidr==50.5, 'expecting',50.5,'got',xmidr)

   call unit_test_done('midr',msg='')
end subroutine test_midr
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_min()
real :: xmin
real(kind=real64) :: dmin

   call unit_test_start('min',msg='')

   call min([-100.0, 200.0, 0.0, 400.0, -200.0],5,1,xmin)
   call unit_test('min', xmin.eq.-200.0, 'checking',xmin,-200.0)

   call min([-100.0d0, 200.0d0, 0.0d0, 400.0d0, -200.0d0],5,1,dmin)
   call unit_test('min', dmin.eq.-200.0d0, 'checking',dmin,-200.0d0)

   call unit_test_done('min',msg='')

end subroutine test_min
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_move()
   call unit_test_start('move',msg='')
   !!call unit_test('move', 0.eq.0, 'checking',100)
   call unit_test_done('move',msg='')
end subroutine test_move
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_nbcdf()
   call unit_test_start('nbcdf',msg='')
   !!call unit_test('nbcdf', 0.eq.0, 'checking',100)
   call unit_test_done('nbcdf',msg='')
end subroutine test_nbcdf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_nbppf()
   call unit_test_start('nbppf',msg='')
   !!call unit_test('nbppf', 0.eq.0, 'checking',100)
   call unit_test_done('nbppf',msg='')
end subroutine test_nbppf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_nbran()
   call unit_test_start('nbran',msg='')
   !!call unit_test('nbran', 0.eq.0, 'checking',100)
   call unit_test_done('nbran',msg='')
end subroutine test_nbran
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_norcdf()
   call unit_test_start('norcdf',msg='')
   !!call unit_test('norcdf', 0.eq.0, 'checking',100)
   call unit_test_done('norcdf',msg='')
end subroutine test_norcdf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_norout()
   call unit_test_start('norout',msg='')
   !!call unit_test('norout', 0.eq.0, 'checking',100)
   call unit_test_done('norout',msg='')
end subroutine test_norout
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_norpdf()
   call unit_test_start('norpdf',msg='')
   !!call unit_test('norpdf', 0.eq.0, 'checking',100)
   call unit_test_done('norpdf',msg='')
end subroutine test_norpdf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_norplt()
   call unit_test_start('norplt',msg='')
   !!call unit_test('norplt', 0.eq.0, 'checking',100)
   call unit_test_done('norplt',msg='')
end subroutine test_norplt
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_norppf()
   call unit_test_start('norppf',msg='')
   !!call unit_test('norppf', 0.eq.0, 'checking',100)
   call unit_test_done('norppf',msg='')
end subroutine test_norppf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_norran()
   call unit_test_start('norran',msg='')
   !!call unit_test('norran', 0.eq.0, 'checking',100)
   call unit_test_done('norran',msg='')
end subroutine test_norran
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_norsf()
   call unit_test_start('norsf',msg='')
   !!call unit_test('norsf', 0.eq.0, 'checking',100)
   call unit_test_done('norsf',msg='')
end subroutine test_norsf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_parcdf()
   call unit_test_start('parcdf',msg='')
   !!call unit_test('parcdf', 0.eq.0, 'checking',100)
   call unit_test_done('parcdf',msg='')
end subroutine test_parcdf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_parplt()
   call unit_test_start('parplt',msg='')
   !!call unit_test('parplt', 0.eq.0, 'checking',100)
   call unit_test_done('parplt',msg='')
end subroutine test_parplt
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_parppf()
   call unit_test_start('parppf',msg='')
   !!call unit_test('parppf', 0.eq.0, 'checking',100)
   call unit_test_done('parppf',msg='')
end subroutine test_parppf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_parran()
   call unit_test_start('parran',msg='')
   !!call unit_test('parran', 0.eq.0, 'checking',100)
   call unit_test_done('parran',msg='')
end subroutine test_parran
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_plot()
   call unit_test_start('plot',msg='')
   !!call unit_test('plot', 0.eq.0, 'checking',100)
   call unit_test_done('plot',msg='')
end subroutine test_plot
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_plot10()
   call unit_test_start('plot10',msg='')
   !!call unit_test('plot10', 0.eq.0, 'checking',100)
   call unit_test_done('plot10',msg='')
end subroutine test_plot10
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_plot6()
   call unit_test_start('plot6',msg='')
   !!call unit_test('plot6', 0.eq.0, 'checking',100)
   call unit_test_done('plot6',msg='')
end subroutine test_plot6
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_plot7()
   call unit_test_start('plot7',msg='')
   !!call unit_test('plot7', 0.eq.0, 'checking',100)
   call unit_test_done('plot7',msg='')
end subroutine test_plot7
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_plot8()
   call unit_test_start('plot8',msg='')
   !!call unit_test('plot8', 0.eq.0, 'checking',100)
   call unit_test_done('plot8',msg='')
end subroutine test_plot8
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_plot9()
   call unit_test_start('plot9',msg='')
   !!call unit_test('plot9', 0.eq.0, 'checking',100)
   call unit_test_done('plot9',msg='')
end subroutine test_plot9
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_plotc()
   call unit_test_start('plotc',msg='')
   !!call unit_test('plotc', 0.eq.0, 'checking',100)
   call unit_test_done('plotc',msg='')
end subroutine test_plotc
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_plotco()
   call unit_test_start('plotco',msg='')
   !!call unit_test('plotco', 0.eq.0, 'checking',100)
   call unit_test_done('plotco',msg='')
end subroutine test_plotco
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_plotct()
   call unit_test_start('plotct',msg='')
   !!call unit_test('plotct', 0.eq.0, 'checking',100)
   call unit_test_done('plotct',msg='')
end subroutine test_plotct
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_plots()
   call unit_test_start('plots',msg='')
   !!call unit_test('plots', 0.eq.0, 'checking',100)
   call unit_test_done('plots',msg='')
end subroutine test_plots
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_plotsc()
   call unit_test_start('plotsc',msg='')
   !!call unit_test('plotsc', 0.eq.0, 'checking',100)
   call unit_test_done('plotsc',msg='')
end subroutine test_plotsc
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_plotsp()
   call unit_test_start('plotsp',msg='')
   !!call unit_test('plotsp', 0.eq.0, 'checking',100)
   call unit_test_done('plotsp',msg='')
end subroutine test_plotsp
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_plotst()
   call unit_test_start('plotst',msg='')
   !!call unit_test('plotst', 0.eq.0, 'checking',100)
   call unit_test_done('plotst',msg='')
end subroutine test_plotst
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_plott()
   call unit_test_start('plott',msg='')
   !!call unit_test('plott', 0.eq.0, 'checking',100)
   call unit_test_done('plott',msg='')
end subroutine test_plott
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_plotu()
   call unit_test_start('plotu',msg='')
   !!call unit_test('plotu', 0.eq.0, 'checking',100)
   call unit_test_done('plotu',msg='')
end subroutine test_plotu
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_plotx()
   call unit_test_start('plotx',msg='')
   !!call unit_test('plotx', 0.eq.0, 'checking',100)
   call unit_test_done('plotx',msg='')
end subroutine test_plotx
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_plotxt()
   call unit_test_start('plotxt',msg='')
   !!call unit_test('plotxt', 0.eq.0, 'checking',100)
   call unit_test_done('plotxt',msg='')
end subroutine test_plotxt
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_plotxx()
   call unit_test_start('plotxx',msg='')
   !!call unit_test('plotxx', 0.eq.0, 'checking',100)
   call unit_test_done('plotxx',msg='')
end subroutine test_plotxx
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_pltsct()
   call unit_test_start('pltsct',msg='')
   !!call unit_test('pltsct', 0.eq.0, 'checking',100)
   call unit_test_done('pltsct',msg='')
end subroutine test_pltsct
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_pltxxt()
   call unit_test_start('pltxxt',msg='')
   !!call unit_test('pltxxt', 0.eq.0, 'checking',100)
   call unit_test_done('pltxxt',msg='')
end subroutine test_pltxxt
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_poicdf()
   call unit_test_start('poicdf',msg='')
   !!call unit_test('poicdf', 0.eq.0, 'checking',100)
   call unit_test_done('poicdf',msg='')
end subroutine test_poicdf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_poiplt()
   call unit_test_start('poiplt',msg='')
   !!call unit_test('poiplt', 0.eq.0, 'checking',100)
   call unit_test_done('poiplt',msg='')
end subroutine test_poiplt
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_poippf()
   call unit_test_start('poippf',msg='')
   !!call unit_test('poippf', 0.eq.0, 'checking',100)
   call unit_test_done('poippf',msg='')
end subroutine test_poippf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_poiran()
   call unit_test_start('poiran',msg='')
   !!call unit_test('poiran', 0.eq.0, 'checking',100)
   call unit_test_done('poiran',msg='')
end subroutine test_poiran
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_poly()
   call unit_test_start('poly',msg='')
   !!call unit_test('poly', 0.eq.0, 'checking',100)
   call unit_test_done('poly',msg='')
end subroutine test_poly
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_propor()
   call unit_test_start('propor',msg='')
   !!call unit_test('propor', 0.eq.0, 'checking',100)
   call unit_test_done('propor',msg='')
end subroutine test_propor
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_range()
   call unit_test_start('range',msg='')
   !!call unit_test('range', 0.eq.0, 'checking',100)
   call unit_test_done('range',msg='')
end subroutine test_range
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_rank()
   call unit_test_start('rank',msg='')
   !!call unit_test('rank', 0.eq.0, 'checking',100)
   call unit_test_done('rank',msg='')
end subroutine test_rank
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_ranper()
   call unit_test_start('ranper',msg='')
   !!call unit_test('ranper', 0.eq.0, 'checking',100)
   call unit_test_done('ranper',msg='')
end subroutine test_ranper
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_read()
   call unit_test_start('read',msg='')
   !!call unit_test('read', 0.eq.0, 'checking',100)
   call unit_test_done('read',msg='')
end subroutine test_read
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_readg()
   call unit_test_start('readg',msg='')
   !!call unit_test('readg', 0.eq.0, 'checking',100)
   call unit_test_done('readg',msg='')
end subroutine test_readg
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_relsd()
   call unit_test_start('relsd',msg='')
   !!call unit_test('relsd', 0.eq.0, 'checking',100)
   call unit_test_done('relsd',msg='')
end subroutine test_relsd
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_replac()
   call unit_test_start('replac',msg='')
   !!call unit_test('replac', 0.eq.0, 'checking',100)
   call unit_test_done('replac',msg='')
end subroutine test_replac
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_retain()
   call unit_test_start('retain',msg='')
   !!call unit_test('retain', 0.eq.0, 'checking',100)
   call unit_test_done('retain',msg='')
end subroutine test_retain
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_runs()
   call unit_test_start('runs',msg='')
   !!call unit_test('runs', 0.eq.0, 'checking',100)
   call unit_test_done('runs',msg='')
end subroutine test_runs
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_sampp()
   call unit_test_start('sampp',msg='')
   !!call unit_test('sampp', 0.eq.0, 'checking',100)
   call unit_test_done('sampp',msg='')
end subroutine test_sampp
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_scale()
   call unit_test_start('scale',msg='')
   !!call unit_test('scale', 0.eq.0, 'checking',100)
   call unit_test_done('scale',msg='')
end subroutine test_scale
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_sd()
   call unit_test_start('sd',msg='')
   !!call unit_test('sd', 0.eq.0, 'checking',100)
   call unit_test_done('sd',msg='')
end subroutine test_sd
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_skipr()
   call unit_test_start('skipr',msg='')
   !!call unit_test('skipr', 0.eq.0, 'checking',100)
   call unit_test_done('skipr',msg='')
end subroutine test_skipr
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_sort()
integer,parameter            :: isz=20
real                         :: aa(isz)
real                         :: bb(isz)
integer                      :: i
integer                      :: ibad
   ibad=0
   call unit_test_start('sort',msg='')
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
   call unit_test('sort', ibad.eq.0, 'checking',100)
   call unit_test_done('sort',msg='')
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
   call unit_test_start('sortc',msg='')
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
   call unit_test('sortc', ibad.eq.0, 'checking ascending')
   call sortc(dd,cc,isz,dd2,cc2) ! put dd and cc back in original order
   call unit_test('sortc', all(cc2.eq.aa), 'checking reversed')
   call unit_test('sortc', all(dd2.eq.bb), 'checking reversed')
   call unit_test_done('sortc',msg='')
end subroutine test_sortc
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_sortp()
   call unit_test_start('sortp',msg='')
   !!call unit_test('sortp', 0.eq.0, 'checking',100)
   call unit_test_done('sortp',msg='')
end subroutine test_sortp
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_spcorr()
   call unit_test_start('spcorr',msg='')
   !!call unit_test('spcorr', 0.eq.0, 'checking',100)
   call unit_test_done('spcorr',msg='')
end subroutine test_spcorr
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_stmom3()
   call unit_test_start('stmom3',msg='')
   !!call unit_test('stmom3', 0.eq.0, 'checking',100)
   call unit_test_done('stmom3',msg='')
end subroutine test_stmom3
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_stmom4()
   call unit_test_start('stmom4',msg='')
   !!call unit_test('stmom4', 0.eq.0, 'checking',100)
   call unit_test_done('stmom4',msg='')
end subroutine test_stmom4
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_subse1()
   call unit_test_start('subse1',msg='')
   !!call unit_test('subse1', 0.eq.0, 'checking',100)
   call unit_test_done('subse1',msg='')
end subroutine test_subse1
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_subse2()
   call unit_test_start('subse2',msg='')
   !!call unit_test('subse2', 0.eq.0, 'checking',100)
   call unit_test_done('subse2',msg='')
end subroutine test_subse2
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_subset()
   call unit_test_start('subset',msg='')
   !!call unit_test('subset', 0.eq.0, 'checking',100)
   call unit_test_done('subset',msg='')
end subroutine test_subset
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_tail()
   call unit_test_start('tail',msg='')
   !!call unit_test('tail', 0.eq.0, 'checking',100)
   call unit_test_done('tail',msg='')
end subroutine test_tail
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_tcdf()
   call unit_test_start('tcdf',msg='')
   !!call unit_test('tcdf', 0.eq.0, 'checking',100)
   call unit_test_done('tcdf',msg='')
end subroutine test_tcdf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_time()
   call unit_test_start('time',msg='')
   !!call unit_test('time', 0.eq.0, 'checking',100)
   call unit_test_done('time',msg='')
end subroutine test_time
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_tol()
   call unit_test_start('tol',msg='')
   !!call unit_test('tol', 0.eq.0, 'checking',100)
   call unit_test_done('tol',msg='')
end subroutine test_tol
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_tplt()
   call unit_test_start('tplt',msg='')
   !!call unit_test('tplt', 0.eq.0, 'checking',100)
   call unit_test_done('tplt',msg='')
end subroutine test_tplt
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_tppf()
   call unit_test_start('tppf',msg='')
   !!call unit_test('tppf', 0.eq.0, 'checking',100)
   call unit_test_done('tppf',msg='')
end subroutine test_tppf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_tran()
   call unit_test_start('tran',msg='')
   !!call unit_test('tran', 0.eq.0, 'checking',100)
   call unit_test_done('tran',msg='')
end subroutine test_tran
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_trim()
   call unit_test_start('trim',msg='')
   !!call unit_test('trim', 0.eq.0, 'checking',100)
   call unit_test_done('trim',msg='')
end subroutine test_trim
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_unicdf()
integer,parameter :: n=40
real              :: x(0:n), y(0:n)
integer           :: i
   call unit_test_start('unicdf',msg='')
   x=[(real(i)/real(n),i=0,n)]
   do i=0,n
         call unicdf(x(i),y(i))
   enddo
   call unit_test('unicdf', all(x.eq.y), 'checking in equals out')
   call unit_test_done('unicdf',msg='')
end subroutine test_unicdf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_unimed()
   call unit_test_start('unimed',msg='')
   !!call unit_test('unimed', 0.eq.0, 'checking',100)
   call unit_test_done('unimed',msg='')
end subroutine test_unimed
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_unipdf()
   call unit_test_start('unipdf',msg='')
   !!call unit_test('unipdf', 0.eq.0, 'checking',100)
   call unit_test_done('unipdf',msg='')
end subroutine test_unipdf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_uniplt()
   call unit_test_start('uniplt',msg='')
   !!call unit_test('uniplt', 0.eq.0, 'checking',100)
   call unit_test_done('uniplt',msg='')
end subroutine test_uniplt
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_unippf()
   call unit_test_start('unippf',msg='')
   !!call unit_test('unippf', 0.eq.0, 'checking',100)
   call unit_test_done('unippf',msg='')
end subroutine test_unippf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_uniran()
   call unit_test_start('uniran',msg='')
   !!call unit_test('uniran', 0.eq.0, 'checking',100)
   call unit_test_done('uniran',msg='')
end subroutine test_uniran
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_unisf()
   call unit_test_start('unisf',msg='')
   !!call unit_test('unisf', 0.eq.0, 'checking',100)
   call unit_test_done('unisf',msg='')
end subroutine test_unisf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_var()
   real,allocatable :: x(:)
   real :: Xvar
   call unit_test_start('var',msg='')
   x = [46.0, 69.0, 32.0, 60.0, 52.0, 41.0]
   call VAR(X,size(x),1,Xvar)
   call unit_test('var', Xvar == 177.2 ,'got',Xvar,'expecting',177.2)
   call unit_test_done('var',msg='')
end subroutine test_var
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_weib()
   call unit_test_start('weib',msg='')
   !!call unit_test('weib', 0.eq.0, 'checking',100)
   call unit_test_done('weib',msg='')
end subroutine test_weib
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_weicdf()
   call unit_test_start('weicdf',msg='')
   !!call unit_test('weicdf', 0.eq.0, 'checking',100)
   call unit_test_done('weicdf',msg='')
end subroutine test_weicdf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_weiplt()
   call unit_test_start('weiplt',msg='')
   !!call unit_test('weiplt', 0.eq.0, 'checking',100)
   call unit_test_done('weiplt',msg='')
end subroutine test_weiplt
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_weippf()
   call unit_test_start('weippf',msg='')
   !!call unit_test('weippf', 0.eq.0, 'checking',100)
   call unit_test_done('weippf',msg='')
end subroutine test_weippf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_weiran()
   call unit_test_start('weiran',msg='')
   !!call unit_test('weiran', 0.eq.0, 'checking',100)
   call unit_test_done('weiran',msg='')
end subroutine test_weiran
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_wind()
   call unit_test_start('wind',msg='')
   !!call unit_test('wind', 0.eq.0, 'checking',100)
   call unit_test_done('wind',msg='')
end subroutine test_wind
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_write()
   call unit_test_start('write',msg='')
   !!call unit_test('write', 0.eq.0, 'checking',100)
   call unit_test_done('write',msg='')
end subroutine test_write
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
end program test_suite_M_datapac
