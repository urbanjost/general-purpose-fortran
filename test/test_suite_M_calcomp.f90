program test_suite_M_calcomp
use, intrinsic :: iso_fortran_env, only : ERROR_UNIT
use :: M_framework__verify, only : unit_check, unit_check_good, unit_check_bad, unit_check_done, unit_check_start
use :: M_framework__verify, only : unit_check_stop
implicit none

!! setup
   call test_axis()
   call test_circl()
   call test_cntour()
   call test_curvx()
   call test_curvy()
   call test_dashl()
   call test_dashp()
   call test_elips()
   call test_factor()
   call test_fit()
   call test_fit4()
   call test_fline()
   call test_grid()
   call test_lgaxs()
   call test_lglin()
   call test_line()
   call test_mpset()
   call test_mset()
   call test_newpen()
   call test_nframe()
   call test_number()
   call test_plot()
   call test_plots()
   call test_polar()
   call test_poly()
   call test_rect()
   call test_reflx()
   call test_scale()
   call test_scalg()
   call test_setpar()
   call test_smoot()
   call test_solut()
   call test_symbol()
   call test_where()
   call test_width()
   call unit_check_stop()
!! teardown
contains
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_axis()
   call unit_check_start('axis',msg='')
   !!call unit_check('axis', 0.eq.0, 'checking',100)
   call unit_check_done('axis',msg='')
end subroutine test_axis
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_circl()
   call unit_check_start('circl',msg='')
   !!call unit_check('circl', 0.eq.0, 'checking',100)
   call unit_check_done('circl',msg='')
end subroutine test_circl
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_cntour()
   call unit_check_start('cntour',msg='')
   !!call unit_check('cntour', 0.eq.0, 'checking',100)
   call unit_check_done('cntour',msg='')
end subroutine test_cntour
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_curvx()
   call unit_check_start('curvx',msg='')
   !!call unit_check('curvx', 0.eq.0, 'checking',100)
   call unit_check_done('curvx',msg='')
end subroutine test_curvx
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_curvy()
   call unit_check_start('curvy',msg='')
   !!call unit_check('curvy', 0.eq.0, 'checking',100)
   call unit_check_done('curvy',msg='')
end subroutine test_curvy
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_dashl()
   call unit_check_start('dashl',msg='')
   !!call unit_check('dashl', 0.eq.0, 'checking',100)
   call unit_check_done('dashl',msg='')
end subroutine test_dashl
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_dashp()
   call unit_check_start('dashp',msg='')
   !!call unit_check('dashp', 0.eq.0, 'checking',100)
   call unit_check_done('dashp',msg='')
end subroutine test_dashp
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_elips()
   call unit_check_start('elips',msg='')
   !!call unit_check('elips', 0.eq.0, 'checking',100)
   call unit_check_done('elips',msg='')
end subroutine test_elips
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_factor()
   call unit_check_start('factor',msg='')
   !!call unit_check('factor', 0.eq.0, 'checking',100)
   call unit_check_done('factor',msg='')
end subroutine test_factor
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_fit()
   call unit_check_start('fit',msg='')
   !!call unit_check('fit', 0.eq.0, 'checking',100)
   call unit_check_done('fit',msg='')
end subroutine test_fit
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_fit4()
   call unit_check_start('fit4',msg='')
   !!call unit_check('fit4', 0.eq.0, 'checking',100)
   call unit_check_done('fit4',msg='')
end subroutine test_fit4
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_fline()
   call unit_check_start('fline',msg='')
   !!call unit_check('fline', 0.eq.0, 'checking',100)
   call unit_check_done('fline',msg='')
end subroutine test_fline
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_grid()
   call unit_check_start('grid',msg='')
   !!call unit_check('grid', 0.eq.0, 'checking',100)
   call unit_check_done('grid',msg='')
end subroutine test_grid
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_lgaxs()
   call unit_check_start('lgaxs',msg='')
   !!call unit_check('lgaxs', 0.eq.0, 'checking',100)
   call unit_check_done('lgaxs',msg='')
end subroutine test_lgaxs
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_lglin()
   call unit_check_start('lglin',msg='')
   !!call unit_check('lglin', 0.eq.0, 'checking',100)
   call unit_check_done('lglin',msg='')
end subroutine test_lglin
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_line()
   call unit_check_start('line',msg='')
   !!call unit_check('line', 0.eq.0, 'checking',100)
   call unit_check_done('line',msg='')
end subroutine test_line
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mpset()
   call unit_check_start('mpset',msg='')
   !!call unit_check('mpset', 0.eq.0, 'checking',100)
   call unit_check_done('mpset',msg='')
end subroutine test_mpset
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mset()
   call unit_check_start('mset',msg='')
   !!call unit_check('mset', 0.eq.0, 'checking',100)
   call unit_check_done('mset',msg='')
end subroutine test_mset
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_newpen()
   call unit_check_start('newpen',msg='')
   !!call unit_check('newpen', 0.eq.0, 'checking',100)
   call unit_check_done('newpen',msg='')
end subroutine test_newpen
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_nframe()
   call unit_check_start('nframe',msg='')
   !!call unit_check('nframe', 0.eq.0, 'checking',100)
   call unit_check_done('nframe',msg='')
end subroutine test_nframe
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_number()
   call unit_check_start('number',msg='')
   !!call unit_check('number', 0.eq.0, 'checking',100)
   call unit_check_done('number',msg='')
end subroutine test_number
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_plot()
   call unit_check_start('plot',msg='')
   !!call unit_check('plot', 0.eq.0, 'checking',100)
   call unit_check_done('plot',msg='')
end subroutine test_plot
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_plots()
   call unit_check_start('plots',msg='')
   !!call unit_check('plots', 0.eq.0, 'checking',100)
   call unit_check_done('plots',msg='')
end subroutine test_plots
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_polar()
   call unit_check_start('polar',msg='')
   !!call unit_check('polar', 0.eq.0, 'checking',100)
   call unit_check_done('polar',msg='')
end subroutine test_polar
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_poly()
   call unit_check_start('poly',msg='')
   !!call unit_check('poly', 0.eq.0, 'checking',100)
   call unit_check_done('poly',msg='')
end subroutine test_poly
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_rect()
   call unit_check_start('rect',msg='')
   !!call unit_check('rect', 0.eq.0, 'checking',100)
   call unit_check_done('rect',msg='')
end subroutine test_rect
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_reflx()
   call unit_check_start('reflx',msg='')
   !!call unit_check('reflx', 0.eq.0, 'checking',100)
   call unit_check_done('reflx',msg='')
end subroutine test_reflx
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_scale()
   call unit_check_start('scale',msg='')
   !!call unit_check('scale', 0.eq.0, 'checking',100)
   call unit_check_done('scale',msg='')
end subroutine test_scale
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_scalg()
   call unit_check_start('scalg',msg='')
   !!call unit_check('scalg', 0.eq.0, 'checking',100)
   call unit_check_done('scalg',msg='')
end subroutine test_scalg
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_setpar()
   call unit_check_start('setpar',msg='')
   !!call unit_check('setpar', 0.eq.0, 'checking',100)
   call unit_check_done('setpar',msg='')
end subroutine test_setpar
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_smoot()
   call unit_check_start('smoot',msg='')
   !!call unit_check('smoot', 0.eq.0, 'checking',100)
   call unit_check_done('smoot',msg='')
end subroutine test_smoot
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_solut()
   call unit_check_start('solut',msg='')
   !!call unit_check('solut', 0.eq.0, 'checking',100)
   call unit_check_done('solut',msg='')
end subroutine test_solut
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_symbol()
   call unit_check_start('symbol',msg='')
   !!call unit_check('symbol', 0.eq.0, 'checking',100)
   call unit_check_done('symbol',msg='')
end subroutine test_symbol
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_where()
   call unit_check_start('where',msg='')
   !!call unit_check('where', 0.eq.0, 'checking',100)
   call unit_check_done('where',msg='')
end subroutine test_where
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_width()
   call unit_check_start('width',msg='')
   !!call unit_check('width', 0.eq.0, 'checking',100)
   call unit_check_done('width',msg='')
end subroutine test_width
!===================================================================================================================================
end program test_suite_M_calcomp
!===================================================================================================================================
