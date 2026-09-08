!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
program runtest
use M_framework__msg
use M_framework__verify, only : unit_test_start,unit_test,unit_test_done,unit_test_good,unit_test_bad,unit_test_msg
use M_pixel
use,intrinsic :: ISO_FORTRAN_ENV, only : int8,int16,int32,int64,real32,real64,real128
implicit none
integer(kind=int64) :: answer
integer(kind=int64) :: expected
   write(*,*)'CHECK RESULTS HAVE NOT CHANGED'
   call test_suite_M_pixel()
contains
subroutine test_suite_M_pixel()
!! setup
   call test_arc()
   call test_centertext()
   call test_circle()
   call test_circleprecision()
   call test_clear()
   call test_closepoly()
   call test_color()
   call test_draw2()
   call test_drawchar()
   call test_drawstr()
   call test_font()
   call test_getdisplaysize()
   call test_getgp2()
   call test_getviewport()
   call test_hershey()
   call test_justfy()
   call test_line()
   call test_linewidth()
   call test_makepoly()
   call test_mapcolor()
   call test_move2()
   call test_ortho2()
   call test_page()
   call test_point2()
   call test_poly2()
   call test_polyline2()
   call test_prefsize()
   call test_print_ascii()
   call test_print_ppm()
   call test_rdraw2()
   call test_rect()
   call test_rmove2()
   call test_state()
   call test_strlength()
   call test_textang()
   call test_textsize()
   call test_vexit()
   call test_vflush()
   call test_viewport()
   call test_vinit()
   call test_xcentertext()
   call test_ycentertext()
end subroutine test_suite_M_pixel
!! teardown
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
function crc32_hash_arr(anything,continue) result (crc_64)
use,intrinsic :: ISO_FORTRAN_ENV, only : int8,int16,int32,int64,real32,real64,real128
implicit none

!$@(#) M_hashkeys::crc32_hash_arr: CRC (Cyclic Redundancy Check) calculation

integer,intent(in)           :: anything(:)
logical,intent(in),optional  :: continue
character(len=1),allocatable :: a(:)
integer(int64)               :: crc_64
integer(int32),save          :: crc
integer                      :: i
integer(int32),save          :: crc_table(0:255)
integer,save                 :: icalled=0
   if(present(continue))then
      if(continue .eqv. .false.)then
         crc=0_int32
      endif
   else
      crc=0_int32
   endif

   !a=transfer(anything,a) ! ifort/ifx bug, a should not have to be allocated
   a=transfer(anything,['A'])

   if(icalled.eq.0)then         ! on first call generate table and use table for speed
      INIT_TABLE: block
         integer :: i, j
         integer(int32) :: k

         do i = 0, 255
            k = i
            do j = 1, 8
               if (btest(k, 0)) then
                  k = ieor(shiftr(k, 1), -306674912_int32)
               else
                  k = shiftr(k, 1)
               endif
            enddo
            crc_table(i) = k
         enddo
      endblock INIT_TABLE
      icalled=1
   endif

   crc = not(crc)
   do i = 1, size(a)
      crc = ieor(shiftr(crc, 8), crc_table(iand(ieor(crc, iachar(a(i))), 255)))
   enddo
   crc = not(crc)
   crc_64=transfer([crc,0_int32],crc_64)
end function crc32_hash_arr
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_arc()
   call unit_test_start('arc',msg='')

   call prefsize(300,300)
   call vinit(' ')
   call ortho2(-2.0,2.0,-2.0,2.0)
   call color(2)
   call linewidth(100)
   call arc(0.0,0.0,1.0,0.0,270.0)

   expected= 729245524
   answer=crc32_hash_arr(pack(P_pixel,.true.))
   write(*,*)'answer=',answer
   call unit_test('arc', answer.eq.expected, 'checking checksum, expected=',expected,' answer=',answer)
   call vexit()

   call unit_test_done('arc',msg='')
end subroutine test_arc
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_centertext()
implicit none
   call unit_test_start('centertext',msg='')
   !!call unit_test('centertext', 0.eq.0, 'checking',100)
   call unit_test_done('centertext',msg='')
end subroutine test_centertext
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_circle()
implicit none
   call unit_test_start('circle',msg='')
   !!call unit_test('circle', 0.eq.0, 'checking',100)
   call unit_test_done('circle',msg='')
end subroutine test_circle
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_circleprecision()
implicit none
   call unit_test_start('circleprecision',msg='')
   !!call unit_test('circleprecision', 0.eq.0, 'checking',100)
   call unit_test_done('circleprecision',msg='')
end subroutine test_circleprecision
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_clear()
implicit none
   call unit_test_start('clear',msg='')
   !!call unit_test('clear', 0.eq.0, 'checking',100)
   call unit_test_done('clear',msg='')
end subroutine test_clear
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_closepoly()
implicit none
   call unit_test_start('closepoly',msg='')
   !!call unit_test('closepoly', 0.eq.0, 'checking',100)
   call unit_test_done('closepoly',msg='')
end subroutine test_closepoly
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_color()
implicit none
   call unit_test_start('color',msg='')
   !!call unit_test('color', 0.eq.0, 'checking',100)
   call unit_test_done('color',msg='')
end subroutine test_color
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_draw2()
implicit none
   call unit_test_start('draw2',msg='')
   !!call unit_test('draw2', 0.eq.0, 'checking',100)
   call unit_test_done('draw2',msg='')
end subroutine test_draw2
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_drawchar()
implicit none
   call unit_test_start('drawchar',msg='')
   !!call unit_test('drawchar', 0.eq.0, 'checking',100)
   call unit_test_done('drawchar',msg='')
end subroutine test_drawchar
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_drawstr()
implicit none
   call unit_test_start('drawstr',msg='')
   !!call unit_test('drawstr', 0.eq.0, 'checking',100)
   call unit_test_done('drawstr',msg='')
end subroutine test_drawstr
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_font()
implicit none
   call unit_test_start('font',msg='')
   !!call unit_test('font', 0.eq.0, 'checking',100)
   call unit_test_done('font',msg='')
end subroutine test_font
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_getdisplaysize()
implicit none
   call unit_test_start('getdisplaysize',msg='')
   !!call unit_test('getdisplaysize', 0.eq.0, 'checking',100)
   call unit_test_done('getdisplaysize',msg='')
end subroutine test_getdisplaysize
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_getgp2()
implicit none
   call unit_test_start('getgp2',msg='')
   !!call unit_test('getgp2', 0.eq.0, 'checking',100)
   call unit_test_done('getgp2',msg='')
end subroutine test_getgp2
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_getviewport()
implicit none
   call unit_test_start('getviewport',msg='')
   !!call unit_test('getviewport', 0.eq.0, 'checking',100)
   call unit_test_done('getviewport',msg='')
end subroutine test_getviewport
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_hershey()
implicit none
   call unit_test_start('hershey',msg='')
   !!call unit_test('hershey', 0.eq.0, 'checking',100)
   call unit_test_done('hershey',msg='')
end subroutine test_hershey
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_justfy()
implicit none
   call unit_test_start('justfy',msg='')
   !!call unit_test('justfy', 0.eq.0, 'checking',100)
   call unit_test_done('justfy',msg='')
end subroutine test_justfy
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_line()
implicit none
   call unit_test_start('line',msg='')
   !!call unit_test('line', 0.eq.0, 'checking',100)
   call unit_test_done('line',msg='')
end subroutine test_line
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_linewidth()
implicit none
   call unit_test_start('linewidth',msg='')
   !!call unit_test('linewidth', 0.eq.0, 'checking',100)
   call unit_test_done('linewidth',msg='')
end subroutine test_linewidth
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_makepoly()
implicit none
   call unit_test_start('makepoly',msg='')
   !!call unit_test('makepoly', 0.eq.0, 'checking',100)
   call unit_test_done('makepoly',msg='')
end subroutine test_makepoly
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mapcolor()
implicit none
   call unit_test_start('mapcolor',msg='')
   !!call unit_test('mapcolor', 0.eq.0, 'checking',100)
   call unit_test_done('mapcolor',msg='')
end subroutine test_mapcolor
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_move2()
implicit none
   call unit_test_start('move2',msg='')
   !!call unit_test('move2', 0.eq.0, 'checking',100)
   call unit_test_done('move2',msg='')
end subroutine test_move2
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_ortho2()
implicit none
   call unit_test_start('ortho2',msg='')
   !!call unit_test('ortho2', 0.eq.0, 'checking',100)
   call unit_test_done('ortho2',msg='')
end subroutine test_ortho2
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_page()
implicit none
   call unit_test_start('page',msg='')
   !!call unit_test('page', 0.eq.0, 'checking',100)
   call unit_test_done('page',msg='')
end subroutine test_page
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_point2()
implicit none
   call unit_test_start('point2',msg='')
   !!call unit_test('point2', 0.eq.0, 'checking',100)
   call unit_test_done('point2',msg='')
end subroutine test_point2
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_poly2()
implicit none
   call unit_test_start('poly2',msg='')
   !!call unit_test('poly2', 0.eq.0, 'checking',100)
   call unit_test_done('poly2',msg='')
end subroutine test_poly2
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_polyline2()
implicit none
   call unit_test_start('polyline2',msg='')
   !!call unit_test('polyline2', 0.eq.0, 'checking',100)
   call unit_test_done('polyline2',msg='')
end subroutine test_polyline2
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_prefsize()
implicit none
   call unit_test_start('prefsize',msg='')
   !!call unit_test('prefsize', 0.eq.0, 'checking',100)
   call unit_test_done('prefsize',msg='')
end subroutine test_prefsize
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_print_ascii()
implicit none
   call unit_test_start('print_ascii',msg='')
   !!call unit_test('print_ascii', 0.eq.0, 'checking',100)
   call unit_test_done('print_ascii',msg='')
end subroutine test_print_ascii
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_print_ppm()
implicit none
   call unit_test_start('print_ppm',msg='')
   !!call unit_test('print_ppm', 0.eq.0, 'checking',100)
   call unit_test_done('print_ppm',msg='')
end subroutine test_print_ppm
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_rdraw2()
implicit none
   call unit_test_start('rdraw2',msg='')
   !!call unit_test('rdraw2', 0.eq.0, 'checking',100)
   call unit_test_done('rdraw2',msg='')
end subroutine test_rdraw2
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_rect()
implicit none
   call unit_test_start('rect',msg='')
   !!call unit_test('rect', 0.eq.0, 'checking',100)
   call unit_test_done('rect',msg='')
end subroutine test_rect
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_rmove2()
implicit none
   call unit_test_start('rmove2',msg='')
   !!call unit_test('rmove2', 0.eq.0, 'checking',100)
   call unit_test_done('rmove2',msg='')
end subroutine test_rmove2
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_state()
implicit none
   call unit_test_start('state',msg='')
   !!call unit_test('state', 0.eq.0, 'checking',100)
   call unit_test_done('state',msg='')
end subroutine test_state
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_strlength()
implicit none
   call unit_test_start('strlength',msg='')
   !!call unit_test('strlength', 0.eq.0, 'checking',100)
   call unit_test_done('strlength',msg='')
end subroutine test_strlength
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_textang()
implicit none
   call unit_test_start('textang',msg='')
   !!call unit_test('textang', 0.eq.0, 'checking',100)
   call unit_test_done('textang',msg='')
end subroutine test_textang
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_textsize()
implicit none
   call unit_test_start('textsize',msg='')
   !!call unit_test('textsize', 0.eq.0, 'checking',100)
   call unit_test_done('textsize',msg='')
end subroutine test_textsize
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_vexit()
implicit none
   call unit_test_start('vexit',msg='')
   !!call unit_test('vexit', 0.eq.0, 'checking',100)
   call unit_test_done('vexit',msg='')
end subroutine test_vexit
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_vflush()
implicit none
   call unit_test_start('vflush',msg='')
   !!call unit_test('vflush', 0.eq.0, 'checking',100)
   call unit_test_done('vflush',msg='')
end subroutine test_vflush
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_viewport()
implicit none
   call unit_test_start('viewport',msg='')
   !!call unit_test('viewport', 0.eq.0, 'checking',100)
   call unit_test_done('viewport',msg='')
end subroutine test_viewport
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_vinit()
implicit none
   call unit_test_start('vinit',msg='')
   !!call unit_test('vinit', 0.eq.0, 'checking',100)
   call unit_test_done('vinit',msg='')
end subroutine test_vinit
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_xcentertext()
implicit none
   call unit_test_start('xcentertext',msg='')
   !!call unit_test('xcentertext', 0.eq.0, 'checking',100)
   call unit_test_done('xcentertext',msg='')
end subroutine test_xcentertext
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_ycentertext()
implicit none
   call unit_test_start('ycentertext',msg='')
   !!call unit_test('ycentertext', 0.eq.0, 'checking',100)
   call unit_test_done('ycentertext',msg='')
end subroutine test_ycentertext
!===================================================================================================================================
end program runtest
