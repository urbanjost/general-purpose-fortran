!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
module M_testsuite
use M_color, only : rgbmono, hue, closest_color_name, color_name2rgb
use M_verify, only : unit_check, unit_check_good, unit_check_bad, unit_check_done, unit_check_start
contains
subroutine test_suite_m_color
call test_hue()                 ! converts a color's components from one color model to another
call test_closest_color_name()  ! given RGB values, try to find closest named color
call test_color_name2rgb()      ! given a color name, return rgb color values in range 0 to 100
call test_rgbmono()             ! convert RGB colors to a reasonable grayscale
contains
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_rgbmono()
real :: gray
integer :: ierr
call unit_check_start('rgbmono')
   call rgbmono(100.0,  0.0,  0.0,gray,ierr); call unit_check('rgbmono',int(gray+0.5).eq.30, msg='red     ')
   call rgbmono(  0.0,100.0,  0.0,gray,ierr); call unit_check('rgbmono',int(gray+0.5).eq.59, msg='green   ')
   call rgbmono(  0.0,  0.0,100.0,gray,ierr); call unit_check('rgbmono',int(gray+0.5).eq.11, msg='blue    ')
   call rgbmono(100.0,100.0,  0.0,gray,ierr); call unit_check('rgbmono',int(gray+0.5).eq.89, msg='Yellow  ')
   call rgbmono(  0.0,100.0,100.0,gray,ierr); call unit_check('rgbmono',int(gray+0.5).eq.70, msg='Cyan    ')
   call rgbmono(100.0,  0.0,100.0,gray,ierr); call unit_check('rgbmono',int(gray+0.5).eq.41, msg='Magenta ')
   call rgbmono(100.0,100.0,100.0,gray,ierr); call unit_check('rgbmono',int(gray+0.5).eq.100,msg='White   ')
   call rgbmono( 00.0,  0.0,  0.0,gray,ierr); call unit_check('rgbmono',int(gray+0.5).eq.0,  msg='Black   ')
   call rgbmono( 50.0,  0.0,  0.0,gray,ierr); call unit_check('rgbmono',int(gray+0.5).eq.15, msg='Maroon  ')
   call rgbmono(100.0, 50.0, 50.0,gray,ierr); call unit_check('rgbmono',int(gray+0.5).eq.65, msg='Pink    ')
call unit_check_done('rgbmono')
end subroutine test_rgbmono
end subroutine test_suite_m_color
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_hue()
use M_verify, only : unit_check, unit_check_good, unit_check_bad, unit_check_done, unit_check_start
use M_verify, only : unit_check_level
implicit none
real              :: c, m, y
real              :: r, g, b
real              ::    i, q
integer           :: status
call unit_check_start('hue')
   call hue('cmy',100.0,  0.0,  0.0,'rgb',r,g,b,status)
   call unit_check('hue',all([r.eq.  0.0,g.eq.100.0,b.eq.100.0 ]),'cyan cmy 100 0 0 to rgb',r,g,b)

   call hue('cmy',  0.0,100.0,  0.0,'rgb',r,g,b,status)
   call unit_check('hue',all([r.eq.100.0,g.eq.  0.0,b.eq.100.0 ]),'magenta cmy to rgb')

   call hue('cmy',  0.0,  0.0,100.0,'rgb',r,g,b,status)
   call unit_check('hue',all([r.eq.100.0,g.eq.100.0,b.eq.  0.0 ]),'yellow cmy to rgb')

   call hue('rgb',100.0,  0.0,100.0,'cmy',c,m,y,status)
   call unit_check('hue',all([c.eq.  0.0,m.eq.100.0,y.eq.  0.0 ]),'magenta rgb to cmy')

   call hue('rgb',  0.0,100.0,100.0,'cmy',c,m,y,status)
   call unit_check('hue',all([c.eq.100.0,m.eq.  0.0,y.eq.  0.0 ]),'cyan rgb to cmy')

   call hue('rgb',100.0,100.0,  0.0,'cmy',c,m,y,status)
   call unit_check('hue',all([c.eq.  0.0,m.eq.  0.0,y.eq.100.0 ]),'yellow rgb to cmy')


   call hue('rgb',100.0,  0.0,  100.0,'yiq',y,i,q,status)
   call unit_check('hue',all(abs([y-41.2999992,i-27.43087,q-52.2599983]).lt.0.10),'rgb to yiq',y,i,q,'for green')

   call hue('rgb',  0.0,100.0,  0.0,'yiq',y,i,q,status)
   call unit_check('hue',all(abs([ y-58.70147,i-(-27.43087),q-(-52.29881) ]).lt.0.10),'rgb to yiq',y,i,q,'for magenta')

   call hue('yiq',41.29853,27.43087,52.29881 ,'rgb',r,g,b,status)
   call unit_check('hue',all(abs([r-100.0,  g-0.0,b-100.0]).lt.0.01),msg='yiq to rgb for green')

   call hue('yiq',58.70147,-27.43087,-52.29881,'rgb',r,g,b,status)
   call unit_check('hue',all(abs([r-0.0,g-100.0, b-0.0]).lt.0.01),msg='yiq to rgb for magenta')


   !                      NAME        RGB(0-255)            HLS(0-100)
   call check_name('hls', 'red',      [ 100,  0,    0   ], [ 0,    50,   100 ])
   call check_name('hls', 'orange',   [ 100,  65,   0   ], [ 39,   50,   100 ])
   call check_name('hls', 'yellow',   [ 100,  100,  0   ], [ 60,   50,   100 ])
   call check_name('hls', 'green',    [ 0,    100,  0   ], [ 120,  50,   100 ])
   call check_name('hls', 'cyan',     [ 0,    100,  100 ], [ 180,  50,   100 ])
   call check_name('hls', 'blue',     [ 0,    0,    100 ], [ 240,  50,   100 ])
   call check_name('hls', 'magenta',  [ 100,  0,    100 ], [ 300,  50,   100 ])
   call check_name('hls', 'black',    [ 0,    0,    0   ], [ 0,    0,    0   ])
   call check_name('hls', 'white',    [ 100,  100,  100 ], [ 0,    100,  0   ])
   call check_name('hsv', 'black',    [ 0,    0,    0   ], [ 0,    0,    0   ])
   !                      NAME        RGB(0-255)            HSV(0-100)
   call check_name('hsv', 'gray50',   [ 50,   50,   50  ], [ 0,    0,    50  ])
   call check_name('hsv', 'silver',   [ 75,   75,   75  ], [ 0,    0,    75  ])
   call check_name('hsv', 'white',    [ 100,  100,  100 ], [ 0,    0,    100 ])
   call check_name('hsv', 'red4',     [ 55,   0,    0   ], [ 0,    100,  55  ])
   call check_name('hsv', 'red',      [ 100,  0,    0   ], [ 0,    100,  100 ])
   call check_name('hsv', 'olive',    [ 50,   50,   0   ], [ 60,   100,  50  ])
   call check_name('hsv', 'yellow',   [ 100,  100,  0   ], [ 60,   100,  100 ])
   call check_name('hsv', 'green',    [ 0,    100,  0   ], [ 120,  100,  100 ])
   call check_name('hsv', 'lime',     [ 0,    100,  0   ], [ 120,  100,  100 ])
   call check_name('hsv', 'teal',     [ 0,    50,   50  ], [ 180,  100,  50  ])
   call check_name('hsv', 'cyan',     [ 0,    100,  100 ], [ 180,  100,  100 ])
   call check_name('hsv', 'navy',     [ 0,    0,    50  ], [ 240,  100,  50  ])
   call check_name('hsv', 'blue',     [ 0,    0,    100 ], [ 240,  100,  100 ])
   call check_name('hsv', 'purple',   [ 63,   13,   94  ], [ 277,  87,   94  ])
   call check_name('hsv', 'magenta4', [ 55,   0,    55  ], [ 300,  100,  55  ])
   call check_name('hsv', 'magenta',  [ 100,  0,    100 ], [ 300,  100,  100 ])
   call check_name('hsv', 'maroon',   [ 69,   19,   38  ], [ 338,  73,   69  ])

call unit_check_done('hue')

contains
subroutine check_name(modelout,name,rgb,other)
implicit none

! given a color convert to MODELOUT and compare to expected values

character(len=*),intent(in)   :: name
integer,intent(in)            :: rgb(3), other(3)
character(len=*),intent(in)   :: modelout
   real                       :: r,g,b
   real                       :: val1,val2,val3
   integer                    :: status

   r=real(rgb(1))
   g=real(rgb(2))
   b=real(rgb(3))
   ! convert RGB values to MODELOUT values
   call hue('rgb',r,g,b,modelout,val1,val2,val3,status)
   if(unit_check_level.gt.0)then
      write(*,*)'EXPECTED '//modelout//' ====>',other
      write(*,*)'RETURNED '//modelout//' ====>',int([val1+0.5,val2+0.5,val3+0.5])
      write(*,*)'STATUS ==========>',status
   endif
   call unit_check('hue', status.eq.0.and.all(abs(int([val1+0.5,val2+0.5,val3+0.5])-other).lt.2 ),'convert from rgb to '//modelout)

end subroutine check_name
!===================================================================================================================================
end subroutine test_hue
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_closest_color_name()
use M_verify, only : unit_check, unit_check_good, unit_check_bad, unit_check_done, unit_check_start
implicit none
call unit_check_start('closest_color_name')
   !                NAME       RGB(0-255)
   call check_name('black',    [ 0,    0,    0   ])
   call check_name('blue',     [ 0,    0,    100 ])
   call check_name('cyan',     [ 0,    100,  100 ])
   call check_name('gray50',   [ 50,   50,   50  ])
   call check_name('green',    [ 0,    100,  0   ])
   call check_name('magenta',  [ 100,  0,    100 ])
   call check_name('magenta4', [ 55,   0,    55  ])
   call check_name('maroon',   [ 69,   19,   38  ])
   call check_name('navy',     [ 0,    0,    50  ])
   call check_name('olive',    [ 50,   50,   0   ])
   call check_name('orange',   [ 100,  65,   0   ])
   call check_name('purple',   [ 63,   13,   94  ])
   call check_name('red',      [ 100,  0,    0   ])
   call check_name('red4',     [ 55,   0,    0   ])
   call check_name('silver',   [ 75,   75,   75  ])
   call check_name('teal',     [ 0,    50,   50  ])
   call check_name('white',    [ 100,  100,  100 ])
   call check_name('yellow',   [ 100,  100,  0   ])
   call check_name('lime',     [ 0,    100,  0   ])

call unit_check_done('closest_color_name')

contains
subroutine check_name(name,rgb)
! given a colorname look up RGB values, compare to expected values, check
implicit none
character(len=*)   :: name
integer,intent(in) :: rgb(3)
real               :: r, g, b
real               :: r2,g2,b2
character(len=20)  :: echoname
character(len=20)  :: closestname

   r=real(rgb(1))
   g=real(rgb(2))
   b=real(rgb(3))

   ! see if get expected name
   call closest_color_name(r,g,b,closestname)
   if(closestname.eq.name)then
      call unit_check('closest_color_name',closestname.eq.name,msg='names match for '//name)
   else
   ! did not get back name put in; but maybe alternate alias
      ! if values the same assume OK
      call color_name2rgb(name,r2,g2,b2,echoname)
      call unit_check('closest_color_name',     &
      & rgb(1) .eq. int(r2+0.5) .and. &
      & rgb(2) .eq. int(g2+0.5) .and. &
      & rgb(3) .eq. int(b2+0.5)  ,msg='close enough for NAME: '//trim(name)//' CLOSESTNAME: '//trim(closestname) )
   endif
end subroutine check_name
!===================================================================================================================================
end subroutine test_closest_color_name
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_color_name2rgb()
use M_verify, only : unit_check, unit_check_good, unit_check_bad, unit_check_done, unit_check_start
implicit none
call unit_check_start('color_name2rgb')
   !                NAME       RGB(0-255)
   call check_name('black',    [ 0,    0,    0   ])
   call check_name('blue',     [ 0,    0,    100 ])
   call check_name('cyan',     [ 0,    100,  100 ])
   call check_name('gray50',   [ 50,   50,   50  ])
   call check_name('green',    [ 0,    100,  0   ])
   call check_name('magenta',  [ 100,  0,    100 ])
   call check_name('magenta4', [ 55,   0,    55  ])
   call check_name('maroon',   [ 69,   19,   38  ])
   call check_name('navy',     [ 0,    0,    50  ])
   call check_name('olive',    [ 50,   50,   0   ])
   call check_name('orange',   [ 100,  65,   0   ])
   call check_name('purple',   [ 63,   13,   94  ])
   call check_name('red',      [ 100,  0,    0   ])
   call check_name('red4',     [ 55,   0,    0   ])
   call check_name('silver',   [ 75,   75,   75  ])
   call check_name('teal',     [ 0,    50,   50  ])
   call check_name('white',    [ 100,  100,  100 ])
   call check_name('yellow',   [ 100,  100,  0   ])
   call check_name('lime',     [ 0,    100,  0   ])

call unit_check_done('color_name2rgb')
contains
subroutine check_name(name,rgb)
! given a colorname look up RGB values, compare to expected values, check
implicit none
character(len=*)   :: name
integer,intent(in) :: rgb(3)
real               :: r,g,b
real               :: r1,g1,b1
real               :: r2,g2,b2
character(len=20)  :: echoname
character(len=20)  :: closestname
   call color_name2rgb(name,r,g,b,echoname)                                   ! given a color name look up RGB values in range 0-100
   call unit_check('color_name2rgb',echoname.ne.'Unknown',msg='make sure echoed name does not equal unknown')
   call unit_check('color_name2rgb',all(int([r+0.5,g+0.5,b+0.5]).eq.rgb),msg='check returned RGB values against expected values' )

   call closest_color_name(r,g,b,closestname)                                 ! see if get name back
   if(closestname.eq.name)then
      call unit_check('color_name2rgb',closestname.eq.name,msg='names match for '//name)
   else                                                     ! did not get back name put in; but maybe alternate alias
      ! if values the same assume OK
      call color_name2rgb(closestname,r1,g1,b1,echoname)
      call color_name2rgb(name,r2,g2,b2,echoname)
      call unit_check('color_name2rgb',     &
      & int(r1+0.5) .eq. int(r2+0.5) .and. &
      & int(g1+0.5) .eq. int(g2+0.5) .and. &
      & int(b1+0.5) .eq. int(b2+0.5)  ,msg='close enough for NAME: '//trim(name)//' CLOSESTNAME: '//trim(closestname) )
   endif

end subroutine check_name
!===================================================================================================================================
end subroutine test_color_name2rgb
end module M_testsuite
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
program runtest
use M_verify, only : unit_check_command, unit_check_keep_going, unit_check_level
use M_testsuite
implicit none
   unit_check_command=''
   unit_check_keep_going=.true.
   unit_check_level=0
   call test_suite_M_color()
contains

end program runtest
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
