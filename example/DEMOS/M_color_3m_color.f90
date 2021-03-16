                 program demo_M_color
                 use M_color, only : hue
                 use M_color, only : closest_color_name
                 use M_color, only : color_name2rgb
                 use M_color, only : rgbmono
                 implicit none
                 character(len=100) :: string ! at least 20 characters
                 character(len=20)  :: name
                 character(len=20)  :: echoname
                 real               :: red,green,blue
                 real               :: gray
                 integer            :: ierr
                    ! find the names of colors given RGB values
                    write(*,*)'Find names given values'

                    call closest_color_name( 100.0,   0.0,   0.0, string)
                    write(*,*)trim(string)

                    call closest_color_name(   0.0, 100.0,   0.0, string)
                    write(*,*)trim(string)

                    call closest_color_name(   0.0,   0.0, 100.0, string)
                    write(*,*)trim(string)

                    ! list colors known to colorname2rgb(3f) & corresponding RGB values
                    write(*,*)'given names find RGB values'
                    ! get the RGB values and English name of the color
                    call color_name2rgb('RED',red,green,blue,echoname)
                    ! display the English name and RGB values for the name
                    write(*,*)echoname,int([red,green,blue])

                    write(*,*)'Do some conversions between RGB, HLS, and HLS'
                    write(*,*)'and check them against expected values'
                    !               NAME        RGB(0-255)            HLS(0-100)
                    call chk('hls','red',     [100, 0,   0  ], [0,   50,  100])
                    call chk('hls','orange',  [100, 65,  0  ], [39,  50,  100])
                    call chk('hls','yellow',  [100, 100, 0  ], [60,  50,  100])
                    call chk('hls','green',   [0,   100, 0  ], [120, 50,  100])
                    call chk('hls','cyan',    [0,   100, 100], [180, 50,  100])
                    call chk('hls','blue',    [0,   0,   100], [240, 50,  100])
                    call chk('hls','magenta', [100, 0,   100], [300, 50,  100])
                    call chk('hls','black',   [0,   0,   0  ], [0,   0,   0  ])
                    call chk('hls','white',   [100, 100, 100], [0,   100, 0  ])
                    !               NAME        RGB(0-255)           HSV(0-100)
                    call chk('hsv','red',     [100, 0,   0  ], [0,   100, 100])
                    call chk('hsv','yellow',  [100, 100, 0  ], [60,  100, 100])
                    call chk('hsv','green',   [0,   100, 0  ], [120, 100, 100])
                    call chk('hsv','cyan',    [0,   100, 100], [180, 100, 100])
                    call chk('hsv','blue',    [0,   0,   100], [240, 100, 100])
                    call chk('hsv','magenta', [100, 0,   100], [300, 100, 100])
                    call chk('hsv','white',   [100, 100, 100], [0,   0,   100])
                    call chk('hsv','black',   [0,   0,   0  ], [0,   0,   0  ])
                    call chk('hsv','gray50',  [50,  50,  50 ], [0,   0,   50 ])
                    call chk('hsv','silver',  [75,  75,  75 ], [0,   0,   75 ])
                    call chk('hsv','red4',    [55,  0,   0  ], [0,   100, 55 ])
                    call chk('hsv','olive',   [50,  50,  0  ], [60,  100, 50 ])
                    call chk('hsv','lime',    [0,   100, 0  ], [120, 100, 100])
                    call chk('hsv','teal',    [0,   50,  50 ], [180, 100, 50 ])
                    call chk('hsv','navy',    [0,   0,   50 ], [240, 100, 50 ])
                    call chk('hsv','purple',  [63,  13,  94 ], [277, 87,  94 ])
                    call chk('hsv','magenta4',[55,  0,   55 ], [300, 100, 55 ])
                    call chk('hsv','maroon',  [69,  19,  38 ], [338, 73,  69 ])

                    write(*,*)'Get some grayscale values from RGB color values'
                    call rgbmono(100.0,  0.0,  0.0,gray,ierr); write(*,*)'red     ',gray
                    call rgbmono(  0.0,100.0,  0.0,gray,ierr); write(*,*)'green   ',gray
                    call rgbmono(  0.0,  0.0,100.0,gray,ierr); write(*,*)'blue    ',gray
                    call rgbmono(100.0,100.0,  0.0,gray,ierr); write(*,*)'Yellow  ',gray
                    call rgbmono(  0.0,100.0,100.0,gray,ierr); write(*,*)'Cyan    ',gray
                    call rgbmono(100.0,  0.0,100.0,gray,ierr); write(*,*)'Magenta ',gray
                    call rgbmono(100.0,100.0,100.0,gray,ierr); write(*,*)'White   ',gray
                    call rgbmono( 00.0,  0.0,  0.0,gray,ierr); write(*,*)'Black   ',gray
                    call rgbmono( 50.0,  0.0,  0.0,gray,ierr); write(*,*)'Maroon  ',gray
                    call rgbmono(100.0, 50.0, 50.0,gray,ierr); write(*,*)'Pink    ',gray
                    contains
                    subroutine chk(modelout,name,rgb,other)
                    ! given a color convert to MODELOUT and compare to expected values
                    character(len=*),intent(in)   :: name
                    integer,intent(in)            :: rgb(3), other(3)
                    character(len=*),intent(in)   :: modelout
                    real                          :: val1,val2,val3
                    integer                       :: status
                       ! convert RGB values to MODELOUT values
                       call hue('rgb',REAL(rgb(1)),REAL(rgb(2)),REAL(rgb(3)),&
                       & modelout,val1,val2,val3,status)
                       ! left-justify name to 10 characters or more
                       write(*,'(a,1x)',advance='no') &
                       & [ character(len=max(10,len_trim(name))) ::' '//trim(name)]
                       write(*,'(a,1x,3(i3,1x))',advance='no') &
                       & modelout//' EXPECTED',other
                       write(*,'(a,1x,3(i3,1x))',advance='no') &
                       & 'GOT',int([val1+0.5,val2+0.5,val3+0.5])
                       write(*,'(a,i0)')'STATUS ',status
                    end subroutine chk
                 end program demo_M_color
