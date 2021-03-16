              program demo_hue
              use M_color, only : hue
              implicit none
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
                 call chk('hsv','black',   [0,   0,   0  ], [0,   0,   0  ])
                 call chk('hsv','white',   [100, 100, 100], [0,   0,   100])

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
              contains
              subroutine chk(modelout,name,rgb,other)
              ! given a color convert to MODELOUT and compare to expected values
              character(len=*),intent(in)   :: name
              integer,intent(in)            :: rgb(3), other(3)
              character(len=*),intent(in)   :: modelout
                 real                       :: val1,val2,val3
                 integer                    :: status
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
              end program demo_hue
