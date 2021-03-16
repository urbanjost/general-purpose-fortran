           program demo_hue
           use M_pixel, only : hue
           implicit none
              !                      NAME       RGB(0-255)            HLS(0-100)
              call check_name('hls','red',      [ 100, 0,   0   ],[ 0,   50,  100 ])
              call check_name('hls','orange',   [ 100, 65,  0   ],[ 39,  50,  100 ])
              call check_name('hls','yellow',   [ 100, 100, 0   ],[ 60,  50,  100 ])
              call check_name('hls','green',    [ 0,   100, 0   ],[ 120, 50,  100 ])
              call check_name('hls','cyan',     [ 0,   100, 100 ],[ 180, 50,  100 ])
              call check_name('hls','blue',     [ 0,   0,   100 ],[ 240, 50,  100 ])
              call check_name('hls','magenta',  [ 100, 0,   100 ],[ 300, 50,  100 ])
              call check_name('hls','black',    [ 0,   0,   0   ],[ 0,   0,   0   ])
              call check_name('hls','white',    [ 100, 100, 100 ],[ 0,   100, 0   ])
              call check_name('hsv','black',    [ 0,   0,   0   ],[ 0,   0,   0   ])
              !                      NAME        RGB(0-255)            HSV(0-100)
              call check_name('hsv','gray50',   [ 50,  50,  50  ],[ 0,   0,   50  ])
              call check_name('hsv','silver',   [ 75,  75,  75  ],[ 0,   0,   75  ])
              call check_name('hsv','white',    [ 100, 100, 100 ],[ 0,   0,   100 ])
              call check_name('hsv','red4',     [ 55,  0,   0   ],[ 0,   100, 55  ])
              call check_name('hsv','red',      [ 100, 0,   0   ],[ 0,   100, 100 ])
              call check_name('hsv','olive',    [ 50,  50,  0   ],[ 60,  100, 50  ])
              call check_name('hsv','yellow',   [ 100, 100, 0   ],[ 60,  100, 100 ])
              call check_name('hsv','green',    [ 0,   100, 0   ],[ 120, 100, 100 ])
              call check_name('hsv','lime',     [ 0,   100, 0   ],[ 120, 100, 100 ])
              call check_name('hsv','teal',     [ 0,   50,  50  ],[ 180, 100, 50  ])
              call check_name('hsv','cyan',     [ 0,   100, 100 ],[ 180, 100, 100 ])
              call check_name('hsv','navy',     [ 0,   0,   50  ],[ 240, 100, 50  ])
              call check_name('hsv','blue',     [ 0,   0,   100 ],[ 240, 100, 100 ])
              call check_name('hsv','purple',   [ 63,  13,  94  ],[ 277, 87,  94  ])
              call check_name('hsv','magenta4', [ 55,  0,   55  ],[ 300, 100, 55  ])
              call check_name('hsv','magenta',  [ 100, 0,   100 ],[ 300, 100, 100 ])
              call check_name('hsv','maroon',   [ 69,  19,  38  ],[ 338, 73,  69  ])
           contains
           subroutine check_name(modelout,name,rgb,other)
           ! given a color convert to MODELOUT and compare to expected values
           character(len=*),intent(in)   :: name
           integer,intent(in)            :: rgb(3), other(3)
           character(len=*),intent(in)   :: modelout
              real                       :: val1,val2,val3
              integer                    :: status
              ! convert RGB values to MODELOUT values
              call hue('rgb',REAL(rgb(1)),REAL(rgb(2)),REAL(rgb(3)), &
              & modelout,val1,val2,val3,status)
                 write(*,*)'COLOR '//trim(name)
                 write(*,*)'EXPECTED '//modelout//' ====>',other
                 write(*,*)'RETURNED '//modelout//' ====>', &
                 & int([val1+0.5,val2+0.5,val3+0.5])
                 write(*,*)'STATUS ==========>',status
           end subroutine check_name
           end program demo_hue
