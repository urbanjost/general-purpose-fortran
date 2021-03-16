             program demo_lowess
             use M_math, only : lowess
             !  test driver for lowess
             !  for expected output, see introduction
             real x(20), y(20), ys(20), rw(20), res(20)
             data x /1,2,3,4,5,10*6,8,10,12,14,50/
             data y /18,2,15,6,10,4,16,11,7,3,14,17,20,12,9,13,1,8,5,19/
             call lowess(x,y,20,.25,0,0.,ys,rw,res)
             write(*,*) ys
             call lowess(x,y,20,.25,0,3.,ys,rw,res)
             write(*,*) ys
             call lowess(x,y,20,.25,2,0.,ys,rw,res)
             write(*,*) ys
             end program demo_lowess
