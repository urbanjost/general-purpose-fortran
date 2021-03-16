          program demo_dashl
          use M_calcomp
          character(len=28) :: ichr1
          character(len=26) :: ichr2
          character(len=10) :: lbcd1,lbcd2
          dimension xarray(62),yarray(62)
          ICHR1='PLOTTED ON A CALCOMP PLOTTER'
          ICHR2='USING  Y = X -0.7*X +0.1*X'
          LBCD1='X-ABSCISSA'
          LBCD2='Y-ORDINATE'
          ! PLOT GRAPH ILLUSTRATING SCALE, AXIS, AND LINE
          deltax=0.04
          ! AXIS DRAWS LABELS AS MUCH AS 0.4 INCHES TO THE NEGATIVE OF AXIS CENTER;
          ! EITHER USE AN ORIGIN OFFSET OF AT LEAST THIS VALUE OR DO NOT USE AN
          ! ORIGIN VALUE OF LESS THAN 0.4 OR CLIPPING WILL OCCUR
          call plots(0.0,12.0,0.0,12.0)
          call width(0)
          call newpen(WHITE)
          call rect(0.0,0.0,11.0,10.0,0.0,7)
          call plot(0.4,0.4,-MOVE)
          deltax=2.0*deltax
          xarray(1)=deltax
          do j=1,60
             yarray(j)=xarray(j)**2-0.7*xarray(j)**3+0.1*xarray(j)**4
             xarray(j+1)=xarray(j)+deltax
          enddo
          call scale(xarray(1), 6.5,60,1)
          call scale(yarray(1),10.0,60,1)
          call axis(0.0,0.0,lbcd1,-10, 6.5, 0.0,xarray(61),xarray(62))
          call axis(0.0,0.0,lbcd2, 10,10.0,90.0,yarray(61),yarray(62))
          call width(20)
          !!call newpen(RED)
          !!linetype=-1
          !!inteq=4
          !!call line(xarray(1),yarray(1),60,1,linetype,inteq)
          call newpen(GREEN)
          call dashl(xarray(1),yarray(1),60,1)
          call newpen(1)
          call symbol(1.3,10.,.14,ichr1,inteq,0.0,28)
          call symbol(1.3,9.7,.14,ichr2,inteq,0.0,26)
          call width(0)
          call number(2.98,9.8,.1,2.0,0.,-1)
          call number(3.96,9.8,.1,3.0,0.,-1)
          call number(4.94,9.8,.1,4.0,0.,-1)
          call plot(0.0,0.0,END)
          end program demo_dashl
