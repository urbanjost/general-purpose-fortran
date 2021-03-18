          program demo_fline
          use M_calcomp
          implicit none
          ! based on concepts of CALIFORNIA COMPUTER PRODUCTS, 1968
          real :: xar(10)=[0.75,1.75,2.25,2.75,3.25,4.25,4.75,5.75,0.0,1.0]
          real :: yar(10)=[3.25,2.00,5.25,6.50,6.75,6.25,3.25,4.25,0.0,1.0]
          character(len=50) :: ibcd
          integer           :: inteq
             call plots(0.0,10.0,0.0,10.0)
          !     DRAW FRAME
             call plot(7.0,0.0,2)
             call plot(7.0,9.0,2)
             call plot(0.0,9.0,2)
             call plot(0.0,0.0,2)
          !     DRAW AXIS
             ibcd='SERVICE TIME'
             call axis(0.75,0.75,ibcd,-12,5.0,0.0,5.0,1.0)
             ibcd='FREQUENCY'
             call axis(0.75,0.75,ibcd, 9,7.0,90.0,0.0,100.0)
          !     DRAW COMMENTS
             ibcd='USING FLINE AND SMOOT SUBROUTINES'
             call symbol(0.7,8.25,0.14,ibcd,inteq,0.0,34)
             call plot(5.0,7.8,3)
             call plot(5.1,7.8,2)
             ibcd='SMOOT'
             call symbol(5.2,7.80,0.09,ibcd,inteq,0.0, 6)
             inteq = 1
             call symbol(5.0,7.60,0.10,ibcd,inteq,0.0,-1)
             inteq=999
             ibcd='FLINE'
             call symbol(5.2,7.60,0.09,ibcd,inteq,0.0, 5)
          ! SMOOTHING
             call smoot(0.75,3.75,0)
             call smoot(1.75,2.5,-2)
             call smoot(2.25,5.75,-2)
             call smoot(2.75,7.0,-2)
             call smoot(3.25,7.25,-2)
             call smoot(4.25,6.75,-2)
             call smoot(4.75,3.75,-2)
             call smoot(5.75,4.75,-24)
          ! FLINE IS USED
             call plot(0.75,3.25,3)
             call fline(xar, yar, -8,1,1,1)
             call nframe()
          end program demo_fline
