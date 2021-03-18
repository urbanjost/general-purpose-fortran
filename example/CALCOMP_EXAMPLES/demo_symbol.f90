          program demo_symbol
          use M_calcomp

          ! produce a symbol table which shows the characters
          ! available in the symbol(3f) routine.
          !
          character(len= 38),parameter :: ichr1='CHARACTERS AVAILABLE IN SYMBOL ROUTINE'
          character(len= 38),parameter :: ichr2='  FOR CALCOMP ON THE CRAY COMPUTER    '
          character(len= 60),parameter :: ichr3='INTEGER FOR USE IN SYMBOL CALL SHOWN TO LEFT OF EACH SYMBOL'
          character(len= 1 )           :: ibcd
          integer                      :: ia,ib
          integer                      :: m
          real                         :: z, xs, ys, x, y
             call plots(0.0,10.0,0.0,10.0)
             call plot(0.8,0.8,1001)
             call plot(0.0,11.0,2)
             call plot(8.5,11.0,2)
             call plot(8.5,0.0,2)
             call plot(0.0,0.0,2)
             call symbol(0.4,10.50,.2,ichr1,inteq,0.0,38)
             call symbol(0.4,10.25,.2,ichr2,inteq,0.0,38)
             call plot(8.1,10.0,3)
             call plot(0.4,10.0,2)
             call plot(0.4, 0.5,2)
             call plot(8.1, 0.5,2)
             z=0.0
             m=0
             xs=0.85
             ys=0.25
             x=0.4
             y=9.5
             do ia=1,6
                do ib=1,15
                   call number(x+0.10,y+0.18,0.14,z,0.0,-1)
                   call symbol(x+xs,y+ys, 0.4, ibcd,m,0.0,-1)
                   Z=Z+1.0
                   M=M+1
                   Y=Y-0.6
                enddo
                if(ia.eq.6) call number(x+0.10,y+0.18,0.14,z,0.0,-1)
                if(ia.eq.6) call symbol(x+xs,y+ys,0.4,ibcd,m,0.0,-1)
                x=x+1.283
                call plot(x,0.5,3)
                call plot(x,10.0,2)
                y=9.5
                xs=0.65
                ys=0.05
             enddo
             call symbol(0.6,0.25,0.12,ichr3,inteq,0.0,60)
             call nframe()
             call plot(0.0,0.0,999)
          end program demo_symbol
