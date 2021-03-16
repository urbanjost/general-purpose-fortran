          program demo_where
          !  Example of WHERE, ELSE WHERE, END WHERE
          parameter (nd=10, ndh=nd/2, nduh=nd-ndh-1)
          real, dimension(nd):: a=[ (2*j,j=1,nd) ]
          real, dimension(nd):: b ! =[ ndh*1.0, 0.0, nduh*2.0 ]
          real, dimension(nd):: c ! =[ nd*-77.77 ]
          integer iflag(nd)
          data b/ndh*1,0.0,nduh*2./,c/nd*-77.77/

          where (b.ne.0) c=a/b
          write (*,2000) c(1:nd)
          !
          !  The above protects against divide by zero, but doesn't actually assign
          !  values to elements in c when the corresponding element in b is zero
          !  The following covers that, and sets a flag when a divide by zero is
          !  present
          !
          where (b(1:nd).ne.0.0)
             c=a/b
             iflag=0
          else where
             c=0.0
             iflag=1
          end where

          write (*,2000) c(1:nd)
          write (*,1000) iflag(1:nd)
          1000 format ('iflag= ',/,(10i7))
          2000 format ('a/b = ',/,(10f7.2))
          end program demo_where
