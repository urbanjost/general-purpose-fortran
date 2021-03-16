          program demo_number
          use M_calcomp
          implicit none
          character(len=28),parameter :: ichr4='EXAMPLE OF NUMBER SUBROUTINE'
          real,parameter              :: znum(4)=[10293.84756,193.75,-204.86,-12345.6789]
          real                        :: y
          integer                     :: ia, ib
          integer                     :: inteq
             call plots(0.0,10.0,0.0,10.0)
             y=9.5
             ! the following tests the NUMBER(3f) subroutine for precision
             call symbol(0.5,2.5,.20,ichr4,inteq,90.0,28)
             y=10.0
             do ia=1,4
                do ib=1,11
                   call number(1.0,y,0.14,znum(ia),0.0,ib-6)
                   y=y-0.2
                enddo
                y=y-0.3
             enddo
             call nframe()
             call plot(0.0,0.0,999)
          end program demo_number
