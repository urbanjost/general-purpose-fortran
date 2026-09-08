        program demo_nextp
        use M_math, only : nextp
        integer,parameter :: n=4
        integer i,a(n)
        a=[(i,i=1,n)]  ! Must be sorted from smallest to largest
        do
           print *,(a(i),i=1,n)
           if(.not.nextp(n,a)) exit
        enddo
        end program demo_nextp
