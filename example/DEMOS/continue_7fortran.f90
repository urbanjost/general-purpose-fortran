      program demo_continue
      ! numbered targets should (almost?) always be a continue statement
      ! with a unique label for each looping structure
      integer :: i,j
        j=5
        do 100 i=1,20
           if(i.lt.5)goto 50
           j=3
           50 continue
           write(*,*)'J=',j
        100 continue
      end program demo_continue
