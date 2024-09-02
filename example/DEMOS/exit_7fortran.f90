     program demo_exit
     implicit none
     integer :: i, j
     logical :: big
        ! EXIT a simple loop
        do i=1, 10
           if(i .eq. 4) exit ! exit loop
        enddo
        write(*,*)'I=',i

        ! EXIT only exits an innermost loop
        do i=1,10
           do j=100, 200
              if(j .eq. 150) exit ! exit inner loop "j" but not "i"
           enddo
        enddo
        write(*,*)'I=',i,' J=',j

        ! EXIT an outermost loop from inner loop by using a construct name
        OUTER: do i=1,10
           INNER: do j=100, 200
              if(j .eq. 150) exit OUTER ! exit named loop "i"
           enddo INNER
        enddo OUTER
        write(*,*)'I=',i,' J=',j

        ! EXIT a BLOCK not just a DO loop
        MYBLOCK: block
           big = .false.
           do i = 1, 100
             if( i==40 )then
               exit MYBLOCK
             endif
           enddo
           big = .true.
        endblock MYBLOCK
        write(*,*)'I=',i,' BIG=',big
     end program demo_exit
