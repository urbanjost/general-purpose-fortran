           program demo_random_kiss64
           use M_random, only : random_kiss64
           implicit none
           integer, parameter    :: i8b = selected_int_kind(18)  ! eight-byte integer
           integer(i8b)          :: i, t

              write(*,*)'HUGE=',huge(0_i8b)

              do i = 1, 100000000
                 t = random_kiss64()
                 if(mod(i,1000000_i8b+1_i8b)==1000000_i8b)write(*,*)i,' T=',T
              enddo

              if (t .eq. 1666297717051644203_i8b) then
                 print *, "100 million calls to KISS() OK"
              else
                 print *, "Fail"
              endif
       end program demo_random_kiss64
