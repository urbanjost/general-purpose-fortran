          program demo_atomnum2symbol
          use M_units, only :  atomnum2symbol
          implicit none
          character(len=2)  :: name
          integer           :: i
          do i=1,109
             call atomnum2symbol(i,name)
             write(*,*)i,name
          enddo
          end program demo_atomnum2symbol
