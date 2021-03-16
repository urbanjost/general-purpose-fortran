          program demo_almost
          use M_verify, only : almost
          real    :: x, y
          logical :: z
          x=1.2345678
          y=1.2300000
          do i=1,8
             z=almost(x,y,real(i),verbose=.true.)
             write(*,*)i,z
          enddo
          end program demo_almost
