          program demo_rgbmono
          use M_color, only : rgbmono
          implicit none
          real    :: gray
          integer :: ierr
          call rgbmono(100.0,  0.0,  0.0,gray,ierr); write(*,*)'red     ',gray
          call rgbmono(  0.0,100.0,  0.0,gray,ierr); write(*,*)'green   ',gray
          call rgbmono(  0.0,  0.0,100.0,gray,ierr); write(*,*)'blue    ',gray
          call rgbmono(100.0,100.0,  0.0,gray,ierr); write(*,*)'Yellow  ',gray
          call rgbmono(  0.0,100.0,100.0,gray,ierr); write(*,*)'Cyan    ',gray
          call rgbmono(100.0,  0.0,100.0,gray,ierr); write(*,*)'Magenta ',gray
          call rgbmono(100.0,100.0,100.0,gray,ierr); write(*,*)'White   ',gray
          call rgbmono( 00.0,  0.0,  0.0,gray,ierr); write(*,*)'Black   ',gray
          call rgbmono( 50.0,  0.0,  0.0,gray,ierr); write(*,*)'Maroon  ',gray
          call rgbmono(100.0, 50.0, 50.0,gray,ierr); write(*,*)'Pink    ',gray
          end program demo_rgbmono
