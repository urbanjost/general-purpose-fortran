           program demo_which
           use M_io, only : which
           implicit none
              write(*,*)'ls is ',which('ls')
              write(*,*)'dir is ',which('dir')
              write(*,*)'install is ',which('install')
           end program demo_which
