           program demo_lookfor
           use M_io, only : lookfor
           implicit none
           character(len=:),allocatable :: returned
              returned=lookfor('ls','PATH')
              write(*,*)'ls is ',returned
              returned=lookfor('dir.exe','PATH')
              write(*,*)'dir is ',returned
           end program demo_lookfor
