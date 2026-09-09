       program demo_getname
       use M_io, only : getname
       implicit none
          write(*,'(*(a))')'Running ',getname()
       end program demo_getname
