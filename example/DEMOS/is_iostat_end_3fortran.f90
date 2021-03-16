           program demo_iostat
             implicit none
             integer :: stat, i
             open(88, file='test.dat')
             read(88, *, iostat=stat) i
             if(is_iostat_end(stat)) stop 'end of file'
           end program demo_iostat
