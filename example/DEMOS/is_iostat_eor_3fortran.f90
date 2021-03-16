           program demo_is_iostat_eor
             implicit none
             integer :: stat, i(50)
             open(88, file='test.dat', form='unformatted')
             read(88, iostat=stat) i
             if(is_iostat_eor(stat)) stop 'end of record'
           end program demo_is_iostat_eor
