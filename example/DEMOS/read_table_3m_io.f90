           program demo_read_table
           use M_io, only : read_table
           implicit none
           doubleprecision,allocatable :: array(:,:)
           integer :: i, ierr

           ! create test file
           open(file='inputfile',unit=10)
           write(10,'(a)') [character(len=80):: &
            '#---#---#---#                          ', &
            '| 1 | 5 | 3 |                          ', &
            '#---#---#---#                          ', &
            '| 4 | 2 | 6 |                          ', &
            '#---#---#---#                          ', &
            '                                       ', &
            '1;10;45                                ', &
            '10, ,, ,,20    45                      ', &
            '  2 20  15                             ', &
            ' big=20.345 medium=20  small=15        ', &
            '                                       ', &
            '30 30e3   0                            ', &
            '  4 300.444e-1 -10                     ', &
            '40 30.5555d0 -10                       ', &
            '  4 300.444E-1 -10                     ', &
            '40 30.5555D0 -10                       ', &
            '                                       ']
           close(unit=10)

           ! read file as a table
           call read_table('inputfile',array,ierr)

           ! print values
           write(*,*)'size=       ',size(array)
           write(*,*)'size(dim=1)=',size(array,dim=1)
           write(*,*)'size=(dim=2)',size(array,dim=2)
           do i=1,size(array,dim=1)
              write(*,*)array(i,:)
           enddo

           ! remove sample file
           open(file='inputfile',unit=10)
           close(unit=10,status='delete')

           end program demo_read_table
