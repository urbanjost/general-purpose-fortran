           program demo_slurp
           use M_io, only      : slurp
           implicit none
           character(len=1),allocatable :: text(:) ! array to hold file in memory
           character(len=*),parameter :: FILENAME='inputfile' ! file to read

           ! create test file
           open(file=FILENAME,unit=10)
           write(10,'(a)') new_line('A')//'esrever lliw'
           write(10,'(a)') 'margorp elpmas eht taht'
           write(10,'(a)') 'elif elpmas a si sihT'
           close(unit=10)

           call slurp(FILENAME,text) ! allocate character array and copy file into it

           if(.not.allocated(text))then
              write(*,*)'*rever* failed to load file '//FILENAME
           else
              ! write file reversed to stdout
              write(*,'(*(a:))',advance='no')text(size(text):1:-1)
              deallocate(text)  ! release memory
           endif

           end program demo_slurp
