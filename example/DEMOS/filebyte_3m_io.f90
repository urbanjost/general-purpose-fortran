      program demo_filebyte
      use M_io, only      : filebyte
      implicit none
      character(len=1),allocatable :: text(:) ! array to hold file in memory
      character(len=*),parameter :: FILENAME='inputfile' ! file to read
      integer :: length,lines

      ! create test file
      open(file=FILENAME,unit=10,action='write')
      write(10,'(a)') new_line('A')//'esrever lliw'
      write(10,'(a)') 'margorp elpmas eht taht'
      write(10,'(a)') 'elif elpmas a si sihT'
      close(unit=10)

      call filebyte(FILENAME,text,length,lines) ! allocate character array and copy file into it

      if(.not.allocated(text))then
         write(*,*)'*rever* failed to load file '//FILENAME
      else
         write(*,'(*(g0))')'lines=',lines,' length=',length
         write(*,'(a)')repeat('=',80)
         ! write file
         write(*,'(*(a:))',advance='no')text
         write(*,'(a)')repeat('=',80)
         ! write file reversed to stdout
         write(*,'(*(a:))',advance='no')text(size(text):1:-1)
         write(*,'(a)')repeat('=',80)
         deallocate(text)  ! release memory
      endif

      end program demo_filebyte
