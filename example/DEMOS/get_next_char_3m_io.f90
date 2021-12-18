     program demo_get_next_char
     use,intrinsic :: iso_fortran_env, only : iostat_end
     use M_io, only : get_next_char
     implicit none
     character(len=4096) :: filename ! filename to read
     character(len=256)  :: message  ! returned error messages
     integer             :: fd       ! file descriptor for input file
     integer             :: ios,ios1 ! hold I/O error flag
     character           :: c1       ! current character read
        filename='test.in'
        open(unit=fd,file=trim(filename),access='stream',status='old',&
        & iostat=ios,action='read',form='unformatted',iomsg=message)
        if(ios.ne.0)then
           write(*,*)&
           '*demo_get_next_char* ERROR: could not open '//&
           trim(filename)
           write(*,*)&
           '*demo_get_next_char* ERROR: '//trim(message)
           stop 5
        endif
        ! loop through read of file one character at a time
        ONE_CHAR_AT_A_TIME: do
           ! get next character from buffered read from file
           call get_next_char(fd,c1,ios1)
           if(ios1.eq.iostat_end)then
              ! reached end of file so stop
              stop
           elseif(ios1.ne.0 )then
              ! error on file read
              write(*,*)&
           '*demo_get_next_char* ERROR: before end of '//&
           trim(filename)
              stop 1
           endif
           ! do something with the characters
           write(*,'(a)',advance='no')c1
        enddo ONE_CHAR_AT_A_TIME
     end program demo_get_next_char
