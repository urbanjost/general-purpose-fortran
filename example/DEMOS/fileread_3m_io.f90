     program demo_fileread
     use M_io,      only : fileread
     implicit none
     character(len=4096)          :: FILENAME   ! file to read
     character(len=:),allocatable :: pageout(:) ! array to hold file in memory
     integer                      :: longest, lines, i
     character(len=*),parameter   :: gen='(*(g0,1x))'
        ! get a filename
        call get_command_argument(1, FILENAME)
        ! allocate character array and copy file into it
        call fileread(FILENAME,pageout)
        if(.not.allocated(pageout))then
           write(*,gen)'*demo_fileread* failed to load file',FILENAME
        else
           ! write file from last line to first line
           longest=len(pageout)
           lines=size(pageout)
           write(*,gen)'number of lines is',lines
           write(*,gen)'and length of lines is',longest
           write(*,'(a)')repeat('%',longest+2)
           write(*,'("%",a,"%")')(trim(pageout(i)),i=lines,1,-1)
           write(*,'(a)')repeat('%',longest+2)
           deallocate(pageout)  ! release memory
        endif
     end program demo_fileread
